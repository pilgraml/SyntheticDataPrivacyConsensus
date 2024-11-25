# OVERARCHING CONSIDERATIONS: SIMULATION
# RELATIONSHIP BETWEEN NUMBER AND CHOICE OF MATCHING VARIABLES AND MEMBERSHIP DISCLOSURE VULNERABILITY

# libraries
library(dplyr)
library(jsonlite)
library(reticulate)
library(parallel)
library(doParallel)
library(pbapply)
library(ggplot2)
library(gridExtra)
library(arules)

# input datasets: out of the 10 datasets only the publicly available exemplar dataset is used as input (see documentation in appendix B)
data_names <- c("nexoid")

# main path
path <- "your-path"

# define plot theme
consistent_theme <- theme_minimal() +
  theme(
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.grid.major = element_line(linewidth = 0.2),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10) 
  )

# simulation 
f1_plots <- list() # to initiate list of plots
letter_counter <- 1  # to create subplot labeling

for (data_name in data_names) {
  
  # define paths to datasets and metadata (as json files)
  path_metadata <- paste0(path, "matching-mmbrshp_", data_name, "_real.json")
  path_training <- paste0(path, "matching-mmbrshp_", data_name, "_real.csv")
  path_attack <- paste0(path, "matching-mmbrshp_", data_name, "_attack.csv")
  path_synth <- paste0(path, "matching-mmbrshp_", data_name, "_synth.csv")
  
  # read in data
  df <- read.csv(path_training)
  attack <- read.csv(path_attack)
  synth <- read.csv(path_synth)
  
  # read in metadata and define datatypes
  info <- jsonlite::fromJSON(path_metadata)
  cnt_idxs <- unlist(info$cnt_idxs) + 1
  cnt_vars <- colnames(df[cnt_idxs])
  dscrt_idxs <- unlist(info$dscrt_idxs) + 1
  dscrt_vars <- colnames(df[dscrt_idxs])
  cat_idxs <- unlist(info$cat_idxs) + 1
  cat_vars <- colnames(df[cat_idxs])
  datetime_idxs <- unlist(info$datetime_idxs) + 1
  datetime_vars <- colnames(df[datetime_idxs])
  miss_val <- info$miss_vals
  
  # enforce datatypes according to metadata
  datatype.uniform <- function(data, miss_val, cnt_vars, dscrt_vars, cat_vars, datetime_vars){ 
    data <- as.data.frame(apply(data, 2, function(x) replace(x, x %in% miss_val, NA)))
    data[cnt_vars] <- lapply(data[cnt_vars], as.numeric)
    data[dscrt_vars] <- lapply(data[dscrt_vars], as.integer)
    data[cat_vars] <- lapply(data[cat_vars], as.factor)
    data[datetime_vars] <- lapply(data[datetime_vars], function(x) 
      as.Date(as.POSIXct(as.character(x), tryFormats = c("%Y/%m/%d %H:%M:%S%z", "%Y/%m/%d %H:%M:%OS", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d"))))
    return(data)
  }
  df <- datatype.uniform(df, miss_val = miss_val, cnt_vars = cnt_vars, dscrt_vars = dscrt_vars, cat_vars = cat_vars, datetime_vars = datetime_vars)
  attack <- datatype.uniform(attack, miss_val = miss_val, cnt_vars = cnt_vars, dscrt_vars = c(dscrt_vars, "member"), cat_vars = cat_vars, datetime_vars = datetime_vars)
  synth <- datatype.uniform(synth, miss_val = miss_val, cnt_vars = cnt_vars, dscrt_vars = dscrt_vars, cat_vars = cat_vars, datetime_vars = datetime_vars)
   
  # discretizing numerical and datetime variables for matching
  synth$origin <- "S"
  attack$origin <- "A"
  df_bind <- rbind(synth, attack[-(ncol(attack)-1)])
  df_bind[datetime_vars] <- lapply(df_bind[datetime_vars], 
                                   function(x) as.numeric(as.Date(x) - as.Date("1900-01-01")))
  num_vars <- c(cnt_vars, dscrt_vars, datetime_vars)
  df_bind[num_vars] <- lapply(num_vars, function(x){
    if (length(unique(df_bind[[x]])) > 20){
      df_bind[[x]] <- arules::discretize(df_bind[[x]], method = "interval", breaks = 20)
    } 
    df_bind[[x]] <- as.character(df_bind[[x]])
    return(df_bind[[x]])
  })
  synth_discr <- df_bind[df_bind$origin == "S", ]
  attack_discr <- df_bind[df_bind$origin == "A", ]
  attack_discr$member <- attack$member
  attack_discr$origin <- NULL
  synth_discr$origin <- NULL
  
  # match records between attack and synthetic data 
  vars <- colnames(attack_discr)[-ncol(attack_discr)]
  cl <- parallel::makeCluster(30)
  doParallel::registerDoParallel(cl)
  invisible(parallel::clusterExport(cl, varlist = c("vars", "synth_discr", "attack_discr")))
  invisible(parallel::clusterEvalQ(cl, expr = {
    library(dplyr)
  }))
  
  # matching for each number of QI (from 1 to 20) 
  attack_sim <- lapply(1:length(vars), function(k){
    
    # matching for each combination of QI for that number
    qi_lst <- combn(vars, k, simplify = FALSE)
    result_lst <- pbapply::pblapply(qi_lst, function(qi){
      synth_filtered <- synth_discr %>%
        select(all_of(qi))
      
      # find matches
      matched_targets <- attack_discr %>%
        semi_join(synth_filtered, by = qi)
      
      # find non-matches
      non_matched_targets <- attack_discr %>%
        anti_join(synth_filtered, by = qi)
      
      # define metrics: true positive (tp), false positive (fp), false negative (fn)
      tp <- sum(matched_targets$member == 1)
      fp <- sum(matched_targets$member == 0)
      fn <- sum(non_matched_targets$member == 1)
      precision <- tp/(tp+fp)
      recall <- tp/(tp+fn)
      f1 <- 2*(precision*recall)/(precision+recall)
      return(list(precision = precision, recall = recall, f1 = f1))
    }, cl = cl)
    
    # average across results
    precision_k <- mean(sapply(result_lst, function(x) x$precision))
    precision_k_sd <- sd(sapply(result_lst, function(x) x$precision))
    recall_k <- mean(sapply(result_lst, function(x) x$recall))
    recall_k_sd <- sd(sapply(result_lst, function(x) x$recall))
    f1_k <- mean(sapply(result_lst, function(x) x$f1))
    f1_k_sd <- sd(sapply(result_lst, function(x) x$f1))
    
    # take maximum across results
    precision_k_max <- max(sapply(result_lst, function(x) x$precision))
    recall_k_max <- max(sapply(result_lst, function(x) x$recall))
    f1_k_max <- max(sapply(result_lst, function(x) x$f1))
    
    # return f1 score
    return(list(f1 = f1_k, f1_sd = f1_k_sd, f1_max = f1_k_max, k = k))
  })
  parallel::stopCluster(cl)
  
  # combine results to dataframe
  results <- do.call(rbind, lapply(attack_sim, function(x) {
    data.frame(k = x$k, f1 = x$f1, f1_sd = x$f1_sd, f1_max = x$f1_max)
  }))
  
  # manually add standard deviation of 0 for single observation point
  results$f1_sd[[20]] <- 0
  
  # plot average f1 score
  plot_f1_avg <- ggplot(results, aes(x = k, y = f1)) +
    geom_line(color = "black") +
    labs(x = "Number of Variables", y = "F1 Score") +
    ggtitle(LETTERS[letter_counter]) +
    scale_y_continuous(limits = c(0, 1)) +
    consistent_theme
  
  # add standard deviation
  plot_f1_avg <- plot_f1_avg + geom_ribbon(aes(ymin = pmax(f1-f1_sd, 0), ymax = pmin(f1+f1_sd, 1)), fill = "grey", alpha = 0.4)
  
  # plot max f1 score
  letter_counter <- letter_counter + 1
  plot_f1_max <- ggplot(results, aes(x = k, y = f1_max)) +
    geom_line(color = "black") +
    labs(x = "Number of Variables", y = "F1 Score") +
    ggtitle(LETTERS[letter_counter]) +
    scale_y_continuous(limits = c(0, 1)) +
    consistent_theme
  letter_counter <- letter_counter + 1
  
  # add plots to existing list
  f1_plots <- c(f1_plots, list(plot_f1_avg, plot_f1_max))
  
  # store results
  ggsave(paste0(path_output, "matching-mmbrshp_", data_name, "_plot-avg.png"), plot = plot_f1_avg, width = 13, height = 11, dpi = 300)
  ggsave(paste0(path_output, "matching-mmbrshp_", data_name, "_plot-max.png"), plot = plot_f1_max, width = 13, height = 11, dpi = 300)
  write.csv(results, paste0(path_output, "matching-mmbrshp_", data_name, "_results.csv"), row.names = FALSE)
}
