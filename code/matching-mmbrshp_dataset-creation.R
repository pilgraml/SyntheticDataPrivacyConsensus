# OVERARCHING CONSIDERATIONS: SIMULATION
# CREATION OF SIMULATION DATASETS

# package pysdg with created conda environment required

# input datasets: 10 datasets (see documentation in appendix B), nexoid as exemplar dataset provided (publicly available)
data_names <- c("born", "california", "cchs", "faers", "florida", "newyork", "nexoid", "texas", "washington", "washington2008")

# main path to the datasets
main_path <- "your-path"

# creation of simulation data from real data
for (data_name in data_names) {
  
  # read in source population with corresponding source metadata file
  df_max <- read.csv(paste0(main_path, data_name, "/", data_name, "_max.csv"))
  df_max_info <- jsonlite::fromJSON(paste0(main_path, data_name, "/", data_name, "_max.json"))
  
  # create population variant with 50,000 records and 20 variables
  pop <- df_max[sample(1:nrow(df_max), 50000),]
  vars_select <- sample(1:ncol(pop), 20)
  pop <- pop[, vars_select]
  
  # define metadata of population variant
  cnt_idxs <- unlist(df_max_info$cnt_idxs) + 1
  dscrt_idxs <- unlist(df_max_info$dscrt_idxs) + 1
  cat_idxs <- unlist(df_max_info$cat_idxs) + 1
  datetime_idxs <- unlist(df_max_info$datetime_idxs) + 1
  miss_val <- df_max_info$miss_vals
  pop_info <- df_max_info
  pop_info$cnt_idxs <- sort(match(intersect(cnt_idxs, vars_select), vars_select)) - 1
  pop_info$dscrt_idxs <- sort(match(intersect(dscrt_idxs, vars_select), vars_select)) - 1
  pop_info$cat_idxs <- sort(match(intersect(cat_idxs, vars_select), vars_select)) - 1
  pop_info$datetime_idxs <- sort(match(intersect(datetime_idxs, vars_select), vars_select)) - 1
  pop_info$quasi_idxs <- character()
  pop_info$downstream_idxs <- character()
  pop_info$ds_name <- paste0(data_name, "_mmrshp_real")
  json_data <- jsonlite::toJSON(pop_info)
  path_metadata <- paste0(main_path, "matching-mmbrshp_", data_name, "_mmbrshp_real_1013.json")
  writeLines(json_data, path_metadata)

  # draw real dataset sample and attack dataset sample
  df <- pop[sample(1:nrow(pop), 10000),]
  attack <- pop[sample(1:nrow(pop), 10000),]
  
  # create label for members
  attack$member <-  ifelse(rownames(attack) %in% rownames(df), 1, 0)
  
  # store training dataset as csv for SDG
  path_training <- paste0(main_path, "matching-mmbrshp_", data_name, "_real.csv")
  write.csv(df, path_training, row.names = FALSE)

  # SDG via pysdg
  gen_name <- "synthcity_bayesian_network"
  reticulate::use_condaenv("pysdg-R", required = TRUE)
  synth.generate <- reticulate::import("pysdg.synth.generate")
  synth.load <- reticulate::import("pysdg.synth.load")
  pandas <- reticulate::import("pandas")
  gen <- synth.generate$Generator(gen_name)
  real <- gen$load(path_training, path_metadata)
  gen$train()
  gen$gen(nrow(real), 1)
  synth <- gen$unload()[[1]]

  # store datasets
  write.csv(attack, paste0(main_path, "matching-mmbrshp_", data_name, "_attack.csv"), row.names = FALSE)
  write.csv(synth, paste0(main_path, "matching-mmbrshp_", data_name, "_synth.csv"), row.names = FALSE)
}