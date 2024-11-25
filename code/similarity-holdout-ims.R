# SIMILARITY METRICS: SIMULATION
# RELATIONSHIP BETWEEN IDENTICAL MATCH RATE IN HOLDOUT DATA, DATASET SIZE AND IDENTITY DISCLOSURE

# libraries
library(ggplot2)

# main path
path_output <- "your-path"

# define input parameter
m <- 100000 # size of population
k.range <- 1:20 # size of equivalence class
n.range <- seq(0, 10000, by = 50) # size of real dataset
n.range <- replace(n.range, 1, 10)
iterations <- 500 # number of repetitions

# generate populations of different identity disclosure vulnerability (i.e., k)
pop.calc <- function(k, m) {
  QI <- 1:(m/k)
  rep(QI, k)
}
pop.store <- lapply(k.range, pop.calc, m = m)


# calculate identical match rate for each of the population with ranging dataset size
ims.calc <- function(pop, n, k) {
  syn.risk.iter <- replicate(iterations, {
    
    # sample real data from population
    origin <- sample(pop, n, replace = FALSE)
    origin.df <- data.frame(origin)
    
    # split into training and holdout
    train.ind <- sample(n, n*0.8, replace = FALSE)
    train <- origin.df[train.ind,]
    holdout <- origin.df[-train.ind,]
    
    # mimic a poorly generalizing synthetic data generation mechanism
    syn <- sample(train, n*0.8, replace = TRUE) 
    
    # calculate identical match rate between holdout data and synthetic data
    dcr.holdout <- outer(syn, holdout, FUN = function(x, y) abs(x == y))
    ims.holdout <- sum(apply(dcr.holdout, 1, max)) / (n*0.8)
    ims.holdout
  })
  
  # average across repetitions
  n.mean <- mean(syn.risk.iter)
  c(n.mean, n, k)
}

# run ims function across all populations
ims.results <- do.call(rbind, lapply(n.range, function(n) {
  do.call(rbind, Map(function(pop, k) {
    ims.calc(pop, n, k)
  }, pop.store, k.range))
}))
colnames(ims.results) <- c("ims.holdout", "n", "k")

# store results
syn.risk.n.k <- as.data.frame(ims.results)
write.csv(syn.risk.n.k, paste0(path_output, "similarity-holdout-ims_results.csv"), row.names = FALSE)

# visualize
X1 <- syn.risk.n.k[,"n"]
Y1 <- syn.risk.n.k[,"ims.holdout"]
D1 <- 1/syn.risk.n.k[, "k"]
plot <- ggplot(data = syn.risk.n.k, aes(X1, Y1, group = D1))+
  geom_line(linewidth=0.2) +
  labs(y = "IMS (Holdout)", x = "Size of original dataset", color = "Actual Vulnerability [1/k]", title = "IMS (Holdout) vs. Dataset Size")

# store results
ggsave(paste0(path_output, "similarity-holdout-ims_plot.png"), plot = plot, width = 13, height = 11, dpi = 300)
