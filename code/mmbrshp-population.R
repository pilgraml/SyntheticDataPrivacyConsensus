# MEMBERSHIP METRICS: SIMULATION
# RELATIONSHIP BETWEEN SAMPLING ASSUMPTION AND ATTACK SUCCESS

# libraries
library(dplyr)

# input datasets: out of the 10 datasets only the publicly available exemplar dataset is used as input (see documentation in appendix B)
hiv_syn_1 <- read.csv("mmbrshp-population_hiv-syn-swr.csv")
hiv_syn_2 <- read.csv("mmbrshp-population_hiv-syn-bn.csv")
hiv_sample <- read.csv("mmbrshp-population_hiv-sample.csv")
hiv <- read.csv("mmbrshp-population_hiv.csv")
pop <- read.csv("mmbrshp-population_pop.csv")

# adversary success when matching the unique identifier over 1,000 attack datasets
iterations <- 1000 

membership <- function(reference, hiv_syn, hiv_sample) {
  naive_guess <- replicate(iterations, {
    attack <- reference[sample(1:nrow(reference), 1000, replace = FALSE),]
    
    # create label for members
    attack$member <-  ifelse(attack$ID %in% hiv_sample$ID, 1, 0)
    
    # find matches
    matched_targets <- attack %>%
      semi_join(hiv_syn, by = c("gender","age"))
    
    # define metrics: true positive (tp), false positive (fp), false negative (fn)
    tp <- sum(matched_targets$member == 1)
    fp <- sum(matched_targets$member == 0)
    precision <- tp/(tp+fp)
    return(precision)
  })
  precision_avg <- mean(naive_guess)
  return(precision_avg)
}

# calculate precision for Scenario A when drawing from the same population the training data is sampled from 
attack_s1sA <- membership(reference = hiv, hiv_syn = hiv_syn_1, hiv_sample)
attack_s2sA <- membership(reference = hiv, hiv_syn = hiv_syn_2, hiv_sample)

# calculate precision for Scenario B when drawing from a super-population
attack_s1sB <- membership(reference = pop, hiv_syn = hiv_syn_1, hiv_sample)
attack_s2sB <- membership(reference = pop, hiv_syn = hiv_syn_2, hiv_sample)
