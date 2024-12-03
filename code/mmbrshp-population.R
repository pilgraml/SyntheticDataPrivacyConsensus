# MEMBERSHIP METRICS: SIMULATION
# RELATIONSHIP BETWEEN SAMPLING ASSUMPTION AND ATTACK SUCCESS

# define input data: all records consist of a unique identifier only
pop <- 1:595198 # population in Ottawa between 15 and 54 years 
hiv <- 1:3680 # HIV positive people in Ottawa between 15 and 54 years 
hiv_sample <- 1:1000 # random dataset sample
hiv_syn <- sample(hiv_sample, 1000, replace = TRUE) # SDG as sampling with replacement

# adversary success when matching the unique identifier
iterations <- 1000 

membership <- function(reference, hiv_syn, hiv_sample) {
  naive_guess <- replicate(iterations, {
    attack <- sample(reference, 1000, replace = FALSE)
    members <- sum(attack %in% hiv_syn)
    precision <- members/length(attack)
    return(precision)
  })
  precision_avg <- mean(naive_guess)
  return(precision_avg)
}

# calculate precision for Scenario A when drawing from the same population the training data is sampled from 
attack_sA <- membership(reference = hiv, hiv_syn, hiv_sample)
# calculate precision for Scenario B when drawing from a super-population
attack_sB <- membership(reference = pop, hiv_syn, hiv_sample)
