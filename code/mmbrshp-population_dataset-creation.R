# MEMBERSHIP METRICS: SIMULATION
# CREATION OF SIMULATION DATASETS (informed by Ottawa Public Health)

# define gender distribution across age groups of individuals in Ottawa from 20-39
age_groups <- c("20-24", "25-29", "30-34", "35-39")
males <- c(36498, 40040, 43013, 41858)
females <- c(34178, 39044, 44502, 43702)

# start empty population dataframe
pop <- data.frame()
for (i in 1:length(age_groups)){
  
  # sample male and female records for the age group i
  sample_m <- data.frame(gender = rep("M",males[i]), age = rep(age_groups[i], males[i]))
  sample_f <- data.frame(gender = rep("F",females[i]), age = rep(age_groups[i], females[i]))
  pop_age <- rbind(sample_m, sample_f)
  pop <- rbind(pop, pop_age)
}
pop <- pop[sample(1:nrow(pop), nrow(pop)), ]

# create gender weights for HIV-positive population 
weights <- ifelse(pop$gender == "M", 3.2, 1)
weights_norm <- weights/sum(weights)

# sample HIV-positive population from population in Ottawa between 15 and 24 years
hiv <- pop[sample(1:nrow(pop), 2172, replace = FALSE, prob = weights_norm), ] 

# sample dataset from HIV-positive population between 15 and 24 years in Ottawa
hiv_sample <- hiv[sample(1:nrow(hiv), 1000), ]

# store training dataset as csv for SDG
path_training <- "mmbrshp-population_hiv-sample.csv"
path_metadata <- "mmbrshp-population_hiv-sample.json"
write.csv(hiv_sample, path_training, row.names = F)

# SDG via sampling with replacement
hiv_syn_1 <- hiv_sample[sample(1:nrow(hiv_sample), 1000, replace = TRUE), ]

# SDG via Bayesian Network
gen_name <- "synthcity_bayesian_network"
reticulate::use_condaenv("pysdg-R", required = TRUE)
synth.generate <- reticulate::import("pysdg.synth.generate")
synth.load <- reticulate::import("pysdg.synth.load")
pandas <- reticulate::import("pandas")
gen <- synth.generate$Generator(gen_name)
real <- gen$load(path_training, path_metadata)
gen$train()
gen$gen(nrow(real), 1)
hiv_syn_2 <- gen$unload()[[1]]

# create unique identifier for real datasets
hiv_sample$ID <- rownames(hiv_sample)
pop$ID <- rownames(pop)
hiv$ID <- rownames(hiv)

# store all datasets 
write.csv(hiv_sample, path_training, row.names = F)
write.csv(pop, "mmbrshp-population_pop.csv", row.names = F)
write.csv(hiv, "mmbrshp-population_hiv.csv", row.names = F)
write.csv(hiv_syn_1, "mmbrshp-population_hiv-syn-swr.csv", row.names = F)
write.csv(hiv_syn_2, "mmbrshp-population_hiv-syn-bn.csv", row.names = F)




