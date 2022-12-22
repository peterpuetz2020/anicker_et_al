# sensitivity analyses for the interaction effects in study 2

# clear workspace
rm(list=ls())

# get functions defined in other script
source("R/functions.R")

# install pacman package if required
if (!("pacman" %in% installed.packages())) install.packages("pacman") 
# load required packages
pacman::p_load(tidyverse, psych, MASS, readxl)

# set random seed to ensure replicability
set.seed(6)

# set number of simulations
mc = 5000

# desired power
power_targeted = 0.8

# set alpha
alpha = 0.05

# range of interaction effects that should be checked, i.e. for which
# power will be calculated
beta_range <- seq(0.10, 0.15, length = 40)

# read in data
da <- read_excel("data/study2_data.xlsx", 
                 sheet = "data")

# restrict data to first intervention
da_intA <-
  da %>% 
  filter(!is.na(KGA_IntA)) %>% 
  # center BioV variable
  mutate(MW_Bio = scale(MW_Bio, center = TRUE, scale = FALSE))

# restrict data to second intervention
da_intB <-
  da %>% 
  filter(!is.na(KGB_IntB)) %>% 
  # center BioV variable
  mutate(MW_Bio = scale(MW_Bio, center = TRUE, scale = FALSE))

## exemplary interpretation
# fit model
mod <- lm(Rebound ~ KGA_IntA*MW_Bio, data = da_intA)
sum_mod <- summary(mod)
sum_mod

# visualizatin of interaction effect
# on probability scale
pred_data <- data.frame(yhat_prob = predict(mod, type = "response"), 
                        yhat_odds = predict(mod, type = "response")/
                          (1-predict(mod, type = "response")), 
                        x = da_intA$MW_Bio,
                        f = as.factor(da_intA$KGA_IntA))
ggplot(pred_data, aes(x = x, y = yhat_prob, group = f,
                      color = f)) + 
  geom_line() 

# for MW_Bio = 0, the intervention effect decreases the rebound by  0.225 units
# for the interaction: one additional biosperic value decreases the intervention
# effect on the rebound
# by 0.0258877, e.g. when increasing the biosperic value from 0 to 1, 
# the intervention effect on rebound decreases from
# -0.2256266 to -0.2515143


# Effect of intervention A on Rebound ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but 
# just want to check the results, go to the end of this script

# estimate model
mod <- lm(Rebound ~ KGA_IntA*MW_Bio, data = da_intA)
sum_mod <- summary(mod)

# get power for range of betas
results <- get_power_lm(data = da_intA,
                        cond ="KGA_IntA",
                        cov = "MW_Bio",
                        intercept = mod$coefficients["(Intercept)"],
                        beta_cond = mod$coefficients["KGA_IntA"],
                        beta_cov = mod$coefficients["MW_Bio"],
                        beta_int = beta_range, 
                        res_std_error = sum_mod$sigma)

# save results
write.table(apply(results, 1, mean), "results/lm_power_inta_rebound.dat", row.names = FALSE, col.names = FALSE)

# Effect of intervention B on Rebound ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but 
# just want to check the results, go to the end of this script

# estimate model
mod <- lm(Rebound ~ KGB_IntB*MW_Bio, data = da_intB)
sum_mod <- summary(mod)

# get power for range of betas
results <- get_power_lm(data = da_intB,
                        cond ="KGB_IntB",
                        cov = "MW_Bio",
                        intercept = mod$coefficients["(Intercept)"],
                        beta_cond = mod$coefficients["KGB_IntB"],
                        beta_cov = mod$coefficients["MW_Bio"],
                        beta_int = beta_range, 
                        res_std_error = sum_mod$sigma)

# save results
write.table(apply(results, 1, mean), "results/lm_power_intb_rebound.dat", row.names = FALSE, col.names = FALSE)

# summarise results -------------------------------------------------------
# read in results data
results_inta_rebound <- read.table("results/lm_power_inta_rebound.dat")$V1
results_intb_rebound <- read.table("results/lm_power_intb_rebound.dat")$V1

# plot power for range of interaction effects in model with 
# intervention a and dependent variable rebound
plotdat_inta_rebound <- data.frame(coefficient = beta_range,
                                   power = results_inta_rebound)
ggplot(plotdat_inta_rebound, aes(x = coefficient, y = power)) +
  geom_path()+
  geom_hline(yintercept = power_targeted, linetype = 2)+
  labs(x = "Effect size (coefficient)", y = "Power")+
  theme_bw()

# plot power for range of interaction effects in model with 
# intervention b and dependent variable rebound
plotdat_intb_rebound <- data.frame(coefficient = beta_range,
                                   power = results_intb_rebound)
ggplot(plotdat_intb_rebound, aes(x = coefficient, y = power)) +
  geom_path()+
  geom_hline(yintercept = power_targeted, linetype = 2)+
  labs(x = "Effect size (coefficient)", y = "Power")+
  theme_bw()

# show odds ratio for interaction effect of intervention a and biosperic value
# on rebound that corresponds (nearly) to a power of 0.8 
plotdat_inta_rebound %>% 
  mutate(deviation = abs(power-power_targeted)) %>% 
  filter(deviation == min (deviation))
# show odds ratio for interaction effect of intervention b and biosperic value
# on rebound that corresponds (nearly) to a power of 0.8 
plotdat_intb_rebound %>% 
  mutate(deviation = abs(power-power_targeted)) %>% 
  filter(deviation == min (deviation))
