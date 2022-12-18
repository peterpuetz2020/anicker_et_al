# sensitivity analyses for the interaction effects in study 1

# clear workspace
rm(list = ls())

# get functions defined in other script
source("R/functions.R")

# install pacman package if required
if (!("pacman" %in% installed.packages()))
  install.packages("pacman")
# load required packages
pacman::p_load(tidyverse, psych, MASS, readxl)

# functions for transforming log odds
LogOdds_Prob <- function(x) {
  exp(x) / (1 + exp(x))
}
Prob_LogOdds <- function(x) {
  log(x / (1 - x))
}

# set random seed to ensure replicability
set.seed(6)

# set number of simulations
mc = 5000

# desired power
power_targeted = 0.8

# set alpha
alpha = 0.05

# Range of interaction effects that should be checked, i.e. for which
# power will be calculated
beta_range <- seq(0.6, 0.8, length = 40)
# corresponding odds ratio
exp(beta_range)

# read in data
da <- read_excel("data/study1_data.xlsx",
                 sheet = "data") %>%
  # recode CCC variable
  mutate(CCC = ifelse(CCC == 2, 1, 0))

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
mod <- glm(CCP ~ KGA_IntA * MW_Bio, data = da_intA, family = binomial)
sum_mod <- summary(mod)
sum_mod

# visualizatin of interaction effect
# on probability scale
pred_data <-
  data.frame(
    yhat_prob = predict(mod, type = "response"),
    yhat_odds = predict(mod, type = "response") /
      (1 - predict(mod, type = "response")),
    x = da_intA$MW_Bio,
    f = as.factor(da_intA$KGA_IntA)
  )
ggplot(pred_data, aes(
  x = x,
  y = yhat_prob,
  group = f,
  color = f
)) +
  geom_line()
# on odds scale
ggplot(pred_data, aes(
  x = x,
  y = yhat_odds,
  group = f,
  color = f
)) +
  geom_line()
# compare with model output
exp(mod$coefficients)
# for MW_Bio = 0, the intervention effect increases the odds by a factor of 3.77 from
LogOdds_Prob(mod$coefficients %*% c(1, 0, 0, 0)) /
  (1 - LogOdds_Prob(mod$coefficients %*% c(1, 0, 0, 0)))# to
LogOdds_Prob(mod$coefficients %*% c(1, 1, 0, 0)) /
  (1 - LogOdds_Prob(mod$coefficients %*% c(1, 1, 0, 0)))
# or the probability from
LogOdds_Prob(mod$coefficients %*% c(1, 0, 0, 0))
# to
LogOdds_Prob(mod$coefficients %*% c(1, 1, 0, 0))

# for the interaction: one additional biosperic value increases the intervention
# effect on the odds
# by a factor of 1.195, e.g. when increasing the biosperic value from 0 to 1,
# the intervention effect on the odds of CCP increases from
# 3.7701425 (see above) to 3.7701425*1.1952695 = 4.5:
LogOdds_Prob(mod$coefficients %*% c(1, 1, 0, 0)) /
  (1 - LogOdds_Prob(mod$coefficients %*% c(1, 1, 0, 0))) / (LogOdds_Prob(mod$coefficients %*% c(1, 0, 0, 0)) /
                                                              (1 - LogOdds_Prob(mod$coefficients %*% c(1, 0, 0, 0))))
LogOdds_Prob(mod$coefficients %*% c(1, 1, 1, 1)) /
  (1 - LogOdds_Prob(mod$coefficients %*% c(1, 1, 1, 1))) / (LogOdds_Prob(mod$coefficients %*% c(1, 0, 1, 0)) /
                                                              (1 - LogOdds_Prob(mod$coefficients %*% c(1, 0, 1, 0))))
# on a probability scale (holds only for this particular increase of biosperic value
# from zero to one)
ggplot(pred_data, aes(
  x = x,
  y = yhat_prob,
  group = f,
  color = f
)) +
  geom_line()
# intervention effect changes from
plogis(mod$coefficients %*% c(1, 1, 0, 0)) -
  plogis(mod$coefficients %*% c(1, 0, 0, 0)) # to
plogis(mod$coefficients %*% c(1, 1, 1, 1)) -
  plogis(mod$coefficients %*% c(1, 0, 1, 0))

# for comparison: ann odds ratio of 1.5 for the interaction effect would
# change this probability
# from
plogis(mod$coefficients %*% c(1, 1, 0, 0)) -
  plogis(mod$coefficients %*% c(1, 0, 0, 0)) # to
plogis(c(mod$coefficients[1:3], log(1.5)) %*% c(1, 1, 1, 1)) -
  plogis(c(mod$coefficients[1:3], log(1.5)) %*% c(1, 0, 1, 0))


# Effect of intervention A on CCC ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but 
# just want to check the results, go to the end of this script

# estimate model
mod <- glm(CCC ~ KGA_IntA * MW_Bio, data = da_intA, family = binomial)
summary(mod)

# get power for range of betas
results <- get_power_logistic(
  data = da_intA,
  cond = "KGA_IntA",
  cov = "MW_Bio",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["KGA_IntA"],
  beta_cov = mod$coefficients["MW_Bio"],
  beta_int = beta_range
)

# save results
write.table(
  results,
  "results/logistic_power_inta_ccc.dat",
  row.names = FALSE,
  col.names = FALSE
)

# Effect of intervention A on CCP ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but 
# just want to check the results, go to the end of this script

# estimate model
mod <- glm(CCP ~ KGA_IntA * MW_Bio, data = da_intA, family = binomial)
summary(mod)

# get power for range of betas
results <- get_power_logistic(
  data = da_intA,
  cond = "KGA_IntA",
  cov = "MW_Bio",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["KGA_IntA"],
  beta_cov = mod$coefficients["MW_Bio"],
  beta_int = beta_range
)

# save results
write.table(
  results,
  "results/logistic_power_inta_ccp.dat",
  row.names = FALSE,
  col.names = FALSE
)


# Effect of intervention B on CCC ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but 
# just want to check the results, go to the end of this script

# estimate model
mod <- glm(CCC ~ KGB_IntB * MW_Bio, data = da_intB, family = binomial)
summary(mod)

# get power for range of betas
results <- get_power_logistic(
  data = da_intB,
  cond = "KGB_IntB",
  cov = "MW_Bio",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["KGB_IntB"],
  beta_cov = mod$coefficients["MW_Bio"],
  beta_int = beta_range
)

# save results
write.table(
  results,
  "results/logistic_power_intb_ccc.dat",
  row.names = FALSE,
  col.names = FALSE
)


# Effect of intervention B on CCP ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but 
# just want to check the results, go to the end of this script

# estimate model
mod <- glm(CCP ~ KGB_IntB * MW_Bio, data = da_intB, family = binomial)
summary(mod)

# get power for range of betas
results <- get_power_logistic(
  data = da_intB,
  cond = "KGB_IntB",
  cov = "MW_Bio",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["KGB_IntB"],
  beta_cov = mod$coefficients["MW_Bio"],
  beta_int = beta_range
)

# save results
write.table(
  results,
  "results/logistic_power_intb_ccp.dat",
  row.names = FALSE,
  col.names = FALSE
)

# summarise results -------------------------------------------------------
# read in results data
results_inta_ccc <-
  read.table("results/logistic_power_inta_ccc.dat")$V1
results_inta_ccp <-
  read.table("results/logistic_power_inta_ccp.dat")$V1
results_intb_ccc <-
  read.table("results/logistic_power_intb_ccc.dat")$V1
results_intb_ccp <-
  read.table("results/logistic_power_intb_ccp.dat")$V1

# plot power for range of interaction effects in model with 
# intervention a and dependent variable ccc
plotdat_inta_ccc <- data.frame(odds_ratio = exp(beta_range),
                               power = results_inta_ccc)
ggplot(plotdat_inta_ccc, aes(x = odds_ratio, y = power)) +
  geom_path() +
  geom_hline(yintercept = power_targeted, linetype = 2) +
  labs(x = "Effect size (odds ratio)", y = "Power") +
  theme_bw()

# plot power for range of interaction effects in model with 
# intervention a and dependent variable ccp
plotdat_inta_ccp <- data.frame(odds_ratio = exp(beta_range),
                               power = results_inta_ccp)
ggplot(plotdat_inta_ccp, aes(x = odds_ratio, y = power)) +
  geom_path() +
  geom_hline(yintercept = power_targeted, linetype = 2) +
  labs(x = "Effect size (odds ratio)", y = "Power") +
  theme_bw()

# # plot power for range of interaction effects in model with 
# intervention b and dependent variable ccc
plotdat_intb_ccc <- data.frame(odds_ratio = exp(beta_range),
                               power = results_intb_ccc)
ggplot(plotdat_intb_ccc, aes(x = odds_ratio, y = power)) +
  geom_path() +
  geom_hline(yintercept = power_targeted, linetype = 2) +
  labs(x = "Effect size (odds ratio)", y = "Power") +
  theme_bw()

# plot power for range of interaction effects in model with 
# intervention b and dependent variable ccp
plotdat_intb_ccp <- data.frame(odds_ratio = exp(beta_range),
                               power = results_intb_ccp)
ggplot(plotdat_intb_ccp, aes(x = odds_ratio, y = power)) +
  geom_path() +
  geom_hline(yintercept = power_targeted, linetype = 2) +
  labs(x = "Effect size (odds ratio)", y = "Power") +
  theme_bw()

# show odds ratio for interaction effect of intervention a and biosperic value on ccc
# that corresponds (nearly) to a power of 0.8 
plotdat_inta_ccc %>%
  mutate(deviation = abs(power - power_targeted)) %>%
  filter(deviation == min (deviation))
# show odds ratio for interaction effect of intervention a and biosperic value on ccp
# that corresponds (nearly) to a power of 0.8 
plotdat_inta_ccp %>%
  mutate(deviation = abs(power - power_targeted)) %>%
  filter(deviation == min (deviation))
# show odds ratio for interaction effect of intervention b and biosperic value on ccc
# that corresponds (nearly) to a power of 0.8 
plotdat_intb_ccc %>%
  mutate(deviation = abs(power - power_targeted)) %>%
  filter(deviation == min (deviation))
# show odds ratio for interaction effect of intervention b and biosperic value on ccp
# that corresponds (nearly) to a power of 0.8 
plotdat_intb_ccp %>%
  mutate(deviation = abs(power - power_targeted)) %>%
  filter(deviation == min (deviation))
