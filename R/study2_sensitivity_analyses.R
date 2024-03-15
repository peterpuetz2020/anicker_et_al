# sensitivity analyses for the interaction effects in study 2

# clear workspace
rm(list = ls())

# get functions defined in other script
source("R/functions.R")

# install pacman package if required
if (!("pacman" %in% installed.packages()))
  install.packages("pacman")
# load required packages
pacman::p_load(tidyverse, psych, MASS, broom, readxl)

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
da <- read_csv("data/study2_data.csv")

# change variable types and recode factors
da <- da %>%
  mutate(across(c(Bildungsabschluss,
                  Einkommen),
                ~ as.factor(.))) %>%
  mutate(
    BioV = as.numeric(scale(BioV, scale = F)),
    Gruppe_Bio = factor(
      Gruppe_Bio,
      levels = c(0, 1),
      labels = c("low", "high")
    ),
    gender = factor(
      Geschlecht,
      levels = c(1, 2, 3),
      labels = c("female",
                 "male",
                 "diverse")
    ),
    # create separate intervention variables
    inf = case_when(
      Bedingung == "KG A" ~ 0,
      Bedingung == "Intervention A" ~ 1,
      Bedingung == "KG B" ~ NA,
      Bedingung == "Intervention B" ~ NA
    ),
    soc = case_when(
      Bedingung == "KG B" ~ 0,
      Bedingung == "Intervention B" ~ 1,
      Bedingung == "KG A" ~ NA,
      Bedingung == "Intervention A" ~ NA
    )
  )


# Effect of intervention A on Rebound ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but
# just want to check the results, go to the end of this script

# estimate model
mod <- lm(Rebound ~ inf * BioV, data = da %>% filter(!is.na(inf)))
sum_mod <- summary(mod)
tidy(mod, confint = T)

# get power for range of betas
results <- get_power_lm(
  data = da %>% filter(!is.na(inf)),
  cond = "inf",
  cov = "BioV",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["inf"],
  beta_cov = mod$coefficients["BioV"],
  beta_int = beta_range,
  res_std_error = sum_mod$sigma
)

# save results
write.table(
  apply(results, 1, mean),
  "results/lm_power_inta_rebound.dat",
  row.names = FALSE,
  col.names = FALSE
)

# Effect of intervention B on Rebound ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but
# just want to check the results, go to the end of this script

# estimate model
mod <- lm(Rebound ~ soc * BioV, data = da %>% filter(!is.na(soc)))
sum_mod <- summary(mod)
tidy(mod, confint = T)

# get power for range of betas
results <- get_power_lm(
  data = da %>% filter(!is.na(soc)),
  cond = "soc",
  cov = "BioV",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["soc"],
  beta_cov = mod$coefficients["BioV"],
  beta_int = beta_range,
  res_std_error = sum_mod$sigma
)

# save results
write.table(
  apply(results, 1, mean),
  "results/lm_power_intb_rebound.dat",
  row.names = FALSE,
  col.names = FALSE
)

# summarise results -------------------------------------------------------
# read in results data
results_inta_rebound <-
  read.table("results/lm_power_inta_rebound.dat")$V1
results_intb_rebound <-
  read.table("results/lm_power_intb_rebound.dat")$V1

# plot power for range of interaction effects in model with
# intervention a and dependent variable rebound
plotdat_inta_rebound <- data.frame(coefficient = beta_range,
                                   power = results_inta_rebound)
ggplot(plotdat_inta_rebound, aes(x = coefficient, y = power)) +
  geom_path() +
  geom_hline(yintercept = power_targeted, linetype = 2) +
  labs(x = "Effect size (coefficient)", y = "Power") +
  theme_bw()

# plot power for range of interaction effects in model with
# intervention b and dependent variable rebound
plotdat_intb_rebound <- data.frame(coefficient = beta_range,
                                   power = results_intb_rebound)
ggplot(plotdat_intb_rebound, aes(x = coefficient, y = power)) +
  geom_path() +
  geom_hline(yintercept = power_targeted, linetype = 2) +
  labs(x = "Effect size (coefficient)", y = "Power") +
  theme_bw()

# show odds ratio for interaction effect of intervention a and biosperic value
# on rebound that corresponds (nearly) to a power of 0.8
plotdat_inta_rebound %>%
  mutate(deviation = abs(power - power_targeted)) %>%
  filter(deviation == min (deviation))
# show odds ratio for interaction effect of intervention b and biosperic value
# on rebound that corresponds (nearly) to a power of 0.8
plotdat_intb_rebound %>%
  mutate(deviation = abs(power - power_targeted)) %>%
  filter(deviation == min (deviation))
