# sensitivity analyses for the interaction effects in study 1

# clear workspace
rm(list = ls())

# get functions defined in other script
source("R/functions.R")

# install pacman package if required
if (!("pacman" %in% installed.packages()))
  install.packages("pacman")
# load required packages
pacman::p_load(tidyverse, psych, MASS, broom)

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
da <- read.csv("data/study1_data.csv", header = T, dec = ",") %>%
  as_tibble() %>%
  # drop NA rows
  filter(!is.na(CCC) & !is.na(CCP))

# change variable types and recode factors
da <- da %>%
  mutate(
    across(c(Bildungsabschluss,
             Einkommen),
           ~ as.factor(.)),
    CCC = factor(
      CCC,
      levels = c(1, 2),
      labels = c("conventional", "organic")
    ),
    CCP = factor(
      CCP,
      levels = c(1, 2),
      labels = c("conventional", "organic")
    ),
    Gruppe_Bio = factor(
      Gruppe_Bio,
      levels = c(2, 1),
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
    ),
    # mean center BioV
    BioV = as.numeric(scale(BioV, scale = F))
  )

# Effect of intervention A on CCC ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but
# just want to check the results, go to the end of this script
# estimate model
mod <- glm(CCC ~ inf * BioV,
           data = da %>% filter(!is.na(inf)),
           family = binomial)
tidy(mod, exponentiate = T, conf.int = T)

# get power for range of betas
results <- get_power_logistic(
  data = da %>% filter(!is.na(inf)),
  cond = "inf",
  cov = "BioV",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["inf"],
  beta_cov = mod$coefficients["BioV"],
  beta_int = beta_range
)

# save results
write.table(
  apply(results, 1, mean),
  "results/logistic_power_inta_ccc.dat",
  row.names = FALSE,
  col.names = FALSE
)

# Effect of intervention A on CCP ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but
# just want to check the results, go to the end of this script

# estimate model
mod <- glm(CCP ~ inf * BioV,
           data = da %>% filter(!is.na(inf)),
           family = binomial)
tidy(mod, exponentiate = T, conf.int = T)

# get power for range of betas
results <- get_power_logistic(
  data = da %>% filter(!is.na(inf)),
  cond = "inf",
  cov = "BioV",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["inf"],
  beta_cov = mod$coefficients["BioV"],
  beta_int = beta_range
)

# save results
write.table(
  apply(results, 1, mean),
  "results/logistic_power_inta_ccp.dat",
  row.names = FALSE,
  col.names = FALSE
)


# Effect of intervention B on CCC ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but
# just want to check the results, go to the end of this script

# estimate model
mod <- glm(CCC ~ soc * BioV,
           data = da %>% filter(!is.na(soc)),
           family = binomial)
tidy(mod, exponentiate = T, conf.int = T)

# get power for range of betas
results <- get_power_logistic(
  data = da %>% filter(!is.na(soc)),
  cond = "soc",
  cov = "BioV",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["soc"],
  beta_cov = mod$coefficients["BioV"],
  beta_int = beta_range
)

# save results
write.table(
  apply(results, 1, mean),
  "results/logistic_power_intb_ccc.dat",
  row.names = FALSE,
  col.names = FALSE
)


# Effect of intervention B on CCP ----------------------------------------------------------------
# the simulations are time-consuming, if you do not want to run them but
# just want to check the results, go to the end of this script

# estimate model
mod <- glm(CCP ~ soc * BioV,
           data = da %>% filter(!is.na(soc)),
           family = binomial)
tidy(mod, exponentiate = T, conf.int = T)

# get power for range of betas
results <- get_power_logistic(
  data = da %>% filter(!is.na(soc)),
  cond = "soc",
  cov = "BioV",
  intercept = mod$coefficients["(Intercept)"],
  beta_cond = mod$coefficients["soc"],
  beta_cov = mod$coefficients["BioV"],
  beta_int = beta_range
)

# save results
write.table(
  apply(results, 1, mean),
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
