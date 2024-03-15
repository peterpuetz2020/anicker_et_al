# code to replicate the tables from study 2

# clear workspace
rm(list = ls())

# in case not installed, install pacman package
if (!"pacman" %in% rownames(installed.packages()))
  install.packages("pacman")
# install (if required) and load packages
pacman::p_load(tidyverse, MASS, broom, readxl)


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
    inf = factor(
      Bedingung,
      levels = c("KG A",
                 "Intervention A"),
      exclude = c("Intervention B",
                  "KG B"),
      labels = c("no",
                 "yes")
    ),
    soc = factor(
      Bedingung,
      levels = c("KG B",
                 "Intervention B"),
      exclude = c("Intervention A",
                  "KG A"),
      labels = c("no",
                 "yes")
    )
  )


# table 5
table(da$Gruppe_Bio, da$Bedingung)


# table 6
# pre
pre <- da %>%
  dplyr::select(contains("_pr")) %>%
  summarise_all(list(
    m = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE)
  )) %>%
  t()

# post
post <- da %>%
  dplyr::select(contains("_post")) %>%
  summarise_all(list(
    m = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE)
  )) %>%
  t()


# table 7
da %>%
  group_by(Bedingung) %>%
  summarise(m = mean(Rebound, na.rm = TRUE),
            sd = sd(Rebound, na.rm = TRUE))


# table 8
mod_table_8a <- lm(Rebound ~ BioV, data = da)
tidy(mod_table_8a,  conf.int = T)

mod_table_8b <- lm(Rebound ~ inf, data = da)
tidy(mod_table_8b,  conf.int = T)

mod_table_8c <- lm(Rebound ~ soc, data = da)
tidy(mod_table_8c, conf.int = T)


# table 9
mod_table_9a <- lm(Rebound ~ inf * BioV, data = da)
tidy(mod_table_9a,  conf.int = T)

mod_table_9b <- lm(Rebound ~ soc * BioV, data = da)
tidy(mod_table_9b, conf.int = T)
