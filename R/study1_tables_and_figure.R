# code to replicate the tables from study 2

# clear workspace
rm(list = ls())

# in case not installed, install pacman package
if (!"pacman" %in% rownames(installed.packages()))
  install.packages("pacman")
# install (if required) and load packages
pacman::p_load(tidyverse, MASS, broom, readxl, scales)

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
    ),
    # mean center BioV
    BioV = as.numeric(scale(BioV, scale = F))
  )

# figure 1
da %>%
  mutate(
    Group = case_when(
      (Bedingung == "KG A" | Bedingung == "KG B") ~ "CG",
      (Bedingung == "Intervention A") ~ "Int A",
      (Bedingung == "Intervention B") ~ "Int B"
    ),
    CCP_graph = ifelse(CCP == "organic", 1, 0),
    CCC_graph = ifelse(CCC == "organic", 1, 0)
  ) %>%
  group_by(Group) %>%
  summarise(m_ccp = mean(CCP_graph),
            m_ccc = mean(CCC_graph)) %>%
  gather(choice, value,-Group) %>%
  mutate(perc = scales::percent(value,
                                accuracy = .2,
                                trim = FALSE)) %>%
  ggplot(aes(x = Group, y = value, fill = Group)) +
  geom_bar(stat = "identity") +
  theme_minimal()  +
  scale_y_continuous(labels = percent, limits = c(0, 0.8)) +
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9")) +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  geom_text(aes(label = perc), nudge_y  = .025)  +
  facet_grid( ~ choice,
              labeller = labeller(
                choice = c(m_ccp = "Coffee choice private",
                           m_ccc = "Coffee choice company")
              ))

ggsave(
  "results/figure_1.png",
  width = 20,
  height = 15,
  units = "cm",
  bg = "white"
)


# table 1
table(da$Gruppe_Bio, da$Bedingung)


# table 2
mod_table_2a <- glm(CCP ~ BioV,
                    data = da, family = binomial())
tidy(mod_table_2a, exponentiate = TRUE, conf.int = T)

mod_table_2b <- glm(CCC ~ BioV,
                    data = da, family = binomial())
tidy(mod_table_2b, exponentiate = TRUE, conf.int = T)


# table 3:
mod_table_3a <- glm(CCP ~ inf,
                    data = da, family = "binomial")
tidy(mod_table_3a, exponentiate = TRUE, conf.int = T)

mod_table_3b <- glm(CCC ~ inf,
                    data = da, family = "binomial")
tidy(mod_table_3b, exponentiate = TRUE, conf.int = T)

mod_table_3c <- glm(CCP ~ soc,
                    data = da, family = "binomial")
tidy(mod_table_3c, exponentiate = TRUE, conf.int = T)

mod_table_3d <- glm(CCC ~ soc,
                    data = da, family = "binomial")
tidy(mod_table_3d, exponentiate = TRUE, conf.int = T)


# table 4:
mod_table_4a <- glm(CCP ~ inf * BioV,
                    data = da, family = "binomial")
tidy(mod_table_4a, exponentiate = TRUE, conf.int = T)

mod_table_4b <- glm(CCC ~ inf * BioV,
                    data = da, family = "binomial")
tidy(mod_table_4b, exponentiate = TRUE, conf.int = T)

mod_table_4c <- glm(CCP ~ soc * BioV,
                    data = da, family = "binomial")
tidy(mod_table_4c, exponentiate = TRUE, conf.int = T)

mod_table_4d <- glm(CCC ~ soc * BioV,
                    data = da, family = "binomial")
tidy(mod_table_4d, exponentiate = TRUE, conf.int = T)
