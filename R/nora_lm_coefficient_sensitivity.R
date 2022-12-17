# Study 2 R Code

# clear workspace
rm(list=ls())

# install pacman package if required
my_packages <- c("pacman")                                        
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
if(length(not_installed)) install.packages(not_installed) 
# load required packages
pacman::p_load(tidyverse, psych, MASS, readxl, effects)

# set number of simulations (the more you choose, the more precise are the results)
mc = 500

# read in data
da <- read_excel("Studie2_Sensitivitätsanalysen.xlsx", 
                 sheet = "Studie2_Sensitivitätsanalysen")


#Range of interaction effects that should be checked, i.e. for which
# power will be calculated
beta_range <- seq(0.1, 0.15, length = 50)
# choose arbitrary effects
beta_cond <- runif(1, 0, 1)
intercept <- runif(1, 0, 1)
beta_cov <- runif(1, 0, 1)

# inta ----------------------------------------------------------------
# restrict data to first intervention
da_intA <-
  da %>% filter(!is.na(KGA_IntA)) %>% 
  # center BioV variable
  mutate(MW_Bio = scale(MW_Bio, center = TRUE, scale = FALSE))

# fit model
moda <- lm(Rebound ~ KGA_IntA*MW_Bio, data = da_intA)
sum_moda <- summary(moda)
sum_moda
# interpretation of interaction effect
pred_data <- data.frame(yhat = moda$fitted.values, 
                        x = da_intA$MW_Bio,
                        f = as.factor(da_intA$KGA_IntA))

ggplot(pred_data, aes(x = x, y = yhat, group = f,
                    color = f)) + 
  geom_line() 
# the intervention effect on the rebound decreases on average by .026 units
# if the biospheric score increases by one

# in terms of standard deviations of rebound:
moda_y_sc <- lm(scale(Rebound) ~ KGA_IntA*MW_Bio, data = da_intA)
summary(moda_y_sc)
# the intervention effect on the rebound decreases on average by .042 standard deviations
# if the biospheric score increases by one

# in terms of standard deviations of rebound and biosperic values:
moda_y_x_sc <- lm(scale(Rebound) ~ KGA_IntA*scale(MW_Bio), data = da_intA)
summary(moda_y_x_sc)
# the intervention effect on the rebound decreases on average by .053 standard deviations
# if the biospheric score increases by one standard deviation

#Sample size
nn <- nrow(da_intA)

#Set random seed
set.seed(6)

results <- sapply(beta_range, function(beta_int){
  #Run 1000 replications
  tmp_results <- replicate(n = mc, expr = {
    #Randomly draw condition from Bernoulli distribution
    #cond = rbinom(nn, 1, p_exp) 
    cond = da_intA$KGA_IntA
    #Randomly draw covariate from standard Normal distribution
    #covariate = rnorm(nn) 
    covariate <- da_intA$MW_Bio
    #Specify the linear model. If a main effect for the covariate is expected,
    #replace the beta value of 0 with another value.
    y = intercept + beta_cond*cond + beta_cov*covariate + 
      beta_int*cond*covariate + rnorm(nn,0,sum_moda$sigma)
    #summary(lm(cond*covariate~cond+covariate))
    #print(paste("y=0 in control group:", mean(y[cond==0])))
    #Run regression
    int = lm(y ~ cond*covariate)
    #two-tailed test to see if the interaction is significant
    #print(paste("or for interaction:", exp(summary(log.int)$coefficients[4,1])))
    #mean(y[cond==0])
    #exp(summary(log.int)$coefficients[4,1])
    summary(int)$coefficients[4,4] < 0.05
  })
  #Compute the power for each coefficient
  #sum(tmp_results)/length(tmp_results)
  mean(tmp_results)
  #mean(mean(y[cond==0]))
})
write.table(results, "power_inta_rebound.dat", row.names = FALSE, col.names = FALSE)
#} else {
results <- read.table("power_inta_rebound.dat")$V1
#}
results

plotdat <- data.frame(coef = beta_range, power = results)
ggplot(plotdat, aes(x = coef, y = power))+
  geom_path()+
  geom_hline(yintercept = .8, linetype = 2)+
  #Add a horizontal line at the point where power exceeds the conventional
  #threshold of .8, and a label for this point
  geom_vline(xintercept = beta_range[which(diff(results > .8)!=0)])+
  annotate("text", x = 0.3, y = .75, label = round(beta_range[which(diff(results > .8)!=0)], 2))+
  labs(x = "Coefficient Estimate", y = "Power")+
  theme_bw()

res <- plotdat %>% 
  mutate(deviation = abs(power-0.8)) %>% 
  filter(deviation == min (deviation))
res
# in terms of standard deviations of Rebound
res$coef / sd(da_intA$Rebound)
# in terms of standard deviations of Rebound and BioV
res$coef / sd(da_intA$Rebound) *sd(da_intA$MW_Bio)


# could also be done in Gpower: t-test - linear bivariate regression - two groups,
# difference between groups and this information
# for the standard deviations sigma_x1 and sigma_x2
da_intA %>% group_by(KGA_IntA) %>% 
  summarise(n=n(),
            sd(MW_Bio))
# and this value for the standard deviation residual sigma
sum_moda$sigma


# intb ----------------------------------------------------------------
# restrict data to other intervention
da_intB <-
  da %>% filter(!is.na(KGB_IntB)) %>% 
  # center BioV variable
  mutate(MW_Bio = scale(MW_Bio, center = TRUE, scale = FALSE))

# fit model
modb <- lm(Rebound ~ KGB_IntB*MW_Bio, data = da_intB)
sum_modb <- summary(modb)

#Sample size
nn <- nrow(da_intB)

#Set random seed
set.seed(6)

results <- sapply(beta_range, function(beta_int){
  #Run 1000 replications
  tmp_results <- replicate(n = mc, expr = {
    #Randomly draw condition from Bernoulli distribution
    #cond = rbinom(nn, 1, p_exp) 
    cond = da_intB$KGB_IntB
    #Randomly draw covariate from standard Normal distribution
    #covariate = rnorm(nn) 
    covariate <- da_intB$MW_Bio
    #Specify the linear model. If a main effect for the covariate is expected,
    #replace the beta value of 0 with another value.
    y = intercept + beta_cond*cond + beta_cov*covariate + 
      beta_int*cond*covariate + rnorm(nn,0,sum_modb$sigma)
    #summary(lm(cond*covariate~cond+covariate))
    #print(paste("y=0 in control group:", mean(y[cond==0])))
    #Run regression
    int = lm(y ~ cond*covariate)
    #two-tailed test to see if the interaction is significant
    #print(paste("or for interaction:", exp(summary(log.int)$coefficients[4,1])))
    #mean(y[cond==0])
    #exp(summary(log.int)$coefficients[4,1])
    summary(int)$coefficients[4,4] < 0.05
  })
  #Compute the power for each coefficient
  #sum(tmp_results)/length(tmp_results)
  mean(tmp_results)
  #mean(mean(y[cond==0]))
})
write.table(results, "power_intb_rebound.dat", row.names = FALSE, col.names = FALSE)
#} else {
results <- read.table("power_intb_rebound.dat")$V1
#}
results

plotdat <- data.frame(coef = beta_range, power = results)
ggplot(plotdat, aes(x = coef, y = power))+
  geom_path()+
  geom_hline(yintercept = .8, linetype = 2)+
  #Add a horizontal line at the point where power exceeds the conventional
  #threshold of .8, and a label for this point
  geom_vline(xintercept = beta_range[which(diff(results > .8)!=0)])+
  annotate("text", x = 0.3, y = .75, label = round(beta_range[which(diff(results > .8)!=0)], 2))+
  labs(x = "Coefficient Estimate", y = "Power")+
  theme_bw()

plotdat %>% 
  mutate(deviation = abs(power-0.8)) %>% 
  filter(deviation == min (deviation))

# could also be done in Gpower: t-test - linear bivariate regression - two groups,
# difference between groups and this information
# for the N2/N1 ratio and standard deviations sigma_x1 and sigma_x2
da_intB %>% group_by(KGB_IntB) %>% 
  summarise(n=n(),
            sd(MW_Bio))
# and this value for the standard deviation residual sigma
sum_modb$sigma



# summarise results -------------------------------------------------------
# load required packages
results_inta_rebound <- read.table("power_inta_rebound.dat")$V1
results_intb_rebound <- read.table("power_intb_rebound.dat")$V1

plotdat_inta_rebound <- data.frame(coef = beta_range, power = results_inta_rebound)
ggplot(plotdat_inta_rebound, aes(x = coef, y = power))+
  geom_path()+
  geom_hline(yintercept = .8, linetype = 2)+
  #Add a horizontal line at the point where power exceeds the conventional
  #threshold of .8, and a label for this point
  geom_vline(xintercept = beta_range[which(
    abs(results_inta_rebound-0.8)==min(abs(results_inta_rebound-0.8)))])+
  labs(x = "Coefficient Estimate", y = "Power")+
  theme_bw()

plotdat_intb_rebound <- data.frame(coef = beta_range, power = results_intb_rebound)
ggplot(plotdat_intb_rebound, aes(x = coef, y = power))+
  geom_path()+
  geom_hline(yintercept = .8, linetype = 2)+
  #Add a horizontal line at the point where power exceeds the conventional
  #threshold of .8, and a label for this point
  geom_vline(xintercept = beta_range[which(
    abs(results_intb_rebound-0.8)==min(abs(results_intb_rebound-0.8)))])+
  labs(x = "Coefficient Estimate", y = "Power")+
  theme_bw()

plotdat_inta_rebound %>% 
  mutate(deviation = abs(power-0.8)) %>% 
  filter(deviation == min (deviation))
plotdat_intb_rebound %>% 
  mutate(deviation = abs(power-0.8)) %>% 
  filter(deviation == min (deviation))
