# define function to get power for a range of betas for the interaction
# in a logistic regression with one binary intervention variable and one continuous covariate
# arguments of the function:
# "data" is the data frame containing the variables in the model
# "cond" is the variable name in "data" indicating the (binary) intervention
# "covariate" is the variable name in "data" indicating the continuous covariate
# "intercept" is the intercept in the logistic regression model fit by glm
# "beta_cond" is the coefficient associated with "cond" in the logistic regression model fit by glm
# "beta_cov" is the coefficient associated with "covariate" in the logistic regression model fit by glm
# "beta_int" is a range of coefficients associated for the interaction for which the power should be calculated

get_power_logistic <-
  function(data,
           cond,
           covariate,
           intercept,
           beta_cond,
           beta_cov,
           beta_int) {
    # compute length of range of betas
    length_list = length(beta_range)
    # replicate mc times, using the following inputs,...
    replicate(mc, pmap_dbl(
      list(
        df = rep(list(data), length_list),
        cond = rep(list(cond), length_list),
        covariate = rep(list(covariate), length_list),
        intercept = rep(list(intercept), length_list),
        beta_cond = rep(list(beta_cond), length_list),
        beta_cov = rep(list(beta_cov), length_list),
        beta_int = as.list(beta_range)
      ),
      # this function
      .f = function(df,
                    cond,
                    covariate,
                    intercept,
                    beta_cond,
                    beta_cov,
                    beta_int) {
        # run mc replications
        # access intervention and covariate
        cond = df[[cond]]
        covariate = df[[covariate]]
        # specify the linear predictor for the logistic model
        lp = intercept + beta_cond * cond + beta_cov * covariate + beta_int *
          cond * covariate
        # apply the link function for logistic regression
        link_lp = exp(lp) / (1 + exp(lp))
        # generate y on a binary scale including random variation
        y = (runif(length(lp)) < link_lp)
        # run a logistic regression
        log.int = glm(y ~ cond * covariate, family = binomial)
        # store if p-value of interaction is lower than alpha
        return(summary(log.int)$coefficients[4, 4] < alpha)
      }
    ))
  }


# define function to get power for a range of betas for the interaction
# in a linear regression with one binary intervention variable and one continuous covariate
# arguments of the function:
# "data" is the data frame containing the variables in the model
# "cond" is the variable name in "data" indicating the (binary) intervention
# "covariate" is the variable name in "data" indicating the continuous covariate
# "intercept" is the intercept in the regression model fit by lm
# "beta_cond" is the coefficient associated with "cond" in the regression model fit by lm
# "beta_cov" is the coefficient associated with "covariate" in the regression model fit by lm
# "beta_int" is a range of coefficients associated for the interaction for which the power should be calculated
# "res_std_error" is the residual standard error in the regression model fit by lm

get_power_lm <-
  function(data,
           cond,
           covariate,
           intercept,
           beta_cond,
           beta_cov,
           beta_int,
           res_std_error) {
    # compute length of range of betas
    length_list = length(beta_range)
    # replicate mc times, using the following inputs,...
    replicate(mc, pmap_dbl(
      list(
        df = rep(list(data), length_list),
        cond = rep(list(cond), length_list),
        covariate = rep(list(covariate), length_list),
        intercept = rep(list(intercept), length_list),
        beta_cond = rep(list(beta_cond), length_list),
        beta_cov = rep(list(beta_cov), length_list),
        beta_int = as.list(beta_range),
        res_std_error =  rep(list(res_std_error), length_list)
      ),
      # this function
      .f = function(df,
                    cond,
                    covariate,
                    intercept,
                    beta_cond,
                    beta_cov,
                    beta_int,
                    res_std_error) {
        # run mc replications
        cond = df[[cond]]
        covariate = df[[covariate]]
        # specify the linear predictor for the linear model
        lp = intercept + beta_cond * cond + beta_cov * covariate + beta_int *
          cond * covariate
        # add noise
        y = lp + rnorm(length(lp), 0, res_std_error)
        # run a regression
        log.int = lm(y ~ cond * covariate)
        # store if p-value of interaction is lower than alpha
        summary(log.int)$coefficients[4, 4] < alpha
      }
    ))
  }

# functions for transforming log odds
LogOdds_Prob <- function(x) {
  exp(x) / (1 + exp(x))
}
Prob_LogOdds <- function(x) {
  log(x / (1 - x))
}