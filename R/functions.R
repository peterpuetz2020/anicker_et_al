get_power <- function(data, cond,  covariate, intercept, beta_cond, beta_cov, beta_int){
  length_list = length(beta_int)
  pmap_dbl(list(df = rep(list(data),length_list),
                         cond = rep(list(cond),length_list), 
                         covariate = rep(list(covariate),length_list),
                         intercept = rep(list(intercept),length_list),
                         beta_cond = rep(list(beta_cond),length_list), 
                         beta_cov = rep(list(beta_cov),length_list), 
                         beta_int = as.list(beta_range)), 
                    .f = function(df, cond, covariate, intercept, beta_cond, beta_cov, beta_int){
                      #Run mc replications
                      tmp_results <- replicate(n = mc, expr = {
                        cond = df[[cond]]
                        covariate = df[[covariate]]
                        # Specify the linear predictor for the logistic model
                        # effects for covariate is set to 0, but
                        # you can choose any other value
                        lp = intercept + beta_cond*cond + beta_cov*covariate + beta_int*cond*covariate
                        # The link function for logistic regression
                        link_lp = exp(lp)/(1 + exp(lp)) 
                        # generate y on a binary scale including random variation
                        y = (runif(length(lp)) < link_lp) 
                        # Run a logistic regression
                        log.int = glm(y ~ cond*covariate, family=binomial)
                        # store if p-value of interaction is lower than alpha
                        summary(log.int)$coefficients[4,4] < alpha
                      })
                      #Compute the power, i.e. share of tests with p < alpha
                      mean(tmp_results)
                    })
}

