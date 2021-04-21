
library("LaplacesDemon")
library("mvtnorm")
library("reshape2")
library("ggplot2")
library("bayestestR")
library("dplyr")

womendata = read.table("WomenWork.dat", header = TRUE)


y = as.vector(womendata[,1])
X = as.matrix(womendata[,2:length(names(womendata))])
tau = 10

Npar = dim(X)[2]
# Setting up the prior
mu <- as.matrix(rep(0,Npar)) # Prior mean vector
sigma <- tau^2*diag(Npar) # Prior covariance matrix


# remember prior * likelihood prop-> post 
# given that we're dealing with logs, we replace multiplication with additions and all other things respectively

logPost <- function(betavec, y, X, m0, sigma){
  loglike = sum( X %*% betavec * y 
                 -log(1 + exp(X %*% betavec)))
  #just stole this 
  if (abs(loglike) == Inf){
    loglike = -20000; # Likelihood is not finite, don't move to any infinity type shit (-inf, inf)
  } 
  
  logprior = dmvnorm(betavec, m0, sigma, log = TRUE)
  
  #logpost
  
  return(loglike + logprior)
}

# Select the initial values for beta
initVal <- matrix(0,Npar,1)


# The argument control is a list of options to the optimizer optim, where fnscale=-1 means that we minimize 
# the negative log posterior. Hence, we maximize the log posterior.  
OptimRes <- optim(initVal, logPost, gr=NULL, y, X, mu, sigma, method="BFGS", control=list(fnscale=-1),hessian=TRUE)

# with these two values we can now draw the post dist for our beta vec
B_hat = OptimRes$par
J_inv = -solve(OptimRes$hessian)

# check that calculations made sense compared to built in logistic regression
lm = glm(Work ~ ., data = womendata, family = binomial)
lm$coefficients
B_hat
# Conclusion: very similar



# get post betas and calc interval for nsmallchild feature
post_betas = rmvnorm(1000, B_hat, J_inv)
nsmall_dist = post_betas[,7]
approx_interval = ci(nsmall_dist, method = "ETI", ci = 0.95)

approxdf <- data.frame(
  low = approx_interval$CI_low,
  high = approx_interval$CI_high,
  postdraws = nsmall_dist
)

ggplot(approxdf, aes(postdraws)) + 
  geom_density(size = 1) +
  geom_vline(aes(xintercept = low), size = 1,
             linetype = "dashed", col = "red") +
  geom_vline(aes(xintercept = high), size = 1,
             linetype = "dashed", col = "red")

# given its magnitude in relation to other features of correlation we conclude that it has high importance
# ex: a coef whose value would end up close around 0 wouldn't impact the target that much during prediction

###################################################################
#that's A done 
###################################################################



