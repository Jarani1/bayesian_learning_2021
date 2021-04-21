
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

# so annoying that they just don't give us a vector
# have to play detecto-magnifying-glass to translate the shitty text to input data

woman = matrix(c(1, 10, 8, 10, 1, 40, 1, 1))

# for each post_beta calculte probability of employment, plot dist 
woman_dist = exp(post_betas %*% woman) / (1 + exp(post_betas %*% woman))


womandf <- data.frame(
  dist = woman_dist
)
ggplot(womandf, aes(x = dist)) +
  ggtitle("posterior predictive distribution") +
  labs(x = "probability", y = "density") +
  geom_density(size = 1) +
  geom_vline(aes(xintercept = 0.5), size = 1,
             linetype = "dashed", col = "blue")

# in terms of getting employed it doesn't look to good 

###################################################################
#that's B done 
###################################################################

# This is like flipping a coin where p = 0.5 but life isn't a fair coin so p will be the mean of the dist
p = mean(woman_dist)

bdist = c(0,1,2,3,4,5,6,7,8) # prob that 0 works 1 works, 2 works, ..., 8 works.
binomial = dbinom(bdist,8, p)

bdf = data.frame(
  x = 1:9,
  y = binomial
)
ggplot(bdf, aes(x,y)) + geom_point() + labs(x = "Women", y = "probability of employement")

###################################################################
#that's C done 
###################################################################