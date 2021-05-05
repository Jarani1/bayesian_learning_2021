library("ggplot2")
library("mvtnorm")


ebay = read.table("eBayNumberOfBidderData.dat",header = TRUE)

# 2a) MLE with glm

model = glm(nBids ~ . - Const , data = ebay, family = "poisson") #remove const from input
summary(model)
model$coefficients
###########################
# 2b)
X = as.matrix(ebaydata[,2:length(ebay)])
y = y = ebay[,1]
#prior zellner's g-prior
nPara = dim(X)[2]
u0 = rep(0,nPara)
priorsigma = 100 * (solve(t(X)%*%X))
n = length(X[,1])

###############################

#assume post approx multivariate normal
#get the prior and likelihood dists

logPP <- function(betavec, X, y, mean, sigma){
  betavec = matrix(betavec, 1,nPara)
  prior = dmvnorm(betavec, mean, sigma = sigma)
  loglike = sum(y *  X  %*% t(betavec) -
                  exp(X %*% t(betavec)) -
                  log(factorial(y)))
  
  #just stole this 
  if (is.infinite(loglike)){
    loglike = -20000 #dont go to infinity pls
  }
  
  return(prior + loglike)
}

initvals =integer(nPara)
optimres = optim(initvals, logPP,  gr = NULL, X, y, mean, priorsigma, method = "BFGS", 
                 control=list(fnscale=-1), hessian=TRUE)
#misery with the dimensions

beta_hat = optimres$par
j2inv = -solve(optimres$hessian)

# compare with glm
beta_hat
model$coefficients
# hell yeah baby, almost identical 

# 2b done

###################


#MCs for monte carlo simulations
metropolis <- function(logPostFunc, c, sigma, MCs, ...){
  accepts = 0
  thetas = matrix(rep(0, nPara), MCs, nPara)
  for(i in 2:MCs){
    thetasprev = thetas[i-1,]
    thetasdraw = rmvnorm(1, thetasprev, c * sigma)
    a = min(1, exp(logPostFunc(thetasdraw, ...)
                   - logPostFunc(thetasprev, ...)))
    if(a != 1){
      thetas[i,] = thetas[i-1,] #reject monte carlo draw
    }else{
      accepts = accepts + 1
      thetas[i,] = thetasdraw  #accept --||--
    }
  }
  print(paste0("Acceptratio: ", accepts/MCs))
  return(thetas)
}
# mean and sigma is for logpp prior, likelihood is calced
# with data given the theta vec we draw, j2in is jacobian
# for the multivariate draw
# off the cigar diag(diag(j2inv))
# diag to extract then diag again to become a matrix
rwm = metropolis(logPP, 3, diag(diag(j2inv)), 1000, X, y, mean, priorsigma)

#after about 200 draws I'm just rejecting and we've converged 

plot(rwm[,1], type = "s")


# fake error calculation below 
rwm[1000,] - beta_hat
rwm[1000,] - model$coefficients
# they are close, nice 



#time for cool plots
#get names
mci = 1:1000
mcmcdf <- as.data.frame(rwm)
names(mcmcdf) = names(ebaydata[-1])
#add mcsims axis
mcmcdf = cbind(mci, mcmcdf)


#tried to do a cool loop and save and etc cool style,
#however, that ended up taking waaaaaaaaay 2 much time 

ggplot(mcmcdf, aes(x = mci, y = Const)) + labs(x = "Monte Carlo Iterations", y = "Intercept") +  geom_step() 


ggplot(mcmcdf, aes(x = mci, y = Sealed)) + labs(x = "Monte Carlo Iterations") +  geom_step() 

# we can see that as the MC iterations grow, our coefs start to converge, we reject new states. 


# 2c) done

############################
# 2d)
############################
## listing in lab desc

listing = matrix(c(1, 1, 1, 1, 0, 0, 0, 1, 0.5), 1,9)
posterior = rwm[1000,]
lambda = exp(posterior %*% t(listing)  )
nobidsprob = ppois(0,lambda)
print(paste0("probability that listing gets 0 bids is: ", format(round(nobidsprob, 2)) ,"%"))

### another made up listing
listing = matrix(c(1, 1, 1, 1, 0, 0, 0, 0, 0), 1,9)
posterior = rwm[1000,]
lambda = exp(posterior %*% t(listing))
nobidsprob = ppois(0,lambda)
print(paste0("probability that listing gets 0 bids is: ", format(round(nobidsprob, 2)) ,"%"))



bids = 0:10
coindf <- data.frame(
  bids = bids,
  density = dpois(bids,lambda)
)

ggplot(coindf, aes(x = factor(bids), density, group = 1))+
  geom_line() +
  labs(x = "Bids", y = "Probability")


# plots look good, done

