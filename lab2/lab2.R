library("ggplot2")
library("mvtnorm")
library("LaplacesDemon")
library("reshape2")
library("bayestestR")


tempdata = read.table("TempLinkoping.txt", col.names = c("time","temp"), header = TRUE)
tempdata = lapply(tempdata, as.double) #data type was string which made everything shit the bed
# 1. lin & poly reg
#plot data

df = data.frame(
  x = 1:length(tempdata$temp),
  temp = tempdata$temp
)

ggplot(df, aes(x, temp)) + geom_point()

# a) conjugate prior for linear regression

# joint prior: beta, sigma
# sigma ~ inv-chi2(v0, sigma0)
# beta | sigma2 ~ N(u0, sigma*inv(omega0))

#prior values
u0 = t(c(-12,105,-100)) # (1x3) for b0,b1,b2 in the quadratic regression model posted in the lab desc (I THINK)
omega0 = diag(x = 0.01, 3,3) 
v0 = 6
sigma0 = 0.25

# draw priors
simpriors <- function(draws){
  sigmas0 = rinvchisq(draws, v0, sigma0)
  betas0 = matrix(0, draws, 3)
  i = 0
  for(sigma0 in sigmas0){
    betas0[i,] = rmvnorm(1, u0, sigma0 * solve(omega0)) #sovle to avoid inf values of inverse
    i = i + 1
  }
  df = data.frame(
    sigmas0 = sigmas0,
    betas0 = betas0
  )
  return(df)
}

# call function and place priors in their respective categories
priors = simpriors(25)
betapriors = priors[,2:4]
sigmapriors = priors$sigmas0



# create X matrix ones, X, X^2
n = length(tempdata$temp)
X = matrix(c(rep(1,n), tempdata$time, tempdata$time^2), nrow = n, ncol = 3)


# dataframe that will hold models
testdf <- data.frame(
  day = 1:n,
  temp = tempdata$temp
)



# model calculation and storage
c = 0
for(i in 1:nrow(betapriors)){
  #regmodel = BETA[1] + (BETA[2] * X) + (BETA[3] * X^2) + eps 
  BETA = as.matrix(betapriors[i,])
  eps = rep(rnorm(1, 0, sigmapriors[i]), n)
  regmodel = X %*% t(BETA) + eps
  testdf[paste0("model", c)] = drop(regmodel)
  c = c + 1
}

# visualization
mtest <- melt(testdf, id.vars=c("day","temp"), variable.name = "models")

ggplot(mtest, aes(x=day, y=value,group=models,
                  colour=models)) + geom_line() + geom_point(aes(day, temp))


# looks good
y = tempdata$temp
b_hat = solve(t(X)%*%X) %*% t(X)%*%y
u_n = solve(t(X)%*%X + omega0) %*% (t(X)%*%(X)%*%b_hat + omega0%*%t(u0))
omega_n = t(X)%*%X + omega0
v_n = v0 + n
vnsigma_n = v0%*%sigma0 + (t(y)%*%y + u0%*%omega0%*%t(u0) - t(u_n)%*%omega_n%*%u_n)
sigma_n = vnsigma_n/v_n


#draw posts
drawposts <- function(draws){
  sigmas_p = rinvchisq(draws, v_n, sigma_n)
  betas_p = matrix(0, draws, 3)
  i = 0
  for(sigma in sigmas_p){
    betas_p[i,] = rmvnorm(1, u_n, sigma * solve(omega_n)) #sovle to avoid inf values of inverse
    i = i + 1
  }
  df = data.frame(
    sigmas_p = sigmas_p,
    betas_p = betas_p
  )
  return(df)
}
post_draws = drawposts(100)

# histogram of each marginal
hist(post_draws$sigmas_p)
hist(post_draws$betas_p.1)
hist(post_draws$betas_p.2)
hist(post_draws$betas_p.3)

# scatterplot
# for each point in time take median and get low/high
med = integer(365)
high = integer(365)
low = integer(365)
i = 0
for(time in tempdata$time){
  pred = post_draws$betas_p.1 + post_draws$betas_p.2 %*% t(time) + post_draws$betas_p.3 %*% t(time^2) 
  interval = ci(pred, method = "ETI", ci = 0.95)
  low[i] = interval$CI_low
  high[i] = interval$CI_high
  med[i] = median(pred)
  i = i + 1
}  

# data frame and plot 

postdf = data.frame(
  days = 1:365,
  median = med,
  low = low,
  high = high,
  temps = tempdata$temp
)

ggplot(postdf, aes(days, temps)) + 
  geom_point() +
  geom_line(data = postdf, aes(days, median), col = "red", size = 1) +
  geom_line(data = postdf, aes(days, low), col = "blue", size = 1) +
  geom_line(data = postdf, aes(days, high), col = "blue", size = 1)


# 1 c) highest expected time
# get max day for each model, plot? 
