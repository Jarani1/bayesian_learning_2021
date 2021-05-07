#lab4
library("rstan")
library("ggplot2")
library("reshape2")
#init vals
u = 20
sigma2 = 4
t = 200

# AR 
arfunc <- function(u,fi,runs){
  xt = integer(runs)
  xt[1] = u
  for(i in 1:runs){
    xt[i] = u +  fi * (max(1,xt[i-1]) - u) + rnorm(1, 0, sigma2)
  }
  return(xt)
}

# try diff vals 
fis = seq(-1,1,0.01)

arfunc(u,-1,t)
resultMatrix = matrix(0,length(fis), t)

row = 1
for( fi in fis){
  resultMatrix[row,] = arfunc(u,fi,t)
  row = row + 1
}


hist(resultMatrix[,1])
hist(resultMatrix[,50])
hist(resultMatrix[,100])
hist(resultMatrix[,150])
hist(resultMatrix[,200])

# looks like we're getting a tighter and thighter normal dist centered around 20


xt = arfunc(u, 0.3, t)
yt = arfunc(u, 0.9, t)

StanModel = '
data {
  int<lower=0> T;
  vector[T] x;
}
parameters {
  real u;
  real fi;
  real<lower=0> sigma;
}
model {
  for (t in 2:T)
    x[t] ~ normal(u + fi * (x[t-1] - u), sigma);
}'

xdata = list(T = t, x = xt)
ydata = list(T = t, x = yt)

xproc = stan(model_code = StanModel, data = xdata)
yproc = stan(model_code = StanModel, data = ydata)

valuesx = summary(xproc, pars = c("u","fi","sigma"),
                  probs = c(0.025, 0.975))$summary
valuesy = summary(yproc, pars = c("u","fi","sigma"),
                  probs = c(0.025, 0.975))$summary

# did stan predict the variables well? 

mean(xt) # from original model
mean(yt) # from original model

## 
#we see that the stan is very close for all three, means for both data sets and fi/sigma

valuesx
#captures all
valuesy
#captures all 
##joint post

## need to figure out how to get convergence then lab is done.
listofdrawsx = extract(xproc)
listofdrawsy = extract(yproc)

plot(listofdrawsx$u)
plot(listofdrawsx$fi)
plot(listofdrawsx$sigma)

1:length(listofdrawsx$u)


udf = data.frame(
  draws = 1:length(listofdrawsx$u),
  my = listofdrawsx$u
)


fidf = data.frame(
  draws = 1:length(listofdrawsx$u),
  fi = listofdrawsx$fi
)

sigmadf = data.frame(
  draws = 1:length(listofdrawsx$u),
  sigma = listofdrawsx$sigma
)

ggplot(udf, aes(x = draws, y = my)) + geom_line()
ggplot(fidf, aes(x = draws, y = fi)) + geom_line()
ggplot(sigmadf, aes(x = draws, y = sigma)) + geom_line()

# lab done

