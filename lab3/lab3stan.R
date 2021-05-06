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
resultMatrix = matrix(0,length(fi), t)

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



  

