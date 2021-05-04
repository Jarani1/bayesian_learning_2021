library("ggplot2")
library("LaplacesDemon")


data = read.table("rainfall.dat")

hist(data$V1)



# CONJUGTE PRIOR
# u ~ N(u0, tao2) 
# sigma2 ~ invchi2(v0, sigma2_0)

#FULL COND POSTERIOR
# u|sigma2,y ~ N(u_n, tao2_n)
# sigma2|u,x ~ invchi2(vn, big_ass_expression)


y = data$V1
n = length(y)
hist(y)
#priors
u0 = 0
tao2_0 = 1
v0 = 5
sigma2_0 = 1
vn = v0 + n
#inits for gibbs
#didn't calc them just put something
#I mean I don't think they matter like that
sigma2 = 1



nDraws = 100
gibbsDraws = matrix(0,nDraws,2)
for(i in 1:nDraws){
  # lecture 3
  #draws from sigma will change these continuously
  weight = (n/sigma2) / (n/sigma2 + 1/tao2_0) #what is weight really
  tao2_n = (n/sigma2 + 1/tao2_0)^-1
  u_n = weight * mean(y) + (1 - weight) * u0
  my = rnorm(1, u_n, sqrt(tao2_n))
  #my will in turn change sigma2 continuously 
  scalingFactor = (v0*sigma2_0 + sum((y-my)^2))/n+v0
  sigma2 = rinvchisq(1, vn, scalingFactor)
  gibbsDraws[i,1] = my
  gibbsDraws[i,2] = sigma2
}


gibbsdf <- data.frame(
  my = gibbsDraws[,1],
  sigma2 = gibbsDraws[,2],
  iteration = 1:nDraws
)
stepsdf <- data.frame(
  my = gibbsDraws[,1][1:100],
  sigma2 = gibbsDraws[,2][1:100]
)



plot(gibbsDraws[,1][1:100], gibbsDraws[,2][1:100], type="s")

ggplot(gibbsdf, aes(iteration, my)) +
  labs(x = "MCMC iteration") +
  geom_line(size = 0.5, col = "red")

ggplot(gibbsdf, aes(iteration, sigma2)) +
  labs(x = "MCMC iteration") +
  geom_line(size = 0.5, col = "blue")

ggplot(stepsdf, aes(my, sigma2)) +
  geom_step(direction = "vh")

