library(ggplot2)
library("LaplacesDemon")
library(reshape2)
library("bayestestR")


# 1.Daniel Bernoulli

#likelihood of sample given some unk variable
#post like * prior ->> most prob unk given data and prior knowledge??

s = 8 
n = 24
f = n - s
a = b = 3

thetas = seq(0,1,0.001)
post = dbeta(thetas, a + s, b + f) 
#true value should be theta of post with highest density
true = thetas[which.max(post)]

data = data.frame(
  x = thetas,
  y = post
)

ggplot(data, aes(x,y)) + geom_line(color="blue") + xlab("thetas") + ylab("density") + 
  geom_vline(xintercept = true, linetype="dashed", color="red", size = 1)



#a) draw random we got mean fix std, what's true std 
# try 10, 100, 1000, 10000
means = integer(100)
stdevs = integer(100)
i = 0
for (draws in seq(0,100,1)){
  sample = rbeta(draws, a+s,b+f)
  means[i] = mean(sample)
  stdevs[i] = sd(sample)
  i = i + 1
}

data1 = data.frame(
  x = seq(1:100),
  y = means
)
means
ggplot(data1, aes(x,y)) + geom_point() + xlab("draws") + ylab("mean theta") + 
  geom_hline(yintercept = true, linetype="dashed", color="red", size = 1)


#b)

# sim 10k
exact = pbeta(0.4,a+s,b+f, lower.tail = FALSE)
sample = rbeta(10000, a+s,b+f)
sim = length(sample[sample>0.4]) / 10000
sim
exact


#c)
#correct base?
logodds = log(sample/(1-sample)) 
plot(density(logodds))

#####################################

#part 2


#####################################
incomes = c(39,20,49,58, 31, 70, 18, 56, 25, 78)
#common for non negative cont, log-normal dist
sample_size = 10000
u = 3.8
n = length(incomes)
tao2 = sum((log(incomes) - u)^2)/n
tao2

#tao for scaled

sim_sig = rinvchisq(sample_size,n, tao2)
true_sig = dinvchisq(seq(0.0001,1,0.0001),n)

df = data.frame(
  sim = sim_sig,
  true = true_sig
)
df_group = melt(df, variable.name = "id", value.name = "sigma")
df_group
ggplot(df_group, aes(x=sigma, color=id)) + geom_density()


#plot them

# b)

#income inequality 
sim_sig/sqrt(2)
gini_coef = (2 * pnorm(sim_sig/sqrt(2), 0, 1)) - 1

ggplot(df, aes(x = gini_coef)) + geom_density()


# c)

ci_90 = ci(gini_coef, method = "ETI", ci = 0.9)
hdi_90 = ci(gini_coef, method = "HDI", ci = 0.9)

ginipost = density(gini_coef)

gdens <- data.frame(
  x = ginipost$x,
  y = ginipost$y,
  n = ginipost$n,
  low = ci_90$CI_low,
  high = ci_90$CI_high
)
ggplot(gdens, aes(x,y)) +
  geom_area(fill="turquoise") +
  geom_vline(xintercept = ci_90$CI_low, color = "red", size = 0.5) +
  geom_vline(xintercept = ci_90$CI_high, color = "red", size = 0.5) +
  # now for the HPD
  geom_vline(xintercept = hdi_90$CI_low, color = "blue", size = 1) +
  geom_vline(xintercept = hdi_90$CI_high, color = "blue", size = 1)


#####################################

#part 3


#####################################
##iid so product is likelihood of seq

#prior

#a)
wind = c(-2.44, 2.14, 2.54, 1.83, 2.02, 2.33, -2.79, 2.23, 2.07, 2.02)
u = 2.39 # known
ks = seq(0.01,10,0.01)
prior = dexp(ks)
likelihood = numeric(length(ks))

#calc likelihood
i = 1
for(k in ks){
  likelihood[i] = prod(exp(k * cos(wind -u))/(2*pi*besselI(k,0)))
  
  i = i + 1
}

post_dist = likelihood * prior
plot(post_dist)

dfvon <- data.frame(
  x = ks,
  prior = prior,
  like = likelihood,
  post = post_dist
)
ggplot(dfvon, aes(x=ks)) + geom_line(aes(y = post), color="red")

#find mode b)
Mode(post_dist)

