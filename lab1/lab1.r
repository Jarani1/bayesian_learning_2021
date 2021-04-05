library(ggplot2)
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
logodds = log(sample/(1-sample)) #NaNs
plot(density(logodds))
