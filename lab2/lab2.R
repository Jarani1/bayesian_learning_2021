library("ggplot2")

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
u0 = t(c(-10,100,-100)) # (1x3) for b0,b1,b2 in the quadratic regression model posted in the lab desc (I THINK)
omega0 = diag(x = 0.01, 3,3) 
v0 = 4
sigma0 = 1

