# 2.1
# Set working directory
setwd("~/courses/tdde01") 
# Read data
data = read.csv("machines.csv")
 
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

# 2.2 Exponential distribution
# PxTheta
pxt = function(x, theta){
    # Prod returns product of all arguments
    return (log(prod(theta*exp(-theta*x))))
}
thetas = seq(from=0, to=2, by=0.005)
theta_fn = sapply(thetas, function(theta) pxt(data$Length, theta))
#par(mar=c(1,1,1,1))
plot(1:length(thetas), theta_fn, xlabel="X", ylabel="Y", type="p")
theta_max = thetas[which.max(theta_fn)]
cat("Max theta value: ")
cat(theta_max)
cat("\n\n")

# 2.3
short_data = data$Length[1:6]
short_theta_fn = sapply(thetas, function(theta) pxt(short_data, theta))
par(mar=c(2,2,2,1))
plot(1:length(thetas), theta_fn, xlabel="X", ylabel="Y", type="l", ylim=c(-60, 0))
lines(1:length(thetas), short_theta_fn, col="blue")
short_theta_max = thetas[which.max(short_theta_fn)]
cat("Max theta value of short dataset: ")
cat(short_theta_max)
cat("\n\n")

# 2.4
pt = function(theta){
   return (10*exp(-10*theta))
}
pxt = function(x, theta) {
    return (prod(theta*exp(-theta*x)))
}
l = function(x, theta){
    return (log(pxt(x, theta)*pt(theta)))
}
theta_fn = sapply(thetas, function(theta) l(data$Length, theta))
l_max = thetas[which.max(theta_fn)]
lines(1:length(thetas), theta_fn, col="red")
cat("Max theta value: ")
cat(l_max)
cat("\n\n")

# 2.5
new_data = rexp(50, theta_max)
hist(new_data)
hist(data$Length)
