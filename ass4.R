# Assignment 4
# Set working directory
setwd("~/courses/tdde01")

# Read data
data = read.csv("tecator.csv")

# 4.1 No?

# 4.2 TODO:

# 4.3
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
val=data[-id,]
