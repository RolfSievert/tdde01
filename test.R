# Search for function: RSiteSearch(”expression")

# Set working directory
setwd("~/courses/tdde01")

# Read data
data = read.csv("spambase.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

# Last elem of list containing answeres
test_ans = test[length(test)]
# Remove last element from test
test = test[1:length(test)-1]

# Create a General Linear Model

# Use predict to get predicted values of model