# Search for function: RSiteSearch(”expression")

# 1.1

# Set working directory
setwd("~/courses/tdde01")

# Read data
data = read.csv("spambase.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

# 1.2
# Create a General Linear Model
model = glm(formula = Spam~., data = train, family = 'binomial')

# Make prediction on test
predict_test = predict(model, test, 'response')
# Make prediction of train
predict_train = predict(model, train, 'response')
# Show result, match actual answers to predicted ones
table_test_0.5 = table(test$Spam, predict_test > 0.5)
table_train_0.5 = table(train$Spam, predict_train > 0.5)
cat("\nPrediction with GML model, test data. 0.5")
print(table_test_0.5)
cat("Missclassification rate: ")
cat((sum(table_test_0.5[2]) + sum(table_test_0.5[3]))/sum(table_test_0.5[1:4]))
cat("\n\n");
cat("Prediction with GML model, train data. 0.5")
print(table_train_0.5)

cat("Missclassification rate:")
cat((sum(table_train_0.5[2]) + sum(table_train_0.5[3]))/sum(table_train_0.5[1:4]))
cat("\n\n");
# 1.3
table_test_0.9 = table(test$Spam, predict_test > 0.9)
cat("Prediction with GML model, test data. 0.9")
print(table_test_0.9)
cat("Missclassification rate:")
cat((sum(table_test_0.9[2]) + sum(table_test_0.9[3]))/sum(table_test_0.9[1:4]))
cat("\n\n");

# 1.4
K = 30
# kknn is classifier
kknn_test = kknn(formula = Spam~., train = train, test = test, k = K)
kknn_train = kknn(formula = Spam~., train = train, test = train, k = K)
table_kknn_test = table(test$Spam, kknn_test$fitted.values > 0.5)
table_kknn_train = table(train$Spam, kknn_train$fitted.values > 0.5)
cat("KKNN result using test data test for K=30")
print(table_kknn_test)
cat("Missclassification rate:")
cat((sum(table_kknn_test[2]) + sum(table_kknn_test[3]))/sum(table_kknn_test[1:4]))
cat("\n\n");
cat("KKNN result using test data train for K=30")
print(table_kknn_train)
cat("Missclassification rate:")
cat((sum(table_kknn_train[2]) + sum(table_kknn_train[3]))/sum(table_kknn_train[1:4]))
cat("\n\n");

# 1.5
K = 1
# kknn is classifier
kknn_test = kknn(formula = Spam~., train = train, test = test, k = K)
kknn_train = kknn(formula = Spam~., train = train, test = train, k = K)
table_kknn_test = table(test$Spam, kknn_test$fitted.values > 0.5)
table_kknn_train = table(train$Spam, kknn_train$fitted.values > 0.5)
cat("KKNN result using test data test for K=1")
print(table_kknn_test)
cat("Missclassification rate:")
cat((sum(table_kknn_test[2]) + sum(table_kknn_test[3]))/sum(table_kknn_test[1:4]))
cat("\n\n");
cat("KKNN result using test data train for K=!")
print(table_kknn_train)
cat("Missclassification rate:")
cat((sum(table_kknn_train[2]) + sum(table_kknn_train[3]))/sum(table_kknn_train[1:4]))
cat("\n\n");
