# Assignment 4
# Set working directory
setwd("~/courses/tdde01/lab1")

# Read data
data = read.csv("tecator.csv")

# 4.1 No?
plot(data$Protein, data$Moisture, xlab="Protein", ylab="Moisture", type = "p")

# 4.2 TODO:

# 4.3
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
eval=data[-id,]

mse_eval_list = c()
mse_train_list = c()
for (x in c(1, 2, 3, 4, 5, 6)){
    model=lm(formula = Moisture ~ poly(Protein, x, raw = TRUE), data = train)
    # Eval data
    mse = mean((eval$Moisture - predict(model, eval))^2)
    mse_eval_list=c(mse_eval_list, mse)

    # Train data2
    mse = mean((train$Moisture - predict(model, train))^2)
    mse_train_list=c(mse_train_list, mse)

}
counts <- table(mse_eval_list, mse_train_list)
barplot(c(mse_eval_list, mse_train_list), 
        main="Prediction on evaluation and training data", 
        ylab="MSE", 
        xlab="Blue: eval, Red: train",
        col=c("blue","blue","blue","blue","blue","blue","red", "red", "red", "red", "red", "red"), 
        ylim=c(32, 35))
stop()

# 4.4
channels = data[,2:102]
model = lm(formula = Fat~. , data = channels)
step_alg = stepAIC(model, direction="both", trace = FALSE)
cat("Length: ", length(coef(step_alg)) -1, "\nCoefficients: ", coef(step_alg))

# 4.5
covariates=channels[,1:100]
response=channels[, 101]
model0=glmnet(as.matrix(covariates), 
              response, 
              alpha=0,
              family="gaussian")
plot(model0, xvar="lambda", label=TRUE)
# The coefficients shrink when lambda increases

# 4.6
# LASSO, alpha = 1
model1=glmnet(as.matrix(covariates), 
              response, 
              alpha=1,
              family="gaussian")
plot(model1, xvar="lambda", label=TRUE)

# 4.7
model=cv.glmnet(as.matrix(covariates), 
                response, 
                lambda = seq(0, 5, 0.005),
                alpha=1,
                family="gaussian")
cat("\n\n\nMin lambda: ", model$lambda.min)
plot(model)
cat("\nNumber of coefficients of lambda min: ") 
coeffs = coef(model, s=model$lambda.min)
coeffs
cat(sum (coeffs[2:101,] != 0))

# 4.8 Compare 4 and 7
