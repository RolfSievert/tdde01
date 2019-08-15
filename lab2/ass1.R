# Add libraries
library("gridExtra")
library("ggplot2")
library("MASS")

# Set working directory
setwd("~/courses/tdde01/lab2")

# Read data
crabs = read.csv("australian-crabs.csv")

crabs.plot = ggplot(data = crabs, 
                    mapping = aes(x=CL, 
                                  y=RW, 
                                  color=sex)) + 
geom_point() + 
ggtitle("Actual sex") +
scale_x_continuous(name = "Carapace length") +
scale_y_continuous(name = "Rear width") +
scale_color_discrete(name = "Sex")

lda.model = lda(formula = sex~CL+RW, data = crabs)
lda.predict = predict(lda.model, crabs)
crabs.predict = ggplot(data = crabs, 
                       mapping = aes(x=CL, 
                                     y=RW, 
                                     color=lda.predict$class)) + 
geom_point() + 
ggtitle("Predicted sex") + 
scale_x_continuous(name = "Carapace length") +
scale_y_continuous(name = "Rear width") +
scale_color_discrete(name = "Sex")

grid.arrange(crabs.plot, crabs.predict)

# Calculate misclassification
crabs.misclass = mean(crabs$sex != lda.predict$class)
cat("Misclassification rate for lda: ", crabs.misclass)

# Step 3
prior.male = 0.9
prior.female = 0.1
crabs.predict.prior = predict(lda.model, 
                              crabs, 
                              prior=c(Male=prior.male, 
                                      Female=prior.female)
                              )
crabs.plot.prior = ggplot(data=crabs,
                          mapping=aes(x=CL, 
                                      y=RW, 
                                      shape=sex,
                                      color=crabs.predict.prior$class)) +
geom_point() + 
ggtitle("Predicted sex using prior p(Male)=0.9, p(Female)=0.1") +
scale_x_continuous(name = "Carapace length") +
scale_y_continuous(name = "Rear width") +
scale_color_discrete(name = "Predicted sex") +
scale_shape_discrete(name = "Actual sex")
# Plot
grid.arrange(crabs.plot.prior)
# Calculate missclassification
crabs.prior.misclass = mean(crabs$sex != crabs.predict.prior$class)
cat("Misclassification rate for lda with p(Male=0.9), p(Female)=0.1: ",
    crabs.prior.misclass)

# Step 4
glm.model = glm(formula = sex~RW+CL, 
                data = crabs, 
                family = 'binomial')
glm.predict = predict(object = glm.model, 
                      newdata = crabs, 
                      type = 'response')
glm.predicted.sex = ifelse(glm.predict > 0.5, "Male", "Female")

# Calculate line from model
intercept = coef(glm.model)[1]
rw = coef(glm.model)[2]
cl = coef(glm.model)[3]
border = 0.5
k = -rw/cl
m = -(intercept)/cl
# Print plot and line
glm.plot = ggplot(data = crabs, 
                  mapping = aes(x=RW, 
                                y=CL, 
                                color=glm.predicted.sex, 
                                shape=crabs$sex)) + 
geom_point() + 
ggtitle("Actual and predicted sex (GLM), and decision boundary") +
scale_x_continuous(name = "Carapace length") +
scale_y_continuous(name = "Rear width") +
scale_color_discrete(name = "Predicted sex") +
scale_shape_discrete(name = "Actual sex") +
geom_abline(intercept = m, slope = k)
grid.arrange(glm.plot)

# Print misclassification
glm.misclass = mean(crabs$sex != glm.predicted.sex)
cat("Misclassification rate for GLM: ",
    glm.misclass)
