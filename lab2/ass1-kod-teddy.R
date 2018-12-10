library("ggplot2")
library("MASS")
# 1.1
setwd("~/TDDE01/lab2")
data <- data.frame(read.csv("australian-crabs.csv"))
ggplot(data = data, mapping = aes(CL, RW, color = sex)) + geom_point()
# Yes, it seems to be two pretty distinct data sets, perhaps the beginning could be tricky
# 1.2
lda.model1 <- lda(sex ~ CL + RW, data = data)
lda.predict1 <- predict(lda.model1, data)
ggplot.0.5 <- ggplot(data = data, mapping = aes(CL, RW, color = lda.predict1$class, shape = sex )) + geom_point()
mcr.0.5 <- mean(lda.predict1$class != data$sex)
# 1.3
lda.model2 <- lda(sex ~ CL + RW, data = data, prior = c(0.1, 0.9))
lda.predict2 <- predict(lda.model2, data)
ggplot.0.9 <- ggplot(data = data, mapping = aes(CL, RW, color = lda.predict2$class, shape = sex )) + geom_point()
mcr.0.9 <- mean(lda.predict2$class != data$sex)
# It got worse since the distribution of the data is 50/50 and not 90/10
#1.4
glm.model <- glm(as.factor(sex) ~ CL + RW, family = binomial, data = data)
glm.predict <- predict(glm.model, data, type = 'response')
glm.predict.0.5 <- ifelse(glm.predict > 0.5, "Male", "Female")
mcr.glm <- mean(glm.predict.0.5 != data$sex)
summary(glm.model)
glm.slope <- coef(glm.model)[2]/(-coef(glm.model)[3])
glm.intercept <- coef(glm.model)[1]/(-coef(glm.model)[3])
ggplot.0.5 + geom_abline(slope = glm.slope, intercept = glm.intercept)
