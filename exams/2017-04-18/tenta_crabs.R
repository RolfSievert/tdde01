library("ggplot2")
library("gridExtra")
library("e1071")
library("MASS")

crabs = read.csv("australian-crabs.csv")

plot = ggplot() + geom_point(data=crabs, mapping=aes(x=CW, y=BD, color=species))
grid.arrange(plot)

# naïve bayes
model = naiveBayes(formula=as.factor(species)~CW+BD, data=crabs)
pred = predict(model, crabs)

t = table(True=pred, Predicted=crabs$species)

# Only one property is not sufficient to classify, both has to be compined to get a good result.

model = glm(formula=as.factor(species)~CW+BD, data = crabs, family = "binomial")
pred = predict(model, crabs, type='response')
pred_spec = ifelse(pred > 0.5, "Blue", "Orange")

# Calculate line from model
intercept = coef(model)[1]
rw = coef(model)[2]
cl = coef(model)[3]
border = 0.5
k = -rw/cl
m = -(intercept)/cl
# Print plot and line
glm.plot = ggplot(data = crabs, 
                  mapping = aes(x=CW, 
                                y=BD,
                                color=pred_spec, 
                                shape=species)) + 
  geom_point() + 
  ggtitle("Actual and predicted species (GLM), and decision boundary") +
  scale_x_continuous(name = "Carapace length") +
  scale_y_continuous(name = "Rear width") +
  scale_color_discrete(name = "Predicted spec.") +
  scale_shape_discrete(name = "Actual spec.") +
  geom_abline(intercept = m, slope = k)
grid.arrange(glm.plot)

### PCA

model = prcomp(subset(crabs, select=c(CW, BD)), scale. = T)
screeplot(model) #to get all eigen values
# Get eigen values (standard deviation ^ 2)
lambda = model$sdev^2
sprintf("%2.3f", lambda/sum(lambda))
PC1 = model$x[,1]
PC2 = model$x[,2]

# Present eigenvector
model = princomp(subset(crabs, select=c(CW, BD)))
l = loadings(model)
print(loadings(model))

### 1.6
crabs$PC1 = PC1
crabs$PC2 = PC2
model = naiveBayes(formula=as.factor(species)~PC1+PC2, data=crabs)
pred = predict(model, crabs)

t = table(True=pred, Predicted=crabs$species)
print(t)

# The data is now rotated so that both components are separatable