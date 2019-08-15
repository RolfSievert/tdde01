library("pls")
library("ggplot2")
library("gridExtra")
library("MASS")
library("tree")

### FUNCtiONS
PC_needed = function(eigenv, border) {
  pc.sum = 0
  count = 0
  for (pc in eigenv/sum(eigenv)) {
    if (pc.sum < border) {
      count = count + 1
      pc.sum = pc.sum + pc
    }
  }
  return (count)
}

video = data.frame(read.csv("video.csv"))

data = subset(video, select=c(-utime, -codec))
data_size = dim(data)[1]
set.seed(12345)
i1 = sample(1:data_size, floor(data_size*0.5))
i2 = setdiff(1:data_size, i1)
train = data[i1,]
test = data[i2,]

### Without scaling
pca = prcomp(data, scale. = FALSE)

# get eigen values
lambda = pca$sdev^2
screeplot(pca)
message(sprintf("%1.3f", lambda/sum(lambda)))
message(sprintf("PC needed with no scaling: %i", PC_needed(lambda, 0.95)))

### With scaling
pca = prcomp(data, scale. = TRUE)
screeplot(pca)
# get eigen values
lambda = pca$sdev^2
message(sprintf("%1.3f", lambda/sum(lambda)))
message(sprintf("PC needed with scaling: %i", PC_needed(lambda, 0.95)))

# With scaling, each data column is normalized to prevent some vectors 
# to contribute to the eigenvalues unproportionally muh

## 1.2
data = subset(video, select=c(-codec))
data_size = dim(data)[1]
set.seed(12345)
i1 = sample(1:data_size, floor(data_size*0.5))
i2 = setdiff(1:data_size, i1)
train = data[i1,]
test = data[i2,]

trainE = numeric(17)
testE = numeric(17)
pcrN = pcr(utime~., 17, data=train, scale=T)
for (comp in 1:17) {
  trP = predict(pcrN, ncomp=comp)
  teP = predict(pcrN, ncomp=comp, newdata=test)
  trainE[comp] = mean((trP-train$utime)^2)
  testE[comp] = mean((teP-test$utime)^2)
}

plot(trainE, type='l', col="red", ylab="Error")
lines(testE, type='l', col="blue")

# The more components used, the more complex and biased the model will be to the 
# training data. This will increase the risk of the model only giving correct results on 
# training data and not test data. To allow other data predictions, there must be
# variance, and that is the bias-variance tradeoff. As seen in the graph, train and
# test prediction correctness increase greatly to component 8, and afterwards there is 
# no big difference in prediction error. Increasing number of components used increases
# the risk that the model is too biased.

pcrF = pcr(utime~., 8, data=train, validation="none", scale=T)
message(sprintf("%f", mean(residuals(pcrF)^2)))
message(Yloadings(pcrF))

### 1.4
dat = subset(video, select=c(duration, frames))
dat$class = ifelse(video$codec == "mpeg4", "mpeg", "other")
plot.projection = ggplot() + geom_point(data=dat, mapping=aes(x=duration, y=frames, color=class))
grid.arrange(plot.projection)
#class = ifelse(video$codec=="mpeg4", "mpeg", "other")
#durs = video$duration

# Almost, just a few won't fit to a linear regression

### 1.5
lda.fit = lda(class~., dat)
lda.pred = predict(lda.fit)
dat.tmp = dat
dat.tmp$pred = lda.pred$class

missclass = mean(dat$class != dat.tmp$pred)

plot = ggplot() + 
  geom_point(dat.tmp, mapping = aes(x=duration, y=frames, color=pred, shape=class))

grid.arrange(plot)

# The data cannot be separated by a linear equation.

### 1.6

tree.fit = tree(formula=as.factor(class)~frames+duration, data=dat)
tree.prune = cv.tree(tree.fit)
pred = predict(tree.fit, type="class")
misclass = mean((pred!=dat$class))
plot(tree.fit)

# Not parallel to any coordinate axes. The tree must therefore separate decisions into stair-like
# branches.
