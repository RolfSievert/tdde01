# Step 1
# Add libraries
library("gridExtra")
library("ggplot2")
library("tree")
library("MASS")
library("e1071")
require("e1071")

# Set working directory
setwd("~/courses/tdde01/lab2")

# Read data
scores = read.csv2("creditscoring.csv")
# Read as strings
scores$good_bad = as.factor(scores$good_bad)

# Split data into train/val/test
n=dim(scores)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5))
train=scores[id,] 
id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
val=scores[id2,]
id3=setdiff(id1,id2)
test=scores[id3,]

# Step 2
# Create tree
tree.deviance = tree(formula = good_bad~., 
                     data = train, 
                     split="deviance")
tree.gini = tree(formula = good_bad~., 
                     data = train, 
                     split="gini")
# Plot tree
plot(tree.deviance)
plot(tree.gini)

# Make predictions
predict.deviance.test = predict(tree.deviance, test, type = "class")
predict.deviance.train = predict(tree.deviance, train, type = "class")
predict.gini.test = predict(tree.gini, test, type = "class")
predict.gini.train = predict(tree.gini, train, type = "class")

# Calculate misclassification rates
misclass = function(predicted, true) {
    return(mean(predicted != true))
}

deviance.test.misclass = misclass(predict.deviance.test, test$good_bad)
deviance.train.misclass = misclass(predict.deviance.train, train$good_bad)
gini.test.misclass = misclass(predict.gini.test, test$good_bad)
gini.train.misclass = misclass(predict.gini.train, train$good_bad)

# Print misclassification rates
cat("Misclassification on deviance with test: ", 
    deviance.test.misclass)
cat("\nMisclassification on deviance with train: ", 
    deviance.train.misclass)
cat("\nMisclassification on gini with test: ", 
    gini.test.misclass)
cat("\nMisclassification on gini with train: ", 
    gini.train.misclass, "\n") 

# Step 3
# Lists of scores
range = 15
pruned=rep(0, range)
trainScore=rep(0, range)
testScore=rep(0, range)

test.tree = tree(formula=good_bad~., data=train)
for(i in 2:range) {
    # Prune the tree
    prunedTree=prune.tree(test.tree, best=i)
    # Make trediction on validation data
    pred=predict(prunedTree, newdata=val, type="tree")
    # Append scores
    trainScore[i]=deviance(prunedTree)
    testScore[i]=deviance(pred)
}

df = data.frame(pruned, trainScore, testScore)

ggplot(mapping=aes(col="Test data")) +
    geom_line(data=df, mapping=aes(x=pruned, y=trainScore, col="Train")) +
    geom_point(data=df, mapping=aes(x=pruned, y=trainScore, col="Train")) +
    geom_line(data=df, mapping=aes(x=pruned, y=testScore, col="Test")) +
    geom_point(data=df, mapping=aes(x=pruned, y=testScore, col="Test"))

# Plot the scores
plot(2:range, trainScore[2:range], type="b", col="red", ylim=c(200, 600))
points(2:range, testScore[2:range], type="b", col="blue")

optLeaves = 4

# Info on optimal tree
optTree = prune.tree(tree.deviance, best=optLeaves)
summary(optTree)
optTree.predict = predict(optTree, newdata=test)
optTree.predict.string = ifelse(optTree.predict[2] > 0.5, "good", "bad")
optTree.misclass = misclass(optTree.predict.string, test$good_bad)
cat("\nMisclassification on optimal tree: ", 
    optTree.misclass)

cat("\nTree depth is 5 as can be seen in the plot. \n")
plot(optTree)
cat("\n\nUsed variables in optimal tree: \n'savings' 'duration' 'history' 'age' 'purpose'\n\n")

# Step 4
# Create model and classify
model.bayes = naiveBayes(formula = good_bad~., data=train)
predict.bayes.test = predict(model.bayes, newdata=test, type="class")
predict.bayes.train = predict(model.bayes, newdata=train, type="class")

# Print info on classification with prediction on test
table.bayes = table(predict.bayes.test, test$good_bad)
cat("Confusion table of naïve bayes:")
print(table.bayes)

misclass.bayes = mean(predict.bayes.test != test$good_bad)
cat("Misclassification with naïve bayes: ", misclass.bayes, "\n\n")

# Print info on classification with prediction on test
table.bayes = table(predict.bayes.train, train$good_bad)
cat("Confusion table of naïve bayes:")
print(table.bayes)

misclass.bayes = mean(predict.bayes.train != train$good_bad)
cat("Misclassification with naïve bayes: ", misclass.bayes, "\n\n")

# Step 5
# Calculate TPR and FPR of optimal tree and bayes
getROC = function(pred, pi) {
    tpr = c()
    fpr = c()
    for (p in pi) {
        # Change probabilities to strings
        tmp = ifelse(pred[,'good'] > p, "good", "bad")
        # Get confusion matrix
        cm = table(predicted=tmp, actual=test$good_bad)
        if('good' %in% rownames(cm)) {
          # Calculate TPR, first dim of cm is predicted
          t = cm['good', 'good'] / sum(cm[,'good'])
          # Calculate FPR
          f = cm['good', 'bad'] / sum(cm[, 'bad'])
          # Append to list of values
          tpr = c(tpr, ifelse(is.finite(t), t, 0))
          fpr = c(fpr, ifelse(is.finite(f), f, 0))
        } else {
          tpr = c(tpr, 0)
          fpr = c(fpr, 0)
        }
    }
    df = data.frame(tpr, fpr)
    return(df)
}

pi = seq(0.05, 0.95, 0.05)
pred.optTree = predict(optTree, newdata=test)
pred.bayes = predict(model.bayes, newdata=test, type='raw')
opt.roc = getROC(pred.optTree, pi)
bayes.roc = getROC(pred.bayes, pi)

#Plot
roc.plot = ggplot(mapping=aes(col=Classifier)) +
    geom_point(data=opt.roc, aes(x=fpr, y=tpr, col="Optimal tree")) +
    geom_line(data=opt.roc, aes(x=fpr, y=tpr, col="Optimal tree")) +
    geom_point(data=bayes.roc, aes(x=fpr, y=tpr, col="Bayes")) +
    geom_line(data=bayes.roc, aes(x=fpr, y=tpr, col="Bayes"))

grid.arrange(roc.plot)

# Step 6

