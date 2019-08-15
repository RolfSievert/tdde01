library("kernlab")

spam = read.csv2("spambase.csv")
set.seed(12345)
len = dim(spam)[1]
index=sample(1:len)
tr = spam[index[1:3000], ]
va = spam[index[3001:3900], ]
te = spam[index[3901:len], ]

C = c(0.5, 1, 5)
for (c in 1:3) {
  ksvm.fit = ksvm(as.factor(Spam)~., data = tr, kpar = list(sigma=0.05), C=C[c], kernel="rbfdot")
  pred = predict(ksvm.fit, va[-58])
  t = table(pred, as.factor(va$Spam))
  message(sprintf("%i, error: %s \n", c, (t[1,2]+t[2,1])/sum(t)))
}

ksvm.fit = ksvm(as.factor(Spam)~., data = tr, kpar = list(sigma=0.05), C=1, kernel="rbfdot")
pred = predict(ksvm.fit, te[-58])
t = table(pred, as.factor(te$Spam))
message(sprintf("C=1, error: %s \n", (t[1,2]+t[2,1])/sum(t)))
