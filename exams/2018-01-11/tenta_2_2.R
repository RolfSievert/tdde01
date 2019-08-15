library("neuralnet")

set.seed(12345)

Points = runif(50, 0, 10)
data = data.frame(Points, Sin=sin(Points))

#indexes = sample(1:50)
indexes=1:50
tr = data[indexes[1:25],]
va = data[indexes[26:50],]

its=10
vals = numeric(its)
w = runif(31, -1, 1)
for (i in (1:its)) {
  model = neuralnet(Sin~Points, data=tr, hidden=c(10), startweights = w, threshold = i/1000)
  comp = compute(model, va$Points)
  mse = sum((comp$net.result - va$Sin)^2) / nrow(va)
  vals[i] = mse
}
plot(x=(1:its)/1000, y=vals)
message(sprintf("First model: %f", vals[1]))

# Next model
w = runif(22, -1, 1)
for (i in (1:its)) {
  model = neuralnet(Sin~Points, data=tr, hidden=c(3, 3), startweights = w, threshold = i/1000)
  comp = compute(model, va$Points)
  mse = sum((comp$net.result - va$Sin)^2) / nrow(va)
  vals[i] = mse
}
plot(x=(1:its)/1000, y=vals)
message(sprintf("Second model: %f", vals[9]))

w = runif(31, -1, 1)
model = neuralnet(Sin~Points, data=tr, hidden=c(10), startweights = w, threshold = 1/1000)
Points = runif(50, 0, 10)
te = data.frame(Points, Sin=sin(Points))
comp = compute(model, te$Points)
mse = sum((comp$net.result - te$Sin)^2) / nrow(te)
message(sprintf("Final model generalization error: %f", mse))