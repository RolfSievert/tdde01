
library("neuralnet")
set.seed(1234567890)

# Random data from 0-10, uniformly distributed
Var = runif(50, 0, 10)
trva = data.frame(Var, Sin=sin(Var))
tr = trva[1:25,] # Training
va = trva[26:50,] # Validation

# Random initialization of the weights in the interval [-1, 1]
winit = runif(31, -1, 1)
validations = c()
for (i in 1:10) {
    nn = neuralnet(formula = Sin ~ Var, data = tr, hidden = 10, startweights = winit, threshold = i/1000)
    comp = compute(nn, va$Var)
    
    mse = sum((comp$net.result - va$Sin)^2) / nrow(va)
    validations = c(validations, mse)
}
plot(x=seq(1/1000, 10/1000, 1/1000), y=validations, xlab="Thresholds", ylab="Validation data")
best_threshold = which.min(validations)/1000

# Optimal nn
nn = neuralnet(formula = Sin ~ Var, data = trva, hidden = 10, startweights = winit, threshold = best_threshold)

plot(nn, ylab="Neural network", rep="best")

# Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn)$rep1, ylim = c(-1, 1))
points(trva, col = "red")
