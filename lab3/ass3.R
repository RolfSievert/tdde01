
library("neuralnet")
set.seed(1234567890)

# Random data from 0-10, uniformly distributed
Var = runif(50, 0, 10)
trva = data.frame(Var, Sin=sin(Var))
tr = trva[1:25,] # Training
va = trva[26:50,] # Validation

# Random initialization of the weights in the interval [-1, 1]
# One hidden layer with 10 units
winit = sample(x = seq(-1, 1, 0.1), size = 10)
validations = c()
for (i in 1:100) {
    nn = neuralnet(formula = Sin ~ Var, data = tr, hidden = TRUE, startweights = winit, threshold = i/1000)
    comp = compute(nn, va$Var)
    # Calculate average percentage devaince of actual value
    sum = 0
    for (i in 1:length(comp$net.result)) {
        diff = abs(va$Sin[i] - comp$net.result[i])
        perc = diff / 2 # +1 to get span from -1 to 1
        sum = sum + perc
    }
    sum = sum/length(comp$net.result)
    validations = c(validations, sum)
}
plot(validations)
print(min(validations))
best_threshold = 7/1000

# Optimal nn
nn = neuralnet(formula = Sin ~ Var, data = trva, startweights = winit, threshold = best_threshold)
plot(nn)

# Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn)$rep1, ylim = c(-1, 1))
points(trva, col = "red")
