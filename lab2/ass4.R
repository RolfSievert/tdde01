# Step 1
# Add libraries
library("gridExtra")
library("ggplot2")
library("MASS")
library("stats")
library("fastICA")

# Set working directory
setwd("~/courses/tdde01/lab2")

# Read data
spectra = data.frame(read.csv2("NIRSpectra.csv"))
spectra = subset(spectra, select=-Viscosity)

# Calculate principal components
pca = prcomp(spectra)

# Get eigen values
lambda = pca$sdev^2
# Plot variance of each component
screeplot(pca)

# Print proportion of variation
sprintf("%2.3f",lambda/sum(lambda)*100)
cat("99% of the variance can be explained by first two PC.\n\n")

# Extract principal components
PC1 = pca$x[,1]
PC2 = pca$x[,2]

# Plot the data projected onto PC 1 and 2
plot.projection = ggplot() +
    geom_point(data=spectra, mapping=aes(x=PC1, y=PC2))
grid.arrange(plot.projection)

# Step 2
plot(pca$rotation[,1], main="Traceplot, PC1")
plot(pca$rotation[,2], main="Traceplot, PC2")

# Step 3a
set.seed(12345)
res = fastICA(X=spectra, 
        n.comp=2, 
        alg.typ= "parallel", 
        fun = "logcosh", 
        alpha = 1, 
        method = "R", 
        row.norm = FALSE, 
        maxit= 200, 
        tol = 0.0001, 
        verbose = TRUE)

W.prime = res$K %*% res$W
# Traceplots of fastICA
plot(W.prime[,1], main="Traceplot, PC1")
plot(W.prime[,2], main="Traceplot, PC2")

# Step 3b
# Extract ICs
IC1 = res$S[,1]
IC2 = res$S[,2]

# Plot the data projected onto PC 1 and 2
plot.projection = ggplot() +
    geom_point(data=spectra, mapping=aes(x=IC1, y=IC2))
grid.arrange(plot.projection)
