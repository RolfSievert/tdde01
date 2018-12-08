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
spectra = read.csv2("NIRSpectra.csv")

# Calculate principal components
pca = princomp(spectra)

# Get eigen values
lambda = pca$sdev^2
# Plot variance of each component
screeplot(pca)

# Print proportion of variation
sprintf("%2.3f",lambda/sum(lambda)*100)
cat("99% of the variance can be explained by first two PC.\n\n")

# Extract principal components
PC1 = pca$scores[,'Comp.1']
PC2 = pca$scores[,'Comp.2']
plane = matrix(c(PC1, PC2), ncol=2) 
colnames(plane)=c("PC1", "PC2")
# Project data onto PC1 and PC2
projection = spectra*plane

# Plot the data projected onto PC 1 and 2
plot.projection = ggplot() +
    geom_point(data=projection, mapping=aes(x=PC1, y=PC2))
grid.arrange(plot.projection)

# Step 2
load = loadings(pca)

plot(load[,1], main="Traceplot, PC1")
plot(load[,2], main="Traceplot, PC2")

# Step 3a
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
plot(load[,1], main="Traceplot, PC1")
plot(load[,2], main="Traceplot, PC2")
# These look identical to step 2

# Step 3b
# Extract principal components
PC1 = res$S[,1]
PC2 = res$S[,2]
plane = matrix(c(PC1, PC2), ncol=2) 
colnames(plane)=c("PC1", "PC2")
projection = spectra*plane

# Plot the data projected onto PC 1 and 2
plot.projection = ggplot() +
    geom_point(data=projection, mapping=aes(x=PC1, y=PC2))
grid.arrange(plot.projection)
# Plot looks mirrored to step 1
