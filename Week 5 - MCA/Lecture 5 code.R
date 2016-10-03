# 5 MCA through homals
library(homals)                       # Load the homals package for MCA
source("Lecture 5 MCA/plot.homals.R") # Overrides default homals plot command
source("Lecture 5 MCA/rescale.R")     # Rescales the output of homals
load("Lecture 5 MCA/titanic.RData")   # Load titanic data 



# MCA
res <- homals(data = titanic, ndim = 2, rank = 2, level = "nominal")
res <- rescale(res)
res$eigenvalues/(nrow(titanic))              # Print eigenvalues
plot(res, plot.type = "jointplot", asp = 1)  # biplot of objects and categories  
plot(res, plot.type = "objplot", asp = 1)    # Plot of the object scores
plot(res, plot.type = "loadplot", asp = 1)   # Loadings plot
plot(res, plot.type = "labplot", var.subset = 1, asp = 1) # object score plot labeled by a12 (doctors are murderers)
summary(res)


# Because there are many observations with the exact same objects score, add some jitter
res.jit <- res
res.jit$objscores <- jitter(res$objscores, amount = .08)
plot(res.jit, plot.type = "jointplot", asp = 1)  # biplot of objects and categories  
plot(res.jit, plot.type = "labplot", var.subset = 1, asp = 1) # object score plot labeled by a12 (doctors are murderers)



