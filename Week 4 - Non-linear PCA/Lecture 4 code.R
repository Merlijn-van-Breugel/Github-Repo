# 5 Nonlinear PCA through homals
library(homals)                   # Load the homals package for nonlinear PCA
source("Lecture 4/plot.homals.R") # Overrides default homals plot command
source("Lecture 4/rescale.R")     # Rescales the output of homals
load("Lecture 4/stimezo.RData")   # Load example data (abortion)

# Nonlinear PCA
res <- homals(data = stimezo[, 1:16], ndim = 2, rank = 1, level = "ordinal")
res <- rescale(res)
res$eigenvalues                              # Print eigenvalues
plot(res, plot.type = "biplot", asp = 1, cex.lab = 2)     # biplot of component loadings and object scores: 
                                             # only available in my own plot.homals!
plot(res, plot.type = "objplot", asp = 1)    # Plot of the object scores
plot(res, plot.type = "loadplot", asp = 1)   # Loadings plot
plot(res, plot.type = "trfplot",  col = "blue") # All transformation plots
plot(res, plot.type = "jointplot", asp = 1)  # biplot of objects and categories  
plot(res, plot.type = "jointplot", var.subset = 12, asp = 1)  # biplot of objects and categories  
plot(res, plot.type = "labplot", var.subset = 12, asp = 1) # object score plot labeled by a12 (doctors are murderers)

# Different transformation levels per variable: A12 (doctors are murderers) multiple nominal
res <- homals(data = stimezo[, 1:16], ndim = 2, 
              rank = c(1, 1, 1, 1, 
                       1, 1, 1, 1, 
                       1, 1, 1, 2,
                       1, 1, 1, 1), 
              level = c("ordinal", "ordinal", "ordinal", "ordinal",
                        "ordinal", "ordinal", "ordinal", "ordinal",
                        "ordinal", "ordinal", "ordinal", "nominal",
                        "ordinal", "ordinal", "ordinal", "ordinal"))
res <- rescale(res)
plot(res, plot.type = "biplot", asp = 1)     # biplot of component loadings and object scores: 
# only available in my own plot.homals!
plot(res, plot.type = "objplot", asp = 1)    # Plot of the object scores
plot(res, plot.type = "loadplot", asp = 1)   # Loadings plot
plot(res, plot.type = "trfplot",  col = "blue") # All transformation plots
plot(res, plot.type = "jointplot", asp = 1)  # biplot of objects and categories  
plot(res, plot.type = "jointplot", var.subset = 12, asp = 1)  # biplot of objects and categories  
plot(res, plot.type = "labplot", var.subset = 12, asp = 1) # object score plot labeled by a12 (doctors are murderers)
summary(res)
