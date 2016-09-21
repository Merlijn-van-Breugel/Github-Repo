#Set directory
setwd("~/GitHub/Multivariate-Statistics-Github-Repo/Week 4 - Non-linear PCA")
#Load dimensionalized table
HS_df  <- read.table("Dimensionalized table Holiday spending.txt", header = TRUE)
HS_mat <- as.matrix(read.table("Dimensionalized table Holiday spending.txt", header = TRUE))

#Load packages and source functions
install.packages('homals')        # Install package homals    
library(homals)                   # Load the homals package for nonlinear PCA
source("plot.homals.R")           # Overrides default homals plot command
source("rescale.R")               # Rescales the output of homals

# Nonlinear PCA
res <- homals(data = HS_df[, 1:8], ndim = 2,
                rank  = c(1, 1, 1, 1, 
                          1, 1, 1, 1), 
                level = c("nominal", "nominal", "ordinal", "nominal",
                          "nominal", "ordinal", "ordinal", "ordinal"))

res <- rescale(res)
res$eigenvalues                              # Print eigenvalues
#MvB: Note that the eigenvalues are larger than usual. They do sum to p(8)

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


