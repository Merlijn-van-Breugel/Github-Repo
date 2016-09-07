# Principal components analysis on drug use

# Raw data
load("Week 2 - Principal Component Analysis/drug_use.RData") #MvB: Note that you give the correct folder of the project. Preferably use Git repository
res.raw <- princomp(drug_use, cor = TRUE)
summary(res.raw)
screeplot(res.raw)
biplot(res.raw, asp = 1)

# Compute proportion of Gross State Product proportionally per sector (divide by row sums)
res.share <- princomp(drug_use / matrix(rowSums(drug_use), nrow = nrow(drug_use), ncol = ncol(drug_use)), cor = T)
summary(res.share)
screeplot(res.share)
biplot(res.share)

# Do a permutation test to see distribution eigenvalues under the null hypothesis of random data
source("Week 2 - Principal Component Analysis/permtestPCA.R")     # Source the permutation test function
drug_use_share <- drug_use / matrix(rowSums(drug_use), nrow = nrow(drug_use), ncol = ncol(drug_use))
res.perm <- permtestPCA(drug_use_share)
#MvB: Looks like 4 components!

# Do a bootstrap on eigenvalues to see how stable the eigenvalues are.
library("boot")                                     # Use the boot package

boot.pca.fn <- function(data, index){               # Make a function that returns the 
  res <- princomp(drug_use_share[index,], cor = TRUE)    # pca singular values for data selected
  return(res$sdev)                                  # by the vector index
}
res.boot <- boot(drug_use_share, boot.pca.fn, R = 1000)  # Run 1000 bootstraps
print(res.boot, digits = 3)                         # Print bootstrap results
plot(res.boot, index =  1)                          # Histogram of 1st singular value of the bootstraps
plot(res.boot, index =  2)                          # Histogram of 2nd singular value of the bootstraps
boot.ci(res.boot, index = 1, type = "perc")         # Show show 95% confidence interval 





#MvB: I think this is just the example for the lecture, not needed for assignment

    # Do SVD and reconstruct the matrix
    X <- matrix(c(3, 5, 1, 2, 3, 2), 3, 2) # Make a matrix
    print(X, digits = 0)                   # Print a matrix
    tt <- svd(X)                           # Compute an SVD
    print(tt$u, digits = 3)                # Print left singular vectors
    print(tt$d, digits = 3)                # Print singular values
    print(tt$v, digits = 3)                # Print right singular vectors
    tt$u %*% diag(tt$d) %*% t(tt$v)        # Reconstruct matrix X
    


