
# Title:    Week 1 Ridge Regression - Scaling Function
# Course:   Multivariate Statistics
# Authors:  Merlijn van Breugel
# Date:     03-09-2016

scaling_function <- function(X,center,standardize){
    
    center_factor_X <- 
        if (center==TRUE) {
            colMeans(X)} else {
                FALSE
            }
    scaling_factor_X <- 
        if (standardize==TRUE) {
            sqrt(apply(X,2,var))} else {
                FALSE
            }
    X_scaled <- scale(X, center=center_factor_X, scale=scaling_factor_X)
    
    return(X_scaled)
}

