
# Title:    Week 1 Ridge Regression - K-fold execution
# Course:   Multivariate Statistics
# Authors:  Merlijn van Breugel
# Date:     01-09-2016


#Define a range of lambda you want so use:
lambda_range <- seq(
    from = 0.1, 
    to = 10, 
    by = 0.1)

#Give number of folds
k <- 5

#Choose Matrices X and y
X <- dataX
y <- datay

#Add column with fold-categories
selection_col <- sample(1:k,dim(X)[1][1],replace=TRUE)
X_foldcat <- cbind(X,selection_col)
y_foldcat <- cbind(y,selection_col)

#Start looping over all k folds
for (i in 1:k) {
    
    X_train     <- X_foldcat[X_foldcat[,dim(X_foldcat)[2][1]]!=i,]
    X_holdout   <- X_foldcat[X_foldcat[,dim(X_foldcat)[2][1]]==i,]
    y_train     <- y_foldcat[y_foldcat[,dim(y_foldcat)[2][1]]!=i,]
    y_holdout   <- y_foldcat[y_foldcat[,dim(y_foldcat)[2][1]]==i,]
    
    #Loop over your lambda range
    for (m in lambda_range){
        
    }
}

