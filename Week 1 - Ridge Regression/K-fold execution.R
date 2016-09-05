
# Title:    Week 1 Ridge Regression - K-fold execution
# Course:   Multivariate Statistics
# Authors:  Merlijn van Breugel
# Date:     01-09-2016


#Select data from supermarket1996
data<- supermarket1996[6:50]                        #X
data<- cbind(supermarket1996["GROCERY_sum"],data)   #append y to X

data<- data[sample(nrow(data)),]                    #Shuffle data
#Choose Matrices X and y
y<- data[,1]
X<- data[,c(-1)]

#Define a logarithmic range of lambda you want so use:
lambda_range <- 10^seq(
    from = -4, 
    to = 4, 
    length.out = 100)

#Give number of folds
k <- 5

#Add column with fold-categories
selection_col   <- sample(1:k,dim(X)[1][1],replace=TRUE)
X_foldcat       <- cbind(X,selection_col)
y_foldcat       <- cbind(y,selection_col)

#Loop over your lambda range
for (m in lambda_range){
    
    #We use the Ridge-function, we have to store results in a matrix 
    
    #Initialize a matrix for estimates
    b_estimates <- NULL
    R_sq_holdout<- NULL
    
    #Start looping over all k folds, use k-fold- function
    test <- k_fold_function(k,X_foldcat,y_foldcat,lambda_range)
        
        

    }

