# Title:    Week 1 Ridge Regression - K-fold execution
# Course:   Multivariate Statistics
# Authors:  Merlijn van Breugel
# Date:     01-09-2016

#Select data from supermarket1996
data<- supermarket1996[6:50]                        #X
data<- cbind(supermarket1996["GROCERY_sum"],data)   #append y to X

data<- data[sample(nrow(data)),]                    #Shuffle data
y<- data[,1]
X<- data[,c(-1)]
#Add column with fold-categories
fold<- c(rep(1,15),rep(2,15),rep(3,15),rep(4,16),rep(5,16)) #this could be done neater
data<- cbind(data,fold)
#Choose Matrices X and y
y_foldcat<- cbind(data[,1],fold)
X_foldcat<- data[,c(-1)]
#Define a logarithmic range of lambda you want so use:
lambda_range <- 10^seq(from = -4,to = 4,length.out = 100)
#Give number of folds
k <- 5
R_sq<- NULL
R_2 <- NULL
L <- NULL
#Loop over your lambda range
for (lambda in lambda_range){
    #Initialize a matrix for estimates
    b_estimates <- NULL
    R_sq_holdout<- NULL
    R_sq <- NULL
    #Start looping over all k folds, use k-fold- function
    R_sq <- k_fold_function(k,X_foldcat,y_foldcat,lambda)
    R_2 <- c(R_2,R_sq)
    L <- c(L,lambda)
}
R_sq_lambda <- cbind(L,R_2)

#Find optimal lambda
row_index_max <- which.max(R_sq_lambda[,2]) #get row index for maximum R^2
lambda_opt <- R_sq_lambda[row_index_max,1]
# do ridge for each lambda to get Beta for each lambda    

#Initialize matrices for estimates
b_estimates <- NULL
R_sq <- NULL
for (lambda in lambda_range){
    #Start looping over all k folds, use k-fold- function
    result          <- as.data.frame(ols_ridge_function(X,y,lambda,TRUE,TRUE))
    b_estimates     <- rbind(b_estimates,cbind(lambda,t(as.matrix(result$estimates))))
    R_sq <- rbind(R_sq,cbind(lambda,result$'R.2'[1]))
    R_2 <- c(R_2,R_sq)
    L <- c(L,lambda)
}
#plot 
matplot(log10(b_estimates[,1]), b_estimates[,c(-1)],type = c("l"),xlab="log10(lambda)",ylab="b estimates")   #for matrix b_estimates
matplot(log10(R_sq[,1]), R_sq[,c(-1)],type = c("l"),xlab="log10(lambda)",ylab="R-squared")                 #for R-squared
#give b coefficients for optimal lambda
b_opt <- ols_ridge_function(X,y,lambda_opt,TRUE,TRUE)[1]
