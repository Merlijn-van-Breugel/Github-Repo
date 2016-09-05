# Title:    Week 1 Ridge Regression - K-fold execution
# Course:   Multivariate Statistics
# Authors:  Merlijn van Breugel
# Date:     01-09-2016


#Select data from supermarket1996
data<- supermarket1996[6:50]                        #X
data<- cbind(supermarket1996["GROCERY_sum"],data)   #append y to X

data<- data[sample(nrow(data)),]                    #Shuffle data
#Add column with fold-categories
fold<- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5)
data<- cbind(data,fold)
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
#init
R_sq_lambda <- NULL

#Loop over your lambda range
for (lambda in lambda_range){
    
    #We use the Ridge-function, we have to store results in a matrix 
    
    #Initialize a matrix for estimates
    b_estimates <- NULL
    R_sq_holdout<- NULL
    
    #Start looping over all k folds, use k-fold- function
    R_sq <- k_fold_function(k,X,y,lambda)
    R_sq_lambda<- rbind(R_sq_lambda,c(lambda,R_sq))
}

#find optimal lambda

# do ridge for each lambda to get Beta for each lambda

#plot 