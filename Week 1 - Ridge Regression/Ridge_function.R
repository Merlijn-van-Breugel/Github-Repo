
# Title:    Week 1 Ridge Regression - Ridge Function
# Course:   Multivariate Statistics
# Authors:  Merlijn van Breugel
# Date:     01-09-2016

ols_ridge_function <- function(X,y,lambda,center,standardize){

    X_scaled <- as.matrix(scaling_function(X, center, standardize))
    y_scaled <- as.matrix(scaling_function(y, center, FALSE))
    size_X  <- dim(X_scaled)
    
    I_lambda<- matrix(0,ncol(X_scaled),ncol(X_scaled))
    I_lambda_sqrt<- I_lambda
    diag(I_lambda) <- lambda
    diag(I_lambda_sqrt) <- sqrt(lambda)
    
    X_ridge <- rbind(X_scaled,I_lambda_sqrt)
    y_zeros <- matrix(0,ncol(X_scaled),1)
    y_ridge <- rbind(as.matrix(y_scaled),y_zeros)          
    
    b <- solve(t(X_ridge) %*% X_ridge) %*% t(X_ridge) %*% y_ridge
    y_est <- X_scaled %*% b
    e <- y_scaled - y_est
    e_sq <- t(e) %*% e
    s_sq <- c(e_sq/(size_X[1][1]-size_X[2][1]))
    inv_X_lambda <- solve(t(X_scaled) %*% X_scaled + I_lambda)
    b_var <- s_sq * inv_X_lambda %*% t(X_scaled) %*% X_scaled %*% inv_X_lambda 
    b_stdev <- sqrt(diag(b_var))
    SST <- (t(y_scaled) %*% y_scaled)
    R_sq <- 1 - e_sq/SST
    
    hat_matrix <- X_scaled %*% inv_X_lambda %*% t(X_scaled)   
    df <- sum(diag(hat_matrix))  
    
    BIC_ridge <- size_X[1][1]*log(e_sq) + 2*df*log(size_X[1][1])
    AIC_ridge <- size_X[1][1]*log(e_sq) + 2*df
    
    ols_ridge <- lm(y_ridge ~ 0 + X_ridge)
    B <- cbind(b,b_stdev)
    colnames(B) = c('estimates','S.E.')

    S <- cbind(R_sq,BIC_ridge,AIC_ridge)
    colnames(S) = c('R^2','BIC_ridge','AIC_ridge')
    result <- list(B,S)
    return(result)
    
    #return(summary(ols_ridge))
    #Use do.call(rbind.data.frame, ols_ridge_function(dataX,datay,10,TRUE)[1]) to get matrix B
    #Use do.call(rbind.data.frame, ols_ridge_function(dataX,datay,10,TRUE)[1])[,2] to get estimates etc.
}

