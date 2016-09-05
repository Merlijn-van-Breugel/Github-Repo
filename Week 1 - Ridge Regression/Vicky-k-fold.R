
data<- supermarket1996[6:50]
data<- cbind(supermarket1996["GROCERY_sum"],data)
#randomly assign observations to folds, add column fold to data with foldnumber

fold1 <- data[sample(1:nrow(data),16,FALSE),]
fold <- rep(1,nrow(fold1))
fold1 <- cbind(fold1,fold)

fold2 <- data[sample(1:nrow(data),16,FALSE),]
fold <- rep(2,nrow(fold2))
fold2 <- cbind(fold2,fold)

fold3 <- data[sample(1:nrow(data),15,FALSE),]
fold <- rep(3,nrow(fold3))
fold3 <- cbind(fold3,fold)

fold4 <- data[sample(1:nrow(data),15,FALSE),]
fold <- rep(4,nrow(fold4))
fold4 <- cbind(fold4,fold)

fold5 <- data[sample(1:nrow(data),15,FALSE),]
fold <- rep(5,nrow(fold5))
fold5 <- cbind(fold5,fold)

data<- rbind(fold1,fold2,fold3,fold4,fold5)

#Still need to adjust the lambda
lambda <- c(1,2)

#test<-0
for(i in 1:length(lambda)){
    y_holdout<-c()
    for(k in 1:5){
        #fill training matrix with 4 folds
        data_train <-data[data$fold != k,]
        X_train <-data_train[2:(ncol(data_train)-1)]
        y_train <-data_train[1]
        
        #fill test matrix with 1 fold
        data_holdout <-data[data$fold == k,]
        X_holdout <-data_holdout[2:(ncol(data_holdout)-1)]
        y_test <-data_holdout[1]
        
        #ridge regression on training data to get b_train
        result<- as.data.frame(ols_ridge_function(X_train,y_train,lambda[i],1))##Still need to save the beta's and criteria
        b_train<- as.matrix(result$estimates)
        
        #estimate y_holdout and save
        y_holdout_k<- as.matrix(X_holdout) %*% b_train
        y_holdout<-rbind(y_holdout,y_holdout_k)
        e_holdout<- 
    }
    #e_holdout       <- y_holdout_scaled - X_holdout_scaled %*% b_estimates[,ncol(b_estimates)]
    #e_sq            <- t(e_holdout) %*% e_holdout
    #SST             <- t(y_holdout_scaled) %*% (y_holdout_scaled)
    #R_sq_holdout    <- cbind(R_sq_holdout,1 - e_sq/SST)
    #colnames(R_sq_holdout)[ncol(R_sq_holdout)] <- paste("Fold #",toString(i))
}
k fold cross Vicky.txt
Openen met
k fold cross Vicky.txt wordt weergegeven.