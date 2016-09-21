#Function which computes Euclidian distances of matrix X, returns distance matrix D 
function_euclidean_distance <- function(X){
  p<-ncol(X)
  n<-nrow(X)
  D<-matrix(,n,n) #distance matrix
  for (i in 1:n){
    for(j in 1:n){
      sum_dist<-0
      for(s in 1:p){
        sum_dist<-sum_dist + (X[i,s]-X[j,s])^2
      }
      D[i,j]<-sqrt(sum_dist)
    }
  }
 return(D)
}
