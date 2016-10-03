Calculate_Yk <- function(Dk,Gk,X)
{
  
  Yk <- solve(Dk) %*% t(Gk) %*% X #calculate Yk  
  
  
  return(Yk)
  
  
}