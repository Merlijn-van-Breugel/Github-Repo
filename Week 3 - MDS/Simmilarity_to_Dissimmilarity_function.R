function_similarity_to_dissimilarity <- function(S){
    D <- matrix(0,dim(S)[1],dim(S)[2])
    for (i in 1:dim(S)[1]){
        for (j in 1:dim(S)[2]){
            D[i,j] = (S[i,j]^(-2) - (S[i,i]*S[j,j])^(-1)) * 10^6 #Scale for readability
        }
    }
    colnames(D) <- colnames(S)
    rownames(D) <- colnames(S)
    return(D)
}
