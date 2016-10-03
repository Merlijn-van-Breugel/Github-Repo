#' Create a super indicator matrix of dummies 
#' 
#' @export
#' @description Creates a super indicator matrix of dummies out of an n by m matrix 
#' of factors
#' @usage as.dummy(X, nlevels = NULL)
#' @param X an n by m matrix of categorical variables. If a variable is a factor, then 
#' the factor level is used for determining the number of categories; this can lead to 
#' a column of zeros for a level that does not occur. 
#' @param nlevels vector of length m with number of categories. If specified, the values 
#' in \code{X} for column \code{j} are assumed to be \code{1:nlevel[j]}. Default is 
#' \code{nlevels = NULL}
#' @return  Returns a list with 
#' \item{G}{the super indicator matrix of the m variables with a dummy variable 
#' for each category of each variable.}
#' \item{nlevel}{a vector of length m with the number of categories per variable}
#' @examples data(jobsatisfaction)
#' G <- as.dummy(jobsatisfaction)

as.dummy <- function(X, nlevels = NULL){
  # Return superindicator matrix
  n <- nrow(X)
  m <- ncol(X)
  if (is.null(nlevels)) nlevel <- rep(0, m) 
  cat.labels <- list(NULL)
  G <- matrix(0, nrow = n, ncol = 0)
  for (j in 1:m){
    if (is.factor(X[,j])){
      cat.labels.j <- levels(X[,j])
      cat.labels   <- c(cat.labels, cat.labels.j)
      nlevel[j]    <- length(cat.labels.j)
    } else if (is.null(nlevels)){
      cat.labels.j <- sort(unique(X[,j],nmax = NA))
      cat.labels   <- c(cat.labels, cat.labels.j)
      nlevel[j]    <- length(cat.labels.j)
    } else {
      cat.labels.j <- 1:nlevels[j]
      cat.labels   <- c(cat.labels, cat.labels.j)
      nlevel[j]    <- nlevels[j]
    }
    Gj <- matrix(as.numeric(outer(X[,j], cat.labels.j, "==")), n, nlevel[j])
    if(is.null(nlevels)){
      colnames(Gj) <- cat.labels.j
    } else {
      colnames(Gj) <- as.character(cat.labels.j)
    }
    G <- cbind(G, Gj)
  }
  rownames(G) <- rownames(X)
  return(list(G = G, nlevel = nlevel))
}