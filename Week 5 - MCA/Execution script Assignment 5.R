setwd("C:\\Users\\Merlijn\\Documents\\GitHub\\Multivariate-Statistics-Github-Repo\\Week 5 - MCA")
source('~/GitHub/Multivariate-Statistics-Github-Repo/Week 5 - MCA/as.dummy.R')
source('~/GitHub/Multivariate-Statistics-Github-Repo/Week 5 - MCA/LossFunction.R')
source('~/GitHub/Multivariate-Statistics-Github-Repo/Week 5 - MCA/UpdateX.R')
source('~/GitHub/Multivariate-Statistics-Github-Repo/Week 5 - MCA/UpdateY.R')
source('~/GitHub/Multivariate-Statistics-Github-Repo/Week 5 - MCA/HomalsMCA.R')
source('~/GitHub/Multivariate-Statistics-Github-Repo/Week 5 - MCA/rotate_Y.R')

#Load packages and source functions
install.packages('homals')        # Install package homals    
library(homals)                   # Load the homals package for nonlinear PCA
source("plot.homals.R")           # Overrides default homals plot command
source("rescale.R")               # Rescales the output of homals

stimezo <- get(load('stimezo.background.Rdata'))
k       <- ncol(stimezo)
n       <- nrow(stimezo)

#Initialize X from standard homals with 1 iteration
res <- homals(data = stimezo, ndim = 2,
              rank = c(2, 2, 2, 2, 
                       2, 2, 2, 2), 
              level= c("nominal", "nominal", "nominal", "nominal",
                       "nominal", "nominal", "nominal", "nominal"),
              iter=1)
X_init  <- res$objscores

#Run standard Homals, this starts automatically with the same X_init
res_st.homals <- homals(data = stimezo, ndim = 2,
              rank  = c(2, 2, 2, 2, 
                        2, 2, 2, 2), 
              level = c("nominal", "nominal", "nominal", "nominal",
                        "nominal", "nominal", "nominal", "nominal"))
#Run our own Homals with initialized X
res_homals <- HomalsMCA(stimezo,2,X_init)   

#Compare both results
res_homals$objscores[1:10,] - rescale(res_st.homals)$objscores[1:10,]

res_homals$cat.centroid[1:2,] - rescale(res_st.homals)$cat.centroid$SEX

res_homals$objscores[1:10,] - rescale(res_st.homals)$objscores[1:10,]
