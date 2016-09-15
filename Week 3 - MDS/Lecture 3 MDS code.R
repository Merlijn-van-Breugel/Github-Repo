load("Lecture 3/returnvoldis.RData")
library(smacof)
# Do a ratio MDS on the returnvoldis data
res <- mds(returnvoldis, ndim = 2, type = "ratio", eps = 1e-8, verbose = FALSE)
# Show main results
summary(res)   
# Print the Stress-1 value
cat(paste("Stress-1:", formatC(res$stress, digits=4)))
# Make a configuration plot
plot(res, plot.type = "confplot", col="blue", label.conf = list(label = TRUE, col = "blue"), las = "1")
# Make a Shepard plot
plot(res, plot.type = "Shepard", col="blue")
rug(res$delta, side = 1)
rug(res$obsdiss, side = 2, col = "black")
rug(res$confdiss, side = 4, col = "darkgray")

# Make two plots (Stress and bubble plot) next two each other
op <- par(mfrow = c(1,2))
plot(res, plot.type = "stressplot", col="blue", 
     label.conf = list(label = TRUE, col = "blue"), las = "1")
plot(res, plot.type = "bubbleplot", asp = 1, col="blue", 
     label.conf = list(label = TRUE, col = "blue"), las = "1")
par(op)



# Do an ordinal MDS on the returnvoldis data
res <- smacofSym(returnvoldis, ndim = 2, type = "ordinal", eps = 1e-8, verbose = FALSE)
# Show main results
summary(res)   
plot(res, plot.type = "confplot", col="blue", label.conf = list(label = TRUE, col = "blue"), las = "1")
plot(res, plot.type = "Shepard", col="blue")
# Print the Stress-1 value
cat(paste("Stress-1:", formatC(res$stress, digits=4)))

# Do a permutation test 
res.perm <- permtest(res)
# Make two plots (histogram and quantile plot) next two each other
op <- par(mfrow = c(1,2))
hist(res.perm$stressvec, xlab = "Stress Values", main = "Histogram Permutations", xlim = c(0, .3))
abline(v = quantile(res.perm$stressvec, c(0.025, 0.975)), col = "gray")
abline(v = res$stress, col = "red", lwd = 2)
plot(res.perm)
par(op)

