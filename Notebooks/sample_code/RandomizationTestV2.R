# Randomize a set of data, compute mean difference, record % exceeding observed mean
#
# filename is the name of the file containing the data - one column of data w/ header
# n1 and n2 are the sample sizes for the two independent sets of data
# iterations is the number of times to loop through the process -- I usually use 10000
# Randomize a set of data, compute mean difference, record % exceeding observed mean
#
# filename is the name of the file containing the data - one column of data w/ header
# n1 and n2 are the sample sizes for the two independent sets of data
# iterations is the number of times to loop through the process -- I usually use 10000
resample.u.between <- function(n1,n2,iterations) {
  #Read in data and initialize variables
  names(score)[1] <- 'var1'
  nsample <- length(score$var1)
  startn2 <- n1+1
  target <- abs(mean(score$var1[1:n1]) - mean(score$var1[startn2:nsample]))
  y <- 1
  ucomp <- vector()
  uarray <- vector()
  #Loop starts here
  while (y <= iterations) {
    #Randomize the sample
    perm <- sample(score$var1,replace=F)
    #cat(perm,"\n")
    #Get the two samples and compute the absolute difference in means
    mean.first <- mean(perm[1:n1])
    mean.second <- mean(perm[startn2:nsample])
    mean.diff <- abs(mean.first - mean.second)
    #Compare mean.diff to target and store in ucomp array
    if (mean.diff >= target) {
      ucomp [y] <- 1
    } else {
      ucomp [y] <- 0 }
    y <- y+1 
  }
  #Loop ends above here
  #Compute mean of ucomp which is proportion of u >= target
  uexceed <- mean(ucomp)
  #cat(uarray,"\n")
  #cat(ucomp,"\n")
  cat("Observed mean difference: ",target,"\n")
  cat("Proportion exceeding observed mean difference: ",uexceed)
}
resample.u.between('sampletimes.txt',11,9,10000)

#For testing:
filename <- 'NPS_GTM_WebEx.txt'
n1 <- 36
n2 <- 31
iterations <- 10000