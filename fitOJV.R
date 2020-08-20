# This script expects a csv file 
# containg 3 variables
# the first column should contain a variable for some type of classification
# the second and the third columns should contain data from the national survey 
# and from job advertisments data, sharing the common classification

# Input has two paramteters is a path string pointing to the csv file
# The first parameter is a path string pointing to the csv file
# The second parameter is a string or a custom function describing the 
# univariate probability density function for a discrete random variable.
# Output is a pdf created in the current working directory.


library(fitdistrplus)
library(grid)
library(gridExtra)

fitOJV <- function(path = NULL, distr = NULL){
  
  path <- path

  df <- read.csv(path, stringsAsFactors = FALSE)
  
  
  cat("Searching for approximate probability distribution familly")
  
  output <- list()
  
  # write results to pdf
  pdf("Results.pdf")
  
  set.seed(1234)
  
  descdist(c(na.exclude(df[, 2])), discrete = TRUE, boot = 1000)
  output$cf[[names(df)[2]]] <- descdist(c(na.exclude(df[, 2])), discrete = TRUE, boot = 1000)
  res <- data.frame(statistic = names(output$cf$JV), value = unlist(output$cf$JV))
  grid.newpage()
  gridExtra::grid.table(res, rows = NULL)

    
  descdist(c(na.exclude(df[, 3])), discrete = TRUE, boot = 1000)
  output$cf[[names(df)[3]]] <- descdist(c(na.exclude(df[, 3])), discrete = TRUE, boot = 1000)
  res <- data.frame(statistic = names(output$cf$JVAd), value = unlist(output$cf$JVAd))
  grid.newpage()
  gridExtra::grid.table(res, rows = NULL)
  
  
  
  cat("Searching for start parameters...")
  
  
  output$mle[[names(df)[2]]] <- fitdist(c(na.exclude(df[, 2])), distr = distr)
  res <- data.frame(names = names(unlist(output$mle$JV[-c(4, 5, 10)])), value = unlist(output$mle$JV[-c(4, 5, 10)]))
  plot(output$mle$JV)
  grid.newpage()
  gridExtra::grid.table(res, rows = NULL)
  
 
  
  output$mle[[names(df)[3]]] <- fitdist(c(na.exclude(df[, 3])), distr = distr)
  res <- data.frame(names = names(unlist(output$mle$JVAd[-c(4, 5, 10)])), value = unlist(output$mle$JVAd[-c(4, 5, 10)]))
  plot(output$mle$JVAd)
  grid.newpage()
  gridExtra::grid.table(res, rows = NULL)
 
  
  dev.off()
    
}