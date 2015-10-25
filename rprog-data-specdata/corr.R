corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  corrs <- numeric()
  
  for ( i in 1:332 ) {
    num <- i
    if (i<10) {
      num <- paste("00",i,".csv",sep="")
      
    } else if (i<100) {
      num <- paste("0",i,".csv",sep="")
    } else {
      num <- paste(i,".csv",sep="")
    }
    dir <- paste(directory,num,sep="/")
    datNew <- read.csv(dir)
    nas <- complete.cases(datNew)
    if (sum(nas) >= threshold) {
      datNew <- datNew[nas,]
      corrs <- c(corrs,cor(datNew$sulfate,datNew$nitrate))
    }
  }
  corrs
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}