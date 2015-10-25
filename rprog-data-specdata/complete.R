complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  #numel <- length(id)
  d <- data.frame( id=integer(), nobs=integer() )
  for ( i in id ) {
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
    #d$id[i] <- i
    #d$nobs[i] <- sum(nas)
    d <- rbind(d,data.frame(id=i,nobs=sum(nas)))
  }
  d
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}