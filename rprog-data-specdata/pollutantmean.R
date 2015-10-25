pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  dat <- NA
  for (i in id) {
    num <- i
    if (i<10) {
      num <- paste("00",i,".csv",sep="")
      
    } else if (i<100) {
      num <- paste("0",i,".csv",sep="")
    } else {
      num <- paste(i,".csv",sep="")
    }
    dir2 <- paste(directory,num,sep="/")
    datNew <- read.csv(dir2)[[pollutant]]
    dat <- c(dat,datNew)
  }
  bad <- is.na(dat)
  dat <- dat[!bad]
  mean(dat)
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
}