
columnmean <- function(y, removeNA = TRUE){
  nc <-ncol(y)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(y[,i], na.rm = removeNA)
  }
  means
}
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  vectordata <- c()
  for(i in id){
    if(i<10){
      index <- paste("00", i, sep='')
    }else if(i<100){
      index <- paste("0", i, sep='')
    }
    filename<-paste('./',directory,'/', index, '.csv', sep='')
    origdata <- read.csv(filename)
    dataset <- na.omit(testdata[pollutant])
    vectordata <-c(vectordata, dataset[,1])
  }
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!  
  return(mean(vectordata))
}
