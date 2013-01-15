threeDigitsFileName <- function(input) {
  x <- c()
  number <- as.integer(input)
  i = 1
  if( number <= 0 ) {
    print("Wrong file specified")
  } 
  if ( number < 10 ) {
    x[i] <- 0
    i <- i+1
  } 
  if ( number < 100) {
    x[i] <- 0
    i <- i+1
  }
  x[i] <- number
  paste(x, collapse="")  
}

composeCSVFileName <- function(dir, number) {
  paste(paste(dir[1],threeDigitsFileName(number), sep="/" ), ".csv", sep="")
}

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  nobs <- c()
  idx = 1
  for(i in id){
    fileName <- composeCSVFileName(directory, i)
    rawData <- read.csv(fileName, header=TRUE)
    cleanData <- rawData[complete.cases(rawData),]
    nobs[idx] <- nrow(cleanData)
    idx <- idx + 1
  }
  
  data.frame(id, nobs)
}

