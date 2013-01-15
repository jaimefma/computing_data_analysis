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

getCleanData <- function(dir, number) {
  pathFile <- composeCSVFileName(dir, number)
  raw <- read.csv(pathFile,header=TRUE)
  raw[complete.cases(raw),]
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  res <- c()
  idx <- 1
  for(i in 1:332){
    cleanData <- getCleanData( directory[1], i )
    if(nrow(cleanData) > threshold) {
        res[idx] <- cor(cleanData$sulfate, cleanData$nitrate)
        idx <- idx + 1
    }
  }
  res
}