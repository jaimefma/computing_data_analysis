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

getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## Your code here
  dataPath <- paste(paste(directory[1],threeDigitsFileName(id[1]), sep="/" ), ".csv", sep="")
  data <- read.csv(dataPath, header=TRUE)
  if( summarize ){
    summary(data)
  }
  data
}

# print(threeDigitsFileName(3))
# print(threeDigitsFileName(23))
# print(threeDigitsFileName(100))
# print(threeDigitsFileName(121))
# 
# data <- getmonitor(1, "specdata")
# head(data)
# data <- getmonitor(101, "specdata", TRUE)
# data <- getmonitor("200", "specdata", TRUE)

