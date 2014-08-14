corr <- function(directory, threshold = 0) 
{
  ##----------------------------------------------------------------------##
  ## 'directory' is the directory where the specdata folder with the CSV files are stored 
  ## E.g. /users/test/Coursera . the specdata folder should be under the directorys
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ##----------------------------------------------------------------------##
  
  Corrdata <- numeric() ## initialize the vector
  
  for (i in 1:332) 
  {     
    id_reformat <- formatC(i, width=3, flag="0") ## reformat the id to append to file name
    data <- read.csv(paste("/users/Dhruvie/desktop/Coursera/specdata/",id_reformat,".csv",sep="") ) ## Read the CSV file
    data_c <- data.frame() # empty data frame that will hold all the records that have complete cases
    NCompleteRows = 0
    for (j in 1:nrow(data)) 
    {
      if (complete.cases(data[j,])) 
      {
        data_c <- rbind(data_c,data[j,])
        NCompleteRows = NCompleteRows + 1
      }
    }
    if (NCompleteRows == 0) 
    { 
      CorrData <- 0 
    }
    else
      if (NCompleteRows > threshold)
      {
        corr_output <- cor(data_c[2],data_c[3])   
        Corrdata <- c(Corrdata,corr_output)     ## Append 
      }
  }
  return(Corrdata)
}
