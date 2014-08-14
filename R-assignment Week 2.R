pollutantmean <- function(directory, pollutant, f_id = 1:332) {
  ##----------------------------------------------------------------------##
  ## 'directory' is the directory where the specdata folder with the CSV files are stored 
  ## E.g. /users/test/Coursera . the specdata folder should be under the directorys
    
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
    
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
    
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  ##----------------------------------------------------------------------##
  Alldata <- data.frame() ## initialize the data frame
  for (i in f_id) {     
       id_reformat <- formatC(i, width=3, flag="0") ## reformat the id to append to file name
       data <- read.csv(paste(directory,"/specdata/",id_reformat,".csv",sep="") ) ## Read the CSV file
       Alldata <- rbind(Alldata,data)     ## Append 
  }

  ColData <- Alldata[,c(pollutant)]  ## Subset Col which has the pollutant data
  ColMean <- round(mean(ColData,na.rm = TRUE),3) ## Find the columnmeans and round it off to 3 digits
  return(ColMean) ## Return the column mean

}

complete <- function(directory, f_id = 1:332) {
   Alldata <- data.frame(id=numeric(0),nobs=numeric(0)) ## initialize the data frame
    for (i in f_id) {     
       id_reformat <- formatC(i, width=3, flag="0") ## reformat the id to append to file name
         data <- read.csv(paste(directory,"/specdata/",id_reformat,".csv",sep="") ) ## Read the CSV file
       data_c <- data.frame()
       for (j in 1:nrow(data)) {
            if (complete.cases(data[j,])) 
                  data_c <- rbind(data_c,data[j,])
          }
       newrow <- c(i,nrow(data_c))        
       Alldata <- rbind(Alldata,newrow)     ## Append 
       }
  colnames(Alldata) <- c("ID", "NObs")
  return(Alldata)
 }

corr <- function(directory, threshold = 0) 
{
 # CompleteRows <- complete(directory)
 # NCompleteRows <- sum(CompleteRows$NObs)
  Corrdata <- numeric() ## initialize the vector
# if (NCompleteRows >= threshold)
 #{
   for (i in 1:332) 
   {     
     id_reformat <- formatC(i, width=3, flag="0") ## reformat the id to append to file name
     data <- read.csv(paste(directory,"/specdata/",id_reformat,".csv",sep="") ) ## Read the CSV file
     data_c <- data.frame()
     NCompleteRows = 0
     for (j in 1:nrow(data)) 
     {
       if (complete.cases(data[j,])) 
       {
         data_c <- rbind(data_c,data[j,])
          NCompleteRows = NCompleteRows + 1
       }
     }
     if (NCompleteRows == 0) { CorrData <- 0 }
     else
     if (NCompleteRows > threshold)
       {
        corr_output <- cor(data_c[2],data_c[3])   
        Corrdata <- c(Corrdata,corr_output)     ## Append 
        }
 }
return(Corrdata)
}
