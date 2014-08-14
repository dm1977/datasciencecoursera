complete <- function(directory, f_id = 1:332) {
  ##----------------------------------------------------------------------##
  ## 'directory' is the directory where the specdata folder with the CSV files are stored 
  ## E.g. /users/test/Coursera . the specdata folder should be under the directorys
  
  ## 'f_id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ##----------------------------------------------------------------------##
  
  Alldata <- data.frame(id=numeric(0),nobs=numeric(0)) ## initialize the data frame
  for (i in f_id) 
  {     
    id_reformat <- formatC(i, width=3, flag="0") ## reformat the id to append to file name
    data <- read.csv(paste("/users/Dhruvie/desktop/Coursera/specdata/",id_reformat,".csv",sep="") ) ## Read the CSV file
    data_c <- data.frame()
    for (j in 1:nrow(data)) 
    {
      if (complete.cases(data[j,])) 
          data_c <- rbind(data_c,data[j,])
    }
    newrow <- c(i,nrow(data_c))        
    Alldata <- rbind(Alldata,newrow)     ## Append 
  }
  colnames(Alldata) <- c("id", "nobs") # Create rowlabels
  return(Alldata)
}