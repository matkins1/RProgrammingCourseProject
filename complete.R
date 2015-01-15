## code creates a function to sum rows by file id for one or several combined files. 
##"directory" argument sets the working directory where files should be located. 
## (original files where various rows long and four columns; date, sulfate, nitrate, and id, in order left to right). 
##"id" argument sets the file or range of files to use (there were 333 files in this data set starting with 
##001.csv and ending with 332.csv)
complete <- function(directory, id = 1:332) {
  filenames <- sprintf("%03d.csv", id)
  filenames <- paste(directory, filenames, sep="/")  ## outputs called files to "filenames"
  
  for (i in filenames) ## iterates over files and combines files into on "datafile"
    
    if (!exists("datafile")){
      datafile <- read.csv(i, header=TRUE)
    }
  
  else if (exists("datafile")){  
    temp_datafile <- read.csv(i, header=TRUE)
    datafile <- rbind(datafile, temp_datafile)
    rm(temp_datafile)
  }
  
  nobs <- complete.cases(datafile$sulfate, datafile$nitrate) ## collapses "datafile" into "nobs," removing incompletes
  
  nobs2 <- aggregate(nobs ~ ID, data = datafile, FUN = sum) ## sums rows by ID
  
  nobs2 ## prints result
}