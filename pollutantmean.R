## code creates a function to find the mean of a column of several combined files. directory argument sets the working directory where files should be located. pollutant argument sets the column (original files where various rows long and four columns; date, sulfate, nitrate, and id, in order left to right). id argument sets the file or range of files to use (there were 333 files in this data set starting with 001.csv and ending with 332.csv).

pollutantmean <- function(directory, pollutant, id = 1:332) {
  filenames <- sprintf("%03d.csv", id)
  filenames <- paste(directory, filenames, sep="/") ## outputs called files to filenames
  
  for (i in filenames)  ## iterates over files and combines files into on datafile
    
    if (!exists("datafile")){
      datafile <- read.csv(i, header=TRUE)
    }
  
    else if (exists("datafile")){  
      temp_datafile <- read.csv(i, header=TRUE)
      datafile <- rbind(datafile, temp_datafile)
      rm(temp_datafile)
    }
  
    if(pollutant == "nitrate") ## transforms column argument "pollution" to proper column number ("nitrate" was column 3 and "sulfate" was column 2)
      x <- 3
    else 
      x <- 2
  
    round(mean(datafile[, x], na.rm = TRUE), 3) ## finds mean of column and rounds to three decimal places
}