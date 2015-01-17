### function corr to take 333 number of original files of pattern 001.csv to 332:csv, located in "directory" an output a vector of the correlations between two variables (two separate columns labeled "sulfate" and "nitrate" in the example files) where there are an observable number of values in each file over the "threshold" amount (format of each .csv file is variable number of rows with four columns: Data, sulfate, nitrate, and ID. Date column is not utilized in this code. sulfate and nitrate columns either contain a number or an "NA." "NA" observations are filtered out in code. ID for each row is a number x where x is the number in the .csv file name [for instance, ID for each row in 001.csv file would be 1].)

corr <- function(directory, threshold = 0) { ## create function corr
  filenames <- sprintf("%03d.csv", 1:332) ## create list of filenames
  filenames <- paste(directory, filenames, sep="/")  
  
  for (i in filenames) ## aggregate all files into datafile
    
    if (!exists("datafile")){
      datafile <- read.csv(i, header=TRUE)
    }
  
  else if (exists("datafile")){  
    temp_datafile <- read.csv(i, header=TRUE)
    datafile <- rbind(datafile, temp_datafile)
    rm(temp_datafile)
  }
  nobs <- complete.cases(datafile$sulfate, datafile$nitrate) ## remove incomplete cases in datafile (duplicate? see below)
  
  summarized_data <- aggregate(nobs ~ ID, data = datafile, FUN = sum) ##summarize data file by ID
  
  full_data_set <- datafile[complete.cases(datafile), ] ## remove incomplete cases in datafile (duplicate? see above)
  
  sum_over_threshold <- summarized_data[which(summarized_data$nobs < threshold), ] ## summarize data and remove under threshold cases
  
  id_over_threshold <- sum_over_threshold[ ,"ID"] ## create list of id's over threshold value
  
  for (i in id_over_threshold) ## create final data for all over threshold observations
    if (!exists("final")){
      final <- full_data_set[full_data_set$ID!=i, ]
    }
  
  else if (exists("final")){  
    new_final <- final[final$ID!=i, ]
    rm(final)
    final <- new_final
    rm(new_final)
  }  

  for (id_to_extract in final) ## summarize list of ids to extract
    id_to_extract <- final[ ,"ID"]
  
  id_to_extract <- unique(id_to_extract) 
  
  for (i in id_to_extract) ## create correlation list for all cases where number of observations is over threshold by ID (by ID is also by data file in the example case) 
    
    if (!exists("correlation")){
      a <- final[which(final$ID==i), ]
      nitrate_a <- a[c("nitrate")]
      sulfate_a <- a[c("sulfate")]
      correlation <- c(cor(nitrate_a,sulfate_a))
      rm(a, nitrate_a, sulfate_a)
    }else if (exists("correlation")){  
      a <- final[which(final$ID==i), ]
      nitrate_a <- a[c("nitrate")]
      sulfate_a <- a[c("sulfate")]
      new_correlation <- c(cor(nitrate_a,sulfate_a))
      correlation <- c(correlation, new_correlation)
      rm(a, nitrate_a, sulfate_a, new_correlation)
    }   
  
if (exists("correlation")) ## returns correlation as computed above if correlation vector contains values, else returns an empty vector
  return(correlation)
else
  none_over_threshold <- numeric(length = 0)
  return(none_over_threshold)  
  
}