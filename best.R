## script from course project in courser r progrmming (rprog-010) - function that takes two variables, "state," and "outcome" and returns
## the best performing hospital name in that state, for that outcome. Three outcomes are "heart attack," "heart failure," and pneumonia.
## data set for this script had numerous columns, with column 7 being state short code, 11 being heart attack outcome (lower outcome is better)
## 17 being heart failure outcome, and 23 being pneumonia outcome

best <- function(state, outcome) {

  install.packages("gtools")  ## install package for sorting character vector
  library(gtools)
  
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE) ## read data
  
    if (!(state %in% data$State)) 
      stop("invalid state") ## check for valid state input
 
    if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia")
      stop("invalid outcome") ## check for valid outcome input
  
  newdata <- subset(data, data[,7] == state) ## subset data by state
  
  if (outcome == "heart attack"){
      a <- mixedsort(newdata[,11])[1] ## sort & extract min value of heart attack clmn
      a1 <- sort(subset(newdata, newdata[,11] == a, select=c(Hospital.Name))) ## return vector of hospital names sorted alphabetically
      return(as.character(a1[1])) ##print first hospital name
}      
      else if(outcome == "heart failure") {
        b <- mixedsort(newdata[,17])[1] ## sort & extract min value of heart failur clmn
        b1 <- sort(subset(newdata, newdata[,17] == b, select=c(Hospital.Name))) ## return vector of hospital names sorted alphabetically
        return(as.character(b1[1])) ##print first hospital name
}
      else if(outcome == "pneumonia") {
        c <- mixedsort(newdata[,23])[1] ## sort & extract min value of pneumonia clmn
        c1 <- sort(subset(newdata, newdata[,23] == c, select=c(Hospital.Name))) ## return vector of hospital names sorted alphabetically
        return(as.character(c1[1])) ##print first hospital name
}
}