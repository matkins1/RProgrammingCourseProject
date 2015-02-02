## script from course project in courser r progrmming (rprog-010) - function that takes three variables, "state," "outcome," and "num" 
## and returns the best, worst, or specific # performing hospital name in that state, for that outcome. Three outcomes are 
## "heart attack," "heart failure," and pneumonia. data set for this script had numerous columns, with column 7 being state short code, 
## 11 being heart attack outcome (lower outcome is better), 17 being heart failure outcome, and 23 being pneumonia outcome


rankhospital <- function(state, outcome, num = "best") {
  
  install.packages("gtools")  ## install package for sorting character vector
  library(gtools)
  
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE) ## read data
  
  if (!(state %in% data$State)) 
    stop("invalid state") ## check for valid state input
  
  if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia")
    stop("invalid outcome") ## check for valid outcome input
  
  if (num != "best" & num != "worst" & !is.numeric(num))
    stop("invalid outcome") ## check for valid num input
  
  newdata <- subset(data, data[,7] == state) ## subset data by state

  if (num == "best"){  ## code to run if num = "best"
  
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
  else if (num == "worst"){  ## code to run if num = "worst"
    
    if (outcome == "heart attack"){
      a <- mixedsort(newdata[,11]) ## sort & extract min value of heart attack clmn
      a1 <- a[a != "Not Available"] ## remove "Not Available" elements
      a2 <- rev(a1)[1] ## reverse sort to get worst element and extract first value
      a3 <- sort(subset(newdata, newdata[,11] == a2, select=c(Hospital.Name))) ## return vector of hospital names sorted alphabetically
      return(as.character(a3[1])) ##print first hospital name
    }      
    else if(outcome == "heart failure") {
      b <- mixedsort(newdata[,17]) ## sort & extract min value of heart failure clmn
      b1 <- b[b != "Not Available"] ## remove "Not Available" elements
      b2 <- rev(b1)[1] ## reverse sort to get worst element and extract first value 
      b3 <- sort(subset(newdata, newdata[,17] == b2, select=c(Hospital.Name))) ## return vector of hospital names sorted alphabetically
      return(as.character(b3[1])) ##print first hospital name
    }
    else if(outcome == "pneumonia") {
      c <- mixedsort(newdata[,23]) ## sort & extract min value of pneumonia clmn
      c1 <- c[c != "Not Available"] ## remove "Not Available" elements
      c2 <- rev(c1)[1] ## reverse sort to get worst element and extract first value 
      c3 <- sort(subset(newdata, newdata[,23] == c2, select=c(Hospital.Name))) ## return vector of hospital names sorted alphabetically
      return(as.character(c3[1])) ##print first hospital name
    }
  }
  else {  ## code to run if num is numeric
    
    if (outcome == "heart attack"){
      a <- mixedsort(newdata[,11])[num] ## sort & extract value of heart attack clmn
      a1 <- subset(newdata, newdata[,11] == a, select=c(Hospital.Name)) ## return vector of hospital names
      a2 <- a1[order(a1),]## sort vector of hospital names alphabetically
      if (length(a2) == 1) ## check number of hospitas in list
        return(as.character(a2[1])) ##print first hospital name
      else
        return(as.character(a2[2])) ##print second hospital name  
    }      
    else if(outcome == "heart failure") {
      b <- mixedsort(newdata[,17])[num] ## sort & extract value of heart failur clmn
      b1 <- subset(newdata, newdata[,17] == b, select=c(Hospital.Name)) ## return vector of hospital names 
      b2 <- b1[order(b1),]## sort vector of hospital names alphabetically
      if (length(b2) == 1) ## check number of hospitas in list
        return(as.character(b2[1])) ##print first hospital name
      else
        return(as.character(b2[2])) ##print second hospital name
    }
    else if(outcome == "pneumonia") {
      c <- mixedsort(newdata[,23])[num] ## sort & extract value of pneumonia clmn
      c1 <-subset(newdata, newdata[,23] == c, select=c(Hospital.Name)) ## return vector of hospital names
      c2 <- c1[order(c1),]## sort vector of hospital names alphabetically
      if (length(c2) == 1) ## check number of hospitas in list
        return(as.character(c2[1])) ##print first hospital name
      else
        return(as.character(c2[2])) ##print second hospital name
    }
  }  
}