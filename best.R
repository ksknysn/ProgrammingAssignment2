best <- function(state, outcome) {
  f<-read.csv("./hospitaldata/outcome-of-care-measures.csv", colClasses = "character")
  
  if(!any(state==f["State"])){
    stop("Invalid State")
  }else if(!any(outcome==c("heart attack","heart failure", "pneumonia"))){
    stop("Invalid outcome")
  }
  
  if(outcome=="heart attack"){p<-11}
  else if(outcome=="heart failure"){p<-17}
  else if(outcome=="pneumonia"){p<-23}
  
  x<-f[f$State==state&!is.na(as.numeric(f[,p])),]
  x[which.min(x[,p]),c(2,p)]
}
