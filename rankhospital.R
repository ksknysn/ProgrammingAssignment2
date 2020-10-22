rankhospital <- function(state, outcome, num="best") {
  f<-read.csv("./hospitaldata/outcome-of-care-measures.csv", colClasses = "character")
  
  if(outcome=="heart attack"){p<-11}
  else if(outcome=="heart failure"){p<-17}
  else if(outcome=="pneumonia"){p<-23}
  
  x<-f[f$State==state&!is.na(as.numeric(f[,p])),c(2,p)]
  x1<-x[order(as.numeric(x[,2]),x[,1]),]
  
  if(num=="best"){n<-1}
  else if(num=="worst"){n<-dim(x1)[1]}
  else(n<-num)
  
  x1[n,]
}
