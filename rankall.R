rankall <- function(outcome, num="best") {
  f<-read.csv("./hospitaldata/outcome-of-care-measures.csv", colClasses = "character")
  
  if(outcome=="heart attack"){p<-11}
  else if(outcome=="heart failure"){p<-17}
  else if(outcome=="pneumonia"){p<-23}
  
  if(num=="best"){n<-1}
  else if(num=="worst"){n<-dim(x1)[1]}
  else(n<-num)
  
  x<-f[!is.na(as.numeric(f[,p])),c(2,7,p)]
  
  states<-unique(f[["State"]])
  states<-sort(states)
  
  df<-data.frame(matrix(ncol=2, nrow=0))
  colnames(df)<-c("Hospital","State")
  
  print(dim(df))
  #sort all states to find the hospital
  for(i in 1:length(states))
  {
    x1<-x[x[2]==states[i],]
    
    x2<-x1[order(as.numeric(x1[,3]),x1[,1]),]
  
    h<-x2[n,]
    s<-states[i]
    df<-rbind(df, c(h[[1]],s))
  }
  
  df
}
