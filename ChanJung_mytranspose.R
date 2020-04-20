
#### 2019181813 ±èÂùÁß 

## mytranspose function

mytranspose <- function(x) {
  
  
  # myvar1
  if(is.matrix(x) && nrow(x)>0 ) {
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] <- x[i,j]
      }
    }
    per <- (t(x) == y)
    l.true <- (length(which(per=="TRUE"))/(nrow(x)*ncol(x)))*100
    return(list(value=y,percentage=paste(l.true,"%",sep = "")))
    
    
  }
  else if(is.matrix(x) && nrow(x)==0){return("<0 x 0 matrix>")}
  
  
  # myvar2
  if(is.vector(x)) {
    x <- as.matrix(x)
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] <- x[i,j]
      }
    }
    y <- as.vector(y)
    per <- (t(x) == y)
    l.true <- (length(which(per=="TRUE"))/(nrow(x)*ncol(x)))*100
    return(list(value=y,percentage=paste(l.true,"%",sep = "")))
    
  }
  else if(is.null(x)){return("null")}

  
  # myvar3
  if(is.data.frame(x)) {
    x <- as.matrix(x)
    y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] <- x[i,j]
      }
    }
    y <- as.data.frame(y)
    per <- (t(x) == y)
    l.true <- (length(which(per=="TRUE"))/(nrow(x)*ncol(x)))*100
    return(list(value=y,percentage=paste(l.true,"%",sep = "")))
    
  }
  
}




