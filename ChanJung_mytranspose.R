
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
    return(y)
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
    return(y)
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
    return(y)
  }
  
}


# myvar1
myvar1 <-  matrix(1:10, nrow=5, ncol=2)
mytranspose(myvar1)
myvar1 <-  matrix(NA, nrow=0, ncol=0)

mytranspose(myvar1)
myvar1 <-  matrix(c(1,2), nrow=1, ncol=2)
mytranspose(myvar1)
myvar1 <-  matrix(c(1,2), nrow=2, ncol=1)
mytranspose(myvar1)

# myvar2
myvar2 <- c(1,2,NA,3)
mytranspose(myvar2)
myvar2 <- c(NA)
mytranspose(myvar2)
myvar2 <- c()
is.null(myvar2)
mytranspose(myvar2)

# myvar3
d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata3 <- data.frame(d,e,f)
mytranspose(mydata3)


