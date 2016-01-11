add2 <- function(x,y){
        x +y
}



above10 <- function(x){
        use <- x>10
        x[use] # subset
}


above <- function(x,n= 10){ # default val
        use <- x>n
        x[use]
        
}

colummean <- function(y, removeNA =TRUE){
        nc <-ncol(y)
        means <-numeric(nc) ## alocate 
        for(i in 1:nc){
                means[i] <-mean(y[,i], na.rm =removeNA)
        }
        means
}