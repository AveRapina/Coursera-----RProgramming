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


f <- function(a,b=1, c=2, d=NULL){
        
}

g <-function(a,b){#laizy evaluation
        a^2
}

l <- function(a,b){
        print(a)
        print(b)
}

myplot <- function(x,y, type ="l",...){# maintain remainder argument original function
        plot(x,y,type = type,...) 
}

make.power <-function(n){ # will find n int the enviroment of scope function
        pow <- function(x){
                x^n
        }
        pow
}