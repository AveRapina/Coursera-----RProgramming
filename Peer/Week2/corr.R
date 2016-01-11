path <-getwd()
setwd(path)

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        if(grep("specdata",directory)==1){
                directory <- ("./specdata/")# set data directory
        }
        
        # get the complete cases on data
        completeTable <- complete("specdata",1:332)
        # get the nobs values
        nobs <-completeTable$nobs
        # find the id that nobs are above threshols
        ids <- completeTable$id[completeTable$nobs>threshold]
        # get the lenght of result vector
        ids_len <- length(ids)
        # find the files on the path
        allFiles <- as.character(list.files(directory))
        # ggenerate the path to be readed
        filePaths <- paste(directory,allFiles,sep="")
        # preallocate corrVector to speed things
        corrVector <- rep(0,ids_len)
        # iterate over the ids that are above thresh
        j<- 1
        for(i in ids){
                # ectract the cvs data
                currentData <- read.csv(filePaths[i],header=T, sep=",")# extract data
                # get the correpatio between sulfate and nitrate excludes non complete
                corrVector[j]<-cor(currentData$sulfate, currentData$nitrate, use ="complete.obs")
                j <-j+1
        }
        return (corrVector)
        
}


#TESTS
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
