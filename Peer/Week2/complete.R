path <-getwd()
setwd(path)

complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        ## Return a data frame of the form:
        ## id nobs
        ## 1 117
        ## 2 1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        if(grep("specdata",directory)==1){
                directory <- ("./specdata/")# set data directory
        }
        
        # get all files in specfolder
        allFiles <- as.character(list.files(directory))
        filePaths <- paste(directory, allFiles, sep="") # generate array paths for the folder
        len <- length(id)
        completeData <-rep(0,len) # prealloocate for speed
        j<-1
        ## loop in all data
        for (i in id){
                # get the data
                currentData <- read.csv(filePaths[i], header=T, sep=",")
                good <- complete.cases(currentData)# get the complete cases
                # get the sum of complete cases and save to array
                completeData[j] <- sum(complete.cases(currentData[good,]))
                
                #completeData[j] <- sum(complete.cases(currentData))
                j <- j + 1
        }
        # generate the data frame with the id and the number of complete cases
        result <- data.frame(id = id, nobs =completeData)#create data frame with results
        return (result)
                
}


# TESTS
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

