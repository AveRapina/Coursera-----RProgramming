path <-getwd()
setwd(path)

# function
pollutantmean <- function(directory, pollutant ="sulfate", id =1:332){
        
        if(grep("specdata",directory)==1){
                directory <- ("./specdata/")# set data directory
        }
        # initialize a vector to hold data
        meanVector <-c()
        # locate all files on folder
        allFiles <- as.character(list.files(directory)) # create the list of files
        filePaths <- paste(directory,allFiles, sep="") # generate the path for files
        # loop
        for(i in id){
                currentData <-read.csv(filePaths[i],header=T,sep=",")# get the data from csv
                head(currentData)
                pollutant
                dataCleaned <-currentData[!is.na(currentData[,pollutant]),pollutant]# get the data 
                meanVector <-c(meanVector,dataCleaned)# compute mean concatenating
        }
        result <-mean(meanVector)
        return(round(result,3))
}

# Test
pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")


