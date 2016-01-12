path <- getwd()
setwd(path)




# get the hospital name
getHospName <- function(data, colNum, state){
        stateSubset <- data[data[,7]==state,] # het the heart attack from the named states
        outcomeArr <- stateSubset[,colNum] # subset data from that collum
        min <- min(outcomeArr, na.rm=T) # get minumum and remove NA
        minIdx <- which(outcomeArr == min) # get the index of min
        hospName <- stateSubset[minIdx, 2] # get the hospital name
        return(hospName)
}


best<-function(state, outcome){
        ## Read outcome data
        ## Ccjeck that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate 
        
        
        # set the directory
        directoty <- directory <- "./data4/outcome-of-care-measures.csv"
        # read the csv
        data <- read.csv(directory, colClasses ="character")
        ## convert to humeric the data of interest
        data[, 11] <- as.numeric(data[,11]) # heart attack
        data[, 17] <- as.numeric(data[,17]) # heart failure
        data[, 23] <- as.numeric(data[,23]) # pneumonia
        
        # create vector with the only valid outcomes possible
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        ## Test for the arguments
        if(!state %in% data$State){
                stop("invalid state entered")
        }else if(!outcome %in% validOutcomes){
                stop("invaled outcome entered")
        }else{ ## do the seelction
                if(outcome=="heart attack"){
                        hospName <-getHospName(data,11,state)
                        
                }else if(outcome=="heart failure"){
                        
                        hospName <-getHospName(data,17,state)
                }else{
                        
                        hospName <-getHospName(data,23,state)
                }
                
                result <- hospName
                return (result)
                
        }
        
}


## Tests
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")