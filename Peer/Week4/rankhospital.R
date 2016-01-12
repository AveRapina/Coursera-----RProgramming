
path<-getwd()
setwd(path)

# get hospital rank
getRank <- function(data, colNum, state, num){
        stateSubset <- data[data[,7]==state,] # get the subset of state
        # get the vallues of heart attack, heart failure, pneumonia of that state
        outcomeArr <- stateSubset[,colNum]
        len <- dim(stateSubset[!is.na(outcomeArr),])[1]
        if(num == "worst"){
                rank <-getRankHelper(stateSubset, outcomeArr, len) # get last =len
        }else if(num >len ){
                rank <-NA
        }else{
                rank <-getRankHelper(stateSubset, outcomeArr, num) # get the rak number
        }
        
        result <- rank
        return (result)
}

# helper to get the rank result
getRankHelper <- function(stateSubset, outcomeArr, num){
        result <- stateSubset[,2][order(outcomeArr,stateSubset[,2])[num]]
        return (result)
}


rankhospital <- function(state, outcome, num="best"){
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
        }else{
                if(num == "best"){
                        rank <-best(state,outcome)
                }else{
                        if(outcome == "heart attack"){
                                rank <- getRank(data, 11,state, num)
                        }else if(outcome == "heart failure"){
                                rank <- getRank(data, 17,state, num)
                        }else{
                                rank <- getRank(data, 23,state, num)
                        }
                        
                        
                }
                result <-rank
                return (result)
                
                
        }
        
        
}


## tests

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)


