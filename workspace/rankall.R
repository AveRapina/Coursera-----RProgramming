path <-getwd()
setwd(path)

rankHelper <- function(stateSubset, outcomeArr, num){
        result <- stateSubset[,2][order(outcomeArr, stateSubset[,2])[num]]
        return (result)
}


numHelper <- function (stateSubset, colNum, num){
        outcomeArr <- as.numeric(stateSubset[,colNum])
        len <-dim(stateSubset[!is.na(outcomeArr),])[1]
        if(num == "best"){
                rank<-rankHelper(stateSubset, outcomeArr,1) # get the fisrt rank
        }else if(num =="worst"){
                rank<-rankHelper(stateSubset, outcomeArr,len) # get the last
        }else if(num > len){
                rank <-NA
        }else{
                rank<-rankHelper(stateSubset, outcomeArr,num) # get the rank order
        }
        
        result <- rank
        return(result)
}



rankall <- function(outcome, num="best"){
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
        
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        
        stateArr <- sort(unique(data$State))# sort State and remove repeats
        arrLen <- length(stateArr) # get the lenght
        hospital <- rep("", arrLen) # create an empty array 
        
        # test the argument
        if(!outcome %in% validOutcomes){
                stop("invalid outcome entered")
        }else{ # loop over the array of states on data
                for(i in 1:arrLen){
                        stateSubset <- data[data[,7]==stateArr[i],]# if state equals
                        if(outcome == "heart attack"){
                                hospital[i]<- numHelper(stateSubset,11,num)
                        }else if(outcome == "heart failure"){
                                hospital[i] <-numHelper(stateSubset,17,num) 
                        }else{
                                hospital[i] <-numHelper(stateSubset,23,num)
                        }
                }
                dataFrame <- data.frame(hospital=hospital, state=stateArr)
                result <-dataFrame
                return(dataFrame)
        }
}


## tests
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
