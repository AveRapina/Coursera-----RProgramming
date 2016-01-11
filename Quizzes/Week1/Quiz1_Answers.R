#1. R was developed by statisticians working at
#-The univertsity of Auckland

#2. The definition of free software consists of four freedoms (freedoms 0 through 3). Which of the following is NOT one of the freedoms that are part of the definition? Select all that apply.
#-The freedom to prevent users from using the software for undesirable purposes.
#-The freedom to sell the software for any price.
#-The freedom to restrict access to the source code for the software.

#3. In R the following are all atomic data types EXCEPT: (Select all that apply)
#-data Frame
#-matrix
#-list
#-table
#-array

#4. If I execute the expression x <- 4 in R, what is the class of the object `x' as determined by the `class()' function?
#-numeric

#5. What is the class of the object defined by x <- c(4, TRUE)?
#-numeric

#6. If I have two vectors x <- c(1,3, 5) and y <- c(3, 2, 10), what is produced by the expression rbind(x, y)?
#-a matrix with two rows and three columns

#7. A key property of vectors in R is that
#-elements of a vector all must be of the same class

#8. Suppose I have a list defined as x <- list(2, "a", "b", TRUE). What does x[[1]] give me? Select all that apply.
#-a numeric vector containing the element 2.

#9. Suppose I have a vector x <- 1:4 and a vector y <- 2. What is produced by the expression x + y?
#-a numeric vector with elements 3, 4, 5, 6.

#10. Suppose I have a vector x <- c(3, 5, 1, 10, 12, 6) and I want to set all elements of this vector that are less than 6 to be equal to zero. What R code achieves this? Select all that apply.
x <- c(3, 5, 1, 10, 12, 6)
x[x < 6] <- 0
x[x %in% 1:5] <- 0
x[x <= 5] <- 0

#11. Use the Week 1 Quiz Data Set to answer questions 11-20. In the dataset provided for this Quiz, what are the column names of the dataset? 
data -< read.csv("hw1_data.csv")
names(data)
#-[1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"  
#[6] "Day" 

#12. Extract the first 2 rows of the data frame and print them to the console. What does the output look like?
x <- data[1:2,]
#-  Ozone Solar.R Wind Temp Month Day
#1    41     190  7.4   67     5   1
#2    36     118  8.0   72     5   2

#13. How many observations (i.e. rows) are in this data frame?
nr <- nrow(data)
#-[1] 153

#14. Extract the last 2 rows of the data frame and print them to the console. What does the output look like?
nr <- nrow(data)
data[(nr-1):nr,]
#-    Ozone Solar.R Wind Temp Month Day
#152    18     131  8.0   76     9  29
#153    20     223 11.5   68     9  30

#15. What is the value of Ozone in the 47th row?
data[47,1]
#-[1] 21

#16. How many missing values are in the Ozone column of this data frame?
sum (is.na(data[,1]))
#-[1] 37

#17. What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.
mean(data[,1], na.rm=TRUE)
mean(data$Ozone, na.rm=TRUE)
#-[1] 42.12931

#18. Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
good <-complete.cases(data)
cleanData <- data[good,]
valid<-cleanData$Ozone >31 & cleanData$Temp>90
mean(cleanData[valid,]$Solar.R)
#-[1] 212.8

#19. What is the mean of "Temp" when "Month" is equal to 6? 
goodMont <- data$Month==6
dataMonth <- data[goodMonth,]
mean(dataMonth$Temp)
#-[1] 79.1

#20. What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
dataMonth <- data[goodMonth,]
goodDataMonth <- complete.cases(dataMonth)
dataMonth <- dataMonth[goodDataMonth,]
max(dataMonth$Ozone)
#-[1] 115
