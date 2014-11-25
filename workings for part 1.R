#Assigment 3

getwd()

setwd("C:/Users/aurquhart/Documents/Github/R_Programming_Assignment3")




#Point and download zip
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
download.file(dataset_url, "data.zip")

#unzip file
unzip("data.zip", exdir = "data")

#check files
list.files("data")

#re-adjust working directory
setwd("C:/Users/aurquhart/Documents/Github/R_Programming_Assignment3/data")

#read in csv file
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

head(outcome)
ncol(outcome) # 46 columns
nrow(outcome) #4706 rows - each row a hospital
names(outcome) #field names

outcome[, 11] <- as.numeric(outcome[, 11]) # make an array from field 11 Hospital.30.Day.Death.

head(outcome[, 11])

hist(outcome[, 11])



#Part 2

#Write a function to d returns the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state
#So function takes 2 arguments - state and outcome

#So quickly explore how this will work

names(outcome) # 46 columns

#Create a dataframe with relevant columns
bestdf <- outcome[,c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",  
                     "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                     "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )]


head(bestdf)

#So the function will then take an argumet which will be a state filter and we will just look at 1 of the outcomes
#create filtered dataset
singledf <- bestdf[ which(bestdf$State=='AL'), ]
singledf <- singledf[,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]

class(singledf)

#access minimum value
singledf[which(singledf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == 
                 min(singledf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm = TRUE)), ]

min(singledf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm = TRUE)
?min

#So now need to automate this process and also create error checks
