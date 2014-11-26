best <- function(state,outcome ) {
  #read in data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Create a dataframe with relevant columns
  bestdf <- df[,c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",  
                  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                  "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )]
  
  
  #Check that state in function exists in data
  DoesStateExist<-subset(bestdf$State, bestdf$State==state)
  if(length(DoesStateExist) == 0) stop("invalid state")
  
  #Fix outcome so it's the same as what the user inputs
  if(outcome == "heart attack") {outcomeanswer <- 
                                   "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }else if (outcome == "pneumonia") {outcomeanswer <- 
                                       "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else if (outcome == "heart failure") {outcomeanswer <- 
                                            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else (outcome == "invalid state")
  
  #outcomeanswer
  
  singledf <- bestdf[ which(bestdf$State== state), ]
  #head(singledf)
  singlemetric <- singledf[,c("Hospital.Name",outcomeanswer)]
  
  #convert answer to be numeric
  singlemetric[, 2:2] <- sapply(singlemetric[, 2:2], as.numeric)
  
  #head(singlemetric)
  answer <-singlemetric[which(singlemetric[outcomeanswer] == 
                                min(singlemetric[[outcomeanswer]],na.rm = TRUE)), ]
  answer$Hospital.Name
}

#state <- "NN"
#state <- "NY"
#outcome <- "pneumonia"

#best("SC", "heart attack")
#best("NY", "pneumonia")
#best("NN","pneumonia")


