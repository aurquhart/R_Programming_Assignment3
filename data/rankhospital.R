rankhospital <- function(state,outcome, num = "best" ) {
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
  
  #get the rank i want to search for later
  if(num == "best") {numanswer <- 1
  } else if (num == "worst") {numanswer <-1
  } else {numanswer <- num}
  
  #filter on state
  singledf <- bestdf[ which(bestdf$State== state), ]
  #head(singledf)
  
  #convert answer to be numeric
  singledf[, 3:5] <- sapply(singledf[, 3:5], as.numeric)
  
  #head(bestdfrank)

  singlemetric <- singledf[,c("Hospital.Name","State",outcomeanswer)]
  #head(singlemetric)
  
  #add a rank by metric and group by state
  #for rank i will sort ascending unless the user inputs worst, then i go descending
  
  if (num == "worst") {
  bestdfrank <- transform(singlemetric, 
                          State.rank = ave(singlemetric[,3], State, 
                                     FUN = function(x) rank(-x, ties.method = "first")))
  } else {
    bestdfrank <- transform(singlemetric, 
                            State.rank = ave(singlemetric[,3], State, 
                                    FUN = function(x) rank(x, ties.method = "first")))}
    

  #head(bestdfrank)

  answer <- bestdfrank[ which(bestdfrank$State.rank==numanswer), ]
  

  answer$Hospital.Name
}

#state <- "NC"

#outcome <- "heart attack"
#num <- "worst"
#rankhospital("NC", "heart attack", "worst")

#head(bestdf)
#head(bestdfrank)
#head(singledf[, 3:5])
#bestdfrank[ which(bestdfrank$State== state), ]




