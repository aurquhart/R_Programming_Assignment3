rankall <- function(outcome, num = "best" ) {
  #read in data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Create a dataframe with relevant columns
  bestdf <- df[,c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",  
                  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                  "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )]
  
  
  
  
  #Fix outcome so it's the same as what the user inputs
  if(outcome == "heart attack") {outcomeanswer <- 
                                   "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }else if (outcome == "pneumonia") {outcomeanswer <- 
                                       "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else if (outcome == "heart failure") {outcomeanswer <- 
                                            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else {stop("invalid outcome")}
  
  #outcomeanswer
  
  #get the rank i want to search for later
  if(num == "best") {numanswer <- 1
  } else if (num == "worst") {numanswer <-1
  } else {numanswer <- num}
  
  
  
  #filter on state
 # singledf <- bestdf[ which(bestdf$State== state), ]
  #head(singledf)
  
  #It might be that the user asks for more records than are available
  #This will capture this.
 # if (nrow(singledf) < sapply(numanswer, as.numeric)) stop(NA)
  
  
  #convert answer to be numeric
 bestdf[, 3:5] <- sapply(bestdf[, 3:5], as.numeric)
  
  #head(bestdfrank)

  singlemetric <- bestdf[,c("Hospital.Name","State",outcomeanswer)]
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
  
  #bring back the hospitals who correspond to the rank
  answer <- bestdfrank[ which(bestdfrank$State.rank==numanswer), ]
 
  #need to tidy up header names
  names(answer)[names(answer)=="Hospital.Name"] <- "hospital"
  names(answer)[names(answer)=="State"] <- "state"
 
  answer[,c("hospital","state")]
 
  #if (nrow(singledf) < sapply(numanswer, as.numeric)) {NA
  #} else   {answer$Hospital.Name
  #}

}


#state <- "WA"

#outcome <- "pneumonia"
#num <- "10"
#rankhospital("NC", "heart attack", "worst")

#head(bestdf)
#head(bestdfrank)
#head(singledf[, 3:5])
#bestdfrank[ which(bestdfrank$State== state), ]


#rankhospital("WA", "pneumonia", 1000)

#rankall("pneumonia", "worst")
#rankall("heart attack", 4)
#rankall("heart failure", 10)

