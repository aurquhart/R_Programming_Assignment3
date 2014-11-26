best <- function(state,outcome ) {
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
  } else (outcome == "invalid state")
  
  
  singledf <- bestdf[ which(bestdf$State== state), ]
  #head(singledf)
  singlemetric <- singledf[,c("Hospital.Name",outcomeanswer)]
  answer <-singlemetric[which(singlemetric[outcomeanswer] == 
                                min(singlemetric[[outcomeanswer]],na.rm = TRUE)), ]
  answer$Hospital.Name
}


#best("SC", "heart attack")