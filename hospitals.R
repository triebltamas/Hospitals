data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

best <- function(state, outcome) {
  
  if(nchar(state) != 2 || !is.character(state)) {
    stop("invalid state")
  }
  if(outcome != "heart failure" && outcome != "pneumonia" &&
     outcome != "heart attack") {
    
    stop("invalid outcome")
    
  }
  data_new <- data[data$State == state,]
  this_col <- if(outcome == "heart attack") {
    11
  } else if(outcome == "heart failure") {
    17
  } else {
    23
  }
  
  result_pool <- data.frame( data_new[, 2], as.numeric(data_new[, this_col]) )
  
  good <- complete.cases(result_pool[1], result_pool[2])
  y <- data.frame(result_pool[, 1][good], result_pool[, 2][good])
  
  x <- y[ order( y[, 2], y[, 1] ), ]
  names(x) <- c("Hospital", "Rate")
  head(x)
  
}

rankhospital <- function(state, outcome, num = "best") {
  if(nchar(state) != 2 || !is.character(state)) {
    stop("invalid state")
  }
  if(outcome != "heart failure" && outcome != "pneumonia" &&
     outcome != "heart attack") {
    
    stop("invalid outcome")
    
  }
  
  data_new <- data[data$State == state,]
  this_col <- if(outcome == "heart attack") {
    11
  } else if(outcome == "heart failure") {
    17
  } else {
    23
  }
  
  result_pool <- data.frame( data_new[, 2], as.numeric(data_new[, this_col]) )
  
  good <- complete.cases(result_pool[1], result_pool[2])
  y <- data.frame(result_pool[, 1][good], result_pool[, 2][good])
  
  x <- y[ order( y[, 2], y[, 1] ), ]
  N <- length(x[, 1])
  z <- data.frame(x[, 1], x[, 2], 1:N)
  names(z) <- c("Hospital", "Rate", "Rank")
  head(z)
  
  rank <- if(num == "best") {
    1
  } else if (num == "worst") {
    N
  } else if ( !is.na(as.integer(num)) ) {
    as.integer(num)
  }
  print("The wanted hospital: ")
  print( z[z$Rank == rank,]$Hospital )
  
}


rankall <- function(outcome, num = "best") {
  if(outcome != "heart failure" && outcome != "pneumonia" &&
     outcome != "heart attack") {
    
    stop("invalid outcome")
    
  }
  
  states <- unique(data$State)
  ranks_df <- data.frame(hospital = character(length(states)), 
                         state = character(length(states)))
  index <- 1
  for (state in states) {
    
    data_new <- data[data$State == state,]
    this_col <- if(outcome == "heart attack") {
      11
    } else if(outcome == "heart failure") {
      17
    } else {
      23
    }
    
    result_pool <- data.frame( data_new[, 2], as.numeric(data_new[, this_col]) )
    
    good <- complete.cases(result_pool[1], result_pool[2])
    y <- data.frame(result_pool[, 1][good], result_pool[, 2][good])
    
    x <- y[ order( y[, 2], y[, 1] ), ]
    N <- length(x[, 1])
    z <- data.frame(x[, 1], x[, 2], 1:N)
    names(z) <- c("Hospital", "Rate", "Rank")
    head(z)
    
    rank <- if(num == "best") {
      1
    } else if (num == "worst") {
      N
    } else if ( !is.na(as.integer(num)) ) {
      as.integer(num)
    }
    hospital <- z[z$Rank == rank,]$Hospital
    if(length(hospital) == 0) {
      hospital <- "<NA>"
    }
    ranks_df$hospital[index] <- hospital
    ranks_df$state[index] <- state
    index <- index + 1
  }
  ranks_df[order(ranks_df[, 2]), ]
}