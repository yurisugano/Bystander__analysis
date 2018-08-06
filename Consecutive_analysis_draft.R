# Calculate consecutive days and breaks
consecutive_counter <- 0
breaks_counter <- 0
first_consecutive_day <- 0
breaks_days <- numeric()
consecutive_max <- 0
consecutive_total <- 0
break_max <- 0 

for(day in 2:length(Rat_Stats$Day)){
  current_day <- Rat_Stats$DidItOpen[day]
  previous_day <- Rat_Stats$DidItOpen[day-1]
  
  if(current_day == 1 & previous_day == 1){
    consecutive_counter <- consecutive_counter + 1
    
    if(first_consecutive_day == 0){
      first_consecutive_day <- day
    }
    
    
    if(consecutive_counter > consecutive_max){
      consecutive_max <- consecutive_counter
    }
    
  }else if(current_day == 0 & previous_day == 1){
    breaks_counter <- breaks_counter + 1
    breaks_days[breaks_counter] <- day
    consecutive_counter <- 0
    
    
  } else if (current_day == 0 & previous_day == 0){
    breaks_counter <- breaks_counter + 1
    breaks_days[breaks_counter] <- day
    consecutive_counter <- 0
    break_max <- break_max + 1
  }
  
}

Rat_Stats$DidItOpen
first_consecutive_day
consecutive_max
breaks_counter
