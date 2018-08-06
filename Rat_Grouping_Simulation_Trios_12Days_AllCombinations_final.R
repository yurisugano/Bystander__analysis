# Install packages

# To-do
  # Coin flip ties 
  # Half-win for ties
  # Duos and trios
  # Average time by day
  # Average openings per day

# Set working directory
  setwd("~/Desktop/Jacobi's Simulation Data/Up To Date Simulation Data")

# Load data
  rat_data <- read.csv('StandardRatsToModelN48.csv')
  
# Add ID column
  rat_data$Rat_ID <- 1:length(rat_data$D1)
  
# Round times

  # Set parameters
  # Number of rats (trios or pairs)
    N_rats <- 2
  # Number of trials
    N_trials <- factorial(48)/(factorial(N_rats)*factorial(48-N_rats))
  # Start day
    Start <- 1
  # End day
    End <- 12
  # Tie Method (CoinFlip or SplitWin)
    Tie_Method <- 'CoinFlip'

# Initiate table
  Output <- data.frame(matrix(0, ncol = 38, nrow = N_trials))
  
# Create list of all combinations
all_combinations <- (combn(1:48,N_rats,simplify=FALSE))
       
# Do the loop!
  for (i in 1:N_trials){
    # Pick the rats
      rows <- unlist(all_combinations[i])
      
    # Subset
      rats <- rat_data[c(rows),] 
      
    # Create stats table
      num_days <- End - Start + 1
      Rat_Stats <- data.frame(cbind(c(Start:End),rep(0,num_days),rep(0,num_days)
                                    ,rep(0,num_days),rep(0,num_days),rep(0,num_days)))
      colnames(Rat_Stats) <- c('Day','DidItOpen','Winner1','Winner2','Winner3','OpeningTime')

    # Loop through the days
      for (j in (Start+2):(End+2)){
        # Set day row
          if(Start == 7){
            day_row <- j-8
          }else{
            day_row <- j-2
          }
          
        # Count unique times per day
          num_unique <- length(unique(rats[,j]))
        # Sort the table in order of times
          ordered_rats <- rats[order(rats[,j]),]
        # No ties
          if(num_unique == 3){
            Rat_Stats[day_row,2] <- 1
            Rat_Stats[day_row,3] <- ordered_rats[1,15]
            Rat_Stats[day_row,6] <- ordered_rats[1,j]
        # One tie  
          }else if(num_unique == 2){ 
            Rat_Stats[day_row,2] <- 1
            # Check if 2nd and 3rd place tied
              if(ordered_rats[1,j] < ordered_rats[2,j]){
                Rat_Stats[day_row,3] <- ordered_rats[1,15]
                Rat_Stats[day_row,6] <- ordered_rats[1,j]                
              }else{
                # Check tie-breaking method
                if(Tie_Method == 'CoinFlip'){
                  # Pick a rat
                  winner_row <- sample(1:2,1,replace=FALSE)
                  winner <- ordered_rats[winner_row,]
                  # Assign
                  Rat_Stats[day_row,3] <- winner[1,15]
                  Rat_Stats[day_row,6] <- winner[1,j]
                }else if(Tie_Method == 'SplitWin'){
                  Rat_Stats[day_row,3] <- ordered_rats[1,15]
                  Rat_Stats[day_row,4] <- ordered_rats[2,15]
                  Rat_Stats[day_row,6] <- ordered_rats[1,j]
                }             
              }
        # All tied  
          }else{
            # Did any open?
            if(ordered_rats[1,j] == 40){
              Rat_Stats[day_row,2] <- 0
              Rat_Stats[day_row,6] <- ordered_rats[1,j]
            }else{
              Rat_Stats[day_row,2] <- 1
              # Check tie-breaking method
              if(Tie_Method == 'CoinFlip'){
                # Pick a rat
                winner_row <- sample(1:3,1,replace=FALSE)
                winner <- ordered_rats[winner_row,]
                # Assign
                Rat_Stats[day_row,3] <- winner[1,15]
                Rat_Stats[day_row,6] <- winner[1,j]
              }else if(Tie_Method == 'SplitWin'){
                Rat_Stats[day_row,3] <- ordered_rats[1,15]
                Rat_Stats[day_row,4] <- ordered_rats[2,15]
                Rat_Stats[day_row,5] <- ordered_rats[3,15]
                Rat_Stats[day_row,6] <- ordered_rats[1,j]
              }              
            }
          }
      }
      
      # Compute winning stats
        Rat1 <- ordered_rats$Rat_ID[1]
        Rat2 <- ordered_rats$Rat_ID[2]
        Rat3 <- ordered_rats$Rat_ID[3]
        
        Rat1_Wins <- 0
        Rat2_Wins <- 0
        Rat3_Wins <- 0
        
        if(Tie_Method == 'CoinFlip'){
          Rat1_Wins <- length(Rat_Stats$Winner1[Rat_Stats$Winner1 == Rat1])
          Rat2_Wins <- length(Rat_Stats$Winner1[Rat_Stats$Winner1 == Rat2])
          Rat3_Wins <- length(Rat_Stats$Winner1[Rat_Stats$Winner1 == Rat3])
        }else{
          for(k in 1:length(Rat_Stats$Day)){
            if(Rat_Stats[k,2]==1 & Rat_Stats[k,4] == 0){
              if(Rat_Stats[k,3] == Rat1){
                Rat1_Wins = Rat1_Wins + 1
              }else if(Rat_Stats[k,3] == Rat2){
                Rat2_Wins = Rat2_Wins + 1
              }else{
                Rat3_Wins <- Rat3_Wins + 1
              }
            }else if(Rat_Stats[k,2]==1 & Rat_Stats[k,4] != 0 & Rat_Stats[k,5] == 0){
              if(Rat_Stats[k,3] == Rat1 | Rat_Stats[k,4] == Rat1){
                Rat1_Wins = Rat1_Wins + 0.5
              }
              if(Rat_Stats[k,3] == Rat2 | Rat_Stats[k,4] == Rat2){
                Rat2_Wins = Rat2_Wins + 0.5
              }
              if(Rat_Stats[k,3] == Rat3 | Rat_Stats[k,4] == Rat3){
                Rat3_Wins <- Rat3_Wins + 0.5
              }              
            }else if(Rat_Stats[k,2]==1 & Rat_Stats[k,4] != 0 & Rat_Stats[k,5] != 0){
              Rat1_Wins <- Rat1_Wins + (1/3)
              Rat2_Wins <- Rat2_Wins + (1/3)
              Rat3_Wins <- Rat3_Wins + (1/3)
            }
          }
        }
        
        # Calculate consecutive days and breaks
        consecutive_counter <- 0
        breaks_counter <- 0
        first_consecutive_day <- 0
        breaks_days <- numeric()
        consecutive_max <- 0
        consecutive_total <- 0
        break_length <- 0 
        
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
            break_length <- break_length + 1
          }
          
        }
        
      # Assign winningest, middle, and losing rat
        Rat_Table <- data.frame(c(Rat1,Rat2,Rat3),c(Rat1_Wins,Rat2_Wins,Rat3_Wins))
        colnames(Rat_Table) <- c('Rat','Wins')
        Rat_Table <- Rat_Table[order(-Rat_Table[,2]),]
        Win_Rat <- Rat_Table[1,1]
        Win_Rat_Wins <- Rat_Table[1,2]
        Mid_Rat <- Rat_Table[2,1]
        Mid_Rat_Wins <- Rat_Table[2,2]
        Lose_Rat <- Rat_Table[3,1]
        Lose_Rat_Wins <- Rat_Table[3,2]
        

      # Organize Output
        Output[i,1] <- i
        Output[i,2] <- Rat_Stats$DidItOpen[1]
        Output[i,3] <- Rat_Stats$OpeningTime[1]
        Output[i,4] <- Rat_Stats$DidItOpen[2]
        Output[i,5] <- Rat_Stats$OpeningTime[2]
        Output[i,6] <- Rat_Stats$DidItOpen[3]
        Output[i,7] <- Rat_Stats$OpeningTime[3]
        Output[i,8] <- Rat_Stats$DidItOpen[4]
        Output[i,9] <- Rat_Stats$OpeningTime[4]
        Output[i,10] <- Rat_Stats$DidItOpen[5]
        Output[i,11] <- Rat_Stats$OpeningTime[5]
        Output[i,12] <- Rat_Stats$DidItOpen[6]
        Output[i,13] <- Rat_Stats$OpeningTime[6]
        Output[i,14] <- Rat_Stats$DidItOpen[7]
        Output[i,15] <- Rat_Stats$OpeningTime[7]
        Output[i,16] <- Rat_Stats$DidItOpen[8]
        Output[i,17] <- Rat_Stats$OpeningTime[8]
        Output[i,18] <- Rat_Stats$DidItOpen[9]
        Output[i,19] <- Rat_Stats$OpeningTime[9]
        Output[i,20] <- Rat_Stats$DidItOpen[10]
        Output[i,21] <- Rat_Stats$OpeningTime[10]
        Output[i,22] <- Rat_Stats$DidItOpen[11]
        Output[i,23] <- Rat_Stats$OpeningTime[11]
        Output[i,24] <- Rat_Stats$DidItOpen[12]
        Output[i,25] <- Rat_Stats$OpeningTime[12]
        Output[i,26] <- sum(Rat_Stats$DidItOpen)
        Output[i,27] <- Win_Rat
        Output[i,28] <- Win_Rat_Wins / sum(Rat_Stats$DidItOpen)
        Output[i,29] <- Mid_Rat
        Output[i,30] <- Mid_Rat_Wins / sum(Rat_Stats$DidItOpen)
        Output[i,31] <- Lose_Rat
        Output[i,32] <- Lose_Rat_Wins / sum(Rat_Stats$DidItOpen)
        Output[i,33] <- first_consecutive_day
        Output[i,34] <- consecutive_max
        Output[i,35] <- consecutive_total
        Output[i,36] <- breaks_counter
        Output[i,37] <- breaks_days[1]
        Output[i,38] <- break_length
        
        
  }
  
  # Set column names
  names <- c('Trial'
             ,'Day1_Open','Day1_Time'
             ,'Day2_Open','Day2_Time'
             ,'Day3_Open','Day3_Time'
             ,'Day4_Open','Day4_Time'
             ,'Day5_Open','Day5_Time'
             ,'Day6_Open','Day6_Time'
             ,'Day7_Open','Day7_Time'
             ,'Day8_Open','Day8_Time'
             ,'Day9_Open','Day9_Time'
             ,'Day10_Open','Day10_Time'
             ,'Day11_Open','Day11_Time'
             ,'Day12_Open','Day12_Time'
             ,'Total_Opens'
             ,'Win_Rat','Win_Rat_Win_Pct'
             ,'Mid_Rat','Mid_Rat_Win_Pct'
             ,'Lose_Rat','Lose_Rat_Win_Pct'
             ,'First_Consecutive_Day', 'Max_Consecutive'
             ,'Num_Consecutive', 'Num_Breaks'
             ,'Breaks_First','Breaks_Max'
  )
  
  colnames(Output) <- names
  
  if (N_rats == 2)
  {
    Output_2rats <- Output
  } else if (N_rats == 3) {
    Output_3rats <- Output
  }
  
  
# Write to file 
  # output_filename <- paste0(N_trials,'_Trials_',N_rats,'_Rats_Days',Start,'-',End,'_',Tie_Method,'_TieBreaker_wFirstConsecutiveDay.csv',sep="")
  # 
  # write.csv(Output,file=output_filename, row.names=FALSE)
  # 

