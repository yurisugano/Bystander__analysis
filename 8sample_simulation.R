### Simulation to sample from all combinations

# Set specific metrics to output (such as % that performed better on D1, etc) on line 60
# Set number of iterations and number of samples to be picked

  N_trials <- 10000
  N_sample <- 8
  
# Create output files
  Comparison_output <- data.frame(matrix(0, ncol = 65, nrow = N_trials))
  Non_openers_output <- data.frame(matrix(0, ncol = 10, nrow = N_trials))
  Openers_output <- data.frame(matrix(0, ncol = 24, nrow = N_trials))
  
# Create matrix to store results for each iteration, and add data from experimental 1 Free and Dyads  
  Results_matrix <- matrix(0, ncol = 10, nrow = 12)
    Results_matrix[ ,2] <- matrix(c(0,2,3,4,6,6,6,8,8,7,6,8))
    Results_matrix[ ,3] <- matrix(c(4,4,7,7,6,6,7,7,8,8,8,8))
    Results_matrix[ ,5] <- matrix(c(40,32,28,26,16,13,10.5,3,1,6,11,2))
    Results_matrix[ ,6] <- matrix(c(28,28.5,13.6,11.4,10.7,10.5,13.7,6,3.3,1,1.5,1.5))
    Results_matrix[ ,8] <- matrix(c(40,40,40,32.5,10.5,3.5,0,1,0,1,1,1))
    Results_matrix[ ,9] <- matrix(c(22.7,37,10,2.3,1.1,1.1,1.1,0.8,0.7,0.98,1.11,0.7))
    
# Loop
  for (i in 1:N_trials){
    if (N_rats == 2){ 
      rows <- sample(1:dim(Output_2rats)[1],N_sample,replace=FALSE)
      rats <- Output_2rats[rows,]
    } else if (N_rats == 3) {
      rows <- sample(1:dim(Output_3rats)[1],N_sample,replace=FALSE)
      rats <- Output_3rats[rows,]
    }
    
    # Get daily N of openings for the iteration. Store N of openings for 1 Free and Dyads
    OpeningN <- lapply(rats[ ,c(2,4,6,8,10,12,14,16,18,20,22,24)],sum)
    Results_matrix[ ,1] <- as.matrix(unlist(OpeningN))
    
    # Get average daily latencies for the iteration. Store latencies for experimental 1 Free and Dyads  
    Opening_latency_mean <- lapply(rats[ ,c(3,5,7,9,11,13,15,17,19,21,23,25)],mean,na.rm=TRUE)
    Opening_latency_long <- gather(data.frame(rats[ ,c(3,5,7,9,11,13,15,17,19,21,23,25)]),
                                   key = "Day", value = "Latency")
    Opening_latency_long[,1] <- as.factor(c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8),
                                          rep(7,8),rep(8,8),rep(9,8),rep(10,8),rep(11,8),rep(12,8)
                                          ))
    Opening_latency_long <- subset(Opening_latency_long, Latency < 40)
    
    
    Opening_latency_summary <- ddply(Opening_latency_long,c("Day"),summarise,
                                       N = length(Latency),
                                       mean = mean(Latency)
    )
    
    
            p <- 1
            while (p < 12) {
              if (Opening_latency_summary[p,1] == p){
                p <- p + 1
              } else {
                day_noopen <- p
                p <- 12
              }
            }
            
            d <- 1
            while (d < day_noopen){
              Openers_output[i,d] <- Opening_latency_summary$mean[d]
              Openers_output[i,d+12] <- Opening_latency_summary$N[d]
              d <- d + 1
            }
            
            Openers_output[i,day_noopen] <- NA
            Openers_output[i,day_noopen+12] <- 0
            
            d <- 12
            while (d > day_noopen) {
              Openers_output[i, d] <- Opening_latency_summary$mean[d-1]
              Openers_output[i, d+12] <- Opening_latency_summary$N[d-1]
              d <- d - 1
            }
    
    Results_matrix[ ,4] <- as.matrix(unlist(Opening_latency_mean))
  
    # Repeat above for median
    Opening_latency_median <- lapply(rats[ ,c(3,5,7,9,11,13,15,17,19,21,23,25)],median,na.rm=TRUE)
    Results_matrix[ ,7] <- as.matrix(unlist(Opening_latency_median))
  
  ## Add results to output file - on the output file, odd columns refer to 1 Free and even columns refer to Dyads
    
    # Calculate difference in number of openings of the iteration from that of 1 Free and Diads
    Comparison_output[i, 1] <- as.integer(sum(Results_matrix[,1])) - 64 
    Comparison_output[i, 2] <- as.integer(sum(Results_matrix[,1])) - 80
  
    #Calculate number of days when simulation did better
    Comparison_output[i, 3] <- 8 - sum(Results_matrix[,1] < Results_matrix[,2])
    Comparison_output[i, 4] <- 8 - sum(Results_matrix[,1] < Results_matrix[,3])
  
    # Sum the averages of daily latency and calculate the difference from simulation
    Comparison_output[i, 5] <- as.integer(sum(Results_matrix[,5])) - as.integer(sum(Results_matrix[,4]))
    Comparison_output[i, 6] <- as.integer(sum(Results_matrix[,6])) - as.integer(sum(Results_matrix[,4]))
  
    # Sum the medians of daily latency and calculate the difference from simulation
    Comparison_output[i, 7] <- as.integer(sum(Results_matrix[,8])) - as.integer(sum(Results_matrix[,7]))
    Comparison_output[i, 8] <- as.integer(sum(Results_matrix[,9])) - as.integer(sum(Results_matrix[,7]))
  
    # Include average/median latency in the output
    Comparison_output[i,15:26] <- Results_matrix[,4]
    Comparison_output[i,28:39] <- Results_matrix[,7]
    Comparison_output[i,40:51] <- Results_matrix[,3] - Results_matrix[,1]
    Comparison_output[i,53:64] <- 8-Results_matrix[,1]
    
  # Here optional statistics can be selected. Examples output a vector of T/F that can be simply counted for percentage later 
      
    # Check in how many simulations rats opened more than X% of the time on Day Y, where 
    # in Opening[Y], Y = day and in X * N_rats, X = percentage.
        Comparison_output[i,10] <- unlist(OpeningN[1]) >= .75 * N_sample
       
    # Check in how many simulations rats performed better than a treshold in a range of days, where 
    # in Opening[Y], Y = day and in X * N_rats, X is the percentage for that day
        Comparison_output[i,11] <- unlist(OpeningN[1]) >= .75 * N_sample & 
                                    unlist(OpeningN[2]) >= .885 * N_sample &
                                    unlist(OpeningN[3]) >= .885 * N_sample &                                 
                                    unlist(OpeningN[4]) >= .885 * N_sample &
                                    unlist(OpeningN[5]) >= .885 * N_sample &
                                    unlist(OpeningN[6]) >= .885 * N_sample
    #   
        
        Comparison_output[i,12] <- unlist(OpeningN[1]) >= .5 * N_sample
        
    # Check in how many simulations rats performed better than a treshold in a range of days, where 
    # in Opening[Y], Y = day and in X * N_rats, X is the percentage for that day
        Comparison_output[i,13] <- unlist(OpeningN[1]) >= .5 * N_sample & 
                                    unlist(OpeningN[2]) >= .5 * N_sample &
                                    unlist(OpeningN[3]) >= .885 * N_sample &                                 
                                    unlist(OpeningN[4]) >= .885 * N_sample &
                                    unlist(OpeningN[5]) >= .75 * N_sample &
                                    unlist(OpeningN[6]) >= .75 * N_sample
        
  # Create metrics for graphing, namely percentage of simulation that did worse in each of the metrics
      
        if (N_rats == 2) {
          Pct_OpeningN_dyad <- sum(Comparison_output[2] < 0) / length(Comparison_output[,2])
          Pct_Openingday_dyad <- sum(Comparison_output[4] < 0) / length(Comparison_output[,2]) 
          Pct_MeanLatency_dyad <- sum(Comparison_output[6] < 0) / length(Comparison_output[,2])
          Pct_MedianLatency_dyad <- sum(Comparison_output[8] < 0) / length(Comparison_output[,2])
          Results_dyad <- Comparison_output
        } else if (N_rats == 3) {
          Pct_OpeningN_tryad <- sum(Comparison_output[2] < 0) / length(Comparison_output[,2])
          Pct_Openingday_tryad <- sum(Comparison_output[4] < 0) / length(Comparison_output[,2]) 
          Pct_MeanLatency_tryad <- sum(Comparison_output[6] < 0) / length(Comparison_output[,2])
          Pct_MedianLatency_tryad <- sum(Comparison_output[8] < 0) / length(Comparison_output[,2])
          Results_tryad <- Comparison_output
        }
        
            
        
  }
        
  temp1 <- gather(Openers_output,key="Days",value="Latency",1:12)
  temp2 <- gather(Openers_output,key="Days",value="Num",13:24)
  temp3 <- data.frame(matrix(ncol = 3, nrow = N_trials * 12))
  temp3[ ,1:2] <- temp1[,13:14]
  temp3[ ,3] <- temp2[ ,14]
  temp3[ ,1] <- as.factor(c(rep(1,10000),rep(2,10000),rep(3,10000),rep(4,10000),rep(5,10000),rep(6,10000),
             rep(7,10000),rep(8,10000),rep(9,10000),rep(10,10000),rep(11,10000),rep(12,10000))) 
  colnames(temp3) <- c("Days", "Latency", "Num")
    
    Openers_triad_Summary <- ddply(temp3,c("Days"),summarise,
                                      latencymed = median(Latency,na.rm = TRUE),
                                      n = mean(Num,na.rm = TRUE),
                                      sdn = sd (Num,na.rm = TRUE),
                                      cin = qt( 1- (0.05 / 2), 10000 - 1) * sdn,
                                      latency = mean(Latency,na.rm = TRUE),
                                      sd = sd (Latency,na.rm = TRUE),
                                      ci = qt( 1- (0.05 / 2), 10000 - 1) * sd,
                                      cimax = quantile(Latency,.975, na.rm = TRUE),
                                      cimin = quantile(Latency,.025, na.rm = TRUE)
    )
    Openers_triad_Summary[11] <- "Group"
    colnames(Openers_triad_Summary)[11] <- "Group"
      
# 