Row_to_fill <- 1



OpeningN <- lapply(Output[ ,c(2,4,6,8,10,12,14,16,18,20,22,24)],sum)
Opening_Pct <- unlist(OpeningN) / length(Output$Day1_Open)
Opening_latency_mean <- lapply(Output[ ,c(3,5,7,9,11,13,15,17,19,21,23,25)],mean,na.rm=TRUE)
Opening_latency_median <- lapply(Output[ ,c(3,5,7,9,11,13,15,17,19,21,23,25)],median,na.rm=TRUE)

First_consecutive_mean <- mean(Output$First_Consecutive_Day)
Breaks_mean <- mean(Output$Num_Breaks)

Output_stats[Row_to_fill,1:12 ] <- as.numeric(Opening_Pct) 
Output_stats[Row_to_fill,14:25] <- unlist(Opening_latency_mean)
Output_stats[Row_to_fill,27:38] <- unlist(Opening_latency_median)
Output_stats[Row_to_fill,40] <-First_consecutive_mean
Output_stats[Row_to_fill,41] <- Breaks_mean 

Output_stats[Row_to_fill,c(13,26,39)] <- '-'

colnames(Output_stats) <- c('Opening % D1', 'Opening % D2', 'Opening % D3', 'Opening % D4', 'Opening % D5', 'Opening % D6',
                            'Opening % D7', 'Opening % D8', 'Opening % D8', 'Opening % D10', 'Opening % D11', 'Opening % D12', '-',
                            'Lat Mean D1', 'Lat Mean D2', 'Lat Mean D3', 'Lat Mean D4', 'Lat Mean D5', 'Lat Mean D6',
                            'Lat Mean D7', 'Lat Mean D8', 'Lat Mean D9', 'Lat Mean D10', 'Lat Mean D11', 'Lat Mean D12', '-',
                            'Lat Median D1', 'Lat Median D2', 'Lat Median D3', 'Lat Median D4', 'Lat Median D5', 'Lat Median D6', 
                            'Lat Median D7', 'Lat Median D8', 'Lat Median D9', 'Lat Median D10', 'Lat Median D11', 'Lat Median D12','-',
                            'First Consectuve Mean', 'Breaks Mean')

write.