library(tidyverse) # replace_na function as a workaround
library(dplyr) # formatting data
library(Amelia) # missmap
library(ggplot2) # plotting
library(lubridate) # handling dates
library(PerformanceAnalytics) # Sharpe ratio and maximum drawdown
options(scipen = 1000000)

# Read the data & keep only needed rows (Average value weighted monthly returns)
ind <- read.csv("49_Industry_Portfolios.CSV", skip = 11, stringsAsFactors = F)
ind <- ind[1:first(grep("Average", ind$X)) - 1, ]

# Make dates into right format
colnames(ind)[1] <- "dates"
ind$dates <- paste0(ind$dates, "01")
ind$dates <- as.Date(ind$dates, "%Y%m%d")

# Extract dates into a vector & delete them from ind
dates <- ind$dates
ind$dates <- NULL

# Remove NAs (-99.99) & make daily returns into decimals
ind <- sapply(ind, as.numeric)
ind[which(ind == -99.99)] <- NA
ind <- ind / 100 + 1

# Show missing values
ind <- as.data.frame(ind)
missmap(ind)
ind <- as.matrix(ind)

# Store NA positions & replace them with 1 to calculate cumulative products
NA_pos <- which(is.na(ind))
ind[NA_pos] <- 1
sector_returns <- apply(ind, 2, cumprod)
# Add back NAs to their original positions
sector_returns[NA_pos] <- NA

# Attach dates to the cumulative products
returns_dates <- as.data.frame(cbind(as.character(dates), sector_returns))
names(returns_dates)[1] <- "dates"
returns_dates$dates <- as.Date(returns_dates$dates)

# Make a time series object to calculate down years
returns_ts <- as.xts(returns_dates, order.by = returns_dates[,1])
returns_ts$dates <- NULL
storage.mode(returns_ts) <- "numeric"
# Add ones to the start of the rows to calculate correctly
returns_ts <- rbind(rep(1, 49), as.data.frame(returns_ts))

# Calculate consecutive down years for each period
down_years_1 <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1, 1, 0))

down_years_2 <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1,
                                                 ifelse(lead(x, 24) / lead(x, 12) < 1,
                                                             1, 0), 0))
down_years_3 <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1,
                                                 ifelse(lead(x, 24) / lead(x, 12) < 1, 
                                                 ifelse(lead(x, 36) / lead(x, 24) < 1, 
                                                             1, 0), 0), 0))
down_years_4 <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1,
                                                 ifelse(lead(x, 24) / lead(x, 12) < 1, 
                                                 ifelse(lead(x, 36) / lead(x, 24) < 1, 
                                                 ifelse(lead(x, 48) / lead(x, 36) < 1,
                                                             1, 0), 0), 0), 0))
down_years_5 <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1,
                                                 ifelse(lead(x, 24) / lead(x, 12) < 1, 
                                                 ifelse(lead(x, 36) / lead(x, 24) < 1, 
                                                 ifelse(lead(x, 48) / lead(x, 36) < 1,
                                                 ifelse(lead(x, 60) / lead(x, 48) < 1,
                                                             1, 0), 0), 0), 0), 0))

# Make data frames containing future i year returns
for(i in 1:5){
  assign(paste0("returns_", i),
         apply(returns_ts, 2, function(x) lead(x, 12 * i)) / returns_ts)
}

# Calculate one year future returns
for(i in 1:5){
  assign(paste0("returns", i),
         apply(returns_1, 2, function(x) lead(x, i * 12)) * get(paste0("down_years_", i)))
}

# Make data frames into a list, replace zeros with NAs
returns_list <- list(returns1, returns2, returns3, returns4, returns5)
returns_list <- lapply(returns_list, function(x) x <- as.data.frame(x))
returns_list <- rapply(returns_list, function(x) ifelse(x == 0, NA, x), how = "replace")
names(returns_list) <- c("r1", "r2", "r3", "r4", "r5")
list2env(returns_list, .GlobalEnv)

# Turn the returns into data frames containing only the yearly returns
for(i in 1:5){
  assign(paste0("r_vector_", i),
         as.data.frame(apply(get(paste0("r", i)), 1, function(x) mean(x, na.rm = T))))
}

# Calculate index returns
index <- apply(returns_ts, 2, function(x) lead(x, 12) / x)
index_vector <- as.data.frame(apply(index, 1, function(x) mean(x, na.rm = T)))

# Rename columns for easier handling
colnames(r_vector_1) <- colnames(r_vector_2) <- colnames(r_vector_3) <-
colnames(r_vector_4) <- colnames(r_vector_5) <- colnames(index_vector) <- "x"

# Add index returns to years where the strategy wasn't invested
handle_missing <- function(x){
  x <- as.data.frame(x)
  colnames(x) <- "x"
  return(ifelse(is.na(x), index_vector$x, x$x))
}
for(i in 1:5){
  assign(paste0("r_vector_", i), handle_missing(get(paste0("r_vector_", i))))
}

# Bind the index return and the strategies together into a single data frame
strategy_vectors <- cbind(index_vector, r_vector_1, r_vector_2, r_vector_3, r_vector_4, r_vector_5)
strategy_vectors <- as.data.frame(strategy_vectors)
colnames(strategy_vectors) <- c("index", "r1", "r2", "r3", "r4", "r5")

############################################
# Make a matrix containing strategy performance
return_matrix <- as.data.frame(matrix(nrow = 6, ncol = 4))
colnames(return_matrix) <- c("Geomean", "Return", "Alpha", "Observations")
# Calculate returns for the index & each strategy
return_matrix[1, 2] <- exp(mean(log(strategy_vectors$index), na.rm = T))
for(i in 1:5){
  x <- strategy_vectors[, i + 1]
  return_matrix[i + 1, 2] <- exp(mean(log(x), na.rm = T))
}
############################################

for(i in 1:12){
  # Make a new data frame containing portfolios formed every month
  x <- strategy_vectors[seq(i, nrow(strategy_vectors), 12), ]
  rownames(x)[1] <- as.character(as.Date(rownames(x)[2]) - years(1))
  
  # Make the returns of each month into a time series object
  temp <- as.zoo(x)
  index(temp) <- as.Date(rownames(x))
  temp <- na.omit(temp)
  temp <- temp - 1
  
  # Plot the max drawdowns & store max drawdowns & Sharpes
  print(chart.Drawdown(temp, main = paste0("Maximum drawdown ",
                                           month.name[month(rownames(temp[1]))]),
                 legend.loc = "bottomright"))
  assign(paste0(month.abb[i], "_dd"), maxDrawdown(temp))
  assign(paste0(month.abb[i], "_sharpe"), SharpeRatio.annualized(temp))
  
  # Calculate cumulative products for plotting the returns
  x <- apply(x, 2, function(x) cumprod(x))
  x <- as.data.frame(x)
  # Add dates from rownames to a column for gather
  x$date <- rownames(x)
  # Gather the data for plotting
  x_formatted <- gather(x, strategy, index, -date)
  print(ggplot(x_formatted, aes(x = as.Date(date), y = index, color = strategy)) +
          geom_line(size = 1.5) +
          xlab("Date") +
          ggtitle(month.name[month(as.Date(x$date[1]))]) +
          scale_y_continuous(trans = 'log2') +
          scale_color_manual(
            values = c("#000000", "#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#D3D3D3")))
}



























