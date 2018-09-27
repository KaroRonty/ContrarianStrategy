library(tidyverse) # replace_na function as a workaround
library(dplyr) # formatting data
library(Amelia) # missmap
library(lubridate) # handling dates
library(quantmod) # calculating monthly returns
options(scipen = 1000000)

# Read the data & keep only needed rows (Average value weighted monthly returns)
ind <- read.csv("49_Industry_Portfolios.CSV", skip = 11, stringsAsFactors = F)
ind <- ind[1:first(grep("Average", ind$X))-1, ]

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

# Attach dates to cumulative products
returns_dates <- as.data.frame(cbind(as.character(dates), sector_returns))
names(returns_dates)[1] <- "dates"
returns_dates$dates <- as.Date(returns_dates$dates)

# Make a time series object to calculate monthly returns
returns_ts <- as.xts(returns_dates, order.by = returns_dates[,1])
returns_ts$dates <- NULL
storage.mode(returns_ts) <- "numeric"

oneyear <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1, 1, 0))

twoyear <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1,
                                            ifelse(lead(x, 24) / lead(x, 12) < 1,
                                                            1, 0), 0))

threeyear <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1,
                                              ifelse(lead(x, 24) / lead(x, 12) < 1, 
                                              ifelse(lead(x, 36) / lead(x, 24) < 1, 
                                                             1, 0), 0), 0))

fouryear <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1,
                                             ifelse(lead(x, 24) / lead(x, 12) < 1, 
                                             ifelse(lead(x, 36) / lead(x, 24) < 1, 
                                             ifelse(lead(x, 48) / lead(x, 36) < 1,
                                                            1, 0), 0), 0), 0))

fiveyear <- apply(returns_ts, 2, function(x) ifelse(lead(x, 12) / x < 1,
                                             ifelse(lead(x, 24) / lead(x, 12) < 1, 
                                             ifelse(lead(x, 36) / lead(x, 24) < 1, 
                                             ifelse(lead(x, 48) / lead(x, 36) < 1,
                                             ifelse(lead(x, 60) / lead(x, 48) < 1,
                                                            1, 0), 0), 0), 0), 0))

# Calculate monthly returns & turn into data frame for below zero calulation
monthly_returns <- returns_ts / apply(returns_ts, 2, function(x) lag(x, 1))
monthly_returns <- as.data.frame(monthly_returns)

years_below_zero_1 <- apply(monthly_returns, 2, function(x) lead(x, 12)) / monthly_returns

# 1 indicates that monthly return is below zero
below_zero <- ifelse(monthly_returns < 1, 1, 0)
# Calculate streak lengths
streaks <- apply(below_zero, 2, function(x) sequence(rle(x)$lengths))
# Keep only negative streaks
streaks <- below_zero * streaks

# Make a data frame for each amount of streaks
for(i in 1:5){
  assign(paste0("streaks_", i), 
         # First row as NA to match the rows of returns_ts
         rbind(rep(NA, 49), ifelse(streaks >= i, 1, NA)))
}

# Add ones to the start of the rows to calculate correctly
returns_ts <- rbind(rep(1, 49), as.data.frame(returns_ts))

# Make a data frame containing future i year returns
for(i in 1:5){
  assign(paste0("year_returns_", i),
         apply(returns_ts, 2, function(x) lead(x, 12 * i)) / returns_ts)
}

strategy_1_1 <- streaks_1 * year_returns_1
strategy_1_1$return <- apply(strategy_1_1, 1, function(x) mean(x, na.rm = T))


strategy_2_1 <- streaks_2 * year_returns_1
strategy_2_1$return <- apply(strategy_2_1, 1, function(x) mean(x, na.rm = T))

x <- expand.grid(1:5, 1:5)
colnames(x) <- c("streaks", "returns")

x <- as.data.frame(NA)
for(i in 1:5){
  for(j in 1:5){
    x <- rbind(x, paste0("strategy_", j, "_", i))
  }
}
x <- as.data.frame(x[2:26, ])
x <- cbind(x, expand.grid(1:5, 1:5))
colnames(x) <- c("strategy", "streaks", "year_returns")
# Unfactor
x$strategy <- as.character(x$strategy)

for(i in 1:5){
  for(j in 1:5){
    assign(x$strategy[i], get(paste0("streaks_", i)) * get(paste0("year_returns_", i)))
  }
}


for(i in 1:nrow(x)){
  assign(x$strategy[i], get(paste0("streaks_", 1)) * get(paste0("year_returns_", 1)))
}


test <- apply(year_returns_1, 2, function(x) lead(x, 1))
x <- test*streaks_5
x <- as.data.frame(x)
x$return <- apply(x, 1, function(x) mean(x, na.rm = T))
geoMean(x$return, na.rm = T)

no_streaks <- matrix(nrow = nrow(streaks), ncol = ncol(streaks))
no_streaks[] <- 1
strategy_all <- no_streaks * year_returns_1
z$return <- apply(z, 1, function(x) mean(x, na.rm = T))
geoMean(z$return, na.rm = T)




































