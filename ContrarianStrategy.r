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
ind <- ind/100 + 1

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

# Replace NAs with -99999 to calculate monthly returns
returns_ts <- replace_na(returns_ts, -99999)

# Calculate monthly returns & turn into data frame for below zero calulation
monthly_returns <- returns_ts / apply(returns_ts, 2, function(x) lag(x, 1))
monthly_returns <- as.data.frame(monthly_returns)

# 1 indicates that monthly return is below zero
below_zero <- ifelse(monthly_returns < 1, 1, 0)
# Calculate streak lengths
streaks <- apply(below_zero, 2, function(x) sequence(rle(x)$lengths))
# Keep only negative streaks
streaks <- below_zero * streaks
