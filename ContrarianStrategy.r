library(dplyr) # formatting data
library(Amelia) # missmap
library(lubridate) # handling dates
library(quantmod) # calculating monthly returns
options(scipen = 1000000)
ind <- read.csv("industrydata.csv", skip = 9, stringsAsFactors = F)

# Make dates into right format
colnames(ind)[1] <- "dates"
ind$dates <- as.Date(ind$dates, "%Y%m%d")

# Extract date
dates <- ind$dates
ind$dates <- NULL
dates <- dates[1:which(is.na(dates))[1]-1]

# Remove Rubbr & Paper because they contain blank periods in the middle
ind <- ind %>% select (-c(Rubbr, Paper))

# Show missing values
#missmap(ind)

# Remove equal weighted returns and NAs & make daily returns into decimals
ind <- ind[1:which(ind == "")[1]-1,]
ind <- sapply(ind, as.numeric)
ind[which(ind == -99.99)] <- NA
ind <- ind/100 + 1
ind <- as.matrix(ind)

# Store NA positions & replace them with 1 to calculate cumulative products
NA_pos <- which(is.na(ind))
ind[NA_pos] <- 1
returns <- apply(ind, 2, cumprod)
# Add back NAs to their original positions
returns[NA_pos] <- NA

# Attach dates
returns_dates <- as.data.frame(cbind(as.character(dates), returns))
names(returns_dates)[1] <- "dates"
returns_dates$dates <- as.Date(returns_dates$dates)

# Add dates after one year
returns_dates$dates_1y <- returns_dates$dates %m+% years(1)

# Make a time series object to calculate monthly returns
returns_ts <- as.xts(returns_dates, order.by = returns_dates[,1])
returns_ts$dates <- NULL
returns_ts$dates_1y <- NULL
storage.mode(returns_ts) <- "numeric"

