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

# Extract dates into a vector
dates <- ind$dates
ind$dates <- NULL
dates <- dates[1:which(is.na(dates))[1]-1]

# Show missing values
missmap(ind) # zero missing values

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

# Replace NAs with to calculate monthly returns
returns_ts <- replace_na(returns_ts, -99999)

monthly_returns <- as.data.frame(matrix(nrow = 1102, ncol = ncol(returns_ts))) ## ADJUST
for (i in 1:ncol(returns_ts)){
    monthly_returns[,i] <- monthlyReturn(returns_ts[,i])
}
# Add back colnames and NAs
colnames(monthly_returns) <- colnames(returns_ts)
monthly_returns[monthly_returns == 0] <- NA
returns_ts[returns_ts == -99999] <- NA

# Add back dates & leading zeros for months
year_month <- expand.grid(1:12,1926:2018)
year_month <- year_month[-c(1:6),]
year_month <- year_month[c(1:1102),] ## ADJUST
year_month$Var1 <- as.character(year_month$Var1)
year_month$Var1 <- ifelse(nchar(year_month$Var1) == 1, paste("0", year_month$Var1, sep = ""),
year_month$Var1)
rownames(monthly_returns) <- paste(year_month$Var2, year_month$Var1, sep = "-")

# 1 indicates that monthly return is below zero
below_zero <- ifelse(monthly_returns < 0, 1, 0)
# Calculate streak lengths
streaks <- apply(below_zero, 2, function(x) sequence(rle(x)$lengths))
# Keep only negative streaks
streaks <- below_zero * streaks
