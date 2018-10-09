library(Amelia) # missmap
library(ggplot2) # plotting
library(lubridate) # handling dates
library(tidyverse) # replace_na function & formatting data
library(PerformanceAnalytics) # Sharpe ratio and maximum drawdown
options(scipen = 1000000)

use_equal_weighted_data <- F

# Read the data & keep only needed rows (Average value weighted monthly returns)
rawdata <- read.csv("49_Industry_Portfolios.CSV", skip = 11, stringsAsFactors = F)
if(use_equal_weighted_data){
  rawdata <- rawdata[I(first(grep("Average", rawdata$X)) + 2):
                       I(first(grep("Annual", rawdata$X)) - 1), ]
} else {rawdata <- rawdata[1:first(grep("Average", rawdata$X)) - 1, ]}


# Format dates
colnames(rawdata)[1] <- "dates"
rawdata$dates <- paste0(rawdata$dates, "01")
rawdata$dates <- as.Date(rawdata$dates, "%Y%m%d")

# Extract dates into a vector & delete them from rawdata
dates <- rawdata$dates
rawdata$dates <- NULL

# Remove NAs (-99.99) & make daily returns into decimals
rawdata <- sapply(rawdata, as.numeric)
rawdata[which(rawdata == -99.99)] <- NA
rawdata <- rawdata / 100 + 1

# Show missing values
rawdata <- as.data.frame(rawdata)
missmap(rawdata)
rawdata <- as.matrix(rawdata)

# Store NA positions & replace them with 1 to calculate cumulative products
NA_pos <- which(is.na(rawdata))
rawdata[NA_pos] <- 1
sector_returns <- apply(rawdata, 2, cumprod)
# Add back NAs to their original positions
sector_returns[NA_pos] <- NA

# Attach dates to the cumulative products and format them
returns_dates <- as.data.frame(cbind(as.character(dates), sector_returns))
colnames(returns_dates)[1] <- "dates"
returns_dates$dates <- as.Date(returns_dates$dates)

# Make a time series object to calculate down years
returns_ts <- as.xts(returns_dates, order.by = returns_dates[, 1])
returns_ts$dates <- NULL
storage.mode(returns_ts) <- "numeric"
# Add ones to the start of the rows to calculate correctly, and add date accordingly
returns_ts <- rbind(rep(1, 49), as.data.frame(returns_ts))
rownames(returns_ts)[1] <- as.character(as.Date(rownames(returns_ts)[2]) - months(1))

#####################
# Contrarian strategy

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

# Calculate index returns
index <- apply(returns_ts, 2, function(x) lead(x, 1) / x)
index_vector <- as.data.frame(apply(index, 1, function(x) mean(x, na.rm = T)))
colnames(index_vector) <- "x"

# Function to add index returns to months where the strategy was not invested
handle_missing <- function(x) {
  x <- as.data.frame(x)
  colnames(x) <- "x"
  return(ifelse(is.na(x), index_vector$x, x$x))
}

# Make a data frame containing monthly returns
returns_monthly <- apply(returns_ts, 2, function(x) lead(x, 1) / x)
rownames(returns_monthly)[1] <- as.character(as.Date(rownames(returns_monthly)[2]) - months(1))

# Calculate returns for every contrarian strategy
for (year in 1:5) {
  # For every holding period
  for (holding_period in 1:5) {
    # Make a temporary data frame containing strategy returns
    temp <- as.data.frame(matrix(
      nrow = nrow(returns_ts),
      ncol = ncol(returns_ts)
    ))
    colnames(temp) <- colnames(returns_ts)
    rownames(temp) <- rownames(returns_ts)
    
    # For each column
    for (c in 1:ncol(get(paste0("down_years_", year)))) {
      # For each row
      for (r in 1:I(nrow(get(paste0("down_years_", year))) - 12 * holding_period - 12 * year)) {
        # Months that are not NA
        if (!is.na(get(paste0("down_years_", year))[r, c])) {
          # If the sector has been down for a amount of years equal to year
          if (get(paste0("down_years_", year))[r, c] == T) {
            # Put the next holding_period * 12 month returns in the data frame
            temp[I(r + 11):I(r + 11 + 12 * holding_period - 1), c] <-
              returns_monthly[I(r + 12 * year):I(r + (12 * holding_period - 1 + 12 * year)), c]
          }
        }
      }
    }
    assign(paste0("strategy_df", year, "_", holding_period), temp)
    # Calculate monthly returns and put to vector
    temp2 <- apply(temp, 1, function(x) mean(x, na.rm = T))
    assign(paste0("strategy_returns", year, "_", holding_period), temp2)
    # Calculate yearly returns with non-invested months invested in the index
    temp3 <- handle_missing(temp2)
    assign(paste0("strategy_vector", year, "_", holding_period), temp3)
  }
}

# Make a data frame for containing returns, Sharpe ratio, max dd and volatility
returns_holder <- as.data.frame(matrix(nrow = 5 * 5, ncol = 6))
colnames(returns_holder) <- c("Strategy", "Return", "Sharpe", "Max DD", "Volatility",
                              "Information ratio")

# Make a data frame for strategies & add index returns to it
strategies_holder <- data.frame(matrix(nrow = nrow(returns_ts), ncol = 1))
colnames(strategies_holder)[1] <- "index"
strategies_holder$index <- index_vector$x

# Add returns into data frame & bind strategies
k <- 1
for (i in 1:5) {
  for (j in 1:5) {
    returns_holder[k, 1] <- paste0(i, "_", j)
    returns_holder[k, 2] <- exp(mean(log(get(paste0("strategy_vector", i, "_", j))),
                                     na.rm = T
    ))^12
    strategies_holder <- cbind(strategies_holder, get(paste0("strategy_vector", i, "_", j)))
    colnames(strategies_holder)[k + 1] <- paste0(i, "_", j)
    k <- k + 1
  }
}

# Delete last NA observation & turn into decimals for performance metric calculation
strategies_holder <- head(strategies_holder, -1)
strategies_holder_xts <- as.xts(strategies_holder - 1)

# Add the index and its returns to returns_holder
returns_holder <- rbind(NA, returns_holder)
returns_holder[1, 1] <- "Index"
returns_holder[1, 2] <- exp(mean(log(strategies_holder$index), na.rm = T))^12

# Calculate Sharpe ratios, max drawdowns and volatilities and put them to returns_holder
returns_holder$Sharpe <- t(unname(SharpeRatio.annualized(strategies_holder_xts, 0.02 / 12)))
returns_holder$`Max DD` <- t(unname(maxDrawdown(strategies_holder_xts)))
returns_holder$Volatility <- t(unname(StdDev.annualized(strategies_holder_xts)))
returns_holder$`Information ratio` <- t(unname(InformationRatio(strategies_holder_xts,
                                                                strategies_holder_xts$index)))

# Make cumulative product calculation for plotting
strategies_cumprod <- apply(strategies_holder, 2, cumprod)
strategies_cumprod <- as.data.frame(strategies_cumprod)
strategies_cumprod$Date <- rownames(strategies_cumprod)

# Gather & plot
strategies_formatted <- gather(strategies_cumprod, Strategy, index, -Date)
ggplot(strategies_formatted, aes(x = as.Date(Date), y = index, color = Strategy)) +
  geom_line(size = 1) +
  ggtitle(paste0("Strategies formed every month")) +
  xlab("Date") +
  ylab("Logarithmic returns") +
  scale_y_continuous(trans = "log2") +
  scale_color_manual(
    values = c(
      "#F8766D", "#F8766D", "#F8766D", "#F8766D", "#F8766D"
      , "#7CAE00", "#7CAE00", "#7CAE00", "#7CAE00", "#7CAE00"
      , "#00BFC4", "#00BFC4", "#00BFC4", "#00BFC4", "#00BFC4"
      , "#C77CFF", "#C77CFF", "#C77CFF", "#C77CFF", "#C77CFF"
      , "#D3D3D3", "#D3D3D3", "#D3D3D3", "#D3D3D3", "#D3D3D3", "#000000"
    )
  )

#############################
# Winner and loser strategies
#################
# Winner strategy

# Calculate yearly returns & top 10% of sectors
momentum_ts <- apply(returns_ts, 2, function(x) lead(x, 9) / x)
momentum_months <- t(apply(momentum_ts, 1, function(x) x > quantile(x, .9, na.rm = T)))

# Keep only portfolios formed in January
momentum_months[month(rownames(momentum_months)) != 1] <- NA

# Make a temporary data frame sharing dimensions with momentum_ts
temp <- as.data.frame(matrix(
  nrow = nrow(momentum_ts),
  ncol = ncol(momentum_ts)))
colnames(temp) <- colnames(momentum_ts)
rownames(temp) <- rownames(momentum_ts)
# For every column
for (c in 1:ncol(momentum_ts)) {
  # For every row
  for (r in 1:I(nrow(momentum_ts) - 23)) {
    # Each January
    if (!is.na(momentum_months[r, c])) {
      # Sectors that were top decile in returns 12-2 months before
      if (momentum_months[r, c] == T) {
        temp[I(r + 12):I(r + 23), c] <- returns_monthly[I(r + 12):I(r + 23), c]
      }
    }
  }
}

# Mean return of each month
Winner <- apply(temp, 1, function(x) mean(x, na.rm = T))
# Remove NAs from the latest years where the strategy was not invested
Winner <- Winner[!is.na(Winner)]
# Join index and momentum strategy returns together
returns_df <- inner_join(
  rownames_to_column(as.data.frame(index_vector)),
  rownames_to_column(as.data.frame(Winner)))

################
# Loser strategy

# Calculate yearly returns & bottom 10% of sectors
contra_ts <- apply(returns_ts, 2, function(x) lead(x, 35) / lag(x, 1))
contra_months <- t(apply(contra_ts, 1, function(x) x < quantile(x, .1, na.rm = T)))

# Keep only portfolios formed in January
contra_months[month(rownames(contra_months)) != 1] <- NA

# Make a temporary data frame sharing dimensions with contra_ts
temp2 <- as.data.frame(matrix(
  nrow = nrow(contra_ts),
  ncol = ncol(contra_ts)))
colnames(temp2) <- colnames(contra_ts)
rownames(temp2) <- rownames(contra_ts)
# For every column
for (c in 1:ncol(contra_ts)) {
  # For every row
  for (r in 1:I(nrow(contra_ts) - 43)) {
    # Each January
    if (!is.na(contra_months[r, c])) {
      # Sectors that were bottom decile in returns during the last 36 months
      if (contra_months[r, c] == T) {
        temp2[I(r + 36):I(r + 47), c] <- returns_monthly[I(r + 35):I(r + 46), c]
      }
    }
  }
}
# Delete months before the first January when the strategy was not invested
temp2 <- temp2[I(month(rownames(temp2))[1] + 2):nrow(temp2), ]
# Mean return of each month
Loser <- apply(temp2, 1, function(x) mean(x, na.rm = T))
# Remove NAs from the latest years where the strategy was not invested
Loser <- Loser[!is.na(Loser)]

##############

# Join index and contra strategy returns together
returns_df <- inner_join(
  rownames_to_column(as.data.frame(index_vector)),
  rownames_to_column(as.data.frame(Winner)))
returns_df <- inner_join(
  returns_df,
  rownames_to_column(as.data.frame(Loser)))

# Rename and calculate cumulative products & add dates from rownames
colnames(returns_df)[2] <- "Index"
returns_cumprod <- cumprod(returns_df[, 2:4])
returns_cumprod$date <- rownames(returns_df)

# Make a data frame for performance metrics
returns_holder_mc <- as.data.frame(matrix(nrow = 3, ncol = 6))
colnames(returns_holder_mc) <- c("Strategy", "Return", "Sharpe", "Max DD", "Volatility",
                                 "Information ratio")
returns_df <- column_to_rownames(returns_df)
# Add strategy names
returns_holder_mc$Strategy <- colnames(returns_df)

# Make an xts object for holding decimal returns
returns_xts <- as.xts(returns_df - 1)

# Calculate returns,  Sharpe ratios, max drawdowns & volatilities
for (i in 1:3) {
  returns_holder_mc$Return[i] <- exp(mean(log(returns_df[, i]), na.rm = T))^12
}
returns_holder_mc$Sharpe <- t(unname(SharpeRatio.annualized(returns_xts, 0.02 / 12)))
returns_holder_mc$`Max DD` <- t(unname(maxDrawdown(returns_xts)))
returns_holder_mc$Volatility <- t(unname(StdDev.annualized(returns_xts)))
returns_holder_mc$`Information ratio` <- t(unname(InformationRatio(returns_xts, returns_xts$Index)))

# Format returns, gather them & plot
returns_cumprod$date <- rownames(returns_df)
returns_formatted <- gather(returns_cumprod, Strategy, index, -date)
returns_formatted$Strategy <- factor(returns_formatted$Strategy,
                                     levels = c("Winner", "Index", "Loser"))

ggplot(returns_formatted, aes(x = as.Date(date), y = index, color = Strategy)) +
  geom_line(size = 1) +
  ggtitle(paste0("Winner and loser strategies")) +
  xlab("Date") +
  ylab("Logarithmic returns") +
  scale_y_continuous(trans = "log2") +
  scale_color_manual(values = c("#7CAE00", "#000000", "#F8766D"))
