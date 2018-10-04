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
# Add ones to the start of the rows to calculate correctly, and name the row accordingly
returns_ts <- rbind(rep(1, 49), as.data.frame(returns_ts))
rownames(returns_ts)[1] <- as.character(as.Date(rownames(returns_ts)[2]) - months(1))

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

# Add index returns to years where the strategy wasn't invested
handle_missing <- function(x){
  x <- as.data.frame(x)
  colnames(x) <- "x"
  return(ifelse(is.na(x), index_vector$x, x$x))
}

# Make a data frame containing monthly returns
returns_monthly <- apply(returns_ts, 2, function(x) lead(x, 1) / x)
rownames(returns_monthly)[1] <- as.character(as.Date(rownames(returns_monthly)[2]) - years(1))

# Calculate returns for every strategy
for (year in 1:5){
  # For every holding period
  for(holding_period in 1:5){
  # Make a temporary data frame containing strategy returns
  temp <- as.data.frame(matrix(nrow = nrow(returns_ts),
                               ncol = ncol(returns_ts)))
  colnames(temp) <- colnames(returns_ts)
  rownames(temp) <- rownames(returns_ts)
  
  # For each column
  for(c in 1:ncol(get(paste0("down_years_", year)))){
    # For each row
    for(r in 1:I(nrow(get(paste0("down_years_", year))) - 12 * holding_period - 12 * year)){
      # Months that are not NA
      if(!is.na(get(paste0("down_years_", year))[r, c])){
        # If the sector has been down for year amount of years
        if(get(paste0("down_years_", year))[r, c] == T){
          # Put the next holding_period * 12 month returns in the data frame
            temp[r:I(r + 12 * holding_period - 1), c] <-
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

# Make a data frame for containing returns, Sharpe ratio & max dd
returns_holder <- as.data.frame(matrix(nrow = 5 * 5, ncol = 4))
colnames(returns_holder) <- c("Strategy", "Return", "Sharpe", "Max DD")

# Make a data frame for strategies & add index returns to it
strategies_holder <- data.frame(matrix(nrow = nrow(returns_ts), ncol = 1))
colnames(strategies_holder)[1] <- "index"
strategies_holder$index <- index_vector$x

# Add returns into data frame & bind strategies
k <- 1
for(i in 1:5){
  for(j in 1:5){
    returns_holder[k, 1] <- paste0(i, "_", j)
    returns_holder[k, 2] <- exp(mean(log(get(paste0("strategy_vector", i, "_", j))),
                                     na.rm = T)) ^ 12
    strategies_holder <- cbind(strategies_holder, get(paste0("strategy_vector", i, "_", j)))
    colnames(strategies_holder)[k + 1] <- paste0(i, "_", j)
    k <- k + 1
  }
}

# Delete last NA observation & make into decimals
strategies_holder <- head(strategies_holder, -1)
strategies_holder_xts <- as.xts(strategies_holder - 1)

# Add the index to returns_holder
returns_holder <- rbind(NA, returns_holder)
returns_holder[1, 1] <- "Index"
returns_holder[1, 2] <- exp(mean(log(strategies_holder$index), na.rm = T)) ^ 12

# Calculate Sharpe ratios & max drawdowns
returns_holder$Sharpe <- t(unname(SharpeRatio.annualized(strategies_holder_xts, 0.02 / 12)))
returns_holder$`Max DD` <- t(unname(maxDrawdown(strategies_holder_xts)))

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
        scale_y_continuous(trans = 'log2') +
        scale_color_manual(
    values = c("#F8766D", "#F8766D", "#F8766D", "#F8766D", "#F8766D"
               , "#7CAE00", "#7CAE00", "#7CAE00", "#7CAE00", "#7CAE00"
               , "#00BFC4", "#00BFC4", "#00BFC4", "#00BFC4", "#00BFC4"
               , "#C77CFF", "#C77CFF", "#C77CFF", "#C77CFF", "#C77CFF"
               , "#D3D3D3", "#D3D3D3", "#D3D3D3", "#D3D3D3", "#D3D3D3", "#000000"))

###############
# Remove unnecessary first row
returns_monthly <- tail(returns_monthly, -1)

# Calculate yearly returns & top 10% of sectors
momentum_ts <- apply(returns_ts, 2, function(x) lead(x, 12) / x)
momentum_months <- t(apply(momentum_ts, 1, function(x) x > quantile(x, .9, na.rm = T)))

# Keep only portfolios formed in January
momentum_months[month(rownames(momentum_months)) != 1] <- NA


# Make a temporary data frame sharing dimensions with momentum_ts
temp <- as.data.frame(matrix(nrow = nrow(momentum_ts),
                             ncol = ncol(momentum_ts)))
colnames(temp) <- colnames(momentum_ts)
rownames(temp) <- rownames(momentum_ts)
# For every column
for(c in 1:ncol(momentum_ts)){
  # For every row
  for(r in 1:I(nrow(momentum_ts) - 23)){
    # Each January
    if(!is.na(momentum_months[r, c])){
      # Sectors that were top x percentage in returns the year before
      if(momentum_months[r,c] == T){
        temp[r:I(r + 11), c] <- returns_monthly[I(r + 12):I(r + 23), c]
      }
    }
  }
}
# Delete months before the first January when the strategy wasn't invested
temp <- temp[I(month(rownames(temp))[1] + 2):nrow(temp), ]
# Mean return of each month
momentum_returns <- apply(temp, 1, function(x) mean(x, na.rm = T))
# Remove NAs from the latest years where the strategy wasn't invested
momentum_returns <- momentum_returns[!is.na(momentum_returns)]
# Join index and momentum strategy returns together
returns_df <- inner_join(rownames_to_column(as.data.frame(index_vector)),
                         rownames_to_column(as.data.frame(momentum_returns)))

#####
# Calculate yearly returns & bottom 10% of sectors
contra_ts <- apply(returns_ts, 2, function(x) lead(x, 12) / x)
contra_months <- t(apply(contra_ts, 1, function(x) x < quantile(x, .1, na.rm = T)))

# Keep only portfolios formed in January
contra_months[month(rownames(contra_months)) != 1] <- NA


# Make a temporary data frame sharing dimensions with contra_ts
temp2 <- as.data.frame(matrix(nrow = nrow(contra_ts),
                              ncol = ncol(contra_ts)))
colnames(temp2) <- colnames(contra_ts)
rownames(temp2) <- rownames(contra_ts)
# For every column
for(c in 1:ncol(contra_ts)){
  # For every row
  for(r in 1:I(nrow(contra_ts) - 23)){
    # Each January
    if(!is.na(contra_months[r, c])){
      # Sectors that were top x percentage in returns the year before
      if(contra_months[r,c] == T){
        temp2[r:I(r + 11), c] <- returns_monthly[I(r + 12):I(r + 23), c]
      }
    }
  }
}
# Delete months before the first January when the strategy wasn't invested
temp2 <- temp2[I(month(rownames(temp2))[1] + 2):nrow(temp2), ]
# Mean return of each month
contra_returns <- apply(temp2, 1, function(x) mean(x, na.rm = T))
# Remove NAs from the latest years where the strategy wasn't invested
contra_returns <- contra_returns[!is.na(contra_returns)]
# Join index and contra strategy returns together
returns_df <- inner_join(rownames_to_column(as.data.frame(index_vector)),
                         rownames_to_column(as.data.frame(momentum_returns)))
returns_df <- inner_join(returns_df,
                         rownames_to_column(as.data.frame(contra_returns)))
# returns_df$x <- lead(returns_df$x, 12)
returns_df[, 2:4] <- cumprod(returns_df[, 2:4])
returns_formatted <- gather(returns_df, strategy, index, -rowname)

ggplot(returns_formatted, aes(x = as.Date(rowname), y = index, color = strategy)) +
  geom_line(size = 1) +
  ggtitle(paste0("Momentum and contrarian strategies")) +
  xlab("Date") +
  scale_y_continuous(trans = 'log2') +
  scale_color_manual(
    values = c("#F8766D", "#7CAE00", "#000000"))


