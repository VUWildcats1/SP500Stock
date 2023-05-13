#Install Packages first time
#install.packages("quantmod")
#install.packages("writexl")
#install.packages("yfR")

# Clear Workspace ---------------------------------------------------------
rm(list=ls())

# Turn Off Scientific Notation --------------------------------------------
options(scipen=999)

# Load Packages -----------------------------------------------------------
library(quantmod)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(writexl)
library(BatchGetSymbols)
library(yfR)
library(tidyquant)

# Get S&P 500 Tickers and Clean -------------------------------------------
sp500 <- GetSP500Stocks()
symbols <- sp500$Tickers
symbols <-gsub("\\.", "-", symbols)

# Set Dates of Review of Adjusted Closing ---------------------------------
first_date <- Sys.Date() - 365*3
last_date <- Sys.Date()

# Run function to get stock data ------------------------------------------
combined_all_data <- yf_get(
  tickers = symbols,
  first_date = first_date,
  last_date = last_date,
  thresh_bad_data = 0.25,
)

# Re-sort data ------------------------------------------------------------
combined_all_data[order(combined_all_data$ticker, combined_all_data$ref_date),]

# Extract Adjusted Closing Price (considers splits, dividends and such-----
adjustedclose<-combined_all_data[,c(1:2,8)]

# Dataframe wide and set dataframe-----------------------------------------
adjustedclose<-as.data.frame(adjustedclose %>%
  pivot_wider(names_from = ticker, values_from = price_adjusted))
date_row_name<-as.matrix(adjustedclose[1])
rownames(adjustedclose)<-date_row_name
adjustedclose<-adjustedclose[-1]

# Populate daily percentage change ----------------------------------------
for (col in names(adjustedclose)[1:ncol(adjustedclose)]) {
  symbol <- col
  new_col_name <- paste(symbol, "% Change")
  col_values <- adjustedclose[[col]]
  adjustedclose[[new_col_name]] <- log(col_values/lag(col_values))
}


# Set Column Count Details ------------------------------------------------
adjustedclose_nbr_col <-ceiling(ncol(adjustedclose)/2)
adjustedclose_col <-adjustedclose_nbr_col+1
adjustedclose_col_end <-ceiling(ncol(adjustedclose))

# Calculate Average Daily Volatility --------------------------------------
for (col in names(adjustedclose)[(ncol(adjustedclose)/2+1):ncol(adjustedclose)]) {
  symbol <- str_sub(col, 1, -10)
  new_col_name <- paste(symbol, "Avg Daily Volatility")
  col_values <- adjustedclose[[col]]
  adjustedclose[[new_col_name]] <- sd(col_values, na.rm=TRUE)
}


# Calculate Annualized Volatility -----------------------------------------
for (col in names(adjustedclose)[(ncol(adjustedclose)-adjustedclose_nbr_col+1):ncol(adjustedclose)]) {
  symbol <- str_sub(col, 1, -22)
  new_col_name <- paste(symbol, "Annualized Volatility")
  col_values <- adjustedclose[[col]]
  adjustedclose[[new_col_name]] <- col_values*sqrt(252)
}


# Calculation TSR (Total Shareholder Returns) -----------------------------
for (col in names(adjustedclose)[1:adjustedclose_nbr_col]) {
  symbol <- sub("\\..*", "",col)
  new_col_name <- paste(symbol, "TSR")
  col_values <- adjustedclose[[col]]
  adjustedclose[[new_col_name]] <- (col_values[nrow(adjustedclose)]-col_values[1])/col_values[1]
}

# Refine the dataframe by reintroducing date ------------------------------
adjustedclose <- tibble::rownames_to_column(adjustedclose, "Date")

# Sort Column Headers to group data by Ticker -----------------------------
new_order = sort(colnames(adjustedclose))
adjustedclose_sorted <- adjustedclose[, new_order]
adjustedclose_sorted<-adjustedclose_sorted %>%  relocate(Date)
adjustedclose_sorted$Date <- as.Date(adjustedclose_sorted$Date, format = "%Y-%m-%d")


# Write Adjusted Close Sorted to Excel ------------------------------------
write_xlsx(adjustedclose_sorted, paste0("VolatilityOutput",Sys.Date(),".xlsx"))

# Transpose as Dataframe --------------------------------------------------
Output <- as.data.frame(t(adjustedclose_sorted))
colnames(Output)<-Output[1,]
Output = Output[-1, ]

SummaryOutput<-adjustedclose[ ,-c(1:(adjustedclose_nbr_col*2+1)) ]

#Reduce to single column --------------------------------------------------
SummaryOutput<- SummaryOutput[-c(2:ncol(SummaryOutput)),]
SummaryOutput<- as.data.frame(t(SummaryOutput))
colnames(SummaryOutput)[1] <- "Value"

#write to excel -----------------------------------------------------------
write_xlsx(SummaryOutput, "VolatilitySummaryOutput.xlsx")