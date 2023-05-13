#Install Packages first time
#install.packages("quantmod")
#install.packages("writexl")
#install.packages("yfR")
# Clear workspace. 
rm(list=ls())

#Turn Off Scientific Notation
options(scipen=999)

#Load Packages
library(quantmod)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(writexl)
library(BatchGetSymbols)
library(yfR)
library(tidyquant)


sp500 <- GetSP500Stocks()
symbols <- sp500$Tickers
symbols <-gsub("\\.", "-", symbols)

first_date <- Sys.Date() - 365*3
last_date <- Sys.Date()

combined_all_data <- yf_get(
  tickers = symbols,
  first_date = first_date,
  last_date = last_date,
  thresh_bad_data = 0.25,
)

combined_all_data[order(combined_all_data$ticker, combined_all_data$ref_date),]


adjustedclose<-combined_all_data[,c(1:2,8)]

  
adjustedclose<-adjustedclose %>%
  pivot_wider(names_from = ticker, values_from = price_adjusted)



combined_all_df <- data.frame(adjustedclose)
#combined_all_df<-combined_all_df[combined_all_df$Date >= "2018-01-01" & combined_all_df$Date <= Sys.Date(), ]
#rownames(combined_all_df) = seq(length=nrow(combined_all_df))
rownames(combined_all_df)<-combined_all_df$ref_date
combined_all_df<-combined_all_df[-1]


#Populate daily to daily percentage change
for (col in names(combined_all_df)[1:ncol(combined_all_df)]) {
  symbol <- col
  new_col_name <- paste(symbol, "% Change")
  col_values <- combined_all_df[[col]]
  combined_all_df[[new_col_name]] <- log(col_values/lag(col_values))
}
#sort from most recent to oldest
#combined_all_df <- combined_all_df[rev(order(combined_all_df$Date)),]
combined_all_df_nbr_col <-ceiling(ncol(combined_all_df)/2)
combined_all_df_col <-combined_all_df_nbr_col+1
combined_all_df_col_end <-ceiling(ncol(combined_all_df))
# combined_all_df<-combined_all_df[,c(combined_all_df_col:combined_all_df_col_end)]

#calculate Average Daily Volatility
for (col in names(combined_all_df)[(ncol(combined_all_df)/2+1):ncol(combined_all_df)]) {
  symbol <- str_sub(col, 1, -10)
  new_col_name <- paste(symbol, "Avg Daily Volatility")
  col_values <- combined_all_df[[col]]
  combined_all_df[[new_col_name]] <- sd(col_values, na.rm=TRUE)
}

#calculate Annualized Volatility
for (col in names(combined_all_df)[(ncol(combined_all_df)-combined_all_df_nbr_col+1):ncol(combined_all_df)]) {
  symbol <- str_sub(col, 1, -22)
  new_col_name <- paste(symbol, "Annualized Volatility")
  col_values <- combined_all_df[[col]]
  combined_all_df[[new_col_name]] <- col_values*sqrt(252)
}

#calculate TSR
# for (col in names(combined_all_df)[1:combined_all_df_nbr_col]) {
#   symbol <- str_sub(col, 1, -6)
#   new_col_name <- paste(symbol, "TSR")
#   col_values <- combined_all_df[[col]]
#   combined_all_df[[new_col_name]] <- (col_values[nrow(combined_all_df)]-col_values[1])/col_values[1]
# }

for (col in names(combined_all_df)[1:combined_all_df_nbr_col]) {
  symbol <- sub("\\..*", "",col)
  new_col_name <- paste(symbol, "TSR")
  col_values <- combined_all_df[[col]]
  combined_all_df[[new_col_name]] <- (col_values[nrow(combined_all_df)]-col_values[1])/col_values[1]
}

combined_all_df <- tibble::rownames_to_column(combined_all_df, "Date")
combined_all_df$Date <- as.Date(combined_all_df$Date, format = "%Y-%m-%d")

new_order = sort(colnames(combined_all_df))
combined_all_df_sorted <- combined_all_df[, new_order]

#write to excel
write_xlsx(combined_all_df, paste0("VolatilityOutput",Sys.Date(),".xlsx"))

#make into dataframe
Output <- as.data.frame(t(combined_all_df))

colnames(Output)<-Output[1,]
Output = Output[-1, ]

SummaryOutput<- Output[ -c(1:(combined_all_df_nbr_col*2)), ]

#Reduce to single column
SummaryOutput<- SummaryOutput[-c(2:ncol(SummaryOutput))]

#Remove irrevant to summary data
SummaryOutput$HeaderName <- row.names(SummaryOutput)
rownames(SummaryOutput) <- 1:nrow(SummaryOutput)
SummaryOutput<-SummaryOutput[-1,]
SummaryOutput<-SummaryOutput[,c(2,1)]
colnames(SummaryOutput)[2] <- "Value"
SummaryOutput<-SummaryOutput[!grepl("Adjusted",SummaryOutput$HeaderName),]
SummaryOutput<-SummaryOutput[!grepl("Adj %",SummaryOutput$HeaderName),]
SummaryOutput<-SummaryOutput[order(SummaryOutput$HeaderName),]

#write to excel
write_xlsx(SummaryOutput, "VolatilitySummaryOutput.xlsx")


volatile_stock<-getSymbols("PYPL",from=Sys.Date()-730,to=Sys.Date(),auto.assign = F)
volatile_stock<-as.data.frame(volatile_stock)
nbrshares<-floor(200000/volatile_stock[1,6])
volatile_stock$nbrshares<-nbrshares
volatile_stock$valueshares<-round(volatile_stock$nbrshares*volatile_stock$PYPL.Adjusted,2)
