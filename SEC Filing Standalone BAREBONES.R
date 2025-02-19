# require packages
require("stringr");require("data.table");require("BatchGetSymbols")
require("quantmod");require("dplyr");require("PerformanceAnalytics");require("pbapply")
library(timeDate)
library(RQuantLib)
library(xts)
library(zoo)

# SET WD AUTOMATICALLY
setwd("C:/Users/Dylan/Desktop/STONKS BACKTEST/new insdier project")

# Define the formatVOL function
formatVOL <- function(x) {
  as.numeric(gsub(",", "", x))
}

# **********************************************************************************
#                INSIDER TRANSACTIONS IMPORTING
# **********************************************************************************
url <- "https://finviz.com/insidertrading.ashx"
TABLE <- read_html(url)
TABLE <- TABLE  %>% 
  html_nodes("table") %>%
  .[[7]] %>%
  html_table(header=TRUE,fill=TRUE)

noms <- names(TABLE)
noms <- gsub("#","Num",noms)
noms <- gsub("\\$","Dollar",noms)
noms <- gsub("\\(","",noms)
noms <- gsub("\\)","",noms)
noms <- gsub(" ","",noms)
colnames(TABLE) <- noms
TABLE$Date <- as.Date(as.character(TABLE$Date),format="%b %d")
TABLE$NumShares<- formatVOL(TABLE$NumShares)
TABLE$ValueDollar<- formatVOL(TABLE$ValueDollar)
TABLE$NumSharesTotal<- formatVOL(TABLE$NumSharesTotal)
TABLE$SECForm4 <- as.POSIXct(as.character(TABLE$SECForm4), format ="%b %d %I:%M %p")

TABLE$SECForm4 <- as.Date(TABLE$SECForm4)
TABLE$DailyReturn <- NA
