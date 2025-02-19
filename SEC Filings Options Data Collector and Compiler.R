# require packages
require("stringr");require("data.table");require("BatchGetSymbols")
require("quantmod");require("dplyr");require("PerformanceAnalytics");require("pbapply")
library(timeDate)
library(RQuantLib)
library(xts)
library(zoo)

# Define the formatVOL function
formatVOL <- function(x) {
  as.numeric(gsub(",", "", x))
}

# **********************************************************************************
#                INSIDER TRANSACTIONS
# **********************************************************************************
url <- "https://finviz.com/insidertrading.ashx"
TABLE <- read_html(url)
TABLE <- TABLE  %>% html_nodes("table") %>% .[[7]] %>%html_table(header=TRUE,fill=TRUE)
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

# remove dupes
TABLE <- na.omit(TABLE)

# Define the directory path
directory_path <- "C:/Users/Dylan/Desktop/STONKS BACKTEST/new insdier project/Filings/"

# Generate a unique filename with a timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
file_name <- paste0("TABLE_", timestamp, ".rds")
save_path <- file.path(directory_path, file_name)

# Save the TABLE data frame to the specified path
saveRDS(TABLE, file = save_path)




# list files and subset for December-2020 Transaction only
FILES = list.files("C:/Users/Dylan/Desktop/STONKS BACKTEST/new insdier project/Filings",full.names = TRUE)


# rowbind all transactions and remove duplicates
ALL = rbindlist(lapply(as.list(FILES), readRDS),use.names = TRUE)
ALL = unique(ALL)
# subset ALL to Date Range
START = "2024-08-1"
END = Sys.Date()
ALL = subset(ALL,ALL$Date>=START)





# extract unique tickers
tickers = unique(ALL$Ticker)

# BatchGetSymbols ~ 1064 unique tickers
data = BatchGetSymbols(tickers=tickers,first.date = START,last.date = END,
                       thresh.bad.data = 0.75,bench.ticker = "^GSPC",
                       type.return = "arit",freq.data = "daily", 
                       how.to.aggregate = "last")
# saving data for re-run
saveRDS(data,"stkBATCH_20201130-20210112.rds")
# data = readRDS("stkBATCH_20201130-20210112.rds")
# exlcude tickers with bad data
KEEP = data$df.control[which(data$df.control$threshold.decision == "KEEP"),]
tickers = KEEP$ticker
# stock data
data = data$df.tickers