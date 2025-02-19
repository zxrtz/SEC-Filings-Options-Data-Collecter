install.packages("quantmod")
install.packages("ggplot2")
library(quantmod)
library(ggplot2)

getOptionChain_mod <- function(symbol) {
  # Get real-time quote for the stock
  quote <- quantmod::getQuote(symbol)
  opening_price <- as.numeric(quote$Open)
  
  # Get option chain
  oc <- getOptionChain(symbol)
  if (!is.null(oc)) {
    # Define the range for ITM and OTM
    range <- c(opening_price - 10, opening_price + 10)
    # Filter calls and puts within the defined range
    oc$calls <- oc$calls[oc$calls$Strike >= range[1] & oc$calls$Strike <= range[2], ]
    oc$puts <- oc$puts[oc$puts$Strike >= range[1] & oc$puts$Strike <= range[2], ]
  }
  return(oc)
}

# Create an empty data frame to store data
option_prices <- data.frame(Time = character(), Price = numeric(), Type = character())

# Get initial plot
gg <- ggplot(option_prices, aes(x = Time, y = Price, color = Type)) +
  geom_line(size = 1.5) +
  labs(x = "Time", y = "Option Price", color = "Option Type") +
  ggtitle("Average Option Price Over Time") +
  theme_minimal()

print(gg)

while(TRUE) {
  # Get current time
  timestamp <- format(Sys.time(), "%H:%M:%S")
  
  # Get option chain
  currentOptionChain <- getOptionChain_mod("SPY")
  
  # Calculate the average price of calls and puts
  avg_call_price <- mean(currentOptionChain$calls$Last)
  avg_put_price <- mean(currentOptionChain$puts$Last)
  
  # Print average call and put prices
  cat("Average Call Price:", avg_call_price, "\n")
  cat("Average Put Price:", avg_put_price, "\n")
  
  # Add data to data frame
  option_prices <- rbind(option_prices, data.frame(Time = timestamp, Price = avg_call_price, Type = "Call"))
  option_prices <- rbind(option_prices, data.frame(Time = timestamp, Price = avg_put_price, Type = "Put"))
  
  # Keep only the data within the last 24 hours
  option_prices <- option_prices[as.POSIXct(option_prices$Time, format="%H:%M:%S") > Sys.time() - 24*3600, ]
  
  # Update plot
  gg <- ggplot(option_prices, aes(x = as.POSIXct(Time, format="%H:%M:%S"), y = Price, color = Type)) +
    geom_line(size = 1.2) +
    labs(x = "Time", y = "Option Price", color = "Option Type") +
    scale_color_manual(values = c("green", "red")) +
    ggtitle("Average Option Price Over Time") +
    theme_minimal()
  
  print(gg)
  
  # Pause execution for x seconds
  Sys.sleep(60)
}

APT <- getOptionChain_mod("SPY")
APT