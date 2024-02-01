

################# IMPLIED VOLATILITY CALCS ######################
library(RQuantLib)

# Define parameters for Black-Scholes model
option_type <- "call"  # "put" for put options
underlying_price <- 491.59
strike_price <- 490
risk_free_rate <- 0.0399
dividend_yield <- 0  # No dividends, so the yield is zero
maturity <- (as.Date("2024-04-19") - as.Date("2024-02-01")) / 365  # Time to expiration in years
market_price <- 13.70  # Current market price of the option

# Calculate the implied volatility
implied_vol <- tryCatch({
  AmericanOptionImpliedVolatility(
    type = option_type,
    value = market_price,
    underlying = underlying_price,
    strike = strike_price,
    dividendYield = dividend_yield,
    riskFreeRate = risk_free_rate,
    maturity = maturity,
    volatility = 0.2  # Initial guess for the implied volatility
  )
}, error = function(e) NA)

# Check if the calculation was successful
if (is.na(implied_vol)) {
  print("The implied volatility could not be calculated.")
} else {
  # Print the implied volatility
  print(paste("The implied volatility is:", implied_vol))
}

###################### OPTION FORECASTING ######################
library(RQuantLib)
install.packages("plotly")
library(plotly)

# Parameters # Assuming no dividends
start_date <- as.Date("2024-02-01")
expiry_date <- as.Date("2024-04-19")
dates <- seq(start_date, expiry_date, by="day")
prices <- seq(from = 450, to = 550, by = 1)  # Example range of prices

# Initialize a data frame to store the results
options_data <- expand.grid(Date = dates, Price = prices)
options_data$Value <- NA

# Calculate option values for each date and price
for(i in 1:nrow(options_data)) {
  maturity <- as.numeric(expiry_date - options_data$Date[i]) / 365
  options_data$Value[i] <- AmericanOption(
    type = "call",
    underlying = options_data$Price[i],
    strike = strike_price,
    dividendYield = dividend_yield,
    riskFreeRate = risk_free_rate,
    maturity = maturity,
    volatility = implied_vol
  )$value
}


# Convert dates to a numeric format for plot
options_data$DateNumeric <- as.numeric(options_data$Date)

# Create a list of custom tick text labels corresponding to the numeric dates
                        # change "day" to "week" for better looking graph
date_ticks <- data.frame(
  DateNumeric = as.numeric(seq(start_date, expiry_date, by="day")),
  DateLabel = as.character(seq(start_date, expiry_date, by="day"))
)

# Create 3D scatter plot
plot <- plot_ly(data = options_data, x = ~DateNumeric, y = ~Price, z = ~Value, type = 'scatter3d', mode = 'markers',
                marker = list(size = 2, color = options_data$Value, colorscale = 'Viridis', opacity = 0.8)) %>%
  layout(title = '3D Scatter Plot of Theoretical Option Value',
         scene = list(xaxis = list(title = 'Date', tickvals = date_ticks$DateNumeric, ticktext = date_ticks$DateLabel),
                      yaxis = list(title = 'Underlying SPY Price'),
                      zaxis = list(title = 'Theoretical Option Value')))

#Show plot
plot
