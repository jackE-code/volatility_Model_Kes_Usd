library(rsconnect)
library(shinydashboard)
library(shiny)
library(quantmod)
library(forecast)
library(plotly)
library(xts)
library(ggplot2)
library(DT)
library(tibble)
library(prophet)
# Set the currency pair
symbol <- "USDKES=X"

# Get historical price data
getSymbols(symbol, src = "yahoo", from = "2005-01-01", to = Sys.Date())

# Handle missing values using linear interpolation
data <- na.approx(Cl(get(symbol)))

# Extract closing prices
KES_data <- Cl(data)

# Differencing to make the data stationary
diff_KES_data <- diff(KES_data, lag = 1, differences = 1)  # Assuming d=1

# Fit ARIMA model
arima_model <- arima(diff_KES_data, order = c(2, 1, 0))  # Replace with your p, d, q values


# Train ARIMA model
arima_model <- auto.arima(KES_data)

# Forecasting
forecast_horizon <- 30
forecast_values <- forecast(arima_model, h = forecast_horizon)$mean


# Convert numerical indices to dates
forecast_dates <- index(KES_data)[length(KES_data)] + seq(1, forecast_horizon)

rate <- KES_data[,"USDKES=X.Close"]


## Create a daily Date object - helps my work on dates

inds <- seq(as.Date("2005-01-03"), as.Date("2024-01-12"), by = "day")

## Create a time series object

rate.ts <- ts(rate, start = c(2005, as.numeric(format(inds[1], "%j"))),
              frequency = 365)

decomposed.series <- decompose(rate.ts)

# Define a plot function to allows us change the title

decomp.plot <- function(x, main = NULL, ...)
{
  if(is.null(main))
    main <- paste("Decomposition of", x$type, "time series")
  plot(cbind(observed = x$random + if (x$type == "additive")
    x$trend + x$seasonal
    else x$trend * x$seasonal, trend = x$trend, seasonal = x$seasonal,
    random = x$random), main = main, ...)
}


# Added Functions Volatility
# Function to calculate volatility
calculate_volatility <- function(time_series, model_order = c(2, 1, 0)) {
  # Differencing to make the data stationary
  diff_data2 <- diff(time_series, lag = 1, differences = 1)
  
  # Fit ARIMA model
  arima_model2 <- arima(diff_data, order = model_order)
  
  # Extract residuals
  residuals <- residuals(arima_model2)
  
  # Calculate volatility as the standard deviation of residuals
  volatility <- sd(residuals)
  
  return(volatility)
}

# Function to train ARIMA model
train_arima_model <- function(time_series, model_order = c(2, 1, 0)) {
  # Differencing to make the data stationary
  diff_data <- diff(time_series, lag = 1, differences = 1)
  
  # Fit ARIMA model
  arima_model2 <- arima(diff_data, order = model_order)
  
  return(arima_model2)
}
# Function to forecast volatility
forecast_volatility <- function(model, horizon = 365) {
  # Forecast using the trained ARIMA model
  forecast_values2 <- forecast(model, h = horizon)$mean
  
  # Extract the last date in the original time series
  last_date <- index(tail(KES_data, 1))
  
  # Generate a sequence of dates for the forecast horizon
  forecast_dates2 <- seq(last_date + 1, length.out = horizon, by = "days")
  
  # Create an xts object with forecast values and dates
  volatility_forecast <- xts(forecast_values2, order.by = as.Date(forecast_dates2))
  
  return(volatility_forecast)
}

# Train ARIMA model
arima_model2 <- train_arima_model(KES_data)

# Forecast volatility for the next year
forecast_horizon2 <- 365
volatility_forecast <- forecast_volatility(arima_model, horizon = forecast_horizon2)


# Extract closing prices and create a time series data frame
dataa <- data.frame(Date = index(Cl(get(symbol))), Price = as.numeric(Cl(get(symbol))))

# Rename the columns as required by prophet
colnames(dataa) <- c("ds", "y")



# Prophet model
prophet_model <- prophet(dataa, daily.seasonality = TRUE)

# Create a data frame with future dates for prediction
future_dates <- make_future_dataframe(prophet_model, periods = 365)

# Generate forecasts
forecast <- predict(prophet_model, future_dates)

#
# End

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Volatility Modelling"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Charts", tabName = "Charts", icon = icon("th")),
      menuItem("Data", tabName = "Data", icon = icon("database")),
      menuItem("Team", tabName = "Team", icon = icon("user")),
      menuItem("Extensive", tabName = "Extent", icon = icon("folder-open"),badgeLabel = "new", badgeColor = "green")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("MODELLING THE VOLATILITY OF USD/KES EXCHANGE RATES USING TIME SERIES ANALYSIS - ARIMA MODELS"),
              fluidRow(
                box(
                  title = "Introduction",
                  status = "success",
                  solidHeader = TRUE,
                  "Embark on a journey through the financial landscapes of Kenya as we delve into the intriguing world of exchange rate dynamics. In 1993, Kenya boldly embraced a floating exchange rate system, paving the way for a captivating exploration of the factors influencing currency values.",
                  br(),
                  "This web app acts as a dynamic rope bridge, connecting the present to the future with daily updates. Discover the pulse of the USD/KES exchange rate and unlock valuable insights. Our study goes beyond numbers; it's a voyage with implications spanning macroeconomics and finance.",
                  br(), br(),
                  width = 12,
                  fluidRow(
                    box(
                      title = "Statement of the Problem",
                      status = "info",
                      "Exchange rate volatility plays a pivotal role in the decision-making processes of investors and policymakers. The Kenyan economy has witnessed significant fluctuations in the exchange rate of the KES against the USD in recent years.",
                      br(), 
                      "In this study, our objective is to provide a comprehensive analysis of how the Kenyan shilling has performed against the USD. We aim to shed light on the high exchange rate volatility experienced by the Kenyan economy, offering valuable insights for informed decision-making.",
                      width = 6
                    ),
                    
                    box(
                      title = "Objectives",
                      status = "info",
                      "Explore the performance of the KES against the US dollar in recent years, focusing on the period from 2005 to 2024.",
                      br(), 
                      "Forecast the future value of the KES against the US dollar, providing a glimpse into potential trends and changes.",
                      br(), 
                      "Deliver detailed data on the trends in KES/USD exchange rates, employing various time series analysis techniques to derive meaningful insights.",
                      width = 6
                    )
                    ,
                  ))
              ),
              
              fluidRow(
                box(
                  title = "Timeseries Plot  (2005 - 2024)",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("chartSeriesPlot"),
                  width = 12
                ),
                box(
                  title = "Exchange Rate Plot",
                  status = "success",
                  solidHeader = TRUE,
                  plotOutput("chartSeriesPlotx"),
                  width = 6
                ),
                box(
                  title = "Model's Forecast Validity Plot",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("prophet_plot"),
                  width = 6
                ),
                box(
                  title = "ARIMA Forecast (2,1,0)",
                  status = "danger",
                  solidHeader = TRUE,
                  plotlyOutput("arimaPlot"),
                  width = 12
                ),
                
                box(
                  
                  title = "Decomposed Time Series Plot",
                  status = "info",
                  solidHeader = TRUE,
                  plotOutput("decompositionPlot"),
                  width = 12)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Charts",
              h2("CHARTS & ASSEMENT"),
              fluidRow(
                box(title = "Time Series Plot (2005 - 2024)",
                    status = "success",
                    solidHeader = TRUE,
                    plotOutput("xtsplot2"),
                    width = 12),
                box(title = "Decomposition series",
                    status = "info",
                    solidHeader = TRUE,
                    plotOutput("decompositionPlot2"),
                    width = 12),
                box(title = "Residual ARIMA(2,1,0)",
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("residualsPlot"),
                    width = 12),
                box(title = "ARIMA(2,1,0)",
                    status = "danger",
                    solidHeader = TRUE,
                    plotlyOutput("arimaPlot2"),
                    width = 12),
                box(
                  title = "Volatility Forecast ARIMA (2,1,0)",
                  status = "success",
                  solidHeader = TRUE,
                  plotOutput("volatilityForecastPlot2"),
                  width = 12
                )
              )
      ),
      # third tab content
      tabItem(tabName = "Data",
              h2("2005 - 2024 Exchange Rates (KES~USD) "),
              fluidRow(
                box(title = "KES ~ USD Exchange Rates",
                    status = "info",
                    solidHeader = TRUE,
                    width = 6,
                    dataTableOutput("exchangeRatesTable")),
                box(title = "Time Series 2005-2024",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    plotOutput("xtsplot")
                )
              )
      ),
      # forth tab content
      tabItem(tabName = "Team",
              fluidRow(
                box(
                  title = "THE TEAM (grp4)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  br(),
                  "Individually, each team member has brought a unique set of skills to the table. Their synergy, akin to a well-conducted symphony, has harmonized mathematical rigor, programming finesse, and financial insight. Through meticulous collaboration, they have created an application that resonates with excellence in both form and function.",
                  br(),
                  br(),
                  br(),
                  fluidRow(
                    box(title = "Elian Kimani",
                        status = "danger",
                        "Building this system has been a rollercoaster of challenges and triumphs. It's immensely gratifying to see our collective effort materialize into a tool that has the potential to influence financial decision-making.",
                        width = 6),
                    box(title = "Ronald Indata",
                        status = "warning",
                        "This project has been a captivating journey where I've honed my analytical skills. I'm proud of our team's ability to transform complex financial concepts into an accessible application",
                        width = 6),
                    box(title = "Deana Resiato",
                        status = "success",
                        "As an aspiring actuary, this project has deepened my understanding of time series analysis. The collaboration within our team has been inspiring, and I'm excited about the impact our work can have in the field of finance.",
                        width = 6),
                    box(title = "Finnly Odira",
                        status = "primary",
                        "This journey has been a masterclass in teamwork and problem-solving. From handling financial data to implementing forecasting models, I've gained invaluable experience that will undoubtedly shape my future as an actuary.",
                        width = 6)
                  )
                )
              )
      ),
      tabItem(tabName = "Extent",
              fluidRow(
                box( title = "Percentage Volatility for All African Countries",
                     width = 12,
                     status = "warning",
                     uiOutput("ui_open_tab_button"))
               
                
              ))
    )
  )
)


# Shiny server
server <- function(input, output) {
  output$ui_open_tab_button <- renderUI({
    shiny::a(
      h4(icon("th"),
         paste0("Africa's Volatility Stats ",input$slider),
         class = "btn btn-default action-button",
         style = "fontweight:600"),
      target = "_blank",
      href = paste0("https://rpubs.com/ENK/Afri_Vol_2024",input$slider)
    )
  })
  output$chartSeriesPlot <- renderPlotly({
    # Create a data frame for Plotly
    plot_data <- data.frame(Date = index(data), ExchangeRate = as.numeric(data))
    
    # Create the chartSeries plot using Plotly
    p <- plot_ly(plot_data, x = ~Date, y = ~ExchangeRate, type = "scatter", mode = "lines") %>%
      layout(title = "USD to KES Exchange Rate", yaxis = list(title = "Exchange Rate"), xaxis = list(title = "Date"))
    
    p
  })
  
  output$prophet_plot <- renderPlotly({
    print("Rendering plot...")
    
    p <- plot_ly() %>%
      add_trace(x = forecast$ds, y = forecast$yhat, type = 'scatter', mode = 'lines', name = 'Forecast') %>%
      add_trace(x = forecast$ds, y = forecast$yhat_lower, fill = 'tonexty', type = 'scatter', mode = 'lines', name = 'Lower Bound') %>%
      add_trace(x = forecast$ds, y = forecast$yhat_upper, fill = 'tonexty', type = 'scatter', mode = 'lines', name = 'Upper Bound') %>%
      layout(title = 'Model Performance',
             xaxis = list(title = 'Date'),
             yaxis = list(title = 'Value'))
    
    print("Plot created.")
    
    return(p)
  })
  
  output$chartSeriesPlotx <- renderPlot({
    # Create the chartSeries plot
    chartSeries(data, name = "USD to KES Exchange Rate", theme = chartTheme("white"))
  })
  output$xtsplot <- renderPlot({
    # Create the chartSeries plot
    chartSeries(data, name = "USD to KES Exchange Rate", theme = chartTheme("white"))
  })
  output$xtsplot2 <- renderPlot({
    # Create the chartSeries plot
    chartSeries(data, name = "USD to KES Exchange Rate", theme = chartTheme("black"))
  })
  output$exchangeRatesTable <- renderDataTable({
    # Assuming 'KES_data' is an xts object
    colnames <- colnames(KES_data)
    data <- data.frame(Date = format(index(KES_data), "%Y-%m-%d"), ExchangeRate = KES_data[, colnames[1]])
    
    # Create DataTable with pagination
    datatable(data, options = list(pageLength = 20))
  }, col.names = c("Date", "ExchangeRate"))
  output$arimaPlot <- renderPlotly({
    # Create the ARIMA forecast plot using plotly
    p <- plot_ly()
    
    # Add ARIMA forecast
    p <- add_lines(p, x = forecast_dates, y = forecast_values, name = "ARIMA Forecast (2,1,0)", type = "scatter", mode = "lines", line = list(color = 'red'))
    
    
    # Customize the layout
    p <- layout(p, title = "ARIMA Forecast for the Next 30 Days", yaxis = list(title = "Price"), xaxis = list(title = "Date"))
    
    p
  })
  
  output$arimaPlot2 <- renderPlotly({
    # Create the ARIMA forecast plot using plotly
    p <- plot_ly()
    
    # Add ARIMA forecast
    p <- add_lines(p, x = forecast_dates, y = forecast_values, name = "ARIMA Forecast (2,1,0)", type = "scatter", mode = "lines", line = list(color = 'red'))
    
    
    # Customize the layout
    p <- layout(p, title = "ARIMA Forecast for the Next 30 Days", yaxis = list(title = "Price"), xaxis = list(title = "Date"))
    
    p
  })
  output$decompositionPlot <- renderPlot({
    # Decompose the time series and plot
    decomposed_data <- decompose(rate.ts)
    
    # Plot the decomposed time series
    plot(decomposed_data)
  })
  
  output$decompositionPlot2 <- renderPlot({
    # Decompose the time series and plot
    decomposed_data2 <- decompose(rate.ts)
    
    # Plot the decomposed time series
    plot(decomposed_data2)
  })
  
  output$volatilityForecastPlot <- renderPlot({
    # Assuming volatility_forecast is your xts/zoo object
    # Convert index to a vector of dates
    volatility_dates <- as.Date(index(volatility_forecast))
    
    # Create a data frame with Date and Value columns
    volatility_data <- data.frame(Date = volatility_dates, Value = as.numeric(volatility_forecast))
    
    # Create the plot using ggplot2
    ggplot(volatility_data, aes(x = Date, y = Value)) +
      geom_line() +
      ggtitle("Volatility Forecast For The Next 365 Days.")
  })
  
  output$volatilityForecastPlot2 <- renderPlot({
    # Assuming volatility_forecast is your xts/zoo object
    # Convert index to a vector of dates
    volatility_dates <- as.Date(index(volatility_forecast))
    
    # Create a data frame with Date and Value columns
    volatility_data <- data.frame(Date = volatility_dates, Value = as.numeric(volatility_forecast))
    
    # Create the plot using ggplot2
    ggplot(volatility_data, aes(x = Date, y = Value)) +
      geom_line() +
      ggtitle("Volatility Forecast For The Next 365 Days.")
  })
  
  
  
  
  output$residualsPlot <- renderPlot({
    # Fit ARIMA model
    arima_model <- arima(diff_KES_data, order = c(2, 1, 0))
    
    # Extract residuals
    residuals <- residuals(arima_model)
    
    # Create a time series for residuals
    residuals_ts <- ts(residuals, start = c(2005, as.numeric(format(inds[2], "%j"))), frequency = 365)
    
    # Plot residuals
    plot(residuals_ts, main = "ARIMA(2, 1, 0) Residuals", ylab = "Residuals")
  })
  
  
}
# Run the Shiny app
# Run the Shiny app
shinyApp(ui = ui, server = server)

