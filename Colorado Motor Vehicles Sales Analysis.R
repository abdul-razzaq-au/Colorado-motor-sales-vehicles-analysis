
#install required library and packages

library(tidyverse)
library(lubridate)
library(scales)
library(dplyr)
library(forecast)
library(zoo)

#Load the data
df <- read.csv("C:/Users/abdul/Downloads/colorado_motor_vehicle_sales.csv")

# Preview

head(df)
str(df)
summary(df)
colSums(is.na(df))
length(unique(df$county))
df %>% count(county)


#Exploratory Data Analysis

#Yearly Sales Trend
yearly_sales <- df %>%
  group_by(year) %>%
  summarise(total_sales = sum(sales))

ggplot(yearly_sales, aes(year, total_sales)) +
  geom_line() + geom_point() +
  labs(title="Total Vehicle Sales by Year",
       x="Year", y="Sales (USD)") +
  scale_y_continuous(labels = comma)

#Quatarly Trend

quarterly_sales <- df %>%
  group_by(year, quarter) %>%
  summarise(total_sales = sum(sales))

ggplot(quarterly_sales,
       aes(year, total_sales, color=factor(quarter))) +
  geom_line() + geom_point() +
  labs(title="Quarterly Sales Trend",
       color="Quarter", x="Year", y="Sales (USD)") +
  scale_y_continuous(labels = comma)

#Top 10 counties

top_counties <- df %>%
  group_by(county) %>%
  summarise(total_sales = sum(sales)) %>%
  arrange(desc(total_sales)) %>%
  slice(1:10)

ggplot(top_counties, aes(reorder(county, total_sales), total_sales)) +
  geom_col() +
  coord_flip() +
  labs(title="Top 10 Counties by Total Sales",
       x="County", y="Total Sales (USD)") +
  scale_y_continuous(labels = comma)


#Quarter heatmap of total sales by counties

df %>%
  group_by(year, quarter) %>%
  summarise(total_sales = sum(sales)) %>%
  ggplot(aes(factor(quarter), factor(year), fill=total_sales)) +
  geom_tile() +
  scale_fill_continuous(labels = comma) +
  labs(title="Sales Heatmap by Year & Quarter",
       x="Quarter", y="Year", fill="Sales")

#Trend Smoothing

yearly_sales$moving_avg <- rollmean(yearly_sales$total_sales, 2, fill = NA)

ggplot(yearly_sales, aes(x = year)) +
  geom_line(aes(y = total_sales), color = "grey60") +
  geom_line(aes(y = moving_avg), color = "blue", size = 1.2) +
  labs(title = "Smoothed Vehicle Sales Trend (2-Year Moving Average)",
       y = "Sales (USD)")

#County Share Over Time

df %>%
  group_by(year, county) %>%
  summarise(total_sales = sum(sales)) %>%
  group_by(year) %>%
  mutate(share = total_sales / sum(total_sales)) %>%
  ggplot(aes(x = year, y = share, fill = county)) +
  geom_area() +
  labs(title = "County Share of Total Colorado Sales Over Time",
       y = "Share of Statewide Sales", x = "Year")


#Year over Year growth

yoy <- yearly_sales %>%
  mutate(yoy_growth = (total_sales - lag(total_sales))/lag(total_sales))

yoy

#Overall year over year growth in sales

yearly_sales <- df %>%
  group_by(year) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  arrange(year) %>%
  mutate(
    yoy_growth = (total_sales - lag(total_sales)) / lag(total_sales) * 100
  )

ggplot(yearly_sales, aes(x = year, y = yoy_growth)) +
  geom_line(color = "#0073C2FF", size = 1.2) +
  geom_point(size = 3, color = "#0073C2FF") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Overall Year-over-Year (YoY) Sales Growth",
       x = "Year",
       y = "YoY Growth (%)") +
  theme_minimal(base_size = 14)

#County-wise YoY growth in sales

#county-wise yoy growth
county_yoy <- df %>%
  group_by(county, year) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop_last") %>%
  arrange(county, year) %>%
  mutate(yoy_growth = (total_sales - lag(total_sales)) / lag(total_sales) * 100)

#plot

ggplot(county_yoy, aes(x = year, y = yoy_growth, color = county)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "County-wise Year-over-Year (YoY) Sales Growth",
       x = "Year",
       y = "YoY Growth (%)",
       color = "County") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")



#Top- vs Low-Growth Counties

growth <- df %>%
  group_by(county, year) %>%
  summarise(total_sales = sum(sales)) %>%
  group_by(county) %>%
  mutate(yoy_growth = (total_sales - lag(total_sales)) / lag(total_sales))

growth_summary <- growth %>%
  summarise(avg_growth = mean(yoy_growth, na.rm = TRUE)) %>%
  arrange(desc(avg_growth))
head(growth_summary, 5)   # Top 5 counties
tail(growth_summary, 5)   # Slowest 5 counties


#FORECAST

# Aggregate to total sales per quarter
ts_data <- df %>%
  arrange(year, quarter) %>%
  group_by(year, quarter) %>%
  summarise(total_sales = sum(sales))

# Convert to time series (quarterly)
sales_ts <- ts(ts_data$total_sales, start = c(min(ts_data$year), min(ts_data$quarter)), frequency = 4)

# Plot the time series
autoplot(sales_ts) +
  labs(title = "Quarterly Vehicle Sales in Colorado (2008–2015)",
       y = "Sales (USD)", x = "Year")

#train test

train <- window(sales_ts, end = c(2014, 4))
test  <- window(sales_ts, start = c(2015, 1))

#fit arima

fit_arima <- auto.arima(train)
summary(fit_arima)

# Forecast next 4 quarters
forecast_arima <- forecast(fit_arima, h = 4)

# Plot forecast vs actual
autoplot(forecast_arima) +
  autolayer(test, series="Actual", color="red") +
  labs(title="ARIMA Forecast vs Actual (2015)",
       y="Sales (USD)", x="Year")

accuracy(forecast_arima, test)

