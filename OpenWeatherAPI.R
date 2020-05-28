
#Pulls a 5 day forecast from the OpenWeather API's JSON endpoint and visualises the trend.

library(tidyverse)
library(jsonlite)

#Cities to collect OpenWeather data for
cities <- c("London, UK", "Birmingham, UK", "Oxford, UK", "Sheffield, UK", "Glasgow, UK")

#Access the API find the lat/long pair. Then use this to return 5 days of trend data for the selected cities
weather_select <- function(city_in)
{
  
  #Use the current forecast API to get the lat and long.
  url_json <- paste("https://api.openweathermap.org/data/2.5/weather?q=",city_in,"&appid=",Sys.getenv("OPEN_WEATHER_API_KEY"), sep = "")
  raw_json <- fromJSON(url_json)
  
  lat <- pluck(raw_json[["coord"]][["lat"]])
  lon <- pluck(raw_json[["coord"]][["lon"]])
  
  full_result <- tibble()
  
  #Historic forecast requires each day to be accessed seperately.
  for(i in 1:5) {
    requested_date <- as.numeric(as.POSIXct(Sys.Date()-i, format="%Y-%m-%d"))
    
    #Create the URL query string for each day of the 5 day forecast and refresh the raw JSON for that day
    url_json <- paste("https://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat,"&lon=",lon,"&dt=",requested_date,"&appid=",Sys.getenv("OPEN_WEATHER_API_KEY"), sep = "")
    raw_json <- fromJSON(url_json)
    
    #Pull out the hourly historic temp readings for the day. 
    daily_result <- pluck(raw_json[["hourly"]])
    daily_result$dt <- as.POSIXct(daily_result$dt, origin = "1970-01-01 00:00:00")
    #Strip out the wind_gust information which is only provided for last 24 hours it seems?
    daily_result <- daily_result %>%
      select(1:11)
    daily_result$city <- city_in
    daily_result$forecast <- FALSE
    
    if(i == 1){
      full_result <- daily_result 
    } else {
      full_result <- rbind(full_result, daily_result)
    }
  }
  
  #Get the current and forecast values. API isn't consistent here, so we'll focus on temperature forecast.
  url_new <- paste("https://api.openweathermap.org/data/2.5/onecall?lat=",lat,"&lon=",lon,"&exclude=minutely,hourly&appid=", Sys.getenv("OPEN_WEATHER_API_KEY"), sep = "")
  forecast_JSON <- fromJSON(url_new)
  
  #Pull the current weather metrics
  current_weather_df <- as_tibble(pluck(forecast_JSON[["current"]]))
  current_weather_df$dt <- as.POSIXct(current_weather_df$dt, origin = "1970-01-01 00:00:00")
  
  #Pull the forecast temp
  forecast_weather_df <- (pluck(forecast_JSON[["daily"]]))
  df <- flatten_df(forecast_weather_df["temp"])
  
  forecast_weather_df <- forecast_weather_df %>%
    select("dt")
  
  forecast_weather_df <- cbind(forecast_weather_df, df)
  forecast_weather_df$temp <- forecast_weather_df$day 
  forecast_weather_df <- forecast_weather_df %>% select(1, 8)
  forecast_weather_df$dt <- as.POSIXct(forecast_weather_df$dt, origin = "1970-01-01 00:00:00")
  
  #Join with current temp data
  forecast_df <- full_join(current_weather_df, forecast_weather_df)
  
  #Trim to just the first few columns 
  forecast_df <- forecast_df %>% select(1:4)
  
  #Add the city name that it was called for
  forecast_df$city <- city_in
  forecast_df$forecast <- TRUE
  
  #Join with the existing full_result data from the historic records. 
  
  full_result <- full_join(full_result, forecast_df)
  
  return(full_result)
}  

#Loop through all the cities in the input list and use the weather_select function to get the data we need
for(i in 1:length(cities)) {
  if(i ==1) {
    weather_df <- weather_select(cities[i]) 
  } else {
    weather_df <- rbind(weather_df, weather_select(cities[i]))
  }
}

#Quick chart to visualise (temp is given in K, so needs conversion by deducting -273.15)

weather_df %>%
  ggplot(aes(x = dt, y = (temp-273.15), color = city, linetype = forecast)) + 
  geom_smooth(se = FALSE, span = 0.7) + 
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_linetype(guide = FALSE) +
  labs(title = "Temperature", subtitle = paste("Five day trend, averaging",round(mean(weather_df$temp-273.15),1),"°C nationally.", sep = " "), caption = paste("Data from OpenWeather API at ",Sys.time(),".", sep = "")) + 
  xlab("Date") + 
  ylab("Temperature in °C")


