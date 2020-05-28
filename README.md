# Open-Weather-API-calls

A proof of concept exercise to access the OpenWeather API directly from R to download and process JSON. 

Based on city names supplied, the script gets lat/lon for each observation from one API and then uses this information to access the Historic and Forecast APIs. These have different behaviours and are differently structured (hourly for historic, daily for forecast beyond two days). 

A simple ggplot is included to visualise the data. There's an annoying gap the free tier data between historic and current data that can be up to 24 hours depending on the distance between your timezone and the server.

You'll need to replace the ##Sys.getenv("OPEN_WEATHER_API_KEY")## part of the API calls with your own key which is freely available with registration. 

