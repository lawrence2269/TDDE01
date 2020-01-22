options(scipen=999) #To avoid scientific notations
RNGversion('3.5.1')

set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv",stringsAsFactors=FALSE, fileEncoding="latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
st$date = as.character(st$date)
st$time = as.character(st$time)

times <- c("04:00:00", "06:00:00","08:00:00","10:00:00","12:00:00","14:00:00", "16:00:00","18:00:00","20:00:00","22:00:00", "00:00:00")
h_distance <- 50000 # These three values are up to the students
h_date <- 7 #days
h_time <- 3 #hours
a <- 58.4108  # The point to predict (up to the students)
b <- 15.6214
latlong = data.frame(latitude = a,longitude = b)
pred_date <- "2019-12-19"
no_of_times = length(times)
latitude = rep(a,no_of_times)
longitude = rep(b,no_of_times)
dates = rep(pred_date,no_of_times)
data_Predict = data.frame(latitude=latitude,longitude=longitude,date=dates,time = times)

colSums(is.na(st))

st = st[as.Date(st$date)<as.Date(pred_date),]

gaussian_Kernel = function(u,h){
  return(exp(-abs(u/h)^2))
}

diff_distance_kernel = function(poi,stations)
{
  distance = distHaversine(poi,stations)
  return(gaussian_Kernel(distance,h_distance))
}

diff_date_kernel = function(doi,day)
{
  day_distance = abs(as.numeric(difftime(strptime(doi,format = "%Y-%m-%d"),strptime(day,format = "%Y-%m-%d"),units = "days")))
  day_distance = day_distance%%365
  day_distance[day_distance>182] = 365-day_distance[day_distance>182]
  return(gaussian_Kernel(day_distance,h_date))
}

diff_hour_kernel = function(hoi,hours)
{
  hour_difference = as.numeric(difftime(as.POSIXct(strptime(hoi, "%H:%M:%S")),
                                        as.POSIXct(strptime(hours, "%H:%M:%S")),
                                        units = "hour"))
  hour_difference = abs(hour_difference)
  hour_difference[hour_difference>12] = 24-hour_difference[hour_difference>12]
  return(gaussian_Kernel(hour_difference,h_time))
}

kernel_calculation_addition = function(toi)
{
  poi_distance = diff_distance_kernel(poi = latlong,stations = st[,4:5])
  doi_distance = diff_date_kernel(pred_date,st$date)
  toi_distance = diff_hour_kernel(toi,st$time)
  
  kernel_Sum = poi_distance + doi_distance + toi_distance
  
  kernel_Sum = (sum(kernel_Sum * st$air_temperature) / sum(kernel_Sum))
 
  return(kernel_Sum)
}

kernel_calculation_multiplication = function(toi)
{
  poi_distance = diff_distance_kernel(poi = latlong,stations = st[,4:5])
  doi_distance = diff_date_kernel(pred_date,st$date)
  toi_distance = diff_hour_kernel(toi,st$time)
  
  kernel_Multiplication = poi_distance * doi_distance * toi_distance
  
  kernel_Multiplication = (sum(kernel_Multiplication * st$air_temperature) / sum(kernel_Multiplication))

  return(kernel_Multiplication)
}

temp_1 = rep(0,length(times))
temp_2 = rep(0,length(times))
i = 0
for(i in 1:length(times)){
  temp_1[i] = kernel_calculation_addition(times[i])
  temp_2[i] = kernel_calculation_multiplication(times[i])
}



plot(temp_1, type="o", xaxt="n", xlab="Time", ylab="Temperature",main="Temperature measured using Kernel Addition")
axis(1, at=1:length(temp_1), labels=times)

plot(temp_2, type="o", xaxt="n", xlab="Time", ylab="Temperature",main="Kernel Multiplication")
axis(1, at=1:length(temp_1), labels=times)

plot(distHaversine(c(a,b),st[4:5]),gaussian_Kernel(distHaversine(c(a,b),st[4:5]),h_distance),xlab = "Distance (in metres)",
     ylab = "Weight",main = "Weights for geographical distance (Linkoping)",col="blue")
plot(gaussian_Kernel(matrix(seq(1,183,1)),h_date),xlim=c(0,185),xlab="Days",
     ylab="Weight",main = "Weights for day distance",col="blue")
plot(gaussian_Kernel(matrix(seq(0,24,2)),h_time),main="Weights for time distance",
     xlab = "Time",ylab="Weight",col="blue")


