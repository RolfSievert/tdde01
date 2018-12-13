
set.seed(1234567890)
library("geosphere")

setwd("~/courses/tdde01/lab3/")

stations_full = read.csv("stations.csv", fileEncoding = "Latin1")
temps = read.csv("temps50k.csv")
st = merge(stations, temps, by="station_number")
stations = stations_full[,]

gauss_kernel = function(u) {
    return (exp(-(abs(u)^2)))
}

h_distance = 200000
h_date = 20
h_time = 4

# Station is station number, interest is vector(long, lat)
kernel_distance = function(station, interest) {
    longitude = stations[stations$station_number == station, 'longitude']
    latitude = stations[stations$station_number == station, 'latitude']
    distance = distHaversine(c(longitude, latitude), interest)
    res = gauss_kernel(distance/h_distance)
    return (res)
}

# Calculates difference in days from day to interest
kernel_date = function(day, interest) {
    day = as.Date(day)
    interest = as.Date(interest)
    distance = as.numeric(difftime(day, interest, units=c("days")))
    res = gauss_kernel(distance/h_date)
    return (res)
}

kernel_time = function(hour, interest) {
    h1 = strptime(hour, format = "%H:%M:%S")
    h2 = strptime(interest, format = "%H:%M:%S")
    distance = as.numeric(abs(h2-h1))
    return (gauss_kernel(distance/h_time))
}

kernel_sum = function(long, lat, date, time) {
    sum1 = 0 # Sum multiplied by temperature value
    sum2 = 0 # Just sum of weights
    
    for (station in stations$station_number) {
        # Calculate station distance
        rows = temps[temps$station_number == station,]
        if (station %in% rows$station_number) {
            dist = kernel_distance(station, c(long, lat))
            for (row in 1:nrow(rows)) {
                # Calculate date difference
                station.data = rows[row,]
                date.diff = kernel_date(station.data$date, date)
                
                # Calculate time difference
                time.diff = kernel_time(station.data$time, time)
                # Get temperature
                temp = station.data$air_temperature
                weight = (dist + date.diff + time.diff)
                sum1 = sum1 + weight*temp
                sum2 = sum2 + weight
            }
        }
    }
    return (sum1/sum2)
}

a = 58.4274 # The point to predict
b = 14.826
date = "2013-11-04"
#date = "2013-11-04" # The date to predict
times = c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", 
          "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", 
          "24:00:00")

temp = vector(length=length(times))

# Fill in code
for (i in 1:length(times)) {
    time = times[i]
    temp[i] = kernel_sum(a, b, date, time)
}

plot(temp, x=seq(4, 24, 2), type="o")
