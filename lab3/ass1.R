library("geosphere")

setwd("~/courses/tdde01/lab3/")

stations_full = read.csv("stations.csv", fileEncoding = "Latin1")
temps = read.csv("temps50k.csv")
st = merge(stations, temps, by="station_number")
n = nrow(stations_full)
set.seed(1234567890)
s = sample(1:n, floor(n*1))
stations = stations_full[s,]

gauss_kernel = function(u) {
    return (exp(-(u^2)))
}

h_distance = 20000
h_date = 50
h_time = 6

# Station is station number, interest is vector(long, lat)
kernel_distance = function(station, interest) {
    distance = distHaversine(station, interest)
    res = gauss_kernel(distance/h_distance)
    return (res)
}

# Calculates difference in days from day to interest
kernel_date = function(dates, interest) {
    dates = as.Date(dates)
    interest = as.Date(interest)
    distance = as.numeric(difftime(dates, interest, units=c("days")))
    res = gauss_kernel((distance %% 365)/h_date)
    return (res)
}

kernel_time = function(hour, interest) {
    h1 = as.numeric(substr(hour, 1, 2))
    h2 = as.numeric(substr(interest, 1, 2))
    distance = abs(((h2-h1) + 12) %% 24 - 12)
    return (gauss_kernel(distance/h_time))
}

kernel_sum = function(long, lat, date, time, product=FALSE) {
    longlat = data.frame(st$longitude, st$latitude)
    distances = kernel_distance(longlat, c(long, lat))
    
    datediffs = kernel_date(st$date, date)
    
    timediffs = kernel_time(st$time, time)

    sum = sum((distances + datediffs + timediffs) * st$air_temperature)
    sum = sum / sum(distances + datediffs + timediffs)
    
    prod = sum((distances * datediffs * timediffs) * st$air_temperature)
    prod = prod / sum(distances * datediffs * timediffs)
    
    print("END")
    if (product) {
        return(prod)
    }
    else {
        return(sum)
    }
}


latitude = 58.4274 # The point to predict
longitude = 14.826
date = "2000-01-04"
#date = "2013-11-04" # The date to predict
times = c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", 
          "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", 
          "00:00:00")

temp = vector(length=length(times))

# Fill in code
for (i in 1:length(times)) {
    time = times[i]
    temp[i] = kernel_sum(longitude, latitude, date, time, TRUE)
}

plot(temp, x=seq(4, 24, 2), type="o")
