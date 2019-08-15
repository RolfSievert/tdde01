library("geosphere")

setwd("~/courses/tdde01/lab3/")

stations = read.csv("stations.csv", fileEncoding = "Latin1")
temps = read.csv("temps50k.csv")
st = merge(stations, temps, by="station_number")

gauss_kernel = function(u) {
    return (exp(-(u^2)))
}

h_distance = 10000
h_date = 8
h_time = 4

# Station is station number, interest is vector(long, lat)
kernel_distance = function(station, interest) {
    distance = distHaversine(station, interest)
    res = gauss_kernel(distance/h_distance)
    return (res)
}

# Calculates gaussian kernel provided with difference in days from day to interest
kernel_date = function(dates, interest) {
    dates = as.Date(dates)
    interest = as.Date(interest)
    distance = as.numeric(difftime(dates, interest, units=c("days")))
    leap_years = floor(floor(distance/365)/4)
    distance = (distance - leap_years) %% 365
    distance = sapply(distance, function(d) min(d, abs(365 - d)))
    res = gauss_kernel(distance/h_date)
    return (res)
}

# Calculates gaussian kernel provided with difference of hour and target hour
kernel_time = function(hour, interest) {
    h1 = as.numeric(substr(hour, 1, 2))
    h2 = as.numeric(substr(interest, 1, 2))
    distance = abs(((h2-h1) + 12) %% 24 - 12)
    return (gauss_kernel(distance/h_time))
}

# Plot kernels
width = 2.5
range = -(h_distance*width):(h_distance*width)
plot(x=range, y=gauss_kernel(range/h_distance), type="l", ylab = "Distance kernel", xlab="Range")

range = -(h_date*width):(h_date*width)
plot(x=range, y=gauss_kernel(range/h_date), type="l", ylab = "Date kernel", xlab="Range")

range = -(h_time*width):(h_time*width)
plot(x=range, y=gauss_kernel(range/h_time), type="l", ylab = "Time kernel", xlab="Range")

# Summation of kernels. Passing product returns kernels multiplied
kernel_sum = function(long, lat, date, time, product=FALSE) {
    longlat = data.frame(st$longitude, st$latitude)
    distances = kernel_distance(longlat, c(long, lat))
    
    datediffs = kernel_date(st$date, date)
    
    timediffs = kernel_time(st$time, time)

    sum = sum((distances + datediffs + timediffs) * st$air_temperature)
    sum = sum / sum(distances + datediffs + timediffs)
    
    prod = sum((distances * datediffs * timediffs) * st$air_temperature)
    prod = prod / sum(distances * datediffs * timediffs)
    
    if (product) {
        return(prod)
    }
    else {
        return(sum)
    }
}

latitude = 58.4274 # The point to predict
longitude = 14.826
date = "2018-12-24" # The date to predict
times = c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", 
          "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", 
          "00:00:00")
# Remove posterior dates
st = st[as.Date(st$date) < as.Date(date),]

temp = lapply(times, function(t) kernel_sum(longitude, latitude, date, t, FALSE))

plot(temp, x=seq(4, 24, 2), type="o", ylab="Sum of kernels", xlab="Hour")

temp = lapply(times, function(t) kernel_sum(longitude, latitude, date, t, TRUE))

plot(temp, x=seq(4, 24, 2), type="o", ylab="Product of kernels", xlab="Hour")