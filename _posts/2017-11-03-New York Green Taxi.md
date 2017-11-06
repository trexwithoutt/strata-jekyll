---
title: "2017-11-03-New York Green Taxi"
author: "REX(RUIZHE) ZHOU"
categories: project
date: "11/3/2017"
---

## Required Packages
```r
library(RCurl)
library(ggplot2)
library(tidyverse)
library(scales)
library(dplyr)
library(shiny)
library(leaflet)
library(shinydashboard)
library(MASS)
library(lmtest)
```

## Load Data

```r
x = getURL('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv')
y = read.csv(text = x)
y = y[y$Fare_amount > 0, ]
## Dimenison of the data
rows = dim(y)[1]
cols = dim(y)[2]
```

- Based on the overview above, we can easily see that there are `r rows` observation and `r cols` variables

## Visual On Vriable `Trip Distance`

```r
ggplot(data = y, aes(y$Trip_distance)) +
  geom_histogram(
    breaks = seq(0, 600, by = 2),
    col = "black",
    fill = "cornflowerblue",
    alpha = .2
    ) + 
  labs(title = "Histogram for Trip Distance Overview (with extreme values)") +
  labs(x = "Trip Distance", y = "Count")

## Overview of outlier bin
out_num = sum(y$Trip_distance > 10)
extreme = max(y$Trip_distance)
```

- The range of the trip distance is widely spreaded. When we look in details, the maximum of this variable reaches `r extreme`, and there is a high expression between 0 to 10

- To take a closer look of the trip distance data, we need to concatenate extreme values (which, here, I chose values above 10) and have a direct visual on major distributed data

```r
## Histogram For the Trip Distance Variable
y %>%
  mutate(Trip_Distance = ifelse(y$Trip_distance > 10, 11, y$Trip_distance)) %>%
  ggplot(aes(Trip_Distance)) +
  geom_histogram(binwidth = .1,
  col = "black",
  fill = c(rep("cornflowerblue", 110), "darkgreen")) +
  labs(title="Histogram for Trip Distance Neat View (without extreme distance spread") +
  labs(x="Trip Distance", y="Count")
  
```

- According to the second histogram above, the `Trip_distance` variable contain values that highly skewed to right, and it can easily observe that the median is smaller than the mean. 

- Based on the major distribution of the `Trip_distance` data, it seems data is approximately under **lognormal distribution**

- I generaly muted values greater than 10 as 11, and painted in green, and, then, we can take a look on extreme values. (The number of values that are greater than 10 is `r out_num`, which is a notable number to be observed)

- The observation on `Trip_distance` data leads me to a **hypothesis**: Since the data doesn't form in a normal distribution, we may consider that people in new york doesn't take a trip is not random, and, there might be a cause result in trip distance mainly locates between range 0 to 10. (eg. trip time: since people usually work daily)


## `Trip_Distance` vs. Time

To take a look at relation between trip distance and time, we can first group the distance value by time

```r
## Create columns of pick and dropoff hour, value contain factors with 24 levels
y$hour_pickup = as.factor(substr(y$lpep_pickup_datetime, 12, 13))
y$hour_dropoff = as.factor(substr(y$Lpep_dropoff_datetime, 12, 13))

## Mean Trip Distance Group By Hours
mean_pickup = aggregate(Trip_distance ~ hour_pickup, y, mean)
mean_pickup$source = as.factor(3)
mean_pickup$hour_pickup = as.numeric(mean_pickup$hour_pickup)
colnames(mean_pickup)[1] = "hour"

mean_dropoff = aggregate(Trip_distance ~ hour_dropoff, y, mean)
mean_dropoff$source = as.factor(4)
mean_dropoff$hour_dropoff = as.numeric(mean_dropoff$hour_dropoff)
colnames(mean_dropoff)[1] = "hour"

## Median Trip Distance Group By Hours
median_pickup = aggregate(Trip_distance ~ hour_pickup, y, median)
median_pickup$source = as.factor(1)
median_pickup$hour_pickup = as.numeric(median_pickup$hour_pickup)
colnames(median_pickup)[1] = "hour"

median_dropoff = aggregate(Trip_distance ~ hour_dropoff, y, median)
median_dropoff$source = as.factor(2)
median_dropoff$hour_dropoff = as.numeric(median_dropoff$hour_dropoff)
colnames(median_dropoff)[1] = "hour"

df = bind_rows(mean_pickup, median_pickup, mean_dropoff, median_dropoff)
ggplot(df, aes(hour, Trip_distance, colour=source)) +
  geom_line() + 
  labs(title="The Mean of Trip Distance vs. Hours") +
  labs(x="Hours", y="Trip Distance") + 
  theme_bw()
```

- According to the plot above, it seems the peak of the mean of trip distance usually happens during the morning, like 6 to 9, and there is a pop up after 8PM. If the cause of taking taxi is related to going to work, I may assume that people ususally take taxi to avoid being late and don't really want to spend to much on taxi after work. The pop back after 8PM might infers that people want to get back home quickly after their night life, and the high volume of ending night life usually after 10 or 11PM.


## Trip vs. Airports

There are three airports locate at new york city area, JFK, LGA, and Newark. According to the
[data dictionary] (http://www.nyc.gov/html/tlc/downloads/pdf/data_dictionary_trip_records_green.pdf), we can determine if a trip was going to JFK and Newark by idenify the RateCodeID, where `2` is JFK and `3` is Newark. To find out trips that were going to LGA, I decided to figure out the location of the destination, since the dropoff longitude and latitude are given. We can easily find the boundary of longitude and latitude of LGA by going to this [website](http://www.get-direction.com/address-to-lat-long.html?place=laguardia%20airport%20entrance%2C%20east%20elmhurst%2C%20ny%2C%20united%20states)

```r
### JFK
jfk = y[y$RateCodeID == 2,]
mean_total_jfk = mean(jfk$Total_amount)
mean_fare_jfk = mean(jfk$Fare_amount)

### Newark
newark = y[y$RateCodeID == 3,]
mean_total_newark = mean(newark$Total_amount)
mean_fare_newark = mean(newark$Fare_amount)

### LGA
lga = y[y$Dropoff_longitude > -73.88 &
          y$Dropoff_longitude < -73.86  &
          y$Dropoff_latitude < 40.788 &
          y$Dropoff_latitude > 40.766 & y$RateCodeID != 2, ]
mean_total_lga = mean(lga$Total_amount)
mean_fare_lga = mean(lga$Fare_amount)

### Create a new dataframe of trips with destination as airports
airport = rbind(jfk, newark, lga)

count_airport = dim(airport)[1]
mean_total_airport = mean(airport$Total_amount)
mean_fare_airport = mean(airport$Fare_amount)
```

- The number of the trips went to airports: `r count_airport`

- Mean fare for the airport trips is $`r mean_fare_airport`

- Mean total for the airport trips is $`r mean_total_airport`

**Overview of mean cost for each airport**

|        | Mean Fare            | Mean Total            |
|--------|----------------------|-----------------------|
| JFK    | `r mean_fare_jfk`    | `r mean_total_jfk`    |
| Newark | `r mean_fare_newark` | `r mean_total_newark` |
| LGA    | `r mean_fare_lga`    | `r mean_total_lga`    |


To look more details about the airport trip, I decided to find out the trip relation with trip distance and time.

*(Airport Trips with Time)*
```r
jfk_count = as.data.frame(table(jfk$hour_dropoff))
newark_count = as.data.frame(table(newark$hour_dropoff))
lga_count = as.data.frame(table(lga$hour_dropoff))
jfk_count$airport = as.factor('jfk')
newark_count$airport = as.factor('newark')
lga_count$airport = as.factor('lga')

df_airport = bind_rows(jfk_count, newark_count,lga_count)
df_airport$Var1 = as.numeric(df_airport$Var1)
ggplot(df_airport, aes(Var1, Freq, colour=airport)) +
  geom_line() +
  labs(title="Number of Trips vs. Hours") +
  labs(x="Hours", y="Number of Trips")+
  theme_bw()
```

- Based on the graph above, the volume of going to lga is much larger than that of jfk and newark. But we still can get an intuitive notice that people usually going to airports from the morning 3AM to the night 23PM, and all three have a peak around 4-5PM. We, thus, can derive some information such that flights operate more during the evening, and usually start to operate the earliest one at aroung 4-6AM.

*(Airport Trips with Time)*

```r
mean_dropoff_jfk = aggregate(Trip_distance ~ hour_dropoff, jfk, mean)
mean_dropoff_newark = aggregate(Trip_distance ~ hour_dropoff, newark, mean)
mean_dropoff_lga = aggregate(Trip_distance ~ hour_dropoff, lga, mean)

mean_dropoff_jfk$airport = 'jfk'
mean_dropoff_newark$airport = 'newark'
mean_dropoff_lga$airport = 'lga'
df_air_distance = bind_rows(mean_dropoff_jfk, mean_dropoff_newark, mean_dropoff_lga)
df_air_distance$airport = as.factor(df_air_distance$airport)
df_air_distance$hour_dropoff = as.numeric(df_air_distance$hour_dropoff)

ggplot(df_air_distance, aes(hour_dropoff, Trip_distance, colour=airport)) +
  geom_line() +
  labs(title="Mean Trips Distance vs. Hours") +
  labs(x="Hours", y="Mean Trip Distance")+
  theme_bw()
```

- Based on the graph above, the travling mean traveling distances for JFK and Newark are much more longer than the mean distance for LGA. This can be easily explain, since that LGA is locate in the major area of newyork, where nearby the manhattan and queens, two districts that contain most of population in new york, and JFK (locate at Brooklyn), Newark (locate at Jersey) much further from main districts of new york than LGA does.


## Tip Analysis

First we create a derived variable tip ratio.

```r
y$Tip_percentage = paste(round((y$Tip_amount / y$Total_amount) * 100, digits =
                            2), "%", sep = '')
y$Tip_ratio = y$Tip_amount / y$Total_amount
```

```r
## Create a new dataframe that contain no factor variables. Thus we can try if there is a regression predictive model

nofactor = data.frame(RateCodeID = y$RateCodeID, Passenger_count = y$Passenger_count, Trip_distance = y$Trip_distance, Fare_amount =y$Fare_amount, Tip_amount = y$Tip_amount, Total_amoungt = y$Total_amount, Tip_ratio = y$Tip_ratio)
nofactor = nofactor[nofactor$Trip_distance > 11, ]
# generate a regression model with all features, and do statistical test to check model assumptions
all = lm(Tip_amount ~., data = nofactor)

## BIC: a method used to select variables
n = length(resid(all))
BIC <- step(all, direction = "backward", trace = 0, k = log(n))
summary(BIC)
par(mfrow=c(2,2))
plot(BIC, pch=20, cex=0.3, col='dodgerblue')

lmtest::bptest(BIC) #p-val:  2.2e-16
set.seed(110116) 
resid5000 = sample(resid(BIC), 5000)
shapiro.test(resid5000) #p-value: 2.2e-16

hat = hatvalues(BIC); hat_bar = mean(hat); i = which(hat > 2 * hat_bar)

n = which(abs(rstandard(BIC)) > 1)
index = which(cooks.distance(BIC) > 4 / length(cooks.distance(BIC)))

y1 = nofactor[-i,]
y2 = y1[-index,]
y3 = y2[-n,]
y3$Tip_amount = y3$Tip_amount + 0.01
all2 = lm(Tip_amount ~., data = y3)
par(mfrow=c(2,2))
plot(all2, pch=20, cex=0.3, col='dodgerblue')
trans = lm(log(Tip_amount) ~., data = y3)
plot(trans, pch=20, cex=0.3, col='dodgerblue')
```
Even though all non-factor variables are significant in regression model, it seems like it still handle a good result after cleaning and transformation.
 
