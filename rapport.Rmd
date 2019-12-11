---
title: "Calculating tracking metrics using ZXY data"
author: "Andreas K. Winther"
output: rmarkdown::github_document
bibliography: references.bib
csl: apa.csl
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Intro

This markdown document shows how to calculate some basic tracking metrics using data published by @pettersen2014soccer. A more indepth look into the following code and the metrics themselves will be part of a future blogpost on wintherperformance.com. 

The following packages are required:
```{r message=FALSE}
library(signal)
library(zoo)
library(dplyr)
library(ggplot2)
```

### Importing and changing column names
Import the dataset. Then change the names of the variables while adding a column with the time elapsed in seconds, and one in minutes. Next, use the x and y columns to calculate distance.
```{r}
df <- read.csv("http://home.ifi.uio.no/paalh/dataset/alfheim/2013-11-03/zxy/2013-11-03_tromso_stromsgodset_first.csv", header = FALSE)

names(df) <- c("timestamp", "tag_id", "x_pos", "y_pos", "heading", "direction", "energy", "speed", "total_distance")

options(digits.secs = 6)
df$timestamp <- as.POSIXct(df$timestamp,format = "%Y-%m-%d %H:%M:%OS")
df$seconds <- as.numeric(df$timestamp - min(df$timestamp))
df$minutes <- df$seconds/60

df <- df %>%
  group_by(tag_id) %>%
  mutate(distance = (sqrt((x_pos - lag(x_pos))^2 + (y_pos - lag(y_pos))^2)))
```

### Filtering/smoothing 
A lot of metrics are derivatives of either speed or velocity. It is therefore useful to check the data for noise and outliers. Examine the "speed" column by first plotting a histogram and then summarising the data:
```{r}
hist(df$speed, breaks = 50)
summary(df$speed)
```
The histogram shows a lot of outliers on the far left side, while the highest speed is recorded at 11.31 m/s or 40.7 km/h. This is highly unlikely, and should be closer to 10 m/s. 
Remove all values with "speed == 0", and then run the speed column through a low-pass filter as such: 
```{r warning=FALSE}
df <- df[df$speed > 0,]

bf1 <- butter(1, 0.06, type = "low", plane = "z")
df <- df %>%
  group_by(tag_id) %>%
  mutate(speedSmoothed = signal::filter(bf1, speed))

summary(df$speedSmoothed)
hist(df$speedSmoothed, breaks = 50)
```

### Total HI distance and total sprint distance

Now to calculate the total HI and sprint distance, do the following:
```{r}
df <- df %>%
  group_by(tag_id) %>%
  mutate(hir_distance = (cumsum(ifelse(speedSmoothed >=5.5 & speedSmoothed <= 7, distance, 0))),
         sprint_distance = (cumsum(ifelse(speedSmoothed > 7, distance, 0))))

df %>%
  group_by(tag_id) %>%
  summarise(TotalDistance = max(total_distance),
            TotalHIRDistance = max(hir_distance),
            TotalSprintDistance = max(sprint_distance)) %>%
  dplyr::filter(!tag_id %in% c(3,6,11))
              
```

### Accelerations and decelerations
Next, to calculate accelerations and decelerations, do the following:

```{r warning=FALSE}
bf2 <- butter(1, 0.03, "low", "z")

df <- df %>%
  group_by(tag_id) %>%
  mutate(acceleration = c(NA, diff(speedSmoothed)) / c(NA, diff(timestamp)),
         accelerationSmoothed = c(NA, NA, signal::filter(bf2, acceleration)))

summary(df$accelerationSmoothed)
hist(df$accelerationSmoothed, breaks = 50)
```


The number of sprints, accelerations and decelerations can then be extracted using the following function:
```{r warning=FALSE}
counter <- function(metric, threshold, timestamp, timeThreshold) {
  metricVector <- vector("numeric", length = length(metric))
  if (threshold > 0) {
    for (i in 1:length(metric)) {
    if (metric[i] > threshold) {
      metricVector[i] = 1
      } 
    } 
  }
  if (threshold < 0) {
    for (i in 1:length(metric)) {
    if (metric[i] < threshold) {
      metricVector[i] = 1
      } 
    } 
  }
  rleMetric <- rle(metricVector)
  indices <- cumsum(rleMetric$lengths)
  if (length(indices) %% 2 == 1) {
    indices <- c(indices, indices[length(indices)])
  }
  metricStart <- indices[seq(from = 1, to = length(indices), by = 2)]
  metricEnd <- indices[seq(from = 2, to = length(indices), by = 2)]
  metricStartStamp <- timestamp[metricStart]
  metricEndStamp <- timestamp[metricEnd]
  metricTimeDiff <- metricEndStamp - metricStartStamp
  return(sum(metricTimeDiff > timeThreshold))
}

df %>%
  group_by(tag_id) %>%
  summarise(accelerations = counter(na.omit(accelerationSmoothed), 2, timestamp, 0.5), 
            decelerations = counter(na.omit(accelerationSmoothed), -2, timestamp, 0.5),
            sprints = counter(speedSmoothed, 7, timestamp, 1)) %>%
  dplyr::filter(!tag_id %in% c(3,6,11))
```

### Work rate
Other common metrics such as sprint and acceleration distance, and sprint and acceleration workrate (distance per minutes), are derived as follows: 

```{r}
metricDistance <- function(metric, metricThreshold, timestamp, distance, timeThreshold) {
  metricVector <- vector("numeric", length = length(metric))
  if (metricThreshold > 0) {
  for (i in 1:length(metric)) {
    if (metric[i] > metricThreshold) {
      metricVector[i] = 1
      }
    }
  }  
  if (metricThreshold < 0) {
    for (i in 1:length(metric)) {
    if (metric[i] < metricThreshold) {
      metricVector[i] = 1
      } 
    } 
  }
  metricIndex <- rle(metricVector)
  metricIndexes <- cumsum(metricIndex$lengths)
  if (length(metricIndexes) %% 2 == 1) {
    metricIndexes <- c(metricIndexes, metricIndexes[length(metricIndexes)])
  }
  metricStart <- metricIndexes[seq(from = 1, to = length(metricIndexes), by = 2)]
  metricEnd <- metricIndexes[seq(from = 2, to = length(metricIndexes), by = 2)]
  metricStartStamp <- timestamp[metricStart] 
  metricEndStamp <- timestamp[metricEnd] 
  metricDistStartStamp <- distance[metricStart] 
  metricDistEndStamp <- distance[metricEnd] 
  metricTimeDiff <- metricEndStamp - metricStartStamp 
  metricDistDiff <- metricDistEndStamp - metricDistStartStamp 
  return(sum(metricDistDiff[metricTimeDiff >= timeThreshold])) 
}

df %>%
  group_by(tag_id) %>%
  summarise(accelDist = metricDistance(na.omit(accelerationSmoothed), 2, timestamp, total_distance, 0.5),
            decelDist = metricDistance(na.omit(accelerationSmoothed), -2, timestamp, total_distance, 0.5),
            sprintDist = metricDistance(speedSmoothed, 7, timestamp, total_distance, 1),
            workRateAccel = accelDist/max(minutes),
            workRateDecel = decelDist/max(minutes),
            workRateSprint = sprintDist/max(minutes),
            TotalDistance = max(total_distance),
            TotalHIRDistance = max(hir_distance),
            TotalSprintDistance = max(sprint_distance)) %>%
  dplyr::filter(!tag_id %in% c(3,6,11))
```

### Number of sprints withing distance bands

The number sprints within certain distance bands can be extracted using the following function:

```{r}
sprintDistance <- function(speed, speedThreshold, timestamp, distance, timeThreshold, meterStart, meterEnd) {
  sprintVector <- vector("numeric", length = length(speed))
  for (i in 1:length(speed)) {
    if (speed[i] > speedThreshold) {
      sprintVector[i] = 1
    }
  }
  sprintIndex <- rle(sprintVector)
  sprintIndexes <- cumsum(sprintIndex$lengths)
  if (length(sprintIndexes) %% 2 == 1) {
    sprintIndexes <- c(sprintIndexes, sprintIndexes[length(sprintIndexes)])
  }
  sprintStart <- sprintIndexes[seq(from = 1, to = length(sprintIndexes), by = 2)]
  sprintEnd <- sprintIndexes[seq(from = 2, to = length(sprintIndexes), by = 2)]
  sprintStartStamp <- timestamp[sprintStart] #Subsets timestamp indexes where a sprint starts
  sprintEndStamp <- timestamp[sprintEnd] #Subsets timestamp indexes where a sprint ends
  sprintDistStartStamp <- distance[sprintStart] #Subsets distance indexes where a sprint starts 
  sprintDistEndStamp <- distance[sprintEnd] #Subsets distance indexes where a sprint ends 
  sprintTimeDiff <- sprintEndStamp - sprintStartStamp #Calculates the time difference between sprint start and end
  sprintDistDiff <- sprintDistEndStamp - sprintDistStartStamp
  sprintDistDiff <- sprintDistDiff >= meterStart & sprintDistDiff <= meterEnd
  return(sum(sprintDistDiff[sprintTimeDiff > timeThreshold]))
}

df %>%
  group_by(tag_id) %>%
  summarise(sprintMeters1_5 = sprintDistance(speedSmoothed, 7, timestamp, total_distance, 0, 1, 5),
            sprintMeters6_10 = sprintDistance(speedSmoothed, 7, timestamp, total_distance, 0, 6, 10),
            sprintMeters11_15 = sprintDistance(speedSmoothed, 7, timestamp, total_distance, 0, 11, 15),
            sprintMeters16_20 = sprintDistance(speedSmoothed, 7, timestamp, total_distance, 0, 16, 20),
            sprintMeters21_25 = sprintDistance(speedSmoothed, 7, timestamp, total_distance, 0, 21, 25),
            sprintMeters26_30 = sprintDistance(speedSmoothed, 7, timestamp, total_distance, 0, 26, 30),
            sprintMeters31_35 = sprintDistance(speedSmoothed, 7, timestamp, total_distance, 0, 31, 35),
            sprintMeters36_40 = sprintDistance(speedSmoothed, 7, timestamp, total_distance, 0, 36, 40),
            sprintMeters41plus = sprintDistance(speedSmoothed, 7, timestamp, total_distance, 0, 41, 80)) %>%
  dplyr::filter(!tag_id %in% c(3,6,11))
```

###Metabolic power

Another interesting (but controversial) metric is metabolic power:
```{r warning=FALSE}

rad2deg <- function(rad) {(rad * 180) / (pi)}

bf3 <- butter(1, 0.01, "low", "z")

df <- df %>%
  group_by(tag_id) %>%
  mutate(alpha = rad2deg(atan(9.81/accelerationSmoothed)),
         g = sqrt(accelerationSmoothed^2 + 9.81^2),
         equivilantSlope = tan(0.5 * pi - atan(9.81/accelerationSmoothed)),
         equivilantMass = g/9.81,
         energyCost = ifelse(accelerationSmoothed == 0, 3.6*1.29, (155.4*equivilantSlope^5 - 30.4*equivilantSlope^4 - 43.3*equivilantSlope^3 + 46.3*equivilantSlope^2 + 19.5*equivilantSlope + 3.6)*equivilantMass*1.29),
         metabolicPower = energyCost*speedSmoothed,
         metabolicPowerSmoothed = c(NA, NA, NA, signal::filter(bf3, metabolicPower)))

df$metabolicPowerSmoothed[is.na(df$metabolicPowerSmoothed)] <- 0 

df <- df %>%
  group_by(tag_id) %>%
  mutate(HighPowerDistance = cumsum(ifelse(metabolicPowerSmoothed > 20 & metabolicPowerSmoothed < 35, distance, 0)),
         ElevatedPowerDistance = cumsum(ifelse(metabolicPowerSmoothed > 35 & metabolicPowerSmoothed < 55, distance, 0)),
         MaxPowerDistance = cumsum(ifelse(metabolicPowerSmoothed > 55, distance, 0)))
         


df %>%
  group_by(tag_id) %>%
  summarise(HighPowerDistance = max(HighPowerDistance),
            ElevatedPowerDistance = max(ElevatedPowerDistance),
            MaxPowerDistance = max(MaxPowerDistance)) %>%
  dplyr::filter(!tag_id %in% c(3,6,11))

```

## References

