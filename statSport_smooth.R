library(dplyr)
library(lubridate)
library(reticulate)

repl_python()

pd <- import("pandas")
sig <- import("scipy.signal")

filepath = "C:/Users/awi027/FFC Dropbox/vaalerenga_raw_extended/Mars2020/14 March 2020-RawDataExtended/2020-03-14-Andrine-Entire Session.csv"
rawdata = pd.read_csv(filepath)


#Sys.getenv("PATH")
#Sys.setenv(PATH = paste("C:\\Rtools\\bin", Sys.getenv("PATH"), sep=";"))
#writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
#Sys.which("make")

options(digits.secs = 6)

filepath <- "C:/Users/awi027/FFC Dropbox/vaalerenga_raw_extended/Mars2020/14 March 2020-RawDataExtended/2020-03-14-Andrine-Entire Session.csv"

kukfett <- read.csv(filepath)

clean_smooth_f1 <- function(filepath) {
  rawdata <- read.csv(filepath)
  
  rawdata10hz <- rawdata %>% 
    select(-Lat, -Lon, -Hacc ,-Quality.of.Signal, -Accl.X, -Accl.Y, -Accl.Z, -Gyro.X, -Gyro.Y, -Gyro.Z) %>%
    distinct(Time, .keep_all = TRUE) %>% 
    mutate(TimeHMS = hms(Time),
           TimeR = strptime(Time, "%H:%M:%OS"),
           Seconds = round(as.numeric(TimeR - min(TimeR)), digits = 2),
           Time.Diff = (Seconds - dplyr::lag(Seconds, default = 0)),
           Smooth.Speed = signal::sgolayfilt(Speed..m.s., p = 1, n = 7),
           Smooth.Distance = Smooth.Speed * Time.Diff,
           Smooth.Accel = ((Smooth.Speed - dplyr::lag(Smooth.Speed, default = 0)) /(Seconds - dplyr::lag(Seconds, default = 0))),
           High.Speed = case_when(Smooth.Speed <= 4.17 ~ 0, Smooth.Speed > 4.17 ~ Smooth.Speed),
           Sprinting = case_when(Smooth.Speed <= 5.56 ~ 0, Smooth.Speed > 5.56 ~ Smooth.Speed),
           High.Speed.Distance = High.Speed * Time.Diff,
           Sprint.Distance = Sprinting * Time.Diff) 
    
  
  rawdata10hz <- rawdata10hz[complete.cases(rawdata10hz),]
  
  return(rawdata10hz)
  
}



cleaned_data1 <- clean_smooth_f1(filepath)


clean_smooth_f2 <- function(filepath) {
  rawdata <- read.csv(filepath)
  
  rawdata10hz <- rawdata %>% 
    select(-Lat, -Lon, -Hacc ,-Quality.of.Signal, -Accl.X, -Accl.Y, -Accl.Z, -Gyro.X, -Gyro.Y, -Gyro.Z) %>%
    distinct(Time, .keep_all = TRUE) %>% 
    dplyr::filter(Hdop < 2, 
                  No..of.Satellites > 8, 
                  Instantaneous.Acceleration.Impulse < 6,
                  Speed..m.s. < 10) %>%
    mutate(Time = as.POSIXct(strptime(Time, format = "%H:%M:%OS")),
           Time.Diff = round(c(0, diff(Time)), digits = 2))
           
  
  
  #,
           #TimeHMS = hms(Time),
           #DateTime = as_datetime(TimeHMS),
           #Seconds = round(as.numeric(TimeR - min(TimeR)), digits = 2),
           #Time.Diff = (Seconds - dplyr::lag(Seconds, default = 0)))
             
    
           
  
  #rawdata10hz <- rawdata10hz[complete.cases(rawdata10hz),]
  
  return(rawdata10hz)
}

cleaned_data2 <- clean_smooth_f2(filepath)



max(cleaned_data1$Speed..m.s.) * 3.6
max(cleaned_data2$Speed..m.s.) * 3.6
max(cleaned_data1$Instantaneous.Acceleration.Impulse) 
max(cleaned_data2$Instantaneous.Acceleration.Impulse)


signalDisruptions <- which(cleaned_data2$Time.Diff > 0.11)

cleaned_data2[signalDisruptions,] %>%
  summarise("Data loss in minutes" = sum(Time.Diff/60))



#Resample data


# create new time index -- must be posixct

start <- min(cleaned_data2$Time)
end <- max(cleaned_data2$Time)

#time.index <- seq.POSIXt(as.POSIXlt(start), as.POSIXlt(end), units = "seconds", by = 0.1)

time.index <- seq(start, end, by = as.difftime(0.1, units='secs'))

time.index.xts <- xts(time.index, order.by = time.index)
# remove ALL rows that have duplicated timestamps

cleaned_data2 <- cleaned_data2 %>%
                 dplyr::filter(!duplicated(Time) | duplicated(Time, fromLast = TRUE))


# convert data to xts object

cleaned_data2.xts <- xts(cleaned_data2, order.by = cleaned_data2$Time)
  

# Join time index

cleaned_data2.join <- merge(x = cleaned_data2.xts, y = time.index, all = TRUE) 

# Remove rows with a duplicated timestamp, but keep the latest one
cleaned_data3  <- cleaned_data2.join[!duplicated(index(cleaned_data2.join))]

index(cleaned_data3) <- as.Date(format(index(cleaned_data3), tz = ""))

!duplicated(coredata(cleaned_data2.join), fromLast = TRUE)

make.index.unique(cleaned_data2.join, eps = 1e-06, drop=TRUE, fromLast=TRUE)

str(time.index)

dput(head(time.index))
dput(head(cleaned_data2.xts))  


# linear interpolatation of speed with omission of leading / lagging NAs; constant interpolation of other variables
 
cleaned_data2_resampled <- cleaned_data2.join %>%
  mutate_at(vars(-one_of("Time")), function(x) na.approx(x, method = "constant", na.rm=F)) %>%
  mutate_at(vars(one_of("Speed..m.s.")), function(x) na.approx(x, na.rm=F)) 

  





#Max values
sum(rawdata10hz$Smooth.Distance)
sum(rawdata10hz$High.Speed.Distance)
sum(rawdata10hz$Sprint.Distance)
max(rawdata10hz$Speed..m.s.) *3.6
max(rawdata10hz$Smooth.Speed) *3.6
max(rawdata10hz$Smooth.Accel)
min(rawdata10hz$Smooth.Accel)




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


rawdata10hz %>%
  summarise(sprints = counter(Speed..m.s., 5.5, Time, 1),
            smooth_sprints = counter(Smooth.Speed, 5.5, Time, 1),
            accels = counter(Acceleration, 2, Time, 0.5),
            decels = counter(Acceleration, 2, Time, 0.5),
            smoothaccels = counter(smooth_accel, 2, Time, 0.5),
            smoothdecels = counter(smooth_accel, -2, Time, 0.5))
             

rawdata10hz <- rawdata10hz %>%
  mutate(hir_distance = (cumsum(ifelse(Smooth.Speed >= 4.17 & Smooth.Speed <= 5.56, Distance, 0))),
         sprint_distance = (cumsum(ifelse(Smooth.Speed > 5.56, Distance, 0))))

rawdata10hz %>%
  summarise(TotalDistance = sum(Distance),
            TotalHIRDistance = max(hir_distance),
            TotalSprintDistance = max(sprint_distance)) 



