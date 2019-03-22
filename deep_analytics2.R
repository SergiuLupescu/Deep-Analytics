library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(corrplot)
library(visdat)
library(forecast)
library(zoo)
library(imputeTS)

# load dataset.
energy <- read_delim("household_power_consumption.txt", 
                     ";",
                     escape_double = FALSE, 
                     col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                      Time = col_time(format = "%H:%M:%S")), 
                     locale = locale(), 
                     trim_ws = TRUE)



# change colnames() for easier to work with names.
colnames(energy) <- c("Date", "Time", "Gap", "Grp", "Voltage", "Gi", 
                      "Sub1", "Sub2", "Sub3")

# create new year column using lubridate year().
energy$year <- year(energy$Date)

# create new column with the weekday name for each row.
energy$weekday <- weekdays(energy$Date)

# create new column with each weekday as factor.
energy$day <- factor(energy$weekday, levels = c("Monday", "Tuesday", "Wednesday",
                                                "Thursday", "Friday", "Saturday",
                                                "Sunday"))

# create new column for the energy used not recorded by the submeters 1-2-3.
energy$Gap <- energy$Gap * 1000
energy$Grp <- energy$Grp * 1000
energy$Sub4 <- energy$Gap/60 - energy$Sub1 - energy$Sub2 - energy$Sub3
energy$Sub4 <- round(energy$Sub4)

theme_set(theme_bw())

energyCor <- energy %>%  select(Gap, Grp, Voltage, Gi, Sub1, Sub2, Sub3, Sub4)

corrplot.mixed(cor(na.omit(energyCor)), upper = "square", number.cex = 1)

#### missing data plot # vis_miss(energy, warn_large_data = F) ####

energy$time <- as.POSIXct(paste(energy$Date, energy$Time), format = "%Y-%m-%d %H:%M:%S")

#### dataset granulated by 10 min ####
test1 <- energy %>% group_by(cut(time, breaks = "10 min")) %>% 
                    summarise(Gap10 = sum(Gap, na.rm = T),
                              Grp10 = sum(Grp, na.rm = T),
                              Voltage10 = sum(Voltage, na.rm = T),
                              Gi10 = sum(Gi, na.rm = T),
                              Sub1ten = sum(Sub1, na.rm = T),
                              Sub2ten = sum(Sub2, na.rm = T),
                              Sub3ten = sum(Sub3, na.rm = T),
                              Sub4ten = sum(Sub4, na.rm = T))


#### dataset for 01/02/2007. ####
test2 <- energy %>%  filter(year(Date) == "2007", month(Date) == 2, day(Date) == 1)
# test2 <- energy %>%  filter(year(Date) == "2007", month(Date) == 6, day(Date) == 20)

# plot for one day for all submeters.
ggplot(test2) +
  geom_line(aes(x = Time, y = Sub1, color = "Sub1"), size = 1) +
  geom_line(aes(x = Time, y = Sub2, color = "Sub2"), size = 1) +
  geom_line(aes(x = Time, y = Sub3, color = "Sub3"), size = 1) +
  geom_line(aes(x = Time, y = Sub4, color = "Sub4"), size = 1) +
  scale_y_continuous(breaks = seq(0, 90, by = 5), limit = c(0, 90)) +
  scale_x_time(breaks = seq(0, 86400, by = 3600)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        text = element_text(size = 22),
        plot.title = element_text(face = "bold", hjust = .5)) +
  ggtitle("01/02/2007 Thursday")
#  ggtitle("20/06/2007 Wednesday")

#### dataset for one week. ####
test3 <- energy %>% group_by(day) %>% summarise(Sub1mean = mean(Sub1, na.rm = T),
                                                Sub2mean = mean(Sub2, na.rm = T),
                                                Sub3mean = mean(Sub3, na.rm = T),
                                                Sub4mean = mean(Sub4, na.rm = T))

ggplot(test3) +
  geom_point(aes(x = day, y = Sub1mean, color = "Sub1mean")) +
  geom_point(aes(x = day, y = Sub2mean, color = "Sub2mean")) +
  geom_point(aes(x = day, y = Sub3mean, color = "Sub3mean")) +
  geom_point(aes(x = day, y = Sub4mean, color = "Sub4mean")) +
  geom_line(aes(x = day, y = Sub4mean, color = "Sub4mean", group = 1)) +
  geom_line(aes(x = day, y = Sub3mean, color = "Sub3mean", group = 1)) +
  geom_line(aes(x = day, y = Sub2mean, color = "Sub2mean", group = 1)) +
  geom_line(aes(x = day, y = Sub1mean, color = "Sub1mean", group = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        text = element_text(size = 22),
        plot.title = element_text(face = "bold", hjust = .5)) +
  ggtitle("")


#### time series spike notes ####
# my_ts <- ts(df, frequency = number of records in one year)

# Stationarity:
#  mean has to be constant, mean()
#  variance has to be constant, sd()
  
# season : patterns that repeats.
# trend : 
# random : not described by season and trend.
  
# decompose() missing values at extremes.
# ets()

# additive: y = y - seasonal
# multiplicative: y = y / seasonal
# ggAcf() , ggPacf() , ggtsdisplay()

# grouping
# build ts
# decompose
# forecast

# models and assumptions:
# naive: next values is the previous ones.
# tslm() linear combination of previous values.
# ARIMA (p, d, q)
# HoltWinters (a, b, g)

# split data: window(ts, start =, end =)
# cross-validation: tsCV()
# accuracy(model, test_set)

# ARIMA auto regressive integrated moving averrage
# only additive ts
# log() from multiplicative to additive
# auto.arima() aproximations for p,d,q
# p: number of peaks in PACF above the threshold.
# q: number of peaks in ACF above the threshold.
# d: number of differences before having a stationary ts.
# 2,1,3 -> 1,2,2 -> 0,3,1. Increase D + 1 and decrease p,q - 1.

# HoltWinters
# a: random
# b: trend
# g: seasonality
# it can handle additive and miltiplicative.
# 0 values uses past values, 1 uses recent values.

# forecasting evaluation: accuracy(model, test_data)

#### grouping for time series ####

na.approx(energy$Gap)
na.approx(energy$Grp)
na.approx(energy$Gi)
na.approx(energy$Sub1)
na.approx(energy$Sub2)
na.approx(energy$Sub3)
na.approx(energy$Sub4)
# na.ma is another option from imputeTS package.
energy_month <- energy %>% group_by(year(Date), month(Date)) %>% filter(year(Date) != 2006) %>% 
                           summarise(GAP = sum(Gap, na.rm = TRUE),
                                     GRP = sum(Grp, na.rm = TRUE),
                                     GI = sum(Gi, na.rm = TRUE),
                                     SUB1 = sum(Sub1, na.rm = TRUE),
                                     SUB2 = sum(Sub2, na.rm = TRUE),
                                     SUB3 = sum(Sub3, na.rm = TRUE),
                                     SUB4 = sum(Sub4, na.rm = TRUE))
#### ts object by month ####
ts_energy <- ts(energy_month, start = c(2007,1), end = c(2010,11), frequency = 12)

#### ts_energy plots ####
autoplot(ts_energy[ ,"GAP"])
autoplot(ts_energy[ ,"GRP"])
autoplot(ts_energy[ ,"GI"])
autoplot(ts_energy[ ,"SUB1"])
autoplot(ts_energy[ ,"SUB2"])
autoplot(ts_energy[ ,"SUB3"])
autoplot(ts_energy[ ,"SUB4"])
autoplot(stl(ts_energy[ ,"GAP"], s.window = "periodic"))

#### ts_energy train and test ####
ts_train <- window(ts_energy, start = c(2007,1), end = c(2009,12))
ts_test <- window(ts_energy, start = c(2010,1), end = c(2010,11))

plot(decompose(ts_train[ ,"GAP"]))

#### tslm dataframe ####
ts_train_GAP <- data.frame(ts_train[ ,"GAP"], as.numeric(time(ts_train)))
ts_train_GRP <- data.frame(ts_train[ ,"GRP"], as.numeric(time(ts_train)))
ts_train_GI <- data.frame(ts_train[ ,"GI"], as.numeric(time(ts_train)))
ts_train_SUB1 <- data.frame(ts_train[ ,"SUB1"], as.numeric(time(ts_train)))
ts_train_SUB2 <- data.frame(ts_train[ ,"SUB2"], as.numeric(time(ts_train)))
ts_train_SUB3 <- data.frame(ts_train[ ,"SUB3"], as.numeric(time(ts_train)))
ts_train_SUB4 <- data.frame(ts_train[ ,"SUB4"], as.numeric(time(ts_train)))
names(ts_train_GAP) <- c("GAP", "Time")
names(ts_train_GRP) <- c("GRP", "Time")
names(ts_train_GI) <- c("GI", "Time")
names(ts_train_SUB1) <- c("SUB1", "Time")
names(ts_train_SUB2) <- c("SUB2", "Time")
names(ts_train_SUB3) <- c("SUB3", "Time")
names(ts_train_SUB4) <- c("SUB4", "Time")
#### tslm model and forecast ####
# GAP model.
mod_tslm_GAP <- tslm(GAP ~ season + trend, ts_train_GAP)
fc_tslm_GAP <- forecast(mod_tslm_GAP, h = 11)
autoplot(fc_tslm_GAP)
plot(fc_tslm_GAP, col = "red")
lines(ts_test[ ,"GAP"], col = "black", lwd = 2)

#GRP model.
mod_tslm_GRP <- tslm(GRP ~ season + trend, ts_train_GRP)
fc_tslm_GRP <- forecast(mod_tslm_GRP, h = 11)
autoplot(fc_tslm_GRP)
plot(fc_tslm_GRP, col = "red")
lines(ts_test[ ,"GRP"], col = "black", lwd = 2)

#GI model.
mod_tslm_GI <- tslm(GI ~ season + trend, ts_train_GI)
fc_tslm_GI <- forecast(mod_tslm_GI, h = 11)
autoplot(fc_tslm_GI)
plot(fc_tslm_GI, col = "red")
lines(ts_test[ ,"GI"], col = "black", lwd = 2)

#SUB1 model.
mod_tslm_SUB1 <- tslm(SUB1 ~ season + trend, ts_train_SUB1)
fc_tslm_SUB1 <- forecast(mod_tslm_SUB1, h = 11)
autoplot(fc_tslm_SUB1)
plot(fc_tslm_SUB1, col = "red")
lines(ts_test[ ,"SUB1"], col = "black", lwd = 2)

#SUB2 model.
mod_tslm_SUB2 <- tslm(SUB2 ~ season + trend, ts_train_SUB2)
fc_tslm_SUB2 <- forecast(mod_tslm_SUB2, h = 11)
autoplot(fc_tslm_SUB2)
plot(fc_tslm_SUB2, col = "red")
lines(ts_test[ ,"SUB2"], col = "black", lwd = 2)

#SUB3 model.
mod_tslm_SUB3 <- tslm(SUB3 ~ season + trend, ts_train_SUB3)
fc_tslm_SUB3 <- forecast(mod_tslm_SUB3, h = 11)
autoplot(fc_tslm_SUB3)
plot(fc_tslm_SUB3, col = "red")
lines(ts_test[ ,"SUB3"], col = "black", lwd = 2)

#SUB4 model.
mod_tslm_SUB4 <- tslm(SUB4 ~ season + trend, ts_train_SUB4)
fc_tslm_SUB4 <- forecast(mod_tslm_SUB4, h = 11)
autoplot(fc_tslm_SUB4)
plot(fc_tslm_SUB4, col = "red")
lines(ts_test[ ,"SUB4"], col = "black", lwd = 2)

#### ARIMA MODEL ###
autoplot(ts_train[ ,"GAP"])
plot(decompose(ts_train[ ,"GAP"]))
ggAcf(ts_train[ ,"GAP"])
ggPacf(ts_train[ ,"GAP"])

mod_arima_GAP <- auto.arima(ts_train[ ,"GAP"], D = 1)
fc_arima_GAP <- forecast(mod_arima_GAP, h = 11)

summary(residuals(mod_tslm_GAP))
checkresiduals(mod_tslm_GAP)

autoplot(fc_arima_GAP)
plot(fc_arima_GAP, col = "red")
lines(ts_test[ ,"GAP"], col = "black", lwd = 2)

#### HOLT WINTERS ####
mod_hw_GAP <- hw(ts_train[ ,"GAP"], seasonal="additive")
fc_hw_GAP <- forecast(mod_hw_GAP, h = 11)

autoplot(fc_hw_GAP)
plot(fc_hw_GAP, col = "red")
lines(ts_test[ ,"GAP"], col = "black", lwd = 2)






#### interesting code for plots ####
# ggtsdisplay(energyTime)
#
# gap1 <- na.interp(energy$Gap)
# autoplot(gap1, series = "Interpolated") +
#   autolayer(ts(energy$Gap), series = "Original") +
#   scale_colour_manual(values = c(`Interpolated` = "red", `Original` = "gray"))
#
# plot(energy$Gap ~ month(energy$time), type="l")

#### code review notes####
# 1 Fill NA
# 2 Make groups
# 3 Create TS
# 4 Decompose
# 5 TS split
# 6 Train model
# 7 Evaluate model
# 8 Forecast
#
# ARIMA -> p = PACF, q = ACF
# ts - decompose$seasonality
# model_ts + decompose$seasonality
#
#
#
#
#
#

