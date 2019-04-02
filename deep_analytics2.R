library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(corrplot)
library(visdat)
library(forecast)
library(zoo)
library(imputeTS)
library(opera)

#### load dataset for energy consumption ####
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
# meters and global active power in Kwh
energy$Gap <- energy$Gap * 1000
energy$Grp <- energy$Grp * 1000
energy$Sub4 <- energy$Gap/60 - energy$Sub1 - energy$Sub2 - energy$Sub3
energy$Sub4 <- round(energy$Sub4)

energy$Gap <- energy$Gap / 1000 / 60
energy$Grp <- energy$Grp / 1000 / 60
energy$Sub1 <- energy$Sub1 / 1000
energy$Sub2 <- energy$Sub2 / 1000
energy$Sub3 <- energy$Sub3 / 1000
energy$Sub4 <- energy$Sub4 / 1000

write.csv(energy, "./power_bi/energy.csv")

theme_set(theme_bw())

energyCor <- energy %>%  select(Gap, Grp, Voltage, Gi, Sub1, Sub2, Sub3, Sub4)

corrplot.mixed(cor(na.omit(energyCor)), upper = "square", number.cex = 1)

#### missing data plot # vis_miss(energy, warn_large_data = F) ####

#### date time column creates NA due to daylight saving hours change ####
# energy$time <- as.POSIXct(paste(energy$Date, energy$Time), format = "%Y-%m-%d %H:%M:%S")

#### granularity 10 min####
test1 <- energy %>% group_by(cut(time, breaks = "10 min")) %>% 
                    summarise(Gap10 = sum(Gap, na.rm = T),
                              Grp10 = sum(Grp, na.rm = T),
                              Voltage10 = sum(Voltage, na.rm = T),
                              Gi10 = sum(Gi, na.rm = T),
                              Sub1ten = sum(Sub1, na.rm = T),
                              Sub2ten = sum(Sub2, na.rm = T),
                              Sub3ten = sum(Sub3, na.rm = T),
                              Sub4ten = sum(Sub4, na.rm = T))


#### 01/02/2007 dataset ####
test2 <- energy %>%  filter(year(Date) == "2007", month(Date) == 2, day(Date) == 1)


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


#### granularity one week ####
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


#### grouping for time series ####
col_na <- c("Gap", "Grp", "Voltage", "Gi", 
            "Sub1", "Sub2", "Sub3", "Sub4")


energy[col_na] <- apply(energy[col_na], 2, FUN = na.approx)

anyNA(energy)
which(is.na(energy), arr.ind = T)

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
autoplot(stl(ts_energy[ ,"GAP"], s.window = "periodic"))

#### ts_energy train and test ####
ts_train <- window(ts_energy, start = c(2007,1), end = c(2009,12))
ts_test <- window(ts_energy, start = c(2010,1), end = c(2010,11))

plot(decompose(ts_train[ ,"GAP"]))

#### tslm dataframe ####
ts_train_GAP <- data.frame(ts_train[ ,"GAP"], as.numeric(time(ts_train)))

names(ts_train_GAP) <- c("GAP", "Time")

#### tslm model and forecast ####
# GAP model.
mod_tslm_GAP <- tslm(GAP ~ season + trend, ts_train_GAP)
fc_tslm_GAP <- forecast(mod_tslm_GAP, h = 11)
autoplot(fc_tslm_GAP)
plot(fc_tslm_GAP, col = "red")
lines(ts_test[ ,"GAP"], col = "black", lwd = 2)

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

autoplot(fc_hw_GAP) +
  autolayer()


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

#### autoplot forecasts and accuracy test ####
autoplot(ts_energy[ ,"GAP"], lwd = 1.5) +
  autolayer(fc_tslm_GAP, series = "TSLM", PI = T, lwd = 1.5, alpha = .6) +
  autolayer(fc_arima_GAP, series = "ARIMA", PI = T, lwd = 1.5, alpha = .6) +
  autolayer(fc_hw_GAP, series = "HW", PI = T, lwd = 1.5, alpha = .6) +
  xlab("Year") + ylab("wH") +
  ggtitle("Forecasts for global active power") +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(fc_tslm_GAP, ts_test[ ,"GAP"])
accuracy(fc_arima_GAP, ts_test[ ,"GAP"])
accuracy(fc_hw_GAP, ts_test[ ,"GAP"])

#### changing forecast from 11 months to 1 month ####
fc_tslm_GAP1 <- forecast(mod_tslm_GAP, h = 2)
fc_arima_GAP1 <- forecast(mod_arima_GAP, h = 2)
fc_hw_GAP1 <- forecast(mod_hw_GAP, h = 2)

#### autoplot and acurracy for 1 month ####
autoplot(ts_energy[ ,"GAP"], lwd = 1.5) +
  autolayer(fc_tslm_GAP1, series = "TSLM", PI = FALSE, lwd = 1.5, alpha = .5) +
  autolayer(fc_arima_GAP1, series = "ARIMA", PI = FALSE, lwd = 1.5, alpha = .5) +
  autolayer(fc_hw_GAP1, series = "HW", PI = FALSE, lwd = 1.5, alpha = .5) +
  xlab("Year") + ylab("wH") +
  ggtitle("Forecasts for global active power") +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(fc_tslm_GAP1, ts_test[ ,"GAP"])
accuracy(fc_arima_GAP1, ts_test[ ,"GAP"])
accuracy(fc_hw_GAP1, ts_test[ ,"GAP"])

#### grouping by day ####
energy_day <- energy %>% group_by(year(Date), month(Date), day(Date)) %>% filter(year(Date) != 2006) %>% 
  summarise(GAP = sum(Gap, na.rm = TRUE),
            GRP = sum(Grp, na.rm = TRUE),
            GI = sum(Gi, na.rm = TRUE),
            SUB1 = sum(Sub1, na.rm = TRUE),
            SUB2 = sum(Sub2, na.rm = TRUE),
            SUB3 = sum(Sub3, na.rm = TRUE),
            SUB4 = sum(Sub4, na.rm = TRUE))

write.csv(energy_day, "./power_bi/energy_day.csv")

#### ts object by day ####
ts_energy_day <- msts(energy_day, seasonal.periods=c(12,365.25), start = 2007)

autoplot(stl(ts_energy_day[ ,"GAP"], s.window = "periodic"))



#### ts_energy train and test ####
ts_train_day <- window(ts_energy_day, start = 2007, end = 2010)
ts_test_day <- window(ts_energy_day, start = c(2010,1))

mod_arima_day <- auto.arima(ts_train_day[ ,"GAP"])
fc_arima_day <- forecast(mod_arima_day, h = 330)


autoplot(fc_arima_day)
plot(fc_arima_day, col = "red")
lines(ts_test_day[ ,"GAP"], col = "black", lwd = 2)


#### opera package ####

ARIMA <- forecast(auto.arima(ts_train[ ,"GAP"], D = 1), h = 11)
HW <- forecast(hw(ts_train[ ,"GAP"], seasonal="additive"), h = 11)
X <- cbind(ARIMA = ARIMA$mean, HW = HW$mean)
MLpol0 <- mixture(model = "MLpol", loss.type = "square")
z <- ts(predict(MLpol0, X, ts_test[ ,"GAP"], type = 'response'), start=c(2010,1), freq=12)
df1 <- cbind(ts_energy[ ,"GAP"], z)
colnames(df1) <- c("Data","Mixture")
autoplot(df1)


summary(mod_arima_GAP)

#### forecast for power bi ####

pb_mod_arima_GAP <- auto.arima(ts_energy[ ,"GAP"])
pb_fc_arima_GAP <- forecast(pb_mod_arima_GAP, h = 12)
autoplot(pb_fc_arima_GAP)
write.csv(pb_fc_arima_GAP, "./power_bi/arima_GAP.csv")




