library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)

# load dataset.
energy <- read_delim("household_power_consumption.txt", 
                     ";",
                     escape_double = FALSE, 
                     col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                      Time = col_time(format = "%H:%M:%S")), 
                     locale = locale(), 
                     trim_ws = TRUE)
summary(energy)

# create new year column using lubridate year().
energy$year <- year(energy$Date)

str(energy$year)

# change colnames() for easier to work with names.
colnames(energy) <- c("Date", "Time", "G.a.p", "G.r.p", "Voltage", "G.i", 
                      "Sub1", "Sub2", "Sub3", "Year")

# some simple plots for initial dataset exploration.
# plot for active power per date using all dataset.
ggplot(energy, aes(x = Date, y = G.a.p)) +
  geom_point()

# plot submeters per date using all dataset.
ggplot(energy) +
  geom_line(aes(x = Date, y = Sub1, color = "red")) +
  geom_line(aes(x = Date, y = Sub2, color = "blue")) +
  geom_line(aes(x = Date, y = Sub3, color = "green"))

# create new column for the energy used not recorded by the submeters 1-2-3.
energy$G.a.p <- energy$G.a.p * 1000
energy$G.r.p <- energy$G.r.p * 1000
energy$Sub4 <- energy$G.a.p/60 - energy$Sub1 - energy$Sub2 - energy$Sub3
energy$Sub4 <- round(energy$Sub4)

# new dataset for only day 01/02/2007.
test <- energy %>%  filter(year(Date) == "2007", month(Date) == 2, day(Date) == 1)

# plotting the test dataset, time per submeters.
ggplot(test) +
  geom_point(aes(x = Time, y = Sub1, color = "Sub1")) +
  geom_point(aes(x = Time, y = Sub2, color = "Sub2")) +
  geom_point(aes(x = Time, y = Sub3, color = "Sub3")) +
  geom_point(aes(x = Time, y = Sub4, color = "Sub4")) +
  scale_x_time(breaks = seq(0, 86400, by = 3600)) +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1), colour = "#05afff"),
        axis.title.x = element_text(colour = "#05afff"),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#05afff", face = "bold", hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "#05afff"),
        legend.key = element_rect(colour = "#05afff",fill = "black")) +
  ggtitle("01/02/2007 Energy Consumption")

# new dataset grouped by time and the mean of the submeters for each group.
test2 <- energy %>% group_by(Time) %>%  summarise(Sub1mean = mean(Sub1, na.rm = T),
                                                  Sub2mean = mean(Sub2, na.rm = T),
                                                  Sub3mean = mean(Sub3, na.rm = T),
                                                  Sub4mean = mean(Sub4, na.rm = T))
# plotting dataset test2 , time per submeters.
ggplot(test2) +
  geom_point(aes(x = Time, y = Sub1mean, color = "Sub1mean")) +
  geom_point(aes(x = Time, y = Sub2mean, color = "Sub2mean")) +
  geom_point(aes(x = Time, y = Sub3mean, color = "Sub3mean")) +
  geom_point(aes(x = Time, y = Sub4mean, color = "Sub4mean")) +
  scale_x_time(breaks = seq(0, 86400, by = 3600)) +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1), colour = "#05afff"),
        axis.title.x = element_text(colour = "#05afff"),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#05afff", face = "bold", hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "#05afff"),
        legend.key = element_rect(colour = "#05afff",fill = "black")) +
  ggtitle("Daily Mean of Energy Consumption")

# new dataset grouped by date and the mean of the submeters for each group.
test3 <- energy %>% group_by(Date) %>% summarise(Sub1mean = mean(Sub1, na.rm = T),
                                                 Sub2mean = mean(Sub2, na.rm = T),
                                                 Sub3mean = mean(Sub3, na.rm = T),
                                                 Sub4mean = mean(Sub4, na.rm = T))
# plotting dataset test3, date per submeters.
ggplot(test3) +
  geom_point(aes(x = Date, y = Sub1mean, color = "Sub1mean")) +
  geom_point(aes(x = Date, y = Sub2mean, color = "Sub2mean")) +
  geom_point(aes(x = Date, y = Sub3mean, color = "Sub3mean")) +
  geom_point(aes(x = Date, y = Sub4mean, color = "Sub4mean")) +
  scale_x_date(date_breaks = "1 month") +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1), colour = "#05afff"),
        axis.title.x = element_text(colour = "#05afff"),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#05afff", face = "bold", hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "#05afff"),
        legend.key = element_rect(colour = "#05afff",fill = "black")) +
  ggtitle("Anual Daily Mean of Consumption")

# new dataset grouped by date and for year 2009 and the mean of the submeters for each group.
test4 <- energy %>% group_by(Date) %>% filter(year(Date) == "2009") %>% 
                    summarise(Sub1mean = mean(Sub1, na.rm = T),
                              Sub2mean = mean(Sub2, na.rm = T),
                              Sub3mean = mean(Sub3, na.rm = T),
                              Sub4mean = mean(Sub4, na.rm = T)) 

# plotting dataset test4, date per submeters.
ggplot(test4) +
  geom_point(aes(x = Date, y = Sub1mean, color = "Sub1mean")) +
  geom_point(aes(x = Date, y = Sub2mean, color = "Sub2mean")) +
  geom_point(aes(x = Date, y = Sub3mean, color = "Sub3mean")) +
  geom_point(aes(x = Date, y = Sub4mean, color = "Sub4mean")) +
  scale_x_date(date_breaks = "1 month") +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1), colour = "#05afff"),
        axis.title.x = element_text(colour = "#05afff"),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#05afff", face = "bold", hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "#05afff"),
        legend.key = element_rect(colour = "#05afff",fill = "black")) +
  ggtitle("2009 Daily Mean of Consumption")

# create new column with the weekday name for each row.
energy$weekday <- weekdays(energy$Date)

# create new column with each weekday as factor.
energy$day <- factor(energy$weekday, levels = c("Monday", "Tuesday", "Wednesday",
                                                "Thursday", "Friday", "Saturday",
                                                "Sunday"))
# new dataset grouped by day and the mean of the submeters for each group.
test5 <- energy %>% group_by(day) %>% summarise(Sub1mean = mean(Sub1, na.rm = T),
                                                Sub2mean = mean(Sub2, na.rm = T),
                                                Sub3mean = mean(Sub3, na.rm = T),
                                                Sub4mean = mean(Sub4, na.rm = T)) 
# plotting dataset test5, day per submeters.
ggplot(test5) +
  geom_point(aes(x = day, y = Sub1mean, color = "Sub1mean")) +
  geom_point(aes(x = day, y = Sub2mean, color = "Sub2mean")) +
  geom_point(aes(x = day, y = Sub3mean, color = "Sub3mean")) +
  geom_point(aes(x = day, y = Sub4mean, color = "Sub4mean")) +
  geom_line(aes(x = day, y = Sub4mean, color = "Sub4mean", group = 1)) +
  geom_line(aes(x = day, y = Sub3mean, color = "Sub3mean", group = 1)) +
  geom_line(aes(x = day, y = Sub2mean, color = "Sub2mean", group = 1)) +
  geom_line(aes(x = day, y = Sub1mean, color = "Sub1mean", group = 1)) +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1), colour = "#05afff"),
        axis.title.x = element_text(colour = "#05afff"),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#05afff", face = "bold", hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "#05afff"),
        legend.key = element_rect(colour = "#05afff",fill = "black")) +
  ggtitle("Weekly")

# new dataset grouped by time and filtered for the working days of the week 
# and the mean of the submeters for each group.
test6 <- energy %>% group_by(Time) %>%  filter(weekday == "Monday" | 
                                               weekday == "Tuesday"| 
                                               weekday == "Wednesday"|
                                               weekday == "Thursday"|
                                               weekday ==  "Friday") %>% 
                    summarise(Sub1mean = mean(Sub1, na.rm = T),
                              Sub2mean = mean(Sub2, na.rm = T),
                              Sub3mean = mean(Sub3, na.rm = T),
                              Sub4mean = mean(Sub4, na.rm = T))

# plotting dataset test6, time per submeters.
ggplot(test6) +
  geom_point(aes(x = Time, y = Sub1mean, color = "Sub1mean")) +
  geom_point(aes(x = Time, y = Sub2mean, color = "Sub2mean")) +
  geom_point(aes(x = Time, y = Sub3mean, color = "Sub3mean")) +
  geom_point(aes(x = Time, y = Sub4mean, color = "Sub4mean")) +
  scale_x_time(breaks = seq(0, 86400, by = 3600)) +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1), colour = "#05afff"),
        axis.title.x = element_text(colour = "#05afff"),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#05afff", face = "bold", hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "#05afff"),
        legend.key = element_rect(colour = "#05afff",fill = "black")) +
  ggtitle("Anual Working Days Mean of Energy Consumption")

# new dataset grouped by time and filtered for the weekend 
# and the mean of the submeters for each group.
test7 <- energy %>% group_by(Time) %>%  filter(weekday == "Saturday" | 
                                               weekday == "Sunday") %>% 
                    summarise(Sub1mean = mean(Sub1, na.rm = T),
                              Sub2mean = mean(Sub2, na.rm = T),
                              Sub3mean = mean(Sub3, na.rm = T),
                              Sub4mean = mean(Sub4, na.rm = T))

# plotting dataset test7, time per submeters.
ggplot(test7) +
  geom_point(aes(x = Time, y = Sub1mean, color = "Sub1mean")) +
  geom_point(aes(x = Time, y = Sub2mean, color = "Sub2mean")) +
  geom_point(aes(x = Time, y = Sub3mean, color = "Sub3mean")) +
  geom_point(aes(x = Time, y = Sub4mean, color = "Sub4mean")) +
  scale_x_time(breaks = seq(0, 86400, by = 3600)) +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1), colour = "#05afff"),
        axis.title.x = element_text(colour = "#05afff"),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#05afff", face = "bold", hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "#05afff"),
        legend.key = element_rect(colour = "#05afff",fill = "black")) +
  ggtitle("Anual Weekends Mean of Energy Consumption")

# new dataset for day 21/09/2008.
test8 <- energy %>%  filter(year(Date) == "2008", month(Date) == 9, day(Date) == 21)

# plotting dataset test8, time per submeters.
ggplot(test8) +
  geom_point(aes(x = Time, y = Sub1, color = "Sub1")) +
  geom_point(aes(x = Time, y = Sub2, color = "Sub2")) +
  geom_point(aes(x = Time, y = Sub3, color = "Sub3")) +
  geom_point(aes(x = Time, y = Sub4, color = "Sub4")) +
  scale_x_time(breaks = seq(0, 86400, by = 3600)) +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1), colour = "#05afff"),
        axis.title.x = element_text(colour = "#05afff"),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#05afff", face = "bold", hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "#05afff"),
        legend.key = element_rect(colour = "#05afff",fill = "black")) +
  ggtitle("Sunday 21/09/2008 Energy Consumption")

# new dataset for day 23/09/2008.
test9 <- energy %>%  filter(year(Date) == "2008", month(Date) == 9, day(Date) == 23)

# plotting dataset test9, time per submeters.
ggplot(test9) +
  geom_point(aes(x = Time, y = Sub1, color = "Sub1")) +
  geom_point(aes(x = Time, y = Sub2, color = "Sub2")) +
  geom_point(aes(x = Time, y = Sub3, color = "Sub3")) +
  geom_point(aes(x = Time, y = Sub4, color = "Sub4")) +
  scale_x_time(breaks = seq(0, 86400, by = 3600)) +
  scale_y_continuous(limit = c(0, 40)) +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                        colour = "light blue"),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1), colour = "#05afff"),
        axis.title.x = element_text(colour = "#05afff"),
        axis.title.y = element_blank(),
        plot.title = element_text(color = "#05afff", face = "bold", hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "#05afff"),
        legend.key = element_rect(colour = "#05afff",fill = "black")) +
  ggtitle("Tuesday 23/09/2008 Energy Consumption")

