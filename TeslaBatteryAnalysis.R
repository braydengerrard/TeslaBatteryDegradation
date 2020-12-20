library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(scales)
options(scipen=1000)


Tesla <- read.csv("TeslaBattery.csv")
names(Tesla)[1] <- "Entry_Date"
Tesla$Entry_Date <- as.Date(Tesla$Entry_Date, format="%d %b %Y")

Tesla$Vehicle.model <- ifelse(grepl("Model 3", Tesla$Vehicle.model), "Model 3",
                              ifelse(grepl("Model S", Tesla$Vehicle.model), "Model S",
                                     ifelse(grepl("Model X", Tesla$Vehicle.model), "Model X", "Other")))
Tesla <- subset(Tesla, Vehicle.model %in% c("Model 3","Model S","Model X"))
names(Tesla)[20] <- "Remaining"
Tesla$Remaining <- gsub("%","",Tesla$Remaining)
Tesla$Remaining <- as.numeric(Tesla$Remaining)
names(Tesla)[22] <- "MKM"
Tesla$MKM <- gsub(",","",Tesla$MKM)
Tesla$MKM <- gsub("km","",Tesla$MKM)
Tesla$MKM <- as.numeric(Tesla$MKM)

Tesla$Remaining <- Tesla$Remaining/100

names(Tesla)[14] <- "Replaced"
Tesla <- Tesla[is.na(Tesla$Replaced),]

ggplot(Tesla, aes(x=MKM, y=Remaining)) +
  geom_point(color="cyan3") +
  stat_smooth(method = "gam", formula = y ~ s(x), size = 1, fill = "deepskyblue") +
  theme_ipsum_rc(grid="Y") +
  geom_vline(xintercept=244840, linetype="longdash", color="red", size=1) +
  annotate("text", x = 248000, y = 0.6, 
           label = 'atop(bold("Average Vehicle"),bold("Lifetime"))',
           hjust=0,
           colour = "red", parse = TRUE,
           size=4.25) +
  labs(title = "Battery Degradation In Tesla Electric Vehicles",
       y = "Capacity Remaining",
       x = "Kilometers Travelled",
       caption = "Source: Dutch-Belgium Tesla Forum, NHTSA") +
  scale_y_continuous(labels = scales::percent,
                     limits=c(0.5,1.03)) +
  scale_x_continuous(label = comma)
