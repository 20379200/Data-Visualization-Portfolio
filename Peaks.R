## HEATHER ####
##
##
## Last edited: 11/03/2021
####
## CONTENTS ####
## 1. Getting the data
## 2. Getting the libraries
## PLOT 1. Death Height in different peaks
## PLOT 2. Citizenship of climbers
## PLOT 3. Elevation highpoint depending on season
## PLOT 4. Elevation highpoint through the years


## 1. Getting the data
library(readxl)
Peaks1 <- read_excel("VADOR/MASTER/MRES/DVA/Peaks1.xlsx")
Peaks2 <- read_excel("VADOR/MASTER/MRES/DVA/Peaks2.xlsx")
Peaks3 <- read_excel("VADOR/MASTER/MRES/DVA/Peaks3.xlsx")

## 2. Getting the libraries for all plots
library(ggthemes)
library(ggplot2)
library(plotly)
library(tidyverse)

## PLOT 1. Death Height in different peaks
# Dotplot x = name of peaks, y = elevation where members of expeditions died
ggplot(Peaks1, aes(x=peak_name, y=death_height_metres)) + 
  geom_point(col="red") + 
  theme_tufte() +
  labs(title="Death Lurks in the Heights", 
       subtitle="Elevation of Deadly Accidents in the Himalaya (1905-2001)",
       x="Peak name",
       y="High of death (m)")


## PLOT 2. Citizenship of climbers
# Barplot of the citizenships of the members in expeditions
counts <- table(Peaks1$citizenship)
counts1 <- counts[which(counts > 20)] 
barplot(counts1, col="thistle",
        main="Top10 citizenships among trekkers",
        xlab="Citizenship",
        ylab="Nº of trekkers")


## PLOT 3. Elevation highpoint depending on season
# When is more feasible to go to the top?
# Can the trekkers get to higher elevations during a specific season?

#changing from character to factor
season <- as.factor(Peaks2$season)
class(season) #just checking

#plotting
ggplot(Peaks2, aes(x=peak_name, y=highpoint_meters),) + 
  geom_point(aes(col=season)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="How high can you get?", 
       subtitle="Maximum height of the expeditions by season",
       x="Peak name",
       y="Elevation hightpoint")
#Spoiler: No. The Season doesn't matter on how hight they get


## PLOT 4. Elevation highpoints through the years
# Have trekkers climbed higher as time passed? (INTERACTIVE)

#Preparing the data
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])})
  dplyr::bind_rows(dats)}

df <- Peaks3

df <- df %>% accumulate_by(~year)

df$highpoint_metres <- as.numeric(df$highpoint_metres)

#plotting
p <- ggplot(df, aes(x = year, y = highpoint_metres, frame = frame)) +
  geom_point(col="grey") +
  scale_y_continuous(limits = c(3500, 9000), 
                     breaks = seq(3500, 9000, 500))

p #just checking

#animating
fig <- ggplotly(p) %>%
  layout(title = "Elevation of expeditions through the years", 
         yaxis = list(
    title = "Summit highpoint (metres)",
    zeroline = F, tickprefix = "$"),
    xaxis = list(title = "Year of ascent", 
                 zeroline = F, 
                 showgrid = F)) %>% 
  animation_opts(frame = 100, 
                 transition = 0, 
                 redraw = FALSE) %>%
  animation_slider(currentvalue = list(prefix = "Year"))

fig #just checking
