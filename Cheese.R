## HEATHER ####
## WHO: <Salvador Prats Aparicio>
## WHAT: Data Visualization and Analysis Module, Dair Production and Consumption Block
## Last edited: 11/03/2021
####
## CONTENTS ####
## 1. Getting the data
## 2. Installing the packages
## 3. Getting the libraries
## PLOT 1. Average cheese comsuption per person
## PLOT 2. Differences in diary products consumptions through the years
## PLOT 3. Trends on Whole and Low Fat Milk in late years
## PLOT 4. Price Fluctuations of Milk in the EEUU

## 1. Getting the data
library(readxl)
cheese <- read_excel("VADOR/MASTER/MRES/DVA/cheese.xlsx")
diaryproducts <- read_excel("VADOR/MASTER/MRES/DVA/diaryproducts.xlsx")
cheese1 <- read_excel("VADOR/MASTER/MRES/DVA/cheese1.xlsx")

## 2. Installing the packages
install.packages("ggalt")

## 3. Getting the libraries
library(ggplot2)
library(ggalt)
library(plotly)

## PLOT 1. Average cheese comsuption per person
# How much is the average of cheese consumption out of a 12lb pounds as a reference?

#plotting a pie graph for each of the kinds of cheese
pie(cheese$Cheddar, main="Cheddar")

pie(cheese$`American Other`, main="American Other")

pie(cheese$`Italian other`, main="Italian other")

pie(cheese$Mozzarella, main="Mozzarella")

pie(cheese$Swiss, main="Swiss")

pie(cheese$Brick, main="Brick")

pie(cheese$Muenster, main="Muenster")

pie(cheese$`Cream and Neufchatel`, main="Cream")

pie(cheese$Blue, main="Blue")


## PLOT 2. Differences in diary products consumptions through the years
# Comparative on the average consumption of dairy products on 1975 and 2017

#for right ordering of the dumbells
diaryproducts$Product <- factor(diaryproducts$Product, 
                                levels=as.character(diaryproducts$Product))

#plotting
gg <- ggplot(diaryproducts, aes(x=Y_1975, xend=Y_2017, y=Product, group=Product)) + 
  geom_dumbbell(color="#0e668b", 
                size=0.75) + 
  scale_x_continuous() + 
  labs(x=NULL, 
       y=NULL, 
       title="Time passes for everyone, even milk", 
       subtitle="Differences in diary products consumptions 1975 - 2017") +
  theme(plot.title = element_text(hjust=0, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)


## PLOT 3. Trends on Whole and Low Fat Milk in late years
# Are we getting scared of fat lately? (ANIMATED)

#rangling data
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])})
  dplyr::bind_rows(dats)}

#setting the accumulation
df <- cheese1 
Ploot2 <- df %>%
  filter(year > 1880, cheese1$milk_type %in% c("Whole", "Low Fat"))
Ploot2 <- Ploot2 %>% accumulate_by(~year)

#plotting and animating
Ploot2 <- Ploot2 %>%
  plot_ly(
    x = ~year, 
    y = ~pounds,
    split = ~milk_type,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F))
Ploot2 <- Ploot2 %>% layout(
  xaxis = list(
    title = "Year",
    zeroline = F),
  yaxis = list(
    title = "Pounds",
    zeroline = F))
Ploot2 <- Ploot2 %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE)
Ploot2 <- Ploot2 %>% animation_slider(hide = T)
Ploot2 <- Ploot2 %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom")

Ploot2


## PLOT 4. Price Fluctuations of Milk in the EEUU

ggplot(cheese1, aes(x=year, y=avg_price_milk)) + 
  geom_line(aes(y=avg_price_milk), col="#F8766D") + 
  labs(title="No One Can Scape Capitalism, and Milk isn't an Exeption", 
       subtitle="Price Fluctuations of Milk in the EEUU", 
       y="Average Price of Milk")
