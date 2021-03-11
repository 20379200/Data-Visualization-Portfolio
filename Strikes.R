## HEATHER ####
## WHO: <Salvador Prats Aparicio>
## WHAT: Data Visualization and Analysis Module, Hymalaian Expeditions Block
## Last edited: 11/03/2021
####
## CONTENTS ####
## 1. Getting the data
## 2. Installing the packages
## 3. Getting the libraries
## PLOT 1. Scatterplot of height vs speed of the plane splitted by damage to the wildlife
## PLOT 2. Barplot of pilot reports by Company
## PLOT 3. Height of incidents for different taxons along the day
## PLOT 4. Race barplot of species strikes through the years


## 1. Getting the data
library(readxl)
Strikes2 <- read_excel("VADOR/MASTER/MRES/DVA/Strikes2.xlsx")
Strikes3 <- read_excel("VADOR/MASTER/MRES/DVA/Strikes3.xlsx")
Strikes4 <- read_excel("VADOR/MASTER/MRES/DVA/Strikes4.xlsx")
Strikes6 <- read_excel("VADOR/MASTER/MRES/DVA/Strikes6.xlsx")

## 2. Intalling needed packages
install.packages("ggthemes")
install.packages("gifski")

## 3. Getting the libraries
library(ggplot2)
library(ggthemes)
library(forecast)
library(gganimate)

## PLOT 1. Scatterplot of height vs speed of the plane splitted by damage to the wildlife

#Checking the data
class(Strikes2$damage) #Damage is a character
Damage <- as.factor(Strikes2$damage) #Turning Damage to a factor
class(Damage) 

#Plotting
Eplot <- ggplot(Strikes2, aes(x=speed, y=height)) + 
  geom_point(aes(col=Damage)) + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="When Planes Attack", 
       subtitle="Wildlife Impact Survival depending on plane's height and speed",
       x="Speed (km/h)",
       y="Height (m)")
Eplot #just checking


## PLOT 2. Barplot of pilot reports by Company

#Creating a frequency table
freqtable <- table(Strikes3$operator)
df <- as.data.frame.table(freqtable)
head(df)

#plotting
ggplot(df, aes(x=reorder(Var1, Freq), y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(title="Bar Chart", 
       subtitle="Operator strikes",
       x="Operator",
       y="Num of reported strikes") +
  theme(legend.position="none") + theme_tufte()


## PLOT 3. Height of incidents for different taxons along the day

g <- ggplot(Strikes3, aes(x=species, y=height))
g + geom_count(aes(col=time_of_day), show.legend=TRUE) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(subtitle="Height of incidents for different taxons along the day", 
       y="Height (m)", 
       x="Taxon", 
       title="Are you an Early bird or a Night Owl?")


## PLOT 4. Race barplot of species strikes through the years

#forcing the variables
Num_of_cases <- as.numeric(Strikes6$Num_of_cases)
Species <- as.factor(Strikes6$Species)

#creating a top10 rank for each year
Strikes_formatted <- Strikes6 %>%
  group_by(Year) %>% 
  mutate(rank = rank(-Num_of_cases),
         Value_rel = Num_of_cases/Num_of_cases[rank==1],
         Value_lbl = paste0(" ",round(Num_of_cases/1e9))) %>%
  group_by(Species) %>%
  filter(rank <=10) %>%
  ungroup()

#plotting every barplot for each frame (one for each year)
hell_plot <- ggplot(Strikes_formatted, aes(rank, group = Species,
                                           fill = as.factor(Species), color = as.factor(Species))) +
  geom_tile(aes(y = Num_of_cases/2,
                height = Num_of_cases,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Species, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Species,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) + 
  transition_states(Year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Species per Year',
       subtitle  =  "Top 10 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data")

#animating it and saving
animate(hell_plot, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"), end_pause = 15, start_pause =  15)