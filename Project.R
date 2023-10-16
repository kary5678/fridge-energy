#Clear Workspace & Set Up R
dev.off()         # remove any previous devices used to produce graphics or pdfs
cat("\014")       # clear all text in the console
rm(list=ls())     # clear out anything in memory
set.seed(18552)   # set a consistent seed for generating future variables

#Set a Working Directory
#setwd("~/Documents/School/Spring 21/DSCI 304")

#Load dataset and necessary libraries
#Data set from https://www.energystar.gov/productfinder/advanced
#Spark site link: https://spark.adobe.com/page/iReIdngvT2Lye/
fridges <- read.csv("ENERGY_STAR_Certified_Residential_Refrigerators.csv")
library(ggplot2)
library(dplyr)
library(stargazer)
library(ggeffects)
library(interactions)
library(reshape2)

library(imager)
library(ggimage)
library(magick)
library(tidyverse)
library(gganimate)
library(png)
library(gapminder)
library(gifski)
library(av)

library(bnlearn)
library(gplots)
library(devtools)
library(broom)
library(lsmeans)
library(psych)
library(cowplot)
library(ggpubr)
library(dplyr)
library(htmlwidgets)
library(plotly)

#clean variables
fridges$Defrost.Type <- as.factor(fridges$Defrost.Type)
fridges$Compact <- as.factor(fridges$Compact)
fridges$Built.in <- as.factor(fridges$Built.in)
fridges$Thru.the.Door.Dispenser <- as.factor(fridges$Thru.the.Door.Dispenser)
fridges$Ice.Maker <- as.factor(fridges$Ice.Maker)
fridges$Counter.Depth <- as.factor(fridges$Counter.Depth)
fridges$Connected.Functionality <- as.factor(fridges$Connected.Functionality)
fridges$Date.Certified <- as.Date(fridges$Date.Certified, "%m/%d/%Y")
fridges$Date.Available.On.Market <- as.Date(fridges$Date.Available.On.Market, "%m/%d/%Y")

#### 1: histogram of top 25 companies by number of models they have on the market
#find number of models per brand to identify the top 25
fridge_brands <- fridges %>% 
  group_by(Brand.Name) %>%
  summarise(num_models = n()) %>%
  arrange(desc(num_models))
ggplot(data=merge(fridge_brands[1:25,], fridges), aes(reorder(x=Brand.Name, num_models)))+
  geom_histogram(stat="count", aes(fill=Meets.ENERGY.STAR.Most.Efficient.2021.Criteria))+
  coord_flip(ylim=c(0,200), expand=FALSE)+
  labs(title="Top 25 Refridgerator Companies by Number of Models", 
       x="Brand Name", y="Number of Models on the Market", fill="Energy Star 2021 Criteria")+
  scale_fill_manual(labels=c("Yes"="Criteria Met","No"="Criteria Not Met"), 
                    values=c("Yes"="#1fbbec", "No"="coral"))
#+geom_text(aes(label=num_models, y=num_models), hjust=-.20)+
 # theme(axis.text.x=element_blank(), axis.ticks.x = element_blank())


### 2: distribution of energy usage variable
#two density curves (one for the fridges, one for their federal standards)
plot(density(fridges$Annual.Energy.Use..kWh.yr.), xlim=c(0, 1000), col="coral", lwd=2, 
     main="Fridge Energy Usage & Federal Standards Distributions", xlab = "Average Annual Energy Use (kWh/yr)")
#polygon(density(fridges$Annual.Energy.Use..kWh.yr), col="steelblue")
lines(density(fridges$US.Federal.Standard..kWh.yr.), lwd=2, col="#1fbbec")
legend(x="right",legend=c("Refrigerator", "Federal Standard"), col = c("coral", "#1fbbec"),
       lty = c(1,1), lwd=2, bty="n")

#boxplot of fridge annual energy consumption
boxplot(fridges$Annual.Energy.Use..kWh.yr., main="Average Energy Usage of Residential Refrigerators",
        horizontal = TRUE, xlab="Average Annual Energy Use (kWh/yr)", col="coral")


### 3: Substantive Effects Plot for width & height on energy
lin_model <- lm(Annual.Energy.Use..kWh.yr. ~ Height..in. + Width..in. +
                  Defrost.Type + Ice.Maker+Connected.Functionality, data=fridges)
summary(lin_model)
p1 <- ggpredict(lin_model, "Height..in.")
p1f <- plot(p1)+
  ggtitle("Effect of Fridge Height on Energy Consumption")+
  labs(y= "Predicted Annual Energy Use (kWh/yr)", x = "Height of Fridge (inches)")

p2 <- ggpredict(lin_model, "Width..in.")
p2f <- plot(p2)+
  ggtitle("Effect of Fridge Width on Energy Consumption")+
  labs(y= "Predicted Annual Energy Use (kWh/yr)", x = "Width of Fridge (inches)")

ggarrange(p1f, p2f, ncol = 1, nrow = 2)

stargazer(lin_model, type="text")


### 4: Interaction Plots for fridge volume & four different features
model_defrost_orig <- lm(Annual.Energy.Use..kWh.yr. ~ Adjusted.Volume..ft3. + Defrost.Type, data=fridges)
model_defrost <- lm(Annual.Energy.Use..kWh.yr. ~ Adjusted.Volume..ft3. + Defrost.Type + 
                      Adjusted.Volume..ft3.*Defrost.Type, data=fridges)
stargazer(model_defrost_orig, model_defrost, type="text")
ip1 <- interact_plot(model_defrost, pred=Adjusted.Volume..ft3., modx=Defrost.Type, interval=TRUE, 
              main="Interactive Effect of Fridge Volume and\nDefrosting Method on Energy Consumption",
              x.label = "Fridge Volume (Adjusted ft³)", y.label = "Average Annual Energy Use (kWh/yr)", 
              legend.main = "Defrost Method", colors=c("red", "red"))


model_ice_orig <- lm(Annual.Energy.Use..kWh.yr. ~ Adjusted.Volume..ft3. + Ice.Maker, data=fridges)
model_ice <- lm(Annual.Energy.Use..kWh.yr. ~ Adjusted.Volume..ft3. + Ice.Maker + 
                  Adjusted.Volume..ft3.*Ice.Maker, data=fridges)
stargazer(model_ice_orig, model_ice, type="text")
ip2 <- interact_plot(model_ice, pred=Adjusted.Volume..ft3., modx=Ice.Maker, interval=TRUE, 
              main="Interactive Effect of Fridge Volume and\nIce Making Ability on Energy Consumption",
              x.label = "Fridge Volume (Adjusted ft³)", y.label = "Average Annual Energy Use (kWh/yr)", 
              legend.main = "Does the\nfridge have an\nice maker?", colors=c("blue2", "blue2"))


model_functionality_orig <- lm(Annual.Energy.Use..kWh.yr. ~ Adjusted.Volume..ft3. + Connected.Functionality,
                               data=fridges)
model_functionality <- lm(Annual.Energy.Use..kWh.yr. ~ Adjusted.Volume..ft3. + Connected.Functionality + 
                            Adjusted.Volume..ft3.*Connected.Functionality, data=fridges)
stargazer(model_functionality_orig, model_functionality, type="text")
ip3 <- interact_plot(model_functionality, pred=Adjusted.Volume..ft3., modx=Connected.Functionality, interval=TRUE, 
              main="Interactive Effect of Fridge Volume and\nConnected Functionalities on Energy Consumption",
              x.label = "Fridge Volume (Adjusted ft³)", y.label = "Average Annual Energy Use (kWh/yr)", 
              legend.main = "Does the fridge\nhave connected\nfunctionalities?", colors=c("seagreen", "seagreen"))


model_compact_orig <- lm(Annual.Energy.Use..kWh.yr. ~ Adjusted.Volume..ft3. + Compact, data=fridges)
model_compact <- lm(Annual.Energy.Use..kWh.yr. ~ Adjusted.Volume..ft3. + Compact + 
                      Adjusted.Volume..ft3.*Compact, data=fridges)
stargazer(model_compact_orig, model_compact, type="text")
ip4 <- interact_plot(model_compact, pred=Adjusted.Volume..ft3., modx=Compact, interval=TRUE, 
              main="Interactive Effect of Fridge Volume and\nCompact Designs on Energy Consumption",
              x.label = "Fridge Volume (Adjusted ft³)", y.label = "Average Annual Energy Use (kWh/yr)", 
              legend.main = "Is the fridge\nCompact?", colors=c("orange", "orange"))

ggarrange(ip1, ip2, ip3, ip4, ncol = 2, nrow = 2)

##make nicer stargazer tables to include
fridges$`Annual Energy Use` <- fridges$Annual.Energy.Use..kWh.yr.
fridges$Volume <- fridges$Adjusted.Volume..ft3.
fridges$Defrost <- fridges$Defrost.Type
fridges$ConnectedFunctions <- fridges$Connected.Functionality

model_defrost_orig <- lm(`Annual Energy Use` ~ Volume + Defrost, data=fridges)
model_defrost <- lm(`Annual Energy Use` ~ Volume + Defrost + Volume*Defrost, data=fridges)
stargazer(model_defrost_orig, model_defrost, type="text")

model_ice_orig <- lm(`Annual Energy Use` ~ Volume + Ice.Maker, data=fridges)
model_ice <- lm(`Annual Energy Use` ~ Volume + Ice.Maker + Volume*Ice.Maker, data=fridges)
stargazer(model_ice_orig, model_ice, type="text")

model_functionality_orig <- lm(`Annual Energy Use` ~ Volume + ConnectedFunctions, data=fridges)
model_functionality <- lm(`Annual Energy Use` ~ Volume + ConnectedFunctions + Volume*ConnectedFunctions, data=fridges)
stargazer(model_functionality_orig, model_functionality, type="text")

model_compact_orig <- lm(`Annual Energy Use` ~ Volume + Compact, data=fridges)
model_compact <-  lm(`Annual Energy Use` ~ Volume + Compact + Volume*Compact, data=fridges)
stargazer(model_compact_orig, model_compact, type="text")


### 5: Interactive Visual: Annual Energy Use Over Time by Type of Fridge
fridges$Year <- format(fridges$Date.Certified, format="%Y")
#make hovering labels look nicer
fridges$Model <- fridges$Model.Number
fridges$`Date Certified` <- fridges$Date.Certified
fridges$`Energy Usage (kWh/yr)` <- fridges$Annual.Energy.Use..kWh.yr.
#only 1 observation of upright freezer, only one observation pre 2013, so remove these
type_plot <- ggplot(subset(fridges, Type!="Upright Freezer" & Year>=2013), 
                    aes(x=Date.Certified, y=Annual.Energy.Use..kWh.yr., col=Type, frame=Type))+
  #geom_point(aes(text=Brand.Name, text2=Model.Number, text3=Date.Available.On.Market, text4=Annual.Energy.Use..kWh.yr.))+
  geom_point(aes(text=Brand.Name, text2=Model, text3=`Date Certified`, text4=`Energy Usage (kWh/yr)`))+
  labs(title="Energy Consumption of Residential Refrigerators by Type",
       x="Year", y="Average Annual Energy Use (kWh/yr)")+
  theme(legend.position="none", plot.margin = margin(0, 1, 0, 0, "cm"))

ggplotly(type_plot, tooltip = c("text", "text2", "text3", "text4"))

### 6: Animated histogram, fridge meeting 2014 vs 2021 energy standards
#first manipulate data to have transition state (criteria) in one variable column
fridges$`2014 Criteria` <- factor(ifelse(fridges$Percent.Less.Energy.Use.than.US.Federal.Standard>=0, yes=1, no=0),
                                  labels = c("Not Met", "Met"))
fridges$`2021 Criteria` <- factor(ifelse(fridges$Meets.ENERGY.STAR.Most.Efficient.2021.Criteria=="Yes", yes=1, no=0), 
                                  labels = c("Not Met", "Met"))

meltEnergy <- melt(subset(fridges, Year>=2014, select=c(Year, `2014 Criteria`, `2021 Criteria`)), id.vars = c("Year"), 
                 variable.name = "Criteria", value.name = "Criterion Met")
meltEnergy$`Criterion Met` <- factor(meltEnergy$`Criterion Met`, levels=c("Not Met", "Met"))

criteria_plot <- ggplot(data=meltEnergy, aes(x=Year))+
  geom_histogram(stat="count", aes(fill=`Criterion Met`))+
  labs(title="Histogram of Fridges Meeting Energy Star Criterion", 
       x="Certification Year", y="Number of Fridge Models", fill='{closest_state}')+
  scale_fill_manual(values=c("Met"="#1fbbec", "Not Met"="#edae49"))  
criteria_plot_anim <- criteria_plot+transition_states(Criteria)+shadow_mark()
criteria_anim_file < -animate(criteria_plot_anim, renderer=av_renderer(), fps=7, res=100, rewind=TRUE, end_pause=15, duration=15)
anim_save("~/Downloads/CriteriaPlot2.mp4",animation=criteria_anim_file, renderer=av_renderer())

#moving histogram 2 main frames for easier analysis
ggplot(data=subset(fridges, Year>=2014), aes(x=Year))+
  geom_histogram(stat="count", aes(fill=`2014 Criteria`))+
  labs(title="Histogram of Fridges Meeting Energy Star Criterion", 
       x="Certification Year", y="Number of Fridge Models", fill='2014 Criteria')+
  scale_fill_manual(values=c("Met"="#1fbbec", "Not Met"="#edae49")) 

ggplot(data=subset(fridges, Year>=2014), aes(x=Year))+
  geom_histogram(stat="count", aes(fill=`2021 Criteria`))+
  labs(title="Histogram of Fridges Meeting Energy Star Criterion", 
       x="Certification Year", y="Number of Fridge Models", fill='2021 Criteria')+
  scale_fill_manual(values=c("Met"="#1fbbec", "Not Met"="#edae49")) 

### 7: Box plot of Fridges Width and Heights By Type
box1 <- ggplot(subset(fridges, Type!="Upright Freezer"), aes(x=Type, y=Height..in.))+
  geom_boxplot(aes(color=Type))+
  labs(title="Average Height of Fridges by Type",
       x="Fridge Type", y="Height of Fridge (inches)")+
  theme_minimal()+
  theme(legend.position = "none")

box2 <- ggplot(subset(fridges, Type!="Upright Freezer"), aes(x=Type, y=Width..in.))+
  geom_boxplot(aes(color=Type))+
  labs(title="Average Width of Fridges by Type",
       x="Fridge Type", y="Width of Fridge (inches)")+
  theme_minimal()+
  theme(legend.position = "none")

ggarrange(box1, box2, ncol=1)

### 8: Animated line plot: percent less energy use by certification year + type of fridge
#find averages for each type of fridge per certification year
efficient_fridges <- fridges %>% group_by(Year, Type) %>%
  summarise(count = n(),
            mean_perc = mean(Percent.Less.Energy.Use.than.US.Federal.Standard))
#remove lonely points like earlier for the bigger picture
efficient_fridges <- subset(efficient_fridges, Type!="Upright Freezer" & Year>=2013)
efficient_fridges$Year <- as.numeric(efficient_fridges$Year)

year_plot <- ggplot(efficient_fridges, aes(x=Year, y=mean_perc))+
  #geom_point(aes(color=Type))+
  geom_line(aes(group=Type, color=Type))+
  scale_x_continuous(breaks=seq(2013,2021,2))+
  ylim(0,20)+
  labs(title="Energy Consumption of Residential Refrigerators by Type",
       x="Certification Year", y="Avg. Percent Energy Less Than Federal Standard (%)", color="Fridge Type")

year_plot_anim <- year_plot+transition_reveal(Year)+shadow_mark()
#animate(year_plot_anim, renderer=gifski_renderer(), height = 4, width = 6, units = "in", res = 150, end_pause=20)
year_anim_file<-animate(year_plot_anim, renderer=av_renderer(),
                            height = 4, width = 6, units = "in", res = 150, end_pause=20)
anim_save("~/Downloads/YearPlot.mp4",animation=year_anim_file, renderer=av_renderer())
