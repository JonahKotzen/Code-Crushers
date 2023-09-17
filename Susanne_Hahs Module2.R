#Establishing data
library(tidyverse)
library(dplyr)
library(features)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds, by=c("speed"="vol")) %>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl) %>%
  print()

pseed.wide <- pseed2 %>%
  select(-amp) %>%
  pivot_wider(names_from=fin, values_from=amp.bl)%>%
  mutate(amp.sum=L+R)%>%
  view()

#Create custom function to compute the standard error of the mean (SE)
se_mean <- function(x){
  se <- sd(x)/sqrt(length(x))
}

#Compute max. and find mean max of amp.sum values
find.maxes <- function(x,y,mult=100){
  f <- fget(features(x=x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peak=round(crit.pts,0))
  return(f$peak)
}
pseed.peaks <- pseed.wide%>%
  group_by(date,fish)%>%
  mutate(peak=frame%in% find.maxes(frame,amp.sum))%>%
  filter(peak==T)

#Add amp.sum.se values
pseed.sum.max <- pseed.peaks%>%
  group_by(fish,speed,bl.s)%>%
  summarize(
    amp.sum.mean=mean(amp.sum,na.rm=TRUE),
    amp.sum.se=se_mean(amp.sum))
#Plot mean amp.sum vs. specific swimming speed with error bars
ggplot(pseed.sum.max,aes(x=bl.s,y=amp.sum.mean,col=fish)) +
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=.1) +geom_point()

#Download and read as tibble 
pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.final <- left_join(pseed.sum.max, pseed.met.rate, by="bl.s")%>%
  select(-fish.y)%>%
  rename("Fish"="fish.x")

#Use ggplot to plot metabolic power output vs. mean max of amp.sum
ggplot(merged_data, aes(x = amp.sum.mean, y = met.rate, color = fish)) +
  geom_point() +
  labs(x = "Mean amp.sum", y = "Metabolic Power Output", title = "Metabolic Power vs. Mean amp.sum")
