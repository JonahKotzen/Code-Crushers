library(tidyverse)
library(dplyr)
library(features)

#load data to establish pseed.wide
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#1: Establish pseed.wide data tibble
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print()

#2Custom function that computes Standard Error of the Mean (SE)
compute_se<-function(x){
  se <- sd(x) / sqrt(length(x))
  return(se)
}

#3) Determine the maxes and then find the mean max of amp.sum values
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

#4) Add the amp.sum.se values (this is also the code that adds amp.sum.max)
pseed.sum.max <- pseed.peaks%>%
  group_by(fish,speed,bl.s)%>%
  summarize(
    amp.sum.mean=mean(amp.sum,na.rm=TRUE),
    amp.sum.se=se_mean(amp.sum))

#5) Plot the mean amp.sum vs specific swimming speed with error bars
ggplot(pseed.sum.max,aes(x=bl.s,y=amp.sum.mean,col=fish)) +
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=.1) +geom_point() +
  labs(x = "Specific Swimming Speed", y="Mean amp.sum", title = "Mean amp.sum vs. Swimming Speed")

#6: Download pseed.met.rate and merge with new pseed.sum.max tibble
pseed.met.rate<-read_csv("pseed.met.rate.csv")
pseed.final <- left_join(pseed.sum.max, pseed.met.rate, by="bl.s")%>%
  select(-fish.y)%>%
  rename("Fish"="fish.x")

view(pseed.final)

#7: Plot metabolic power output vs. mean maximum of amp.sum
pseed.met.rate2%>%
  ggplot(aes(x=amp.sum.mean, y=met.rate, colour = fish))+geom_point()
  labs(x = "Mean amp.sum", y = "Metabolic Power Output", title = "Metabolic Power vs. Mean amp.sum")

