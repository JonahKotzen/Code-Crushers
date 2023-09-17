library(tidyverse)
library(features)

#1) Combine code to establish pseed.wide
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

#2) Create a custom function that computes SE
se_mean <- function(x){
  se <- sd(x)/sqrt(length(x))
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
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=.1) +geom_point()


#6) Download pseed.met.rate and read it as a tibble
pseed.met.rate <- read_csv("pseed.met.rate.csv")

#Join the pseed.met.rate table with pseed.sum.max

pseed.final <- merge(x=pseed.sum.max,y=pseed.met.rate[c("date","met.rate")], by="date",all.x=TRUE)
  
view(pseed.final)

#7) Use ggplot to plot metabolic power output vs mean max of amp.sum
ggplot(pseed.final,aes(x=amp.sum.mean,y=met.rate,col=Fish))+geom_point()
