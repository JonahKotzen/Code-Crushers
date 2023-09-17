library(tidyverse)
library(features)

#combine code to establish pseed.wide
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

#Create a custom function that computes SE

#x and y values, amplitude and timeframe, pass info to features function
#will only take peaks for own tibble, pass amplitude data and summarize
#group by fish, experiment, fin
#use values of those arguments in brackets to do operations on those values
se_mean <- function(amp.sum){
  se <- sd(amp.sum)/sqrt(length(amp.sum))
}

pseed.sum.max <- pseed.wide%>%
  group_by(fish,speed,bl.s)%>%
  summarize(
    amp.sum.mean=mean(amp.sum,na.rm=TRUE),
    amp.sum.se=se_mean(amp.sum))


ggplot(pseed.sum.max,aes(x=speed,y=amp.sum.mean,col=fish)) +
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=.1) +
  geom_line()+geom_point()

<<<<<<< HEAD
=======

#need to find the peaks since these are the max amps
#amp.sum=amp of both fins, group_by(fish, speed)
#will have to compute amplitude of cycles in each experiment
#6 look for common variable names, look at table
  #fish, date, speeds and body lengths are common variable
#use features to fnd max of amp.sums
>>>>>>> 462ff0fb63c688811f63d4666a1022ea2d3da537

#Download pseed.met.rate and read it as a tibble
pseed.met.rate <- read_csv("pseed.met.rate.csv")

<<<<<<< HEAD
#Join the pseed.met.rate table with pseed.wide
=======
>>>>>>> 462ff0fb63c688811f63d4666a1022ea2d3da537
pseed.final <- left_join(pseed.sum.max, pseed.met.rate, by="bl.s")%>%
  select(-fish.y)%>%
  rename("Fish"="fish.x")
  
view(pseed.final)
#use features to fnd max of amp.sums
<<<<<<< HEAD

ggplot(pseed.final,aes(x=met.rate,y=amp.sum.mean,col=Fish))+
  geom_line()+geom_point()
=======
>>>>>>> 462ff0fb63c688811f63d4666a1022ea2d3da537
