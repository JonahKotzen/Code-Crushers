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


#Download pseed.met.rate and read it as a tibble
pseed.met.rate <- read_csv("pseed.met.rate.csv")

#Join the pseed.met.rate table with pseed.wide
pseed.final <- left_join(pseed.sum.max, pseed.met.rate, by="bl.s")%>%
  select(-fish.y)%>%
  rename("Fish"="fish.x")
  
view(pseed.final)
#use features to fnd max of amp.sums

ggplot(pseed.final,aes(x=met.rate,y=amp.sum.mean,col=Fish))+
  geom_line()+geom_point()
