#this is where the code for the project report is
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
  group_by(fish,speed)%>%
  summarize(
    amp.sum.mean=mean(amp.sum,na.rm=TRUE),
    amp.sum.se=se_mean(amp.sum))

view(pseed.sum.max)
#need to find the peaks since these are the max amps
#amp.sum=amp of both fins, group_by(fish, speed)
#will have to compute amplitude of cycles in each experiment
#6 look for common variable names, look at table
  #fish, date, speeds and body lengths are common variable
#use features to fnd max of amp.sums