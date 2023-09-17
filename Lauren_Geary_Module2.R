library(tidyverse)
library(dplyr)

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
compute_se<-function(amp.sum){
  se <- sd(amp.sum) / sqrt(length(amp.sum))
  return(se)
}

#3-#4: Compute the mean maximum of all amp.sums across all fin-beat cycles
pseed.sum.max <- pseed.wide%>%
  group_by(fish,speed,bl.s)%>%
  summarize(
    amp.sum.mean=mean(amp.sum,na.rm=TRUE),
    amp.sum.se=compute_se(amp.sum))


#5: Plot the mean amp.sum vs. specific swimming speed and add error bars that correspond to the SE of amp.sum
ggplot(pseed.sum.max, aes(x = speed, y = amp.sum.mean, color = fish)) +
  geom_point() +
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), width = 0.2) +
  labs(x = "Specific Swimming Speed", y = "Mean amp.sum", title = "Mean amp.sum vs. Swimming Speed")

#6: Download pseed.met.rate and merge with new pseed.sum.max tibble
pseed.met.rate<-read_csv("pseed.met.rate.csv")
pseed.final <- left_join(pseed.sum.max, pseed.met.rate, by="bl.s")%>%
  select(-fish.y)%>%
  rename("Fish"="fish.x")

view(pseed.final)

#7: Plot metabolic power output vs. mean maximum of amp.sum
ggplot(merged_data, aes(x = amp.sum.mean, y = met.rate, color = fish)) +
  geom_point() +
  labs(x = "Mean amp.sum", y = "Metabolic Power Output", title = "Metabolic Power vs. Mean amp.sum")

