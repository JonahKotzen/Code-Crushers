#Establishing data
library(tidyverse)
library(dplyr)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl) %>%
  print()

pseed.wide <- pseed2 %>%
  select(-amp) %>%
  pivot_wider(names_from=fin, values_from=amp.bl)%>%
  mutate(amp.sum=L+R)%>%
  print()

#Create custom function to compute the standard error of the mean (SE)
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


#Compute mean maximum of all amp.sum for each swimming speed and fish 
pseed.sum.max <- pseed.wide %>%
  group_by(speed, fish,bl.s) %>%
  summarise(amp.sum.mean = mean(amp.sum, na.rm = TRUE),amp.sum.se = compute_se(amp.sum)) %>%
  print()



#Plot mean amp.sum vs specific swimmming speed and add error bars
ggplot(pseed.sum.max, aes(x = speed, y = amp.sum.mean, color = fish)) +
  geom_point() +
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), width = 0.1,) +
  labs(x = "Specific Swimming Speed", y = "Mean amp.sum", title = "Mean amp.sum vs. Swimming Speed")




#Download file as tibble and merge with pseed.sum.max
pseed.met.rate<-read_csv("pseed.met.rate.csv")
pseed.final <- left_join(pseed.sum.max, pseed.met.rate, by="bl.s")%>%
  select(-fish.y)%>%
  rename("Fish"="fish.x")

view(pseed.final)



#Plot metabolic power output vs. mean maximum of amp.sum 
ggplot(merged_data, aes(x = amp.sum.mean, y = met.rate, color = fish)) +
  geom_point() +
  labs(x = "Mean amp.sum", y = "Metabolic Power Output", title = "Metabolic Power vs. Mean amp.sum")