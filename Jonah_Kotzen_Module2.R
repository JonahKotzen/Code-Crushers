#---establish beginning---#
library(tidyverse)
library(dplyr)
library(features)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")


# Step 1: Combine the code to establish pseed2 data tibble & create pseed.wide
pseed2 <- pseed %>%
  left_join(speeds, by = c("speed" = "vol")) %>%
  left_join(pseed.bl, by = "fish") %>%
  mutate(bl.s = cm.s / bl)
pseed2

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 
View(pseed.wide)


# Step 2: Create a custom function to compute the standard error of the mean (SE)
compute_se <- function(x) {
  se <- sd(x) / sqrt(length(x))
  return(se)
}


# Calculate the mean maximum amp.sum for each specific swimming speed and fish
pseed.sum.max <- pseed.wide %>%
  group_by(speed, fish, bl.s) %>%
  summarise(
    amp.sum.mean = mean(amp.sum, na.rm = TRUE),
    amp.sum.se = compute_se(amp.sum)
  ) %>%
  print()

#EXPERIMENTAL SECTION# (the max_amp_sum needs to be computed as the mean of it I believe)

find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}

pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T)
View(pseed.max)

pseed.max.prefinal <- pseed.max %>%
  group_by(date, fish, bl.s) %>%
  summarise(
    max_amp_sum = max(amp),  # Assuming 'amp' is the column with amplitudes
    amp.sum.mean = mean(max_amp_sum), 
  ) 

pseed.sum.max <- pseed.max.prefinal %>%
  group_by(speed, fish, date) %>%
  summarise(
    amp.sum.mean = mean(max_amp_sum, na.rm = TRUE),
    amp.sum.se = compute_se(amp.sum)
  ) %>%
  print()


# Step 4: Plot mean amp.sum vs. specific swimming speed with error bars
ggplot(pseed.sum.max, aes(x = speed, y = amp.sum.mean, color = fish)) +
  geom_point() +
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), width = 0.2) +
  labs(x = "Specific Swimming Speed", y = "Mean amp.sum", title = "Mean amp.sum vs. Swimming Speed")


# Step 5: Merge external_data with pseed.sum.max
pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.final <- left_join(pseed.sum.max, pseed.met.rate, by="bl.s")%>%
  select(-fish.y)%>%
  rename("Fish"="fish.x")


# Step 6: Plot metabolic power output vs. mean maximum of amp.sum
ggplot(merged_data, aes(x = amp.sum.mean, y = met.rate, color = fish)) +
  geom_point() +
  labs(x = "Mean amp.sum", y = "Metabolic Power Output", title = "Metabolic Power vs. Mean amp.sum")
