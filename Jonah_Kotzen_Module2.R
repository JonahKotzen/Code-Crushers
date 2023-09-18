#---establish beginning---#
library(tidyverse)
library(dplyr)
library(features)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")


# Step 1: Combine the code to establish pseed2 data tibble & create pseed.wide
pseed2 <- pseed %>%
  left_join(speeds, by=c("speed"="vol")) %>% 
  left_join(pseed.bl, by=("fish")) %>% 
  mutate(bl.s= cm.s/bl) 
pseed2

pseed.wide <- pseed2 %>%
  select(-amp) %>%
  pivot_wider(names_from = fin, values_from = amp.bl) %>%
  mutate(amp.sum = L+R)
View(pseed.wide)


# Step 2: Create a custom function to compute the standard error of the mean (SE) and for finding maxes
compute_se <- function(x) {
  se <- sd(x) / sqrt(length(x))
  return(se)
}

find.maxes <- function(x,y,mult=100){
  f <- fget(features(x=x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peak=round(crit.pts,0))
  return(f$peak)
}

# Calculate the mean maximum amp.sum for each specific swimming speed and fish

pseed.peaks <- pseed.wide %>%
  group_by(date,fish) %>%
  mutate(peak=frame %in% find.maxes(frame,amp.sum)) %>%
  filter(peak==T)

pseed.sum.max <- pseed.peaks %>%
  group_by(date,fish,bl.s) %>%
  summarise(amp.sum.mean = mean(amp.sum),amp.sum.se = compute_se(amp.sum))%>%
  print()

# Step 4: Plot mean amp.sum vs. specific swimming speed with error bars
pseed.sum.max %>%
  ggplot(aes(x = bl.s, y = amp.sum.mean, colour = fish)) +
  geom_point() +
  geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se), width = 0.08) +
  geom_point()


# Step 5: Merge external_data with pseed.sum.max
pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.met.rate2 <- merge(x = pseed.sum.max, y = pseed.met.rate[, c("date", "met.rate")], by = "date", all.x = TRUE)


# Step 6: Plot metabolic power output vs. mean maximum of amp.sum
pseed.met.rate2 %>%
  ggplot(aes(x = amp.sum.mean, y = met.rate, colour = fish)) +
  geom_point()
