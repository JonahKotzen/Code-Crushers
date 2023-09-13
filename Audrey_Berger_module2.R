library(tidyverse)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds, by=c("speed"="vol"))%>%
  print()
view(pseed2)

pseed2%>%
  select(fish)%>%
  unique()

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")

pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()