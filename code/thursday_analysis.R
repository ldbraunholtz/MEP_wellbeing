## for thursday meeting

library(tidyverse)
library(here)
library(ggimage)
library(tidyverse)
library(ggtext)

###----------------- breakout room - choc plot -----------------###

## read in data:
mepwell <- read_csv(here("data", "data_for_group.csv"))

cal <- read.csv(here("data", "data_for_group_CALORIES.csv"), stringsAsFactors = FALSE)

mepwell <- mepwell %>%
  filter(Category == "Active outdoors")

table(mepwell$Activity)

mepwell$Activity[mepwell$Activity == "Bike ride" | mepwell$Activity == "Cycle"] <- "Cycling"
mepwell$Activity[mepwell$Activity == "running"] <- "Running"
mepwell$Activity[mepwell$Activity == "Jogging"] <- "Running"
mepwell$Activity[mepwell$Activity == "Walk" | mepwell$Activity == "walking" | mepwell$Activity == "Walking - always with Ninjapie" | mepwell$Activity == "Walking / Hiking" | mepwell$Activity == "Walking/running w/podcast"] <- "Walking"
mepwell$Activity[mepwell$Activity == "Built Snowman" | mepwell$Activity == "Playing in the snow" ] <- "Snow Activity"

mepwell$Group[mepwell$Group == "TEAM: - If you're happy and you know it, wash your hands"] <- "TEAM: - If you're\nhappy and you know\nit, wash your hands"

### get the df

join <- full_join(mepwell, cal, by = c("Activity"="ACTIVITY")) %>%
  rename(calories = "CALORIE.EXPENDITURE...HR") %>%
  mutate(tot_time = rowSums(.[4:31], na.rm = TRUE)) %>%
  mutate(tot_cals = (tot_time/60) * calories) %>%
  mutate(n_choc = (tot_cals/people_in_group)/300) %>% # estimated average calories per choc bar = 300
  group_by(Group) %>%
  mutate(tot_choc = sum(n_choc)) %>%
  ungroup()

join$image <- here("choc.png")

# get ready to plot
plottheme <- theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "#886F50", colour = NA),
  panel.background = element_rect(fill = "#E9E1D6", colour = NA),
  legend.background = element_rect(fill = "#886F50", colour = NA),
  axis.text = element_text(colour = "linen"),
  axis.title = element_text(colour = "linen")
)

ggplot(data = join, aes(x = n_choc, fct_reorder(Group, tot_choc))) + 
  geom_col(fill = "#886F50") +
  geom_image(aes(x = tot_choc, image = image), size = 0.2, by = "height") + 
  labs(y = "", x = "Average number of chocolate bars earned per person during outdoor \nexercise in January 2021", caption = "Based on following estimated values for calorie expenditure (kcal/hour): walking = 263, running = 557, \ncycling = 600, football = 400, snow activities = 285, surfing = 350. \nEstimated kcal content of chocolate bar = 300. ") +
  plottheme
