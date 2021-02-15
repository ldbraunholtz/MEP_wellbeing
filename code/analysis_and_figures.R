## ---------------------- Data analysis and figures ----------------------------####


library(tidyverse)
library(here)
#library(googlesheets4)
#library(devtools)
#devtools::install_github("jcbain/cuttlefish")

## get colour pallette for plotting with
#colours_vector <- cuttlefish::create_palette("MEP_Logo_transparent.png", n = 20)
#team_colours <- cuttlefish::find_segmented(colours_vector, 6)
#team_colours <- cuttlefish::find_prominent(colours_vector, 6)
#library(scales)
#show_col(colours_vector)
#show_col(team_colours)
team_colours <- c("3" = "#768D3B", "Royal purple" = "#886F50", "Goaldiggers" = "#6AB0DB", "TEAM: - If you're\nhappy and you know\nit, wash your hands" = "#B9E39D", "Well in Hell" = "#E5F6F9", Wellabies = "#E9E1D6")

## read in data:
mepwell <- read_csv(here("data", "MEP_Wellbeing.csv"))

###----------------- Tidy the data -----------------####

mepwell[mepwell== "-" ] <- NA

mepwell <- mepwell %>%
  select(1:31) %>% # only keep the days we've got data til - 8th Jan
  mutate_at(vars(starts_with("Jan")), funs(as.numeric)) %>%
  filter_at(vars(starts_with("Jan")), any_vars(!is.na(.)))

mepwell$Group[mepwell$Group == "TEAM: - If you're happy and you know it, wash your hands"] <- "TEAM: - If you're\nhappy and you know\nit, wash your hands"


# create activity categories

mepwell$Category <- as.factor(mepwell$Activity)

unique(mepwell$Activity)

mepwell$Category <- 
  fct_collapse(mepwell$Category,
               `Active indoors` = c("Ab workout", "Ballet", "Body pump", "Capoeira",
                                    "Cross trainer", "Dance class", "HIIT", "Kickboxing", 
                                    "Oddities: you tube dance (sigh) - also discovered the joys of wireless headphones and trying out some hiphop moves with no one watching in the dark :)",
                                    "Physiotherapy/pilates", 
                                    "Stretch Class/ Body Conditioning", "Stretching/yoga",
                                    "Table tennis", "Tap", "Workout", "Yoga", 
                                    "Yoga/ workout", "HIIT / S&C"),
               `Active outdoors` = c("Bike ride", "Built Snowman", "Cycle", "Cycling", 
                                     "Football", "Jogging", "Playing in the snow", 
                                     "Running", "Surfing", "Walk", "walking", "Walking", 
                                     "Walking - always with Ninjapie", "Walking / Hiking", 
                                     "Walking/running w/podcast", "running"),
               `Arts, crafts and cooking` = c("Arts and Crafts", "Arts/Crafts", "Baking",
                                              "Cross stitch", "Drawing", "Embroidery", 
                                              "Kneading bread", "Knitting", "Painting", 
                                              "Weaving", "Macrame"),
               `Being musical` = c("Choir", "Guitar/Berimbau", 
                                   "Singing and Dancing like a lunatic", "Violin", 
                                   "Virtual choir practice", "online concert", "Relaxing with music"),
               `Learning/practicing a skill` = c("Duolingo", "Juggling", 
                                                 "Language Puzzles"),
               Meditation = c("Meditating", "Meditation"),
               `Playing games` = c("Board games", "Board games & card games", 
                                   "Boardgames", "Exploding kittens game", "Games", 
                                   "Jigsaw", "Jigsaw/other puzzles", "Snap card game",
                                   "Sudoko", "Tabletop Games", "Zoom quiz"),
               `Playing video games` = c("Gaming", "Video games"),
               `Reading/Listening to books` = c("audiobook", "Listen to audiobook", 
                                                "Podcast", "podcast", "Reading", 
                                                "Reading (Earthsea)", 
                                                "reading (Hornblower)", 
                                                "Reading/ audiobook"),
               `Self care` = c("Bubble bath", "Long bubble bath", 
                               "Phoning family/friends"),
               `Time with kids` = c("Art with my children", "Baking with children for fun",
                                    "Playing cards with children", 
                                    "Reading to my daughter", 
                                    "Stuff with Izzy: reading, activity boo, hide and seekk",                                     "Stuff with kids: reading books, playing, homework activities"),
               `Time with pets` = c("Entertaining Mischief (cat)", 
                                    "Hanging out with my mice", 
                                    "Playing fetch with the Kipster", 
                                    "Playing with ferrets", "Walking my dog"), `Social things` = c("Babysitting Niece", "Skype family", "Pub quiz")
               
               )
levels(mepwell$Category)



###----------------- Setting up plot theme  -----------------####

plottheme <- theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "#4D4D4D", colour = NA),
  panel.background = element_rect(fill = "#878787", colour = NA),
  legend.background = element_rect(fill = "#4D4D4D", colour = NA),
  axis.text = element_text(colour = "linen"),
  axis.title = element_text(colour = "linen")
)


###----------------- Plot of time spent per activity  -----------------####

mepwell <- mepwell %>%
  mutate(tot_time = rowSums(.[4:31], na.rm = TRUE)) %>%
  group_by(Category) %>%
  mutate(Cat_time = sum(tot_time))

ggplot(mepwell, aes(x = tot_time, y = reorder(Category, Cat_time), fill = Group)) + 
  geom_col() + 
  scale_fill_manual(values = team_colours, labels = function(x) str_wrap(x, width = 7)) + 
  labs(x = "Time per activity in minutes", y = "") +
  plottheme +
  theme(legend.position = "none")
ggsave("outputs/minutespactivity_finalweek.jpg")

###----------------- Plot of time spent per person per group -----------------####


### Calculate people per group:
mepwell %>% 
  ungroup() %>%
  select(1:3, 33) %>% 
  filter(tot_time != 0) %>% 
  select(Group, Name) %>% 
  unique() %>% 
  count(Group) ->
  ppgroup

## Calculate how much time each group spent, divided by group members:
mepwell %>% 
  ungroup() %>%
  select(1:3, 33) %>% 
  group_by(Group) %>% 
  summarise(sum = sum(tot_time)) %>% 
  left_join(ppgroup) %>% 
  mutate(sumpp = sum / n) ->
  progresssummary

## Add line breaks to very long group name:

ggplot(progresssummary, aes(x = sumpp, y = fct_reorder(Group, sumpp), fill = Group)) +
  geom_col() +
  scale_fill_manual(values = team_colours) +
  labs(x = "Time per person in minutes", y = "") +
  plottheme +
  theme(legend.position = "none")
ggsave("outputs/minutespppgroup_finalweek.jpg")

###----------------- for postcard entry -----------------###

mepwell %>%
  mutate(tot_time = rowSums(.[4:31], na.rm = TRUE)) %>%
  group_by(Group, Name) %>%
  summarise(person_time = sum(tot_time)) -> person_time

mepwell %>%
  mutate(tot_time = rowSums(.[4:31], na.rm = TRUE)) %>%
  group_by(Group, Activity) %>%
  mutate(Cat_time = sum(tot_time)) %>%
  summarise(top_act = max(tot_time), .groups = "drop_last") %>%
  slice(which.max(top_act)) -> top_act

left_join(person_time, top_act, by = "Group") %>%
  select(-top_act) -> final_spread

write.csv(final_spread, here("outputs", "final_summary.csv"), row.names = FALSE)
