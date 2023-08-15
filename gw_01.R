### gameweek 1 
## script started august 14, 2023

# setup
library(tidyverse)
library(jsonlite)

# import 
general_info = fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")
fixtures = fromJSON("https://fantasy.premierleague.com/api/fixtures/")
player = fromJSON("https://fantasy.premierleague.com/api/element-summary/308/")
manager = fromJSON("https://fantasy.premierleague.com/api/entry/3919272/")
transfers = fromJSON("https://fantasy.premierleague.com/api/entry/3919272/transfers/")
picks = fromJSON("https://fantasy.premierleague.com/api/entry/3919272/event/1/picks/")

# load player stats 
gw1 = tibble(general_info$elements)

write_csv(gw1, "gw01.csv")


# data frames 
events = tibble(general_info$events)
game_settings = tibble(general_info$game_settings)
phases = tibble(general_info$phases)
teams = tibble(general_info$teams)
elements_stats = tibble(general_info$element_stats)
elements_types = tibble(general_info$element_type)
df_fixtures = tibble(fixtures)
player_fixtures = tibble(player$fixtures)
history = tibble(player$history)
history_past = tibble(player$history_past)



# individial data 
ben = fromJSON("https://fantasy.premierleague.com/api/entry/3919272/event/1/picks/")
ethan = fromJSON("https://fantasy.premierleague.com/api/entry/6814424/event/1/picks/")
scott = fromJSON("https://fantasy.premierleague.com/api/entry/395362/event/1/picks/")
patrick = fromJSON("https://fantasy.premierleague.com/api/entry/6702327/event/1/picks/")
christian = fromJSON("https://fantasy.premierleague.com/api/entry/7781854/event/1/picks/")

ben_picks = ben$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points) %>% 
  mutate(total_points = total_points*multiplier)

ethan_picks = ethan$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points) %>% 
  mutate(total_points = total_points*multiplier)

scott_picks = scott$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points) %>% 
  mutate(total_points = total_points*multiplier)

patrick_picks = patrick$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points) %>% 
  mutate(total_points = total_points*multiplier)

christian_picks = christian$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points) %>% 
  mutate(total_points = total_points*multiplier)

