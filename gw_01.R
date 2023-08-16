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

color_hex = c("#04F5FF","#E90052","#EAF205","#00FF85","#38003C")

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

# individual points totals 
ben_picks = ben$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points, element_type) %>% 
  mutate(total_points = total_points*multiplier,
         name = "Ben")

ethan_picks = ethan$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points, element_type) %>% 
  mutate(total_points = total_points*multiplier,
         name = "Ethan")

scott_picks = scott$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points, element_type) %>% 
  mutate(total_points = total_points*multiplier,
         name = "Scott")

patrick_picks = patrick$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points, element_type) %>% 
  mutate(total_points = total_points*multiplier,
         name = "Patrick")

christian_picks = christian$picks %>% 
  left_join(elements, by = c("element" = "id")) %>% 
  select(first_name, second_name, element:is_vice_captain, total_points, element_type) %>% 
  mutate(total_points = total_points*multiplier, 
         name = "Christian")

# combined points totals 
gw_1_points = ben_picks %>% 
  bind_rows(ethan_picks, scott_picks, patrick_picks, christian_picks)

# scoreboard 
gw_1_points %>% 
  group_by(name) %>% 
  summarize(points = sum(total_points)) %>% 
  ungroup() %>% 
  arrange(desc(points)) %>% 
  ggplot(aes(points, reorder(name, points)))+
  geom_col(aes(fill = name))+
  scale_fill_manual(values = color_hex, guide = "none")+
  theme_minimal()+
  labs(
    title = "Total Points",
    subtitle = "Through GW1",
    y = NULL,
    x = "Points"
  )+
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(face = "italic", size = 16),
    axis.text.y = element_text(face = "italic", size = 16)
  )
  

# breakdown by position 
gw_1_points %>%
  filter(total_points > 0) %>% 
  mutate(position = case_when(
    element_type == 1 ~ "GKP",
    element_type == 2 ~ "DEF",
    element_type == 3 ~ "MID",
    element_type == 4 ~ "FWD",
    TRUE ~ NA
  )) %>% 
  ggplot(aes("", total_points, fill = position))+
  geom_col(stat = "identity", width = 1, position = "fill")+
  facet_wrap(~name)+
  coord_polar("y", start = 0)+
  theme_void()


