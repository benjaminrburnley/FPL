### script to pull most recent data on all players

# setup
library(tidyverse)
library(jsonlite)
library(patchwork)

team_colors = c("#EF0107", "#95BFE5", "#B50E12", "#e30613", "#0057B8", "#6C1D45", "#034694", "#1B458F", "#003399", "#000000", 
                "#C8102E", "#F78F1E", "#6CABDD", "#DA291C", "#241F20", "#DD0000", "#EE2737", "#132257", "#7A263A", "#FDB913")

#### GET INDEX ####
general_info = fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/") # pull general information JSON

elements = tibble(general_info$elements) # pull out elements dataframe

index = elements$id #index of all active player ids 


#### LOOP TO PULL ALL WEEKLY PLAYER DATA ####
data = tibble() #__________________________________________create base data frame to append to

# loop to pull data
for(i in index){
  x = fromJSON(paste("https://fantasy.premierleague.com/api/element-summary/",as.character(i),"/", sep = ""))
  y = tibble(x$history)
  data = data %>% 
    bind_rows(y)
}

names = elements %>% 
  dplyr::select(id,first_name, second_name, element_type)

#### PULL MANAGER DATA #### 

get_manager = function(name, id ){
  test = tibble()
  for(i in 1:max(data$round)){
    x = fromJSON(paste("https://fantasy.premierleague.com/api/entry/", as.character(id),"/event/", as.character(i),"/picks/", sep = ""))
    y = tibble(x$picks) %>% 
      mutate(round = i)
    test = test %>% 
      bind_rows(y) %>% 
      mutate(manager = name)
  }
  return(test)
}

# individal manager pulls
ben = get_manager("Ben", 3919272)
ethan = get_manager("Ethan", 6814424)
scott = get_manager("Scott", 395362)
patrick = get_manager("Patrick", 6702327)
christian = get_manager("Christian", 7781854)

# bind together with names and points 
scores = ben %>% 
  bind_rows(ethan, scott, patrick, christian) %>% 
  left_join(names, by = c("element" = "id")) %>% 
  left_join(data, by = c("element" = "element", "round" = "round"), relationship = "many-to-many")

#### Scoreboards ####
average_scores = tibble(general_info$events) %>% 
  select(id, average_entry_score)

# total points
scores %>% 
  left_join(average_scores, by = c("round" = "id")) %>% 
  mutate(round_points = total_points * multiplier) %>% 
  group_by(manager) %>% 
  summarise(total_points = sum(round_points,na.rm = T)) %>% 
  arrange(desc(total_points))

# best gameweek
scores %>% 
  mutate(round_points = total_points * multiplier) %>% 
  group_by(manager, round) %>% 
  summarise(total_points = sum(round_points,na.rm = T)) %>% 
  left_join(average_scores, by = c("round" = "id")) %>% 
  mutate(points_plus = round(total_points/average_entry_score * 100)) %>% 
  select(-average_entry_score) %>% 
  arrange(desc(total_points)) %>% 
  ungroup() %>% 
  top_n(n = 10)

# gameweek wins 
scores %>% 
  mutate(round_points = total_points * multiplier) %>% 
  group_by(manager, round) %>% 
  summarise(total_points = sum(round_points,na.rm = T)) %>% 
  group_by(round) %>% 
  mutate(won_gameweek = total_points == max(total_points)) %>% 
  group_by(manager) %>% 
  summarise(wins = sum(won_gameweek)) %>% 
  arrange(desc(wins))
  

### Question: Which team are we pulling from

# get id, web name, and teamid from elements 
team_id = elements %>% 
  dplyr::select(id, web_name, team)

# merge with scores table
scores = scores %%
  left_join(team_id, by = c("element" = "id"))


our_teams = scores %>% 
  group_by(team) %>% 
  count()

# counts by team selection
teams = tibble(general_info$teams)
team_picks = teams %>% 
  dplyr::select(id, name) %>% 
  left_join(our_teams, by = c("id" = "team")) %>% 
  mutate(n = replace_na(n, 0), 
         prop = n/sum(n)*100)

p1 = ggplot(team_picks, aes(reorder(name, prop), prop, fill = name))+
  geom_col()+
  scale_fill_manual(values = team_colors, guide = "none")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1))+
  theme_minimal()+
  labs(
    title = "Where are our picks coming from?",
    subtitle = "Percent of players in our squads from each team",
    x = NULL,
    y = "Percent"
  )+
  theme(
    plot.title = element_text(size = 16, face = "bold")
  )

team_points = scores %>% 
  mutate(scored_points = total_points * multiplier) %>% 
  group_by(team) %>% 
  summarize(team_points = sum(scored_points, na.rm = T)) %>% 
  right_join(teams, by = c("team" = "id")) %>% 
  dplyr::select(name, team_points) %>% 
  mutate(team_points = replace_na(team_points, 0),
         prop = team_points/sum(team_points)*100)

p2 = ggplot(team_points, aes(reorder(name, prop), prop, fill = name))+
  geom_col()+
  scale_fill_manual(values = team_colors, guide = "none")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1))+
  theme_minimal()+
  labs(
    title = "Where are our points coming from?",
    subtitle = "Percent of our points scored in our squads by each team",
    x = NULL,
    y = "Percent"
  )+
  theme(
    plot.title = element_text(size = 16, face = "bold")
  )
p1 + p2

haaland = scores %>% 
  mutate(scored_points = total_points * multiplier,
         haaland = if_else(element == 355, "Haaland", "Everybody Else")) %>% 
  group_by(haaland) %>% 
  summarize(points = sum(scored_points, na.rm = T))

ggplot(haaland, aes("", points, fill = haaland))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()+
  scale_fill_manual(values = c("#38003C", "#6CABDD"))+
  labs(
    title = "What portion of our points is just Erling Haaland?",
    fill = NULL
  )+
  theme(
    plot.title = element_text(size = 16, face = "bold")
  )

### What teams are scoring the most points in general? 
elements %>% 
  group_by(team) %>% 
  summarize(team_points = sum(total_points, na.rm = T)) %>% 
  mutate(prop = team_points/sum(team_points)* 100) %>% 
  right_join(teams, by = c("team" = "id")) %>% 
  dplyr::select(name, team_points, prop) %>% 
  ggplot(aes(reorder(name, team_points), team_points, fill = name))+
  geom_col()+
  scale_fill_manual(values = team_colors, guide = "none")+
  coord_flip()+
  theme_minimal()+
  labs(
    title = "Which teams are the most points?",
    subtitle = "Percent of points scored from each team in the league",
    x = NULL,
    y = "Total Points"
  )+
  theme(
    plot.title = element_text(size = 16, face = "bold")
  )
