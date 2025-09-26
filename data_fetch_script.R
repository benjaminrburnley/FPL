### API pulls 

# packages 
library(tidyverse)
library(jsonlite)


general_info = fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/") # pull general information

elements = tibble(general_info$elements) # pull out elements dataframe

# write elements for weekly changes in
write_csv(elements, "data/gw38_elements.csv")

index = elements$id #index of all active player ids 

names = elements %>% 
  dplyr::select(id,first_name, second_name,web_name, element_type, team)

data = tibble() #__________________________________________create base data frame to append to

# loop to pull data individidual player data for each game
for(i in index){
  x = fromJSON(paste("https://fantasy.premierleague.com/api/element-summary/",as.character(i),"/", sep = ""))
  y = tibble(x$history)
  data = data %>%
    bind_rows(y)
}

### write data for new gameweek 
write_csv(data, "data/gw38_stats.csv")


#### MANAGER DATA 

# function to pull manager scores
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

# function to pull manager metadata 
get_hits = function(name, id){
  test = tibble()
  for(i in 1:max(data$round)){
    x = fromJSON(paste("https://fantasy.premierleague.com/api/entry/", as.character(id),"/event/", as.character(i),"/picks/", sep = ""))
    y = tibble(
      manager = name,
      round = i,
      hit = x$entry_history[["event_transfers_cost"]],
      pob = x$entry_history[["points_on_bench"]]
    )
    test = test %>% 
      bind_rows(y)
  }
  return(test)
}

### pull data 
# # individual manager pulls
ben = get_manager("Ben", 4053672)
ethan = get_manager("Ethan", 4680842)
scott = get_manager("Scott", 298444)
patrick = get_manager("Patrick", 6698309)
christian = get_manager("Christian", 7297662)
yuheng = get_manager("Yuheng", 4693202)
luke = get_manager("Luke", 7320070)
dan = get_manager("Dan", 8564597)

# get hits 
ben_hits = get_hits("Ben", 4053672)
ethan_hits = get_hits("Ethan", 4680842)
scott_hits = get_hits("Scott", 298444)
patrick_hits = get_hits("Patrick", 6698309)
christian_hits = get_hits("Christian", 7297662)
yuheng_hits = get_hits("Yuheng", 4693202)
luke_hits = get_hits("Luke", 7320070)
dan_hits = get_hits("Dan", 8564597)

# # bind together with names and points
scores = ben %>%
  bind_rows(ethan, scott, patrick, christian, yuheng, luke, dan) %>%
  left_join(names, by = c("element" = "id")) %>%
  left_join(data, by = c("element" = "element", "round" = "round"), relationship = "many-to-many")

write_csv(scores, "data/gw38_scores.csv")

# bind hits 
hits = ben_hits %>% 
  bind_rows(ethan_hits, scott_hits, patrick_hits, christian_hits, yuheng_hits, luke_hits, dan_hits)

# calculate total hits
total_hits = hits %>% 
  group_by(manager) %>% 
  summarize(hits = sum(hit))

# get fpl global average score 
average_scores = tibble(general_info$events) %>% 
  select(id, average_entry_score) %>% 
  filter(average_entry_score != 0)

# calculate league scoreboard
scoreboard = scores %>% 
  left_join(average_scores, by = c("round" = "id")) %>% 
  mutate(gw_points = total_points * multiplier) %>% 
  group_by(manager, round) %>% 
  summarize(gw_points = sum(gw_points, na.rm = T)) %>% 
  left_join(hits, by = c("manager", "round")) %>% 
  mutate(adj_points = gw_points - hit,
         total_points = cumsum(adj_points)) %>% 
  ungroup() %>% 
  left_join(average_scores, by = c("round" = "id")) %>% 
  mutate(points_plus = round(gw_points/average_entry_score * 100))

write_csv(scoreboard,"data/gw38_scoreboard.csv")
