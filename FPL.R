## Fantasy Premier League 
library(jsonlite)
library(tidyverse)
library(worldfootballR)

# helpful article https://www.game-change.co.uk/2023/02/10/a-complete-guide-to-the-fantasy-premier-league-fpl-api/


general_info = fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")
fixtures = fromJSON("https://fantasy.premierleague.com/api/fixtures/")
player = fromJSON("https://fantasy.premierleague.com/api/element-summary/2/")
manager = fromJSON("https://fantasy.premierleague.com/api/entry/3919272/")
transfers = fromJSON("https://fantasy.premierleague.com/api/entry/3919272/transfers/")
picks = fromJSON("https://fantasy.premierleague.com/api/entry/3919272/event/GW1/picks/")

### INFO ####
color_names = c("blue","pink","white","green","purple")
color_hex = c("#04F5FF","#E90052","#FFFFFF","#00FF85","#38003C")

colors = bind_cols(color_names,color_hex)


#### dataframes ####
events = tibble(general_info$events)
game_settings = tibble(general_info$game_settings)
phases = tibble(general_info$phases)
teams = tibble(general_info$teams)
elements = tibble(general_info$elements)
elements_stats = tibble(general_info$element_stats)
elements_types = tibble(general_info$element_type)
df_fixtures = tibble(fixtures)
player_fixtures = tibble(player$fixtures)
history = tibble(player$history)
history_past = tibble(player$history_past)

#filters for types of player 
forwards = elements %>%
  filter(element_type == 4)
midfielders = elements %>%
  filter(element_type == 3)
defenders = elements %>%
  filter(element_type == 2)
keepers = elements %>%
  filter(element_type == 1)


#### EDA ####
head(events)


### MODELS #### 



#### ANALYSIS #### 
