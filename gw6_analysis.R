### gameweek 6 analysis 

# libraries
library(tidyverse)
library(jsonlite)
library(ggrepel)

# read in 
general_info = fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")

elements = tibble(general_info$elements) # pull out elements dataframe

## plot 
elements %>% 
  filter(minutes > 0) %>% 
  ggplot(aes(now_cost/10, as.numeric(points_per_game)))+
  geom_point(position = "jitter", alpha = .75)+
  geom_smooth(method = "lm", se = F, alpha = 0.5)+
  geom_text_repel(aes(label = web_name))+
  theme_minimal()+
  labs(
    title = "Price vs. Points-Per-Game",
    x = "Cost",
    y = "Points-Per-Game"
  )+
  theme(
    plot.title = element_text(face = "bold", size = 32)
  )

data = read_csv("data/gw06_scores.csv")
