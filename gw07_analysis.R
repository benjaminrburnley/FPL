## gameweek 7 analysis

# libraries
library(tidyverse)
library(jsonlite)
library(ggrepel)

# data 
general_info = fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")

elements = tibble(general_info$elements)

# standardized values 
elements %>% 
  filter(minutes > 0) %>% 
  mutate(position = case_when(
    element_type == 1 ~ "GK",
    element_type == 2 ~ "DEF",
    element_type == 3 ~ "MID",
    element_type == 4 ~ "FWD",
    TRUE ~ NA
  )) %>% 
  mutate(bpfg = case_when(
    element_type == 1 ~ goals_scored * 12,
    element_type == 2 ~ goals_scored * 12,
    element_type == 3 ~ goals_scored * 18,
    element_type == 4 ~ goals_scored * 24,
    TRUE ~ NA
  ),
  bpfa = assists * 9,
  bpfcs = if_else(element_type %in% 1:2, clean_sheets * 12, 0),
  bpfpks = penalties_saved * 15,
  bpfs = saves * 2,
  adjustment = bpfg + bpfa + bpfcs + bpfpks + bpfs,
  adj_bps = bps - adjustment) %>% 
  select(web_name, total_points, adj_bps, position) %>% 
  mutate(s_tps = scale(total_points),
         s_adj_bps = scale(adj_bps)) %>% 
  ggplot(aes(s_adj_bps, s_tps, color = position))+
  geom_text_repel(aes(label = web_name), show.legend = F)+
  geom_point(position = "jitter")+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  annotate("text", x = -3, y = -3, label = "Bad at FPL")+
  annotate("text", x = -3, y = 4, label = "Good at Points \n Bad at Bonus Points", face = "bold")+
  annotate("text", x = 3, y = 4, label = "Good at Points \n Good at Bonus Points")+
  annotate("text", x = 3, y = -3, label = "Bad at Points \n Good at Bonus Points")+
  xlim(-3,3)+
  ylim(-3,4)+
  theme_minimal()+
  labs(
    color = "Position"
  )+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    text = element_text(face = "bold")
  )

# logged values 
elements %>% 
  filter(minutes > 0) %>% 
  mutate(position = case_when(
    element_type == 1 ~ "GK",
    element_type == 2 ~ "DEF",
    element_type == 3 ~ "MID",
    element_type == 4 ~ "FWD",
    TRUE ~ NA
  )) %>% 
  mutate(bpfg = case_when(
    element_type == 1 ~ goals_scored * 12,
    element_type == 2 ~ goals_scored * 12,
    element_type == 3 ~ goals_scored * 18,
    element_type == 4 ~ goals_scored * 24,
    TRUE ~ NA
  ),
  bpfa = assists * 9,
  bpfcs = if_else(element_type %in% 1:2, clean_sheets * 12, 0),
  bpfpks = penalties_saved * 15,
  bpfs = saves * 2,
  adjustment = bpfg + bpfa + bpfcs + bpfpks + bpfs,
  adj_bps = bps - adjustment) %>% 
  select(web_name, total_points, adj_bps, position) %>% 
  ggplot(aes(adj_bps, total_points, color = position))+
  geom_text_repel(aes(label = web_name), show.legend = F)+
  geom_point(position = "jitter")+
  annotate("text", x = -3, y = -3, label = "Bad at FPL")+
  annotate("text", x = -3, y = 4, label = "Good at Points \n Bad at Bonus Points", face = "bold")+
  annotate("text", x = 3, y = 4, label = "Good at Points \n Good at Bonus Points")+
  annotate("text", x = 3, y = -3, label = "Bad at Points \n Good at Bonus Points")+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal()+
  labs(
    color = "Position"
  )+
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    text = element_text(face = "bold")
  )

## rank 
elements %>% 
  filter(minutes > 180) %>% 
  filter(selected_by_percent > 1) %>% 
  mutate(position = case_when(
    element_type == 1 ~ "GK",
    element_type == 2 ~ "DEF",
    element_type == 3 ~ "MID",
    element_type == 4 ~ "FWD",
    TRUE ~ NA
  )) %>% 
  mutate(bpfg = case_when(
    element_type == 1 ~ goals_scored * 12,
    element_type == 2 ~ goals_scored * 12,
    element_type == 3 ~ goals_scored * 18,
    element_type == 4 ~ goals_scored * 24,
    TRUE ~ NA
  ),
  bpfa = assists * 9,
  bpfcs = if_else(element_type %in% 1:2, clean_sheets * 12, 0),
  bpfpks = penalties_saved * 15,
  bpfs = saves * 2,
  adjustment = bpfg + bpfa + bpfcs + bpfpks + bpfs,
  adj_bps = bps - adjustment) %>% 
  select(web_name, total_points, adj_bps, position) %>% 
  mutate(rank_points = rank(total_points),
         rank_bps = rank(adj_bps)) %>%
  ggplot(aes(rank_bps, rank_points, color = position))+
  geom_text_repel(aes(label = web_name), show.legend = F)+
  geom_point(position = "jitter")+
  geom_vline(aes(xintercept = median(rank_bps)))+
  geom_hline(aes(yintercept = median(rank_points)))+
  xlim(-30,190)+
  ylim(-30,190)+
  annotate("text", x = -20, y = -20, label = "Bad at FPL")+
  annotate("text", x = -20, y = 180, label = "Good at Points \n Bad at Bonus Points", face = "bold")+
  annotate("text", x = 180, y = 180, label = "Good at Points \n Good at Bonus Points")+
  annotate("text", x = 180, y = -20, label = "Bad at Points \n Good at Bonus Points")+
  theme_minimal()+
  labs(
    color = "Position"
  )+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    text = element_text(face = "bold")
  )
