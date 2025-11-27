library(httr)
library(jsonlite)
library(tidyverse)
library(hockeyR)
library(sportyR)


df_game333 <- get_pwhl_game_data(333)

game333_shots <- df_game333 %>% 
  filter(event == "shot") %>% 
  select(time_formatted, team_id, player_id, x_location,
         y_location, goalie_id, shot_type_description, shot_quality_description)

head(game333_shots)

game333_shots$x_adj <- (game333_shots$x_location - 290) / 2.9  # Center and scale
game333_shots$y_adj <- (game333_shots$y_location - 147) / 3.47  # Center and scale


rink <- geom_hockey("nhl")

shot_plot <- rink +
  geom_point(data = game333_shots, 
             aes(x = x_adj, 
                 y = y_adj, 
                 color = factor(team_id)),
             size = 3,
             alpha = 0.7) +
  scale_color_manual(values = c("8" = "#0F4470", "9" = "#A76741"),
                     labels = c("8" = "SEA", "9" = "VAN"),
                     name = "Team") +
  labs(title = "Shot Locations by Team",
       subtitle = "PWHL Game") +
  theme_minimal()

print(shot_plot)

