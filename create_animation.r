library(tidyverse)
library(gganimate)

# Read data ----
url <- "https://github.com/SamEdwardes/ufc-data/raw/master/raw_total_fight_data.csv"
df <- read_delim(url, delim = ";", col_type = cols()) %>%
  janitor::clean_names() %>%
  mutate(date = lubridate::mdy(date))

# Tidy data ----
winner_df <- df %>%
  filter(winner != "NA") %>%
  mutate(year = lubridate::year(date)) %>%
  select(winner, year) %>%
  group_by(winner, year) %>%
  summarise(wins = n()) %>%
  # fill in missing values
  ungroup() %>%
  complete(year, nesting(winner), fill = list(wins = 0)) %>%
  # running total of wins
  ungroup() %>%
  group_by(winner) %>%
  mutate(wins_running_total = cumsum(wins)) %>%
  # rank by year
  ungroup() %>%
  group_by(year) %>%
  arrange(year, desc(wins_running_total)) %>%
  mutate(rank = rank(-wins_running_total, ties.method = "first")) %>%
  # stuff for animation to work
  ungroup() %>%
  group_by(year) %>%
  mutate(
    wins_running_total_rel = wins_running_total / wins_running_total[rank == 1],
    wins_label = paste0(" ", round(wins_running_total))
  ) %>%
  # keep only the top 10
  ungroup() %>%
  filter(rank <= 10, wins_running_total > 0) %>%
  arrange(year, rank)

# Animation theme ----
animation_theme <- theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(size = .1, color = "grey"),
    panel.grid.minor.x = element_line(size = .1, color = "grey"),
    plot.title = element_text(size = 40, hjust = 0.5, face = "bold", colour = "Black", vjust = -1),
    plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "grey"),
    plot.caption = element_text(size = 8, hjust = 0.5, face = "italic", color = "grey"),
    plot.background = element_blank(),
    plot.margin = margin(2, 4, 2, 8, "cm") # top, right, bottom, left
)

# Create animation ----
anim_top_10 <- winner_df %>%
  filter(year >= 2000, year <= 2003) %>%
  ggplot(aes(rank,
    group = winner,
    fill = as.factor(winner)
    # colour = as.factor(winner)
  )) +
  # scale_color_viridis_d(name="") +
  scale_fill_viridis_d(name="") +
  geom_tile(aes(
    y = wins_running_total / 2,
    height = wins_running_total,
    width = 0.9
  ), alpha = 0.8, colour = NA) +
  geom_text(aes(y = 0, label = paste(winner, " ")), vjust = 0.2, hjust = 1, size = 10) +
  geom_text(aes(y = wins_running_total, label = wins_label, hjust = 0), size = 10) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  animation_theme +
  transition_states(year, transition_length = 1, state_length = 3) +
  view_follow(fixed_x = TRUE) +
  labs(
    title = "UFC Top 10 All Time Winning Figthers : {closest_state}",
    subtitle = ""
  )

gif_seconds <- 5 # number of seconds
gif_fps <- 5 # frames-per-second

# Save gif file ----
animate(anim_top_10, nframes = gif_seconds * gif_fps, 
  start_pause = 3, end_pause = 3,
  fps = gif_fps, width = 1200, height = 1000,
  renderer = gifski_renderer("ufc_top_10.gif")
)

print("COMPLETE")
