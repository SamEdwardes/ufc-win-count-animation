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
source("animation_theme.R")

# Create animation ----
anim_top_10 <- winner_df %>%
  # filter(year >= 2000, year <= 2003) %>% # uncomment for smaller df to format
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
    subtitle = "",
    caption = "Number of wins"
  )

gif_seconds <- 20 # number of seconds
gif_fps <- 20 # frames-per-second

# Save gif file ----
animate(anim_top_10, nframes = gif_seconds * gif_fps, 
  start_pause = 10, end_pause = 15,
  fps = gif_fps, width = 1200, height = 1000,
  renderer = gifski_renderer("imgs/ufc_top_10.gif")
)

print("COMPLETE")
