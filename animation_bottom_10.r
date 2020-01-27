library(tidyverse)
library(gganimate)

# Read data ----
url <- "https://github.com/SamEdwardes/ufc-data/raw/master/raw_total_fight_data.csv"
df <- read_delim(url, delim = ";", col_type = cols()) %>%
  janitor::clean_names() %>%
  mutate(date = lubridate::mdy(date))

# Tidy data ----
loser_df <- df %>%
  filter(winner != "NA") %>%
  select(winner, date, r_fighter, b_fighter) %>%
  mutate(
    year = lubridate::year(date),
    loser = if_else(winner == b_fighter, r_fighter, b_fighter)
  ) %>%
  select(loser, year) %>%
  group_by(loser, year) %>%
  summarise(losses = n()) %>%
  # fill in missing values
  ungroup() %>%
  complete(year, nesting(loser), fill = list(losses = 0)) %>%
  # running total of losses
  ungroup() %>%
  group_by(loser) %>%
  mutate(losses_running_total = cumsum(losses)) %>%
  # rank by year
  ungroup() %>%
  group_by(year) %>%
  arrange(year, desc(losses_running_total)) %>%
  mutate(rank = rank(-losses_running_total, ties.method = "first")) %>%
  # stuff for animation to work
  ungroup() %>%
  group_by(year) %>%
  mutate(
    losses_running_total_rel = losses_running_total / losses_running_total[rank == 1],
    losses_label = paste0(" ", round(losses_running_total))
  ) %>%
  # keep only the top 10
  ungroup() %>%
  filter(rank <= 10, losses_running_total > 0) %>%
  arrange(year, rank)

# Animation theme ----
source("animation_theme.R")

# Create animation ----
anim_bottom_10 <- loser_df %>%
  # filter(year >= 2000, year <= 2003) %>% # uncomment for smaller df to format
  ggplot(aes(rank,
    group = loser,
    fill = as.factor(loser)
    # colour = as.factor(loser)
  )) +
  # scale_color_viridis_d(name="") +
  scale_fill_viridis_d(name="") +
  geom_tile(aes(
    y = losses_running_total / 2,
    height = losses_running_total,
    width = 0.9
  ), alpha = 0.8, colour = NA) +
  geom_text(aes(y = 0, label = paste(loser, " ")), vjust = 0.2, hjust = 1, size = 10) +
  geom_text(aes(y = losses_running_total, label = losses_label, hjust = 0), size = 10) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  animation_theme +
  transition_states(year, transition_length = 1, state_length = 3) +
  view_follow(fixed_x = TRUE) +
  labs(
    title = "UFC Top 10 All Time Losing Figthers : {closest_state}",
    subtitle = "",
    caption = "Number of losses"
  )

gif_seconds <- 20 # number of seconds
gif_fps <- 20 # frames-per-second

# Save gif file ----
animate(anim_bottom_10, nframes = gif_seconds * gif_fps, 
  start_pause = 10, end_pause = 15,
  fps = gif_fps, width = 1200, height = 1000,
  renderer = gifski_renderer("imgs/ufc_bottom_10.gif")
)

print("COMPLETE")
