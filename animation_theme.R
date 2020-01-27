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
  plot.title = element_text(size = 40, hjust = 0.5, face = "bold", colour = "black", vjust = -1),
  plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "black"),
  plot.caption = element_text(size = 20, hjust = 0.5, face = "italic", color = "black",
                              margin = margin(0.5, 0, 0, 0, "cm")), # top, right, bottom, left
  plot.background = element_blank(),
  plot.margin = margin(2, 4, 2, 10, "cm") # top, right, bottom, left
)