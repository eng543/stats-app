our_theme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(color = 'dark grey'),
  plot.title = element_text(size = 26),
  plot.subtitle = element_text(size = 16),
  text = element_text(size = 22),
  axis.title = element_text(size = 20),
  strip.background = element_rect(fill='white'),
  strip.background.x = element_rect(color = 'dark grey', size = 1.2),
  strip.text = element_text(face = 'bold'),
  panel.spacing = unit(2, "lines"),
  axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
  axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))
)