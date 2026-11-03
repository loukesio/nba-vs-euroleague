# Shared ggplot2 look for all figures.

library(ggplot2)

LEAGUE_COLOURS <- c(NBA = "#1F5FA8", EuroLeague = "#E07B00", Euroleague = "#E07B00")
PHASE_COLOURS  <- c(regular = "grey55", playoffs = "#B2182B")

theme_project <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.15)),
      plot.subtitle = element_text(colour = "grey35", margin = margin(b = 6)),
      plot.caption = element_text(colour = "grey45", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92"),
      strip.text = element_text(face = "bold", hjust = 0),
      legend.position = "top",
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

# Rule changes to mark on trend plots (season = starting year)
RULE_CHANGES <- tibble::tribble(
  ~season, ~league,      ~label,
  2010,    "EuroLeague", "FIBA 3pt line 6.75 m",
  2014,    "EuroLeague", "14-s reset after ORB (FIBA)",
  2018,    "NBA",        "14-s reset after ORB",
  2019,    "both",       "COVID: NBA bubble, EuroLeague season cancelled",
  2022,    "NBA",        "Take-foul rule"
)
