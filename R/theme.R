# Shared ggplot2 look for all figures.

library(ggplot2)

# The look of every figure comes from the ggbasket package (white page, ltc colours, Fira fonts,
# finding-first titles, "Data / Graphic" caption): https://loukesio.github.io/ggbasket/
library(ggbasket)

LEAGUE_COLOURS <- c(ggbasket_colours$league[c("NBA", "EuroLeague")], Euroleague = ggbasket_colours$league[["EuroLeague"]])
PHASE_COLOURS  <- c(regular = "grey55", playoffs = ggbasket_colours$above)

theme_project <- function(base_size = 10) {
  f <- ggbasket_fonts()
  (theme_ggbasket(base_size = base_size + 1.5) +
     theme(legend.position = "top", legend.justification = "left", plot.caption.position = "plot")) %+replace%
    # plain-text caption (keeps "\n" line breaks), in ggbasket's monospaced caption font
    theme(plot.caption = ggtext::element_textbox_simple(family = f$mono, colour = "grey45", size = rel(0.72),
                                                       lineheight = 1.15, margin = margin(t = 10)))
}

# captions in ggbasket's style; "\n" in older caption strings becomes a markdown line break
project_caption <- function(data, note = NULL) {
  ggbasket_caption(data, note = note)
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

# ggbasket's court theme with the project's wrapping caption
theme_project_court <- function(base_size = 11.5) {
  f <- ggbasket_fonts()
  ggbasket::theme_court(base_size = base_size) %+replace%
    theme(plot.caption = ggtext::element_textbox_simple(family = f$mono, colour = "grey45", size = rel(0.72),
                                                        lineheight = 1.15, margin = margin(t = 10)))
}
