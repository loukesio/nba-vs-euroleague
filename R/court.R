# Court geometry (metres, origin = centre of the hoop, y towards half court),
# shot-zone definitions and a ggplot2 half-court layer for NBA and FIBA/EuroLeague rules.

library(ggplot2)

COURT <- list(
  NBA = list(
    half_length = 14.325, width = 15.24, hoop_baseline = 1.600,
    arc_r = 7.24, corner_x = 6.71, lane_w = 4.88, ft_line = 5.79,
    restricted_r = 1.22, board_baseline = 1.22, board_w = 1.83, circle_r = 1.83
  ),
  EuroLeague = list(
    half_length = 14.000, width = 15.00, hoop_baseline = 1.575,
    arc_r = 6.75, corner_x = 6.60, lane_w = 4.90, ft_line = 5.80,
    restricted_r = 1.25, board_baseline = 1.20, board_w = 1.80, circle_r = 1.80
  )
)
# y (from hoop) where the straight corner line meets the arc
for (lg in names(COURT)) COURT[[lg]]$corner_y <- with(COURT[[lg]], sqrt(arc_r^2 - corner_x^2))

# ---- shot zones --------------------------------------------------------------------
ZONES <- c("Rim area", "Short mid-range", "Long mid-range", "Corner 3", "Above-break 3")
# Rim area radius, same for both leagues. 1.75 m rather than the 1.25 m restricted area,
# because EuroLeague operators record layups/dunks ~1.1 m from the hoop on average
# (a ring at 1.0-1.5 m), while NBA tracking puts them at 0-0.5 m.
RIM_R <- 1.75
SHORT_MID_R <- 4.25  # ~14 ft

shot_zone <- function(league, x, y, dist, value) {
  corner_y <- ifelse(league == "NBA", COURT$NBA$corner_y, COURT$EuroLeague$corner_y)
  z <- dplyr::case_when(
    value == 3 & y <= corner_y ~ "Corner 3",
    value == 3                 ~ "Above-break 3",
    dist < RIM_R               ~ "Rim area",
    dist < SHORT_MID_R         ~ "Short mid-range",
    TRUE                       ~ "Long mid-range"
  )
  factor(z, levels = ZONES)
}

# ---- drawing -------------------------------------------------------------------------
arc_pts <- function(r, from, to, n = 200, x0 = 0, y0 = 0) {
  t <- seq(from, to, length.out = n)
  data.frame(x = x0 + r * cos(t), y = y0 + r * sin(t))
}

court_lines <- function(league) {
  k <- COURT[[league]]
  base <- -k$hoop_baseline
  top  <- k$half_length - k$hoop_baseline
  w2 <- k$width / 2
  a0 <- asin(k$corner_y / k$arc_r)
  ft_y <- k$ft_line - k$hoop_baseline
  lines <- list(
    outline  = data.frame(x = c(-w2, -w2, w2, w2, -w2), y = c(top, base, base, top, top)),
    # 3-point line: straight corner segment, arc, straight corner segment
    three    = rbind(data.frame(x = -k$corner_x, y = base),
                     arc_pts(k$arc_r, pi - a0, a0),
                     data.frame(x = k$corner_x, y = base)),
    lane    = data.frame(x = c(-k$lane_w / 2, -k$lane_w / 2, k$lane_w / 2, k$lane_w / 2),
                          y = c(base, ft_y, ft_y, base)),
    ft_circle = arc_pts(k$circle_r, 0, pi, y0 = ft_y),
    restricted = arc_pts(k$restricted_r, 0, pi),
    hoop     = arc_pts(0.225, 0, 2 * pi),
    board    = data.frame(x = c(-k$board_w / 2, k$board_w / 2),
                          y = rep(k$board_baseline - k$hoop_baseline, 2)),
    centre   = arc_pts(k$circle_r, pi, 2 * pi, y0 = top)
  )
  dplyr::bind_rows(lines, .id = "piece")
}

# A ggplot2 layer list: court lines for one league (or both via facet data).
geom_court <- function(league = "NBA", colour = "grey25", linewidth = 0.35) {
  d <- dplyr::bind_rows(lapply(league, \(l) transform(court_lines(l), league = l)))
  list(
    geom_path(data = d, aes(x, y, group = interaction(league, piece)),
              colour = colour, linewidth = linewidth, inherit.aes = FALSE),
    coord_fixed(xlim = c(-7.7, 7.7), ylim = c(-1.7, 9.5), expand = FALSE)
  )
}

theme_court <- function(base_size = 10) {
  theme_void(base_size = base_size) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(face = "bold", size = rel(1.15)),
          plot.subtitle = element_text(colour = "grey35", margin = margin(b = 6)),
          plot.caption = element_text(colour = "grey45", hjust = 0),
          strip.text = element_text(face = "bold", margin = margin(b = 3)),
          legend.position = "bottom")
}
