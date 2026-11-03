# RQ1 figures. Reads the processed shots and the results tables written by steps 01-03.

source(here::here("R", "config.R"))
source(here::here("R", "court.R"))
source(here::here("R", "theme.R"))
source(here::here("R", "hex.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(dplyr); library(tidyr); library(patchwork) })

rq_dir <- here::here("RQ1_differences_today")
fig <- \(name) file.path(rq_dir, "results", "figures", name)
tab <- \(name) read.csv(file.path(rq_dir, "results", "tables", name))
LEAGUES <- c("NBA", "EuroLeague")

shots <- load_shots(TODAY_SEASONS) |> filter(phase == "regular")
court_both <- bind_rows(lapply(LEAGUES, \(l) transform(court_lines(l), league = l))) |>
  mutate(league = factor(league, LEAGUES))
court_layer <- geom_path(data = court_both, aes(x, y, group = piece), colour = "grey30",
                         linewidth = 0.3, inherit.aes = FALSE)
half_court <- coord_fixed(xlim = c(-7.7, 7.7), ylim = c(-1.7, 9.5), expand = FALSE)
caption_today <- "Regular season 2023/24-2025/26. Each panel shows its own league's court markings."
caption_plain <- "Regular season 2023/24-2025/26."
note_rim <- paste("Note: EuroLeague scorers place layups and dunks ~1.1 m from the hoop on average,",
                  "so close-range EuroLeague shots appear as a ring rather than at the rim.")

# ---- Fig 1: Goldsberry-style shot charts -----------------------------------------------
hb <- hex_bin(shots, by = "league") |>
  group_by(league) |>
  # hexagon size ~ frequency (capped so the rim does not dominate the scale)
  mutate(size = pmin(1, sqrt(share / quantile(share, 0.98)))) |>
  ungroup() |>
  filter(n >= 15)
poly <- hex_polygons(hb, scale = pmax(0.25, hb$size)) |> mutate(league = factor(league, LEAGUES))

f1 <- ggplot(poly, aes(px, py, group = hex_id, fill = pps)) +
  geom_polygon(colour = NA) +
  court_layer + half_court +
  facet_wrap(~ league) +
  ggbasket::scale_fill_vs_average(midpoint = 1.05, limits = c(0.6, 1.5), name = "Points per shot") +
  labs(title = "Same three-point share, different spots",
       subtitle = "11% of NBA shots come from the corner against 4% in the EuroLeague; 36% of EuroLeague shots are above-the-break threes (NBA 30%). Hexagon size = how often shots are taken there; colour = points per shot",
       caption = caption_today) +
  theme_project_court() +
  theme(legend.key.width = unit(1.4, "cm"))
ggsave(fig("fig1_shot_charts_today.png"), f1, width = 8.5, height = 5.8, dpi = 300)

# ---- Fig 2: difference in shot frequency on a common grid -------------------------------
CELL <- 0.5
diff_grid <- shots |>
  filter(abs(x) < 7.5, y < 9.5) |>
  mutate(gx = (floor(x / CELL) + 0.5) * CELL, gy = (floor(y / CELL) + 0.5) * CELL) |>
  count(league, gx, gy) |>
  group_by(league) |> mutate(share = 100 * n / sum(n)) |> ungroup() |>
  select(-n) |>
  pivot_wider(names_from = league, values_from = share, values_fill = 0) |>
  mutate(diff = NBA - EuroLeague)

three_both <- bind_rows(lapply(LEAGUES, \(l) transform(court_lines(l), league = l))) |>
  filter(piece %in% c("three", "outline", "lane", "hoop", "restricted"))
f2 <- ggplot(diff_grid, aes(gx, gy, fill = diff)) +
  geom_tile(width = CELL, height = CELL) +
  geom_path(data = three_both, aes(x, y, group = interaction(league, piece), linetype = league),
            inherit.aes = FALSE, colour = "grey20", linewidth = 0.35) +
  scale_linetype_manual(values = c(NBA = "solid", EuroLeague = "22"), name = "Lines") +
  scale_fill_gradient2(low = LEAGUE_COLOURS[["EuroLeague"]], high = LEAGUE_COLOURS[["NBA"]],
                       mid = "white", limits = c(-1.2, 1.2), oob = scales::squish,
                       name = "Share of all shots,\nNBA minus EuroLeague (pp)") +
  half_court +
  labs(title = "NBA: corners and rim. EuroLeague: short twos and the top of the arc",
       subtitle = "Blue = more common in the NBA, orange = more common in the EuroLeague (0.5 m cells)",
       caption = paste(caption_plain, "Solid lines: NBA court; dashed: EuroLeague (FIBA) court.<br>", note_rim)) +
  theme_project_court() + theme(legend.key.width = unit(1.2, "cm"))
ggsave(fig("fig2_shot_difference_map.png"), f2, width = 7.5, height = 7.2, dpi = 300)

# ---- Fig 3: zone profile (Bayesian estimates) -------------------------------------------
zones <- tab("shot_zones_today.csv") |>
  mutate(zone = factor(zone, rev(ZONES)), league = factor(league, LEAGUES))
dodge <- position_dodge(width = 0.5)
f3a <- ggplot(zones, aes(100 * share, zone, colour = league)) +
  geom_pointrange(aes(xmin = 100 * share_lo, xmax = 100 * share_hi), position = dodge, size = 0.35) +
  scale_colour_manual(values = LEAGUE_COLOURS, name = NULL) +
  labs(x = "Share of field-goal attempts (%)", y = NULL, subtitle = "**Shot mix**") + theme_project()
f3b <- ggplot(zones, aes(pps, zone, colour = league)) +
  geom_pointrange(aes(xmin = pps_lo, xmax = pps_hi), position = dodge, size = 0.35) +
  scale_colour_manual(values = LEAGUE_COLOURS, name = NULL) +
  labs(x = "Points per shot", y = NULL, subtitle = "**Shot value**") + theme_project() +
  theme(axis.text.y = element_blank())
f3 <- (f3a + f3b) + plot_layout(guides = "collect") +
  plot_annotation(title = "Corner threes: rare in the EuroLeague, but worth more",
                  subtitle = "Corner threes: 4% of EuroLeague shots at 1.28 points per shot, 11% of NBA shots at 1.16. Posterior means and 95% credible intervals (binomial models with team-season effects)",
                  caption = caption_plain, theme = theme_project()) &
  theme(legend.position = "top")
ggsave(fig("fig3_zone_profile.png"), f3, width = 9, height = 5.2, dpi = 300)

# ---- Fig 4: distance profile ----------------------------------------------------------------
dp <- tab("distance_profile_today.csv") |> mutate(league = factor(league, LEAGUES))
lines_df <- tibble(league = factor(LEAGUES, LEAGUES), d = c(7.24, 6.75))
f4a <- ggplot(dp, aes(band, 100 * share, colour = league)) +
  geom_vline(data = lines_df, aes(xintercept = d, colour = league), linetype = 2, linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = LEAGUE_COLOURS, name = NULL) +
  labs(x = NULL, y = "% of attempts (per 25 cm)", subtitle = "**How far from the basket?**") + theme_project()
f4b <- ggplot(dp |> filter(n >= 200), aes(band, 100 * fg, colour = league)) +
  geom_vline(data = lines_df, aes(xintercept = d, colour = league), linetype = 2, linewidth = 0.4) +
  geom_ribbon(aes(ymin = 100 * (fg - 2 * fg_se), ymax = 100 * (fg + 2 * fg_se), fill = league),
              alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = LEAGUE_COLOURS, name = NULL) +
  scale_fill_manual(values = LEAGUE_COLOURS, guide = "none") +
  labs(x = "Distance from the basket (m)", y = "FG%", subtitle = "**How accurate?**") + theme_project() +
  theme(legend.position = "none")
f4 <- (f4a / f4b) + plot_annotation(
  title = "Threes come from just behind each league's own line",
  subtitle = "Beyond 2.5 m the two leagues are equally accurate. Dashed lines: 3-point line at the top of the arc (NBA 7.24 m, EuroLeague 6.75 m)",
  caption = paste0(caption_plain, "<br>", note_rim), theme = theme_project())
ggsave(fig("fig4_distance_profile.png"), f4, width = 7.5, height = 6.5, dpi = 300)

# ---- Fig 5: box-score differences (standardised) --------------------------------------------
bd <- tab("box_differences_today.csv") |>
  filter(!metric %in% c("np", "tor")) |>   # tor duplicates turnovers per 100 possessions
  mutate(label = reorder(label, effect_size),
         group = factor(group, c("Pace", "Efficiency", "Four factors", "Shooting", "Other",
                                 "Fouls", "Rebounding", "Rotation")))
f5 <- ggplot(bd, aes(effect_size, label)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_pointrange(aes(xmin = es_lo, xmax = es_hi, colour = effect_size > 0), size = 0.3) +
  geom_text(aes(label = sprintf("NBA %s | EL %s", signif(nba, 3), signif(el, 3)),
                x = ifelse(effect_size > 0, es_hi + 0.05, es_lo - 0.05),
                hjust = ifelse(effect_size > 0, 0, 1)), size = 2.5, colour = "grey30") +
  scale_colour_manual(values = c(`TRUE` = LEAGUE_COLOURS[["NBA"]], `FALSE` = LEAGUE_COLOURS[["EuroLeague"]]),
                      labels = c(`TRUE` = "Higher in the NBA", `FALSE` = "Higher in the EuroLeague"), name = NULL) +
  scale_x_continuous(expand = expansion(mult = 0.35)) +
  labs(x = "Difference NBA - EuroLeague, in game-to-game SDs", y = NULL,
       title = "The two leagues now shoot alike; the NBA plays faster and the EuroLeague fouls far more",
       subtitle = "Posterior mean and 95% CrI from mixed models with team- and opponent-season effects",
       caption = caption_plain) +
  theme_project()
ggsave(fig("fig5_box_differences.png"), f5, width = 8.5, height = 6.5, dpi = 300)

# ---- Fig 6: modelled accuracy surface ---------------------------------------------------------
sp_path <- file.path(rq_dir, "results", "tables", "spatial_accuracy_grid.csv")
if (file.exists(sp_path)) {
  sp <- read.csv(sp_path) |> mutate(league = factor(league, LEAGUES))
  sp <- bind_rows(sp, sp |> mutate(x_abs = -x_abs))  # mirror back to both sides
  f6 <- ggplot(sp, aes(x_abs, y, fill = 100 * p_make)) +
    geom_tile(width = 0.5, height = 0.5) +
    court_layer + half_court + facet_wrap(~ league) +
    scale_fill_viridis_c(option = "magma", limits = c(25, 65), oob = scales::squish, name = "P(make), %") +
    labs(title = "EuroLeague accuracy falls off faster on the mid-range wings and deep beyond the arc",
         subtitle = "Bayesian binomial model with a smooth 2-D surface per league (posterior mean)",
         caption = caption_today) +
    theme_project_court() + theme(legend.key.width = unit(1.2, "cm"))
  ggsave(fig("fig6_accuracy_surface.png"), f6, width = 8.5, height = 5.8, dpi = 300)
}
