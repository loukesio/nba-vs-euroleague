# RQ2 figures: smooth trends, convergence, lead-lag, and shot charts over time.

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
source(here::here("R", "court.R"))
source(here::here("R", "theme.R"))
source(here::here("R", "hex.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(dplyr); library(tidyr); library(patchwork) })

rq_dir <- here::here("RQ2_trends_leadlag")
fig <- \(name) file.path(rq_dir, "results", "figures", name)
tab <- \(name) read.csv(file.path(rq_dir, "results", "tables", name))
LEAGUES <- c("NBA", "EuroLeague")
season_axis <- scale_x_continuous(breaks = seq(2010, 2025, 5), labels = \(s) season_label(s))

# ---- Fig 1: smooth trends for the headline metrics --------------------------------------------
KEY <- c("pace40", "ortg", "p3a_share", "efg", "p2_pct", "p3_pct", "fta_100", "ft_per_foul",
         "fcm_100", "ast_100", "tov_100", "blk_100", "drb_rate", "entropy_min")
curves <- tab("smooth_trend_curves.csv") |>
  filter(metric %in% KEY) |>
  left_join(METRICS |> select(metric, label), by = "metric") |>
  mutate(label = factor(label, METRICS$label[match(KEY, METRICS$metric)]),
         league = factor(league, LEAGUES))
rules <- RULE_CHANGES |> filter(season >= 2011) |> distinct(season)

f1 <- ggplot(curves, aes(season, colour = league, fill = league)) +
  geom_vline(data = rules, aes(xintercept = season - 0.5), colour = "grey80", linewidth = 0.3) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
  geom_line(aes(y = fit), linewidth = 0.7) +
  geom_point(aes(y = obs_mean), size = 0.8, alpha = 0.7) +
  facet_wrap(~ label, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = LEAGUE_COLOURS, name = NULL) +
  scale_fill_manual(values = LEAGUE_COLOURS, name = NULL) +
  season_axis +
  labs(x = NULL, y = NULL,
       title = "In 15 seasons the NBA caught up with the EuroLeague's three-point habit, while the pace gap widened",
       subtitle = "Points: season means (regular season). Lines and bands: Bayesian smooth trend (RW2), 95% CrI. Grey lines: rule changes.",
       caption = paste("Grey lines: 2014/15 14-s reset after offensive rebounds (FIBA/EuroLeague); 2018/19 same rule in the NBA;",
                       "2019/20 COVID (NBA bubble, EuroLeague cancelled after the regular season); 2022/23 NBA take-foul rule.")) +
  theme_project() +
  theme(axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7),
        strip.text = element_text(size = 9.5, face = "bold", hjust = 0), panel.spacing.y = unit(0.9, "lines"))
ggsave(fig("fig1_trends.png"), f1, width = 7.5, height = 10, dpi = 300)

# ---- Fig 2: convergence (gap NBA - EuroLeague over time, comparable metrics) ---------------------
gaps <- tab("smooth_trend_gap_curves.csv") |>
  filter(metric %in% KEY) |>
  left_join(METRICS |> select(metric, label), by = "metric") |>
  mutate(label = factor(label, METRICS$label[match(KEY, METRICS$metric)]))
# colour each panel by the posterior probability that the gap shrank between 2010/11 and 2025/26
shrank <- tab("convergence_2010_2025.csv") |> select(metric, p_gap_shrank)
gaps <- gaps |> left_join(shrank, by = "metric") |>
  mutate(verdict = case_when(p_gap_shrank >= 0.9 ~ "Converged", p_gap_shrank <= 0.1 ~ "Drifted apart",
                             TRUE ~ "Unclear"))
VERDICT_COLOURS <- c(Converged = ggbasket_colours$defence, `Drifted apart` = ggbasket_colours$above, Unclear = "grey45")
f2 <- ggplot(gaps, aes(season, fit, colour = verdict, fill = verdict)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ label, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = VERDICT_COLOURS, name = NULL) +
  scale_fill_manual(values = VERDICT_COLOURS, name = NULL) +
  season_axis +
  labs(x = NULL, y = "NBA minus EuroLeague",
       title = "Shot mix and shooting converged; pace, free throws and rebounding drifted apart",
       subtitle = paste("Difference between the two leagues' smooth trends (95% credible band); 0 = no difference.",
                        "Colour = posterior probability that the gap shrank from 2010/11 to 2025/26:",
                        "converged (\u2265 0.9), drifted apart (\u2264 0.1) or unclear.")) +
  theme_project() +
  theme(axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7),
        strip.text = element_text(size = 9.5, face = "bold", hjust = 0), panel.spacing.y = unit(0.9, "lines"))
ggsave(fig("fig2_convergence.png"), f2, width = 7.5, height = 10, dpi = 300)

# ---- Fig 3: lead-lag posterior --------------------------------------------------------------------
lags <- tab("lead_lag_posterior.csv")
ord <- lags |> group_by(label) |> summarise(e = sum(k * p)) |> arrange(e) |> pull(label)
f3 <- ggplot(lags |> mutate(label = factor(label, ord)), aes(k, label, fill = p)) +
  geom_tile(colour = "white") +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.3) +
  scale_fill_viridis_c(option = "mako", direction = -1, limits = c(0, 1), name = "Posterior\nprobability") +
  scale_x_continuous(breaks = -5:5) +
  annotate("text", x = c(-3.5, 3.5), y = length(ord) + 0.9,
           label = c("<- NBA follows", "EuroLeague follows ->"), size = 3, colour = "grey30") +
  coord_cartesian(clip = "off") +
  labs(x = "Lag k (seasons)", y = NULL,
       title = "On threes the EuroLeague moves with the NBA or a season behind; on most other metrics there is no clear leader",
       subtitle = "Posterior probability of each lag in the shift model (EuroLeague = NBA trend shifted by k seasons)",
       caption = paste("Probability piling up at the edges (k = -5 or 5) means no lag inside the window fits well:",
                       "read it as 'no clear lead-lag', not as a 5-season lag. EuroLeague seasons 2006/07-2020/21.")) +
  theme_project() +
  theme(panel.grid = element_blank(), plot.margin = margin(18, 6, 6, 6),
        legend.position = "right", legend.title = element_text(size = 8))
ggsave(fig("fig3_lead_lag.png"), f3, width = 8, height = 6.5, dpi = 300)

# ---- Fig 4: "the EuroLeague today = the NBA in season X" -----------------------------------------
tgap <- tab("time_gap_today.csv") |> filter(p_reached > 0.8)
if (nrow(tgap)) {
  f4 <- ggplot(tgap, aes(nba_season_equiv, reorder(label, nba_season_equiv))) +
    geom_pointrange(aes(xmin = lo, xmax = hi), colour = LEAGUE_COLOURS[["NBA"]], size = 0.35) +
    geom_vline(xintercept = 2025, linetype = 2, colour = "grey50") +
    scale_x_continuous(breaks = seq(2001, 2025, 4), labels = \(s) season_label(s)) +
    labs(x = "NBA season with the same level as the EuroLeague in 2025/26", y = NULL,
         title = "On volume, the EuroLeague of 2025/26 looks like the NBA of three to five seasons ago",
         subtitle = "Median and 90% interval over posterior draws; metrics where the NBA reached the EuroLeague's current level") +
    theme_project()
  ggsave(fig("fig4_time_gap.png"), f4, width = 8, height = 4.5, dpi = 300)
}

# ---- Fig 5: shot charts over time ----------------------------------------------------------------
SNAP <- c(2010, 2014, 2018, 2022, 2025)
shots <- load_shots(SNAP) |> filter(phase == "regular")
hb <- hex_bin(shots, by = c("league", "season"), xbins = 25) |>
  group_by(league, season) |>
  mutate(size = pmin(1, sqrt(share / quantile(share, 0.98)))) |> ungroup() |>
  filter(n >= 10)
poly <- hex_polygons(hb, xbins = 25, scale = pmax(0.2, hb$size)) |>
  mutate(league = factor(league, LEAGUES), season_lab = season_label(season))
court_both <- bind_rows(lapply(LEAGUES, \(l) transform(court_lines(l), league = l))) |>
  mutate(league = factor(league, LEAGUES))
f5 <- ggplot(poly, aes(px, py, group = hex_id, fill = 100 * share)) +
  geom_polygon() +
  geom_path(data = court_both, aes(x, y, group = piece), colour = "grey30", linewidth = 0.25,
            inherit.aes = FALSE) +
  coord_fixed(xlim = c(-7.7, 7.7), ylim = c(-1.7, 9.5), expand = FALSE) +
  facet_grid(league ~ season_lab, switch = "y") +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt", name = "% of shots") +
  labs(title = "How shot charts changed, 2010/11 to 2025/26",
       subtitle = "Regular season; hexagon size and colour = share of the league's attempts") +
  theme_court(9) + theme(legend.key.width = unit(1.2, "cm"))
ggsave(fig("fig5_shot_charts_over_time.png"), f5, width = 12, height = 5.4, dpi = 300)
