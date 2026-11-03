# RQ4 figures. Reads the tables written by steps 01-02, the processed shots, and team names from raw data.

source(here::here("R", "config.R"))
source(here::here("R", "court.R"))
source(here::here("R", "theme.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(dplyr); library(tidyr); library(purrr); library(patchwork); library(ggrepel) })

rq_dir <- here::here("RQ4_ahead_of_their_time")
fig <- \(name) file.path(rq_dir, "results", "figures", name)
tab <- \(name) read.csv(file.path(rq_dir, "results", "tables", name))
LEAGUES <- c("NBA", "EuroLeague")
AHEAD_FILL <- "#E6F1F2"; BEHIND_FILL <- "#F2F2F4"   # cool tints, no beige

# ---- names -----------------------------------------------------------------------------------
team_names <- bind_rows(
  map_dfr(SEASONS, \(s) arrow::read_parquet(
    file.path(RAW_NBA, "player_box", sprintf("player_box_%d.parquet", s)),
    col_select = c("team_abbreviation", "team_short_display_name")) |>
      distinct() |> transmute(league = "NBA", season = s, team = team_abbreviation,
                              name = team_short_display_name)),
  map_dfr(SEASONS, function(s) {
    g <- jsonlite::fromJSON(file.path(RAW_EL, "games", sprintf("E%d.json", s)))$data
    tibble(league = "EuroLeague", season = s,
           team = c(g$local$club$code, g$road$club$code),
           name = coalesce(na_if(c(g$local$club$editorialName, g$road$club$editorialName), ""),
                           c(g$local$club$abbreviatedName, g$road$club$abbreviatedName))) |>
      distinct(team, .keep_all = TRUE)
  })
) |> distinct(league, season, team, .keep_all = TRUE)
write.csv(team_names, file.path(rq_dir, "results", "tables", "team_names.csv"), row.names = FALSE)

# ---- data -------------------------------------------------------------------------------------
tp <- tab("team_time_position.csv") |>
  left_join(tab("team_typicality.csv"), by = c("league", "season", "team")) |>
  left_join(team_names, by = c("league", "season", "team")) |>
  mutate(league = factor(league, LEAGUES), label = paste(name, season_short(season)))
pl <- tab("player_styles.csv") |>
  mutate(league = factor(league, LEAGUES), name = nice_player(player))

# Shared backdrop for "time machine" plots: above the diagonal = ahead, below = behind
time_backdrop <- function(lo = 2009.5, hi = 2025.5, text = TRUE) {
  out <- list(
    annotate("polygon", x = c(lo, lo, hi), y = c(lo, hi, hi), fill = AHEAD_FILL),
    annotate("polygon", x = c(lo, hi, hi), y = c(lo, lo, hi), fill = BEHIND_FILL),
    geom_abline(slope = 1, intercept = 0, colour = "grey55", linetype = "22", linewidth = 0.4)
  )
  if (text) out <- c(out, list(
    annotate("text", x = lo + 0.4, y = lo + 0.62 * (hi - lo), label = "AHEAD OF\nTHEIR TIME", hjust = 0,
             vjust = 1, lineheight = 0.9,
             colour = "#00798c", fontface = "bold", size = 3.4),
    annotate("text", x = hi - 0.4, y = lo + 0.3, label = "BEHIND THEIR TIME", hjust = 1, vjust = 0,
             colour = "#8BA7C4", fontface = "bold", size = 3.4)))
  out
}
season_axis <- list(
  scale_x_continuous(breaks = seq(2010, 2025, 3), labels = season_label, expand = c(0, 0)),
  scale_y_continuous(breaks = seq(2010, 2025, 3), labels = season_label, expand = c(0, 0)),
  coord_cartesian(xlim = c(2009.5, 2025.5), ylim = c(2009.5, 2025.5))
)

# ---- Fig 1: the time machine, teams -------------------------------------------------------------
pick_ahead  <- tp |> filter(years_ahead >= 3, gain >= 0.6) |> group_by(league) |>
  slice_max(years_ahead * gain, n = 6) |> ungroup()
pick_behind <- tp |> filter(years_ahead <= -3, gain >= 0.6) |> group_by(league) |>
  slice_max(-years_ahead * gain, n = 4) |> ungroup()
picks <- bind_rows(pick_ahead, pick_behind)

f1 <- ggplot(tp, aes(season, tau)) +
  time_backdrop() +
  geom_point(aes(size = gain, colour = league), alpha = 0.28, stroke = 0,
             position = position_jitter(width = 0.22, height = 0, seed = 1)) +
  geom_linerange(data = picks, aes(ymin = tau_lo, ymax = tau_hi), colour = "grey20", linewidth = 0.35) +
  geom_point(data = picks, aes(size = gain, fill = league), shape = 21, colour = "grey10", stroke = 0.5) +
  geom_text_repel(family = ggbasket_fonts()$body, data = picks, aes(label = label), size = 2.9, lineheight = 0.9, seed = 3,
                  min.segment.length = 0, segment.colour = "grey40", segment.size = 0.3,
                  box.padding = 0.5, max.overlaps = Inf) +
  season_axis +
  facet_wrap(~ league) +
  scale_colour_manual(values = LEAGUE_COLOURS, guide = "none") +
  scale_fill_manual(values = LEAGUE_COLOURS, guide = "none") +
  scale_size_continuous(range = c(0.6, 4), name = "How much closer to\nthat season's league") +
  labs(title = "Houston and Real Madrid played their league's future; Chicago 2022/23 and Partizan 2022/23 its past",
       subtitle = paste("Each dot is a team-season. Height = the season whose league-average style the team",
                        "is closest to.\nLabelled: the clearest cases (bars = 90% credible interval)."),
       x = "Season played", y = "Season it played like",
       caption = paste("Style = pace, shot mix (5 zones), free throws, assists, turnovers, offensive rebounds,",
                       "blocks, steals, fouls, rotation.\nThe league's path ends in 2025/26, so teams at the",
                       "top edge may be even further ahead.")) +
  theme_project() +
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.spacing = unit(1.2, "lines"),
        strip.text = element_text(size = 12))
ggsave(fig("fig1_time_machine_teams.png"), f1, width = 11, height = 6.2, dpi = 300)

# ---- Fig 2: who stood out each season ----------------------------------------------------------
most_diff <- tp |> group_by(league, season) |> slice_max(p_most_different, n = 1, with_ties = FALSE) |>
  ungroup() |> filter(p_most_different >= 0.5)
most_typ <- tp |> group_by(league, season) |> slice_max(p_most_typical, n = 1, with_ties = FALSE) |> ungroup()
# multi-season outlier streaks get a connecting line (e.g. Morey's Rockets)
streaks <- most_diff |> group_by(league, team) |> filter(n() >= 3) |> ungroup()

f2 <- ggplot(tp, aes(season, distance)) +
  geom_point(colour = "grey70", size = 1.3, alpha = 0.7,
             position = position_jitter(width = 0.18, height = 0, seed = 2)) +
  geom_line(data = streaks, aes(group = team), colour = ggbasket_colours$above, linewidth = 0.8, alpha = 0.5) +
  geom_point(data = most_diff, colour = ggbasket_colours$above, size = 2.6) +
  geom_point(data = most_typ, aes(colour = league), shape = 18, size = 3.4) +
  geom_text_repel(family = ggbasket_fonts()$body, data = most_diff, aes(label = name), size = 2.7, colour = ggbasket_colours$above, seed = 4,
                  nudge_y = 0.6, direction = "y", segment.size = 0.25, min.segment.length = 0) +
  geom_text(data = most_typ, aes(label = name, colour = league), size = 2.3, vjust = 2.1) +
  facet_wrap(~ league, ncol = 1) +
  scale_x_continuous(breaks = SEASONS, labels = season_short) +
  scale_colour_manual(values = LEAGUE_COLOURS, guide = "none") +
  labs(title = "Houston were the NBA's most different team for seven straight seasons",
       subtitle = paste("Every team's distance from its league's average style that season.",
                        "<span style='color:#b5515b'>**Red**</span>: most different team (posterior",
                        "probability >= 0.5). Diamonds: most typical team."),
       x = NULL, y = "Distance from the league average\n(in typical team-to-team differences)",
       caption = "Regular season. Lines join a team that was the most different in three or more seasons.") +
  theme_project() +
  theme(plot.subtitle = ggtext::element_markdown(), panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 12))
ggsave(fig("fig2_outlier_teams.png"), f2, width = 11, height = 7.5, dpi = 300)

# ---- Fig 3: careers through basketball time ------------------------------------------------------
CAREERS <- tribble(
  ~league,      ~player,                ~note,
  "NBA",        "Brook Lopez",          "learned the three in 2016/17",
  "NBA",        "Robin Lopez",          "his twin, who never did",
  "NBA",        "Al Horford",           "the stretch big, early",
  "NBA",        "Nikola Jokic",         "the passing centre",
  "NBA",        "Kyle Korver",          "the shooter's shooter",
  "NBA",        "DeMar DeRozan",        "the last mid-range star",
  "EuroLeague", "NAVARRO, JUAN CARLOS", "La Bomba",
  "EuroLeague", "TEODOSIC, MILOS",      "the pull-up passer",
  "EuroLeague", "LLULL, SERGIO",        "the deep-range guard",
  "EuroLeague", "FOTSIS, ANTONIS",      "the stretch four",
  "EuroLeague", "TAVARES, WALTER",      "the rim protector",
  "EuroLeague", "SHENGELIA, TORNIKE",   "the inside forward"
)
car <- pl |>
  inner_join(CAREERS, by = c("league", "player")) |>
  mutate(facet = factor(paste0("**", name, "**<br><span style='color:grey40'>", note, "</span>"),
                        levels = unique(paste0("**", nice_player(CAREERS$player),
                                               "**<br><span style='color:grey40'>", CAREERS$note, "</span>"))))

f3 <- ggplot(car, aes(season, tau)) +
  time_backdrop(text = FALSE) +
  geom_linerange(aes(ymin = tau_lo, ymax = tau_hi, colour = league), alpha = 0.35, linewidth = 1.6) +
  geom_line(aes(colour = league), linewidth = 0.5) +
  geom_point(aes(colour = league, size = gain)) +
  season_axis +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025), labels = season_short, expand = c(0, 0)) +
  scale_y_continuous(breaks = c(2010, 2015, 2020, 2025), labels = season_short, expand = c(0, 0)) +
  facet_wrap(~ facet, ncol = 6) +
  scale_colour_manual(values = LEAGUE_COLOURS, guide = "none") +
  scale_size_continuous(range = c(0.8, 3), guide = "none") +
  labs(title = "EuroLeague guards like Navarro, Teodosic and Spanoulis were ahead of their time for most of their careers",
       subtitle = paste("Each point is one season of a player's career: the season whose average player",
                        "at the same position he resembles most.\nAbove the diagonal = ahead of his time,",
                        "below = behind. Bars = 90% credible interval; bigger points = clearer signal."),
       x = "Season played", y = "Season he played like",
       caption = "Positions: guard, forward, centre. Top row NBA, bottom row EuroLeague.") +
  theme_project() +
  theme(strip.text = ggtext::element_markdown(hjust = 0, lineheight = 1.1, size = 9),
        panel.grid.major = element_blank(), panel.spacing = unit(0.8, "lines"),
        axis.text = element_text(size = 7))
ggsave(fig("fig3_career_paths.png"), f3, width = 13, height = 5.6, dpi = 300)

# ---- Fig 4: the Lopez twins on the court -----------------------------------------------------------
twins <- load_shots(c(2015, 2016)) |>
  filter(league == "NBA", phase == "regular", player == "Lopez",
         (season == 2015 & team %in% c("BKN", "NYK")) | (season == 2016 & team %in% c("BKN", "CHI"))) |>
  mutate(who = if_else(team == "BKN", "Brook Lopez", "Robin Lopez"),
         who = factor(who, c("Brook Lopez", "Robin Lopez")),
         when = factor(season_label(season)),
         kind = if_else(value == 3, "Three", "Two"))
twin_n <- twins |> group_by(who, when) |>
  summarise(lab = sprintf("%d shots, %d threes", n(), sum(value == 3)), .groups = "drop")
court_nba <- court_lines("NBA")

f4a <- ggplot(twins, aes(x, y)) +
  geom_point(aes(colour = kind), size = 0.55, alpha = 0.55, stroke = 0) +
  geom_path(data = court_nba, aes(x, y, group = piece), colour = "grey30", linewidth = 0.3,
            inherit.aes = FALSE) +
  geom_text(data = twin_n, aes(x = 0, y = 9.1, label = lab), size = 2.8, colour = "grey30") +
  coord_fixed(xlim = c(-7.7, 7.7), ylim = c(-1.7, 9.5), expand = FALSE) +
  facet_grid(who ~ when, switch = "y") +
  scale_colour_manual(values = c(Three = ggbasket_colours$highlight, Two = "grey45"), name = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 2.5, alpha = 1))) +
  theme_project_court() +
  theme(strip.text.y.left = element_text(angle = 90, face = "bold"), legend.position = "top")

twin_path <- pl |> filter(name %in% c("Brook Lopez", "Robin Lopez"))
f4b <- ggplot(twin_path, aes(season, tau, colour = name)) +
  time_backdrop(text = FALSE) +
  geom_linerange(aes(ymin = tau_lo, ymax = tau_hi), alpha = 0.3, linewidth = 1.8) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  annotate("text", x = 2016.3, y = 2024.6, label = "Brook starts\nshooting threes", hjust = 0,
           vjust = 1, size = 3, colour = ggbasket_colours$highlight, lineheight = 0.9) +
  annotate("text", x = 2019.2, y = 2021.5, label = "Robin's one season\nof threes, next to Brook\nin Milwaukee",
           hjust = 1, vjust = 1, size = 3, colour = "grey30", lineheight = 0.9) +
  season_axis +
  scale_colour_manual(values = c("Brook Lopez" = ggbasket_colours$highlight, "Robin Lopez" = "grey35"), name = NULL) +
  labs(x = "Season played", y = "Season he played like (centres)") +
  theme_project() + theme(panel.grid.major = element_blank(), legend.position = "top")

f4 <- (f4a | f4b) + plot_layout(widths = c(1.25, 1)) +
  plot_annotation(
    title = "Same genes, different eras: Brook Lopez jumped a decade in one summer",
    subtitle = paste("In 2016/17 Brook Lopez moved out to the three-point line and jumped from the 2010s",
                     "centre to the centre of today. Robin stayed near the rim for most of his career."),
    caption = "Regular-season shots. Right: time position among NBA centres (90% credible intervals).",
    theme = theme_project())
ggsave(fig("fig4_lopez_twins.png"), f4, width = 12, height = 6.4, dpi = 300)

# ---- Fig 5: the league in one player ---------------------------------------------------------------
l1 <- pl |> group_by(league, season) |> slice_max(p_league_in_one, n = 1, with_ties = FALSE) |> ungroup() |>
  add_count(league, name, name = "times") |>
  left_join(team_names |> mutate(league = factor(league, LEAGUES)),
            by = c("league", "season", "team"), suffix = c("", "_team"))
f5 <- ggplot(l1, aes(season, 0)) +
  geom_tile(aes(fill = league, alpha = p_league_in_one), height = 1, width = 0.94) +
  geom_text(aes(label = name, fontface = if_else(times > 1, "bold", "plain")), angle = 90, hjust = 0,
            y = -0.44, size = 3.1, colour = "grey10") +
  geom_text(aes(label = sprintf("%s · %.0f%%", name_team, 100 * p_league_in_one)), angle = 90,
            hjust = 1, y = 0.46, size = 2.4, colour = "grey25") +
  facet_wrap(~ league, ncol = 1) +
  scale_x_continuous(breaks = SEASONS, labels = season_short, expand = c(0.01, 0.01)) +
  scale_fill_manual(values = LEAGUE_COLOURS, guide = "none") +
  scale_alpha_continuous(range = c(0.12, 0.65), limits = c(0, 1), guide = "none") +
  labs(title = "Nikola Kalinic was the EuroLeague's most typical player in six seasons",
       subtitle = paste("Each season, the rotation player whose style is closest to the league's average style.",
                        "Shading and % = posterior probability of being the closest.\nBold = chosen in more",
                        "than one season: Nikola Kalinic was the EuroLeague's 'average' player six times."),
       x = NULL, y = NULL) +
  theme_project() +
  theme(axis.text.y = element_blank(), panel.grid.major = element_blank(), strip.text = element_text(size = 12))
ggsave(fig("fig5_league_in_one_player.png"), f5, width = 12, height = 7, dpi = 300)

# ---- Fig 6: unicorns, the most unusual players --------------------------------------------------
dev <- tab("player_profile_deviations.csv")
FEATURE_LABELS <- c(n_rim = "Rim", n_short_mid = "Short mid", n_long_mid = "Long mid", n_corner3 = "Corner 3",
                    n_above3 = "Above-break 3", ast = "Assists", tov = "Turnovers", orb = "Off. reb.",
                    drb = "Def. reb.", blk = "Blocks", stl = "Steals", pf = "Fouls", ftr = "FTA / FGA")
unicorns <- pl |> group_by(league, player) |> slice_max(dist_position, n = 1) |> ungroup() |>
  group_by(league) |> slice_max(dist_position, n = 5) |> ungroup()
uni <- dev |>
  semi_join(unicorns, by = c("league", "season", "player_id")) |>
  pivot_longer(all_of(names(FEATURE_LABELS)), names_to = "feature", values_to = "z") |>
  mutate(feature = factor(feature, names(FEATURE_LABELS), FEATURE_LABELS),
         z = pmax(-3, pmin(6, z)),
         league = factor(league, LEAGUES),
         who = paste0(nice_player(player), "\n", season_label(season), " · ",
                      c(G = "guard", F = "forward", C = "centre")[position])) |>
  arrange(league, desc(season))
uni$who <- factor(uni$who, unique(uni$who))
OFFSET <- 3
f6 <- ggplot(uni, aes(feature, z + OFFSET)) +
  geom_hline(yintercept = OFFSET, colour = "grey55", linewidth = 0.3) +
  geom_col(aes(fill = z > 0), width = 0.85, colour = NA) +
  geom_text(data = uni |> group_by(who) |> slice_max(z, n = 3) |> ungroup(),
            aes(label = feature, y = pmax(z, 0) + OFFSET + 0.9),
            size = 2.1, colour = "grey20") +
  coord_polar(clip = "off") +
  scale_y_continuous(limits = c(0, 6 + OFFSET + 1.3)) +
  facet_wrap(~ who, ncol = 5) +
  scale_fill_manual(values = c(`TRUE` = "#B2182B", `FALSE` = "#6E9BC5"),
                    labels = c(`TRUE` = "More than a typical player of his position",
                               `FALSE` = "Less"), name = NULL) +
  labs(title = "Unicorns: the players least like anyone else at their position, 2010-2026",
       subtitle = paste("Style fingerprint against the average player of the same position and season",
                        "(the grey circle). Top row NBA, bottom row EuroLeague.\nBars are in units of the",
                        "typical player-to-player difference; the largest features are named."),
       x = NULL, y = NULL) +
  theme_project() +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        strip.text = element_text(hjust = 0.5, size = 9, lineheight = 1))
ggsave(fig("fig6_unicorns.png"), f6, width = 12, height = 6.6, dpi = 300)
