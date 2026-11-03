# RQ3 figures: playoff shifts (box scores and shot zones) for both leagues.

source(here::here("R", "config.R"))
source(here::here("R", "metrics.R"))
source(here::here("R", "court.R"))
source(here::here("R", "theme.R"))
suppressMessages({ library(dplyr); library(tidyr); library(patchwork) })

rq_dir <- here::here("RQ3_playoffs")
fig <- \(name) file.path(rq_dir, "results", "figures", name)
tab <- \(name) read.csv(file.path(rq_dir, "results", "tables", name))
LEAGUES <- c("NBA", "EuroLeague")
caption <- "Matched design: only teams that reached the playoffs, compared with their own regular season. 2010/11-2025/26."

# ---- Fig 1: standardised playoff shifts --------------------------------------------------------
ps <- tab("playoff_shifts.csv") |> filter(metric != "np")
long <- bind_rows(
  ps |> transmute(label, league = "NBA", es = es_nba, lo = es_nba_lo, hi = es_nba_hi),
  ps |> transmute(label, league = "EuroLeague", es = es_el, lo = es_el_lo, hi = es_el_hi)
) |>
  mutate(league = factor(league, LEAGUES),
         label = factor(label, ps$label[order(ps$es_nba)]))
f1 <- ggplot(long, aes(es, label, colour = league)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.55), size = 0.28) +
  scale_colour_manual(values = LEAGUE_COLOURS, name = NULL) +
  labs(x = "Playoff minus regular season, in SDs of the metric", y = NULL,
       title = "In the playoffs both leagues slow down, foul more and shoot worse",
       subtitle = "Posterior mean and 95% CrI from mixed models with team- and opponent-season effects",
       caption = caption) +
  theme_project()
ggsave(fig("fig1_playoff_shifts.png"), f1, width = 8, height = 6.5, dpi = 300)

# ---- Fig 2: does the NBA playoff shift point towards the EuroLeague? ---------------------------
f2 <- ggplot(ps, aes(p_nba_toward_el, reorder(label, p_nba_toward_el))) +
  geom_vline(xintercept = 0.5, linetype = 2, colour = "grey50") +
  geom_segment(aes(x = 0.5, xend = p_nba_toward_el, yend = reorder(label, p_nba_toward_el)),
               colour = "grey70") +
  geom_point(aes(colour = p_nba_toward_el > 0.5), size = 2.2) +
  scale_colour_manual(values = c(`TRUE` = LEAGUE_COLOURS[["EuroLeague"]], `FALSE` = "grey40"),
                      labels = c(`TRUE` = "towards EuroLeague", `FALSE` = "away from EuroLeague"),
                      name = "NBA playoff shift points ...") +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x = "Posterior probability that the NBA playoff shift points towards the EuroLeague level", y = NULL,
       title = "NBA playoffs look more 'European' on 11 of 22 metrics: slower, more fouls and free throws, but not in shooting",
       subtitle = "Tests the 2019 paper's claim metric by metric", caption = caption) +
  theme_project()
ggsave(fig("fig2_nba_toward_euroleague.png"), f2, width = 8, height = 6.5, dpi = 300)

# ---- Fig 3: shot-zone shifts -------------------------------------------------------------------
zs <- tab("playoff_zone_shifts.csv") |>
  mutate(zone = factor(zone, rev(ZONES)), league = factor(league, LEAGUES))
f3 <- ggplot(zs, aes(shift_pp, zone, colour = league)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5), size = 0.35) +
  scale_colour_manual(values = LEAGUE_COLOURS, name = NULL) +
  labs(x = "Change in share of attempts, playoffs - regular season (percentage points)", y = NULL,
       title = "In the playoffs EuroLeague teams swap rim shots for above-the-break threes", caption = caption) +
  theme_project()
ggsave(fig("fig3_playoff_zone_shifts.png"), f3, width = 8, height = 3.8, dpi = 300)
