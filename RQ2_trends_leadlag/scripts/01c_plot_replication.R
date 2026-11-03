# RQ2 step 1c — redraw the replication figure from the saved comparison table (no refitting).
source(here::here("R", "config.R"))
source(here::here("R", "theme.R"))
suppressMessages(library(dplyr))
rq_dir <- here::here("RQ2_trends_leadlag")
comparison <- read.csv(file.path(rq_dir, "results", "tables", "replication_vs_published.csv"))
comparison$league <- ifelse(comparison$league == "Euroleague", "EuroLeague", comparison$league)
p <- ggplot(comparison, aes(published, mean, colour = league)) +
  geom_abline(linetype = 2, colour = "grey60") +
  geom_pointrange(aes(ymin = mean - 2 * sd, ymax = mean + 2 * sd), size = 0.25) +
  facet_wrap(~ param, scales = "free",
             labeller = as_labeller(c(mu = "Season-average level (mu)", s = "Trend per season (s)"))) +
  scale_colour_manual(values = LEAGUE_COLOURS) +
  labs(x = "Published estimate (Mandić et al. 2019, Table 1)",
       y = "Our estimate (± 2 SD)", colour = NULL,
       title = sprintf("All %d published estimates reproduced, to within %.3f", nrow(comparison),
                       max(abs(comparison$mean - comparison$published))),
       subtitle = "The 2019 model re-implemented in Stan and fitted to the authors' own data. Points on the dashed line = identical estimates; 17 variables, 2 leagues.") +
  theme_project()
ggsave(file.path(rq_dir, "results", "figures", "rq2_replication_check.png"), p,
       width = 8.5, height = 5, dpi = 300)
