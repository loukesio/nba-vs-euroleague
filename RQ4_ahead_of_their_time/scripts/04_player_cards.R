# RQ4 step 4 — Player cards: which team in the OTHER league would suit a player best, then and now?
#
# A player's style fingerprint (step 02) is measured against the average player of the same position,
# league and season, in units of the usual player-to-player spread. That makes fingerprints comparable
# across leagues and eras: "+2.5 in assists" means the same thing for a 2011 EuroLeague guard and a
# 2025 NBA guard. A team is a good fit for him if
#   ROLE fit:   the team uses players at his position the way he played (minutes-weighted fingerprint of
#               the team's rotation players at that position), and
#   SYSTEM fit: the team plays the way his own team played (team fingerprints from step 01).
# Similarity = cosine of the two fingerprints (+1 = same direction, 0 = unrelated, -1 = opposite).
# Fit = average of role and system similarity.
#   "Then" = team-seasons of the other league during the seasons he played.
#   "Now"  = the other league's teams of 2025/26: his style moved to today, relative to today's league.
# Uncertainty: 2,000 draws of his fingerprint from its posterior (step 02) -> interval for the fit and
# P(best fit) = share of draws in which the team comes out on top.

source(here::here("R", "config.R"))
source(here::here("R", "court.R"))
source(here::here("R", "theme.R"))
source(here::here("R", "data.R"))
suppressMessages({ library(dplyr); library(tidyr); library(purrr); library(patchwork) })

rq_dir <- here::here("RQ4_ahead_of_their_time")
tab_dir <- file.path(rq_dir, "results", "tables")
fig_dir <- file.path(rq_dir, "results", "figures")
rd <- \(f) read.csv(file.path(tab_dir, f), colClasses = c(player_id = "character"))
LEAGUES <- c("NBA", "EuroLeague")

PLAYER_FEATURES <- c(n_rim = "Rim shots", n_short_mid = "Short mid-range", n_long_mid = "Long mid-range",
                     n_corner3 = "Corner threes", n_above3 = "Above-break threes", ast = "Assists",
                     tov = "Turnovers", orb = "Off. rebounds", drb = "Def. rebounds", blk = "Blocks",
                     stl = "Steals", pf = "Fouls", ftr = "Free throws per shot")
TEAM_FEATURES <- c("pace40", "fta_100", "ast_100", "tov_100", "orb_rate", "blk_100", "stl_100",
                   "fcm_100", "entropy_min", "rim", "short_mid", "long_mid", "corner3", "above3")

dev   <- rd("player_profile_deviations.csv")
sdv   <- rd("player_profile_sd.csv")
pst   <- rd("player_styles.csv")
tdev  <- read.csv(file.path(tab_dir, "team_profile_deviations.csv"))
tname <- read.csv(file.path(tab_dir, "team_names.csv"))

cosine <- function(A, b) as.vector(A %*% b) / (sqrt(rowSums(A^2)) * sqrt(sum(b^2)))
wmean_rows <- function(M, w) colSums(M * w) / sum(w)

player_fit <- function(league, player, seasons = NULL, now = max(SEASONS), S = 2000) {
  set.seed(2026)
  me <- pst |> filter(.data$league == !!league, .data$player == !!player)
  if (!is.null(seasons)) me <- me |> filter(season %in% seasons)
  stopifnot(nrow(me) > 0)
  pos <- names(which.max(table(me$position)))
  key <- c("league", "season", "player_id")
  my_dev <- me |> select(all_of(key), minutes) |> left_join(dev, by = key)
  my_sd  <- me |> select(all_of(key), minutes) |> left_join(sdv, by = key)
  w <- my_dev$minutes
  v  <- wmean_rows(as.matrix(my_dev[, names(PLAYER_FEATURES)]), w)
  vs <- sqrt(colSums((as.matrix(my_sd[, names(PLAYER_FEATURES)]) * w)^2)) / sum(w)
  # his team's system, minutes-weighted over his seasons
  u <- me |> select(league, season, team, minutes) |>
    left_join(tdev, by = c("league", "season", "team"))
  u <- wmean_rows(as.matrix(u[, TEAM_FEATURES]), u$minutes)

  target <- setdiff(LEAGUES, league)
  windows <- list(then = sort(unique(me$season)), now = now)

  fits <- imap_dfr(windows, function(ss, when) {
    # how each target team uses players at his position (minutes-weighted fingerprint)
    slots <- pst |> filter(.data$league == target, season %in% ss, position == pos) |>
      select(all_of(key), team, minutes) |> left_join(dev, by = key) |>
      group_by(league, season, team) |>
      summarise(across(all_of(names(PLAYER_FEATURES)), \(x) sum(x * minutes) / sum(minutes)),
                .groups = "drop") |>
      inner_join(tdev, by = c("league", "season", "team"))
    A <- as.matrix(slots[, names(PLAYER_FEATURES)])
    role <- cosine(A, v)
    system <- cosine(as.matrix(slots[, TEAM_FEATURES]), u)
    # posterior draws of his fingerprint -> uncertainty in role fit and in the ranking
    V <- matrix(rnorm(S * length(v), v, vs), S, byrow = TRUE)
    role_draws <- (A / sqrt(rowSums(A^2))) %*% t(V / sqrt(rowSums(V^2)))   # teams x draws
    fit_draws <- (role_draws + system) / 2
    best <- tabulate(apply(fit_draws, 2, which.max), nrow(slots)) / S
    slots |> select(league, season, team) |>
      mutate(when = when, role = role, system = system, fit = (role + system) / 2,
             fit_lo = apply(fit_draws, 1, quantile, .05), fit_hi = apply(fit_draws, 1, quantile, .95),
             p_best = best)
  }) |>
    left_join(tname, by = c("league", "season", "team")) |>
    arrange(when, desc(fit))

  # closest players in the other league, same position (style comps)
  comps <- imap_dfr(windows, function(ss, when) {
    cand <- pst |> filter(.data$league == target, season %in% ss, position == pos) |>
      select(all_of(key), player, team) |> left_join(dev |> select(-player, -position), by = key)
    cand |> mutate(when = when, similarity = cosine(as.matrix(cand[, names(PLAYER_FEATURES)]), v)) |>
      slice_max(similarity, n = 25) |> select(when, season, player, team, similarity)
  })

  list(player = player, league = league, position = pos, seasons = sort(unique(me$season)),
       teams = unique(me$team), profile = tibble(feature = names(v), z = v, sd = vs),
       fits = fits, comps = comps, now = now, target = target)
}

# a player's regular-season shots in the seasons of f
player_shots <- function(f, shots = NULL) {
  me <- pst |> filter(league == f$league, player == f$player)
  sh <- (if (is.null(shots)) load_shots(f$seasons) else shots) |>
    filter(league == f$league, phase == "regular", season %in% f$seasons)
  if (f$league == "NBA") {
    # NBA box scores (ESPN ids) and shots (NBA Stats ids, surname only) don't share ids: link as in
    # 00_data step 05 -- same season and team, full name ends with the shot surname, closest shot count
    norm_name <- function(x) {
      x <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
      x <- gsub("\\b(jr|sr|ii|iii|iv)\\b\\.?", "", x)
      trimws(gsub("[^a-z ]", "", gsub("-", " ", x)))
    }
    full <- norm_name(nice_player(f$player))
    link <- sh |> mutate(team = coalesce(NBA_TEAM_MAP[team], team)) |>
      filter(team %in% f$teams) |> count(season, team, player_id, player) |>
      filter(endsWith(full, norm_name(player)), nchar(norm_name(player)) > 0) |>
      inner_join(me |> select(season, my_shots = shots), by = "season") |>
      group_by(season) |> slice_min(abs(n - my_shots), n = 1, with_ties = FALSE) |> ungroup()
    sh <- sh |> semi_join(link, by = c("season", "player_id"))
  } else {
    sh <- sh |> filter(player_id %in% unique(me$player_id))
  }
  sh
}

# best fits of one window ("then"/"now"): one row per franchise (its best season)
fit_panel <- function(f, when, title, n_show = 8) {
  tcol <- LEAGUE_COLOURS[[f$target]]
  # P(best) summed over a franchise's seasons
  all_d <- f$fits |> filter(.data$when == !!when)
  typical <- median(all_d$fit)
  d <- all_d |> group_by(team) |> mutate(p_best = sum(p_best)) |>
    slice_max(fit, n = 1, with_ties = FALSE) |> ungroup() |> arrange(desc(fit)) |>
    slice_head(n = n_show) |>
    mutate(lab = if (.env$when == "then") sprintf("%s (%s)", name, season_short(season)) else name,
           lab = factor(lab, rev(lab)))
  dl <- d |> select(lab, Role = role, System = system) |> pivot_longer(-lab)
  ggplot(d, aes(y = lab)) +
    geom_vline(xintercept = typical, linetype = "dashed", colour = "grey55", linewidth = 0.4) +
    annotate("text", x = typical, y = Inf, label = "typical team", vjust = -0.4, hjust = 0.5,
             size = 2.7, colour = "grey45") +
    coord_cartesian(clip = "off") +
    geom_segment(aes(x = fit_lo, xend = fit_hi, yend = lab), colour = tcol, linewidth = 2.2,
                 alpha = 0.35, lineend = "round") +
    geom_point(data = dl, aes(x = value, shape = name), colour = "grey35", size = 1.8) +
    geom_point(aes(x = fit), colour = tcol, size = 3.2) +
    geom_text(aes(x = pmax(fit_hi, role, system) + 0.04,
                  label = if_else(p_best >= 0.05, sprintf("%.0f%%", 100 * p_best), "")),
              hjust = 0, size = 2.8, colour = "grey25") +
    scale_shape_manual(values = c(Role = 2, System = 0), name = NULL) +
    scale_x_continuous(limits = c(NA, 1.05), breaks = seq(-0.2, 1, 0.2)) +
    labs(title = title, x = "Fit (cosine similarity; 1 = identical style)", y = NULL) +
    theme_project() + theme(panel.grid.major.y = element_blank(), legend.position = "top")
}

# ---- the card -------------------------------------------------------------------------------
player_card <- function(f, file = NULL, nickname = NULL, n_show = 8, shots = NULL) {
  pname <- nice_player(f$player)
  col <- LEAGUE_COLOURS[[f$league]]; tcol <- LEAGUE_COLOURS[[f$target]]
  pos_word <- c(G = "guards", F = "forwards", C = "centres")[f$position]
  span <- sprintf("%s-%s", season_label(min(f$seasons)), season_label(max(f$seasons)))

  sh <- player_shots(f, shots) |> mutate(result = if_else(made, "Made", "Missed"))
  pA <- ggplot(sh, aes(x, y)) +
    geom_point(aes(shape = result, colour = result), size = 1.5, stroke = 0.6, alpha = 0.8) +
    geom_path(data = court_lines(f$league), aes(x, y, group = piece), colour = "grey35",
              linewidth = 0.3, inherit.aes = FALSE) +
    coord_fixed(xlim = c(-7.7, 7.7), ylim = c(-1.7, 9.5), expand = FALSE) +
    scale_shape_manual(values = c(Made = 16, Missed = 1), name = NULL) +
    scale_colour_manual(values = c(Made = col, Missed = "grey55"), name = NULL) +
    labs(title = "Every shot", subtitle = sprintf("%s regular season, %d shots", span, nrow(sh))) +
    theme_project_court() + theme(legend.position = "top")

  # B: fingerprint
  prof <- f$profile |>
    mutate(label = factor(PLAYER_FEATURES[feature], rev(PLAYER_FEATURES)),
           lo = z - 1.645 * sd, hi = z + 1.645 * sd)
  pB <- ggplot(prof, aes(z, label)) +
    geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
    geom_col(aes(fill = z > 0), width = 0.7) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.25, orientation = "y", colour = "grey20",
                  linewidth = 0.3) +
    scale_fill_manual(values = c(`TRUE` = col, `FALSE` = "grey70"), guide = "none") +
    labs(title = "Style fingerprint",
         subtitle = sprintf("vs the average %s %s of the same season", f$league,
                            sub("s$", "", pos_word)),
         x = "Difference, in typical player-to-player spreads (90% CrI)", y = NULL) +
    theme_project() + theme(panel.grid.major.y = element_blank())

  # C, D: best fits
  pC <- fit_panel(f, "then", n_show = n_show, title = sprintf("Best %s fits in his time (%s)", f$target, span))
  pD <- fit_panel(f, "now", n_show = n_show, title = sprintf("Best %s fits today (%s)", f$target, season_label(f$now)))

  comp_txt <- function(when) {
    f$comps |> filter(.data$when == !!when) |> arrange(desc(similarity)) |>
      distinct(player, .keep_all = TRUE) |> slice_head(n = 3) |>
      mutate(t = sprintf("%s (%s)", nice_player(player), season_short(season))) |> pull(t) |>
      paste(collapse = ", ")
  }
  my_teams <- paste(unique(tname$name[tname$league == f$league & tname$team %in% f$teams]),
                    collapse = "/")
  card <- (pA | pB) / (pC | pD) + plot_layout(heights = c(1.05, 1)) +
    plot_annotation(
      title = paste0(pname, if (!is.null(nickname)) paste0("  ·  ", nickname) else ""),
      subtitle = sprintf(paste0("%s, %s.\n",
                                "Most similar %s %s by style.  Then: %s.\n",
                                "Today: %s.\n",
                                "Fit = average of ROLE (how the team uses its %s) and SYSTEM ",
                                "(how the team plays compared with %s). %% = chance the franchise is the best fit.\n",
                                "Compare teams against the dashed 'typical team' line, not across panels: ",
                                "fits are on different scales then and now."),
                         my_teams, span, f$target, pos_word, comp_txt("then"), comp_txt("now"),
                         pos_word, my_teams),
      caption = paste("Fingerprints are relative to each league and season, so 'today' means: a player who",
                      "stood out in his era the way he did, placed in today's league. Data start in 2010/11."),
      theme = theme_project(11) + theme(plot.title = ggtext::element_textbox_simple(size = 18, colour = col, face = "bold", margin = margin(b = 4))))
  if (!is.null(file)) ggsave(file, card, width = 13, height = 10, dpi = 300)
  invisible(card)
}

# ---- several eras of one player ---------------------------------------------------------------
# Each era gets its own fingerprint (minutes-weighted over its seasons), its own team system and its
# own "then" window. The "today" panel puts every era's version of him into the same league of today.
era_label <- function(ss) {
  if (length(ss) == 1) season_short(ss) else sprintf("%s-%s", season_short(min(ss)), season_short(max(ss)))
}
# ordered eras -> light-to-dark shades of the player's league colour
era_colours <- function(col, n) {
  colorRampPalette(c("white", col, "black"))(11)[list(6, c(4, 8), c(4, 6, 9))[[n]]]
}

player_fit_eras <- function(league, player, eras, now = max(SEASONS)) {
  fl <- map(eras, \(ss) player_fit(league, player, seasons = ss, now = now))
  set_names(fl, make.unique(map_chr(fl, \(f) era_label(f$seasons)), sep = " "))
}

# n rows per level of a discrete y, nudged apart by group (manual dodge, works for any geom)
dodge_y <- function(y, group, width = 0.6) {
  k <- nlevels(group)
  as.integer(y) + if (k == 1) 0 else (as.integer(group) - (k + 1) / 2) * width / (k - 1)
}

player_card_eras <- function(fl, file = NULL, nickname = NULL, n_show = 8, shots = NULL) {
  if (length(fl) == 1) return(player_card(fl[[1]], file, nickname, n_show, shots))
  f1 <- fl[[1]]; eras <- names(fl)
  col <- LEAGUE_COLOURS[[f1$league]]; tcol <- LEAGUE_COLOURS[[f1$target]]
  ecol <- setNames(era_colours(col, length(fl)), eras)
  pos_word <- c(G = "guards", F = "forwards", C = "centres")[f1$position]

  # A: one court per era
  sh <- imap_dfr(fl, \(f, e) player_shots(f, shots) |> mutate(era = e)) |>
    mutate(era = factor(era, eras), result = if_else(made, "Made", "Missed"))
  n_sh <- table(sh$era)
  pA <- ggplot(sh, aes(x, y)) +
    geom_point(aes(shape = result, colour = era), size = 0.9, stroke = 0.4, alpha = 0.75) +
    geom_path(data = court_lines(f1$league), aes(x, y, group = piece), colour = "grey35",
              linewidth = 0.25, inherit.aes = FALSE) +
    facet_wrap(~era, nrow = 1, labeller = as_labeller(\(e) sprintf("%s  (%d shots)", e, n_sh[e]))) +
    coord_fixed(xlim = c(-7.7, 7.7), ylim = c(-1.7, 9.5), expand = FALSE) +
    scale_shape_manual(values = c(Made = 16, Missed = 1), name = NULL) +
    scale_colour_manual(values = ecol, guide = "none") +
    labs(title = "Every shot, era by era", subtitle = "Filled = made, open = missed") +
    theme_project_court() + theme(legend.position = "top")

  # B: fingerprints side by side
  prof <- imap_dfr(fl, \(f, e) f$profile |> mutate(era = e)) |>
    mutate(era = factor(era, eras), label = factor(PLAYER_FEATURES[feature], rev(PLAYER_FEATURES)),
           yn = dodge_y(label, era), lo = z - 1.645 * sd, hi = z + 1.645 * sd)
  pB <- ggplot(prof, aes(z, yn, colour = era)) +
    geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", linewidth = 0.5) +
    geom_point(size = 2) +
    scale_y_continuous(breaks = seq_along(levels(prof$label)), labels = levels(prof$label),
                       expand = expansion(add = 0.5)) +
    scale_colour_manual(values = ecol, name = "Era") +
    labs(title = "How his style changed",
         subtitle = sprintf("vs the average %s %s of the same season (90%% CrI)", f1$league,
                            sub("s$", "", pos_word)),
         x = "Difference, in typical player-to-player spreads", y = NULL) +
    theme_project() + theme(panel.grid.major.y = element_blank(), legend.position = "top")

  # C: best fits within each era
  pC <- imap(fl, \(f, e) fit_panel(f, "then", n_show = n_show,
                                   title = sprintf("Best %s fits, %s", f$target, e)) +
               theme(plot.title = ggtext::element_textbox_simple(colour = ecol[[e]], face = "bold")))

  # D: today -- which version of him fits where
  now_d <- imap_dfr(fl, \(f, e) f$fits |> filter(when == "now") |> mutate(era = e))
  top <- now_d |> group_by(name) |> summarise(m = max(fit)) |> slice_max(m, n = n_show)
  d <- now_d |> filter(name %in% top$name) |>
    mutate(name = factor(name, rev(top$name[order(-top$m)])), era = factor(era, eras),
           yn = dodge_y(name, era))
  best_txt <- now_d |> group_by(era) |> slice_max(p_best, n = 1, with_ties = FALSE) |>
    mutate(era = factor(era, eras)) |> arrange(era) |>
    transmute(t = sprintf("%s -> %s (%.0f%%)", era, name, 100 * p_best)) |> pull(t)
  pD <- ggplot(d, aes(y = yn, colour = era)) +
    geom_segment(aes(x = fit_lo, xend = fit_hi, yend = yn), linewidth = 1.6, alpha = 0.4,
                 lineend = "round") +
    geom_point(aes(x = fit), size = 2.6) +
    scale_y_continuous(breaks = seq_along(levels(d$name)), labels = levels(d$name),
                       expand = expansion(add = 0.5)) +
    scale_colour_manual(values = ecol, name = "Version of him") +
    labs(title = sprintf("Which version of him fits today's %s (%s)?", f1$target, season_label(f1$now)),
         subtitle = paste("Best fit:", paste(best_txt, collapse = ";  ")),
         x = "Fit (cosine similarity; 1 = identical style)", y = NULL) +
    theme_project() + theme(panel.grid.major.y = element_blank(), legend.position = "top")

  comp_txt <- imap_chr(fl, \(f, e) {
    cc <- f$comps |> filter(when == "then") |> arrange(desc(similarity)) |>
      distinct(player, .keep_all = TRUE) |> slice_head(n = 3)
    sprintf("%s: %s", e, paste(nice_player(cc$player), collapse = ", "))
  })
  my_teams <- paste(unique(tname$name[tname$league == f1$league &
                                        tname$team %in% unlist(map(fl, "teams"))]), collapse = "/")
  card <- (pA | pB) / wrap_plots(pC, nrow = 1) / pD + plot_layout(heights = c(1, 1, 0.9)) +
    plot_annotation(
      title = paste0(nice_player(f1$player), if (!is.null(nickname)) paste0("  ·  ", nickname) else ""),
      subtitle = paste0(my_teams, ". ", length(fl), " eras.\n",
                        sprintf("Most similar %s %s in each era:\n", f1$target, pos_word),
                        paste0("   ", comp_txt, collapse = "\n"), "\n",
                        "Fit = average of ROLE and SYSTEM; % = chance the franchise is the best fit; ",
                        "compare teams within a panel, against its dashed 'typical team' line."),
      caption = "Each era's fingerprint is relative to its own league and seasons. Data start in 2010/11.",
      theme = theme_project(11) + theme(plot.title = ggtext::element_textbox_simple(size = 18, colour = col, face = "bold", margin = margin(b = 4))))
  if (!is.null(file)) ggsave(file, card, width = 14, height = 15, dpi = 300)
  invisible(card)
}

slug <- \(x) gsub("[^a-z]+", "_", tolower(nice_player(x)))
run_card <- function(league, player, nickname = NULL, ...) {
  f <- player_fit(league, player, ...)
  write.csv(f$fits, file.path(tab_dir, paste0("fit_", slug(player), ".csv")), row.names = FALSE)
  player_card(f, file.path(fig_dir, paste0("card_", slug(player), ".png")), nickname)
  cat("\n==", nice_player(player), "-> best", f$target, "fits\n")
  print(f$fits |> group_by(when) |> slice_head(n = 5) |>
          select(when, season, name, role, system, fit, fit_lo, fit_hi, p_best), n = 10)
  print(as_tibble(f$comps), n = 10)
  invisible(f)
}

# ---- team cards ----------------------------------------------------------------------------
# The same idea from the team side: which teams of the other league played most like this team
# (SYSTEM similarity of team fingerprints), and which players of the other league would fill each of
# its position slots best (ROLE similarity to the minutes-weighted fingerprint of its rotation there).
# Team fingerprints are point estimates (step 01), so there are no intervals here.
TEAM_LABELS <- c(pace40 = "Pace", fta_100 = "Free-throw attempts", ast_100 = "Assists",
                 tov_100 = "Turnovers", orb_rate = "Off. rebounds", blk_100 = "Blocks",
                 stl_100 = "Steals", fcm_100 = "Fouls", entropy_min = "Rotation depth",
                 rim = "Rim shots", short_mid = "Short mid-range", long_mid = "Long mid-range",
                 corner3 = "Corner threes", above3 = "Above-break threes")
POS_WORDS <- c(G = "Guards", F = "Forwards", C = "Centres")

team_fit <- function(league, team, seasons = NULL, now = max(SEASONS), n_players = 5) {
  mine <- tdev |> filter(.data$league == !!league, .data$team == !!team)
  if (!is.null(seasons)) mine <- mine |> filter(season %in% seasons)
  stopifnot(nrow(mine) > 0)
  u <- colMeans(as.matrix(mine[, TEAM_FEATURES]))
  target <- setdiff(LEAGUES, league)
  windows <- list(then = sort(unique(mine$season)), now = now)
  key <- c("league", "season", "player_id")
  feats <- names(PLAYER_FEATURES)

  teams <- imap_dfr(windows, function(ss, when) {
    cand <- tdev |> filter(.data$league == target, season %in% ss)
    cand |> select(league, season, team) |>
      mutate(when = when, similarity = cosine(as.matrix(cand[, TEAM_FEATURES]), u))
  }) |>
    left_join(tname, by = c("league", "season", "team")) |>
    arrange(when, desc(similarity))

  # this team's position slots over the chosen seasons
  slots <- pst |> filter(.data$league == !!league, .data$team == !!team, season %in% windows$then) |>
    select(all_of(key), position, minutes) |> left_join(dev |> select(-player, -position), by = key) |>
    group_by(position) |>
    summarise(across(all_of(feats), \(x) sum(x * minutes) / sum(minutes)), .groups = "drop")
  players <- imap_dfr(windows, function(ss, when) {
    map_dfr(seq_len(nrow(slots)), function(i) {
      cand <- pst |> filter(.data$league == target, season %in% ss, position == slots$position[i]) |>
        select(all_of(key), player, team) |> left_join(dev |> select(-player, -position), by = key)
      cand |> mutate(when = when, position = slots$position[i],
                     similarity = cosine(as.matrix(cand[, feats]), unlist(slots[i, feats]))) |>
        arrange(desc(similarity)) |> distinct(player, .keep_all = TRUE) |>
        slice_head(n = n_players) |> select(when, position, season, player, team, similarity)
    })
  })

  list(league = league, team = team, target = target, seasons = windows$then, now = now,
       name = tname$name[tname$league == league & tname$team == team & tname$season == max(windows$then)][1],
       profile = tibble(feature = names(u), z = u), teams = teams, slots = slots, players = players)
}

team_card <- function(f, file = NULL, n_show = 10) {
  col <- LEAGUE_COLOURS[[f$league]]; tcol <- LEAGUE_COLOURS[[f$target]]
  span <- if (length(f$seasons) == 1) season_label(f$seasons) else
    sprintf("%s-%s", season_label(min(f$seasons)), season_label(max(f$seasons)))

  prof <- f$profile |> mutate(label = factor(TEAM_LABELS[feature], rev(TEAM_LABELS)))
  pA <- ggplot(prof, aes(z, label)) +
    geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
    geom_col(aes(fill = z > 0), width = 0.7) +
    scale_fill_manual(values = c(`TRUE` = col, `FALSE` = "grey70"), guide = "none") +
    labs(title = "Team fingerprint", subtitle = sprintf("vs the average %s team of the same season", f$league),
         x = "Difference, in typical team-to-team spreads", y = NULL) +
    theme_project() + theme(panel.grid.major.y = element_blank())

  sim_panel <- function(when, title) {
    all_d <- f$teams |> filter(.data$when == !!when)
    d <- all_d |> group_by(team) |> slice_max(similarity, n = 1, with_ties = FALSE) |> ungroup() |>
      arrange(desc(similarity)) |> slice_head(n = n_show) |>
      mutate(lab = if (.env$when == "then") sprintf("%s (%s)", name, season_short(season)) else name,
             lab = factor(lab, rev(lab)))
    ggplot(d, aes(similarity, lab)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.4) +
      geom_segment(aes(x = 0, xend = similarity, yend = lab), colour = tcol, alpha = 0.35, linewidth = 1.2) +
      geom_point(colour = tcol, size = 3.2) +
      scale_x_continuous(limits = c(min(0, d$similarity), 1)) +
      labs(title = title, x = "Similarity (cosine; 1 = identical style)", y = NULL) +
      theme_project() + theme(panel.grid.major.y = element_blank())
  }
  pB <- sim_panel("then", sprintf("%s teams that played most like it (%s)", f$target, span))
  pC <- sim_panel("now", sprintf("%s teams most like it today (%s)", f$target, season_label(f$now)))

  pl <- f$players |>
    mutate(when = factor(if_else(when == "then", sprintf("In its time (%s)", span),
                                 sprintf("Today (%s)", season_label(f$now))),
                         c(sprintf("In its time (%s)", span), sprintf("Today (%s)", season_label(f$now)))),
           position = factor(POS_WORDS[position], POS_WORDS),
           lab = sprintf("%s (%s)", nice_player(player), season_short(season)),
           lab = tidytext_order(lab, similarity, interaction(position, when)))
  pD <- ggplot(pl, aes(similarity, lab)) +
    geom_point(colour = tcol, size = 2.4) +
    facet_wrap(~ position + when, ncol = 2, scales = "free_y") +
    scale_y_discrete(labels = \(x) sub("___.*$", "", x)) +
    labs(title = sprintf("%s players who would fill its slots best", f$target),
         subtitle = "Similarity to how the team used each position (minutes-weighted rotation fingerprint)",
         x = "Similarity (cosine)", y = NULL) +
    theme_project() + theme(panel.grid.major.y = element_blank())

  card <- (pA | (pB / pC)) / pD + plot_layout(heights = c(1, 1.1)) +
    plot_annotation(
      title = sprintf("%s  ·  %s", f$name, span),
      subtitle = sprintf("%s. Fingerprints are relative to each league and season; similarities are point estimates.",
                         f$league),
      theme = theme_project(11) + theme(plot.title = ggtext::element_textbox_simple(size = 18, colour = col, face = "bold", margin = margin(b = 4))))
  if (!is.null(file)) ggsave(file, card, width = 13, height = 12, dpi = 300)
  invisible(card)
}
# order labels within facets (unique level per facet, stripped for display)
tidytext_order <- function(x, by, within) {
  k <- paste(x, within, sep = "___")
  factor(k, unique(k[order(within, by)]))
}

# Players to card (add more here); skipped when this file is sourced (e.g. by the app)
if (sys.nframe() == 0) {
  run_card("EuroLeague", "DIAMANTIDIS, DIMITRIS", nickname = "3D, Panathinaikos")
  run_card("NBA", "Manu Ginobili", nickname = "San Antonio Spurs")
  run_card("EuroLeague", "SPANOULIS, VASSILIS", nickname = "Kill Bill, Olympiacos")
  run_card("NBA", "Tony Parker", nickname = "San Antonio Spurs")
}
