# Build one row per player-season (regular season, 2010/11-2025/26) -> processed/player_seasons.parquet
#
# Box-score totals come from the player box scores; shot locations (zone counts) from the shot table.
#   * EuroLeague: the shot feed's player ID is the box-score person code, so they join directly.
#   * NBA: box scores are ESPN (full names, ESPN team codes); shots are NBA Stats (surname only,
#     NBA team codes). Players are matched within season and team on surname; when two players
#     share a surname (the Morris twins, the Antetokounmpos) the closest field-goal attempt count wins.
# Possessions played are estimated as minutes x the team's possessions per minute, so rates can be
# expressed per 100 possessions, as for teams.

source(here::here("R", "config.R"))
suppressMessages({ library(dplyr); library(purrr); library(tidyr); library(arrow); library(jsonlite) })

ZONE_COLS <- c("Rim area" = "n_rim", "Short mid-range" = "n_short_mid", "Long mid-range" = "n_long_mid",
               "Corner 3" = "n_corner3", "Above-break 3" = "n_above3")

shots <- read_parquet(file.path(PROCESSED, "shots.parquet")) |>
  filter(phase == "regular") |>
  mutate(team = if_else(league == "NBA" & team %in% names(NBA_TEAM_MAP),
                        NBA_TEAM_MAP[team], team))
zone_counts <- shots |>
  count(league, season, team, player_id, player, zone) |>
  mutate(zone = ZONE_COLS[as.character(zone)]) |>
  pivot_wider(names_from = zone, values_from = n, values_fill = 0) |>
  mutate(shots = rowSums(across(all_of(unname(ZONE_COLS)))))

# Team possessions per minute (regular season), to turn player minutes into possessions
team_pace <- read_parquet(file.path(PROCESSED, "team_games.parquet")) |>
  filter(phase == "regular", season %in% SEASONS) |>
  group_by(league, season, team) |>
  summarise(poss_per_min = sum(np) / sum(game_min), .groups = "drop")

# ---- NBA box scores ---------------------------------------------------------------
pos_group <- function(p) case_when(p %in% c("C", "Center") ~ "C",
                                   p %in% c("F", "SF", "PF", "Forward") ~ "F",
                                   p %in% c("G", "PG", "SG", "Guard") ~ "G")
mode1 <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_character_ else names(which.max(table(x))) }

nba_box <- map_dfr(SEASONS, function(s) {
  read_parquet(file.path(RAW_NBA, "player_box", sprintf("player_box_%d.parquet", s))) |>
    filter(season_type == 2, !did_not_play %in% TRUE, coalesce(as.numeric(minutes), 0) > 0) |>
    group_by(season = s, team = team_abbreviation, player_id = as.character(athlete_id)) |>
    summarise(player = first(athlete_display_name),
              position = mode1(pos_group(athlete_position_abbreviation)),
              games = n(), minutes = sum(as.numeric(minutes)),
              p3a = sum(three_point_field_goals_attempted), p2a = sum(field_goals_attempted) - p3a,
              fta = sum(free_throws_attempted), ast = sum(assists), tov = sum(turnovers),
              orb = sum(offensive_rebounds), drb = sum(defensive_rebounds), blk = sum(blocks),
              stl = sum(steals), pf = sum(fouls), pts = sum(points), .groups = "drop")
}) |> mutate(league = "NBA")

# Surname matching: normalise (ASCII, lower case, no suffixes or punctuation), then a box player
# matches a shot player if the box full name ends with the shot surname.
norm_name <- function(x) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  x <- gsub("\\b(jr|sr|ii|iii|iv)\\b\\.?", "", x)
  trimws(gsub("[^a-z ]", "", gsub("-", " ", x)))
}
nba_link <- nba_box |>
  transmute(season, team, player_id, fga_box = p2a + p3a, full = norm_name(player)) |>
  inner_join(zone_counts |> filter(league == "NBA") |>
               transmute(season, team, shot_id = player_id, surname = norm_name(player), shots),
             by = c("season", "team"), relationship = "many-to-many") |>
  filter(endsWith(full, surname), nchar(surname) > 0) |>
  mutate(gap = abs(fga_box - shots)) |>
  arrange(gap)
# one-to-one: greedy on the smallest FGA gap
used <- new.env(); keep <- logical(nrow(nba_link))
kb <- paste("b", nba_link$season, nba_link$team, nba_link$player_id)
ks <- paste("s", nba_link$season, nba_link$team, nba_link$shot_id)
for (i in seq_len(nrow(nba_link))) {
  if (is.null(used[[kb[i]]]) && is.null(used[[ks[i]]])) {
    keep[i] <- TRUE; used[[kb[i]]] <- TRUE; used[[ks[i]]] <- TRUE
  }
}
nba_link <- nba_link[keep, ] |> select(season, team, player_id, shot_id)

nba <- nba_box |>
  left_join(nba_link, by = c("season", "team", "player_id")) |>
  left_join(zone_counts |> filter(league == "NBA") |> select(-league, -player),
            by = c("season", "team", "shot_id" = "player_id")) |>
  select(-shot_id)

# ---- EuroLeague box scores ----------------------------------------------------------
el_box <- map_dfr(SEASONS, function(s) {
  sc <- paste0("E", s)
  games <- fromJSON(file.path(RAW_EL, "games", paste0(sc, ".json")))$data |>
    filter(played, phaseType$code %in% names(EL_PHASE_MAP),
           EL_PHASE_MAP[phaseType$code] == "regular")
  map_dfr(seq_len(nrow(games)), function(i) {
    path <- file.path(RAW_EL, "box", sc, paste0(games$gameCode[i], ".json"))
    if (!file.exists(path)) return(NULL)
    box <- fromJSON(path)
    map_dfr(c("local", "road"), function(side) {
      p <- box[[side]]$players
      st <- p$stats
      tibble(season = s, team = games[[side]]$club$code[i],
             player_id = p$player$person$code, player = p$player$person$name,
             position = pos_group(p$player$positionName),
             minutes = st$timePlayed / 60, p2a = st$fieldGoalsAttempted2, p3a = st$fieldGoalsAttempted3,
             fta = st$freeThrowsAttempted, ast = st$assistances, tov = st$turnovers,
             orb = st$offensiveRebounds, drb = st$defensiveRebounds, blk = st$blocksFavour,
             stl = st$steals, pf = st$foulsCommited, pts = st$points)
    })
  })
}) |>
  filter(minutes > 0) |>
  group_by(season, team, player_id) |>
  summarise(player = first(player), position = mode1(position), games = n(),
            across(minutes:pts, sum), .groups = "drop") |>
  mutate(league = "EuroLeague")

el <- el_box |>
  left_join(zone_counts |> filter(league == "EuroLeague") |> select(-league, -player),
            by = c("season", "team", "player_id"))

# ---- Combine stints into player-seasons -------------------------------------------------
stints <- bind_rows(nba, el) |>
  mutate(across(all_of(c(unname(ZONE_COLS), "shots")), \(x) coalesce(x, 0L))) |>
  left_join(team_pace, by = c("league", "season", "team")) |>
  mutate(poss = minutes * poss_per_min)

player_seasons <- stints |>
  group_by(league, season, player_id) |>
  arrange(desc(minutes), .by_group = TRUE) |>
  summarise(player = first(player), position = mode1(position),
            team = first(team), teams = paste(unique(team), collapse = "/"),
            across(c(games, minutes, poss, p2a, p3a, fta, ast, tov, orb, drb, blk, stl, pf, pts,
                     all_of(unname(ZONE_COLS)), shots), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(fga = p2a + p3a)

write_parquet(player_seasons, file.path(PROCESSED, "player_seasons.parquet"))

# Coverage check: share of field-goal attempts with a location, per league and season
player_seasons |>
  group_by(league, season) |>
  summarise(players = n(), fga = sum(fga), located = sum(shots) / sum(fga),
            no_position = mean(is.na(position)), .groups = "drop") |>
  print(n = 40)
