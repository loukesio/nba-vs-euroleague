# Project-wide settings. Source this at the top of every script.

# Large data lives on the external drive; each RQ folder's `data/` is a symlink into it.
DATA_ROOT <- Sys.getenv("NBA_EL_DATA", "/Volumes/Untitled/NBA_VS_Euroleague_data")
if (!dir.exists(DATA_ROOT)) stop("Data drive not found: ", DATA_ROOT, " (set NBA_EL_DATA to override)")

RAW_NBA   <- file.path(DATA_ROOT, "raw", "nba")
RAW_EL    <- file.path(DATA_ROOT, "raw", "euroleague")
PROCESSED <- file.path(DATA_ROOT, "processed")

# Seasons are labelled by their starting year: 2010 = 2010/11.
SEASONS       <- 2010:2025   # main analysis window
SEASONS_LONG  <- 2001:2025   # NBA box scores back to 2001/02, used only for the lead-lag model
TODAY_SEASONS <- 2023:2025   # "today" for RQ1

# Postseason definition (decided with the user):
#   EuroLeague: PO (playoff series) + FF (Final Four) = playoffs; RS + TS (Top 16) = regular season; PI (play-in) dropped.
#   NBA: season type 2 = regular season, 3 = playoffs; play-in and All-Star games dropped.
EL_PHASE_MAP <- c(RS = "regular", TS = "regular", PO = "playoffs", FF = "playoffs")

season_label <- function(s) sprintf("%d/%02d", s, (s + 1) %% 100)
season_short <- function(s) sprintf("%02d/%02d", s %% 100, (s + 1) %% 100)

# EuroLeague names come as "SURNAME, FIRST": turn into "First Surname" (NBA names pass through)
nice_player <- function(x) {
  el <- grepl(",", x)
  x[el] <- tools::toTitleCase(tolower(sub("^(.*), (.*)$", "\\2 \\1", x[el])))
  x
}

# NBA Stats team tricodes (shot data) -> ESPN abbreviations (box scores, team_games)
NBA_TEAM_MAP <- c(SAS = "SA", NOH = "NO", NOP = "NO", NYK = "NY", GSW = "GS", NJN = "NJ",
                  WAS = "WSH", UTA = "UTAH")
