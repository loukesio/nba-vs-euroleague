# NBA vs EuroLeague, 2010/11–2025/26

> Manuscript in preparation for **SportRxiv** (`manuscript/`). Figures are drawn with the
> [ggbasket](https://loukesio.github.io/ggbasket/) package. Code: MIT licence (`LICENSE`).

A reproducible, Bayesian update of Mandić, Jakovljević, Erčulj & Štrumbelj (2019),
*Trends in NBA and Euroleague basketball*, PLOS ONE 14(10): e0223524 (`paper/`).

| Question | Folder |
|---|---|
| **RQ1** How do the leagues differ today (2023/24–2025/26)? Box scores, shot zones, shot maps | `RQ1_differences_today/` |
| **RQ2** How have they changed since 2010/11, and does one league follow the other? | `RQ2_trends_leadlag/` |
| **RQ3** How does play change in the playoffs? | `RQ3_playoffs/` |
| **RQ4** Which teams and players embodied their season, played differently, or were ahead of their time? | `RQ4_ahead_of_their_time/` |

Each RQ folder has `scripts/` (run in numeric order), `results/figures`, `results/tables`,
and `data/` → a symlink to the external data drive (cached model fits and intermediate data).
Stan programs live in `RQ2_trends_leadlag/models/`.

## Running

```r
source("run_all.R")   # downloads (cached), builds tables, fits models, draws figures
```

Data live on the external drive (`/Volumes/Untitled/NBA_VS_Euroleague_data`); override with the
environment variable `NBA_EL_DATA`. Packages: tidyverse, arrow, httr2, jsonlite, hoopR, brms,
cmdstanr (CmdStan 2.40), posterior, patchwork, hexbin; exact versions are listed in `package_versions.txt`.

## Data

| | Source | Seasons |
|---|---|---|
| NBA box scores | hoopR / sportsdataverse release files (ESPN) | 2001/02–2025/26 |
| NBA shots (x, y) | hoopR `load_nba_stats_shots()` (NBA Stats) | 2010/11–2025/26 |
| EuroLeague box scores | official API `api-live.euroleague.net` v2/v3 | 2001/02–2025/26 |
| EuroLeague shots (x, y) | official API `live.euroleague.net/api/Points` | 2010/11–2025/26 |
| Original paper data | PLOS ONE S1 Data | 2000/01–2016/17 |

Seasons before 2010/11 are used only for the lead-lag model (RQ2).

**Definitions**
- *Seasons* are labelled by their starting year (2010 = 2010/11).
- *Playoffs*: NBA playoffs (no play-in). EuroLeague: playoff series + Final Four; the old
  Top 16 counts as regular season; the 2025/26 play-in is dropped. (The 2019 paper put the
  Top 16 with the playoffs.)
- *Metrics* follow the 2019 paper exactly. Possessions = 2PA + 3PA + 0.5·FTA + TOV; counts are
  per 100 possessions; four factors for team and opponent; free throws per foul received; and the
  entropy of minutes played (rotation). Added here: possessions per 40 minutes, points per 100
  possessions, and the share of field-goal attempts taken from three.
- *Rebounds and turnovers* are player-credited sums in both leagues. Team rebounds and team
  turnovers are recorded differently by ESPN and the EuroLeague, so they are left out.
- *Shot coordinates* are in metres with the origin at the hoop. *Zones*: rim area (< 1.75 m),
  short mid-range (1.75–4.25 m), long mid-range (2-pointers beyond 4.25 m), corner 3 and
  above-the-break 3. Each league's own corner geometry is used (straight segment at 6.71 m
  NBA, 6.60 m FIBA).

**Known data caveats**
- EuroLeague rim shots are recorded about 1.1 m from the hoop, so they show up as a ring. This
  is why the rim area is 1.75 m.
- The ESPN team box score's `total_turnovers` double-counts in some seasons; `turnovers` and the
  player sums are used instead.
- `live.euroleague.net` rate-limits (HTTP 429), so shots are downloaded by one slow worker.

## Methods in one paragraph each
- **RQ1:** mixed models (brms), one per metric:
  `z ~ 0 + league + (1|team_season) + (1|opp_season), sigma ~ 0 + league`. Shot zones use
  binomial models with team-season effects. Accuracy across the court uses a binomial model with
  a 2-D smooth surface per league.
- **RQ2:** (1) the 2019 model (season means scattered around a linear trend) is written in Stan on
  sufficient statistics and validated on the authors' own data. It reproduces all 68 published
  estimates within 0.013. (2) Smooth RW2 trends per league, and the NBA − EuroLeague gap over
  time. (3) A shift model with a discrete lag marginalised out (P(lag = k)), and the "EuroLeague
  today = NBA in season X" time gap.
- **RQ3:** matched team-seasons (playoff teams vs their own regular season), with opponent-season
  effects. Playoff shift per league, and whether the NBA's shift points towards the EuroLeague.
- **RQ4:** a style profile per team-season (14 features) and per rotation player-season (13 features,
  relative to position), with conjugate empirical-Bayes shrinkage (Dirichlet-multinomial for the shot
  mix, gamma-Poisson for rates, normal-normal for game means). Typical/different = distance to the
  league-season average; "ahead of their time" = the season on the league's smoothed path through
  style space that a team or player is closest to. NBA players are linked to their shots by season,
  team and surname (99.6% of NBA field-goal attempts located).

## Reproducing

The raw and processed data (several GB) are not in this repository. `00_data/scripts/` downloads
everything from the public sources above (hoopR release files, EuroLeague live API) into the folder
given by `NBA_EL_DATA`; `run_all.R` then rebuilds every table, model and figure. Data remain subject
to their providers' terms.

## Citation

Theodosiou, L. (2026). *NBA and EuroLeague basketball, 2010/11–2025/26: a Bayesian replication and
update of Mandić et al. (2019)*. Preprint in preparation.
