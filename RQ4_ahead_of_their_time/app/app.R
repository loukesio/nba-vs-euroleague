# RQ4 explorer: player and team cards on demand.
# Run from anywhere:  shiny::runApp(here::here("RQ4_ahead_of_their_time", "app"))
# Uses the functions and tables of RQ4 steps 01-04 (run those first); nothing is recomputed upstream.

source(here::here("RQ4_ahead_of_their_time", "scripts", "04_player_cards.R"))
source(here::here("RQ4_ahead_of_their_time", "app", "parse.R"))
suppressMessages({ library(shiny); library(bslib); library(DT) })

# shots are read lazily, only the league and seasons a card needs
shots_for <- function(league, seasons) {
  arrow::open_dataset(file.path(PROCESSED, "shots.parquet")) |>
    filter(league == !!league, season %in% seasons, phase == "regular") |>
    collect()
}

# ---- choices ------------------------------------------------------------------------------------
player_index <- pst |>
  group_by(league, player) |>
  summarise(from = min(season), to = max(season), minutes = sum(minutes), .groups = "drop") |>
  arrange(desc(minutes)) |>
  mutate(value = paste(league, player, sep = "|"),
         label = sprintf("%s  ·  %s %s-%s", nice_player(player), league, season_short(from), season_short(to)))
PLAYER_CHOICES <- setNames(player_index$value, player_index$label)

team_index <- tname |>
  group_by(league, team) |>
  summarise(name = name[which.max(season)], from = min(season), to = max(season), .groups = "drop") |>
  arrange(league, name) |>
  mutate(value = paste(league, team, sep = "|"), label = sprintf("%s  ·  %s", name, league))
TEAM_CHOICES <- split(setNames(team_index$value, team_index$label), team_index$league)
# every name a franchise has had, for free-text matching
team_aliases <- tname |> distinct(league, team, name) |> mutate(value = paste(league, team, sep = "|"))

# split a career into n consecutive eras of (nearly) equal length
split_career <- function(seasons, n) {
  n <- min(n, length(seasons))
  unname(split(seasons, cut(seq_along(seasons), n, labels = FALSE)))
}
era_txt <- function(ss) if (length(ss) == 1) season_label(ss) else
  sprintf("%s to %s", season_label(min(ss)), season_label(max(ss)))

# ---- UI -----------------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "NBA vs EuroLeague · who would fit where?",
  theme = bs_theme(version = 5, primary = "#1f5aa6"),
  sidebar = sidebar(
    width = 360,
    card(
      card_header("Ask in your own words"),
      textAreaInput("ask", NULL, rows = 2,
                    placeholder = "e.g. Spanoulis 2010-2012 vs 2013-2016 vs 2017-2020"),
      actionButton("ask_go", "Go", icon = icon("wand-magic-sparkles"), class = "btn-primary"),
      uiOutput("understood"),
      helpText("Years are season start years (2012 = 2012/13); '2012/13' works too.",
               "Try: 'Ginobili in 3 eras', 'Olympiacos 2012-2013', 'Doncic NBA today 2022'.")
    ),
    radioButtons("mode", NULL, c("Player" = "player", "Team" = "team"), inline = TRUE),
    conditionalPanel("input.mode == 'player'",
      selectizeInput("player", "Player", choices = NULL, options = list(placeholder = "Type a name...")),
      radioButtons("n_eras", "Card",
                   c("One period" = 1, "Compare 2 eras" = 2, "Compare 3 eras" = 3), inline = TRUE),
      uiOutput("era_sliders"),
      textInput("nickname", "Card subtitle (optional)", "")
    ),
    conditionalPanel("input.mode == 'team'",
      selectizeInput("team", "Team", choices = TEAM_CHOICES, selected = "EuroLeague|OLY"),
      uiOutput("team_seasons")
    ),
    selectInput("now", "'Today' season", rev(SEASONS), selected = max(SEASONS)),
    sliderInput("n_show", "Teams shown per panel", 5, 15, 8, step = 1),
    input_task_button("go", "Build card", icon = icon("play")),
    helpText("Player fits use 2,000 posterior draws of the player's fingerprint;",
             "team similarities are point estimates."),
    downloadButton("download", "Download card (PNG)")
  ),
  navset_card_tab(
    full_screen = TRUE,
    nav_panel("Card", uiOutput("card_ui")),
    nav_panel("All team fits", DTOutput("fits")),
    nav_panel("Similar players", DTOutput("comps"))
  )
)

# ---- server -------------------------------------------------------------------------------------
server <- function(input, output, session) {
  updateSelectizeInput(session, "player", choices = PLAYER_CHOICES, server = TRUE,
                       selected = "EuroLeague|SPANOULIS, VASSILIS")
  spec <- reactiveVal(NULL)          # what to build: set by the button or by the free-text box
  era_defaults <- reactiveVal(NULL)  # eras from the free-text box, to show in the sliders

  player_row <- reactive({ req(input$player); player_index[player_index$value == input$player, ] })

  output$era_sliders <- renderUI({
    r <- player_row(); n <- as.integer(input$n_eras)
    d <- era_defaults()
    eras <- if (!is.null(d) && d$key == input$player && length(d$eras) == n) d$eras else
      split_career(seq(r$from, r$to), n)
    tagList(lapply(seq_len(n), function(i) {
      sliderInput(paste0("era", i), if (n == 1) "Seasons (start year)" else sprintf("Era %d", i),
                  min = r$from, max = r$to, value = range(eras[[min(i, length(eras))]]),
                  step = 1, sep = "")
    }))
  })
  output$team_seasons <- renderUI({
    req(input$team)
    r <- team_index[team_index$value == input$team, ]
    d <- era_defaults()
    v <- if (!is.null(d) && d$key == input$team) range(d$eras[[1]]) else c(r$from, r$to)
    sliderInput("t_seasons", "Seasons (start year)", min = r$from, max = r$to, value = v, step = 1, sep = "")
  })

  # ---- build from the sidebar
  observeEvent(input$go, {
    if (input$mode == "player") {
      n <- as.integer(input$n_eras)
      eras <- lapply(seq_len(n), \(i) { v <- input[[paste0("era", i)]]; req(v); seq(v[1], v[2]) })
      spec(list(mode = "player", key = input$player, eras = eras, now = as.integer(input$now),
                nickname = input$nickname))
    } else {
      req(input$t_seasons)
      spec(list(mode = "team", key = input$team, eras = list(seq(input$t_seasons[1], input$t_seasons[2])),
                now = as.integer(input$now)))
    }
  })

  # ---- build from free text
  parsed <- reactiveVal(NULL)
  observeEvent(input$ask_go, {
    req(nzchar(trimws(input$ask)))
    p <- parse_request(input$ask, player_index, team_aliases)
    parsed(p)
    if (is.na(p$mode)) return()
    now <- if (is.null(p$now)) as.integer(input$now) else p$now
    if (p$mode == "player") {
      r <- player_index[player_index$value == p$key, ]
      career <- seq(r$from, r$to)
      eras <- if (length(p$eras)) p$eras else split_career(career, if (is.null(p$split)) 1 else p$split)
      eras <- Filter(length, lapply(eras, \(e) intersect(e, career)))
      if (!length(eras)) eras <- list(career)
      p$eras <- eras
      updateRadioButtons(session, "mode", selected = "player")
      updateSelectizeInput(session, "player", choices = PLAYER_CHOICES, server = TRUE, selected = p$key)
      updateRadioButtons(session, "n_eras", selected = length(eras))
    } else {
      r <- team_index[team_index$value == p$key, ]
      span <- seq(r$from, r$to)
      eras <- if (length(p$eras)) Filter(length, lapply(p$eras, \(e) intersect(e, span))) else list(span)
      if (!length(eras)) eras <- list(span)
      p$dropped <- length(eras) - 1
      p$eras <- eras[1]
      updateRadioButtons(session, "mode", selected = "team")
      updateSelectizeInput(session, "team", selected = p$key)
    }
    if (!is.null(p$now)) updateSelectInput(session, "now", selected = p$now)
    era_defaults(list(key = p$key, eras = p$eras))
    parsed(p)
    spec(list(mode = p$mode, key = p$key, eras = p$eras, now = now, nickname = ""))
  })

  output$understood <- renderUI({
    p <- parsed(); req(p)
    if (is.na(p$mode)) return(div(class = "text-danger small mt-2",
                                  "I couldn't find a player or team in that. Try a surname or a team name."))
    who <- if (p$mode == "player") names(PLAYER_CHOICES)[PLAYER_CHOICES == p$key] else
      team_index$label[team_index$value == p$key]
    eras <- if (is.null(p$eras)) "" else paste(vapply(p$eras, era_txt, ""), collapse = "  |  ")
    div(class = "small mt-2",
        strong("Understood: "), who, br(),
        if (p$mode == "player") sprintf("%d era%s: %s", length(p$eras), if (length(p$eras) > 1) "s" else "", eras)
        else paste("Seasons:", eras),
        if (isTRUE(p$dropped > 0)) tagList(br(), em("Team cards take one era; I used the first.")),
        if (!is.null(p$now)) tagList(br(), sprintf("Today = %s", season_label(p$now))),
        if (length(p$alternatives)) tagList(br(), em("Other matches: ", paste(p$alternatives, collapse = "; "))))
  })

  # ---- compute
  result <- reactive({
    s <- spec(); req(s)
    key <- strsplit(s$key, "|", fixed = TRUE)[[1]]
    if (s$mode == "player") {
      have <- pst |> filter(league == key[1], player == key[2], season %in% unlist(s$eras))
      validate(need(nrow(have) > 0, "No seasons for this player in that range."))
      eras <- Filter(\(e) any(have$season %in% e), s$eras)
      fl <- player_fit_eras(key[1], key[2], eras, now = s$now)
      nick <- if (nzchar(s$nickname %||% "")) s$nickname else NULL
      card <- player_card_eras(fl, nickname = nick, n_show = input$n_show,
                               shots = shots_for(key[1], unlist(map(fl, "seasons"))))
      list(type = "player", fl = fl, card = card, n = length(fl),
           slug = paste0(slug(key[2]), if (length(fl) > 1) paste0("_", length(fl), "eras") else ""))
    } else {
      ss <- s$eras[[1]]
      have <- tdev |> filter(league == key[1], team == key[2], season %in% ss)
      validate(need(nrow(have) > 0, "No seasons for this team in that range."))
      f <- team_fit(key[1], key[2], seasons = ss, now = s$now)
      list(type = "team", f = f, card = team_card(f, n_show = input$n_show), n = 1,
           slug = paste0("team_", tolower(key[2]), "_", min(ss), "_", max(ss)))
    }
  })
  card_size <- reactive({
    r <- result()
    if (r$type == "team") c(13, 12) else if (r$n > 1) c(14, 15) else c(13, 10)
  })

  output$card_ui <- renderUI({
    if (is.null(spec())) return(div(class = "p-4 text-muted",
      "Ask in your own words, or pick a player or team and press 'Build card'."))
    tagList(
      div(class = "small text-muted mb-2",
          "Drawn at full size and scaled to the window; use the expand icon (bottom right) or the ",
          "download button to see it larger."),
      imageOutput("card", height = "auto")
    )
  })
  # draw the card at its design size (as the saved PNGs), then let the browser scale it evenly
  card_png <- reactive({
    r <- result(); sz <- card_size()
    file <- tempfile(fileext = ".png")
    ggsave(file, r$card, width = sz[1], height = sz[2], dpi = 150, bg = "white")
    file
  })
  output$card <- renderImage({
    list(src = card_png(), contentType = "image/png", alt = "card",
         style = "width: 100%; height: auto; display: block;")
  }, deleteFile = FALSE)

  num <- \(d) mutate(d, across(where(is.double), \(x) round(x, 3)))
  output$fits <- renderDT({
    r <- result()
    d <- if (r$type == "player") {
      imap_dfr(r$fl, \(f, e) f$fits |> mutate(era = e)) |>
        transmute(era, when, season = season_label(season), team = name, fit, fit_lo, fit_hi,
                  role, system, p_best)
    } else {
      r$f$teams |> transmute(when, season = season_label(season), team = name, similarity)
    }
    datatable(num(d), filter = "top", rownames = FALSE, options = list(pageLength = 25))
  })
  output$comps <- renderDT({
    r <- result()
    d <- if (r$type == "player") {
      imap_dfr(r$fl, \(f, e) f$comps |> mutate(era = e)) |> arrange(era, when, desc(similarity)) |>
        transmute(era, when, season = season_label(season), player = nice_player(player), team, similarity)
    } else {
      r$f$players |> transmute(when, position = POS_WORDS[position], season = season_label(season),
                               player = nice_player(player), team, similarity)
    }
    datatable(num(d), filter = "top", rownames = FALSE, options = list(pageLength = 25))
  })

  output$download <- downloadHandler(
    filename = \() paste0("card_", result()$slug, ".png"),
    content = \(file) file.copy(card_png(), file)
  )
}

shinyApp(ui, server)
