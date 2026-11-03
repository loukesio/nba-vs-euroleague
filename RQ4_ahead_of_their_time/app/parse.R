# Free-text requests -> a card spec. Rule-based, works offline. Understands e.g.
#   "Spanoulis 2010-2012 vs 2013-2016 vs 2017-2020"     three eras of one player
#   "Diamantidis", "Tony Parker 2012/13"               whole career / one season
#   "Ginobili in 3 eras", "Llull two eras"             career split into equal eras
#   "Olympiacos 2012-2013", "Spurs 2013 today 2020"    team card; 'today' season
# Years are season START years (2012 = 2012/13), as in the sliders. "2012/13" and "12/13" also work.

norm_text <- function(x) {
  x <- tolower(stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII"))
  x <- gsub("\\b(jr|sr|ii|iii|iv)\\b\\.?", "", x)
  gsub("\\s+", " ", trimws(gsub("[^a-z0-9/ -]", " ", gsub("[-\u2013\u2014]", " - ", x))))
}

# season expressions -> "S2012" tokens, so ranges and singles parse with one pattern
tag_seasons <- function(t) {
  t <- gsub("\\b(20[0-9]{2}) ?(?:/|-) ?([0-9]{2})\\b", "S\\1~\\2", t, perl = TRUE)
  t <- gsub("\\b([0-9]{2})/([0-9]{2})\\b", "S20\\1~\\2", t, perl = TRUE)
  # "2012-13" is one season only if 13 follows 12; otherwise it was a range of years
  m <- gregexpr("S(20[0-9]{2})~([0-9]{2})", t, perl = TRUE)
  regmatches(t, m) <- list(vapply(regmatches(t, m)[[1]], function(tok) {
    y <- as.integer(substr(tok, 2, 5)); y2 <- as.integer(substr(tok, 7, 8))
    if ((y + 1) %% 100 == y2) sprintf("S%d", y) else sprintf("S%d - S20%02d", y, y2)
  }, ""))
  gsub("\\b(20[0-9]{2})\\b", "S\\1", t, perl = TRUE)
}

WORD_N <- c(one = 1, two = 2, three = 3, "1" = 1, "2" = 2, "3" = 3)

parse_request <- function(text, players, teams) {
  t <- tag_seasons(norm_text(text))
  out <- list(now = NULL, eras = list(), split = NULL)

  m <- regmatches(t, regexec("\\b(today|now)( is| in| =)? S([0-9]{4})", t))[[1]]
  if (length(m)) { out$now <- as.integer(m[4]); t <- sub(m[1], " ", t, fixed = TRUE) }
  m <- regmatches(t, regexec("\\b(one|two|three|[123]) (eras|era|periods|parts|phases)\\b", t))[[1]]
  if (length(m)) out$split <- WORD_N[[m[2]]]

  rng <- regmatches(t, gregexpr("S[0-9]{4}(?: ?(?:-|to|until|through|till) ?S[0-9]{4})?", t, perl = TRUE))[[1]]
  out$eras <- lapply(rng, function(r) {
    y <- as.integer(regmatches(r, gregexpr("[0-9]{4}", r))[[1]])
    seq(min(y), max(y))
  })
  out$eras <- head(out$eras, 3)
  words <- gsub("S[0-9]{4}", " ", t)

  # players: full name > surname (+1 if first name also given); ties -> more minutes
  has <- function(phrase) grepl(paste0("\\b", phrase, "\\b"), words)
  pn <- norm_text(nice_player(players$player))
  first <- sub(" .*$", "", pn); sur <- sub("^\\S+ ", "", pn)
  cand <- which(nchar(sur) >= 3 & vapply(sur, has, TRUE))
  score <- vapply(cand, function(i) {
    # longer surname matches win ("Williams-Goss" over "Williams")
    (if (has(pn[i])) 4 else 2 + (nchar(first[i]) >= 2 && has(first[i]))) + nchar(sur[i]) / 100
  }, 1)
  if (length(cand)) {
    best <- cand[score == max(score)]
    if (length(out$eras)) {   # a player in both leagues: prefer the league his years fall in
      yrs <- unlist(out$eras)
      ov <- vapply(best, function(i) sum(yrs >= players$from[i] & yrs <= players$to[i]), 1)
      best <- best[ov == max(ov)]
    }
    lg <- if (grepl("\\bnba\\b", words)) "NBA" else if (grepl("\\beuro ?league\\b", words)) "EuroLeague" else NA
    pick <- best[order(-(players$league[best] == lg %in% TRUE), -players$minutes[best])]
    out$mode <- "player"; out$key <- players$value[pick[1]]
    out$alternatives <- players$label[setdiff(cand[order(-score, -players$minutes[cand])], pick[1])][1:3]
    out$alternatives <- out$alternatives[!is.na(out$alternatives)]
    return(out)
  }

  # teams: any of a franchise's names over the years; the longest matching name wins
  tn <- norm_text(teams$name)
  hit <- which(nchar(tn) >= 3 & vapply(tn, has, TRUE))
  if (length(hit)) {
    pick <- hit[order(-nchar(tn[hit]))][1]
    out$mode <- "team"; out$key <- teams$value[pick]
    return(out)
  }
  out$mode <- NA
  out
}
