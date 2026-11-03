# Hexagonal binning for shot charts (Goldsberry style: hexagon SIZE = frequency,
# COLOUR = efficiency). Bins are computed per group with hexbin, then drawn as polygons.

hex_bin <- function(shots, by = "league", xbins = 30, xbnds = c(-7.6, 7.6), ybnds = c(-1.7, 9.5)) {
  shots |>
    dplyr::filter(x >= xbnds[1], x <= xbnds[2], y >= ybnds[1], y <= ybnds[2]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(function(d, key) {
      hb <- hexbin::hexbin(d$x, d$y, xbins = xbins, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      centres <- hexbin::hcell2xy(hb)
      cells <- tibble::tibble(cell = hb@cell, cx = centres$x, cy = centres$y)
      d |>
        dplyr::mutate(cell = hb@cID) |>
        dplyr::group_by(cell) |>
        dplyr::summarise(n = dplyr::n(), pps = mean(pts), fg = mean(made), .groups = "drop") |>
        dplyr::left_join(cells, by = "cell") |>
        dplyr::mutate(share = n / sum(n))
    }) |>
    dplyr::ungroup()
}

# Polygon vertices for hexagons centred at (cx, cy); `scale` in [0, 1] shrinks each hexagon.
hex_polygons <- function(hb, xbins = 30, xbnds = c(-7.6, 7.6), scale = 1) {
  w <- diff(xbnds) / xbins                 # hexbin cell width
  r <- w / sqrt(3)                         # circumradius of a pointy-top hexagon
  ang <- pi / 6 + (0:5) * pi / 3
  hb$hex_id <- seq_len(nrow(hb))
  out <- hb[rep(seq_len(nrow(hb)), each = 6), ]
  s <- rep(scale, each = 6)
  out$px <- out$cx + s * r * cos(rep(ang, nrow(hb)))
  out$py <- out$cy + s * r * sin(rep(ang, nrow(hb)))
  out
}
