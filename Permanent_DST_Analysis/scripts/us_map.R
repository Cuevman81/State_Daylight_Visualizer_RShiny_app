# ==============================================================================
# US MAP — Winter daylight by state under the two "lock the clock" options.
#
#   Row 1  SUNRISE: latest the sun rises on a winter morning
#   Row 2  SUNSET : earliest the sun sets on a winter evening
#   Cols   Permanent daylight saving time (std offset +1)  |  Permanent standard time
#
# In winter you can brighten the morning OR the evening, not both. Permanent DST
# delays sunrise into dark 8-9 AM mornings but keeps evening light; permanent
# standard time does the reverse. In both rows, RED = more darkness during waking
# hours. Metric depends on latitude AND position within the time zone.
#
# Sun times: {suncalc}. Boundaries: {maps}. Author: Rodney Cuevas
# ==============================================================================
suppressPackageStartupMessages({
  library(suncalc); library(maps); library(mapdata); library(ggplot2)
  library(dplyr); library(tidyr); library(patchwork)
})

BASE <- "/Users/rodneycuevas/Library/CloudStorage/OneDrive-MississippiDepartmentofEnvironmentalQuality/Documents/Github/Sunrise_Sunset_Data/Permanent_DST_Analysis"
PLOT_DIR <- file.path(BASE, "plots")
DATA_DIR <- file.path(BASE, "data")

# ------------------------------------------------------------------------------
# 1. Representative point + predominant standard UTC offset for each lower-48 state
# ------------------------------------------------------------------------------
states <- tibble::tribble(
  ~state,            ~lat,   ~lon,     ~off, ~exempt,
  "alabama",         32.8,  -86.8,     -6,   FALSE,
  "arizona",         34.3, -111.7,     -7,   TRUE,
  "arkansas",        34.9,  -92.4,     -6,   FALSE,
  "california",      37.2, -119.4,     -8,   FALSE,
  "colorado",        39.0, -105.5,     -7,   FALSE,
  "connecticut",     41.6,  -72.7,     -5,   FALSE,
  "delaware",        39.0,  -75.5,     -5,   FALSE,
  "florida",         28.6,  -82.4,     -5,   FALSE,
  "georgia",         32.6,  -83.4,     -5,   FALSE,
  "idaho",           44.3, -114.5,     -7,   FALSE,
  "illinois",        40.0,  -89.2,     -6,   FALSE,
  "indiana",         39.9,  -86.3,     -5,   FALSE,
  "iowa",            42.0,  -93.5,     -6,   FALSE,
  "kansas",          38.5,  -98.4,     -6,   FALSE,
  "kentucky",        37.5,  -85.3,     -5,   FALSE,
  "louisiana",       31.0,  -92.0,     -6,   FALSE,
  "maine",           45.4,  -69.2,     -5,   FALSE,
  "maryland",        39.0,  -76.8,     -5,   FALSE,
  "massachusetts",   42.3,  -71.8,     -5,   FALSE,
  "michigan",        44.3,  -85.4,     -5,   FALSE,
  "minnesota",       46.3,  -94.3,     -6,   FALSE,
  "mississippi",     32.7,  -89.7,     -6,   FALSE,
  "missouri",        38.4,  -92.5,     -6,   FALSE,
  "montana",         47.0, -109.6,     -7,   FALSE,
  "nebraska",        41.5,  -99.8,     -6,   FALSE,
  "nevada",          39.3, -116.6,     -8,   FALSE,
  "new hampshire",   43.7,  -71.6,     -5,   FALSE,
  "new jersey",      40.1,  -74.7,     -5,   FALSE,
  "new mexico",      34.4, -106.1,     -7,   FALSE,
  "new york",        42.9,  -75.6,     -5,   FALSE,
  "north carolina",  35.5,  -79.4,     -5,   FALSE,
  "north dakota",    47.4, -100.5,     -6,   FALSE,
  "ohio",            40.3,  -82.8,     -5,   FALSE,
  "oklahoma",        35.6,  -97.5,     -6,   FALSE,
  "oregon",          43.9, -120.6,     -8,   FALSE,
  "pennsylvania",    40.9,  -77.8,     -5,   FALSE,
  "rhode island",    41.7,  -71.6,     -5,   FALSE,
  "south carolina",  33.9,  -80.9,     -5,   FALSE,
  "south dakota",    44.4, -100.2,     -6,   FALSE,
  "tennessee",       35.9,  -86.4,     -6,   FALSE,
  "texas",           31.5,  -99.3,     -6,   FALSE,
  "utah",            39.3, -111.7,     -7,   FALSE,
  "vermont",         44.1,  -72.7,     -5,   FALSE,
  "virginia",        37.5,  -78.9,     -5,   FALSE,
  "washington",      47.4, -120.5,     -8,   FALSE,
  "west virginia",   38.6,  -80.6,     -5,   FALSE,
  "wisconsin",       44.6,  -89.9,     -6,   FALSE,
  "wyoming",         43.0, -107.5,     -7,   FALSE,
  "alaska",          61.22,-149.90,    -9,   FALSE,   # Anchorage; observes DST
  "hawaii",          21.31,-157.86,   -10,   TRUE     # Honolulu; no DST
)

# ------------------------------------------------------------------------------
# 2. Latest winter sunrise & earliest winter sunset (wall clock), both offsets
#    Window spans early-Dec (earliest sunset) through early-Jan (latest sunrise).
# ------------------------------------------------------------------------------
winter_days <- seq(as.Date("2025-12-01"), as.Date("2026-01-20"), by = "day")
grid <- tidyr::crossing(states, date = winter_days)

st <- getSunlightTimes(data = data.frame(date = grid$date, lat = grid$lat, lon = grid$lon),
                       keep = c("sunrise", "sunset"), tz = "UTC")
grid$sunrise <- st$sunrise
grid$sunset  <- st$sunset

# wall-clock decimal hour after applying an offset (POSIXct shift handles UTC wrap)
localdec <- function(posix, offh) {
  p <- posix + offh * 3600
  as.numeric(format(p, "%H", tz = "UTC")) + as.numeric(format(p, "%M", tz = "UTC")) / 60
}

# Exempt states (Arizona) keep standard time even under a permanent-DST law,
# so their "permanent DST" offset stays at the standard offset (no +1).
metric <- grid %>%
  mutate(dst_off = off + ifelse(exempt, 0, 1),
         sr_std = localdec(sunrise, off),   sr_dst = localdec(sunrise, dst_off),
         ss_std = localdec(sunset,  off),   ss_dst = localdec(sunset,  dst_off)) %>%
  group_by(state, exempt) %>%
  summarise(latest_sr_std   = max(sr_std), latest_sr_dst   = max(sr_dst),
            earliest_ss_std = min(ss_std), earliest_ss_dst = min(ss_dst),
            .groups = "drop")

fmt_clock <- function(h) {                       # 24h decimal -> 12h clock string
  hh <- floor(h); mm <- round((h - hh) * 60)
  ap <- ifelse(hh < 12, "AM", "PM"); d <- ((hh - 1) %% 12) + 1
  sprintf("%d:%02d %s", d, mm, ap)
}

# --- binning (RED = darker waking hours in BOTH rows) ---
sr_breaks <- c(-Inf, 7.5, 8.0, 8.5, 9.0, Inf)
sr_labs   <- c("before 7:30", "7:30–8:00", "8:00–8:30", "8:30–9:00", "9:00 or later")
ss_breaks <- c(-Inf, 16.5, 17.0, 17.5, 18.0, Inf)
ss_labs   <- c("before 4:30", "4:30–5:00", "5:00–5:30", "5:30–6:00", "6:00 or later")
bin_sr <- function(h) cut(h, sr_breaks, sr_labs, right = FALSE)
bin_ss <- function(h) cut(h, ss_breaks, ss_labs, right = FALSE)

ramp <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")   # light -> dark
pal_sr <- setNames(ramp,       sr_labs)   # late sunrise = dark red
pal_ss <- setNames(rev(ramp),  ss_labs)   # early sunset = dark red

# console + CSV
cat("\nWorst DARK-MORNING states (latest winter sunrise, permanent DST):\n")
metric %>% arrange(desc(latest_sr_dst)) %>%
  transmute(state, sunrise_DST = fmt_clock(latest_sr_dst)) %>% head(8) %>%
  as.data.frame() %>% print(row.names = FALSE)
cat("\nWorst DARK-EVENING states (earliest winter sunset, permanent standard time):\n")
metric %>% arrange(earliest_ss_std) %>%
  transmute(state, sunset_STD = fmt_clock(earliest_ss_std)) %>% head(8) %>%
  as.data.frame() %>% print(row.names = FALSE)

write.csv(
  metric %>% transmute(state,
                       latest_winter_sunrise_permanent_dst      = fmt_clock(latest_sr_dst),
                       latest_winter_sunrise_permanent_standard = fmt_clock(latest_sr_std),
                       earliest_winter_sunset_permanent_dst     = fmt_clock(earliest_ss_dst),
                       earliest_winter_sunset_permanent_standard= fmt_clock(earliest_ss_std)),
  file.path(DATA_DIR, "us_states_winter_daylight.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. Assemble the map (lower 48 + Alaska & Hawaii insets)
# ------------------------------------------------------------------------------
us <- map_data("state")
sc_lv <- c("Permanent daylight saving time", "Permanent standard time")

# Inset polygons: pull AK/HI from worldHires (Alaska is a USA *subregion*; Hawaii is
# its own *region*), drop antimeridian/outlying pieces, then scale + translate into
# the empty lower-left of the lower-48 frame.
build_inset <- function(poly, region_name, sx, sy, cx, cy, grp_off, drop_pred) {
  bad  <- unique(poly$group[drop_pred(poly$long, poly$lat)])
  poly <- poly %>% filter(!(group %in% bad))
  x0 <- mean(range(poly$long)); y0 <- mean(range(poly$lat))
  poly %>% mutate(long = (long - x0) * sx + cx,
                  lat  = (lat  - y0) * sy + cy,
                  region = region_name, subregion = NA_character_,
                  group = group + grp_off)
}
ak_raw <- map_data("worldHires", "USA") %>% filter(subregion == "Alaska")
hi_raw <- map_data("worldHires", "Hawaii")
ak_poly <- build_inset(ak_raw, "alaska", sx = 0.19, sy = 0.36, cx = -117, cy = 26.5,
                       grp_off = 1e5, drop_pred = function(lon, lat) lon < -169 | lon > -128)
hi_poly <- build_inset(hi_raw, "hawaii", sx = 1.2, sy = 1.2, cx = -105, cy = 24.0,
                       grp_off = 2e5, drop_pred = function(lon, lat) lon < -161)
us_all <- bind_rows(us, ak_poly, hi_poly)

to_long <- function(dst_cat, std_cat) {
  metric %>%
    transmute(state,
              `Permanent daylight saving time` = dst_cat,
              `Permanent standard time`        = std_cat) %>%
    pivot_longer(-state, names_to = "scenario", values_to = "cat") %>%
    mutate(scenario = factor(scenario, levels = sc_lv))
}
long_sr <- to_long(bin_sr(metric$latest_sr_dst),   bin_sr(metric$latest_sr_std))
long_ss <- to_long(bin_ss(metric$earliest_ss_dst), bin_ss(metric$earliest_ss_std))

join_map <- function(long) {
  us_all %>% left_join(long, by = c("region" = "state"), relationship = "many-to-many") %>%
    filter(!is.na(scenario)) %>% mutate(scenario = droplevels(scenario))
}
map_sr <- join_map(long_sr)
map_ss <- join_map(long_ss)

# Arizona exempt marker (only differs from neighbors in the permanent-DST column)
az <- us %>% filter(region == "arizona")
az_hi  <- az %>% mutate(scenario = factor(sc_lv[1], levels = sc_lv))
az_lab <- data.frame(lon = -111.7, lat = 34.3, scenario = factor(sc_lv[1], levels = sc_lv))

# AK & HI insets: a darker outline keeps pale fills (e.g. Hawaii's early sunrise)
# visible against the off-white background; small name tag for identification.
inset_outline <- bind_rows(ak_poly, hi_poly)      # no scenario -> drawn in every panel
inset_name <- data.frame(lon = c(-117, -105), lat = c(20.8, 20.8),
                         label = c("Alaska", "Hawaii"))

INK <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#898781"; SURF <- "#fcfcfb"

make_row <- function(mapdata, pal, legend_title, row_title, show_strip) {
  ggplot(mapdata, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = cat), color = "#ffffff", linewidth = 0.18) +
    geom_polygon(data = inset_outline, aes(long, lat, group = group),
                 fill = NA, color = "#5a5a5a", linewidth = 0.3) +
    geom_polygon(data = az_hi, aes(long, lat, group = group),
                 fill = NA, color = "#1a1a1a", linewidth = 0.5, linetype = "22") +
    geom_text(data = az_lab, aes(lon, lat, group = NULL), label = "AZ\nexempt",
              fontface = "plain", size = 2.5, color = "#1a1a1a", lineheight = 0.9) +
    geom_text(data = inset_name, aes(lon, lat, group = NULL, label = label),
              size = 2.7, color = INK2, fontface = "plain") +
    facet_wrap(~scenario, ncol = 2) +
    coord_quickmap() +
    scale_fill_manual(values = pal, drop = FALSE, na.translate = FALSE,
                      name = legend_title,
                      guide = guide_legend(nrow = 1, label.position = "bottom",
                                           keywidth = 2.3, keyheight = 0.55)) +
    labs(title = row_title) +
    theme_void(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = SURF, color = NA),
      panel.background = element_rect(fill = SURF, color = NA),
      strip.text = if (show_strip)
        element_text(face = "bold", size = 12.5, color = INK, margin = margin(4, 0, 6, 0))
        else element_blank(),
      plot.title = element_text(face = "bold", size = 13.5, color = INK, hjust = 0,
                                margin = margin(2, 0, 2, 0)),
      legend.position = "top", legend.justification = "left",
      legend.title = element_text(size = 10, color = INK2, face = "bold"),
      legend.text  = element_text(size = 9, color = INK2),
      legend.margin = margin(0, 0, 2, 0),
      plot.margin = margin(2, 8, 4, 8)
    )
}

p_sr <- make_row(map_sr, pal_sr, "Latest winter sunrise  (redder = darker mornings)",
                 "SUNRISE  ·  latest the sun comes up on a winter morning", TRUE)
p_ss <- make_row(map_ss, pal_ss, "Earliest winter sunset  (redder = darker evenings)",
                 "SUNSET  ·  earliest the sun goes down on a winter evening", FALSE)

combined <- (p_sr / p_ss) +
  plot_annotation(
    title = "Permanent DST vs. Permanent Standard Time: Winter Daylight, State by State",
    subtitle = paste0("In winter you can brighten the MORNING or the EVENING — not both. Left column = permanent DST (sunrise pushed into dark 8–9 AM,\n",
                      "but evening light kept); right column = permanent standard time (bright mornings, but the sun sets before 5 PM). In both rows red =\n",
                      "more darkness during waking hours. Alaska is the extreme: under permanent DST the sun would not rise until ~11 AM."),
    caption = paste0("Arizona does not observe daylight saving time — it stays on standard time year-round, so it is unchanged by a permanent-DST law (marked on the DST maps). ",
                     "Hawaii is exempt\ntoo (it barely changes near the equator). Alaska & Hawaii shown as insets, not to scale (Alaska at Anchorage, Hawaii at Honolulu). ",
                     "Representative point per state · predominant\ntime zone. Sun times: {suncalc} · Boundaries: {maps} · Analysis: Rodney Cuevas"),
    theme = theme(
      plot.background = element_rect(fill = SURF, color = NA),
      plot.title    = element_text(face = "bold", size = 18, color = INK, hjust = 0,
                                   margin = margin(4, 0, 4, 0)),
      plot.subtitle = element_text(size = 10.8, color = INK2, hjust = 0, lineheight = 1.15,
                                   margin = margin(0, 0, 6, 0)),
      plot.caption  = element_text(size = 8, color = MUTED, hjust = 0, lineheight = 1.2,
                                   margin = margin(8, 0, 0, 0)),
      plot.margin = margin(14, 16, 10, 16)
    )
  )

outfile <- file.path(PLOT_DIR, "04_us_map_winter_daylight.png")
ggsave(outfile, combined, width = 14, height = 11.4, dpi = 200, bg = SURF)
cat("\nwrote", outfile, "\n")
