# ==============================================================================
# US MAP — the permanent-DST trade-off in DAY COUNTS, state by state.
#
# The Jackson bar chart (figure 02) asks: how many days a year does the sun
# rise after 7:30 AM (the cost) and stay up past 6:00 PM (the benefit)?
# This map answers the same two questions for every state.
#
#   Row 1  THE COST    : days/year the sun rises after 7:30 AM
#   Row 2  THE BENEFIT : days/year the sun is still up at 6:00 PM
#   Cols   Today (current law)  |  Permanent daylight saving time
#
# In BOTH rows red = worse (more dark mornings / fewer bright evenings).
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
# 1. Representative point + predominant standard UTC offset (same as us_map.R)
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
# 2. Full-year day counts on the wall clock under both scenarios
# ------------------------------------------------------------------------------
days <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
grid <- tidyr::crossing(states, date = days)

st <- getSunlightTimes(data = data.frame(date = grid$date, lat = grid$lat, lon = grid$lon),
                       keep = c("sunrise", "sunset"), tz = "UTC")
grid$sunrise <- st$sunrise
grid$sunset  <- st$sunset

localdec <- function(posix, offh) {
  p <- posix + offh * 3600
  as.numeric(format(p, "%H", tz = "UTC")) + as.numeric(format(p, "%M", tz = "UTC")) / 60
}

# Current law: spring-forward Mar 9, fall-back Nov 2 (2025); exempt states never shift.
dst_on <- grid$date >= as.Date("2025-03-09") & grid$date < as.Date("2025-11-02")

metric <- grid %>%
  mutate(off_cur  = off + ifelse(!exempt & dst_on, 1, 0),
         off_perm = off + ifelse(exempt, 0, 1),
         sr_cur  = localdec(sunrise, off_cur),  ss_cur  = localdec(sunset, off_cur),
         sr_perm = localdec(sunrise, off_perm), ss_perm = localdec(sunset, off_perm)) %>%
  group_by(state, exempt) %>%
  summarise(dark_cur    = sum(sr_cur  >= 7.5),  dark_perm   = sum(sr_perm  >= 7.5),
            bright_cur  = sum(ss_cur  >= 18.0), bright_perm = sum(ss_perm >= 18.0),
            .groups = "drop")

# console + CSV
cat("\nMost DARK MORNINGS under permanent DST (days/yr sunrise after 7:30 AM):\n")
metric %>% arrange(desc(dark_perm)) %>%
  transmute(state, current = dark_cur, permanent_dst = dark_perm) %>% head(8) %>%
  as.data.frame() %>% print(row.names = FALSE)
cat("\nStates where the sun would be up past 6 PM EVERY day under permanent DST:",
    sum(metric$bright_perm == 365), "of 50\n")

write.csv(
  metric %>% transmute(state,
                       days_sunrise_after_730am_current       = dark_cur,
                       days_sunrise_after_730am_permanent_dst = dark_perm,
                       days_sunset_after_6pm_current          = bright_cur,
                       days_sunset_after_6pm_permanent_dst    = bright_perm),
  file.path(DATA_DIR, "us_states_daycounts.csv"), row.names = FALSE)

# --- binning (RED = worse in BOTH rows) ---
cost_breaks <- c(-Inf, 30, 90, 120, 150, 180, Inf)
cost_labs   <- c("under 30", "30–90", "90–120", "120–150", "150–180", "180 or more")
ben_breaks  <- c(-Inf, 240, 270, 300, 330, 365, Inf)
ben_labs    <- c("under 240", "240–270", "270–300", "300–330", "330–364", "all 365")
bin_cost <- function(n) cut(n, cost_breaks, cost_labs, right = FALSE)
bin_ben  <- function(n) cut(n, ben_breaks,  ben_labs,  right = FALSE)

ramp6 <- c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026")
pal_cost <- setNames(ramp6,      cost_labs)   # more dark mornings   = dark red
pal_ben  <- setNames(rev(ramp6), ben_labs)    # fewer bright evenings = dark red

# ------------------------------------------------------------------------------
# 3. Assemble the map (lower 48 + Alaska & Hawaii insets) — same as us_map.R
# ------------------------------------------------------------------------------
us <- map_data("state")
sc_lv <- c("Today  (current law)", "Permanent daylight saving time")

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

to_long <- function(cur_cat, perm_cat) {
  metric %>%
    transmute(state,
              `Today  (current law)`             = cur_cat,
              `Permanent daylight saving time`   = perm_cat) %>%
    pivot_longer(-state, names_to = "scenario", values_to = "cat") %>%
    mutate(scenario = factor(scenario, levels = sc_lv))
}
long_cost <- to_long(bin_cost(metric$dark_cur),  bin_cost(metric$dark_perm))
long_ben  <- to_long(bin_ben(metric$bright_cur), bin_ben(metric$bright_perm))

join_map <- function(long) {
  us_all %>% left_join(long, by = c("region" = "state"), relationship = "many-to-many") %>%
    filter(!is.na(scenario)) %>% mutate(scenario = droplevels(scenario))
}
map_cost <- join_map(long_cost)
map_ben  <- join_map(long_ben)

# Arizona exempt marker (only meaningful in the permanent-DST column)
az <- us %>% filter(region == "arizona")
az_hi  <- az %>% mutate(scenario = factor(sc_lv[2], levels = sc_lv))
az_lab <- data.frame(lon = -111.7, lat = 34.3, scenario = factor(sc_lv[2], levels = sc_lv))

inset_outline <- bind_rows(ak_poly, hi_poly)
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
                                           keywidth = 2.0, keyheight = 0.55)) +
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

p_cost <- make_row(map_cost, pal_cost,
                   "Days per year the sun rises after 7:30 AM  (redder = more dark mornings)",
                   "THE COST  ·  mornings the sun rises after 7:30 AM", TRUE)
p_ben  <- make_row(map_ben, pal_ben,
                   "Days per year the sun is still up at 6:00 PM  (redder = fewer bright evenings)",
                   "THE BENEFIT  ·  evenings the sun is still up at 6:00 PM", FALSE)

combined <- (p_cost / p_ben) +
  plot_annotation(
    title = "The Permanent-DST Trade-Off, State by State",
    subtitle = paste0("How many days a year does the sun rise after 7:30 AM (the cost) — and stay up past 6:00 PM (the benefit)? Left column = today's\n",
                      "clock; right column = permanent daylight saving time. Permanent DST deepens the morning cost most in the north and west of each\n",
                      "time zone, and pushes 17 states to year-round post-6 PM sunsets. In both rows red = worse."),
    caption = paste0("Arizona and Hawaii do not observe daylight saving time — they stay on standard time year-round, so a permanent-DST law would not change them. ",
                     "Alaska & Hawaii shown as\ninsets, not to scale (Alaska at Anchorage, Hawaii at Honolulu). Counts computed daily over a full year (2025) at a representative point per state · predominant\n",
                     "time zone. Thresholds match the Jackson, MS trade-off chart (7:30 AM / 6:00 PM). Sun times: {suncalc} · Boundaries: {maps} · Analysis: Rodney Cuevas"),
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

outfile <- file.path(PLOT_DIR, "06_us_map_daycounts.png")
ggsave(outfile, combined, width = 14, height = 11.4, dpi = 200, bg = SURF)
cat("\nwrote", outfile, "\n")
