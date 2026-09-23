# ==============================================================================
# 0. SETUP: LIBRARIES & CONSTANTS
# ==============================================================================
library(shiny)
library(bslib)
library(bsicons)
library(thematic)
library(suncalc)
library(ggplot2)
library(dplyr)
library(lubridate)
library(maps)
library(tidyr)
library(ggrepel)
library(lutz)
library(tools)
library(leaflet)
library(DT)
library(glue)
library(shinycssloaders)

# Dependencies deliberately removed:
#   {furrr}/{future} - the state grid used to make ~29,000 scalar suncalc calls
#     and needed parallel workers. getSunlightTimes() vectorizes over lat/lon via
#     its data= argument, so the grid is now 2 calls -- see 1D.
#   {metR}, {purrr}  - declared but never called anywhere in this file.
#   {sf}             - only ever used to build a state polygon for a
#     point-in-state test, which maps::map.where() does directly and without
#     failing on the {maps} outlines' self-intersections (see 1D).
#   {emojifont}      - the moon calendar now draws the same Unicode glyphs its
#     own legend already described, instead of FontAwesome glyphs that needed a
#     registered font family to render.

# Enable consistent ggplot2 styling
thematic_on(bg = "auto", fg = "auto", accent = "auto", font = "sans")

# Global constants
PLOT_CAPTION <- "Source: suncalc R Package | Author: Rodney Cuevas"

# Plot ink colors. The app ships the dark "cyborg" bslib theme; keeping these in
# one place means restyling (or adding a light theme) is a single edit rather
# than hunting hardcoded "white"/"gray15" through every render function.
INK_ON_DARK    <- "white"    # text drawn directly on the plot panel
INK_MUTED      <- "gray70"   # captions / secondary annotation
LABEL_FILL     <- "gray15"   # ggrepel label background
TILE_FILL      <- "gray20"   # calendar tiles
CACHE_VERSION  <- "v5"       # bump whenever a data function's OUTPUT changes
FULL_MOON_NAMES <- c(
  "January" = "Wolf Moon",    "February" = "Snow Moon",    "March"    = "Worm Moon",
  "April"   = "Pink Moon",    "May"      = "Flower Moon",  "June"     = "Strawberry Moon",
  "July"    = "Buck Moon",    "August"   = "Sturgeon Moon","September"= "Harvest Moon",
  "October" = "Hunter's Moon","November" = "Beaver Moon",  "December" = "Cold Moon"
)

if (!dir.exists("cache")) dir.create("cache")

# ==============================================================================
# 1. MODULAR DATA FUNCTIONS
# ==============================================================================

# --- Equation of Time (NOAA analytical) ----------------------------------------
# Deliberately NOT derived from suncalc's solarNoon. suncalc approximates solar
# transit with a two-term series plus a 0.0009-day constant, which leaves its
# solar noon ~1.3 min late -- verified against these equations, mean difference
# -1.29 min. That is immaterial for sunrise/sunset, but the Equation of Time is a
# +/-16 min signal, so a 1.3 min bias visibly skews the curve (it compresses the
# November peak and deepens the February trough). These are the same NOAA
# equations used in the Permanent_DST_Analysis scripts.
equation_of_time_mins <- function(dates) {
  jd <- as.numeric(as.Date(dates)) + 2440588.0   # JD at 12:00 UT on each date
  T  <- (jd - 2451545.0) / 36525
  L0 <- (280.46646 + T * (36000.76983 + 0.0003032 * T)) %% 360
  M  <- 357.52911 + T * (35999.05029 - 0.0001537 * T)
  Mr <- M * pi / 180
  e  <- 0.016708634 - T * (0.000042037 + 0.0000001267 * T)
  eps0  <- 23 + (26 + (21.448 - T * (46.815 + T * (0.00059 - T * 0.001813))) / 60) / 60
  omega <- 125.04 - 1934.136 * T
  eps   <- eps0 + 0.00256 * cos(omega * pi / 180)
  y     <- tan(eps / 2 * pi / 180)^2
  L0r   <- L0 * pi / 180
  4 * (180 / pi) * (y * sin(2 * L0r) - 2 * e * sin(Mr) +
                    4 * e * y * sin(Mr) * cos(2 * L0r) -
                    0.5 * y * y * sin(4 * L0r) - 1.25 * e * e * sin(2 * Mr))
}

# --- Solar declination, and the true equinoxes/solstices ------------------------
# The value boxes used to pick "the day daylight is closest to 12 hours". That is
# the EQUILUX, not the equinox: because sunrise/sunset are defined with
# refraction and the sun's disc radius, 12h of daylight happens several days
# before the March equinox and after the September one. At Jackson it reported
# Mar 16 and Sep 27 against true dates of Mar 20 and Sep 22 -- four to five days
# out on boxes explicitly labelled "Equinox".
#
# The equinox is when solar declination crosses zero and the solstices are its
# extremes, so compute declination directly (same NOAA series as above).
solar_declination_deg <- function(times) {
  jd <- as.numeric(times) / 86400 + 2440587.5
  T  <- (jd - 2451545.0) / 36525
  L0 <- (280.46646 + T * (36000.76983 + 0.0003032 * T)) %% 360
  M  <- 357.52911 + T * (35999.05029 - 0.0001537 * T)
  Mr <- M * pi / 180
  C  <- sin(Mr)   * (1.914602 - T * (0.004817 + 0.000014 * T)) +
        sin(2*Mr) * (0.019993 - 0.000101 * T) +
        sin(3*Mr) * 0.000289
  omega    <- 125.04 - 1934.136 * T
  app_long <- L0 + C - 0.00569 - 0.00478 * sin(omega * pi / 180)
  eps0 <- 23 + (26 + (21.448 - T * (46.815 + T * (0.00059 - T * 0.001813))) / 60) / 60
  eps  <- eps0 + 0.00256 * cos(omega * pi / 180)
  asin(sin(eps * pi / 180) * sin(app_long * pi / 180)) * 180 / pi
}

get_season_dates <- function(analysis_year, tz) {
  hrs  <- seq(as.POSIXct(paste0(analysis_year, "-01-01 00:00:00"), tz = "UTC"),
              as.POSIXct(paste0(analysis_year, "-12-31 23:00:00"), tz = "UTC"),
              by = "hour")
  decl <- solar_declination_deg(hrs)
  d0   <- decl[-length(decl)]; d1 <- decl[-1]

  # NB: as.Date() on a POSIXct ignores its tzone and converts in UTC, so the
  # tz= argument is required to get the LOCAL calendar date.
  local_date <- function(instant) as.Date(with_tz(instant, tz), tz = tz)

  cross <- function(ascending) {
    idx <- if (ascending) which(d0 < 0 & d1 >= 0) else which(d0 > 0 & d1 <= 0)
    if (length(idx) == 0) return(as.Date(NA))
    i <- idx[1]
    local_date(hrs[i] + (0 - d0[i]) / (d1[i] - d0[i]) * 3600)
  }

  # Declination is flat either side of a solstice, so which.max() on an hourly
  # series can land hours from the true extremum. Fit a parabola to the three
  # samples around it (gets within ~10 min of the published instant).
  refine <- function(idx) {
    i <- max(2, min(length(decl) - 1, idx))
    ya <- decl[i - 1]; yb <- decl[i]; yc <- decl[i + 1]
    denom <- ya - 2 * yb + yc
    off <- if (abs(denom) < 1e-12) 0 else 0.5 * (ya - yc) / denom
    local_date(hrs[i] + off * 3600)
  }

  list(
    spring = cross(TRUE),
    fall   = cross(FALSE),
    summer = refine(which.max(decl)),
    winter = refine(which.min(decl))
  )
}

# --- Timezone lookup with an at-sea fallback -----------------------------------
# tz_lookup_coords() returns NA over open ocean, and an NA tz propagates straight
# into getSunlightTimes(tz = NA), which errors. Ocean clicks are easy to make on a
# world map, so fall back to the nearest whole-hour offset from longitude.
safe_tz <- function(lat, lon) {
  tz <- tryCatch(tz_lookup_coords(lat, lon, method = "accurate"),
                 error = function(e) NA_character_)
  if (length(tz) != 1 || is.na(tz) || !nzchar(tz)) {
    offset <- round(lon / 15)
    # Etc/GMT signs are inverted relative to UTC offsets (Etc/GMT+5 == UTC-5)
    tz <- if (offset == 0) "UTC" else sprintf("Etc/GMT%+d", -offset)
  }
  tz
}

# --- Exact lunar phase instants (Meeus, Astronomical Algorithms, ch. 49) --------
# The previous implementation sampled getMoonIllumination() once per day at
# midnight UTC and flagged the first day whose value had ALREADY crossed a phase
# threshold -- i.e. the day after the event. Every major phase came out a day
# late (verified: 12/12 full moons in 2025 were +1).
#
# The obvious repair -- sample hourly and interpolate the crossing -- does not
# actually work, because suncalc's `phase` is DISCONTINUOUS at 0.5: it is built
# as 0.5 +/- 0.5*inc/PI and the sign flips at full moon, so the series steps
# straight from ~0.486 to ~0.514 without passing through 0.5. suncalc's
# `fraction` peak is also ~3.5 h from the true instant, because its lunar
# position is a low-order approximation.
#
# So phase instants come from Meeus ch. 49 instead, which is independent of
# suncalc and self-contained. Verified against published 2025 times:
# max error 0.5 min, mean 0.3 min across all 24 new and full moons.
.deg2rad <- function(d) d * pi / 180

moon_phase_jde <- function(k) {
  Tt  <- k / 1236.85
  JDE <- 2451550.09766 + 29.530588861 * k +
         0.00015437 * Tt^2 - 0.000000150 * Tt^3 + 0.00000000073 * Tt^4
  E   <- 1 - 0.002516 * Tt - 0.0000074 * Tt^2
  M   <- .deg2rad(2.5534   +  29.10535670 * k - 0.0000014 * Tt^2 - 0.00000011 * Tt^3)
  Mp  <- .deg2rad(201.5643 + 385.81693528 * k + 0.0107582 * Tt^2 +
                  0.00001238 * Tt^3 - 0.000000058 * Tt^4)
  F   <- .deg2rad(160.7108 + 390.67050284 * k - 0.0016118 * Tt^2 -
                  0.00000227 * Tt^3 + 0.000000011 * Tt^4)
  Om  <- .deg2rad(124.7746 -   1.56375588 * k + 0.0020672 * Tt^2 + 0.00000215 * Tt^3)

  quarter <- round((k %% 1) * 4) / 4
  cor     <- numeric(length(k))

  # New and Full share a term list differing only in the leading coefficients
  nf <- function(c1, c2, c3, c4, c5, c6, c7) {
    c1*sin(Mp) + c2*E*sin(M) + c3*sin(2*Mp) + c4*sin(2*F) +
    c5*E*sin(Mp-M) + c6*E*sin(Mp+M) + c7*E^2*sin(2*M) -
    0.00111*sin(Mp-2*F) - 0.00057*sin(Mp+2*F) + 0.00056*E*sin(2*Mp+M) -
    0.00042*sin(3*Mp) + 0.00042*E*sin(M+2*F) + 0.00038*E*sin(M-2*F) -
    0.00024*E*sin(2*Mp-M) - 0.00017*sin(Om) - 0.00007*sin(Mp+2*M) +
    0.00004*sin(2*Mp-2*F) + 0.00004*sin(3*M) + 0.00003*sin(Mp+M-2*F) +
    0.00003*sin(2*Mp+2*F) - 0.00003*sin(Mp+M+2*F) + 0.00003*sin(Mp-M+2*F) -
    0.00002*sin(Mp-M-2*F) - 0.00002*sin(3*Mp+M) + 0.00002*sin(4*Mp)
  }
  isNew  <- quarter == 0
  isFull <- quarter == 0.5
  isQ    <- quarter %in% c(0.25, 0.75)
  if (any(isNew))
    cor[isNew]  <- nf(-0.40720, 0.17241, 0.01608, 0.01039,
                       0.00739, -0.00514, 0.00208)[isNew]
  if (any(isFull))
    cor[isFull] <- nf(-0.40614, 0.17302, 0.01614, 0.01043,
                       0.00734, -0.00515, 0.00209)[isFull]
  if (any(isQ)) {
    q <- -0.62801*sin(Mp) + 0.17172*E*sin(M) - 0.01183*E*sin(Mp+M) +
          0.00862*sin(2*Mp) + 0.00804*sin(2*F) + 0.00454*E*sin(Mp-M) +
          0.00204*E^2*sin(2*M) - 0.00180*sin(Mp-2*F) - 0.00070*sin(Mp+2*F) -
          0.00040*sin(3*Mp) - 0.00034*E*sin(2*Mp-M) + 0.00032*E*sin(M+2*F) +
          0.00032*E*sin(M-2*F) - 0.00028*E^2*sin(Mp+2*M) + 0.00027*E*sin(2*Mp+M) -
          0.00017*sin(Om) - 0.00005*sin(Mp-M-2*F) + 0.00004*sin(2*Mp+2*F) -
          0.00004*sin(Mp+M+2*F) + 0.00004*sin(Mp-2*M) + 0.00003*sin(Mp+M-2*F) +
          0.00003*sin(3*M) + 0.00002*sin(2*Mp-2*F) + 0.00002*sin(Mp-M+2*F) -
          0.00002*sin(3*Mp+M)
    W <- 0.00306 - 0.00038*E*cos(M) + 0.00026*cos(Mp) - 0.00002*cos(Mp-M) +
         0.00002*cos(Mp+M) + 0.00002*cos(2*F)
    cor[isQ] <- (q + ifelse(quarter == 0.25, W, -W))[isQ]
  }

  # 14 additional planetary-argument corrections
  A <- cbind(
    .deg2rad(299.77 +  0.107408*k - 0.009173*Tt^2), .deg2rad(251.88 +  0.016321*k),
    .deg2rad(251.83 + 26.651886*k), .deg2rad(349.42 + 36.412478*k),
    .deg2rad( 84.66 + 18.206239*k), .deg2rad(141.74 + 53.303771*k),
    .deg2rad(207.14 +  2.453732*k), .deg2rad(154.84 +  7.306860*k),
    .deg2rad( 34.52 + 27.261239*k), .deg2rad(207.19 +  0.121824*k),
    .deg2rad(291.34 +  1.844379*k), .deg2rad(161.72 + 24.198154*k),
    .deg2rad(239.56 + 25.513099*k), .deg2rad(331.55 +  3.592518*k))
  add <- as.vector(sin(A) %*% c(0.000325,0.000165,0.000164,0.000126,0.000110,
                                0.000062,0.000060,0.000056,0.000047,0.000042,
                                0.000040,0.000037,0.000035,0.000023))

  JDE + cor + add
}

get_major_phases <- function(from_date, to_date, tz) {
  from_date <- as.Date(from_date); to_date <- as.Date(to_date)
  yr_from   <- as.numeric(format(from_date, "%Y")) +
               (as.numeric(format(from_date, "%j")) - 1) / 365.25
  yr_to     <- as.numeric(format(to_date, "%Y")) +
               (as.numeric(format(to_date, "%j")) - 1) / 365.25
  k_seq     <- seq(floor((yr_from - 2000) * 12.3685) - 2,
                   ceiling((yr_to - 2000) * 12.3685) + 2)

  types <- c("New Moon" = 0, "First Quarter" = 0.25,
             "Full Moon" = 0.5, "Third Quarter" = 0.75)

  res <- bind_rows(lapply(names(types), function(nm) {
    jde <- moon_phase_jde(k_seq + types[[nm]])
    data.frame(
      # JDE is Dynamical Time; deltaT is ~69 s this era (sub-minute effect)
      instant    = as.POSIXct((jde - 2440587.5) * 86400 - 69,
                              origin = "1970-01-01", tz = "UTC"),
      phase_type = nm,
      stringsAsFactors = FALSE
    )
  }))

  res %>%
    mutate(
      phase_time = with_tz(instant, tz),
      # tz= is required: as.Date() on a POSIXct converts in UTC regardless of
      # the value's own timezone, which would date any phase falling in the
      # local evening (00:00-06:00 UTC for US Central) a day late.
      date       = as.Date(phase_time, tz = tz)
    ) %>%
    filter(date >= from_date, date <= to_date) %>%
    arrange(phase_time) %>%
    select(date, phase_time, phase_type)
}

# --- 1A. Lunar Events (full restoration: Super/Micro New, Blue Moon, Black Moon) ---
# major_phases     = phases inside the analysis year (drives Blue Moon)
# major_phases_ext = phases spanning Dec(year-1) .. Mar(year+1). A season runs
#   solstice-to-equinox and therefore straddles the New Year, so the seasonal
#   Black Moon test needs phases from outside the analysis year to count a
#   four-new-moon winter correctly.
get_lunar_events <- function(daily_moon_data, major_phases, year, lat, lon,
                             major_phases_ext = major_phases) {
  moon_position        <- getMoonPosition(date = daily_moon_data$date, lat = lat, lon = lon)
  daily_moon_data$distance_km <- moon_position$distance

  dist_super <- quantile(daily_moon_data$distance_km, 0.10)
  dist_micro <- quantile(daily_moon_data$distance_km, 0.90)

  full_moons_dates <- major_phases$date[major_phases$phase_type == "Full Moon"]
  new_moons_dates  <- major_phases$date[major_phases$phase_type == "New Moon"]

  super_full <- daily_moon_data %>%
    filter(date %in% full_moons_dates, distance_km <= dist_super) %>%
    mutate(event = "Super Full Moon", details = paste("Moon at approx.", round(distance_km), "km"))

  micro_full <- daily_moon_data %>%
    filter(date %in% full_moons_dates, distance_km >= dist_micro) %>%
    mutate(event = "Micro Full Moon", details = paste("Moon at approx.", round(distance_km), "km"))

  super_new <- daily_moon_data %>%
    filter(date %in% new_moons_dates, distance_km <= dist_super) %>%
    mutate(event = "Super New Moon", details = "Closest new moon of the year")

  micro_new <- daily_moon_data %>%
    filter(date %in% new_moons_dates, distance_km >= dist_micro) %>%
    mutate(event = "Micro New Moon", details = "Farthest new moon of the year")

  blue_moons <- major_phases %>%
    filter(phase_type == "Full Moon") %>%
    mutate(month = month(date)) %>%
    group_by(month) %>%
    filter(n() > 1) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(event = "Blue Moon", details = "Second full moon in one month")

  # Black Moon: third new moon in an astronomical season that contains four.
  # Seasons run solstice->equinox and cross the New Year, so we build boundaries
  # for the previous, current and next year and assign each new moon to the
  # season it actually falls in -- the old version cut the year at Jan 1/Dec 31,
  # which split winter in two and made a four-new-moon winter undetectable.
  # Boundary dates are the conventional ones (they drift <=1 day across
  # 1950-2050, which cannot change a new-moon count).
  season_bounds <- do.call(rbind, lapply((year - 1):(year + 1), function(y) {
    data.frame(
      start  = as.Date(paste0(y, c("-03-20", "-06-21", "-09-22", "-12-21"))),
      season = paste(c("Spring", "Summer", "Autumn", "Winter"), y),
      stringsAsFactors = FALSE
    )
  })) %>% arrange(start)
  season_bounds$end <- c(season_bounds$start[-1] - 1,
                         max(season_bounds$start) + 120)

  new_moons_ext <- major_phases_ext %>% filter(phase_type == "New Moon")

  black_moons <- if (nrow(new_moons_ext) == 0) {
    new_moons_ext[0, ] %>% mutate(event = character(0), details = character(0))
  } else {
    new_moons_ext %>%
      mutate(season = season_bounds$season[
        findInterval(as.Date(date), season_bounds$start)
      ]) %>%
      filter(!is.na(season)) %>%
      group_by(season) %>%
      filter(n() >= 4) %>%
      slice(3) %>%
      ungroup() %>%
      # only surface the ones that land inside the analysis year
      filter(year(date) == year) %>%
      mutate(event = "Black Moon", details = "Third new moon in a season with four")
  }

  # One row per date, keeping every event on it. This used to be
  # distinct(date, .keep_all = TRUE), which kept only the first event of the
  # day -- so a Blue Moon that was also a super or micro moon (the Aug 2023
  # "Super Blue Moon", 31 May 2026) was silently dropped from the table.
  bind_rows(super_full, micro_full, super_new, micro_new, blue_moons, black_moons) %>%
    select(date, event, details) %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(event   = paste(unique(event),   collapse = " + "),
              details = paste(unique(details), collapse = "; "),
              .groups = "drop")
}

# --- 1B. Moon Data (restored: phase_name, phase_alpha, full_moon_data, yearly_summary_data) ---
get_combined_moon_data <- function(lat, lon, analysis_year) {
  cache_file <- file.path("cache", sprintf("moon_cache_%s_%s_%s_%s.rds",
                                           CACHE_VERSION,
                                           round(lat, 2), round(lon, 2), analysis_year))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  tz        <- safe_tz(lat, lon)
  all_dates <- seq(ymd(paste0(analysis_year, "-01-01")),
                   ymd(paste0(analysis_year, "-12-31")), by = "day")

  moon_illum <- getMoonIllumination(date = all_dates)
  moon_times <- getMoonTimes(date = all_dates, lat = lat, lon = lon, tz = tz)
  moon_pos   <- getMoonPosition(date = all_dates, lat = lat, lon = lon)

  daily_data <- data.frame(
    date         = all_dates,
    illumination = moon_illum$fraction,
    phase_angle  = moon_illum$phase,
    moonrise     = moon_times$rise,
    moonset      = moon_times$set,
    distance_km  = moon_pos$distance,
    altitude_deg = moon_pos$altitude * 180 / pi,
    azimuth_deg  = (moon_pos$azimuth * 180 / pi + 180) %% 360   # convert to compass bearing
  ) %>%
    mutate(
      duration_hours = as.numeric(difftime(moonset, moonrise, units = "hours")),
      duration_hours = ifelse(duration_hours < 0, duration_hours + 24, duration_hours)
    )

  # Exact phase instants. The window runs Dec of the prior year through Mar of
  # the next so that (a) a phase falling on Jan 1 is not lost to a lag() NA, and
  # (b) the seasonal Black Moon test can see a whole solstice-to-equinox winter.
  major_phases_ext <- get_major_phases(
    paste0(analysis_year - 1, "-12-01"),
    paste0(analysis_year + 1, "-03-31"),
    tz
  )
  major_phases <- major_phases_ext %>% filter(year(date) == analysis_year)

  calendar_data <- daily_data %>%
    mutate(
      month        = month(date, label = TRUE, abbr = FALSE),
      weekday      = wday(date, label = TRUE, week_start = 1),
      day_of_month = day(date)
    ) %>%
    group_by(month) %>%
    mutate(week_of_month = ceiling((day_of_month + (wday(first(date), week_start = 1) - 1)) / 7)) %>%
    ungroup() %>%
    mutate(
      phase_name = case_when(
        phase_angle < 0.03 | phase_angle > 0.97          ~ "New Moon",
        phase_angle >= 0.03 & phase_angle < 0.22          ~ "Crescent",
        phase_angle >= 0.22 & phase_angle < 0.28          ~ "Quarter",
        phase_angle >= 0.28 & phase_angle < 0.47          ~ "Gibbous",
        phase_angle >= 0.47 & phase_angle < 0.53          ~ "Full Moon",
        phase_angle >= 0.53 & phase_angle < 0.72          ~ "Gibbous",
        phase_angle >= 0.72 & phase_angle < 0.78          ~ "Quarter",
        phase_angle >= 0.78 & phase_angle < 0.97          ~ "Crescent",
        TRUE ~ ""
      ),
      # Unicode geometric shapes rather than FontAwesome glyphs: no {emojifont}
      # dependency, no font family to register, and these are exactly the symbols
      # the calendar's own legend describes (they used to disagree -- the legend
      # named five shapes while the tiles drew three FontAwesome icons).
      phase_emoji = case_when(
        phase_name == "New Moon"  ~ "○",   # white circle
        phase_name == "Crescent"  ~ "☽",   # crescent
        phase_name == "Quarter"   ~ "◐",   # half-filled circle
        phase_name == "Gibbous"   ~ "◑",   # mostly-filled circle
        phase_name == "Full Moon" ~ "●",   # black circle
        TRUE ~ ""
      ),
      phase_alpha = ifelse(phase_name == "Gibbous", 0.5, 1.0)
    )

  lunar_events <- get_lunar_events(daily_data, major_phases, analysis_year, lat, lon,
                                   major_phases_ext = major_phases_ext)

  phase_to_abbr <- c("New Moon" = "NM", "First Quarter" = "FQ",
                     "Full Moon" = "FM", "Third Quarter" = "TQ")

  # One row per full moon (a month can hold two). The second in a calendar month
  # is the Blue Moon and gets named as such, so the header can say something true
  # instead of repeating the traditional name twice.
  full_moon_data <- major_phases %>%
    filter(phase_type == "Full Moon") %>%
    arrange(date) %>%
    mutate(month_name = month.name[month(date)]) %>%
    group_by(month_name) %>%
    mutate(
      is_blue   = row_number() > 1,
      moon_name = ifelse(is_blue, "Blue Moon", unname(FULL_MOON_NAMES[month_name]))
    ) %>%
    ungroup() %>%
    select(month_name, moon_name, date, phase_time, is_blue)

  yearly_summary_data <- major_phases %>%
    mutate(
      month_name = month.name[month(date)],
      day_num    = day(date),
      phase_abbr = phase_to_abbr[phase_type],
      at_time    = format(phase_time, "%l:%M %p") %>% trimws()
    ) %>%
    arrange(date) %>%
    group_by(month_name) %>%
    summarise(Events = paste(glue("{day_num} ({phase_abbr} {at_time})"), collapse = ", "),
              .groups = "drop") %>%
    mutate(month_name = factor(month_name, levels = month.name)) %>%
    arrange(month_name)

  res <- list(
    daily_data          = daily_data,
    major_phases        = major_phases,
    calendar_data       = calendar_data,
    lunar_events        = lunar_events,
    full_moon_data      = full_moon_data,
    yearly_summary_data = yearly_summary_data,
    tz                  = tz
  )
  saveRDS(res, cache_file)
  return(res)
}

# --- 1C. Sun Point Data (updated: full twilight fields, golden/blue hour, DST detection) ---
get_combined_sun_data <- function(lat, lon, analysis_year) {
  cache_file <- file.path("cache", sprintf("sun_point_cache_%s_%s_%s_%s.rds",
                                           CACHE_VERSION,
                                           round(lat, 2), round(lon, 2), analysis_year))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  tz        <- safe_tz(lat, lon)
  all_dates <- seq(ymd(paste0(analysis_year, "-01-01")),
                   ymd(paste0(analysis_year, "-12-31")), by = "day")

  sun_times <- getSunlightTimes(
    date = all_dates, lat = lat, lon = lon, tz = tz,
    keep = c("sunrise", "sunset", "solarNoon",
             "dawn", "dusk",
             "nauticalDawn", "nauticalDusk",
             "nightEnd", "night",
             "goldenHourEnd", "goldenHour")
  )

  # Polar Day/Night handling: if sunrise is NA, determine if it's 0 or 24 hours
  # We use the sun position at solar noon to check if it's above or below the horizon.
  daily_data <- sun_times %>%
    mutate(
      daylight_hours = as.numeric(difftime(sunset, sunrise, units = "hours"))
    )

  if (any(is.na(daily_data$daylight_hours))) {
    # NB: the function is getSunlightPosition(). This line used to call
    # getSunPosition(), which does not exist in current suncalc -- so the polar
    # day/night branch crashed the moment it was reached (i.e. for any location
    # above the Arctic Circle, where sunrise/sunset are NA).
    pos_noon <- getSunlightPosition(date = sun_times$solarNoon, lat = lat, lon = lon)
    daily_data$daylight_hours <- ifelse(
      is.na(daily_data$daylight_hours),
      ifelse(pos_noon$altitude > 0, 24, 0),
      daily_data$daylight_hours
    )
  }

  # Two distinct quantities the old code conflated into one chart:
  #   equation_of_time  - apparent minus mean solar time. Astronomy. +/-16 min.
  #   clock_noon_offset - where solar noon lands on the WALL CLOCK, which also
  #                       carries the timezone's longitude offset and the 1-hour
  #                       DST step. Useful, but not the Equation of Time.
  daily_data <- daily_data %>%
    mutate(
      equation_of_time  = equation_of_time_mins(date),
      clock_noon_offset = as.numeric(difftime(
        with_tz(solarNoon, tz),
        floor_date(with_tz(solarNoon, tz), "day") + hours(12),
        units = "mins"
      ))
    )

  daily_data <- daily_data %>%
    mutate(
      solar_noon_time   = as.POSIXct(format(solarNoon, "%H:%M:%S"), format = "%H:%M:%S"),
      # Fixed: sunrise - dawn (positive), dusk - sunset (positive)
      twilight_civil    = as.numeric(difftime(sunrise, dawn,         units = "mins")) +
                          as.numeric(difftime(dusk,    sunset,       units = "mins")),
      twilight_nautical = as.numeric(difftime(dawn,    nauticalDawn, units = "mins")) +
                          as.numeric(difftime(nauticalDusk, dusk,    units = "mins")),
      golden_hour_mins  = as.numeric(difftime(goldenHourEnd, sunrise, units = "mins")) +
                          as.numeric(difftime(sunset, goldenHour,     units = "mins"))
    )

  # DST detection
  tz_abbr   <- format(with_tz(as.POSIXct(all_dates), tz), "%Z")
  tz_idx    <- which(tz_abbr != dplyr::lag(tz_abbr, default = tz_abbr[1]))
  dst_dates <- if (length(tz_idx) >= 1) all_dates[tz_idx] else NULL

  res <- list(daily_data = daily_data, tz = tz, dst_dates = dst_dates,
              seasons = get_season_dates(analysis_year, tz))
  saveRDS(res, cache_file)
  return(res)
}

# --- 1D. State Grid (restored: N/C/S, daily curve data, facet labels, contour-ready) ---
get_state_sun_grid <- function(state_name, analysis_year, resolution = 30) {
  state_name <- tolower(state_name)
  cache_file <- file.path("cache", sprintf("sun_grid_cache_%s_%s_%s_%s.rds",
                                           CACHE_VERSION,
                                           state_name, analysis_year, resolution))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  # Alaska and Hawaii are in the 'world' database (USA region), not the 'state' database
  if (state_name %in% c("alaska", "hawaii")) {
    raw_map <- map_data("world", region = "USA") %>%
      filter(tolower(subregion) == state_name)
  } else {
    raw_map <- map_data("state", region = state_name)
  }

  if (nrow(raw_map) == 0) stop("State geometry not found for: ", state_name)

  # Alaska's western Aleutians cross the antimeridian, so its raw longitudes span
  # -178 to +180 and st_bbox() reports a box 358 degrees wide. Gridding that box
  # spreads `resolution` columns across the whole planet and only a handful land
  # in Alaska. Drop the far-western tail so the bbox describes the mainland.
  if (state_name == "alaska") {
    keep_groups <- raw_map %>%
      group_by(group) %>%
      summarise(min_long = min(long), .groups = "drop") %>%
      filter(min_long > -180, min_long < 0) %>%
      pull(group)
    raw_map <- raw_map %>% filter(group %in% keep_groups, long < 0)
  }

  # Grid the bounding box, then keep the points that fall inside the state.
  #
  # This used to build an sf POLYGON with st_combine/st_cast/st_union and filter
  # with st_filter. The {maps} outlines contain small self-intersections, which
  # s2 rejects outright -- 10 of the 48 lower states (California, Colorado,
  # Kentucky, New Mexico, New York, North Carolina, South Dakota, Tennessee,
  # Texas and Virginia) threw "Loop 0 is not valid: Edge N crosses edge M" and
  # the whole Regional Trends tab failed for them. st_make_valid() rescued only
  # some. maps::map.where() answers the same point-in-state question directly
  # off the same outlines, cannot fail on topology, and drops the sf dependency.
  bbox_lon <- range(raw_map$long)
  bbox_lat <- range(raw_map$lat)
  grid_points <- expand.grid(
    lon = seq(bbox_lon[1], bbox_lon[2], length.out = resolution),
    lat = seq(bbox_lat[1], bbox_lat[2], length.out = resolution)
  )

  map_db <- if (state_name %in% c("alaska", "hawaii")) "world" else "state"
  where  <- map.where(database = map_db, x = grid_points$lon, y = grid_points$lat)
  inside <- if (map_db == "state") {
    !is.na(where) & sub(":.*", "", where) == state_name
  } else {
    !is.na(where) & grepl(state_name, where, ignore.case = TRUE)
  }
  state_grid_df <- grid_points[inside, , drop = FALSE]
  rownames(state_grid_df) <- NULL

  if (nrow(state_grid_df) < 3)
    stop("Not enough grid points inside ", state_name,
         " at this resolution - try a higher detail setting.")

  # Points of Interest: South / Center / North.
  # Previously this used ntile(lat, 100) and kept ranks 1/50/100, which silently
  # produced FEWER than three points for any state with under 100 grid points --
  # Hawaii at 30pt resolution yielded only two, so the "N/C/S" chart lost a line
  # and the facet labels printed NA. Select by latitude quantile instead, which
  # always returns exactly three.
  pick_poi <- function(q, label) {
    target <- as.numeric(quantile(state_grid_df$lat, q, names = FALSE))
    tol    <- max(diff(range(state_grid_df$lat)) / 50, 1e-9)
    band   <- state_grid_df[abs(state_grid_df$lat - target) <= tol, , drop = FALSE]
    if (nrow(band) == 0)
      band <- state_grid_df[which.min(abs(state_grid_df$lat - target)), , drop = FALSE]
    data.frame(location = label, lat = mean(band$lat), lon = mean(band$lon),
               stringsAsFactors = FALSE)
  }
  poi <- bind_rows(pick_poi(0.02, "South"),
                   pick_poi(0.50, "Center"),
                   pick_poi(0.98, "North"))

  center_poi <- poi %>% filter(location == "Center")
  tz         <- safe_tz(center_poi$lat, center_poi$lon)

  # Monthly daylight change across the grid.
  #
  # This used to call getSunlightTimes() twice per (grid point x month) through
  # future_pmap_dbl -- roughly 29,000 scalar calls at 50pt resolution, which is
  # why it needed parallel workers and a progress bar. getSunlightTimes()
  # vectorizes over lat/lon as well as date via its data= argument, so the whole
  # grid is now exactly two calls and furrr is no longer needed.
  grid_month <- tidyr::crossing(month = 1:12, state_grid_df) %>%
    mutate(
      start_date = ymd(paste(analysis_year, month, "01", sep = "-")),
      end_date   = ceiling_date(start_date, "month") - days(1)
    )

  # Daylight length, in minutes, for parallel vectors of date/lat/lon.
  #
  # Asking suncalc for UTC times means that west of about UTC-7 the sunset it
  # returns for a given UTC day falls on the NEXT UTC day, so the raw difference
  # comes out negative -- Hawaii reported -13.0 h and Alaska -17.6 h of daylight,
  # which then poisoned the facet labels and the N/C/S curves. The events are a
  # sunrise/sunset pair either way, so wrapping the negative case by +24 h
  # recovers the true duration (checked against a tz-free hour-angle
  # calculation: within 3 minutes for Jackson, Honolulu, Anchorage and Seattle).
  day_len_mins <- function(dates, lat, lon) {
    st  <- getSunlightTimes(data = data.frame(date = dates, lat = lat, lon = lon),
                            keep = c("sunrise", "sunset"), tz = "UTC")
    len <- as.numeric(difftime(st$sunset, st$sunrise, units = "mins"))
    ifelse(!is.na(len) & len < 0, len + 24 * 60, len)
  }

  start_len <- day_len_mins(grid_month$start_date, grid_month$lat, grid_month$lon)
  end_len   <- day_len_mins(grid_month$end_date,   grid_month$lat, grid_month$lon)

  # Above the Arctic Circle sunrise/sunset can be NA (polar day or night). Treat
  # those as 24 h or 0 h so Alaska's northern grid renders instead of going blank.
  fill_polar <- function(len, dates, lat, lon) {
    na_idx <- which(is.na(len))
    if (length(na_idx) == 0) return(len)
    noon <- as.POSIXct(paste(dates[na_idx], "12:00:00"), tz = "UTC") -
            (lon[na_idx] / 15) * 3600
    alt  <- getSunlightPosition(data = data.frame(date = noon,
                                                  lat  = lat[na_idx],
                                                  lon  = lon[na_idx]))$altitude
    len[na_idx] <- ifelse(alt > 0, 24 * 60, 0)
    len
  }
  start_len <- fill_polar(start_len, grid_month$start_date, grid_month$lat, grid_month$lon)
  end_len   <- fill_polar(end_len,   grid_month$end_date,   grid_month$lat, grid_month$lon)

  changes <- end_len - start_len

  # Daily data for N/C/S (vectorized: 3 calls x 365 dates)
  all_dates <- seq(ymd(paste0(analysis_year, "-01-01")),
                   ymd(paste0(analysis_year, "-12-31")), by = "day")
  daily_list <- lapply(seq_len(nrow(poi)), function(i) {
    p  <- poi[i, ]
    st <- getSunlightTimes(date = all_dates, lat = p$lat, lon = p$lon, tz = "UTC")
    dl <- day_len_mins(all_dates, rep(p$lat, length(all_dates)),
                       rep(p$lon, length(all_dates)))
    dl <- fill_polar(dl, all_dates, rep(p$lat, length(all_dates)),
                     rep(p$lon, length(all_dates))) / 60
    data.frame(
      date           = all_dates,
      location       = p$location,
      daylight_hours = dl,
      sunrise        = st$sunrise,
      sunset         = st$sunset,
      stringsAsFactors = FALSE
    )
  })
  daily_data <- bind_rows(daily_list)

  # Facet labels: N/C/S avg daily daylight by month
  monthly_avg <- daily_data %>%
    mutate(month = month(date)) %>%
    group_by(month, location) %>%
    summarise(avg_h = mean(daylight_hours, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = location, values_from = avg_h) %>%
    arrange(month)

  for (col in c("North", "Center", "South")) {
    if (!col %in% names(monthly_avg)) monthly_avg[[col]] <- NA_real_
  }

  monthly_avg <- monthly_avg %>%
    mutate(
      facet_label_chr = sprintf("%s\nN:%.1fh | C:%.1fh | S:%.1fh",
                                month.name[month], North, Center, South),
      facet_label     = factor(facet_label_chr, levels = facet_label_chr)
    )

  final_grid <- grid_month %>%
    mutate(
      daylight_change = changes,
      month_name      = factor(month.name[month], levels = month.name)
    ) %>%
    left_join(monthly_avg %>% select(month, facet_label), by = "month")

  # DST detection for center location
  tz_abbr   <- format(with_tz(as.POSIXct(all_dates), tz), "%Z")
  tz_idx    <- which(tz_abbr != dplyr::lag(tz_abbr, default = tz_abbr[1]))
  dst_dates <- if (length(tz_idx) >= 1) all_dates[tz_idx] else NULL

  res <- list(
    final_grid = final_grid,
    daily_data = daily_data,
    poi        = poi,
    tz         = tz,
    dst_dates  = dst_dates
  )
  saveRDS(res, cache_file)
  return(res)
}

# ==============================================================================
# 2. UI: THE USER INTERFACE
# ==============================================================================

ui <- page_navbar(
  theme   = bs_theme(bootswatch = "cyborg", primary = "#00bc8c", secondary = "#343a40"),
  title   = "Celestial Almanac Dashboard",
  id      = "main_nav",
  fillable = FALSE,

  sidebar = sidebar(
    title = "Location & Settings",
    leafletOutput("map", height = "280px"),
    verbatimTextOutput("coords_display"),
    numericInput("year",  "Analysis Year",    value = year(Sys.Date()), min = 1950, max = 2050),
    selectInput("state",  "Regional State",   choices = toTitleCase(state.name), selected = "Mississippi"),
    selectInput("month_cal", "Calendar Month",
                choices  = month.name,
                selected = month.name[month(Sys.Date())]),
    selectInput("grid_res", "State Grid Detail",
                choices  = c("Fast (15pt)" = "15", "Standard (30pt)" = "30", "Detailed (50pt)" = "50"),
                selected = "30"),
    actionButton("go", "Analyze Skies", icon = icon("rocket"), class = "btn-primary w-100"),
    hr(),
    p(HTML("<b>Author:</b> Rodney Cuevas")),
    p(HTML("<b>Source:</b> suncalc R Package"))
  ),

  # ---- OVERVIEW ----
  nav_panel("Overview",
    layout_column_wrap(
      width = 1/4,
      value_box(title = "Daylight Today",  value = textOutput("vb_daylight"),
                showcase = bsicons::bs_icon("sun-fill"),         theme = "warning"),
      value_box(title = "Moon Phase",      value = textOutput("vb_moon_phase"),
                showcase = bsicons::bs_icon("moon-stars-fill"),  theme = "info"),
      value_box(title = "Next Full Moon",  value = textOutput("vb_next_full"),
                showcase = bsicons::bs_icon("calendar-event"),   theme = "primary"),
      value_box(title = "Moon Distance",   value = textOutput("vb_moon_dist"),
                showcase = bsicons::bs_icon("arrows-expand"),    theme = "secondary")
    ),
    layout_column_wrap(
      width = 1/4,
      value_box(title = "Spring Equinox",  value = textOutput("vb_spring"),
                showcase = bsicons::bs_icon("flower1"),               theme = "success",   height = "110px"),
      value_box(title = "Summer Solstice", value = textOutput("vb_summer"),
                showcase = bsicons::bs_icon("brightness-high-fill"),  theme = "warning",   height = "110px"),
      value_box(title = "Fall Equinox",    value = textOutput("vb_fall"),
                showcase = bsicons::bs_icon("tree"),                  theme = "secondary", height = "110px"),
      value_box(title = "Winter Solstice", value = textOutput("vb_winter"),
                showcase = bsicons::bs_icon("snow"),                  theme = "info",      height = "110px")
    ),
    layout_columns(
      card(
        card_header("Today's Celestial Schedule"),
        card_body(
          downloadButton("dl_schedule", "Download CSV", class = "btn-sm btn-outline-secondary mb-2"),
          withSpinner(DTOutput("today_schedule"))
        )
      ),
      card(
        card_header("Analysis Location"),
        withSpinner(leafletOutput("mini_map", height = "400px"))
      )
    )
  ),

  # ---- SOLAR ANALYSIS ----
  nav_panel("Solar Analysis",
    tabsetPanel(
      nav_panel("Daylight Curves",
        card(full_screen = TRUE,
             card_body("Annual hours of daylight at the selected point. Dashed line = today."),
             withSpinner(plotOutput("p_annual_curve",   height = "78vh")))),
      nav_panel("Sunrise/Sunset",
        card(full_screen = TRUE,
             card_body("Daily sunrise & sunset times. Red dotted lines mark DST transitions."),
             withSpinner(plotOutput("p_sunrise_sunset", height = "78vh")))),
      nav_panel("Monthly Shift",
        card(full_screen = TRUE,
             card_body("Minutes of daylight gained/lost from the 1st to last day of each month."),
             withSpinner(plotOutput("p_monthly_avg",    height = "78vh")))),
      nav_panel("Solar Noon",
        card(full_screen = TRUE,
             card_body("How far ahead or behind the sun runs against a steady clock — the Equation of Time."),
             withSpinner(plotOutput("p_solar_noon",     height = "78vh")))),
      nav_panel("Sun Angles",
        card(full_screen = TRUE,
             card_body("Solar noon altitude angle throughout the year (higher = more intense sun)."),
             withSpinner(plotOutput("p_sun_angles",     height = "78vh")))),
      nav_panel("Seasonal Angles",
        card(full_screen = TRUE,
             card_body("Peak solar altitude at the four astronomical turning points."),
             withSpinner(plotOutput("p_seasonal_angle", height = "78vh")))),
      nav_panel("Symmetry",
        card(full_screen = TRUE,
             card_body("Spring and autumn days with equal daylight are connected — daylight is nearly symmetric around the solstice."),
             withSpinner(plotOutput("p_symmetry",       height = "78vh")))),
      nav_panel("Twilight",
        card(full_screen = TRUE,
             card_body("Daily duration of civil and nautical twilight (combined morning + evening)."),
             withSpinner(plotOutput("p_twilight",       height = "78vh")))),
      nav_panel("Extremes",
        card(full_screen = TRUE,
             card_body("Highlights the shortest and longest days of the year."),
             withSpinner(plotOutput("p_extremes",       height = "78vh"))))
    )
  ),

  # ---- LUNAR ANALYSIS ----
  nav_panel("Lunar Analysis",
    tabsetPanel(
      nav_panel("Calendar",
        card(full_screen = TRUE,
             card_header(textOutput("full_moon_name_display")),
             card_body("Use 'Calendar Month' in the sidebar to browse any month."),
             withSpinner(plotOutput("p_moon_calendar",  height = "78vh")))),
      nav_panel("Yearly Phases",
        card(full_screen = TRUE,
             card_body(
               downloadButton("dl_phases", "Download CSV", class = "btn-sm btn-outline-secondary mb-2"),
               withSpinner(DTOutput("yearly_summary_table"))))),
      nav_panel("Illumination",
        card(full_screen = TRUE,
             card_body("Daily illumination % with vertical lines marking each major phase."),
             withSpinner(plotOutput("p_moon_illum",     height = "78vh")))),
      nav_panel("Moon Distance",
        card(full_screen = TRUE,
             card_body("Earth-Moon distance throughout the year. Gold = full moon dates. Labeled = supermoons."),
             withSpinner(plotOutput("p_moon_distance",  height = "78vh")))),
      nav_panel("Lunar Events",
        card(full_screen = TRUE,
             card_body(
               downloadButton("dl_events", "Download CSV", class = "btn-sm btn-outline-secondary mb-2"),
               withSpinner(DTOutput("moon_events_table"))))),
      nav_panel("Moon Times",
        card(full_screen = TRUE,
             card_body("Moonrise and moonset times. Gaps = days when moon doesn't rise or set."),
             withSpinner(plotOutput("p_moon_times",     height = "78vh")))),
      nav_panel("Moon Visibility",
        card(full_screen = TRUE,
             card_body("Total hours the moon is above the horizon each day."),
             withSpinner(plotOutput("p_moon_duration",  height = "78vh"))))
    )
  ),

  # ---- SKY WATCHER ----
  nav_panel("Sky Watcher",
    layout_columns(
      card(
        card_header("Dark Sky Windows"),
        card_body("Nautical dusk-to-dawn blocks colored by moon illumination — brighter yellow = more moonlight interference."),
        withSpinner(plotOutput("p_dark_sky",     height = "78vh"))
      ),
      card(
        card_header("Photography: Golden & Blue Hours"),
        card_body("Daily windows for optimal photography light: blue hour (civil twilight) and golden hour (just after sunrise / before sunset)."),
        withSpinner(plotOutput("p_photography",  height = "78vh"))
      )
    )
  ),

  # ---- REGIONAL TRENDS ----
  nav_panel("Regional Trends",
    tabsetPanel(
      nav_panel("Monthly Daylight Shift",
        card(
          card_header("State-wide Daylight Shift with Contours"),
          card_body("Monthly change in daylight (first to last day). Facet labels show avg N/C/S hours."),
          withSpinner(plotOutput("p_state_grid",  height = "88vh"))
        )
      ),
      nav_panel("N / C / S Annual Curve",
        card(
          card_header("Annual Daylight Curve: North vs Center vs South"),
          card_body("Longer summer days in the north; smaller variation in the south."),
          withSpinner(plotOutput("p_ncs_curve",   height = "78vh"))
        )
      )
    )
  )
)

# ==============================================================================
# 3. SERVER
# ==============================================================================

server <- function(input, output, session) {

  # ---- 3A. Coordinates & Map ----
  coords <- reactiveValues(lat = 32.3547, lon = -89.3985)  # Default: Mississippi

  observeEvent(input$map_click, {
    coords$lat <- input$map_click$lat
    coords$lon <- input$map_click$lng
    leafletProxy("map") %>% clearMarkers() %>%
      addMarkers(lng = coords$lon, lat = coords$lat)
  })

  # isolate() so this builds ONCE. Reading coords reactively here meant every
  # map click invalidated the render and rebuilt the widget at zoom 4 -- so
  # zooming in to pick a precise point was exactly what threw the zoom away.
  # The observeEvent above moves the marker via leafletProxy instead.
  output$map <- renderLeaflet({
    isolate({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        setView(lng = coords$lon, lat = coords$lat, zoom = 4) %>%
        addMarkers(lng = coords$lon, lat = coords$lat)
    })
  })

  output$coords_display <- renderText({
    sprintf("Lat: %.4f\nLon: %.4f", coords$lat, coords$lon)
  })

  # Same treatment: built once, then panned by the observer below rather than
  # re-rendered (which previously happened alongside the proxy update).
  output$mini_map <- renderLeaflet({
    isolate({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        setView(lng = coords$lon, lat = coords$lat, zoom = 8) %>%
        addMarkers(lng = coords$lon, lat = coords$lat, label = "Analyzed Point")
    })
  })

  observe({
    coords$lat; coords$lon
    leafletProxy("mini_map") %>%
      clearMarkers() %>%
      setView(lng = coords$lon, lat = coords$lat, zoom = 8) %>%
      addMarkers(lng = coords$lon, lat = coords$lat, label = "Analyzed Point")
  })

  # ---- 3B. Analysis parameters, snapshotted at the button press ----
  #
  # The data reactives below only recompute when "Analyze Skies" is clicked, but
  # outputs used to read input$year / input$state / coords$lat directly, which
  # update the instant the control moves. The two halves drifted apart: changing
  # the state dropdown redrew Texas's border over Mississippi's grid and
  # relabelled the title to match, while the data underneath was unchanged.
  #
  # Everything downstream now reads this snapshot, so the whole dashboard always
  # describes one consistent set of parameters.
  params <- eventReactive(input$go, {
    req(coords$lat, coords$lon, input$year, input$state)
    list(
      lat   = coords$lat,
      lon   = coords$lon,
      year  = input$year,
      state = input$state,
      res   = as.integer(input$grid_res)
    )
  }, ignoreNULL = FALSE)

  ref_today <- reactive({
    yr    <- params()$year
    today <- Sys.Date()
    if (year(today) == yr) today else as.Date(paste0(yr, "-01-01"))
  })

  # NULL when analysis year != current year (hides "today" markers on plots)
  today_line <- reactive({
    if (year(Sys.Date()) == params()$year) Sys.Date() else NULL
  })

  # ---- 3C. Data Reactives ----
  # Each is wrapped so a failure surfaces as a readable message in the affected
  # card instead of a raw red stack trace across the whole dashboard.
  safely_compute <- function(label, expr) {
    tryCatch(expr, error = function(e) {
      validate(need(FALSE, paste0(label, " could not be calculated: ",
                                  conditionMessage(e))))
      NULL
    })
  }

  solar_data <- eventReactive(input$go, {
    p <- params()
    withProgress(message = "Calculating Solar Data...", value = 0.5,
      safely_compute("Solar data", get_combined_sun_data(p$lat, p$lon, p$year)))
  }, ignoreNULL = FALSE)

  lunar_data <- eventReactive(input$go, {
    p <- params()
    withProgress(message = "Calculating Lunar Data...", value = 0.5,
      safely_compute("Lunar data", get_combined_moon_data(p$lat, p$lon, p$year)))
  }, ignoreNULL = FALSE)

  state_data <- eventReactive(input$go, {
    p <- params()
    withProgress(message = glue("Simulating {p$state} Grid..."), value = 0.5,
      safely_compute(paste(p$state, "grid"),
                     get_state_sun_grid(p$state, p$year, resolution = p$res)))
  }, ignoreNULL = FALSE)

  # ---- 3D. Seasonal dates ----
  # True equinoxes/solstices from solar declination (see get_season_dates).
  # Previously derived from "closest to 12 hours of daylight", which finds the
  # equilux and ran 4-5 days off the equinox.
  seasonal_dates <- reactive({ solar_data()$seasons })

  # ---- 3E. Overview Outputs ----
  output$vb_daylight <- renderText({
    d <- solar_data()$daily_data %>% filter(as.Date(date) == ref_today())
    if (nrow(d) == 0) d <- solar_data()$daily_data %>% slice(1)
    sprintf("%.1f Hours", d$daylight_hours[1])
  })

  output$vb_moon_phase <- renderText({
    d <- lunar_data()$daily_data %>% filter(as.Date(date) == ref_today())
    if (nrow(d) == 0) d <- lunar_data()$daily_data %>% slice(1)
    sprintf("%.0f%% Illum.", d$illumination[1] * 100)
  })

  output$vb_next_full <- renderText({
    f <- lunar_data()$major_phases %>%
      filter(phase_type == "Full Moon", date >= ref_today()) %>% slice(1)
    if (nrow(f) == 0) return("None found")
    format(f$date[1], "%b %d")
  })

  output$vb_moon_dist <- renderText({
    d <- lunar_data()$daily_data %>% filter(as.Date(date) == ref_today())
    if (nrow(d) == 0) d <- lunar_data()$daily_data %>% slice(1)
    formatC(round(d$distance_km[1]), format = "d", big.mark = ",")
  })

  output$vb_spring <- renderText({ req(seasonal_dates()); format(seasonal_dates()$spring, "%b %d") })
  output$vb_summer <- renderText({ req(seasonal_dates()); format(seasonal_dates()$summer, "%b %d") })
  output$vb_fall   <- renderText({ req(seasonal_dates()); format(seasonal_dates()$fall,   "%b %d") })
  output$vb_winter <- renderText({ req(seasonal_dates()); format(seasonal_dates()$winter, "%b %d") })

  # Single source of truth for the day's schedule. The screen table and the CSV
  # used to be built separately and had drifted apart -- the download silently
  # omitted Golden Hour End, Nautical Dawn and Nautical Dusk.
  schedule_today <- reactive({
    s <- solar_data()$daily_data %>% filter(as.Date(date) == ref_today())
    m <- lunar_data()$daily_data  %>% filter(as.Date(date) == ref_today())
    if (nrow(s) == 0) s <- solar_data()$daily_data %>% slice(1)
    if (nrow(m) == 0) m <- lunar_data()$daily_data  %>% slice(1)

    # The moon skips a rise or a set roughly every 26 days, and inside the polar
    # circles the sun does too. Those came out as a literal "NA" in the table.
    fmt <- function(x, absent = "—") {
      out <- format(x, "%I:%M %p")
      ifelse(is.na(x) | is.na(out), absent, out)
    }
    data.frame(
      Event = c("Sunrise", "Sunset", "Solar Noon", "Golden Hour End",
                "Moonrise", "Moonset", "Civil Dawn", "Civil Dusk",
                "Nautical Dawn", "Nautical Dusk"),
      Time  = c(
        fmt(s$sunrise, "Does not rise"), fmt(s$sunset, "Does not set"),
        fmt(s$solarNoon),                fmt(s$goldenHourEnd),
        fmt(m$moonrise, "Does not rise"), fmt(m$moonset, "Does not set"),
        fmt(s$dawn),                     fmt(s$dusk),
        fmt(s$nauticalDawn),             fmt(s$nauticalDusk)
      ),
      stringsAsFactors = FALSE
    )
  })

  output$today_schedule <- renderDT({
    datatable(schedule_today(), options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })

  output$full_moon_name_display <- renderText({
    req(input$month_cal, lunar_data())
    fm <- lunar_data()$full_moon_data %>% filter(month_name == input$month_cal)
    if (nrow(fm) == 0) return(paste("Moon Phase Calendar —", input$month_cal))
    # A month with two full moons returned a length-2 vector here, and paste()
    # vectorised over it, so renderText printed the whole sentence twice. Name
    # the traditional moon, then mention the Blue Moon if there is one.
    traditional <- fm %>% filter(!is_blue) %>% slice(1)
    blue        <- fm %>% filter(is_blue)  %>% slice(1)
    msg <- if (nrow(traditional) > 0)
      paste("The full moon in", input$month_cal, "is known as the", traditional$moon_name)
    else
      paste("Moon Phase Calendar —", input$month_cal)
    if (nrow(blue) > 0)
      msg <- paste0(msg, " — and a Blue Moon follows on the ", day(blue$date), ".")
    msg
  })

  # ---- 3F. Download Handlers ----
  output$dl_schedule <- downloadHandler(
    filename = function() paste0("celestial_schedule_", ref_today(), ".csv"),
    content  = function(file) write.csv(schedule_today(), file, row.names = FALSE)
  )

  output$dl_phases <- downloadHandler(
    filename = function() paste0("lunar_phases_", params()$year, ".csv"),
    content  = function(file) write.csv(lunar_data()$yearly_summary_data, file, row.names = FALSE)
  )

  output$dl_events <- downloadHandler(
    filename = function() paste0("lunar_events_", params()$year, ".csv"),
    content  = function(file) write.csv(lunar_data()$lunar_events, file, row.names = FALSE)
  )

  # ---- 3G. Solar Analysis Plots ----

  output$p_annual_curve <- renderPlot({
    df <- solar_data()$daily_data
    p  <- ggplot(df, aes(x = date, y = daylight_hours)) +
      geom_area(fill = "orange", alpha = 0.2) +
      geom_line(color = "orange", linewidth = 1.2) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Annual Daylight Hours", y = "Hours", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.6)
    p
  })

  output$p_sunrise_sunset <- renderPlot({
    df  <- solar_data()$daily_data
    tz  <- solar_data()$tz
    dst <- solar_data()$dst_dates
    dummy_date <- as.Date("1970-01-01")

    df_long <- df %>%
      mutate(
        rise_time = ymd_hms(paste(dummy_date, format(sunrise, "%H:%M:%S"))),
        set_time  = ymd_hms(paste(dummy_date, format(sunset,  "%H:%M:%S")))
      ) %>%
      pivot_longer(cols = c(rise_time, set_time), names_to = "event", values_to = "time")

    p <- ggplot(df_long, aes(x = date, y = time, color = event)) +
      geom_line(linewidth = 1) +
      scale_y_datetime(date_labels = "%I:%M %p") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_color_manual(values = c("rise_time" = "gold", "set_time" = "navy"),
                         labels = c("rise_time" = "Sunrise", "set_time" = "Sunset")) +
      labs(title = "Sunrise and Sunset Times",
           subtitle = paste("Local time (", tz, ")  |  Red dotted = DST transitions"),
           y = "Time of Day", x = "Month", color = "Event", caption = PLOT_CAPTION) +
      theme_minimal()

    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)

    if (!is.null(dst) && length(dst) > 0) {
      dst_labels <- data.frame(
        date  = dst,
        label = if (length(dst) >= 2) c("Spring\nForward", "Fall\nBack") else rep("DST", length(dst))
      )
      p <- p +
        geom_vline(xintercept = dst, color = "red", linetype = "dotted", linewidth = 0.9) +
        geom_text(data = dst_labels, aes(x = date, label = label),
                  y = max(df_long$time, na.rm = TRUE), vjust = 1.2, size = 3,
                  color = "red", inherit.aes = FALSE)
    }
    p
  })

  output$p_monthly_avg <- renderPlot({
    # Not an average: it is the first-to-last-day delta for each month. The old
    # code took mean() of a value already constant within the group and titled
    # the chart "Average Monthly Change", which described neither.
    df <- solar_data()$daily_data %>%
      mutate(month = month(date, label = TRUE, abbr = FALSE)) %>%
      group_by(month) %>%
      summarise(avg_change = (last(daylight_hours) - first(daylight_hours)) * 60,
                .groups = "drop")

    ggplot(df, aes(x = month, y = avg_change, fill = avg_change)) +
      geom_col() +
      geom_text(aes(label = round(avg_change, 1),
                    vjust = ifelse(avg_change >= 0, -0.5, 1.5)), size = 3.5, fontface = "bold") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      labs(title = "Monthly Change in Daylight",
           subtitle = "Minutes gained/lost from first to last day of each month",
           y = "Minutes per Month", x = NULL, caption = PLOT_CAPTION) +
      theme_minimal() + theme(legend.position = "none")
  })

  output$p_solar_noon <- renderPlot({
    # This chart used to plot solar noon against LOCAL CLOCK noon and call the
    # result the Equation of Time. That measurement also contains the timezone's
    # longitude offset and the one-hour DST step -- for Jackson it ran +8 min in
    # January and +65 min in July, a ~56 min cliff at each transition that
    # flattened the actual +/-16 min signal. equation_of_time is now computed
    # properly upstream; clock_noon_offset keeps the old quantity for anyone who
    # wants "when does the sun peak on my clock".
    df_plot <- solar_data()$daily_data %>% mutate(dev_mins = equation_of_time)

    p <- ggplot(df_plot, aes(x = date, y = dev_mins)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "The Equation of Time",
           subtitle = paste0("How far ahead or behind the sun runs against clock time. ",
                             "Positive = sundial ahead of a steady clock."),
           y = "Apparent minus mean solar time (minutes)", x = "Month",
           caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  # Exact solar-noon altitude, shared by the two angle plots.
  # Both used to inline the same five lines of Cooper's declination
  # approximation (+/-0.5 deg). suncalc already knows the real answer.
  noon_altitude <- reactive({
    p  <- params()
    df <- solar_data()$daily_data
    df$altitude <- getSunlightPosition(
      data = data.frame(date = df$solarNoon, lat = p$lat, lon = p$lon)
    )$altitude * 180 / pi
    df
  })

  output$p_sun_angles <- renderPlot({
    df <- noon_altitude()
    p <- ggplot(df, aes(x = date, y = altitude)) +
      geom_line(color = "red", linewidth = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Solar Noon Altitude Angle",
           subtitle = paste("Latitude:", round(params()$lat, 2), "° — higher values = more intense midday sun"),
           y = "Altitude (°)", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  output$p_seasonal_angle <- renderPlot({
    df   <- noon_altitude()
    seas <- seasonal_dates()
    seasonal_pts <- data.frame(
      date  = c(seas$spring, seas$summer, seas$fall, seas$winter),
      label = c("Spring\nEquinox", "Summer\nSolstice", "Fall\nEquinox", "Winter\nSolstice")
    ) %>% left_join(df %>% select(date, altitude), by = "date")

    ggplot(df, aes(x = date, y = altitude)) +
      geom_line(color = "orange", linewidth = 1, alpha = 0.6) +
      geom_point(data = seasonal_pts, color = "red", size = 5) +
      geom_label_repel(data = seasonal_pts,
                       aes(label = paste0(label, "\n", round(altitude, 1), "°")),
                       fill = LABEL_FILL, color = INK_ON_DARK, size = 3.2, label.padding = unit(0.4, "lines")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Solar Altitude at Seasonal Turning Points",
           subtitle = paste("Latitude:", round(params()$lat, 2), "°"),
           y = "Solar Noon Altitude (°)", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
  })

  output$p_symmetry <- renderPlot({
    df       <- solar_data()$daily_data
    solstice <- df$date[which.max(df$daylight_hours)]
    growing  <- df %>% filter(date < solstice)
    shrinking <- df %>% filter(date > solstice)

    # Only the six first-of-month rows are ever drawn, so pair just those
    # instead of matching every growing-season day and discarding ~165 of them.
    anchors <- growing %>% filter(day(date) == 1, month(date) %in% 1:6)
    sample_matches <- bind_rows(lapply(seq_len(nrow(anchors)), function(i) {
      target <- anchors$daylight_hours[i]
      idx    <- which.min(abs(shrinking$daylight_hours - target))
      if (length(idx) == 0) return(NULL)
      data.frame(growing_date  = anchors$date[i],
                 matching_date = shrinking$date[idx],
                 daylight      = target)
    }))

    p <- ggplot(df, aes(x = date, y = daylight_hours)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      geom_vline(xintercept = solstice, color = "orange", linetype = "dotted", linewidth = 1) +
      geom_segment(data = sample_matches,
                   aes(x = growing_date, xend = matching_date, y = daylight, yend = daylight),
                   color = "steelblue", linetype = "dotted", linewidth = 0.8, inherit.aes = FALSE) +
      geom_point(data = sample_matches, aes(x = growing_date,  y = daylight),
                 color = "steelblue", size = 3, inherit.aes = FALSE) +
      geom_point(data = sample_matches, aes(x = matching_date, y = daylight),
                 color = "steelblue", size = 3, inherit.aes = FALSE) +
      geom_text(data = sample_matches,
                aes(x = growing_date,  y = daylight, label = format(growing_date, "%b %d")),
                nudge_y = 0.2, size = 3.2, hjust = 1.1, inherit.aes = FALSE) +
      geom_text(data = sample_matches,
                aes(x = matching_date, y = daylight, label = format(matching_date, "%b %d")),
                nudge_y = 0.2, size = 3.2, hjust = -0.1, inherit.aes = FALSE) +
      annotate("text", x = solstice, y = min(df$daylight_hours) + 0.5,
               label = "Summer\nSolstice", angle = 90, vjust = -0.4, color = "orange", size = 3.5) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "The Symmetry of Daylight",
           subtitle = "Every spring day has an autumn counterpart with nearly equal daylight (blue dotted pairs).",
           y = "Hours of Daylight", x = "Date", caption = PLOT_CAPTION) +
      theme_minimal()

    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  output$p_twilight <- renderPlot({
    df <- solar_data()$daily_data %>%
      select(date, twilight_civil, twilight_nautical) %>%
      pivot_longer(cols = c(twilight_civil, twilight_nautical),
                   names_to = "type", values_to = "minutes") %>%
      mutate(type = recode(type,
                           "twilight_civil"    = "Civil Twilight",
                           "twilight_nautical" = "Nautical Twilight"))
    p <- ggplot(df, aes(x = date, y = minutes, color = type)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("Civil Twilight" = "purple", "Nautical Twilight" = "navy")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Daily Twilight Duration (Combined Morning + Evening)",
           y = "Minutes", x = "Month", color = "Type", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  output$p_extremes <- renderPlot({
    df        <- solar_data()$daily_data
    df_labels <- df %>%
      filter(date %in% c(date[which.min(daylight_hours)], date[which.max(daylight_hours)]))
    p <- ggplot(df, aes(x = date, y = daylight_hours)) +
      geom_line(alpha = 0.4) +
      geom_point(data = df_labels, color = "red", size = 4) +
      geom_text_repel(data = df_labels,
                      aes(label = paste0(format(date, "%b %d"), "\n",
                                         round(daylight_hours, 1), " hrs")),
                      nudge_y = 0.6, size = 3.5) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Yearly Solar Extremes",
           subtitle = "Shortest and longest days of the year",
           y = "Daylight Hours", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  # ---- 3H. Lunar Analysis Plots ----

  output$p_moon_calendar <- renderPlot({
    req(input$month_cal)
    df <- lunar_data()$calendar_data %>%
      filter(as.character(month) == input$month_cal) %>%
      mutate(weekday = factor(weekday, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    req(nrow(df) > 0)

    legend_text <- paste(
      "Icons: \u25CB New Moon  \u263D Crescent  \u25D0 Quarter",
      "\u25D1 Gibbous (faded)  \u25CF Full (solid)"
    )
    ggplot(df, aes(x = weekday, y = -week_of_month)) +
      geom_tile(fill = TILE_FILL, color = INK_ON_DARK, linewidth = 0.5) +
      geom_text(aes(label = day_of_month), nudge_y = 0.25, color = INK_ON_DARK, size = 4) +
      # No family= : these are Unicode geometric shapes now, so they render in
      # the default font. The old "fontawesome-webfont" family only resolved if
      # {emojifont} had registered it, and fell back silently otherwise.
      geom_text(aes(label = phase_emoji, alpha = phase_alpha),
                size = 10, color = "skyblue") +
      scale_alpha_identity() +
      scale_x_discrete(position = "top") +
      theme_void() +
      labs(title   = paste("Moon Phase Calendar:", input$month_cal, params()$year),
           caption = legend_text) +
      theme(
        plot.title   = element_text(hjust = 0.5, size = 18),
        plot.caption = element_text(hjust = 0.5, size = 10, color = INK_MUTED),
        axis.text.x  = element_text(color = "gray80", face = "bold")
      )
  })

  output$yearly_summary_table <- renderDT({
    datatable(lunar_data()$yearly_summary_data,
      colnames = c("Month", "Major Phase Dates (NM=New, FQ=First Quarter, FM=Full, TQ=Third Quarter)"),
      options  = list(pageLength = 12, searching = FALSE, lengthChange = FALSE, dom = "t"),
      rownames = FALSE,
      caption  = paste("Major Moon Phases for", params()$year)
    )
  })

  output$p_moon_illum <- renderPlot({
    df     <- lunar_data()$daily_data
    phases <- lunar_data()$major_phases
    p <- ggplot(df, aes(x = date, y = illumination * 100)) +
      geom_area(fill = "skyblue", alpha = 0.3) +
      geom_line(color = "skyblue", linewidth = 1.2) +
      geom_vline(data = phases, aes(xintercept = date),
                 linetype = "dashed", alpha = 0.7, color = INK_ON_DARK) +
      geom_text(data = phases, aes(x = date, label = phase_type),
                y = 5, angle = 90, vjust = -0.5, hjust = 0, size = 2.8, color = INK_ON_DARK) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Daily Moon Illumination and Major Phases",
           subtitle = "Dashed lines mark each major phase date",
           y = "Illumination %", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "solid", color = "red", alpha = 0.6)
    p
  })

  output$p_moon_distance <- renderPlot({
    df <- lunar_data()$daily_data

    # Explicit as.Date() on both sides avoids POSIXct vs Date type mismatch
    # that would cause the join to produce all-NA distance_km values.
    full_dates <- as.Date(lunar_data()$major_phases$date[
      lunar_data()$major_phases$phase_type == "Full Moon"
    ])
    phases     <- df %>%
      filter(as.Date(date) %in% full_dates) %>%
      select(date, distance_km)
    supermoons <- phases %>% slice_min(distance_km, n = 2)

    p <- ggplot(df, aes(x = date, y = distance_km)) +
      geom_line(color = "gray60", linewidth = 1) +
      geom_point(data = phases, aes(y = distance_km), color = "gold", size = 3) +
      geom_label_repel(data = supermoons,
                       aes(y = distance_km,
                           label = paste0("Supermoon\n", format(date, "%b %d"),
                                          "\n", formatC(round(distance_km), format="d", big.mark=","), " km")),
                       fill = LABEL_FILL, color = "gold", size = 3, label.padding = unit(0.3, "lines")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_y_continuous(labels = function(x) formatC(x, format = "d", big.mark = ",")) +
      labs(title = "Earth-Moon Distance Throughout the Year",
           subtitle = "Gold points = full moon dates. Labeled = two closest supermoons.",
           y = "Distance (km)", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  output$moon_events_table <- renderDT({
    df <- lunar_data()$lunar_events
    if (nrow(df) == 0)
      return(datatable(data.frame(Message = "No special lunar events found for this year."),
                       options = list(dom = "t")))
    datatable(df,
      colnames = c("Date", "Event", "Details"),
      options  = list(pageLength = 15, searching = FALSE, lengthChange = FALSE),
      rownames = FALSE,
      caption  = paste("Special Lunar Events for", params()$year)
    )
  })

  output$p_moon_times <- renderPlot({
    df         <- lunar_data()$daily_data
    dummy_date <- as.Date("1970-01-01")
    df_long <- df %>%
      filter(!is.na(moonrise), !is.na(moonset)) %>%
      mutate(
        rise_time = ymd_hms(paste(dummy_date, format(moonrise, "%H:%M:%S"))),
        set_time  = ymd_hms(paste(dummy_date, format(moonset,  "%H:%M:%S")))
      ) %>%
      pivot_longer(cols = c(rise_time, set_time), names_to = "event", values_to = "time")

    p <- ggplot(df_long, aes(x = date, y = time, color = event)) +
      geom_line(linewidth = 1) +
      scale_y_datetime(date_labels = "%I:%M %p") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_color_manual(values = c("rise_time" = "orange", "set_time" = "navy"),
                         labels = c("rise_time" = "Moonrise", "set_time" = "Moonset")) +
      labs(title = "Moonrise and Moonset Times",
           subtitle = "Gaps = days when moon does not rise or set for this location",
           y = "Time of Day", x = "Month", color = "Event", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  output$p_moon_duration <- renderPlot({
    df <- lunar_data()$daily_data
    p  <- ggplot(df, aes(x = date, y = duration_hours)) +
      geom_area(fill = "purple", alpha = 0.2) +
      geom_line(color = "purple", linewidth = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Moon Visibility Duration",
           subtitle = "Hours the moon is above the horizon each day",
           y = "Hours", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  # ---- 3I. Sky Watcher ----

  output$p_dark_sky <- renderPlot({
    sun  <- solar_data()$daily_data
    moon <- lunar_data()$daily_data

    df <- sun %>%
      select(date, nauticalDusk, nauticalDawn) %>%
      left_join(moon %>% select(date, illumination), by = "date") %>%
      filter(!is.na(nauticalDusk), !is.na(nauticalDawn)) %>%
      mutate(
        dusk_h = hour(nauticalDusk) + minute(nauticalDusk) / 60,
        dawn_h = hour(nauticalDawn) + minute(nauticalDawn) / 60,
        # The evening segment runs dusk -> midnight and the morning one
        # midnight -> dawn, which assumes dusk lands before local midnight. At
        # high latitudes it can fall after it (e.g. 00:30), and the old code
        # then drew a bar spanning almost the entire day. Clamp those cases.
        dusk_h = ifelse(dusk_h < 12, 24, dusk_h),
        dawn_h = ifelse(dawn_h > 12, 0,  dawn_h)
      )

    p <- ggplot(df, aes(x = date)) +
      geom_segment(aes(xend = date, y = dusk_h, yend = 24, color = illumination * 100), linewidth = 2) +
      geom_segment(aes(xend = date, y = 0,      yend = dawn_h, color = illumination * 100), linewidth = 2) +
      scale_color_gradient(low = "navy", high = "lightyellow", name = "Moon\nIllum %",
                           limits = c(0, 100)) +
      scale_y_continuous(
        breaks = seq(0, 24, 4),
        labels = c("Midnight","4 AM","8 AM","Noon","4 PM","8 PM","Midnight")
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title    = "Dark Sky Observation Windows",
           subtitle = "Nautical dusk-to-dawn. Bright yellow = high moon interference.",
           y = "Hour of Day (Local)", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", color = "red", alpha = 0.6)
    p
  })

  output$p_photography <- renderPlot({
    sun        <- solar_data()$daily_data
    dummy_date <- as.Date("1970-01-01")

    # Build segment ribbons for each light period
    df_segs <- bind_rows(
      sun %>% filter(!is.na(dawn), !is.na(sunrise)) %>%
        transmute(date,
                  y_start = ymd_hms(paste(dummy_date, format(dawn,         "%H:%M:%S"))),
                  y_end   = ymd_hms(paste(dummy_date, format(sunrise,      "%H:%M:%S"))),
                  type    = "Morning Blue Hour"),
      sun %>% filter(!is.na(sunrise), !is.na(goldenHourEnd)) %>%
        transmute(date,
                  y_start = ymd_hms(paste(dummy_date, format(sunrise,      "%H:%M:%S"))),
                  y_end   = ymd_hms(paste(dummy_date, format(goldenHourEnd,"%H:%M:%S"))),
                  type    = "Morning Golden Hour"),
      sun %>% filter(!is.na(goldenHour), !is.na(sunset)) %>%
        transmute(date,
                  y_start = ymd_hms(paste(dummy_date, format(goldenHour,   "%H:%M:%S"))),
                  y_end   = ymd_hms(paste(dummy_date, format(sunset,       "%H:%M:%S"))),
                  type    = "Evening Golden Hour"),
      sun %>% filter(!is.na(sunset), !is.na(dusk)) %>%
        transmute(date,
                  y_start = ymd_hms(paste(dummy_date, format(sunset,       "%H:%M:%S"))),
                  y_end   = ymd_hms(paste(dummy_date, format(dusk,         "%H:%M:%S"))),
                  type    = "Evening Blue Hour")
    ) %>% filter(!is.na(y_start), !is.na(y_end), y_end >= y_start)

    ggplot(df_segs, aes(x = date, ymin = y_start, ymax = y_end, fill = type)) +
      geom_ribbon(alpha = 0.9) +
      scale_y_datetime(date_labels = "%I:%M %p") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_fill_manual(
        values = c(
          "Morning Blue Hour"   = "#1a6fc4",
          "Morning Golden Hour" = "#f5a623",
          "Evening Golden Hour" = "#e67e22",
          "Evening Blue Hour"   = "#2980b9"
        )
      ) +
      labs(title    = "Photography: Golden & Blue Hour Windows",
           subtitle = "Daily light windows ideal for photography throughout the year",
           y = "Time of Day", x = "Month", fill = "Light Period", caption = PLOT_CAPTION) +
      theme_minimal()
  })

  # ---- 3J. Regional Trends ----

  output$p_state_grid <- renderPlot({
    req(state_data())
    df <- state_data()$final_grid %>%
      # Guard: replace any NA facet_label with the plain month name
      mutate(facet_label = factor(
        ifelse(is.na(as.character(facet_label)),
               as.character(month_name),
               as.character(facet_label)),
        levels = unique(as.character(facet_label[!is.na(facet_label)]))
      ))

    # Use plain map_data polygon for the state border — avoids geom_sf/coord_sf
    # conflicts inside facet_wrap that cause "replacement has N rows, data has 1".
    state_border <- map_data("state", region = tolower(params()$state))
    contour_breaks <- seq(-60, 70, by = 10)

    ggplot(df, aes(x = lon, y = lat)) +
      geom_raster(aes(fill = daylight_change)) +
      # na.rm suppresses "zero contours" warnings when a facet has no variation
      geom_contour(aes(z = daylight_change), color = "white", alpha = 0.8,
                   linewidth = 0.35, breaks = contour_breaks, na.rm = TRUE) +
      geom_polygon(data = state_border,
                   aes(x = long, y = lat, group = group),
                   fill = NA, color = "white", linewidth = 0.5,
                   inherit.aes = FALSE) +
      facet_wrap(~facet_label, ncol = 4) +
      scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred",
                           midpoint = 0, na.value = "transparent", name = "Change\n(min)") +
      coord_equal(expand = FALSE) +
      labs(title    = paste("Monthly Daylight Change Across", params()$state, "(", params()$year, ")"),
           subtitle = "Change from first to last day each month | Facet labels: avg daily daylight N/C/S",
           x = "Longitude", y = "Latitude", caption = PLOT_CAPTION) +
      theme_minimal() +
      theme(
        strip.background = element_rect(fill = "gray25", color = "gray40"),
        strip.text       = element_text(color = INK_ON_DARK, face = "bold",
                                        size = 9, lineheight = 1.3),
        panel.spacing    = unit(1.2, "lines"),
        legend.position  = "bottom",
        legend.key.width = unit(1.8, "cm"),
        legend.title     = element_text(size = 9),
        legend.text      = element_text(size = 8)
      )
  })

  output$p_ncs_curve <- renderPlot({
    req(state_data())
    df <- state_data()$daily_data

    p <- ggplot(df, aes(x = date, y = daylight_hours, color = location)) +
      geom_line(linewidth = 1.3) +
      scale_color_manual(values = c("North" = "darkred", "Center" = "darkgreen", "South" = "darkblue")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title    = paste("Annual Daylight Curve for", params()$state, "(", params()$year, ")"),
           subtitle = "Longer summer days in the north; south has smaller seasonal variation.",
           y = "Hours of Daylight", x = "Month", color = "Location", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })
}

# ==============================================================================
# 4. RUN THE APP
# ==============================================================================
shinyApp(ui = ui, server = server)
