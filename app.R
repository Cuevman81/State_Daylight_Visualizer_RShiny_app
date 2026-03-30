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
library(sf)
library(maps)
library(metR)
library(tidyr)
library(purrr)
library(ggrepel)
library(lutz)
library(tools)
library(furrr)
library(leaflet)
library(emojifont)
library(DT)
library(glue)
library(shinycssloaders)

# Activate parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

# Enable consistent ggplot2 styling
thematic_on(bg = "auto", fg = "auto", accent = "auto", font = "sans")

# Global constants
PLOT_CAPTION <- "Source: suncalc R Package | Author: Rodney Cuevas"
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

# --- 1A. Lunar Events (full restoration: Super/Micro New, Blue Moon, Black Moon) ---
get_lunar_events <- function(daily_moon_data, major_phases, year, lat, lon) {
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

  # Black Moon: third new moon in a season with four
  solstices <- as.Date(c(
    paste0(year, "-03-20"), paste0(year, "-06-21"),
    paste0(year, "-09-22"), paste0(year, "-12-21")
  ))
  black_moons <- major_phases %>%
    filter(phase_type == "New Moon") %>%
    mutate(season = cut(date,
      breaks = c(as.Date(paste0(year, "-01-01")), solstices, as.Date(paste0(year, "-12-31"))),
      labels = c("Winter", "Spring", "Summer", "Autumn", "Winter-Late")
    )) %>%
    group_by(season) %>%
    filter(n() >= 4) %>%
    slice(3) %>%
    ungroup() %>%
    mutate(event = "Black Moon", details = "Third new moon in a season with four")

  bind_rows(super_full, micro_full, super_new, micro_new, blue_moons, black_moons) %>%
    select(date, event, details) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE)
}

# --- 1B. Moon Data (restored: phase_name, phase_alpha, full_moon_data, yearly_summary_data) ---
get_combined_moon_data <- function(lat, lon, analysis_year) {
  cache_file <- file.path("cache", sprintf("moon_cache_v3_%s_%s_%s.rds",
                                           round(lat, 4), round(lon, 4), analysis_year))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  tz        <- tz_lookup_coords(lat, lon, method = "accurate")
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

  major_phases <- daily_data %>%
    mutate(
      phase_yesterday = lag(phase_angle, 1),
      phase_type = case_when(
        phase_yesterday > 0.75 & phase_angle < 0.25 ~ "New Moon",
        phase_yesterday < 0.25 & phase_angle >= 0.25 ~ "First Quarter",
        phase_yesterday < 0.50 & phase_angle >= 0.50 ~ "Full Moon",
        phase_yesterday < 0.75 & phase_angle >= 0.75 ~ "Third Quarter",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(phase_type))

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
      phase_emoji = case_when(
        phase_name == "New Moon"                          ~ fontawesome("fa-circle-o"),
        phase_name == "Crescent"                          ~ fontawesome("fa-moon-o"),
        phase_name == "Quarter"                           ~ fontawesome("fa-adjust"),
        phase_name %in% c("Gibbous", "Full Moon")         ~ fontawesome("fa-circle"),
        TRUE ~ ""
      ),
      phase_alpha = ifelse(phase_name == "Gibbous", 0.5, 1.0)
    )

  lunar_events <- get_lunar_events(daily_data, major_phases, analysis_year, lat, lon)

  phase_to_abbr <- c("New Moon" = "NM", "First Quarter" = "FQ",
                     "Full Moon" = "FM", "Third Quarter" = "TQ")

  full_moon_data <- major_phases %>%
    filter(phase_type == "Full Moon") %>%
    mutate(
      month_name = month.name[month(date)],
      moon_name  = FULL_MOON_NAMES[month_name]
    ) %>%
    select(month_name, moon_name)

  yearly_summary_data <- major_phases %>%
    mutate(
      month_name = month.name[month(date)],
      day_num    = day(date),
      phase_abbr = phase_to_abbr[phase_type]
    ) %>%
    arrange(date) %>%
    group_by(month_name) %>%
    summarise(Events = paste(glue("{day_num} ({phase_abbr})"), collapse = ", "), .groups = "drop") %>%
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
  cache_file <- file.path("cache", sprintf("sun_point_cache_v3_%s_%s_%s.rds",
                                           round(lat, 4), round(lon, 4), analysis_year))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  tz        <- tz_lookup_coords(lat, lon, method = "accurate")
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
    pos_noon <- getSunPosition(date = sun_times$solarNoon, lat = lat, lon = lon)
    daily_data$daylight_hours <- ifelse(
      is.na(daily_data$daylight_hours),
      ifelse(pos_noon$altitude > 0, 24, 0),
      daily_data$daylight_hours
    )
  }

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

  res <- list(daily_data = daily_data, tz = tz, dst_dates = dst_dates)
  saveRDS(res, cache_file)
  return(res)
}

# --- 1D. State Grid (restored: N/C/S, daily curve data, facet labels, contour-ready) ---
get_state_sun_grid <- function(state_name, analysis_year, resolution = 30) {
  state_name <- tolower(state_name)
  cache_file <- file.path("cache", sprintf("sun_grid_cache_v3_%s_%s_%s.rds",
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

  state_sf <- st_as_sf(raw_map, coords = c("long", "lat"), crs = 4326) %>%
    group_by(group) %>%
    summarise(geometry = st_combine(geometry), .groups = "drop") %>%
    st_cast("POLYGON") %>%
    summarise(geometry = st_union(geometry))

  bbox        <- st_bbox(state_sf)
  grid_points <- expand.grid(
    lon = seq(bbox["xmin"], bbox["xmax"], length.out = resolution),
    lat = seq(bbox["ymin"], bbox["ymax"], length.out = resolution)
  )
  state_grid    <- st_filter(st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4326), state_sf)
  state_grid_df <- as.data.frame(st_coordinates(state_grid))
  colnames(state_grid_df) <- c("lon", "lat")

  # Points of Interest: South / Center / North
  poi <- state_grid_df %>%
    arrange(lat) %>%
    mutate(lat_rank = ntile(lat, 100)) %>%
    group_by(lat_rank) %>%
    summarise(lat = mean(lat), lon = mean(lon), .groups = "drop") %>%
    filter(lat_rank %in% c(1, 50, 100)) %>%
    mutate(location = c("South", "Center", "North")[match(lat_rank, c(1, 50, 100))]) %>%
    select(location, lat, lon)

  center_poi <- poi %>% filter(location == "Center")
  tz         <- tz_lookup_coords(center_poi$lat, center_poi$lon, method = "accurate")

  # Monthly daylight change across the grid (parallel)
  calc_change <- function(lat, lon, year, month) {
    start <- ymd(paste(year, month, "01", sep = "-"))
    end   <- ceiling_date(start, "month") - days(1)
    s     <- getSunlightTimes(date = start, lat = lat, lon = lon)
    e     <- getSunlightTimes(date = end,   lat = lat, lon = lon)
    as.numeric(difftime(e$sunset, e$sunrise, units = "mins")) -
    as.numeric(difftime(s$sunset, s$sunrise, units = "mins"))
  }

  grid_month <- tidyr::crossing(month = 1:12, state_grid_df)
  changes    <- future_pmap_dbl(
    list(lat = grid_month$lat, lon = grid_month$lon,
         year = analysis_year, month = grid_month$month),
    ~calc_change(..1, ..2, ..3, ..4)
  )

  # Daily data for N/C/S (vectorized: 3 calls x 365 dates)
  all_dates <- seq(ymd(paste0(analysis_year, "-01-01")),
                   ymd(paste0(analysis_year, "-12-31")), by = "day")
  daily_list <- lapply(seq_len(nrow(poi)), function(i) {
    p  <- poi[i, ]
    st <- getSunlightTimes(date = all_dates, lat = p$lat, lon = p$lon)
    data.frame(
      date           = all_dates,
      location       = p$location,
      daylight_hours = as.numeric(difftime(st$sunset, st$sunrise, units = "hours")),
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
    state_sf   = state_sf,
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
             card_body("Deviation of solar noon from 12:00 — visualizes the Equation of Time."),
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

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = coords$lon, lat = coords$lat, zoom = 4) %>%
      addMarkers(lng = coords$lon, lat = coords$lat)
  })

  output$coords_display <- renderText({
    sprintf("Lat: %.4f\nLon: %.4f", coords$lat, coords$lon)
  })

  output$mini_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = coords$lon, lat = coords$lat, zoom = 8) %>%
      addMarkers(lng = coords$lon, lat = coords$lat, label = "Analyzed Point")
  })

  observe({
    coords$lat; coords$lon
    leafletProxy("mini_map") %>%
      clearMarkers() %>%
      setView(lng = coords$lon, lat = coords$lat, zoom = 8) %>%
      addMarkers(lng = coords$lon, lat = coords$lat, label = "Analyzed Point")
  })

  # ---- 3B. Reference date (accounts for non-current analysis years) ----
  ref_today <- reactive({
    yr    <- input$year
    today <- Sys.Date()
    if (year(today) == yr) today else as.Date(paste0(yr, "-01-01"))
  })

  # NULL when analysis year != current year (hides "today" markers on plots)
  today_line <- reactive({
    if (year(Sys.Date()) == input$year) Sys.Date() else NULL
  })

  # ---- 3C. Data Reactives ----
  solar_data <- eventReactive(input$go, {
    req(coords$lat, coords$lon)
    withProgress(message = "Calculating Solar Data...", value = 0.5,
      get_combined_sun_data(coords$lat, coords$lon, input$year))
  }, ignoreNULL = FALSE)

  lunar_data <- eventReactive(input$go, {
    req(coords$lat, coords$lon)
    withProgress(message = "Calculating Lunar Data...", value = 0.5,
      get_combined_moon_data(coords$lat, coords$lon, input$year))
  }, ignoreNULL = FALSE)

  state_data <- eventReactive(input$go, {
    req(input$state)
    withProgress(message = glue("Simulating {input$state} Grid..."), value = 0.5,
      get_state_sun_grid(input$state, input$year, resolution = as.integer(input$grid_res)))
  }, ignoreNULL = FALSE)

  # ---- 3D. Seasonal dates (computed from solar data) ----
  seasonal_dates <- reactive({
    df <- solar_data()$daily_data
    list(
      summer = df$date[which.max(df$daylight_hours)],
      winter = df$date[which.min(df$daylight_hours)],
      spring = (df %>% filter(month(date) %in% 2:4) %>%
                  mutate(d = abs(daylight_hours - 12)) %>% slice_min(d, n = 1))$date[1],
      fall   = (df %>% filter(month(date) %in% 8:10) %>%
                  mutate(d = abs(daylight_hours - 12)) %>% slice_min(d, n = 1))$date[1]
    )
  })

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

  output$today_schedule <- renderDT({
    s <- solar_data()$daily_data %>% filter(as.Date(date) == ref_today())
    m <- lunar_data()$daily_data  %>% filter(as.Date(date) == ref_today())
    if (nrow(s) == 0) s <- solar_data()$daily_data %>% slice(1)
    if (nrow(m) == 0) m <- lunar_data()$daily_data  %>% slice(1)
    df <- data.frame(
      Event = c("Sunrise", "Sunset", "Solar Noon", "Golden Hour End",
                "Moonrise", "Moonset", "Civil Dawn", "Civil Dusk",
                "Nautical Dawn", "Nautical Dusk"),
      Time  = c(
        format(s$sunrise,       "%I:%M %p"), format(s$sunset,        "%I:%M %p"),
        format(s$solarNoon,     "%I:%M %p"), format(s$goldenHourEnd, "%I:%M %p"),
        format(m$moonrise,      "%I:%M %p"), format(m$moonset,       "%I:%M %p"),
        format(s$dawn,          "%I:%M %p"), format(s$dusk,          "%I:%M %p"),
        format(s$nauticalDawn,  "%I:%M %p"), format(s$nauticalDusk,  "%I:%M %p")
      )
    )
    datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })

  output$full_moon_name_display <- renderText({
    req(input$month_cal, lunar_data())
    name_for_month <- lunar_data()$full_moon_data %>%
      filter(month_name == input$month_cal) %>% pull(moon_name)
    if (length(name_for_month) > 0)
      paste("The full moon in", input$month_cal, "is known as the", name_for_month)
    else
      paste("Moon Phase Calendar —", input$month_cal)
  })

  # ---- 3F. Download Handlers ----
  output$dl_schedule <- downloadHandler(
    filename = function() paste0("celestial_schedule_", ref_today(), ".csv"),
    content  = function(file) {
      s  <- solar_data()$daily_data %>% filter(as.Date(date) == ref_today())
      m  <- lunar_data()$daily_data  %>% filter(as.Date(date) == ref_today())
      if (nrow(s) == 0) s <- solar_data()$daily_data %>% slice(1)
      if (nrow(m) == 0) m <- lunar_data()$daily_data  %>% slice(1)
      df <- data.frame(
        Event = c("Sunrise","Sunset","Solar Noon","Moonrise","Moonset","Civil Dawn","Civil Dusk"),
        Time  = c(format(s$sunrise,  "%I:%M %p"), format(s$sunset,   "%I:%M %p"),
                  format(s$solarNoon,"%I:%M %p"), format(m$moonrise, "%I:%M %p"),
                  format(m$moonset,  "%I:%M %p"), format(s$dawn,     "%I:%M %p"),
                  format(s$dusk,     "%I:%M %p"))
      )
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$dl_phases <- downloadHandler(
    filename = function() paste0("lunar_phases_", input$year, ".csv"),
    content  = function(file) write.csv(lunar_data()$yearly_summary_data, file, row.names = FALSE)
  )

  output$dl_events <- downloadHandler(
    filename = function() paste0("lunar_events_", input$year, ".csv"),
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
    df <- solar_data()$daily_data %>%
      mutate(month = month(date, label = TRUE, abbr = FALSE)) %>%
      group_by(month) %>%
      mutate(daylight_change = last(daylight_hours) - first(daylight_hours)) %>%
      summarise(avg_change = mean(daylight_change) * 60)

    ggplot(df, aes(x = month, y = avg_change, fill = avg_change)) +
      geom_col() +
      geom_text(aes(label = round(avg_change, 1),
                    vjust = ifelse(avg_change >= 0, -0.5, 1.5)), size = 3.5, fontface = "bold") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      labs(title = "Average Monthly Change in Daylight",
           subtitle = "Minutes gained/lost from first to last day of each month",
           y = "Minutes per Month", x = NULL, caption = PLOT_CAPTION) +
      theme_minimal() + theme(legend.position = "none")
  })

  output$p_solar_noon <- renderPlot({
    df <- solar_data()$daily_data
    tz <- solar_data()$tz

    # Compute deviation from local clock noon (12:00:00) on each day.
    # with_tz() keeps the same instant but expresses it in the local timezone,
    # so floor_date(..., "day") gives local midnight and +hours(12) gives local noon.
    df_plot <- df %>%
      mutate(
        sn_local   = with_tz(solarNoon, tz),
        local_noon = floor_date(sn_local, "day") + hours(12),
        dev_mins   = as.numeric(difftime(sn_local, local_noon, units = "mins"))
      )

    p <- ggplot(df_plot, aes(x = date, y = dev_mins)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Solar Noon Variation (Equation of Time)",
           subtitle = paste("Deviation of solar noon from 12:00 local time (", tz, ")"),
           y = "Deviation from 12:00 (minutes)", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  output$p_sun_angles <- renderPlot({
    lat_rad <- coords$lat * pi / 180
    df <- solar_data()$daily_data %>%
      mutate(
        declination = 23.45 * sin((360/365) * (yday(date) - 81) * pi/180),
        decl_rad    = declination * pi / 180,
        altitude    = (pi/2 - abs(lat_rad - decl_rad)) * 180 / pi
      )
    p <- ggplot(df, aes(x = date, y = altitude)) +
      geom_line(color = "red", linewidth = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Solar Noon Altitude Angle",
           subtitle = paste("Latitude:", round(coords$lat, 2), "° — higher values = more intense midday sun"),
           y = "Altitude (°)", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
    if (!is.null(today_line()))
      p <- p + geom_vline(xintercept = today_line(), linetype = "dashed", alpha = 0.5)
    p
  })

  output$p_seasonal_angle <- renderPlot({
    lat_rad <- coords$lat * pi / 180
    df <- solar_data()$daily_data %>%
      mutate(
        declination = 23.45 * sin((360/365) * (yday(date) - 81) * pi/180),
        decl_rad    = declination * pi / 180,
        altitude    = (pi/2 - abs(lat_rad - decl_rad)) * 180 / pi
      )
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
                       fill = "gray15", color = "white", size = 3.2, label.padding = unit(0.4, "lines")) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Solar Altitude at Seasonal Turning Points",
           subtitle = paste("Latitude:", round(coords$lat, 2), "°"),
           y = "Solar Noon Altitude (°)", x = "Month", caption = PLOT_CAPTION) +
      theme_minimal()
  })

  output$p_symmetry <- renderPlot({
    df       <- solar_data()$daily_data
    solstice <- df$date[which.max(df$daylight_hours)]
    growing  <- df %>% filter(date < solstice)
    shrinking <- df %>% filter(date > solstice)

    matches <- lapply(seq_len(nrow(growing)), function(i) {
      target <- growing$daylight_hours[i]
      idx    <- which.min(abs(shrinking$daylight_hours - target))
      if (length(idx) > 0)
        data.frame(growing_date  = growing$date[i],
                   matching_date = shrinking$date[idx],
                   daylight      = target)
    })
    matches_df <- bind_rows(matches)
    sample_matches <- matches_df %>% filter(day(growing_date) == 1, month(growing_date) %in% 1:6)

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
      geom_tile(fill = "gray20", color = "white", linewidth = 0.5) +
      geom_text(aes(label = day_of_month), nudge_y = 0.25, color = "white", size = 4) +
      geom_text(aes(label = phase_emoji, alpha = phase_alpha),
                family = "fontawesome-webfont", size = 10, color = "skyblue") +
      scale_alpha_identity() +
      scale_x_discrete(position = "top") +
      theme_void() +
      labs(title   = paste("Moon Phase Calendar:", input$month_cal, input$year),
           caption = legend_text) +
      theme(
        plot.title   = element_text(hjust = 0.5, size = 18),
        plot.caption = element_text(hjust = 0.5, size = 10, color = "gray70"),
        axis.text.x  = element_text(color = "gray80", face = "bold")
      )
  })

  output$yearly_summary_table <- renderDT({
    datatable(lunar_data()$yearly_summary_data,
      colnames = c("Month", "Major Phase Dates (NM=New, FQ=First Quarter, FM=Full, TQ=Third Quarter)"),
      options  = list(pageLength = 12, searching = FALSE, lengthChange = FALSE, dom = "t"),
      rownames = FALSE,
      caption  = paste("Major Moon Phases for", input$year)
    )
  })

  output$p_moon_illum <- renderPlot({
    df     <- lunar_data()$daily_data
    phases <- lunar_data()$major_phases
    p <- ggplot(df, aes(x = date, y = illumination * 100)) +
      geom_area(fill = "skyblue", alpha = 0.3) +
      geom_line(color = "skyblue", linewidth = 1.2) +
      geom_vline(data = phases, aes(xintercept = date),
                 linetype = "dashed", alpha = 0.7, color = "white") +
      geom_text(data = phases, aes(x = date, label = phase_type),
                y = 5, angle = 90, vjust = -0.5, hjust = 0, size = 2.8, color = "white") +
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
                       fill = "gray15", color = "gold", size = 3, label.padding = unit(0.3, "lines")) +
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
      caption  = paste("Special Lunar Events for", input$year)
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
        dawn_h = hour(nauticalDawn) + minute(nauticalDawn) / 60
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
    state_border <- map_data("state", region = tolower(input$state))
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
      labs(title    = paste("Monthly Daylight Change Across", input$state, "(", input$year, ")"),
           subtitle = "Change from first to last day each month | Facet labels: avg daily daylight N/C/S",
           x = "Longitude", y = "Latitude", caption = PLOT_CAPTION) +
      theme_minimal() +
      theme(
        strip.background = element_rect(fill = "gray25", color = "gray40"),
        strip.text       = element_text(color = "white", face = "bold",
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
      labs(title    = paste("Annual Daylight Curve for", input$state, "(", input$year, ")"),
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
