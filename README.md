# Celestial Almanac Dashboard
An R Shiny App that generates comprehensive celestial visualizations for any U.S. location. It calculates and plots solar, lunar, and astronomical phenomena, including daylight curves, moon phases, dark sky windows, and photography light periods.

<img width="1968" height="2586" alt="image" src="https://github.com/user-attachments/assets/ddde3665-3b51-4974-bcfd-a1c45da2e7f4" />

---

## About This Project
This application provides a comprehensive, interactive almanac of celestial data. What began as a simple daylight visualizer has evolved into a full-featured dashboard that integrates solar, lunar, and regional trends with specialized tools for astronomers and photographers.

## Features

### 1. Solar Analysis
*   **Annual Daylight Curves**: Compare daylight hours at specific latitudes.
*   **Sunrise & Sunset**: Daily times with automatic Daylight Saving Time (DST) detection.
*   **Equation of Time**: The true difference between apparent and mean solar time (±16 min), computed from the NOAA solar equations rather than measured against a DST-affected wall clock.
*   **Seasonal Markers**: True equinoxes and solstices, found from solar declination — not the *equilux* (the day closest to 12 hours of daylight), which falls several days off.
*   **Daylight Symmetry**: See the near-perfect symmetry of daylight around the summer solstice.
*   **Twilight Exploration**: Detailed durations for Civil and Nautical twilight.

### 2. Lunar Analysis
*   **Phase Calendar**: Monthly moon phase visualization with traditional full moon names.
*   **Major Phases**: New/first-quarter/full/third-quarter dates **and times**, computed with the Meeus ch. 49 algorithm and expressed in the location's own timezone (within a minute of the U.S. Naval Observatory's published times: max 0.8 min, mean 0.3 min across all 99 phases of 2025–2026).
*   **Lunar Events**: Tracking for Supermoons, Micro-moons, Blue Moons, and Black Moons.
*   **Illumination & Distance**: Daily illumination percentages and Earth-Moon distance. Distance uses the Meeus ch. 47 series (within 8 km of JPL Horizons at every new and full moon of 2025–2026), and each new or full moon is judged super/micro at its own instant.
*   **Visibility Schedule**: Estimated moonrise and moonset times for your specific location (days when the moon does not rise or set are labelled rather than left blank).

### 3. Specialized Tools
*   **Sky Watcher**: Dark sky observation windows (nautical dusk-to-dawn) shaded by the Moon's illumination that night (whether or not the Moon is up).
*   **Photography Planner**: Ideal windows for Golden Hour and Blue Hour photography.

### 4. Regional Trends
*   **Daylight Heatmaps**: State-wide grids showing the monthly change in daylight minutes.
*   **Latitudinal Comparison**: Compare Annual Daylight Curves for North, Center, and South points within a state.

## Technical Highlights
*   **Interactive Mapping**: Click anywhere on the map to analyze coordinates or select a state for regional data.
*   **Vectorized Sun Math**: State grids are computed in two vectorized `suncalc` calls rather than ~29,000 scalar ones, so no parallel workers are needed.
*   **Persistent Caching**: Calculations are cached to `.rds` files, making repeated analysis near-instant. The cache key carries a version tag — bump `CACHE_VERSION` in `app.R` whenever a data function's output changes.
*   **Consistent Parameters**: The analysis is driven by a snapshot of the controls taken when you press **Analyze Skies**, so every chart, title and table always describes the same location, year and state.
*   **Modern UI**: Built with `{bslib}` using the "Cyborg" theme for a sleek, dark-mode experience.

## How to Run Locally
### Prerequisites
-   R (version 4.1 or newer recommended)
-   RStudio

### Installation & Setup
1.  **Clone the repository:**
    ```bash
    git clone https://github.com/Cuevman81/State_Daylight_Visualizer_RShiny_app.git
    cd State_Daylight_Visualizer_RShiny_app
    ```
2.  **Install required packages:**
    ```R
    install.packages(c("shiny", "bslib", "bsicons", "thematic", "suncalc", "ggplot2",
                       "dplyr", "lubridate", "maps", "tidyr", "ggrepel", "lutz",
                       "leaflet", "DT", "glue", "shinycssloaders"))
    ```
    `{sf}`, `{metR}`, `{purrr}`, `{furrr}` and `{emojifont}` were previously listed
    but are no longer used — `{metR}` and `{purrr}` were never called, `{furrr}`
    became unnecessary once the grid was vectorized, `{sf}` was replaced by
    `maps::map.where()` (which does not choke on the self-intersections in the
    `{maps}` state outlines), and the moon calendar now draws plain Unicode glyphs.
3.  **Run the app:**
    Open the `app.R` file in RStudio and click **Run App**. The `cache/` directory will be created automatically on first run.

---

## Companion Study: Permanent Daylight Saving Time

[`Permanent_DST_Analysis/`](Permanent_DST_Analysis/) is a standalone analysis built on the same
solar math, prompted by the U.S. House passing the Sunshine Protection Act 308–117 on
14 July 2026. It charts what **permanent daylight saving time** and **permanent standard time**
would each do to mornings and evenings — for Jackson, Mississippi and for all 50 states.

The short version: permanent DST means later winter sunrises **and later** winter sunsets, not
earlier ones. In winter you can brighten the morning or the evening, not both.

| | Current law | Permanent DST |
|---|---|---|
| Latest winter sunrise (Jackson) | ~7:03 AM | **~8:03 AM** |
| Earliest winter sunset (Jackson) | ~4:55 PM | **~5:55 PM** |
| Days sunrise is after 7:30 AM | 0 | **105** |
| Days sunset is after 6:00 PM | 243 | **329** |

Nationally, permanent DST would hold the sun below the horizon past 9 AM across the northern
tier (North Dakota 9:30, Michigan 9:18, Indiana 9:08), while permanent standard time would put
sunset before 4:30 PM in New England (Maine 3:55). Arizona and Hawaii are exempt and would not
change at all.

See [`Permanent_DST_Analysis/README.md`](Permanent_DST_Analysis/README.md) for the full write-up,
methodology and reproduction steps. Sun positions come from the NOAA solar equations (Python) and
`{suncalc}` (R); the folder is self-contained and does not affect the Shiny app.

## Credits 
-   **Author:** Rodney Cuevas 
-   **Data Source:** `{suncalc}` R Package
