
# ============================================================
# DINOS VOLLEYBALL ANALYTICS
# CALGARY | CANADA WEST | 2026/27
# ============================================================

library(shiny)
library(bslib)

source("R/helper_functions.R")

# ------------------------------------------------------------
# DATA PATHS
# ------------------------------------------------------------

DATA <- list(
  key_metrics_season = "data/exports/season/OVERVIEW/KEY_METRICS_SEASON_SUMMARY.csv",
  key_metrics_matches = "data/exports/season/OVERVIEW/KEY_METRICS_ALL_MATCHES.csv",
  benchmark = "data/exports/season/OVERVIEW/BENCHMARK_METRICS_UOFC_ONLY.csv",
  skill_season = "data/exports/season/OVERVIEW/SKILL_AREA_SEASON_SUMMARY.csv",
  skill_matches = "data/exports/season/OVERVIEW/SKILL_AREA_ALL_MATCHES.csv",

  attack_season = "data/exports/season/ATTACK_DETAILS/UOFC_ATTACK_DETAILS_SEASON.csv",
  attack_player = "data/exports/season/ATTACK_DETAILS/UOFC_ATTACK_DETAILS_PLAYER_SEASON.csv",
  attack_team = "data/exports/season/ATTACK_DETAILS/UOFC_ATTACK_DETAILS_TEAM_SEASON.csv",

  attack_map_events = "data/exports/season/ATTACK_MAPS/UOFC_ATTACK_MAP_EVENTS.csv",
  attack_map_team = "data/exports/season/ATTACK_MAPS/UOFC_ATTACK_MAP_TEAM_SUMMARY.csv",
  attack_map_player = "data/exports/season/ATTACK_MAPS/UOFC_ATTACK_MAP_PLAYER_SUMMARY.csv",

  rotation_season = "data/exports/season/ROTATION/ROTATION_DETAILS_SEASON_SUMMARY.csv",
  rotation_matches = "data/exports/season/ROTATION/ROTATION_DETAILS_ALL_MATCHES.csv",

  passing_events = "data/exports/season/PASSING/UOFC_PASSING_MAP_EVENTS.csv",
  passing_category = "data/exports/season/PASSING/UOFC_PASSING_CATEGORY_SUMMARY.csv",
  passing_player = "data/exports/season/PASSING/UOFC_PASSING_PLAYER_SUMMARY.csv",

  serving_events = "data/exports/season/SERVING/UOFC_SERVING_MAP_EVENTS.csv",
  serving_team = "data/exports/season/SERVING/UOFC_SERVING_MAP_TEAM_BY_ZONE.csv",
  serving_player = "data/exports/season/SERVING/UOFC_SERVING_MAP_PLAYER_TOTALS.csv",

  blocking_events = "data/exports/season/BLOCKING/UOFC_BLOCKING_VS_ATTACK_EVENTS.csv",
  blocking_attack = "data/exports/season/BLOCKING/UOFC_BLOCKING_BY_ATTACK_TYPE.csv",
  blocking_player = "data/exports/season/BLOCKING/UOFC_INDIVIDUAL_BLOCKING_BY_ATTACK_TYPE.csv",
  blocking_totals = "data/exports/season/BLOCKING/UOFC_INDIVIDUAL_BLOCKING_TOTALS.csv"
)

# ------------------------------------------------------------
# ANALYSIS SECTION PLACEHOLDERS
# ------------------------------------------------------------

analysis_section <- function(title, description, files) {

  div(
    class = "content-panel analysis-section",

    div(
      class = "panel-heading",
      h3(title),
      span(class = "panel-tag", "ANALYSIS")
    ),

    empty_state(
      paste(title, "will appear here"),
      description,
      files
    )
  )
}

# ------------------------------------------------------------
# PERIOD PAGE
# Each period will eventually filter all analysis modules.
# ------------------------------------------------------------

period_page <- function(period_name, period_description) {

  div(
    class = "period-content",

    # Page heading
    div(
      class = "section-heading",

      div(
        tags$div(
          class = "section-kicker",
          "TEAM PERFORMANCE"
        ),

        h2(period_name),

        p(period_description)
      ),

      div(
        class = "data-pill",
        icon("database"),
        "DATA NOT CONNECTED"
      )
    ),

    # KPI cards
    div(
      class = "metric-grid",

      kpi_card(
        "MATCHES PLAYED",
        "—",
        "Matches in selected period"
      ),

      kpi_card(
        "MATCH WIN %",
        "—",
        "Overall match results"
      ),

      kpi_card(
        "ATTACK EFFICIENCY",
        "—",
        "Kills minus errors per attempt"
      ),

      kpi_card(
        "SIDEOUT %",
        "—",
        "Reception phase conversion"
      )
    ),

    # Key performance profile
    analysis_section(
      "KEY METRICS · PERFORMANCE PROFILE",
      "Team performance across game phase, passing, attacking, serving and defensive metrics.",
      c(
        "KEY_METRICS_SEASON_SUMMARY.csv",
        "KEY_METRICS_ALL_MATCHES.csv"
      )
    ),

    # Skill area
    analysis_section(
      "PERFORMANCE BY SKILL AREA",
      "Compare the Dinos' performance across attack, reception, serving, blocking and other available skill metrics.",
      c(
        "SKILL_AREA_SEASON_SUMMARY.csv",
        "SKILL_AREA_ALL_MATCHES.csv"
      )
    ),

    # Attack
    analysis_section(
      "ATTACK ANALYSIS",
      "Explore attack efficiency, attack distribution, player contributions and attack locations.",
      c(
        "UOFC_ATTACK_DETAILS_SEASON.csv",
        "UOFC_ATTACK_DETAILS_PLAYER_SEASON.csv",
        "UOFC_ATTACK_MAP_EVENTS.csv"
      )
    ),

    # Rotation
    analysis_section(
      "ROTATION ANALYSIS",
      "Compare sideout and breakpoint performance across the six rotations.",
      c(
        "ROTATION_DETAILS_SEASON_SUMMARY.csv",
        "ROTATION_DETAILS_ALL_MATCHES.csv"
      )
    ),

    # Passing
    analysis_section(
      "PASSING ANALYSIS",
      "Review reception quality, pass destinations, individual passers and attack outcomes following reception.",
      c(
        "UOFC_PASSING_MAP_EVENTS.csv",
        "UOFC_PASSING_CATEGORY_SUMMARY.csv",
        "UOFC_PASSING_PLAYER_SUMMARY.csv"
      )
    ),

    # Serving
    analysis_section(
      "SERVING ANALYSIS",
      "Explore serve locations, reception pressure, serving outcomes and individual server performance.",
      c(
        "UOFC_SERVING_MAP_EVENTS.csv",
        "UOFC_SERVING_MAP_TEAM_BY_ZONE.csv",
        "UOFC_SERVING_MAP_PLAYER_TOTALS.csv"
      )
    ),

    # Blocking
    analysis_section(
      "BLOCKING ANALYSIS",
      "Evaluate block points, positive and negative block outcomes, and blocking performance against opponent attack types.",
      c(
        "UOFC_BLOCKING_VS_ATTACK_EVENTS.csv",
        "UOFC_BLOCKING_BY_ATTACK_TYPE.csv",
        "UOFC_INDIVIDUAL_BLOCKING_BY_ATTACK_TYPE.csv",
        "UOFC_INDIVIDUAL_BLOCKING_TOTALS.csv"
      )
    ),

    # Benchmark
    analysis_section(
      "TEAM BENCHMARKS",
      "Compare team performance against the available benchmark metrics.",
      "BENCHMARK_METRICS_UOFC_ONLY.csv"
    )
  )
}

# ------------------------------------------------------------
# USER INTERFACE
# ------------------------------------------------------------

ui <- fluidPage(

  tags$head(

    tags$title("Dinos Volleyball Analytics"),

    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@500;600;700;800&family=Inter:wght@400;500;600;700&display=swap"
    ),

    tags$link(
      rel = "stylesheet",
      href = "style.css"
    ),

    tags$link(
      rel = "stylesheet",
      href = "app.css"
    )
  ),

  # ----------------------------------------------------------
  # LARGE DINOS HEADER
  # ----------------------------------------------------------

  div(
    class = "dinos-dashboard-header",

    div(
      class = "dinos-brand-area",

      if (file.exists("www/dinos_logo.png")) {

        tags$img(
          src = "dinos_logo.png",
          class = "dinos-team-logo",
          alt = "University of Calgary Dinos"
        )

      } else {

        div(
          class = "logo-placeholder",
          "DINOS"
        )
      },

      div(
        class = "dinos-brand-copy",

        h1("DINOS VOLLEYBALL ANALYTICS"),

        div(
          class = "dinos-season-label",

          span("CALGARY"),
          span(class = "meta-dot", "·"),
          span("CANADA WEST"),
          span(class = "meta-dot", "·"),
          span("2026/27")
        )
      )
    ),

    div(
      class = "header-right",

      div(
        class = "header-context",

        div(
          class = "header-context-label",
          "TEAM"
        ),

        div(
          class = "header-context-value",
          "University of Calgary"
        ),

        div(
          class = "header-context-note",
          "Women's Volleyball"
        )
      ),

      div(class = "header-divider"),

      div(
        class = "header-season-mark",

        div(
          class = "header-season-label",
          "SEASON"
        ),

        div(
          class = "header-season-value",
          "2026/27"
        )
      )
    )
  ),

  # ----------------------------------------------------------
  # FIVE PERIOD SELECTORS
  # ----------------------------------------------------------

  navset_pill(

    id = "period_view",

    selected = "MOST RECENT MATCH",

    nav_panel(
      "MOST RECENT MATCH",
      value = "MOST RECENT MATCH",

      period_page(
        "Most Recent Match",
        "Team performance overview for the latest completed match."
      )
    ),

    nav_panel(
      "PREVIOUS MATCH",
      value = "PREVIOUS MATCH",

      period_page(
        "Previous Match",
        "Team performance overview for the match immediately before the latest match."
      )
    ),

    nav_panel(
      "LAST 5",
      value = "LAST 5",

      period_page(
        "Last 5 Matches",
        "Performance trends and team analysis across the five most recent completed matches."
      )
    ),

    nav_panel(
      "LAST 10",
      value = "LAST 10",

      period_page(
        "Last 10 Matches",
        "Performance trends and team analysis across the ten most recent completed matches."
      )
    ),

    nav_panel(
      "FULL SEASON",
      value = "FULL SEASON",

      period_page(
        "Full Season",
        "Complete season performance, player contributions and team tendencies."
      )
    )
  ),

  # ----------------------------------------------------------
  # FOOTER
  # ----------------------------------------------------------

  div(
    class = "app-footer",
    "DINOS VOLLEYBALL ANALYTICS  •  CALGARY  •  CANADA WEST  •  2026/27"
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {

  # Load any available CSV files.
  # Missing files are expected during initial deployment.

  dashboard_data <- reactive({

    lapply(
      DATA,
      read_dashboard_csv
    )

  })

}

# ------------------------------------------------------------
# RUN APPLICATION
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
