
# ============================================================
# DINOS VOLLEYBALL ANALYTICS
# University of Calgary | Canada West | 2026/27
# ============================================================

library(shiny)
library(bslib)

source("R/helper_functions.R")

# ------------------------------------------------------------
# DATA PATHS
# CSV DATA WILL BE CONNECTED AFTER INITIAL DEPLOYMENT
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
# REUSABLE MODULE PAGE
# ------------------------------------------------------------

module_page <- function(kicker, title, description,
                        feature_names, expected_files) {

  div(
    class = "page-content",

    div(
      class = "section-heading",

      div(
        tags$div(class = "section-kicker", kicker),
        h2(title),
        p(description)
      ),

      div(
        class = "data-pill",
        icon("database"),
        "Awaiting season exports"
      )
    ),

    div(
      class = "feature-grid",

      lapply(seq_along(feature_names), function(i) {

        div(
          class = "feature-tile",

          div(
            class = "feature-number",
            sprintf("%02d", i)
          ),

          div(
            class = "feature-name",
            feature_names[[i]]
          )
        )
      })
    ),

    div(
      class = "content-panel module-placeholder",

      empty_state(
        paste(title, "dashboard is ready for data"),

        "The page layout and navigation are in place. Upload the corresponding CSV exports and we will connect the charts, tables, player selectors and match filters.",

        expected_files
      )
    )
  )
}

# ------------------------------------------------------------
# APPLICATION UI
# ------------------------------------------------------------

ui <- page_navbar(

  title = NULL,

  id = "main_nav",

  selected = "Overview",

  fillable = FALSE,

  collapsible = TRUE,

  header = tagList(

    tags$head(

      tags$title("Dinos Volleyball Analytics"),

      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@500;600;700;800&family=Inter:wght@400;500;600;700&display=swap"
      ),

      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css"
      ),

      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "app.css"
      )
    ),

    # --------------------------------------------------------
    # LARGE DINOS HEADER
    # --------------------------------------------------------

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

        div(
          class = "header-divider"
        ),

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
    )
  ),

  # ----------------------------------------------------------
  # OVERVIEW
  # ----------------------------------------------------------

  nav_panel(

    "Overview",

    value = "Overview",

    div(
      class = "page-content",

      div(
        class = "section-heading",

        div(
          tags$div(
            class = "section-kicker",
            "TEAM SNAPSHOT"
          ),

          h2("Season overview"),

          p(
            "A central view of the Dinos' performance across the season."
          )
        ),

        div(
          class = "data-pill",
          icon("database"),
          "CSV data not uploaded yet"
        )
      ),

      div(
        class = "metric-grid",

        kpi_card(
          "MATCHES PLAYED",
          "—",
          "Season match count"
        ),

        kpi_card(
          "MATCH WIN %",
          "—",
          "Overall team result"
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

      div(
        class = "content-grid overview-grid",

        div(
          class = "content-panel panel-wide",

          div(
            class = "panel-heading",

            h3("Performance by skill area"),

            span(
              class = "panel-tag",
              "SEASON"
            )
          ),

          empty_state(
            "Skill-area performance will appear here",

            "This section will compare the team's attack, serve, reception, block and other available skill metrics once the season CSV files are uploaded.",

            c(
              "SKILL_AREA_SEASON_SUMMARY.csv",
              "SKILL_AREA_ALL_MATCHES.csv"
            )
          )
        ),

        div(
          class = "content-panel",

          div(
            class = "panel-heading",

            h3("Team benchmarks"),

            span(
              class = "panel-tag",
              "COMPARISON"
            )
          ),

          empty_state(
            "Benchmark comparison",

            "Team-only benchmark metrics will populate this panel when the benchmark export is available.",

            "BENCHMARK_METRICS_UOFC_ONLY.csv"
          )
        ),

        div(
          class = "content-panel",

          div(
            class = "panel-heading",

            h3("Match-by-match trend"),

            span(
              class = "panel-tag",
              "MATCHES"
            )
          ),

          empty_state(
            "Match results and trends",

            "The match-level performance timeline will appear here after the overview CSVs are added.",

            "KEY_METRICS_ALL_MATCHES.csv"
          )
        )
      )
    )
  ),

  # ----------------------------------------------------------
  # ATTACK
  # ----------------------------------------------------------

  nav_panel(

    "Attack",

    value = "Attack",

    module_page(
      "ATTACK ANALYSIS",

      "Attack performance",

      "Explore team and player attacking efficiency, attack distribution and court-location tendencies.",

      c(
        "Attack details",
        "Attack maps",
        "Player comparison"
      ),

      c(
        "UOFC_ATTACK_DETAILS_SEASON.csv",
        "UOFC_ATTACK_DETAILS_PLAYER_SEASON.csv",
        "UOFC_ATTACK_MAP_EVENTS.csv",
        "UOFC_ATTACK_MAP_TEAM_SUMMARY.csv"
      )
    )
  ),

  # ----------------------------------------------------------
  # ROTATION
  # ----------------------------------------------------------

  nav_panel(

    "Rotation",

    value = "Rotation",

    module_page(
      "ROTATION ANALYSIS",

      "Performance by rotation",

      "Compare sideout and breakpoint performance across the six rotations, with season and match context.",

      c(
        "Rotation performance",
        "Sideout trends",
        "Rotation comparison"
      ),

      c(
        "ROTATION_DETAILS_SEASON_SUMMARY.csv",
        "ROTATION_DETAILS_ALL_MATCHES.csv"
      )
    )
  ),

  # ----------------------------------------------------------
  # PASSING
  # ----------------------------------------------------------

  nav_panel(

    "Passing",

    value = "Passing",

    module_page(
      "RECEPTION ANALYSIS",

      "Passing performance",

      "Review reception quality, pass destinations and individual passer contributions.",

      c(
        "Reception quality",
        "Pass destination map",
        "Individual passers"
      ),

      c(
        "UOFC_PASSING_MAP_EVENTS.csv",
        "UOFC_PASSING_CATEGORY_SUMMARY.csv",
        "UOFC_PASSING_PLAYER_SUMMARY.csv"
      )
    )
  ),

  # ----------------------------------------------------------
  # SERVING
  # ----------------------------------------------------------

  nav_panel(

    "Serving",

    value = "Serving",

    module_page(
      "SERVE ANALYSIS",

      "Serving performance",

      "Explore serve locations, reception pressure, serving outcomes and individual server performance.",

      c(
        "Serve distribution",
        "Reception pressure",
        "Individual servers"
      ),

      c(
        "UOFC_SERVING_MAP_EVENTS.csv",
        "UOFC_SERVING_MAP_TEAM_BY_ZONE.csv",
        "UOFC_SERVING_MAP_PLAYER_TOTALS.csv"
      )
    )
  ),

  # ----------------------------------------------------------
  # BLOCKING
  # ----------------------------------------------------------

  nav_panel(

    "Blocking",

    value = "Blocking",

    module_page(
      "BLOCKING ANALYSIS",

      "Blocking vs opponent attack types",

      "Evaluate how the Dinos' block performs against opponent attack combinations and individual blockers.",

      c(
        "Block outcomes by attack type",
        "Individual blockers",
        "Block-point and positive-touch rates"
      ),

      c(
        "UOFC_BLOCKING_VS_ATTACK_EVENTS.csv",
        "UOFC_BLOCKING_BY_ATTACK_TYPE.csv",
        "UOFC_INDIVIDUAL_BLOCKING_BY_ATTACK_TYPE.csv",
        "UOFC_INDIVIDUAL_BLOCKING_TOTALS.csv"
      )
    )
  ),

  # ----------------------------------------------------------
  # FOOTER
  # ----------------------------------------------------------

  footer = tags$div(
    class = "app-footer",
    "DINOS VOLLEYBALL ANALYTICS  •  CALGARY  •  CANADA WEST  •  2026/27"
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {

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
