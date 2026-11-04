# ============================================================
# DINOS COACHES DASHBOARD
# CALGARY | CANADA WEST | 2026/27
# ============================================================

library(shiny)
library(bslib)

# ------------------------------------------------------------
# PATHS AND SAFE DATA HELPERS
# ------------------------------------------------------------

SEASON_DIR <- "data/exports/season"
MATCH_DIR  <- "data/exports/matches"
ROSTER_FILE <- "data/reference/UOC_MASTER_ROSTER.csv"

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
             na.strings = c("", "NA")),
    error = function(e) {
      warning(sprintf("Could not read %s: %s", path, conditionMessage(e)))
      NULL
    }
  )
}

season_file <- function(filename) file.path(SEASON_DIR, filename)

# Efficiency is displayed as a decimal ratio, never as a percentage.
# Values already stored as ratios (e.g. 0.138) are retained.
# Values stored in percentage units (e.g. 13.8 or -11.1) are divided by 100.
normalize_efficiency <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  needs_conversion <- !is.na(x) & abs(x) > 1
  x[needs_conversion] <- x[needs_conversion] / 100
  x
}

format_efficiency <- function(x, digits = 3) {
  x <- normalize_efficiency(x)
  ifelse(is.na(x), "—", formatC(x, format = "f", digits = digits))
}

format_percent <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", paste0(formatC(x, format = "f", digits = digits), "%"))
}

# Apply efficiency normalization to any explicitly named efficiency column.
prepare_display_table <- function(data) {
  if (is.null(data) || !nrow(data)) return(data)
  out <- data
  efficiency_cols <- grep("efficiency|eff\\.", names(out),
                          ignore.case = TRUE, value = TRUE)
  for (nm in efficiency_cols) {
    if (is.numeric(out[[nm]]) || is.character(out[[nm]])) {
      out[[nm]] <- format_efficiency(out[[nm]])
    }
  }
  out
}

safe_subset <- function(data, match_ids) {
  if (is.null(data) || !nrow(data)) return(data)
  id_col <- intersect(c("match_id", "Match_ID"), names(data))
  if (!length(id_col) || is.null(match_ids)) return(data)
  data[as.character(data[[id_col[1]]]) %in% match_ids, , drop = FALSE]
}

match_label <- function(id, date = NULL) {
  date_text <- date
  if (is.null(date_text) || is.na(date_text) || !nzchar(date_text)) {
    date_text <- gsub("^_?", "", id)
    date_text <- gsub("_", "-", date_text)
    date_text <- substr(date_text, 1, 10)
  }
  paste0("Calgary vs UBC · ", date_text)
}

# ------------------------------------------------------------
# LOAD CORE DATA
# ------------------------------------------------------------

key_matches <- read_csv_safe(season_file("KEY_METRICS_ALL_MATCHES.csv"))
key_season  <- read_csv_safe(season_file("KEY_METRICS_SEASON_SUMMARY.csv"))
skill_matches <- read_csv_safe(season_file("SKILL_AREA_ALL_MATCHES.csv"))
skill_season  <- read_csv_safe(season_file("SKILL_AREA_SEASON_SUMMARY.csv"))
rotation_matches <- read_csv_safe(season_file("ROTATION_DETAILS_ALL_MATCHES.csv"))
rotation_season  <- read_csv_safe(season_file("ROTATION_DETAILS_SEASON_SUMMARY.csv"))

attack_events <- read_csv_safe(season_file("UOFC_ATTACK_MAP_EVENTS.csv"))
attack_players <- read_csv_safe(season_file("UOFC_ATTACK_DETAILS_PLAYER_SEASON.csv"))
attack_team <- read_csv_safe(season_file("UOFC_ATTACK_DETAILS_TEAM_SEASON.csv"))

passing_events <- read_csv_safe(season_file("UOFC_PASSING_MAP_EVENTS.csv"))
passing_players <- read_csv_safe(season_file("UOFC_PASSING_PLAYER_SUMMARY.csv"))
passing_categories <- read_csv_safe(season_file("UOFC_PASSING_CATEGORY_SUMMARY.csv"))

serving_events <- read_csv_safe(season_file("UOFC_SERVING_MAP_EVENTS.csv"))
serving_players <- read_csv_safe(season_file("UOFC_SERVING_MAP_PLAYER_TOTALS.csv"))
serving_zones <- read_csv_safe(season_file("UOFC_SERVING_MAP_TEAM_BY_ZONE.csv"))

blocking_events <- read_csv_safe(season_file("UOFC_BLOCKING_VS_ATTACK_EVENTS.csv"))
blocking_types <- read_csv_safe(season_file("UOFC_BLOCKING_BY_ATTACK_TYPE.csv"))
blocking_players <- read_csv_safe(season_file("UOFC_INDIVIDUAL_BLOCKING_TOTALS.csv"))

benchmark_data <- read_csv_safe(season_file("SKILL_AREA_BENCHMARK_METRICS_UOFC_ONLY.csv"))
roster <- read_csv_safe(ROSTER_FILE)

# Match list is derived from the actual match-level key metrics.
if (!is.null(key_matches) && all(c("match_id", "match_date") %in% names(key_matches))) {
  match_index <- unique(key_matches[c("match_id", "match_date")])
  match_index <- match_index[order(as.character(match_index$match_date),
                                   decreasing = TRUE), , drop = FALSE]
} else {
  match_index <- data.frame(match_id = character(), match_date = character())
}

# ------------------------------------------------------------
# UI HELPERS
# ------------------------------------------------------------

section_card <- function(title, subtitle = NULL, ...) {
  div(
    class = "content-panel analysis-section",
    div(class = "panel-heading",
        h3(title),
        span(class = "panel-tag", "MATCH ANALYSIS")),
    if (!is.null(subtitle)) p(class = "section-subtitle", subtitle),
    ...
  )
}

table_or_message <- function(data, message = "No data available for this selection.") {
  if (is.null(data) || !nrow(data)) {
    return(tags$p(class = "no-data", message))
  }
  tableOutput
}

metric_card_ui <- function(label, value, note = "") {
  div(class = "metric-card",
      div(class = "metric-label", label),
      div(class = "metric-value", value),
      div(class = "metric-note", note))
}

# ------------------------------------------------------------
# USER INTERFACE
# ------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$title("Dinos Coaches Dashboard"),
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@500;600;700;800&family=Inter:wght@400;500;600;700&display=swap"),
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$link(rel = "stylesheet", href = "app.css"),
    tags$style(HTML("
      .header-actions {display:flex; gap:18px; align-items:center;}
      .header-action {border:0; background:transparent; color:#526274;
        display:flex; flex-direction:column; align-items:center; gap:3px;
        font-family:'Barlow Condensed',sans-serif; font-weight:700;
        letter-spacing:.7px; padding:5px 10px;}
      .header-action .glyphicon {font-size:25px; color:#263746;}
      .header-action.active, .header-action:hover {color:#C8102E;}
      .header-action.active .glyphicon, .header-action:hover .glyphicon {color:#C8102E;}
      .dashboard-period {margin:0 auto 18px;}
      .period-selector {display:flex; gap:7px; background:#fff; border:1px solid #dfe5eb;
        border-radius:16px; padding:8px; box-shadow:0 5px 15px rgba(25,45,65,.055);}
      .period-selector .btn {flex:1; border:0; background:transparent; color:#526274;
        font-family:'Barlow Condensed',sans-serif; font-weight:700; min-height:55px;}
      .period-selector .btn.active {background:#C8102E; color:#fff;}
      .content-panel {background:#fff; border:1px solid #e0e6ec; border-radius:15px;
        box-shadow:0 4px 13px rgba(25,45,65,.045);}
      .analysis-section {margin:16px 5px; padding:22px;}
      .panel-heading {display:flex; justify-content:space-between; align-items:center; gap:12px;}
      .panel-heading h3 {font-family:'Barlow Condensed',sans-serif; font-size:25px; margin:0;}
      .panel-tag {background:#FBECEF; color:#A50E26; border-radius:20px; padding:6px 10px;
        font-size:10px; font-weight:800; letter-spacing:.5px;}
      .section-subtitle,.no-data {color:#77838e; font-size:12px; margin:8px 0 14px;}
      .data-table {width:100%; border-collapse:collapse; font-size:12px;}
      .data-table th {background:#f5f6f8; color:#526274; font-weight:700;}
      .data-table th,.data-table td {padding:8px 10px; border-bottom:1px solid #e8edf1; text-align:left;}
      .report-row {background:#fff; border:1px solid #e0e6ec; border-radius:12px;
        padding:16px 18px; margin:10px 0; display:flex; justify-content:space-between; align-items:center;}
      .report-title {font-weight:700; color:#263746;}
      .report-subtitle {font-size:11px; color:#82909d; margin-top:3px;}
      .report-open {background:#C8102E; color:#fff; border:0; border-radius:8px; padding:8px 15px;}
      .report-open:hover {background:#A50E26; color:#fff;}
      .roster-table {width:100%;}
      .roster-table th {background:#C8102E; color:#fff;}
      .roster-table th,.roster-table td {padding:12px 14px; border-bottom:1px solid #e8edf1;}
      .number-cell {font-weight:800; color:#C8102E; width:90px;}
      .role-cell {text-transform:capitalize; color:#647383;}
      .report-back {margin-bottom:15px;}
      .app-footer {color:#9aa3ac; font-size:9px; font-weight:700; letter-spacing:1.3px;
        padding:20px; text-align:center;}
      @media(max-width:700px){.dinos-dashboard-header{flex-direction:column;align-items:flex-start;}
        .header-actions{width:100%;justify-content:flex-end;}
        .period-selector{overflow-x:auto}.period-selector .btn{min-width:115px;}}
    "))
  ),

  div(class = "dinos-dashboard-header",
    div(class = "dinos-brand-area",
      if (file.exists("www/dinos_logo.png")) {
        tags$img(src = "dinos_logo.png", class = "dinos-team-logo",
                 alt = "University of Calgary Dinos")
      } else div(class = "logo-placeholder", "DINOS"),
      div(class = "dinos-brand-copy",
        h1("DINOS COACHES DASHBOARD"),
        div(class = "dinos-season-label",
          span("CALGARY"), span(class = "meta-dot", "·"),
          span("CANADA WEST"), span(class = "meta-dot", "·"), span("2026/27")
        )
      )
    ),
    div(class = "header-actions",
      actionButton("go_athletes", label = NULL,
        icon = icon("users"), class = "header-action",
        title = "Athletes"),
      actionButton("go_reports", label = NULL,
        icon = icon("file-lines"), class = "header-action",
        title = "Reports")
    )
  ),

  uiOutput("main_content"),

  div(class = "app-footer",
      "DINOS COACHES DASHBOARD  •  CALGARY  •  CANADA WEST  •  2026/27")
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {

  page <- reactiveVal("dashboard")
  selected_period <- reactiveVal("MOST RECENT MATCH")
  selected_report <- reactiveVal(NULL)

  observeEvent(input$go_athletes, {
    page("athletes")
  })

  observeEvent(input$go_reports, {
    page("reports")
    selected_report(NULL)
  })

  output$main_content <- renderUI({

    if (page() == "athletes") {
      roster_data <- roster
      if (is.null(roster_data) || !all(c("number", "name", "role") %in% names(roster_data))) {
        return(div(class = "content-panel analysis-section",
          h2("ATHLETES"),
          p("Roster file not found or required columns are missing. Expected: data/reference/UOC_MASTER_ROSTER.csv")
        ))
      }

      roster_data <- roster_data[order(as.numeric(roster_data$number)), , drop = FALSE]

      return(tagList(
        div(class = "section-heading",
          div(div(class = "section-kicker", "TEAM ROSTER"),
              h2("ATHLETES"),
              p("University of Calgary · Women's Volleyball · 2026/27")),
          div(class = "data-pill", paste(nrow(roster_data), "ROSTERED ATHLETES"))
        ),
        div(class = "content-panel analysis-section",
          tableOutput("roster_table")
        )
      ))
    }

    if (page() == "reports") {
      if (!is.null(selected_report())) {
        return(tagList(
          actionButton("back_reports", "← ALL MATCH REPORTS",
                       class = "btn btn-default report-back"),
          uiOutput("individual_report")
        ))
      }

      if (!nrow(match_index)) {
        return(div(class = "content-panel analysis-section",
          h2("MATCH REPORTS"),
          p("No match index is available. Check KEY_METRICS_ALL_MATCHES.csv.")
        ))
      }

      return(tagList(
        div(class = "section-heading",
          div(div(class = "section-kicker", "MATCH LIBRARY"),
              h2("REPORTS"),
              p("Select a match to open its individual performance report.")),
          div(class = "data-pill", paste(nrow(match_index), "MATCHES AVAILABLE"))
        ),
        uiOutput("report_list")
      ))
    }

    # DASHBOARD PAGE
    tagList(
      div(class = "period-selector dashboard-period",
        actionButton("period_recent", "MOST RECENT MATCH", class = "btn period-btn"),
        actionButton("period_previous", "PREVIOUS MATCH", class = "btn period-btn"),
        actionButton("period_5", "LAST 5", class = "btn period-btn"),
        actionButton("period_10", "LAST 10", class = "btn period-btn"),
        actionButton("period_season", "FULL SEASON", class = "btn period-btn")
      ),
      div(class = "section-heading",
        div(div(class = "section-kicker", "TEAM PERFORMANCE"),
            h2(textOutput("period_title", inline = TRUE)),
            p(textOutput("period_subtitle", inline = TRUE))),
        div(class = "data-pill", textOutput("period_status", inline = TRUE))
      ),
      uiOutput("dashboard_body")
    )
  })

  observeEvent(input$back_reports, {
    selected_report(NULL)
  })

  observeEvent(input$period_recent, selected_period("MOST RECENT MATCH"))
  observeEvent(input$period_previous, selected_period("PREVIOUS MATCH"))
  observeEvent(input$period_5, selected_period("LAST 5"))
  observeEvent(input$period_10, selected_period("LAST 10"))
  observeEvent(input$period_season, selected_period("FULL SEASON"))

  selected_match_ids <- reactive({
    if (!nrow(match_index)) return(character(0))
    ids <- as.character(match_index$match_id)
    switch(selected_period(),
      "MOST RECENT MATCH" = ids[1],
      "PREVIOUS MATCH" = if (length(ids) >= 2) ids[2] else character(0),
      "LAST 5" = head(ids, 5),
      "LAST 10" = head(ids, 10),
      "FULL SEASON" = ids,
      ids[1]
    )
  })

  period_label <- reactive({
    switch(selected_period(),
      "MOST RECENT MATCH" = "Most Recent Match",
      "PREVIOUS MATCH" = "Previous Match",
      "LAST 5" = "Last 5 Matches",
      "LAST 10" = "Last 10 Matches",
      "FULL SEASON" = "Full Season"
    )
  })

  output$period_title <- renderText(period_label())

  output$period_subtitle <- renderText({
    n <- length(selected_match_ids())
    if (selected_period() == "FULL SEASON") {
      "Complete season performance and player contributions."
    } else {
      paste("Performance across", n, "available match(es) in the selected period.")
    }
  })

  output$period_status <- renderText({
    paste(length(selected_match_ids()), "MATCH(ES) INCLUDED")
  })

  output$roster_table <- renderTable({
    if (is.null(roster)) return(NULL)
    d <- roster[order(as.numeric(roster$number)),
                c("number", "name", "role"), drop = FALSE]
    names(d) <- c("NUMBER", "ATHLETE", "ROLE")
    d
  }, striped = TRUE, bordered = FALSE, spacing = "m", rownames = FALSE)

  output$kpi_cards <- renderUI({
    d <- if (selected_period() == "FULL SEASON") key_season else
      safe_subset(key_matches, selected_match_ids())

    get_metric <- function(pattern, efficiency = FALSE) {
      if (is.null(d) || !nrow(d) ||
          !all(c("metric_name", "metric_value") %in% names(d))) return("—")
      hit <- grepl(pattern, d$metric_name, ignore.case = TRUE)
      if (!any(hit)) return("—")

      rows <- d[hit, , drop = FALSE]
      if (all(c("numerator", "denominator") %in% names(rows))) {
        num <- sum(as.numeric(rows$numerator), na.rm = TRUE)
        den <- sum(as.numeric(rows$denominator), na.rm = TRUE)
        value <- if (den > 0) num / den else NA_real_
        if (!efficiency) value <- value * 100
      } else {
        value <- as.numeric(rows$metric_value[1])
      }

      if (efficiency) format_efficiency(value) else format_percent(value)
    }

    div(class = "metric-grid",
      metric_card_ui("MATCHES INCLUDED", length(selected_match_ids()),
                     "Matches in selected period"),
      metric_card_ui("ATTACK EFFICIENCY",
                     get_metric("^Attack Efficiency$", TRUE),
                     "Decimal ratio · not percentage"),
      metric_card_ui("SIDE-OUT",
                     get_metric("^Side-Out", FALSE),
                     "Side-out points / opportunities"),
      metric_card_ui("IN-SYSTEM RECEPTION",
                     get_metric("^In System", FALSE),
                     "Perfect and positive receptions")
    )
  })

  output$dashboard_body <- renderUI({
    tagList(
      uiOutput("kpi_cards"),
      section_card("KEY PERFORMANCE METRICS",
        "Match-level metrics for the selected period.",
        tableOutput("key_metrics_table")),
      section_card("PERFORMANCE BY SKILL AREA",
        "Skill outcomes and event distributions for the selected matches.",
        tableOutput("skill_table")),
      section_card("ATTACK ANALYSIS",
        "Attack outcomes, player contributions and attack efficiency.",
        tableOutput("attack_table")),
      section_card("ROTATION ANALYSIS",
        "Side-out, breakpoint, reception and attack efficiency by rotation.",
        tableOutput("rotation_table")),
      section_card("PASSING ANALYSIS",
        "Reception quality and attack outcomes following reception.",
        tableOutput("passing_table")),
      section_card("SERVING ANALYSIS",
        "Serve outcomes and serving pressure by player.",
        tableOutput("serving_table")),
      section_card("BLOCKING ANALYSIS",
        "Block points, positive/negative touches and block errors.",
        tableOutput("blocking_table")),
      section_card("TEAM BENCHMARKS",
        "Available UofC benchmark metrics. These are not external league averages.",
        tableOutput("benchmark_table"))
    )
  })

  # Key metrics: efficiencies remain decimal ratios; other percentage metrics retain %.
  output$key_metrics_table <- renderTable({
    d <- if (selected_period() == "FULL SEASON") key_season else safe_subset(key_matches, selected_match_ids())
    if (is.null(d) || !nrow(d)) return(NULL)

    if (selected_period() != "FULL SEASON" &&
        all(c("metric_name", "metric_value") %in% names(d))) {
      # Recalculate selected-period values from summed numerators/denominators.
      if (all(c("metric_name", "numerator", "denominator") %in% names(d))) {
        groups <- split(d, d$metric_name)
        d <- do.call(rbind, lapply(groups, function(g) {
          num <- sum(as.numeric(g$numerator), na.rm = TRUE)
          den <- sum(as.numeric(g$denominator), na.rm = TRUE)
          value <- if (den > 0) num / den else NA_real_
          metric_name <- as.character(g$metric_name[1])
          if (!grepl("eff", metric_name, ignore.case = TRUE)) value <- value * 100
          data.frame(metric_name = metric_name, numerator = num,
                     denominator = den, metric_value = value)
        }))
      }
    }

    d <- d[, intersect(c("metric_category", "metric_name", "numerator",
                         "denominator", "metric_value"), names(d)), drop = FALSE]
    if ("metric_value" %in% names(d) && "metric_name" %in% names(d)) {
      is_eff <- grepl("eff", d$metric_name, ignore.case = TRUE)
      d$metric_value <- ifelse(is_eff,
        format_efficiency(d$metric_value),
        format_percent(d$metric_value))
    }
    d
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$skill_table <- renderTable({
    d <- if (selected_period() == "FULL SEASON") skill_season else safe_subset(skill_matches, selected_match_ids())
    if (is.null(d)) return(NULL)
    if (all(c("section", "outcome", "event_count", "denominator") %in% names(d))) {
      agg <- aggregate(cbind(event_count, denominator) ~ section + outcome,
                       data = d, FUN = function(x) sum(as.numeric(x), na.rm = TRUE))
      agg$percentage <- ifelse(agg$denominator > 0,
                               100 * agg$event_count / agg$denominator, NA)
      agg$percentage <- format_percent(agg$percentage)
      return(agg)
    }
    d
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$attack_table <- renderTable({
    d <- safe_subset(attack_events, selected_match_ids())
    if (is.null(d) || !nrow(d)) return(attack_team)
    required <- c("Attack_Type", "Attack_Outcome")
    if (all(required %in% names(d))) {
      tab <- as.data.frame(table(d$Attack_Type, d$Attack_Outcome),
                           stringsAsFactors = FALSE)
      names(tab) <- c("ATTACK TYPE", "OUTCOME", "ATTEMPTS")
      tab <- tab[tab$ATTEMPTS > 0, ]
      return(tab)
    }
    prepare_display_table(d)
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$rotation_table <- renderTable({
    d <- safe_subset(rotation_matches, selected_match_ids())
    if (is.null(d) || !nrow(d)) d <- rotation_season
    if (is.null(d)) return(NULL)
    cols <- intersect(c("rotation", "SO_num", "SO_den", "SO_pct",
                        "PS_num", "PS_den", "PS_pct",
                        "Attack_Attempts", "Kills", "Attack_Errors",
                        "Attack_Efficiency", "First_Ball_Efficiency",
                        "Transition_Efficiency"), names(d))
    out <- d[, cols, drop = FALSE]
    if ("Attack_Efficiency" %in% names(out))
      out$Attack_Efficiency <- format_efficiency(out$Attack_Efficiency)
    if ("First_Ball_Efficiency" %in% names(out))
      out$First_Ball_Efficiency <- format_efficiency(out$First_Ball_Efficiency)
    if ("Transition_Efficiency" %in% names(out))
      out$Transition_Efficiency <- format_efficiency(out$Transition_Efficiency)
    out
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$passing_table <- renderTable({
    d <- safe_subset(passing_events, selected_match_ids())
    if (is.null(d) || !nrow(d)) return(passing_players)
    if ("Pass_Category" %in% names(d)) {
      tab <- as.data.frame(table(d$Pass_Category), stringsAsFactors = FALSE)
      names(tab) <- c("PASS CATEGORY", "RECEPTIONS")
      tab
    } else d
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$serving_table <- renderTable({
    d <- safe_subset(serving_events, selected_match_ids())
    if (is.null(d) || !nrow(d)) return(serving_players)
    if ("Server_Name" %in% names(d)) {
      tab <- aggregate(cbind(Is_Ace, Is_Serve_Error) ~ Server_Name,
                       data = transform(d,
                         Is_Ace = as.numeric(Is_Ace),
                         Is_Serve_Error = as.numeric(Is_Serve_Error)),
                       FUN = function(x) sum(x, na.rm = TRUE))
      names(tab) <- c("SERVER", "ACES", "SERVE ERRORS")
      tab
    } else d
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$blocking_table <- renderTable({
    d <- safe_subset(blocking_events, selected_match_ids())
    if (is.null(d) || !nrow(d)) return(blocking_types)
    if ("Block_Outcome" %in% names(d)) {
      tab <- as.data.frame(table(d$Block_Outcome), stringsAsFactors = FALSE)
      names(tab) <- c("BLOCK OUTCOME", "BLOCKS")
      tab
    } else d
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$benchmark_table <- renderTable({
    benchmark_data
  }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

  output$report_list <- renderUI({
    lapply(seq_len(nrow(match_index)), function(i) {
      id <- as.character(match_index$match_id[i])
      date <- as.character(match_index$match_date[i])
      div(class = "report-row",
        div(
          div(class = "report-title", "CALGARY vs UBC"),
          div(class = "report-subtitle", date, " · Individual match report")
        ),
        actionButton(paste0("open_report_", i), "OPEN REPORT",
                     class = "report-open")
      )
    })
  })

  observe({
    lapply(seq_len(nrow(match_index)), function(i) {
      local({
        idx <- i
        observeEvent(input[[paste0("open_report_", idx)]], {
          selected_report(as.character(match_index$match_id[idx]))
        }, ignoreInit = TRUE)
      })
    })
  })

  output$individual_report <- renderUI({
    req(selected_report())
    id <- selected_report()
    date <- match_index$match_date[match_index$match_id == id][1]

    tagList(
      div(class = "section-heading",
        div(div(class = "section-kicker", "INDIVIDUAL MATCH REPORT"),
            h2("CALGARY vs UBC"),
            p(as.character(date), " · 2026/27"))
      ),
      section_card("KEY METRICS", "Performance indicators for this match.",
                   tableOutput("report_key_table")),
      section_card("ATTACK", "Attack outcomes for this match.",
                   tableOutput("report_attack_table")),
      section_card("ROTATION", "Rotation performance for this match.",
                   tableOutput("report_rotation_table")),
      section_card("PASSING", "Reception categories for this match.",
                   tableOutput("report_passing_table")),
      section_card("SERVING", "Serve outcomes for this match.",
                   tableOutput("report_serving_table")),
      section_card("BLOCKING", "Block outcomes for this match.",
                   tableOutput("report_blocking_table"))
    )
  })

  output$report_key_table <- renderTable({
    d <- safe_subset(key_matches, selected_report())
    if (is.null(d)) return(NULL)
    d <- d[, intersect(c("metric_category", "metric_name", "numerator",
                         "denominator", "metric_value"), names(d)), drop = FALSE]
    if (all(c("metric_name", "metric_value") %in% names(d))) {
      is_eff <- grepl("eff", d$metric_name, ignore.case = TRUE)
      d$metric_value <- ifelse(is_eff,
        format_efficiency(d$metric_value),
        format_percent(d$metric_value))
    }
    d
  }, striped = TRUE, bordered = FALSE, rownames = FALSE)

  output$report_attack_table <- renderTable({
    d <- safe_subset(attack_events, selected_report())
    if (is.null(d)) return(NULL)
    if (all(c("Attack_Type", "Attack_Outcome") %in% names(d))) {
      tab <- as.data.frame(table(d$Attack_Type, d$Attack_Outcome),
                           stringsAsFactors = FALSE)
      names(tab) <- c("ATTACK TYPE", "OUTCOME", "ATTEMPTS")
      tab[tab$ATTEMPTS > 0, ]
    } else d
  }, striped = TRUE, bordered = FALSE, rownames = FALSE)

  output$report_rotation_table <- renderTable({
    d <- safe_subset(rotation_matches, selected_report())
    if (is.null(d)) return(NULL)
    out <- d[, intersect(c("rotation", "SO_num", "SO_den", "SO_pct",
                           "PS_num", "PS_den", "PS_pct",
                           "Attack_Attempts", "Kills", "Attack_Errors",
                           "Attack_Efficiency"), names(d)), drop = FALSE]
    if ("Attack_Efficiency" %in% names(out))
      out$Attack_Efficiency <- format_efficiency(out$Attack_Efficiency)
    out
  }, striped = TRUE, bordered = FALSE, rownames = FALSE)

  output$report_passing_table <- renderTable({
    d <- safe_subset(passing_events, selected_report())
    if (is.null(d) || !"Pass_Category" %in% names(d)) return(NULL)
    tab <- as.data.frame(table(d$Pass_Category), stringsAsFactors = FALSE)
    names(tab) <- c("PASS CATEGORY", "RECEPTIONS")
    tab
  }, striped = TRUE, bordered = FALSE, rownames = FALSE)

  output$report_serving_table <- renderTable({
    d <- safe_subset(serving_events, selected_report())
    if (is.null(d)) return(NULL)
    if ("Serve_Outcome" %in% names(d)) {
      tab <- as.data.frame(table(d$Serve_Outcome), stringsAsFactors = FALSE)
      names(tab) <- c("SERVE OUTCOME", "SERVES")
      tab
    } else d
  }, striped = TRUE, bordered = FALSE, rownames = FALSE)

  output$report_blocking_table <- renderTable({
    d <- safe_subset(blocking_events, selected_report())
    if (is.null(d)) return(NULL)
    if ("Block_Outcome" %in% names(d)) {
      tab <- as.data.frame(table(d$Block_Outcome), stringsAsFactors = FALSE)
      names(tab) <- c("BLOCK OUTCOME", "BLOCKS")
      tab
    } else d
  }, striped = TRUE, bordered = FALSE, rownames = FALSE)
}

shinyApp(ui = ui, server = server)
