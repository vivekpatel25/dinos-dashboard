# ============================================================
# DINOS COACHES DASHBOARD
# Phase 1: Key Metrics + Athletes + Match Reports
# CALGARY | CANADA WEST | 2026/27
# ============================================================

library(shiny)
library(bslib)

# ----------------------------- PATHS -----------------------------

MATCH_ROOT <- "data/exports/matches"
ROSTER_PATH <- "data/reference/UOC_MASTER_ROSTER.csv"

# Efficiency metrics are ratios, not percentages.
is_efficiency_metric <- function(x) {
  grepl("efficiency|\\beff\\b|atk eff", x, ignore.case = TRUE)
}

normalise_efficiency <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  # Percentage-scale values (e.g. 13.8) become ratios (0.138).
  # Existing decimal ratios (e.g. 0.138) remain unchanged.
  idx <- !is.na(x) & abs(x) > 1
  x[idx] <- x[idx] / 100
  x
}

# ----------------------------- DATA LOADING -----------------------------

read_csv_safe <- function(path) {
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
             na.strings = c("", "NA")),
    error = function(e) {
      warning(paste("Could not read", path, "-", conditionMessage(e)))
      NULL
    }
  )
}

find_key_metric_files <- function() {
  if (!dir.exists(MATCH_ROOT)) return(character(0))
  dashboard_files <- list.files(
    MATCH_ROOT,
    pattern = "^MATCH_DASHBOARD_SUMMARY\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(dashboard_files)) return(dashboard_files)

  # Backward-compatible fallback only when dashboard summaries are absent.
  list.files(
    MATCH_ROOT,
    pattern = "KEY[_ -]?METRICS.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}

extract_match_label <- function(path, data) {
  date <- if ("match_date" %in% names(data)) {
    as.character(data$match_date[1])
  } else NA_character_

  opponent <- if ("opponent" %in% names(data)) {
    unique(as.character(data$opponent))
  } else character(0)

  opponent <- opponent[!is.na(opponent) & nzchar(opponent)]
  opponent <- if (length(opponent)) opponent[1] else "Opponent"

  if (is.na(date) || !nzchar(date)) {
    folder <- basename(dirname(path))
    date <- sub(".*?(\\d{4}-\\d{2}-\\d{2}).*", "\\1", folder)
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) date <- NA_character_
    if (is.na(date)) date <- folder
  }

  date_obj <- suppressWarnings(as.Date(date))
  date_label <- if (!is.na(date_obj)) {
    format(date_obj, "%b %d, %Y")
  } else date

  list(date = date, date_label = date_label, opponent = opponent)
}

load_match_metrics <- function() {
  files <- find_key_metric_files()
  if (!length(files)) return(data.frame())

  out <- lapply(files, function(f) {
    d <- read_csv_safe(f)
    if (is.null(d) || !nrow(d)) return(NULL)

    required <- c("match_id", "match_date", "team", "opponent",
                  "metric_category", "metric_name", "metric_value")
    if (!all(required %in% names(d))) {
      warning(paste("Skipping key metrics file with unexpected columns:", f))
      return(NULL)
    }

    # Each dashboard-ready match CSV is stored inside its own match folder.
    # Legacy KEY_METRICS exports at the root remain supported as a fallback.
    if (!"match_result" %in% names(d)) d$match_result <- NA_character_
    if (!"match_set_score" %in% names(d)) d$match_set_score <- NA_character_
    d$source_path <- f
    d$match_folder <- if (basename(f) == "MATCH_DASHBOARD_SUMMARY.csv") {
      basename(dirname(f))
    } else {
      tools::file_path_sans_ext(basename(f))
    }
    d
  })

  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(data.frame())
  all_data <- do.call(rbind, out)
  all_data$match_date <- as.character(all_data$match_date)
  all_data
}

all_metrics <- load_match_metrics()

# ----------------------------- ROSTER -----------------------------

roster <- read_csv_safe(ROSTER_PATH)

find_col <- function(data, candidates) {
  hit <- candidates[tolower(candidates) %in% tolower(names(data))]
  if (!length(hit)) return(NA_character_)
  names(data)[match(tolower(hit[1]), tolower(names(data)))]
}

if (!is.null(roster) && nrow(roster)) {
  number_col <- find_col(roster, c("number", "player_number", "jersey"))
  name_col <- find_col(roster, c("name", "player_name", "athlete"))
  role_col <- find_col(roster, c("role", "position"))
  if (!anyNA(c(number_col, name_col, role_col))) {
    roster_view <- data.frame(
      Number = roster[[number_col]],
      Athlete = roster[[name_col]],
      Role = roster[[role_col]],
      stringsAsFactors = FALSE
    )
    roster_view <- roster_view[order(suppressWarnings(as.numeric(roster_view$Number)),
                                     na.last = TRUE), , drop = FALSE]
  } else {
    roster_view <- data.frame()
  }
} else {
  roster_view <- data.frame()
}

# ----------------------------- MATCH INDEX -----------------------------

if (nrow(all_metrics)) {
  # One index row per match, using Calgary's row so opponent is consistent.
  index_cols <- intersect(
    c("match_id", "match_date", "opponent", "match_result",
      "match_set_score", "sets_won", "match_folder"),
    names(all_metrics)
  )
  calgary_rows <- all_metrics[
    grepl("University of Calgary", all_metrics$team, ignore.case = TRUE),
    index_cols, drop = FALSE
  ]
  match_index <- unique(calgary_rows)
  match_index <- match_index[order(as.Date(match_index$match_date),
                                   decreasing = TRUE), , drop = FALSE]
  rownames(match_index) <- NULL
} else {
  match_index <- data.frame(
    match_id = character(), match_date = character(), opponent = character(),
    match_result = character(), match_set_score = character()
  )
}

# Optional metadata file remains supported, but the new match CSV already
# carries match_result and match_set_score.
MATCH_METADATA_PATH <- "data/exports/match_metadata.csv"
match_metadata <- read_csv_safe(MATCH_METADATA_PATH)
if (!is.null(match_metadata) && nrow(match_metadata) &&
    "match_id" %in% names(match_metadata)) {
  match_index <- merge(match_index, match_metadata, by = "match_id",
                       all.x = TRUE, sort = FALSE, suffixes = c("", ".meta"))
  match_index <- match_index[order(as.Date(match_index$match_date),
                                   decreasing = TRUE), , drop = FALSE]
  rownames(match_index) <- NULL
}

# ----------------------------- METRIC PREPARATION -----------------------------

get_team_rows <- function(match_id) {
  if (!nrow(all_metrics)) return(data.frame())
  d <- all_metrics[all_metrics$match_id == match_id, , drop = FALSE]

  # The uploaded CSV contains one row set for each team.
  # The dashboard always presents University of Calgary as the primary team.
  uoc <- d[grepl("University of Calgary", d$team, ignore.case = TRUE), ,
           drop = FALSE]
  opp <- d[!grepl("University of Calgary", d$team, ignore.case = TRUE), ,
           drop = FALSE]

  if (!nrow(uoc)) return(data.frame())
  list(uoc = uoc, opponent = opp)
}

metric_value_for_display <- function(value, metric_name) {
  if (is_efficiency_metric(metric_name)) {
    return(normalise_efficiency(value))
  }
  suppressWarnings(as.numeric(value))
}

metric_display_text <- function(value, metric_name) {
  if (is.na(value)) return("—")
  if (is_efficiency_metric(metric_name)) {
    return(sprintf("%.3f", normalise_efficiency(value)))
  }
  paste0(sprintf("%.1f", as.numeric(value)), "%")
}

metric_scale_value <- function(value, metric_name) {
  v <- metric_value_for_display(value, metric_name)
  if (is.na(v)) return(0)
  if (is_efficiency_metric(metric_name)) {
    return(max(0, min(1, v)))
  }
  max(0, min(100, v)) / 100
}

# Lower-is-better metrics. All other listed metrics are treated as higher-is-better.
lower_is_better <- function(metric_name) {
  grepl("aced|disruption", metric_name, ignore.case = TRUE)
}

metric_row_ui <- function(uoc_row, opp_row = NULL) {
  metric_name <- as.character(uoc_row$metric_name[1])
  category <- as.character(uoc_row$metric_category[1])
  uoc_raw <- as.numeric(uoc_row$metric_value[1])
  uoc_value <- metric_value_for_display(uoc_raw, metric_name)

  opp_raw <- NA_real_
  if (!is.null(opp_row) && nrow(opp_row)) {
    opp_raw <- as.numeric(opp_row$metric_value[1])
  }
  opp_value <- if (!is.na(opp_raw)) {
    metric_value_for_display(opp_raw, metric_name)
  } else NA_real_

  # Dashboard colour convention requested:
  # Aced % is the only red metric; every other metric uses green.
  bar_colour <- if (grepl("^Aced %$", metric_name, ignore.case = TRUE)) {
    "#E34D40"
  } else {
    "#159B69"
  }
  uoc_width <- metric_scale_value(uoc_raw, metric_name) * 100
  opp_left <- if (is.na(opp_raw)) NA_real_ else metric_scale_value(opp_raw, metric_name) * 100

  value_text <- metric_display_text(uoc_raw, metric_name)
  opp_text <- if (is.na(opp_raw)) "" else {
    paste0(as.character(opp_row$team[1]), " ", metric_display_text(opp_raw, metric_name))
  }

  div(
    class = "metric-line",
    div(class = "metric-description",
      div(class = "metric-name", metric_name),
      div(class = "metric-formula",
          if ("metric_formula" %in% names(uoc_row)) as.character(uoc_row$metric_formula[1]) else "")
    ),
    div(class = "metric-number", style = paste0("color:", bar_colour), value_text),
    div(class = "metric-track-wrap",
      div(class = "metric-track",
        div(class = "metric-fill",
            style = paste0("width:", uoc_width, "%;background:", bar_colour)),
        if (!is.na(opp_left)) {
          div(class = "opponent-marker",
              style = paste0("left:", opp_left, "%"))
        }
      ),
      if (nzchar(opp_text)) div(class = "opponent-label",
        style = paste0("left:", ifelse(is.na(opp_left), 0, opp_left), "%"),
        opp_text)
    )
  )
}

metrics_panel_ui <- function(data, title_suffix = NULL) {
  if (is.null(data) || !nrow(data)) {
    return(div(class = "empty-message", "No key metrics are available for this match."))
  }

  # Use the rows passed into this renderer. This also supports combined-period
  # data, where there is no single match_id to look up.
  uoc <- data[grepl("University of Calgary", data$team, ignore.case = TRUE), ,
              drop = FALSE]
  opp <- data[!grepl("University of Calgary", data$team, ignore.case = TRUE), ,
              drop = FALSE]

  if (!nrow(uoc)) {
    return(div(class = "empty-message",
               "University of Calgary metrics could not be identified in this file."))
  }

  categories <- unique(as.character(uoc$metric_category))
  tagList(
    lapply(categories, function(cat) {
      cat_rows <- uoc[uoc$metric_category == cat, , drop = FALSE]
      div(class = "metric-category",
        div(class = "metric-category-title", toupper(cat)),
        lapply(seq_len(nrow(cat_rows)), function(i) {
          opp_row <- opp[opp$metric_name == cat_rows$metric_name[i], , drop = FALSE]
          metric_row_ui(cat_rows[i, , drop = FALSE],
                        if (nrow(opp_row)) opp_row[1, , drop = FALSE] else NULL)
        })
      )
    })
  )
}

# ----------------------------- UI -----------------------------

ui <- fluidPage(
  tags$head(
    tags$title("Dinos Coaches Dashboard"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@500;600;700;800&family=Inter:wght@400;500;600;700&display=swap');
      :root{--dinos-red:#C8102E;--navy:#263746;--muted:#718096;--line:#dce4eb;}
      body{background:#edf1f5;color:var(--navy);font-family:Inter,Arial,sans-serif;}
      .container-fluid{padding:16px 22px 30px;}
      .top-header{background:#fff;border:1px solid #e0e6ec;border-radius:22px;
        min-height:145px;padding:22px 34px;display:flex;align-items:center;gap:28px;
        box-shadow:0 8px 22px rgba(31,49,67,.07);margin-bottom:22px;}
      .dinos-logo{width:155px;height:112px;object-fit:contain;}
      .brand-copy{flex:1;min-width:220px;}
      .brand-copy h1{font-family:'Barlow Condensed',sans-serif;font-size:54px;
        line-height:.95;font-weight:800;margin:0;color:#263746;}
      .brand-subtitle{font-size:15px;letter-spacing:3px;color:#728096;margin-top:16px;}
      .brand-subtitle b{color:var(--dinos-red);padding:0 7px;}
      .header-nav{display:flex;gap:5px;align-items:center;justify-content:center;flex:0 0 auto;}
      .header-nav .btn{border:0;background:transparent;color:#596b7d;padding:7px 8px;
        display:flex;flex-direction:column;align-items:center;justify-content:center;gap:3px;
        font-weight:800;font-family:'Barlow Condensed',sans-serif;letter-spacing:.7px;font-size:13px;
        width:82px;min-width:82px;min-height:66px;white-space:nowrap;}
      .header-nav .btn .glyphicon{display:block;font-size:21px;line-height:22px;margin:0;}
      .header-nav .btn span{display:block;text-align:center;line-height:15px;}
      .header-nav .btn .nav-icon{margin:0;}
      .header-nav .btn:hover,.header-nav .btn.active{color:var(--dinos-red);}
      .header-latest{border-left:1px solid #e6ebef;padding-left:20px;text-align:right;
        min-width:190px;font-size:11px;color:#718096;}
      .header-latest strong{display:block;color:var(--navy);font-size:12px;letter-spacing:.8px;}
      .header-latest .latest-date{font-family:'Barlow Condensed';font-size:18px;color:var(--navy);}
      .header-latest .latest-result{display:block;margin-top:4px;color:var(--dinos-red);
        font-size:12px;letter-spacing:.7px;}
      .page-heading{display:flex;align-items:end;justify-content:space-between;margin:18px 4px 16px;}
      .eyebrow{color:var(--dinos-red);font-weight:800;font-size:11px;letter-spacing:2px;}
      .page-heading h2{font-family:'Barlow Condensed';font-size:38px;font-weight:800;margin:3px 0;color:var(--navy);}
      .page-heading p{color:var(--muted);margin:0;font-size:14px;}
      .count-pill{border:1px solid #f3c4cc;background:#fff0f2;color:#b30e2b;
        border-radius:30px;padding:10px 15px;font-size:11px;font-weight:800;letter-spacing:.5px;}
      .white-panel{background:#fff;border:1px solid #dfe6ed;border-radius:18px;
        box-shadow:0 5px 17px rgba(31,49,67,.045);padding:24px;margin-bottom:18px;}
      .roster-table{width:100%;table-layout:fixed;border-collapse:collapse;}
      .roster-table th{background:#f4f6f8;color:#263746;text-transform:uppercase;
        font-size:13px;letter-spacing:.6px;padding:15px 18px;text-align:left;
        border-bottom:2px solid #d9e0e6;}
      .roster-table td{padding:14px 18px;border-bottom:1px solid #e5e9ed;font-size:15px;}
      .roster-table tr:nth-child(even){background:#fafbfc;}
      .roster-table th:first-child,.roster-table td:first-child{width:18%;text-align:center;}
      .roster-table th:nth-child(2),.roster-table td:nth-child(2){width:52%;}
      .roster-table th:nth-child(3),.roster-table td:nth-child(3){width:30%;}
      .roster-number{font-weight:800;color:var(--dinos-red);}
      .roster-role{text-transform:capitalize;color:#657486;}
      .period-bar{display:flex;gap:7px;background:#fff;border:1px solid #dfe6ed;
        border-radius:16px;padding:8px;box-shadow:0 5px 17px rgba(31,49,67,.045);margin:18px 0;}
      .period-bar .btn{flex:1;min-height:56px;border:0;background:transparent;color:#526579;
        font-family:'Barlow Condensed';font-size:17px;font-weight:800;border-radius:11px;}
      .period-bar .btn:hover{background:#fff0f2;color:var(--dinos-red);}
      .period-bar .btn.active{background:var(--dinos-red);color:#fff;}
      .metric-section{background:#fff;border:1px solid #dfe6ed;border-radius:17px;
        padding:22px 26px;margin:16px 0;box-shadow:0 4px 14px rgba(31,49,67,.04);}
      .metric-section-title{font-family:'Barlow Condensed';font-size:26px;font-weight:700;
        color:#5b6d7c;border-bottom:1px solid #dce4eb;padding-bottom:8px;margin-bottom:15px;}
      .metric-category{margin:16px 0 18px;}
      .metric-category-title{font-size:11px;font-weight:800;letter-spacing:1.4px;
        border-bottom:2px solid #334554;padding:0 0 9px;margin-bottom:0;color:#526475;}
      .metric-line{display:grid;grid-template-columns:minmax(190px,28%) 90px minmax(240px,1fr);
        gap:18px;align-items:center;min-height:62px;padding:10px 0;border-bottom:1px solid #e0e6eb;}
      .metric-name{font-family:'Barlow Condensed';font-weight:700;font-size:18px;color:#263746;}
      .metric-formula{font-size:10px;color:#7b8996;margin-top:2px;line-height:1.35;}
      .metric-number{font-family:'Barlow Condensed';font-size:28px;font-weight:800;text-align:right;}
      .metric-track-wrap{position:relative;padding-top:23px;padding-bottom:3px;}
      .metric-track{height:13px;border-radius:20px;background:#f3e9e2;position:relative;overflow:visible;}
      .metric-fill{height:13px;border-radius:20px;min-width:0;}
      /* Opponent tick and label share the same horizontal coordinate. */
      .opponent-marker{position:absolute;top:-7px;height:27px;width:3px;background:#ef4438;
        transform:translateX(-50%);z-index:3;border-radius:2px;}
      .opponent-label{position:absolute;top:2px;transform:translateX(-50%);
        white-space:nowrap;color:#e33e35;font-size:9px;line-height:11px;max-width:170px;
        overflow:hidden;text-overflow:ellipsis;text-align:center;z-index:4;}
      .empty-message{color:#7c8995;padding:18px 2px;font-size:13px;}
      .report-card{background:#fff;border:1px solid #dfe6ed;border-radius:15px;
        padding:18px 22px;margin:10px 0;display:flex;justify-content:space-between;align-items:center;}
      .report-card-title{font-weight:800;color:#263746;}
      .report-card-date{font-size:12px;color:#788797;margin-top:4px;}
      .btn-dinos{background:var(--dinos-red);color:#fff;border:0;border-radius:9px;
        padding:9px 17px;font-weight:800;}
      .btn-dinos:hover{background:#a80d27;color:#fff;}
      .back-btn{margin:2px 0 14px;}
      .footer{padding:20px;text-align:center;color:#9aa4ad;font-size:10px;letter-spacing:1.5px;}
      @media(max-width:900px){
        .top-header{flex-wrap:wrap;padding:18px;gap:12px}.dinos-logo{width:100px;height:80px;}
        .brand-copy h1{font-size:38px}.header-latest{order:4;width:100%;text-align:left;border-left:0;padding:8px 0;}
        .metric-line{grid-template-columns:minmax(130px,1fr) 75px minmax(120px,1.2fr);gap:9px;}
      }
      @media(max-width:600px){
        .container-fluid{padding:9px}.top-header{border-radius:14px;}
        .brand-copy h1{font-size:30px}.brand-subtitle{font-size:10px;letter-spacing:1px;}
        .header-nav{width:100%;justify-content:center;gap:4px}.header-nav .btn{width:78px;min-width:78px;font-size:12px}.period-bar{overflow-x:auto;}
        .period-bar .btn{min-width:112px;font-size:14px;}
        .metric-line{grid-template-columns:1fr 75px;}.metric-track-wrap{grid-column:1/-1;}
        .metric-section{padding:14px}.page-heading h2{font-size:32px;}
      }
    "))
  ),

  uiOutput("app_header"),
  uiOutput("page_body"),
  div(class = "footer", "DINOS COACHES DASHBOARD · CALGARY · CANADA WEST · 2026/27")
)

# ----------------------------- SERVER -----------------------------

server <- function(input, output, session) {
  page <- reactiveVal("dashboard")
  active_period <- reactiveVal("MOST RECENT MATCH")
  selected_match <- reactiveVal(NULL)

  observeEvent(input$nav_dashboard, { page("dashboard"); selected_match(NULL) })
  observeEvent(input$nav_athletes, { page("athletes") })
  observeEvent(input$nav_reports, { page("reports"); selected_match(NULL) })
  observeEvent(input$back_reports, { selected_match(NULL) })

  observeEvent(input$period_recent, active_period("MOST RECENT MATCH"))
  observeEvent(input$period_previous, active_period("PREVIOUS MATCH"))
  observeEvent(input$period_5, active_period("LAST 5"))
  observeEvent(input$period_10, active_period("LAST 10"))
  observeEvent(input$period_season, active_period("FULL SEASON"))

  output$app_header <- renderUI({
    latest <- if (nrow(match_index)) match_index[1, , drop = FALSE] else NULL
    latest_date <- if (!is.null(latest)) {
      format(as.Date(latest$match_date), "%b %d, %Y")
    } else "No match loaded"
    latest_opp <- if (!is.null(latest)) as.character(latest$opponent) else "Add match exports"
    result_value <- if (!is.null(latest) && "match_result" %in% names(latest)) {
      as.character(latest$match_result)
    } else NA_character_
    score_value <- if (!is.null(latest) && "match_set_score" %in% names(latest)) {
      as.character(latest$match_set_score)
    } else NA_character_
    latest_result <- if (!is.na(result_value) && nzchar(result_value)) {
      paste0(toupper(result_value),
             if (!is.na(score_value) && nzchar(score_value)) paste0(" · ", score_value) else "")
    } else "RESULT NOT AVAILABLE"

    div(class = "top-header",
      if (file.exists("www/dinos_logo.png")) {
        tags$img(src = "dinos_logo.png", class = "dinos-logo")
      } else div(class = "dinos-logo", "DINOS"),
      div(class = "brand-copy",
        h1("DINOS COACHES DASHBOARD"),
        div(class = "brand-subtitle",
          "CALGARY", tags$b("·"), "CANADA WEST", tags$b("·"), "2026/27")
      ),
      div(class = "header-nav",
        actionButton("nav_dashboard", label = tagList(
          tags$span(class = "glyphicon glyphicon-home nav-icon"),
          tags$span("DASHBOARD")),
          class = if (page() == "dashboard") "btn active" else "btn"),
        actionButton("nav_athletes", label = tagList(
          tags$span(class = "glyphicon glyphicon-user nav-icon"),
          tags$span("ATHLETES")),
          class = if (page() == "athletes") "btn active" else "btn"),
        actionButton("nav_reports", label = tagList(
          tags$span(class = "glyphicon glyphicon-file nav-icon"),
          tags$span("REPORTS")),
          class = if (page() == "reports") "btn active" else "btn")
      ),
      div(class = "header-latest",
        strong("LATEST MATCH"),
        div(class = "latest-date", latest_date),
        div(latest_opp),
        tags$strong(latest_result, class = "latest-result")
      )
    )
  })

  output$page_body <- renderUI({
    if (page() == "athletes") {
      if (!nrow(roster_view)) {
        return(tagList(
          div(class = "page-heading",
            div(div(class = "eyebrow", "TEAM ROSTER"), h2("ATHLETES"),
                p("Roster file missing or expected columns not found."))),
          div(class = "white-panel", "Check data/reference/UOC_MASTER_ROSTER.csv.")
        ))
      }

      return(tagList(
        div(class = "page-heading",
          div(div(class = "eyebrow", "TEAM ROSTER"), h2("ATHLETES"),
              p("University of Calgary · Women's Volleyball · 2026/27")),
          div(class = "count-pill", paste(nrow(roster_view), "ROSTERED ATHLETES"))
        ),
        div(class = "white-panel",
          tags$table(class = "roster-table",
            tags$thead(tags$tr(
              tags$th("NUMBER"), tags$th("ATHLETE"), tags$th("ROLE")
            )),
            tags$tbody(lapply(seq_len(nrow(roster_view)), function(i) {
              tags$tr(
                tags$td(class = "roster-number", roster_view$Number[i]),
                tags$td(roster_view$Athlete[i]),
                tags$td(class = "roster-role", roster_view$Role[i])
              )
            }))
          )
        )
      ))
    }

    if (page() == "reports") {
      if (!is.null(selected_match())) {
        return(tagList(
          actionButton("back_reports", "← ALL MATCH REPORTS", class = "btn btn-default back-btn"),
          uiOutput("selected_match_report")
        ))
      }

      if (!nrow(match_index)) {
        return(div(class = "white-panel",
          h2("MATCH REPORTS"),
          p("No MATCH_DASHBOARD_SUMMARY.csv files found. Check each match folder.")
        ))
      }

      return(tagList(
        div(class = "page-heading",
          div(div(class = "eyebrow", "MATCH LIBRARY"), h2("REPORTS"),
              p("Choose a match to open its individual key metrics report.")),
          div(class = "count-pill", paste(nrow(match_index), "MATCHES AVAILABLE"))
        ),
        lapply(seq_len(nrow(match_index)), function(i) {
          local({
            idx <- i
            div(class = "report-card",
              div(
                div(class = "report-card-title",
                    paste("CALGARY vs", match_index$opponent[idx])),
                div(class = "report-card-date",
                    paste0(format(as.Date(match_index$match_date[idx]), "%B %d, %Y"),
                      if (!is.na(match_index$match_result[idx]) && nzchar(as.character(match_index$match_result[idx])))
                        paste0(" · ", toupper(as.character(match_index$match_result[idx])),
                          if (!is.na(match_index$match_set_score[idx]) && nzchar(as.character(match_index$match_set_score[idx])))
                            paste0(" · ", match_index$match_set_score[idx]) else "") else ""))
              ),
              actionButton(paste0("open_match_", idx), "OPEN REPORT",
                           class = "btn-dinos")
            )
          })
        })
      ))
    }

    # Main team dashboard
    period_ids <- selected_match_ids()
    period_text <- switch(active_period(),
      "MOST RECENT MATCH" = "Most Recent Match",
      "PREVIOUS MATCH" = "Previous Match",
      "LAST 5" = "Last 5 Matches",
      "LAST 10" = "Last 10 Matches",
      "FULL SEASON" = "Full Season"
    )

    tagList(
      div(class = "period-bar",
        actionButton("period_recent", "MOST RECENT MATCH",
          class = if (active_period() == "MOST RECENT MATCH") "btn active" else "btn"),
        actionButton("period_previous", "PREVIOUS MATCH",
          class = if (active_period() == "PREVIOUS MATCH") "btn active" else "btn"),
        actionButton("period_5", "LAST 5",
          class = if (active_period() == "LAST 5") "btn active" else "btn"),
        actionButton("period_10", "LAST 10",
          class = if (active_period() == "LAST 10") "btn active" else "btn"),
        actionButton("period_season", "FULL SEASON",
          class = if (active_period() == "FULL SEASON") "btn active" else "btn")
      ),
      div(class = "page-heading",
        div(div(class = "eyebrow", "TEAM PERFORMANCE"),
            h2(period_text),
            p(if (length(period_ids)) {
              paste("Key metrics across", length(period_ids), "available match(es).")
            } else "No match data available for this period.")),
        div(class = "count-pill", paste(length(period_ids), "MATCH(ES) INCLUDED"))
      ),
      uiOutput("period_metrics")
    )
  })

  selected_match_ids <- reactive({
    if (!nrow(match_index)) return(character(0))
    ids <- as.character(match_index$match_id)
    switch(active_period(),
      "MOST RECENT MATCH" = ids[1],
      "PREVIOUS MATCH" = if (length(ids) >= 2) ids[2] else character(0),
      "LAST 5" = head(ids, 5),
      "LAST 10" = head(ids, 10),
      "FULL SEASON" = ids,
      ids[1]
    )
  })

  output$period_metrics <- renderUI({
    ids <- selected_match_ids()
    if (!length(ids)) return(div(class = "white-panel empty-message",
                                  "No MATCH_DASHBOARD_SUMMARY.csv data available for this selection."))

    # For a single match, show the opponent's tick marker beside each metric.
    if (length(ids) == 1) {
      return(div(class = "metric-section",
        div(class = "metric-section-title", "KEY METRICS · PERFORMANCE PROFILE"),
        metrics_panel_ui(all_metrics[all_metrics$match_id == ids[1], , drop = FALSE])
      ))
    }

    # For multiple matches, combine numerator/denominator for each team/metric.
    d <- all_metrics[all_metrics$match_id %in% ids, , drop = FALSE]
    if (!nrow(d)) return(div(class = "white-panel", "No match metrics found."))

    # Preserve category and metric order from the most recent match.
    reference_rows <- all_metrics[
      all_metrics$match_id == match_index$match_id[1], ,
      drop = FALSE
    ]
    category_order <- unique(as.character(reference_rows$metric_category))
    metric_order <- unique(as.character(reference_rows$metric_name))

    # Aggregate both Calgary and opponent numerator/denominator values.
    agg <- aggregate(
      cbind(numerator, denominator) ~ team + metric_category + metric_name,
      data = d,
      FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
    )
    agg$metric_value <- ifelse(
      agg$denominator > 0,
      agg$numerator / agg$denominator,
      NA_real_
    )
    # Percentages are numerator/denominator * 100; efficiencies remain ratios.
    is_pct <- !is_efficiency_metric(agg$metric_name)
    agg$metric_value[is_pct] <- 100 * agg$metric_value[is_pct]
    agg$match_id <- "aggregate"
    agg$metric_formula <- ""

    agg$metric_category <- factor(agg$metric_category, levels = category_order)
    agg$metric_name <- factor(agg$metric_name, levels = metric_order)
    agg <- agg[order(agg$metric_category, agg$metric_name), , drop = FALSE]
    agg$metric_category <- as.character(agg$metric_category)
    agg$metric_name <- as.character(agg$metric_name)

    div(class = "metric-section",
      div(class = "metric-section-title", "KEY METRICS · COMBINED PERIOD"),
      metrics_panel_ui(agg)
    )
  })

  output$selected_match_report <- renderUI({
    req(selected_match())
    id <- selected_match()
    row <- match_index[match_index$match_id == id, , drop = FALSE]
    date_text <- if (nrow(row)) format(as.Date(row$match_date[1]), "%B %d, %Y") else ""
    d <- all_metrics[all_metrics$match_id == id, , drop = FALSE]

    tagList(
      div(class = "page-heading",
        div(div(class = "eyebrow", "INDIVIDUAL MATCH REPORT"),
            h2(paste("CALGARY vs", if (nrow(row)) row$opponent[1] else "OPPONENT")),
            p(date_text)),
        div(class = "count-pill", "KEY METRICS")
      ),
      div(class = "metric-section",
        div(class = "metric-section-title", "KEY METRICS · PERFORMANCE PROFILE"),
        metrics_panel_ui(d)
      )
    )
  })

  observe({
    if (nrow(match_index)) {
      lapply(seq_len(nrow(match_index)), function(i) {
        local({
          idx <- i
          observeEvent(input[[paste0("open_match_", idx)]], {
            selected_match(as.character(match_index$match_id[idx]))
          }, ignoreInit = TRUE)
        })
      })
    }
  })
}

shinyApp(ui = ui, server = server)
