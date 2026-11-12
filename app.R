# ============================================================
# DINOS COACHES DASHBOARD
# CALGARY | CANADA WEST | 2026/27
# ============================================================

library(shiny)
library(bslib)

# ----------------------------- PATHS -----------------------------

MATCH_ROOT <- "data/exports/matches"
ROSTER_PATH <- "data/reference/UOC_MASTER_ROSTER.csv"

is_efficiency_metric <- function(x) {
  grepl("efficiency|\\beff\\b|atk eff", x, ignore.case = TRUE)
}

normalise_efficiency <- function(x) {
  x <- suppressWarnings(as.numeric(x))
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

  # Prefer original per-match KEY_METRICS exports: they retain the
  # numerator/denominator needed to validate percentage metrics.
  key_metric_files <- list.files(
    MATCH_ROOT,
    pattern = "KEY[_ -]?METRICS.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(key_metric_files)) return(key_metric_files)

  # Fallback only when source metric exports are unavailable.
  list.files(
    MATCH_ROOT,
    pattern = "^MATCH_DASHBOARD_SUMMARY\\.csv$",
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

    if (!"match_result" %in% names(d)) d$match_result <- NA_character_
    if (!"match_set_score" %in% names(d)) d$match_set_score <- NA_character_

    d$source_path <- f

    # Every export belongs to its game folder, for example:
    # data/exports/matches/2026-10-23_CAL_vs_UBC/KEY_METRICS.csv
    # Always store the parent folder, regardless of the CSV filename.
    d$match_folder <- basename(dirname(f))

    d
  })

  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(data.frame())

  all_data <- do.call(rbind, out)
  all_data$match_date <- as.character(all_data$match_date)
  all_data
}

all_metrics <- load_match_metrics()

# Match-level results are sourced from MATCH_DASHBOARD_SUMMARY.csv.
# KEY_METRICS.csv remains the source for detailed metric rows only.
find_match_summary_files <- function() {
  if (!dir.exists(MATCH_ROOT)) return(character(0))
  list.files(
    MATCH_ROOT,
    pattern = "^MATCH_DASHBOARD_SUMMARY\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}

load_match_summaries <- function() {
  files <- find_match_summary_files()
  if (!length(files)) return(data.frame())

  out <- lapply(files, function(f) {
    d <- read_csv_safe(f)
    if (is.null(d) || !nrow(d) ||
        !all(c("match_id", "team", "opponent") %in% names(d))) {
      return(NULL)
    }
    d$match_folder <- basename(dirname(f))
    d$summary_source_path <- f
    d
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(data.frame())
  z <- do.call(rbind, out)
  z$match_id <- as.character(z$match_id)
  z
}

match_summaries <- load_match_summaries()

# ----------------------------- SKILL AREA DATA -----------------------------

find_skill_area_files <- function() {
  if (!dir.exists(MATCH_ROOT)) return(character(0))

  list.files(
    MATCH_ROOT,
    pattern = "_SKILL_AREA\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}

load_skill_area_data <- function() {
  files <- find_skill_area_files()
  if (!length(files)) return(data.frame())

  parts <- lapply(files, function(f) {
    d <- read_csv_safe(f)

    if (is.null(d) || !nrow(d) ||
        !all(c("match_id", "team", "export_type", "section") %in% names(d))) {
      return(NULL)
    }

    d$source_path <- f
    d
  })

  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(data.frame())

  out <- do.call(rbind, parts)
  out$match_id <- as.character(out$match_id)
  out
}

all_skill_area <- load_skill_area_data()

skill_palette <- c(
  "Kills" = "#159B69",
  "In play" = "#9CB6C5",
  "Blocked" = "#F39A27",
  "Errors" = "#E34D40",
  "Good pass" = "#159B69",
  "Medium" = "#F4B12E",
  "Out-of-system" = "#F39A27",
  "Overpass" = "#F36B21",
  "Aced" = "#E34D40",
  "Stuff" = "#159B69",
  "Positive" = "#C45A00",
  "Fulfilled" = "#9CB6C5",
  "Negative" = "#F0A12C",
  "Aces" = "#08764F",
  "OOS Pass" = "#7CC7A5",
  "Good Pass" = "#9CB6C5",
  "Serve Error" = "#E34D40",
  "Dig Error" = "#E34D40"
)

skill_outcomes <- function(d, section) {
  x <- d[
    tolower(d$section) == tolower(section) &
      tolower(d$export_type) == "skill_outcome",
    ,
    drop = FALSE
  ]

  if (!nrow(x)) return(data.frame(label = character(), count = numeric()))

  x$count <- suppressWarnings(as.numeric(x$event_count))
  x$count[is.na(x$count)] <- 0

  code <- as.character(x$evaluation_code)

  x$label <- switch(
    tolower(section),

    attack = ifelse(
      code == "#", "Kills",
      ifelse(code %in% c("!", "+", "-"), "In play",
      ifelse(code == "/", "Blocked",
      ifelse(code == "=", "Errors", NA)))
    ),

    reception = ifelse(
      code %in% c("#", "+"), "Good pass",
      ifelse(code == "!", "Medium",
      ifelse(code == "-", "Out-of-system",
      ifelse(code == "/", "Overpass",
      ifelse(code == "=", "Aced", NA))))
    ),

    block = ifelse(
      code == "#", "Stuff",
      ifelse(code == "+", "Positive",
      ifelse(code == "!", "Fulfilled",
      ifelse(code == "-", "Negative",
      ifelse(code == "=", "Errors", NA))))
    ),

    serve = ifelse(
      code == "#", "Aces",
      ifelse(code == "/", "Overpass",
      ifelse(code == "+", "OOS Pass",
      ifelse(code == "-", "Good Pass",
      ifelse(code == "!", "Medium",
      ifelse(code == "=", "Serve Error", NA)))))
    ),

    dig = ifelse(
      code %in% c("+", "-", "/"), "In play",
      ifelse(code == "=", "Dig Error", NA)
    ),

    NA_character_
  )

  x <- x[!is.na(x$label), , drop = FALSE]

  if (!nrow(x)) return(data.frame(label = character(), count = numeric()))

  aggregate(count ~ label, x, sum)
}

skill_card <- function(title, tag, outcomes, bench = NULL,
                       opponent_bench = NULL, show_opponent = FALSE,
                       detail_text = NULL) {

  if (is.null(outcomes) || !nrow(outcomes) || sum(outcomes$count) <= 0) {
    return(
      div(
        class = "skill-card",
        div(class = "skill-card-head", span(title), span(tag)),
        div(class = "skill-card-empty", "No skill data available.")
      )
    )
  }

  outcomes <- outcomes[outcomes$count > 0, , drop = FALSE]
  total <- sum(outcomes$count)
  outcomes$pct <- 100 * outcomes$count / total

  cols <- unname(skill_palette[outcomes$label])
  cols[is.na(cols)] <- "#9CB6C5"

  ends <- cumsum(outcomes$pct)
  starts <- c(0, head(ends, -1))

  grad <- paste0(
    cols, " ",
    sprintf("%.2f%%", starts), " ",
    sprintf("%.2f%%", ends),
    collapse = ","
  )

  if (title == "ATTACK") {
    k <- sum(outcomes$count[outcomes$label == "Kills"])
    e <- sum(outcomes$count[outcomes$label == "Errors"])
    center <- sprintf("%.3f", (k - e) / total)
    center_label <- "ATK EFF"

  } else if (title == "SERVE RECEIVE") {
    gp <- sum(outcomes$count[outcomes$label == "Good pass"])
    center <- sprintf("%.0f%%", 100 * gp / total)
    center_label <- "GOOD"

  } else {
    center <- as.character(total)
    center_label <- switch(
      title,
      "BLOCK" = "BLOCKS",
      "SERVE" = "SERVES",
      "DEFENSE" = "DIGS"
    )
  }

  legend <- lapply(seq_len(nrow(outcomes)), function(i) {
    div(
      class = "skill-legend-row",
      span(class = "skill-swatch", style = paste0("background:", cols[i])),
      span(class = "skill-legend-name", outcomes$label[i]),
      strong(class = "skill-legend-pct", sprintf("%.0f%%", outcomes$pct[i])),
      span(class = "skill-legend-count", as.character(outcomes$count[i]))
    )
  })

  conversion_value <- NA_real_
  converted_n <- NA_real_
  in_play_n <- NA_real_

  if (title == "DEFENSE" && !is.null(bench) && nrow(bench)) {
    conversion_value <- suppressWarnings(as.numeric(bench$metric_value[1]))
    converted_n <- suppressWarnings(as.numeric(bench$numerator[1]))
    in_play_n <- suppressWarnings(as.numeric(bench$denominator[1]))
  }

  donut <- div(
    class = "skill-donut",
    style = paste0("background:conic-gradient(", grad, ");"),
    div(
      class = "skill-donut-hole",
      strong(center),
      tags$small(center_label)
    )
  )

  if (title == "DEFENSE" && is.finite(conversion_value)) {
    ring_pct <- max(0, min(100, conversion_value))

    donut <- div(
      class = "skill-conversion-ring",
      style = paste0(
        "background:conic-gradient(#159B69 0 ",
        ring_pct, "%, #e5ebf0 ", ring_pct, "% 100%);"
      ),
      div(class = "skill-conversion-ring-inner", donut)
    )

    conv_col <- "#159B69"

    legend <- c(
      legend,
      list(
        div(
          class = "skill-legend-row skill-converted-row",
          span(class = "skill-swatch", style = paste0("background:", conv_col)),
          span(class = "skill-legend-name", "Converted"),
          strong(
            class = "skill-legend-pct",
            sprintf("%.0f%%", conversion_value)
          ),
          span(
            class = "skill-legend-count",
            if (is.finite(converted_n)) as.character(round(converted_n)) else "—"
          )
        )
      )
    )
  }

  benchmark_ui <- NULL

  if (!is.null(bench) && nrow(bench) &&
      is.finite(suppressWarnings(as.numeric(bench$metric_value[1])))) {

    val <- max(0, min(100, as.numeric(bench$metric_value[1])))
    opp_val <- NA_real_
    opp_name <- "Opponent"

    if (isTRUE(show_opponent) && !is.null(opponent_bench) &&
        nrow(opponent_bench)) {

      opp_val <- suppressWarnings(as.numeric(opponent_bench$metric_value[1]))

      if ("team" %in% names(opponent_bench) &&
          !is.na(opponent_bench$team[1])) {
        opp_name <- as.character(opponent_bench$team[1])
      }
    }

    tick_ui <- if (isTRUE(show_opponent) && is.finite(opp_val)) {
      div(
        class = "opponent-marker skill-opponent-marker",
        style = paste0("left:", max(0, min(100, opp_val)), "%;"),
        title = paste0(opp_name, ": ", sprintf("%.1f%%", opp_val))
      )
    } else NULL

    benchmark_ui <- div(
      class = "skill-benchmark",
      span(toupper(bench$metric_name[1])),
      strong(sprintf("%.1f%%", as.numeric(bench$metric_value[1]))),
      div(
        class = "skill-benchmark-track",
        div(class = "skill-benchmark-fill", style = paste0("width:", val, "%;")),
        tick_ui
      ),
      if (isTRUE(show_opponent) && is.finite(opp_val)) {
        div(
          class = "skill-benchmark-label-wrap",
          div(
            class = "opponent-label skill-opponent-label",
            style = paste0("left:", max(0, min(100, opp_val)), "%;"),
            paste0(opp_name, " ", sprintf("%.1f%%", opp_val))
          )
        )
      }
    )
  }

  tagList(
    div(
      class = "skill-card",
      div(class = "skill-card-head", span(title), span(tag)),
      div(
        class = "skill-card-body",
        div(class = "skill-donut-wrap", donut),
        div(class = "skill-legend", legend)
      ),
      if (!is.null(detail_text) && nzchar(detail_text)) {
        div(class = "skill-detail", detail_text)
      },
      benchmark_ui
    )
  )
}

skill_area_panel_ui <- function(ids, show_opponent = FALSE) {

  if (!length(ids) || !nrow(all_skill_area)) {
    return(
      div(
        class = "skill-area-panel",
        div(class = "skill-area-heading", "BY SKILL AREA"),
        div(class = "skill-empty", "No *_SKILL_AREA.csv files found in the match folders.")
      )
    )
  }

  d <- all_skill_area[
    all_skill_area$match_id %in% as.character(ids) &
      grepl("University of Calgary", all_skill_area$team, ignore.case = TRUE),
    ,
    drop = FALSE
  ]

  if (!nrow(d)) {
    return(
      div(
        class = "skill-area-panel",
        div(class = "skill-area-heading", "BY SKILL AREA"),
        div(class = "skill-empty", "No University of Calgary skill-area data found for this period.")
      )
    )
  }

  specs <- list(
    list(section = "Attack", title = "ATTACK", tag = "KILL"),
    list(section = "Reception", title = "SERVE RECEIVE", tag = "PASS"),
    list(section = "Block", title = "BLOCK", tag = "NET"),
    list(section = "Serve", title = "SERVE", tag = "SRV"),
    list(section = "Dig", title = "DEFENSE", tag = "DEF")
  )

  cards <- lapply(specs, function(z) {

    out <- skill_outcomes(d, z$section)

    bn <- if (z$section == "Serve") {
      "Disruption %"
    } else if (z$section == "Dig") {
      "Conversion %"
    } else NA_character_

    b <- data.frame()
    ob <- data.frame()

    if (!is.na(bn)) {

      b0 <- all_skill_area[
        all_skill_area$match_id %in% as.character(ids) &
          all_skill_area$export_type == "benchmark" &
          all_skill_area$metric_name == bn &
          all_skill_area$benchmark_side == "UofC value",
        ,
        drop = FALSE
      ]

      if (nrow(b0)) {
        num <- sum(as.numeric(b0$numerator), na.rm = TRUE)
        den <- sum(as.numeric(b0$denominator), na.rm = TRUE)

        b <- data.frame(
          metric_name = bn,
          numerator = num,
          denominator = den,
          metric_value = if (den > 0) 100 * num / den else NA_real_
        )
      }

      if (isTRUE(show_opponent) && length(ids) == 1) {

        ob0 <- all_skill_area[
          all_skill_area$match_id %in% as.character(ids) &
            all_skill_area$export_type == "benchmark" &
            all_skill_area$metric_name == bn &
            all_skill_area$benchmark_side == "Opponent value",
          ,
          drop = FALSE
        ]

        if (nrow(ob0)) ob <- ob0[1, , drop = FALSE]
      }
    }

    n_for <- function(label) {
      sum(out$count[out$label == label], na.rm = TRUE)
    }

    detail <- switch(
      z$section,

      Attack = {
        swings <- sum(out$count, na.rm = TRUE)
        kills <- n_for("Kills")

        paste0(
          "Kill% ",
          if (swings > 0) sprintf("%.0f%%", 100 * kills / swings) else "—",
          " · ", swings, " swings"
        )
      },

      Reception = paste0(sum(out$count, na.rm = TRUE), " receptions"),

      Block = paste0(
        n_for("Stuff"), " stuffs · ",
        n_for("Errors"), " errors"
      ),

      Serve = paste0(
        sum(out$count, na.rm = TRUE), " serves · ",
        n_for("Aces"), " aces · ",
        n_for("Serve Error"), " errors"
      ),

      Dig = {
        converted <- if (nrow(b)) as.numeric(b$numerator[1]) else NA_real_
        playable <- if (nrow(b)) as.numeric(b$denominator[1]) else NA_real_

        if (is.finite(converted) && is.finite(playable)) {
          paste0(
            round(converted), " of ", round(playable),
            " in-play digs converted to a kill"
          )
        } else {
          "In-play dig conversion"
        }
      }
    )

    skill_card(
      z$title, z$tag, out, b, ob,
      show_opponent = show_opponent,
      detail_text = detail
    )
  })

  div(
    class = "skill-area-panel",
    div(class = "skill-area-heading", "BY SKILL AREA"),
    div(class = "skill-grid", cards)
  )
}

# ----------------------------- ROTATION + ATTACK DETAIL DATA -----------------------------

MATCH_DETAIL_ROOT <- MATCH_ROOT

match_folders_for_ids <- function(ids) {
  if (!length(ids) || !nrow(match_index)) return(character(0))

  rows <- match_index[
    as.character(match_index$match_id) %in% as.character(ids),
    , drop = FALSE
  ]

  folders <- if ("match_folder" %in% names(rows)) {
    as.character(rows$match_folder)
  } else {
    character(0)
  }

  folders <- folders[!is.na(folders) & nzchar(folders)]
  unique(folders)
}

match_dates_for_ids <- function(ids) {
  if (!length(ids) || !nrow(match_index)) return(character(0))

  rows <- match_index[
    as.character(match_index$match_id) %in% as.character(ids),
    , drop = FALSE
  ]

  dates <- as.character(rows$match_date)
  dates <- dates[!is.na(dates) & nzchar(dates)]
  unique(dates)
}

find_detail_files <- function(pattern, ids) {
  if (!length(ids) || !dir.exists(MATCH_DETAIL_ROOT)) {
    return(character(0))
  }

  files <- list.files(
    MATCH_DETAIL_ROOT,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (!length(files)) return(character(0))

  # Match by the containing game folder, not the CSV filename. This is
  # essential when files are named ROTATION_DETAILS.csv, TEAM_ATTACK.csv,
  # etc. inside folders such as 2026-10-23_CAL_vs_UBC.
  folders <- match_folders_for_ids(ids)

  if (length(folders)) {
    keep <- basename(dirname(files)) %in% folders
    selected <- files[keep]
    if (length(selected)) return(selected)
  }

  # Fallback for older exports where match_folder is unavailable:
  # identify the game folder using the match date in its path.
  dates <- match_dates_for_ids(ids)
  if (!length(dates)) return(character(0))

  files[vapply(files, function(f) {
    path_text <- gsub("-", "[-_]", normalizePath(f, winslash = "/", mustWork = FALSE))
    any(vapply(dates, function(dt) grepl(dt, path_text), logical(1)))
  }, logical(1))]
}

load_detail_csvs <- function(pattern, ids) {
  files <- find_detail_files(pattern, ids)

  if (!length(files)) return(data.frame())

  z <- lapply(files, read_csv_safe)
  z <- Filter(function(x) !is.null(x) && nrow(x), z)

  if (!length(z)) return(data.frame())

  do.call(rbind, z)
}

rotation_display_names <- c(
  SO_num = "SO WON", SO_den = "SO CHANCES", SO_pct = "SO%",
  FBSO_num = "FIRST-BALL SO WON", FBSO_den = "FIRST-BALL SO CHANCES", FBSO_pct = "FBSO%",
  PS_num = "POINTS SCORED", PS_den = "POINT SCORING CHANCES", PS_pct = "PS%",
  Reception_n = "RECEPTIONS", In_System_n = "IN-SYSTEM PASSES", In_System_pct = "IN SYS%",
  OOS_n = "OUT-OF-SYSTEM PASSES", OOS_pct = "OOS%", Aced_n = "ACES ALLOWED", Aced_pct = "ACED%",
  Total_Serves = "SERVES", Aces = "ACES", Opp_OOS = "OPP. OOS", Serve_Errors = "SERVE ERRORS",
  Serve_In_pct = "SERVE IN%", Attack_Attempts = "ATTACK ATTEMPTS", Kills = "KILLS",
  Attack_Errors = "ATTACK ERRORS", Attack_Efficiency = "ATTACK EFF",
  First_Ball_Attempts = "FB ATTEMPTS", First_Ball_Kills = "FB KILLS",
  First_Ball_Errors = "FB ERRORS", First_Ball_Efficiency = "FB EFF",
  Transition_Attempts = "ATTEMPTS", Transition_Kills = "KILLS",
  Transition_Errors = "ERRORS", Transition_Efficiency = "EFF",
  Digs = "DIGS", Dig_to_Kill_n = "DIG TO KILL", Dig_to_Kill_pct = "CONVERSION %", Dig_Errors = "DIG ERROR"
)

rotation_table_ui <- function(ids) {
  d <- load_detail_csvs("ROTATION_DETAILS\\.csv$", ids)
  if (!nrow(d) || !all(c("team", "rotation") %in% names(d))) return(NULL)

  d <- d[grepl("University of Calgary", d$team, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(d)) return(NULL)

  rot <- d[d$rotation %in% paste0("R", 1:6), , drop = FALSE]
  if (!nrow(rot)) return(NULL)

  metadata_cols <- c("match_id", "match_date", "source_file", "team", "rotation")
  numeric_cols <- names(rot)[vapply(rot, function(x) {
    is.numeric(x) || all(is.na(x) | grepl("^-?[0-9]+(\\.[0-9]+)?$", as.character(x)))
  }, logical(1))]
  numeric_cols <- setdiff(numeric_cols, metadata_cols)
  if (!length(numeric_cols)) return(NULL)

  for (nm in numeric_cols) rot[[nm]] <- suppressWarnings(as.numeric(rot[[nm]]))

  # Pool raw counts across selected matches; rates are averaged temporarily,
  # then recalculated from pooled numerators/denominators where possible.
  rate_cols <- numeric_cols[grepl("(_pct$|Efficiency$)", numeric_cols)]
  count_cols <- setdiff(numeric_cols, rate_cols)

  if (length(ids) > 1 || anyDuplicated(rot$rotation)) {
    count_part <- if (length(count_cols)) aggregate(
      rot[, count_cols, drop = FALSE], by = list(rotation = rot$rotation),
      FUN = function(x) sum(x, na.rm = TRUE)
    ) else data.frame(rotation = unique(rot$rotation))
    rate_part <- if (length(rate_cols)) aggregate(
      rot[, rate_cols, drop = FALSE], by = list(rotation = rot$rotation),
      FUN = function(x) mean(x, na.rm = TRUE)
    ) else data.frame(rotation = unique(rot$rotation))
    rot <- merge(count_part, rate_part, by = "rotation", all = TRUE, sort = FALSE)
  }

  calc <- function(n, den) ifelse(!is.na(den) & den > 0, 100 * n / den, NA_real_)
  rate_pairs <- list(
    SO_pct = c("SO_num", "SO_den"), FBSO_pct = c("FBSO_num", "FBSO_den"),
    PS_pct = c("PS_num", "PS_den"), In_System_pct = c("In_System_n", "Reception_n"),
    OOS_pct = c("OOS_n", "Reception_n"), Aced_pct = c("Aced_n", "Reception_n"),
    Serve_In_pct = c("Total_Serves", "Serve_Errors"),
    Dig_to_Kill_pct = c("Dig_to_Kill_n", "Digs", "Dig_Errors")
  )
  for (nm in names(rate_pairs)) {
    pair <- rate_pairs[[nm]]
    if (all(pair %in% names(rot))) {
      if (nm == "Serve_In_pct") rot[[nm]] <- calc(rot[[pair[1]]] - rot[[pair[2]]], rot[[pair[1]]])
      else if (nm == "Dig_to_Kill_pct") rot[[nm]] <- calc(rot[[pair[1]]], rot[[pair[2]]] - rot[[pair[3]]])
      else rot[[nm]] <- calc(rot[[pair[1]]], rot[[pair[2]]])
    }
  }
  efficiency_pairs <- list(
    Attack_Efficiency = c("Attack_Attempts", "Kills", "Attack_Errors"),
    First_Ball_Efficiency = c("First_Ball_Attempts", "First_Ball_Kills", "First_Ball_Errors"),
    Transition_Efficiency = c("Transition_Attempts", "Transition_Kills", "Transition_Errors")
  )
  for (nm in names(efficiency_pairs)) {
    pair <- efficiency_pairs[[nm]]
    if (all(pair %in% names(rot))) {
      rot[[nm]] <- ifelse(rot[[pair[1]]] > 0,
        (rot[[pair[2]]] - rot[[pair[3]]]) / rot[[pair[1]]], NA_real_)
    }
  }

  # AVG counts represent average volume per rotation; rates are pooled rates.
  avg <- rot[1, , drop = FALSE]
  avg[,] <- NA
  avg$rotation <- "AVG"
  for (nm in numeric_cols) avg[[nm]] <- mean(rot[[nm]], na.rm = TRUE)
  for (nm in names(rate_pairs)) {
    pair <- rate_pairs[[nm]]
    if (all(pair %in% names(rot))) {
      if (nm == "Serve_In_pct") avg[[nm]] <- calc(sum(rot[[pair[1]]]-rot[[pair[2]]], na.rm=TRUE), sum(rot[[pair[1]]], na.rm=TRUE))
      else if (nm == "Dig_to_Kill_pct") avg[[nm]] <- calc(sum(rot[[pair[1]]], na.rm=TRUE), sum(rot[[pair[2]]]-rot[[pair[3]]], na.rm=TRUE))
      else avg[[nm]] <- calc(sum(rot[[pair[1]]], na.rm=TRUE), sum(rot[[pair[2]]], na.rm=TRUE))
    }
  }
  for (nm in names(efficiency_pairs)) {
    pair <- efficiency_pairs[[nm]]
    if (all(pair %in% names(rot))) avg[[nm]] <- if (sum(rot[[pair[1]]], na.rm=TRUE) > 0)
      (sum(rot[[pair[2]]], na.rm=TRUE)-sum(rot[[pair[3]]], na.rm=TRUE))/sum(rot[[pair[1]]], na.rm=TRUE) else NA_real_
  }

  rot <- rbind(rot, avg)
  rot$rotation <- factor(rot$rotation, levels = c(paste0("R", 1:6), "AVG"))
  rot <- rot[order(rot$rotation), , drop = FALSE]

  # Display only the coach-requested rotation metrics, in the requested order.
  # Metrics absent from a particular CSV are safely omitted; unrelated CSV
  # columns are never added to the report.
  metric_order <- c(
    "SO_pct", "FBSO_pct", "PS_pct",
    "In_System_pct", "OOS_pct", "Aced_pct",
    "Aces", "Serve_Errors", "Serve_In_pct",
    "Attack_Attempts", "Kills", "Attack_Errors", "Attack_Efficiency",
    "First_Ball_Attempts", "First_Ball_Kills", "First_Ball_Errors", "First_Ball_Efficiency",
    "Transition_Attempts", "Transition_Kills", "Transition_Errors", "Transition_Efficiency",
    "Digs", "Dig_to_Kill_n", "Dig_to_Kill_pct", "Dig_Errors"
  )
  metric_cols <- metric_order[metric_order %in% names(rot)]
  if (!length(metric_cols)) return(NULL)
  display <- data.frame(ROTATION = as.character(rot$rotation), stringsAsFactors = FALSE)
  pct_cols <- metric_cols[grepl("_pct$", metric_cols)]
  eff_cols <- metric_cols[grepl("Efficiency$", metric_cols)]
  for (nm in metric_cols) display[[nm]] <- rot[[nm]]

  label_for <- function(nm) {
    if (nm %in% names(rotation_display_names)) return(unname(rotation_display_names[[nm]]))
    toupper(gsub("_", " ", nm))
  }
  names(display) <- c("ROTATION", vapply(metric_cols, label_for, character(1)))

  groups <- function(nm) {
    if (nm == "ROTATION") return("ROTATION")
    if (nm %in% c("SO_pct", "FBSO_pct", "PS_pct")) return("GAME PHASE")
    if (nm %in% c("In_System_pct", "OOS_pct", "Aced_pct")) return("RECEPTION & DEFENSE")
    if (nm %in% c("Aces", "Serve_Errors", "Serve_In_pct")) return("SERVE")
    if (nm %in% c("Attack_Attempts", "Kills", "Attack_Errors", "Attack_Efficiency")) return("ATTACK")
    if (nm %in% c("First_Ball_Attempts", "First_Ball_Kills", "First_Ball_Errors", "First_Ball_Efficiency")) return("FIRST BALL")
    if (nm %in% c("Transition_Attempts", "Transition_Kills", "Transition_Errors", "Transition_Efficiency")) return("TRANSITION")
    if (nm %in% c("Digs", "Dig_to_Kill_n", "Dig_to_Kill_pct", "Dig_Errors")) return("DEFENSE")
    "OTHER"
  }
  # Build one merged header cell for each consecutive metric group.
  # Using rle() guarantees repeated group labels are represented by a single
  # <th colspan="..."> rather than one header cell per metric.
  group_labels <- vapply(c("ROTATION", metric_cols), groups, character(1))
  group_runs <- rle(group_labels)
  group_cells <- lapply(seq_along(group_runs$values), function(i) {
    tags$th(
      group_runs$values[i],
      colspan = group_runs$lengths[i],
      class = "rotation-group-head"
    )
  })
  group_row <- tags$tr(group_cells)
  head_row <- tags$tr(lapply(names(display), function(nm) tags$th(nm)))

  # Heatmap ranks each metric across R1-R6. Error/negative metrics are reversed.
  error_cols <- metric_cols[grepl("Error|Aced|OOS", metric_cols, ignore.case = TRUE)]
  rows <- lapply(seq_len(nrow(display)), function(r) {
    is_avg <- display$ROTATION[r] == "AVG"
    cells <- lapply(seq_along(names(display)), function(j) {
      nm <- names(display)[j]
      if (j == 1L) return(tags$td(class = if (is_avg) "rot-avg-label" else "rot-label", display$ROTATION[r]))
      raw_nm <- metric_cols[j - 1L]
      v <- suppressWarnings(as.numeric(display[[nm]][r]))
      if (!is.finite(v)) return(tags$td(class = "rot-cell rot-neutral", "—"))
      is_pct <- raw_nm %in% pct_cols
      is_eff <- raw_nm %in% eff_cols
      if (is_pct) label <- sprintf("%.1f%%", v)
      else if (is_eff) label <- sprintf("%.3f", v)
      else if (abs(v - round(v)) < 1e-8) label <- format(round(v), big.mark = ",", scientific = FALSE)
      else label <- sprintf("%.1f", v)

      values <- suppressWarnings(as.numeric(display[[nm]][display$ROTATION != "AVG"]))
      values <- values[is.finite(values)]
      score <- if (length(values) <= 1L || length(unique(values)) <= 1L) 0.5 else {
        rank_value <- rank(values, ties.method = "average")
        rank_value[which(display$ROTATION[display$ROTATION != "AVG"] == display$ROTATION[r])[1]] / length(values)
      }
      if (raw_nm %in% error_cols) score <- 1 - score
      bg <- if (is_avg) "#e8edf1" else if (score >= .75) "#198754" else if (score >= .50) "#bfe8cf" else if (score >= .25) "#f6c36a" else "#f3c1c1"
      fg <- if (!is_avg && score >= .75) "#ffffff" else "#243447"
      tags$td(class = if (is_avg) "rot-avg-cell" else "rot-cell",
        style = paste0("background:", bg, ";color:", fg, ";"), label)
    })
    tags$tr(class = if (is_avg) "rotation-average-row" else "", cells)
  })

  div(class = "detail-section",
    div(class = "detail-section-title", "CALGARY ROTATION DETAILS"),
    div(class = "rotation-table-wrap",
      tags$table(class = "rotation-table rotation-table-full",
        tags$thead(group_row, head_row), tags$tbody(rows)
      )
    )
  )
}

# ----------------------------- ATTACK DETAIL -----------------------------

attack_detail_ui <- function(ids, selected_player = "All",
                             show_opponent = FALSE,
                             metric_data = all_metrics) {

  team <- load_detail_csvs("TEAM_ATTACK\\.csv$", ids)
  players <- load_detail_csvs("PLAYER_ATTACK\\.csv$", ids)

  if (!nrow(team) && !nrow(players)) {
    return(
      div(
        class = "detail-section",
        div(class = "detail-section-title", "ATTACK DETAIL"),
        div(class = "detail-empty", "No attack detail CSV found for this selection.")
      )
    )
  }

  is_player <- !identical(selected_player, "All") && nrow(players) > 0

  d <- if (is_player) {
    players[players$Player == selected_player, , drop = FALSE]
  } else {
    team
  }

  if (!nrow(d) && !is_player && nrow(players)) d <- players

  if (!nrow(d)) {
    return(
      div(
        class = "detail-section",
        div(class = "detail-section-title", "ATTACK DETAIL"),
        div(class = "detail-empty", "No attack records for this selection.")
      )
    )
  }

  group_col <- if ("Attack Type" %in% names(d)) "Attack Type" else names(d)[1]

  count_cols <- c(
    "ATT", "K", "E",
    "FB Attempts", "FB Kills", "FB Errors",
    "Transition Attempts", "Transition Kills", "Transition Errors"
  )

  for (nm in count_cols) {
    if (!nm %in% names(d)) d[[nm]] <- NA_real_
  }

  for (nm in count_cols) {
    d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
  }

  aggregate_counts <- function(x, group = group_col) {
    aggregate(
      x[, count_cols, drop = FALSE],
      by = setNames(list(x[[group]]), group),
      FUN = function(v) sum(v, na.rm = TRUE)
    )
  }

  if (length(ids) > 1 || anyDuplicated(d[[group_col]])) {
    d <- aggregate_counts(d)
  }

  safe_eff <- function(k, e, a) {
    ifelse(!is.na(a) & a > 0, (k - e) / a, NA_real_)
  }

  d$`Total Eff.` <- safe_eff(d$K, d$E, d$ATT)

  d$`FB Eff.` <- safe_eff(
    d$`FB Kills`, d$`FB Errors`, d$`FB Attempts`
  )

  d$`Transition Eff.` <- safe_eff(
    d$`Transition Kills`,
    d$`Transition Errors`,
    d$`Transition Attempts`
  )

  # Calgary team benchmark for player comparisons.
  team_counts <- team

  if (nrow(team_counts)) {
    for (nm in count_cols) {
      if (!nm %in% names(team_counts)) team_counts[[nm]] <- NA_real_
    }

    for (nm in count_cols) {
      team_counts[[nm]] <- suppressWarnings(as.numeric(team_counts[[nm]]))
    }
  }

  pooled_eff <- function(x, attempts, kills, errors) {
    a <- sum(x[[attempts]], na.rm = TRUE)

    if (a <= 0) {
      NA_real_
    } else {
      (sum(x[[kills]], na.rm = TRUE) -
         sum(x[[errors]], na.rm = TRUE)) / a
    }
  }

  team_bench <- if (nrow(team_counts)) {
    c(
      `FB Eff.` = pooled_eff(
        team_counts, "FB Attempts", "FB Kills", "FB Errors"
      ),
      `Transition Eff.` = pooled_eff(
        team_counts,
        "Transition Attempts",
        "Transition Kills",
        "Transition Errors"
      )
    )
  } else {
    c(`FB Eff.` = NA_real_, `Transition Eff.` = NA_real_)
  }

  # Opponent comparison values are read directly from the match KEY_METRICS.csv.
  # This deliberately does not use MATCH_DASHBOARD_SUMMARY.csv / all_metrics,
  # because that export may contain a different set of metric rows.
  opponent_metric <- function(kind) {

    if (!isTRUE(show_opponent) || length(ids) != 1) {
      return(NA_real_)
    }

    key_metrics <- load_detail_csvs("KEY_METRICS.*\\.csv$", ids)

    if (!nrow(key_metrics)) {
      return(NA_real_)
    }

    required_cols <- c("team", "metric_name", "metric_value")
    if (!all(required_cols %in% names(key_metrics))) {
      return(NA_real_)
    }

    # Keep only the selected match when the identifier is present.
    if ("match_id" %in% names(key_metrics)) {
      key_metrics <- key_metrics[
        as.character(key_metrics$match_id) %in% as.character(ids),
        ,
        drop = FALSE
      ]
    }

    if (!nrow(key_metrics)) {
      return(NA_real_)
    }

    # The opponent is the team row that is not University of Calgary.
    opponent_rows <- key_metrics[
      !grepl("University of Calgary", key_metrics$team, ignore.case = TRUE),
      ,
      drop = FALSE
    ]

    if (!nrow(opponent_rows)) {
      return(NA_real_)
    }

    metric_names <- tolower(trimws(as.character(opponent_rows$metric_name)))
    metric_names <- gsub("[^a-z0-9]+", " ", metric_names)
    metric_names <- trimws(metric_names)

    if (kind == "FB Eff.") {
      # Exact Key Metrics label: "First Ball Atk Eff"
      hit <- grepl("^first ball atk eff$", metric_names)
    } else {
      # Exact Key Metrics label: "Transition Atk Eff"
      hit <- grepl("^transition atk eff$", metric_names)
    }

    vals <- suppressWarnings(as.numeric(opponent_rows$metric_value[hit]))
    vals <- vals[is.finite(vals)]

    if (!length(vals)) {
      return(NA_real_)
    }

    # Key Metrics efficiencies are decimals (e.g. 0.295), while the
    # attack summary bars also use decimal efficiency values.
    normalise_efficiency(vals[1])
  }

  if (is_player) {

    bar_values <- c(
      `FB Eff.` = pooled_eff(
        d, "FB Attempts", "FB Kills", "FB Errors"
      ),
      `Transition Eff.` = pooled_eff(
        d,
        "Transition Attempts",
        "Transition Kills",
        "Transition Errors"
      )
    )

    bar_bench <- team_bench
    bench_label <- "CALGARY TEAM AVG."

  } else {

    bar_values <- c(
      `FB Eff.` = pooled_eff(
        team_counts, "FB Attempts", "FB Kills", "FB Errors"
      ),
      `Transition Eff.` = pooled_eff(
        team_counts,
        "Transition Attempts",
        "Transition Kills",
        "Transition Errors"
      )
    )

    bar_bench <- c(
      `FB Eff.` = opponent_metric("FB Eff."),
      `Transition Eff.` = opponent_metric("Transition Eff.")
    )

    bench_label <- "OPPONENT"
  }

  d <- d[order(-d$ATT), , drop = FALSE]

  fmt_eff <- function(x) {
    ifelse(!is.finite(x), "—", sprintf("%.3f", x))
  }

  # Attack-type table contains values only. No individual bars.
  rows <- lapply(seq_len(nrow(d)), function(i) {
    tags$tr(
      tags$td(
        class = "attack-type-cell",
        as.character(d[[group_col]][i])
      ),
      tags$td(ifelse(is.finite(d$ATT[i]), as.character(d$ATT[i]), "—")),
      tags$td(ifelse(is.finite(d$K[i]), as.character(d$K[i]), "—")),
      tags$td(ifelse(is.finite(d$E[i]), as.character(d$E[i]), "—")),
      tags$td(fmt_eff(d$`Total Eff.`[i])),
      tags$td(fmt_eff(d$`FB Eff.`[i])),
      tags$td(fmt_eff(d$`Transition Eff.`[i]))
    )
  })

  # Two summary bars below the table.
  # Efficiency bars use a shared 0.000 to 0.800 visual scale.
  # Both Calgary's fill and the opponent comparison tick use this same scale.
  # The scale is intentionally not printed in the dashboard.
  efficiency_position <- function(x) {
    if (length(x) == 0 || !is.finite(x)) return(NA_real_)
    max(0, min(100, as.numeric(x) / 0.8 * 100))
  }

  make_summary_bar <- function(metric, label, colour) {

    value <- unname(bar_values[metric])
    benchmark <- unname(bar_bench[metric])

    if (length(value) == 0 || !is.finite(value)) value <- NA_real_
    if (length(benchmark) == 0 || !is.finite(benchmark)) {
      benchmark <- NA_real_
    }

    width <- if (is.finite(value)) efficiency_position(value) else 0
    benchmark_position <- if (is.finite(benchmark)) {
      efficiency_position(benchmark)
    } else {
      NA_real_
    }

    tick <- if (is.finite(benchmark_position)) {
      tags$span(
        class = "attack-benchmark-tick",
        style = paste0("left:", benchmark_position, "%;"),
        title = paste0(
          bench_label, ": ", sprintf("%.3f", benchmark)
        ),
        `aria-label` = paste0(
          bench_label, " ", sprintf("%.3f", benchmark)
        )
      )
    } else NULL

    div(
      class = "attack-summary-metric",

      div(
        class = "attack-summary-heading",
        span(label),
        strong(fmt_eff(value))
      ),

      div(
        class = "attack-summary-track",
        div(
          class = "attack-summary-fill",
          style = paste0(
            "width:", width, "%;background:", colour, ";"
          )
        ),
        tick
      ),

      if (is.finite(benchmark_position)) {
        div(
          class = "attack-summary-benchmark",
          span(
            style = paste0("left:", benchmark_position, "%;"),
            paste0(
              bench_label, " ", sprintf("%.3f", benchmark)
            )
          )
        )
      }
    )
  }

  title_text <- if (is_player) {
    paste0("ATTACK DETAIL · ", selected_player)
  } else {
    "ATTACK DETAIL · CALGARY TEAM"
  }

  div(
    class = "detail-section attack-detail-section",

    div(class = "detail-section-title", title_text),

    div(
      class = "attack-filter-row",
      selectInput(
        "attack_player_filter",
        "",
        choices = c("All", sort(unique(as.character(players$Player)))),
        selected = selected_player,
        width = "240px"
      )
    ),

    tags$table(
      class = "attack-detail-table",
      tags$thead(
        tags$tr(
          lapply(
            c(
              "ATTACK TYPE", "ATT", "K", "E",
              "TOTAL EFF.", "FIRST-BALL EFF.", "TRANSITION EFF."
            ),
            tags$th
          )
        )
      ),
      tags$tbody(rows)
    ),

    div(
      class = "attack-summary-bars",
      make_summary_bar(
        "FB Eff.",
        "FIRST-BALL ATTACK EFFICIENCY",
        "#159B69"
      ),
      make_summary_bar(
        "Transition Eff.",
        "TRANSITION ATTACK EFFICIENCY",
        "#159B69"
      )
    ),

    NULL
  )
}


# ----------------------------- BLOCKING DETAIL -----------------------------

block_detail_ui <- function(ids, selected_attack_types = NULL) {

  d <- load_detail_csvs("BLOCK_EVENTS\\.csv$", ids)

  if (!nrow(d)) {
    return(
      div(
        class = "detail-section",
        div(class = "detail-section-title", "BLOCKING DETAIL"),
        div(class = "detail-empty", "No BLOCK_EVENTS.csv found for this match selection.")
      )
    )
  }

  required <- c("Opponent_Attack_Code", "Block_Outcome")
  if (!all(required %in% names(d))) {
    return(
      div(
        class = "detail-section",
        div(class = "detail-section-title", "BLOCKING DETAIL"),
        div(class = "detail-empty", "The block events export is missing attack-code or block-outcome columns.")
      )
    )
  }

  # Keep linked opponent attacks only when the export provides that flag.
  if ("Attack_Linked" %in% names(d)) {
    linked <- tolower(as.character(d$Attack_Linked)) %in% c("true", "1", "yes")
    d <- d[linked, , drop = FALSE]
  }

  code <- toupper(trimws(as.character(d$Opponent_Attack_Code)))

  # Exclude these attack codes completely from the blocking analysis.
  excluded_codes <- c("VV", "AX", "ZX", "Z3")

  # Translate opponent attack origin into the corresponding Calgary block position.
  d$Block_Position <- NA_character_
  d$Block_Position[code %in% c("XV", "X8", "C8")] <- "LEFT SIDE"
  d$Block_Position[code %in% c("X1", "XX")] <- "RIGHT SIDE"
  d$Block_Position[code == "C2"] <- "GAP"
  d$Block_Position[code == "V5"] <- "MIDDLE"

  # Every other C-coded attack is treated as a middle attack.
  other_c <- grepl("^C", code) & !code %in% c("C8", "C2")
  d$Block_Position[other_c] <- "MIDDLE"

  d <- d[
    !is.na(d$Block_Position) &
      !code %in% excluded_codes,
    ,
    drop = FALSE
  ]

  # Attack-type controls: all eligible codes are selected by default.
  attack_type_codes <- sort(unique(toupper(trimws(as.character(d$Opponent_Attack_Code)))))
  attack_type_labels <- c(
    "C2" = "C2 - Gap",
    "C5" = "C5",
    "C6" = "C6",
    "C8" = "C8 - Slide",
    "V5" = "V5 - Pipe",
    "X1" = "X1 - Hut",
    "XV" = "XV - Red",
    "X8" = "X8 - Five",
    "XX" = "XX - Go"
  )
  type_choices <- setNames(
    attack_type_codes,
    ifelse(
      attack_type_codes %in% names(attack_type_labels),
      unname(attack_type_labels[attack_type_codes]),
      attack_type_codes
    )
  )
  if (is.null(selected_attack_types)) selected_attack_types <- attack_type_codes
  selected_attack_types <- intersect(as.character(selected_attack_types), attack_type_codes)
  d <- d[toupper(trimws(as.character(d$Opponent_Attack_Code))) %in% selected_attack_types, , drop = FALSE]

  if (!nrow(d)) {
    return(
      div(
        class = "detail-section",
        div(class = "detail-section-title", "BLOCKING DETAIL"),
        div(class = "detail-empty", "No eligible opponent attack codes found for blocking analysis.")
      )
    )
  }

  outcome_levels <- c("Block Point", "Positive", "Negative", "Block Error")
  outcome_labels <- c(
    "Block Point" = "STUFFED",
    "Positive" = "POSITIVE",
    "Negative" = "NEGATIVE",
    "Block Error" = "ERROR"
  )

  outcome_colours <- c(
    "Block Point" = "#159B69",
    "Positive" = "#E0A21A",
    "Negative" = "#C94A35",
    "Block Error" = "#718096"
  )

  positions <- c("LEFT SIDE", "MIDDLE", "GAP", "RIGHT SIDE")

  d$Block_Outcome <- trimws(as.character(d$Block_Outcome))
  d$Block_Outcome[!d$Block_Outcome %in% outcome_levels] <- NA_character_
  d <- d[!is.na(d$Block_Outcome), , drop = FALSE]

  if (!nrow(d)) {
    return(
      div(
        class = "detail-section",
        div(class = "detail-section-title", "BLOCKING DETAIL"),
        div(class = "detail-empty", "No recognized block outcomes found in the eligible events.")
      )
    )
  }

  counts <- expand.grid(
    Block_Position = positions,
    Block_Outcome = outcome_levels,
    stringsAsFactors = FALSE
  )
  counts$count <- 0L

  observed <- as.data.frame(
    table(
      factor(d$Block_Position, levels = positions),
      factor(d$Block_Outcome, levels = outcome_levels)
    ),
    stringsAsFactors = FALSE
  )
  names(observed) <- c("Block_Position", "Block_Outcome", "count")
  counts <- merge(
    counts,
    observed,
    by = c("Block_Position", "Block_Outcome"),
    all.x = TRUE,
    suffixes = c("", ".observed"),
    sort = FALSE
  )
  counts$count <- ifelse(
    !is.na(counts$count.observed),
    counts$count.observed,
    counts$count
  )
  counts$count.observed <- NULL

  position_total <- vapply(
    positions,
    function(pos) sum(counts$count[counts$Block_Position == pos]),
    numeric(1)
  )
  grand_total <- sum(position_total)

  # Scale all position bars against the busiest position so low-volume zones
  # have shorter bars and high-volume zones have taller bars.
  max_position_total <- max(position_total, na.rm = TRUE)
  if (!is.finite(max_position_total) || max_position_total <= 0) {
    max_position_total <- 1
  }

  # Court lanes show opponent attack volume above the net and Calgary block outcomes below.
  court_lanes <- lapply(positions, function(pos) {
    total <- position_total[[pos]]
    pos_counts <- counts[counts$Block_Position == pos, , drop = FALSE]
    segments <- lapply(outcome_levels, function(outcome) {
      n <- pos_counts$count[pos_counts$Block_Outcome == outcome]
      if (!length(n) || n <= 0 || total <= 0) return(NULL)
      div(
        class = "block-stack-segment",
        style = paste0(
          "height:", 100 * n / max_position_total, "%;width:100%;background:",
          outcome_colours[[outcome]], ";"
        ),
        title = paste0(outcome_labels[[outcome]], ": ", n)
      )
    })

    div(
      class = "block-court-lane",
      div(class = "block-opponent-half",
        strong(as.character(total)),
        span(paste("OPP.", if (pos == "LEFT SIDE") "RIGHT" else if (pos == "RIGHT SIDE") "LEFT" else pos))
      ),
      div(class = "block-court-block-half",
        div(class = "block-court-stack", segments),
        div(class = "block-court-position", pos)
      )
    )
  })

  table_rows <- lapply(positions, function(pos) {
    total <- position_total[[pos]]
    pos_counts <- counts[counts$Block_Position == pos, , drop = FALSE]
    cells <- lapply(outcome_levels, function(outcome) {
      n <- pos_counts$count[pos_counts$Block_Outcome == outcome]
      n <- if (length(n)) n[1] else 0
      pct <- if (total > 0) 100 * n / total else 0
      tags$td(
        if (total > 0) {
          paste0(n, " (", sprintf("%.0f", pct), "%)")
        } else {
          "—"
        }
      )
    })

    tags$tr(
      tags$td(class = "block-position-name", pos),
      tags$td(as.character(total)),
      cells
    )
  })

  legend <- lapply(outcome_levels, function(outcome) {
    div(
      class = "block-legend-item",
      span(
        class = "block-legend-swatch",
        style = paste0("background:", outcome_colours[[outcome]], ";")
      ),
      span(outcome_labels[[outcome]])
    )
  })

  div(
    class = "detail-section block-detail-section",
    div(class = "detail-section-title", "BLOCKING DETAIL"),
    div(
      class = "block-type-filter",
      div(class = "block-type-filter-title", "INCLUDE ATTACK TYPES"),
      checkboxGroupInput(
        "block_attack_type_filter",
        label = NULL,
        choices = type_choices,
        selected = selected_attack_types,
        inline = TRUE
      )
    ),
    div(class = "block-detail-subtitle", paste0(grand_total, " eligible opponent attacks faced")),

    div(
      class = "block-court",
      div(class = "block-court-top-label", "OPPONENT ATTACKING SIDE"),
      div(class = "block-court-lanes", court_lanes),
      div(class = "block-court-net", "NET"),
      div(class = "block-court-bottom-label", "CALGARY BLOCKING SIDE")
    ),

    div(class = "block-legend", legend),

    div(
      class = "block-table-wrap",
      tags$table(
        class = "block-detail-table",
        tags$thead(
          tags$tr(
            tags$th("BLOCK POSITION"),
            tags$th("ATTACKS"),
            tags$th("STUFFED"),
            tags$th("POSITIVE"),
            tags$th("NEGATIVE"),
            tags$th("ERROR")
          )
        ),
        tags$tbody(table_rows)
      )
    ),

    NULL
  )
}


# ----------------------------- SERVING MAP -----------------------------

serving_map_ui <- function(ids, serve_from = "All", server_name = "All", selected_outcomes = c("Ace", "Positive", "Negative")) {

  d <- load_detail_csvs("SERVING_MAP_EVENTS\\.csv$", ids)

  section_title <- div(class = "detail-section-title", "SERVING MAP")

  if (!nrow(d)) {
    return(div(
      class = "detail-section serving-map-section",
      section_title,
      div(class = "detail-empty", "No SERVING_MAP_EVENTS.csv found for this match selection.")
    ))
  }

  required <- c(
    "Server_Name", "Player_Number", "Serve_Start_Zone",
    "Landing_Zone", "Landing_Subzone", "Serve_Outcome",
    "Serve_Evaluation_Code", "Set_Number"
  )
  missing <- setdiff(required, names(d))
  if (length(missing)) {
    return(div(
      class = "detail-section serving-map-section",
      section_title,
      div(class = "detail-empty",
          paste("Serving export is missing required columns:", paste(missing, collapse = ", ")))
    ))
  }

  d$Serve_Outcome <- trimws(as.character(d$Serve_Outcome))
  d$Server_Name <- trimws(as.character(d$Server_Name))
  d$Serve_Start_Zone <- suppressWarnings(as.integer(d$Serve_Start_Zone))
  d$Landing_Zone <- suppressWarnings(as.integer(d$Landing_Zone))
  d$Landing_Subzone <- toupper(trimws(as.character(d$Landing_Subzone)))
  d$Set_Number <- suppressWarnings(as.integer(d$Set_Number))

  # Use the five explicitly defined DVW serve evaluation codes as the source of truth:
  # # = Ace, + = OOS, ! = Medium, - = Negative/free attack, = = Serve error.
  # The sample export can also contain "/" (Positive, no attack); keep it visible as
  # a separate mix segment, but do not silently count it as OOS.
  d$Serve_Evaluation_Code <- trimws(as.character(d$Serve_Evaluation_Code))
  serve_code_labels <- c(
    "#" = "Ace",
    "+" = "OOS",
    "!" = "Medium",
    "-" = "Negative – Free Attack",
    "=" = "Error",
    "/" = "No Attack (/)"
  )
  outcome_colours <- c(
    "Ace" = "#D9433A",
    "OOS" = "#159B69",
    "Medium" = "#79BFA0",
    "Negative – Free Attack" = "#F0782B",
    "Error" = "#AAB5C0",
    "No Attack (/)" = "#9AA7B2"
  )

  # Preserve all rows for outcome cards and starting-zone summary, including errors.
  d_summary_source <- d
  d$Outcome_Display <- unname(serve_code_labels[d$Serve_Evaluation_Code])
  d <- d[!is.na(d$Outcome_Display), , drop = FALSE]
  d$Outcome_Group <- ifelse(
    d$Outcome_Display == "Ace", "Ace",
    ifelse(d$Outcome_Display %in% c("OOS", "Medium"), "Positive",
           ifelse(d$Outcome_Display == "Negative – Free Attack", "Negative", "Exclude"))
  )
  outcome_filter_choices <- c("Ace", "Positive", "Negative")
  if (is.null(selected_outcomes)) selected_outcomes <- outcome_filter_choices
  selected_outcomes <- intersect(as.character(selected_outcomes), outcome_filter_choices)
  d <- d[d$Outcome_Group %in% selected_outcomes, , drop = FALSE]
  # Keep every classified serve in totals. If the export has a landing zone
  # but no subzone, plot it at the centre of that zone rather than dropping it.
  d <- d[d$Landing_Zone %in% 1:9, , drop = FALSE]
  d$Landing_Subzone[
    is.na(d$Landing_Subzone) | !d$Landing_Subzone %in% c("A", "B", "C", "D")
  ] <- "CENTER"

  if (!nrow(d)) {
    return(div(
      class = "detail-section serving-map-section",
      section_title,
      div(class = "detail-empty", "No serve events with valid landing zones and subzones were found.")
    ))
  }

  # Filters use values from the loaded match files.
  from_choices <- c("All", paste("Zone", sort(unique(d$Serve_Start_Zone[!is.na(d$Serve_Start_Zone)]))))
  server_choices <- c("All", sort(unique(d$Server_Name[!is.na(d$Server_Name) & nzchar(d$Server_Name)])))
  if (is.null(serve_from) || !serve_from %in% from_choices) serve_from <- "All"
  if (is.null(server_name) || !server_name %in% server_choices) server_name <- "All"
  if (serve_from != "All") {
    z <- suppressWarnings(as.integer(sub("^Zone ", "", serve_from)))
    d <- d[d$Serve_Start_Zone == z, , drop = FALSE]
  }
  if (server_name != "All") d <- d[d$Server_Name == server_name, , drop = FALSE]

  if (!nrow(d)) {
    return(div(
      class = "detail-section serving-map-section",
      section_title,
      div(
        class = "serve-filter-row",
        radioButtons("serve_from_filter", "SERVE FROM", choices = from_choices,
                     selected = serve_from, inline = TRUE),
        selectInput("serve_server_filter", "SERVER", choices = server_choices, selected = server_name, width = "210px"),
        checkboxGroupInput("serve_outcome_filter", "SHOW OUTCOMES",
                           choices = outcome_filter_choices, selected = selected_outcomes, inline = TRUE)
      ),
      div(class = "detail-empty", "No serves match the selected filters.")
    ))
  }

  # Five outcome cards follow the requested evaluation-code mapping.
  card_source <- d_summary_source
  card_source$Serve_Evaluation_Code <- trimws(as.character(card_source$Serve_Evaluation_Code))
  card_source$Serve_Start_Zone <- suppressWarnings(as.integer(card_source$Serve_Start_Zone))
  if (serve_from != "All") {
    z <- suppressWarnings(as.integer(sub("^Zone ", "", serve_from)))
    card_source <- card_source[card_source$Serve_Start_Zone == z, , drop = FALSE]
  }
  if (server_name != "All") {
    card_source <- card_source[card_source$Server_Name == server_name, , drop = FALSE]
  }
  card_levels <- c("Ace", "OOS", "Medium", "Negative", "Error")
  card_codes <- c("#", "+", "!", "-", "=")
  card_colours <- c(
    "Ace" = "#D9433A", "OOS" = "#159B69", "Medium" = "#79BFA0",
    "Negative" = "#F0782B", "Error" = "#AAB5C0"
  )
  card_total <- nrow(card_source)
  stat_cards <- lapply(seq_along(card_levels), function(i) {
    outcome <- card_levels[[i]]
    n <- sum(card_source$Serve_Evaluation_Code == card_codes[[i]], na.rm = TRUE)
    pct <- if (card_total > 0) 100 * n / card_total else 0
    div(
      class = "serve-stat-card",
      div(class = "serve-stat-top",
          span(class = "serve-stat-dot", style = paste0("background:", card_colours[[outcome]], ";")),
          span(outcome)),
      strong(as.character(n)),
      tags$small(paste0(sprintf("%.0f", pct), "%"))
    )
  })

  # The source provides landing zone + A/B/C/D subzone, not continuous X/Y.
  # Rotate the receiving court 180 degrees so the net is at the bottom:
  # back row 1-6-5, middle 9-8-7, front row 2-3-4.
  # Subzones are used to calculate endpoints but are not printed on the court.
  zone_layout <- list(
    "1" = c(row = 1, col = 1), "6" = c(row = 1, col = 2), "5" = c(row = 1, col = 3),
    "9" = c(row = 2, col = 1), "8" = c(row = 2, col = 2), "7" = c(row = 2, col = 3),
    "2" = c(row = 3, col = 1), "3" = c(row = 3, col = 2), "4" = c(row = 3, col = 3)
  )
  # A/B/C/D are rotated with the court. Coordinates are relative to each zone.
  subzone_xy <- list(
    "A" = c(x = 24, y = 24),
    "B" = c(x = 24, y = 76),
    "C" = c(x = 76, y = 76),
    "D" = c(x = 76, y = 24),
    "CENTER" = c(x = 50, y = 50)
  )

  # Serve origins are shown on the serving side below the net.
  # Standard back-row starts: 5 = left, 6 = middle, 1 = right.
  # Other observed start zones are placed in the corresponding lateral lane.
  serve_origin_x <- c("1" = 84, "5" = 16, "6" = 50, "7" = 28, "9" = 72)
  start_zone_text <- as.character(d$Serve_Start_Zone)

  landing_xy <- lapply(seq_len(nrow(d)), function(i) {
    zone <- as.character(d$Landing_Zone[i])
    sub <- d$Landing_Subzone[i]
    cell <- zone_layout[[zone]]
    xy <- subzone_xy[[sub]]
    if (is.null(cell) || is.null(xy)) return(NULL)

    x <- (cell[["col"]] - 1) * 100 / 3 + xy[["x"]] / 3
    y <- (cell[["row"]] - 1) * 80 / 3 + xy[["y"]] / 3 * 0.8
    # Tiny deterministic jitter separates repeated landing coordinates.
    jx <- ((i * 7) %% 9) - 4
    jy <- ((i * 11) %% 9) - 4
    c(x = max(1, min(99, x + jx)), y = max(1, min(79, y + jy)))
  })

  # SVG trajectory layer: each line begins at the actual serving-zone lane,
  # crosses the net, and ends at the A/B/C/D-derived landing coordinate.
  serve_lines <- lapply(seq_len(nrow(d)), function(i) {
    endpoint <- landing_xy[[i]]
    start_zone <- start_zone_text[i]
    x_start <- unname(serve_origin_x[start_zone])
    if (is.null(endpoint) || !length(x_start) || is.na(x_start)) return(NULL)

    tags$line(
      x1 = x_start, y1 = 88.5,
      x2 = endpoint[["x"]], y2 = endpoint[["y"]],
      stroke = "#aeb9c1", `stroke-width` = "0.28",
      `stroke-opacity` = "0.24"
    )
  })

  marker_nodes <- lapply(seq_len(nrow(d)), function(i) {
    endpoint <- landing_xy[[i]]
    if (is.null(endpoint)) return(NULL)

    colour <- outcome_colours[[d$Outcome_Display[i]]]
    title_text <- paste0(
      d$Server_Name[i], " (#", d$Player_Number[i], ")",
      " | Serve from Zone ", d$Serve_Start_Zone[i],
      " | Landing ", d$Landing_Zone[i], "-", d$Landing_Subzone[i],
      " | ", d$Outcome_Display[i]
    )

    tags$span(
      class = "serve-map-marker",
      title = title_text,
      `aria-label` = title_text,
      style = paste0(
        "left:", endpoint[["x"]], "%;top:", endpoint[["y"]], "%;",
        "background:", colour, ";"
      )
    )
  })

  zone_cells <- lapply(1:9, function(zone) {
    loc <- zone_layout[[as.character(zone)]]
    div(
      class = "serve-zone-cell",
      style = paste0(
        "left:", (loc[["col"]] - 1) * 100 / 3, "%;",
        "top:", (loc[["row"]] - 1) * 80 / 3, "%;"
      ),
      strong(as.character(zone))
    )
  })

  start_zones <- sort(unique(start_zone_text[!is.na(start_zone_text)]))
  origin_nodes <- lapply(start_zones, function(z) {
    x <- unname(serve_origin_x[z])
    if (!length(x) || is.na(x)) return(NULL)
    n <- sum(start_zone_text == z, na.rm = TRUE)
    div(
      class = "serve-origin",
      style = paste0("left:", x, "%;"),
      span(class = "serve-origin-dot"),
      strong(paste0("Z", z)),
      tags$small(as.character(n))
    )
  })

  # One summary row per actual serve-start zone (not one row per landing zone).
  # Evaluation-code counts: + = OOS only; ! = Medium; # = Ace; - = Negative; = = Error.
  summary_data <- d_summary_source
  summary_data$Serve_Outcome <- trimws(as.character(summary_data$Serve_Outcome))
  summary_data$Serve_Start_Zone <- suppressWarnings(as.integer(summary_data$Serve_Start_Zone))
  if (serve_from != "All") {
    z <- suppressWarnings(as.integer(sub("^Zone ", "", serve_from)))
    summary_data <- summary_data[summary_data$Serve_Start_Zone == z, , drop = FALSE]
  }
  if (server_name != "All") {
    summary_data <- summary_data[summary_data$Server_Name == server_name, , drop = FALSE]
  }

  start_zones_for_table <- sort(unique(summary_data$Serve_Start_Zone[
    !is.na(summary_data$Serve_Start_Zone)
  ]))

  percent_text <- function(n, total) {
    if (total <= 0) return("—")
    paste0(sprintf("%.0f", 100 * n / total), "%")
  }

  make_start_zone_row <- function(zone) {
    rows <- summary_data[summary_data$Serve_Start_Zone == zone, , drop = FALSE]
    n <- nrow(rows)
    code <- trimws(as.character(rows$Serve_Evaluation_Code))
    ace_n <- sum(code == "#", na.rm = TRUE)
    oos_n <- sum(code == "+", na.rm = TRUE)
    medium_n <- sum(code == "!", na.rm = TRUE)
    negative_n <- sum(code == "-", na.rm = TRUE)
    error_n <- sum(code == "=", na.rm = TRUE)
    no_attack_n <- sum(code == "/", na.rm = TRUE)
    in_n <- sum(code %in% c("#", "+", "!", "-", "/"), na.rm = TRUE)
    points_won <- if ("UOFC_Point" %in% names(rows)) {
      sum(suppressWarnings(as.numeric(rows$UOFC_Point)) == 1, na.rm = TRUE)
    } else if ("Point_Won_By" %in% names(rows)) {
      sum(grepl("University of Calgary", rows$Point_Won_By, ignore.case = TRUE), na.rm = TRUE)
    } else {
      NA_integer_
    }

    # Outcome mix bar uses mutually exclusive serve outcomes.
    mix <- c(
      Ace = ace_n,
      OOS = oos_n,
      Medium = medium_n,
      Negative = negative_n,
      Error = error_n,
      `No attack (/)` = no_attack_n
    )
    mix_colours <- c(
      Ace = "#D9433A", OOS = "#159B69", Medium = "#79BFA0",
      Negative = "#F0782B", Error = "#AAB5C0",
      `No attack (/)` = "#D6DDE3"
    )
    mix_bar <- div(
      class = "serve-breakdown-bar",
      lapply(names(mix), function(nm) {
        if (mix[[nm]] <= 0 || n <= 0) return(NULL)
        div(
          class = "serve-breakdown-segment",
          style = paste0("width:", 100 * mix[[nm]] / n, "%;background:", mix_colours[[nm]], ";"),
          title = paste0(nm, ": ", mix[[nm]])
        )
      })
    )

    tags$tr(
      tags$td(paste("Zone", zone), class = "serve-breakdown-label"),
      tags$td(as.character(n)),
      tags$td(mix_bar),
      tags$td(paste0(ace_n, " (", percent_text(ace_n, n), ")")),
      tags$td(paste0(oos_n, " (", percent_text(oos_n, n), ")")),
      tags$td(paste0(medium_n, " (", percent_text(medium_n, n), ")")),
      tags$td(paste0(negative_n, " (", percent_text(negative_n, n), ")")),
      tags$td(paste0(error_n, " (", percent_text(error_n, n), ")")),
      tags$td(paste0(in_n, " (", percent_text(in_n, n), ")")),
      tags$td(if (is.na(points_won)) "—" else
        paste0(points_won, " (", percent_text(points_won, n), ")"))
    )
  }

  start_zone_rows <- lapply(start_zones_for_table, make_start_zone_row)

  # Used by the heading above the map; calculate after all active filters.
  total_serves <- nrow(d)

  outcome_levels <- c("Ace", "OOS", "Medium", "Negative – Free Attack")
  legend <- lapply(outcome_levels, function(outcome) {
    div(
      class = "serve-legend-item",
      span(class = "serve-stat-dot", style = paste0("background:", outcome_colours[[outcome]], ";")),
      span(outcome)
    )
  })

  div(
    class = "detail-section serving-map-section",
    section_title,
    div(
      class = "serve-filter-row",
      radioButtons("serve_from_filter", "SERVE FROM", choices = from_choices,
                   selected = serve_from, inline = TRUE),
      selectInput("serve_server_filter", "SERVER", choices = server_choices, selected = server_name, width = "210px"),
      checkboxGroupInput("serve_outcome_filter", "SHOW OUTCOMES",
                         choices = outcome_filter_choices, selected = selected_outcomes, inline = TRUE)
    ),
    div(class = "block-detail-subtitle", paste(total_serves, "serves displayed")),
    div(
      class = "serve-map-layout",
      div(
        class = "serve-court-column",
        div(
          class = "serve-court",
          zone_cells,
          tags$svg(
            class = "serve-trajectory-layer",
            viewBox = "0 0 100 100",
            preserveAspectRatio = "none",
            serve_lines
          ),
          marker_nodes,
          div(class = "serve-court-net", "NET"),
          origin_nodes
        ),
        div(class = "serve-map-server-label", "SERVING TEAM")
      ),
      div(
        class = "serve-summary-column",
        div(class = "serve-legend", legend),
        div(class = "serve-server-summary-title", "SERVE OUTCOME BY STARTING ZONE"),
        div(
          class = "serve-table-wrap",
          tags$table(
            class = "serve-server-table serve-breakdown-table serve-start-zone-table",
            tags$thead(tags$tr(
              tags$th("SERVE FROM"),
              tags$th("N"),
              tags$th("OUTCOME MIX"),
              tags$th("ACE"),
              tags$th("OOS (+)"),
              tags$th("MEDIUM (!)"),
              tags$th("NEGATIVE (-)"),
              tags$th("ERROR (=)"),
              tags$th("IN"),
              tags$th("POINTS WON")
            )),
            tags$tbody(start_zone_rows)
          )
        ),
        div(class = "serve-outcome-cards", stat_cards)
      )
    )
  )
}


# ----------------------------- PASSING MAPS -----------------------------

pass_map_controls <- function(from_choices, pass_from, passer_choices, passer_name,
                              selected_categories, show_lines) {
  tagList(
    div(
      class = "pass-filter-row",
      radioButtons("pass_from_filter", "RECEPTION START ZONE",
                   choices = from_choices, selected = pass_from, inline = TRUE),
      selectInput("pass_passer_filter", "PASSER",
                  choices = passer_choices, selected = passer_name, width = "220px")
    ),
    div(
      class = "pass-toggle-panel",
      checkboxInput("pass_lines_toggle", "Passing lines",
                    value = isTRUE(show_lines)),
      checkboxInput("pass_show_good", "Good passes",
                    value = "Good passes" %in% selected_categories),
      checkboxInput("pass_show_oos", "OOS pass",
                    value = "OOS passes" %in% selected_categories),
      checkboxInput("pass_show_overpass", "Overpass",
                    value = "Overpasses" %in% selected_categories),
      checkboxInput("pass_show_errors", "Passing errors",
                    value = "Passing errors" %in% selected_categories)
    )
  )
}

passing_map_ui <- function(ids, pass_from = "All", passer_name = "All",
                           selected_categories = c("Good passes", "OOS passes",
                                                   "Overpasses", "Passing errors"),
                           show_lines = TRUE) {

  d <- load_detail_csvs("PASSING_MAP_EVENTS\\.csv$", ids)
  section_title <- div(class = "detail-section-title", "PASSING MAPS")

  if (!nrow(d)) {
    return(div(
      class = "detail-section passing-map-section",
      section_title,
      div(class = "detail-empty",
          "No PASSING_MAP_EVENTS.csv found for this match selection.")
    ))
  }

  required <- c(
    "Player_Name", "Reception_Start_Zone", "Reception_Start_Subzone",
    "Pass_Destination_Zone", "Pass_Destination_Subzone",
    "Reception_Evaluation_Code", "Pass_Quality"
  )
  missing <- setdiff(required, names(d))
  if (length(missing)) {
    return(div(
      class = "detail-section passing-map-section",
      section_title,
      div(class = "detail-empty",
          paste("Passing export is missing required columns:",
                paste(missing, collapse = ", ")))
    ))
  }

  d$Player_Name <- trimws(as.character(d$Player_Name))
  d$Reception_Start_Zone <- suppressWarnings(as.integer(d$Reception_Start_Zone))
  d$Pass_Destination_Zone <- suppressWarnings(as.integer(d$Pass_Destination_Zone))
  d$Reception_Start_Subzone <- trimws(as.character(d$Reception_Start_Subzone))
  d$Reception_Start_Subzone[
    is.na(d$Reception_Start_Subzone) | toupper(d$Reception_Start_Subzone) %in% c("", "NA", "N/A")
  ] <- ""
  d$Pass_Destination_Subzone <- toupper(trimws(as.character(d$Pass_Destination_Subzone)))
  d$Pass_Destination_Subzone[
    is.na(d$Pass_Destination_Subzone) | d$Pass_Destination_Subzone %in% c("", "NA", "N/A")
  ] <- ""
  d$Reception_Evaluation_Code <- trimws(as.character(d$Reception_Evaluation_Code))
  d$Pass_Quality <- trimws(as.character(d$Pass_Quality))

  # The sample CSV has no Reception_Start_Subzone values. When that field is
  # populated in future exports, use it; otherwise start at the centre of
  # the recorded reception-start zone.
  if (!"Pass_Category" %in% names(d)) d$Pass_Category <- NA_character_

  category_for_row <- function(i) {
    code <- d$Reception_Evaluation_Code[i]
    quality <- tolower(d$Pass_Quality[i])
    # These three outcomes do not have a reliable setter destination in the DVW.
    # They are plotted at the reception contact point only.
    if (code == "=") return("Passing errors")
    if (code == "/") return("Overpasses")
    if (code == "-") return("OOS passes")
    if (code == "!" || grepl("oos|limited", quality)) return("OOS passes")
    if (code %in% c("#", "+") || grepl("perfect|positive", quality)) return("Good passes")
    "OOS passes"
  }
  d$Pass_Map_Category <- vapply(seq_len(nrow(d)), category_for_row, character(1))

  category_colours <- c(
    "Good passes" = "#159B69",
    "OOS passes" = "#E6A11A",
    "Overpasses" = "#D9433A",
    "Passing errors" = "#D9433A"
  )

  from_choices <- c(
    "All",
    paste("Zone", sort(unique(d$Reception_Start_Zone[
      !is.na(d$Reception_Start_Zone)
    ])))
  )
  passer_choices <- c(
    "All",
    sort(unique(d$Player_Name[!is.na(d$Player_Name) & nzchar(d$Player_Name)]))
  )
  if (is.null(pass_from) || !pass_from %in% from_choices) pass_from <- "All"
  if (is.null(passer_name) || !passer_name %in% passer_choices) passer_name <- "All"
  if (is.null(selected_categories)) selected_categories <- names(category_colours)
  selected_categories <- intersect(as.character(selected_categories), names(category_colours))

  d_all <- d
  if (pass_from != "All") {
    selected_zone <- suppressWarnings(as.integer(sub("^Zone ", "", pass_from)))
    d <- d[d$Reception_Start_Zone == selected_zone, , drop = FALSE]
  }
  if (passer_name != "All") d <- d[d$Player_Name == passer_name, , drop = FALSE]

  # Keep the zone table independent of map-visibility toggles so its totals
  # continue to represent all selected reception attempts.
  summary_data <- d
  d <- d[d$Pass_Map_Category %in% selected_categories, , drop = FALSE]

  if (!nrow(d)) {
    return(div(
      class = "detail-section passing-map-section",
      section_title,
      pass_map_controls(from_choices, pass_from, passer_choices, passer_name,
                        selected_categories, show_lines),
      div(class = "detail-empty", "No passes match the selected filters.")
    ))
  }

  # Court orientation follows the supplied court diagram, with the net
  # running horizontally below the front row:
  # Front / nearest net: 4-3-2; middle: 7-8-9; back: 5-6-1.
  # Valid pass trajectories run from reception contact to recorded destination.
  # For codes =, / and -, the DVW does not provide a reliable setter destination.
  zone_layout <- list(
    "4" = c(row = 1, col = 1), "3" = c(row = 1, col = 2), "2" = c(row = 1, col = 3),
    "7" = c(row = 2, col = 1), "8" = c(row = 2, col = 2), "9" = c(row = 2, col = 3),
    "5" = c(row = 3, col = 1), "6" = c(row = 3, col = 2), "1" = c(row = 3, col = 3)
  )

  # Subzone coordinates are relative to each zone. The start-subzone field is
  # blank in the supplied sample, so start positions safely fall back to zone centre.
  subzone_xy <- list(
    "A" = c(x = 76, y = 76),
    "B" = c(x = 76, y = 24),
    "C" = c(x = 24, y = 24),
    "D" = c(x = 24, y = 76),
    "1" = c(x = 24, y = 24),
    "2" = c(x = 76, y = 24),
    "3" = c(x = 24, y = 76),
    "4" = c(x = 76, y = 76),
    "CENTER" = c(x = 50, y = 50)
  )

  zone_point <- function(zone, subzone = NA_character_) {
    loc <- zone_layout[[as.character(zone)]]
    if (is.null(loc)) return(NULL)
    sub <- toupper(trimws(as.character(subzone)))
    if (is.na(sub) || !nzchar(sub) || is.null(subzone_xy[[sub]])) sub <- "CENTER"
    xy <- subzone_xy[[sub]]
    x <- (loc[["col"]] - 1) * 100 / 3 + xy[["x"]] / 3
    y <- (loc[["row"]] - 1) * 100 / 3 + xy[["y"]] / 3
    c(x = max(1, min(99, x)), y = max(1, min(99, y)))
  }

  start_points <- lapply(seq_len(nrow(d)), function(i) {
    zone_point(d$Reception_Start_Zone[i], d$Reception_Start_Subzone[i])
  })
  end_points <- lapply(seq_len(nrow(d)), function(i) {
    zone_point(d$Pass_Destination_Zone[i], d$Pass_Destination_Subzone[i])
  })

  line_nodes <- if (isTRUE(show_lines)) {
    lapply(seq_len(nrow(d)), function(i) {
      # DVW does not provide a reliable setter destination for =, /, or -.
      # These outcomes must never receive a trajectory line.
      if (d$Reception_Evaluation_Code[i] %in% c("=", "/", "-")) return(NULL)
      a <- start_points[[i]]
      b <- end_points[[i]]
      if (is.null(a) || is.null(b)) return(NULL)
      tags$line(
        x1 = a[["x"]], y1 = a[["y"]],
        x2 = b[["x"]], y2 = b[["y"]],
        stroke = "#9BAAB5", `stroke-width` = "0.24",
        `stroke-opacity` = "0.30"
      )
    })
  } else NULL

  marker_nodes <- lapply(seq_len(nrow(d)), function(i) {
    code <- d$Reception_Evaluation_Code[i]
    no_destination <- code %in% c("=", "/", "-")
    endpoint <- if (no_destination) start_points[[i]] else end_points[[i]]
    if (is.null(endpoint)) return(NULL)

    # Exact requested marker styling:
    # = solid red, no white border
    # / red with white border
    # - orange with white border
    marker_class <- if (code == "=") {
      "marker-error"
    } else if (code == "/") {
      "marker-overpass"
    } else if (code == "-") {
      "marker-oos"
    } else {
      "marker-standard"
    }
    colour <- if (code == "-") "#E6A11A" else
      if (code %in% c("=", "/")) "#D9433A" else
      unname(category_colours[d$Pass_Map_Category[i]])

    tip <- paste0(
      d$Player_Name[i], " | Reception Z", d$Reception_Start_Zone[i],
      if (nzchar(d$Reception_Start_Subzone[i])) paste0("-", d$Reception_Start_Subzone[i]) else "",
      " | Code ", code, " | ", d$Pass_Quality[i],
      if (no_destination) " | Reception contact only; no setter destination recorded" else
        paste0(" | Destination Z", d$Pass_Destination_Zone[i],
               if (nzchar(d$Pass_Destination_Subzone[i])) paste0("-", d$Pass_Destination_Subzone[i]) else "")
    )
    div(
      class = paste("pass-map-marker", marker_class),
      title = tip,
      style = paste0(
        "left:", endpoint[["x"]], "%;top:", endpoint[["y"]], "%;",
        "background:", colour, ";"
      )
    )
  })

  zone_cells <- lapply(names(zone_layout), function(zone) {
    loc <- zone_layout[[zone]]
    div(
      class = "pass-zone-label",
      style = paste0(
        "left:", (loc[["col"]] - 1) * 100 / 3, "%;",
        "top:", (loc[["row"]] - 1) * 100 / 3, "%;"
      ),
      strong(zone)
    )
  })

  legend <- lapply(names(category_colours), function(nm) {
    div(
      class = "pass-legend-item",
      span(
        class = paste(
          "pass-legend-dot",
          if (nm == "Passing errors") "legend-error" else
            if (nm == "Overpasses") "legend-overpass" else
            if (nm == "OOS passes") "legend-oos" else "legend-standard"
        ),
        style = paste0("background:", category_colours[[nm]], ";")
      ),
      span(nm)
    )
  })

  # Zone table is grouped by reception START zone. All receptions count as
  # attempts, including aces/errors and overpasses with no recorded destination.
  start_zones <- sort(unique(summary_data$Reception_Start_Zone[
    !is.na(summary_data$Reception_Start_Zone)
  ]))

  reception_team <- if ("Reception_Team" %in% names(summary_data)) {
    as.character(summary_data$Reception_Team)
  } else if ("Player_Key" %in% names(summary_data)) {
    sub("::.*$", "", as.character(summary_data$Player_Key))
  } else {
    rep("University of Calgary", nrow(summary_data))
  }

  sideout_available <- "Point_Won_By" %in% names(summary_data) &&
    any(!is.na(summary_data$Point_Won_By) & nzchar(as.character(summary_data$Point_Won_By)))

  zone_summary_rows <- lapply(start_zones, function(zone) {
    z <- summary_data[summary_data$Reception_Start_Zone == zone, , drop = FALSE]
    n <- nrow(z)
    good <- sum(z$Pass_Map_Category == "Good passes", na.rm = TRUE)
    oos <- sum(z$Pass_Map_Category == "OOS passes", na.rm = TRUE)
    ace_overpass <- sum(z$Pass_Map_Category %in% c("Passing errors", "Overpasses"),
                        na.rm = TRUE)
    so <- if (sideout_available) {
      team_for_zone <- reception_team[summary_data$Reception_Start_Zone == zone]
      sum(as.character(z$Point_Won_By) == team_for_zone, na.rm = TRUE)
    } else {
      NA_integer_
    }

    # Compact stacked outcome bar, matching the serve-outcome table style.
    pct_good <- if (n > 0) 100 * good / n else 0
    pct_oos <- if (n > 0) 100 * oos / n else 0
    pct_ace <- if (n > 0) 100 * ace_overpass / n else 0
    outcome_bar <- div(
      class = "pass-outcome-bar",
      title = paste0(
        "Good: ", good, " | OOS: ", oos, " | Ace + OP: ", ace_overpass
      ),
      div(class = "pass-outcome-segment pass-good-segment",
          style = paste0("width:", pct_good, "%;")),
      div(class = "pass-outcome-segment pass-oos-segment",
          style = paste0("width:", pct_oos, "%;")),
      div(class = "pass-outcome-segment pass-error-segment",
          style = paste0("width:", pct_ace, "%;"))
    )

    tags$tr(
      tags$td(paste("Zone", zone)),
      tags$td(as.character(n)),
      tags$td(outcome_bar),
      tags$td(paste0(good, " (", round(pct_good), "%)")),
      tags$td(paste0(oos, " (", round(pct_oos), "%)")),
      tags$td(paste0(ace_overpass, " (", round(pct_ace), "%)")),
      tags$td(if (is.na(so)) "—" else as.character(so))
    )
  })

  div(
    class = "detail-section passing-map-section",
    section_title,
    pass_map_controls(from_choices, pass_from, passer_choices, passer_name,
                      selected_categories, show_lines),
    div(class = "block-detail-subtitle",
        paste(nrow(d), "passes displayed")),
    div(
      class = "pass-map-layout",
      div(
        class = "pass-court-column",
        div(
          class = "pass-court",
          zone_cells,
          tags$svg(
            class = "pass-trajectory-layer",
            viewBox = "0 0 100 100",
            preserveAspectRatio = "none",
            tags$circle(
              cx = "58", cy = "24", r = "24",
              fill = "#BFE8D8", `fill-opacity` = "0.38",
              stroke = "#54BE98", `stroke-width` = "0.45",
              `stroke-dasharray` = "1.4 1.1"
            ),
            line_nodes
          ),
          marker_nodes,
          div(class = "pass-court-net", "NET")
        ),
        div(class = "pass-court-caption", "RECEIVING TEAM")
      ),
      div(
        class = "pass-summary-column",
        div(class = "pass-boundary-key",
            span(class = "pass-boundary-swatch"),
            span("Good-pass boundary (7 ft from net)")),
        div(class = "pass-summary-title", "PASS OUTCOMES BY RECEPTION START ZONE"),
        div(
          class = "pass-summary-table-wrap",
          tags$table(
            class = "pass-summary-table",
            tags$thead(tags$tr(
              tags$th("ZONE"),
              tags$th("TOTAL PASSES ATT."),
              tags$th("OUTCOME MIX"),
              tags$th("GOOD PASS"),
              tags$th("OOS"),
              tags$th("ACE + OP"),
              tags$th("SO")
            )),
            tags$tbody(zone_summary_rows)
          )
        ),
        if (!sideout_available) {
          div(class = "pass-table-note", "SO not available in this CSV export.")
        } else NULL
      )
    )
  )
}

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

    roster_view <- roster_view[
      order(
        suppressWarnings(as.numeric(roster_view$Number)),
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]

  } else {
    roster_view <- data.frame()
  }

} else {
  roster_view <- data.frame()
}

# ----------------------------- MATCH INDEX -----------------------------

if (nrow(all_metrics) || nrow(match_summaries)) {

  index_cols <- c(
    "match_id", "match_date", "opponent",
    "match_result", "match_set_score", "sets_won", "match_folder"
  )

  if (nrow(all_metrics)) {
    metric_index_cols <- intersect(index_cols, names(all_metrics))
    calgary_rows <- all_metrics[
      grepl("University of Calgary", all_metrics$team, ignore.case = TRUE),
      metric_index_cols,
      drop = FALSE
    ]
  } else {
    summary_index_cols <- intersect(index_cols, names(match_summaries))
    calgary_rows <- match_summaries[
      grepl("University of Calgary", match_summaries$team, ignore.case = TRUE),
      summary_index_cols,
      drop = FALSE
    ]
  }

  match_index <- unique(calgary_rows)

  match_index <- match_index[
    order(as.Date(match_index$match_date), decreasing = TRUE),
    ,
    drop = FALSE
  ]

  # A match can appear in multiple export rows/files. The dashboard index
  # must contain exactly one row per match_id.
  match_index <- match_index[
    !duplicated(as.character(match_index$match_id)),
    ,
    drop = FALSE
  ]

  rownames(match_index) <- NULL

  # Fill result, score, opponent and date from the dedicated dashboard summary.
  if (nrow(match_summaries)) {
    summary_rows <- match_summaries[
      grepl("University of Calgary", match_summaries$team, ignore.case = TRUE),
      , drop = FALSE
    ]
    summary_rows <- summary_rows[!duplicated(as.character(summary_rows$match_id)), , drop = FALSE]
    summary_fields <- intersect(
      c("match_id", "match_date", "opponent", "match_result", "match_set_score", "sets_won", "match_folder"),
      names(summary_rows)
    )
    summary_rows <- summary_rows[, summary_fields, drop = FALSE]
    names(summary_rows)[names(summary_rows) != "match_id"] <- paste0(names(summary_rows)[names(summary_rows) != "match_id"], ".summary")
    match_index <- merge(match_index, summary_rows, by = "match_id", all = TRUE, sort = FALSE)
    for (nm in c("match_date", "opponent", "match_result", "match_set_score", "sets_won", "match_folder")) {
      sn <- paste0(nm, ".summary")
      if (sn %in% names(match_index)) {
        if (!nm %in% names(match_index)) match_index[[nm]] <- NA
        missing <- is.na(match_index[[nm]]) | !nzchar(as.character(match_index[[nm]]))
        match_index[[nm]][missing] <- match_index[[sn]][missing]
        match_index[[sn]] <- NULL
      }
    }
    match_index <- match_index[order(as.Date(match_index$match_date), decreasing = TRUE), , drop = FALSE]
    match_index <- match_index[!duplicated(as.character(match_index$match_id)), , drop = FALSE]
    rownames(match_index) <- NULL
  }

} else {

  match_index <- data.frame(
    match_id = character(),
    match_date = character(),
    opponent = character(),
    match_result = character(),
    match_set_score = character()
  )
}

MATCH_METADATA_PATH <- "data/exports/match_metadata.csv"

match_metadata <- if (file.exists(MATCH_METADATA_PATH)) {
  read_csv_safe(MATCH_METADATA_PATH)
} else {
  NULL
}

if (!is.null(match_metadata) && nrow(match_metadata) &&
    "match_id" %in% names(match_metadata)) {

  match_index <- merge(
    match_index,
    match_metadata,
    by = "match_id",
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", ".meta")
  )

  match_index <- match_index[
    order(as.Date(match_index$match_date), decreasing = TRUE),
    ,
    drop = FALSE
  ]

  # Metadata joins can also multiply rows if duplicate IDs exist there.
  match_index <- match_index[
    !duplicated(as.character(match_index$match_id)),
    ,
    drop = FALSE
  ]

  rownames(match_index) <- NULL
}

# ----------------------------- METRIC PREPARATION -----------------------------

get_team_rows <- function(match_id) {

  if (!nrow(all_metrics)) return(data.frame())

  d <- all_metrics[
    all_metrics$match_id == match_id,
    ,
    drop = FALSE
  ]

  uoc <- d[
    grepl("University of Calgary", d$team, ignore.case = TRUE),
    ,
    drop = FALSE
  ]

  opp <- d[
    !grepl("University of Calgary", d$team, ignore.case = TRUE),
    ,
    drop = FALSE
  ]

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

lower_is_better <- function(metric_name) {
  grepl("aced|disruption", metric_name, ignore.case = TRUE)
}

metric_row_ui <- function(uoc_row, opp_row = NULL, show_opponent = TRUE) {

  metric_name <- as.character(uoc_row$metric_name[1])
  category <- as.character(uoc_row$metric_category[1])
  uoc_raw <- as.numeric(uoc_row$metric_value[1])

  # Recalculate Aced % from the source counts to avoid stale summary values.
  if (grepl("^Aced %$", metric_name, ignore.case = TRUE) &&
      all(c("numerator", "denominator") %in% names(uoc_row))) {
    num <- suppressWarnings(as.numeric(uoc_row$numerator[1]))
    den <- suppressWarnings(as.numeric(uoc_row$denominator[1]))
    if (is.finite(num) && is.finite(den) && den > 0) {
      uoc_raw <- 100 * num / den
    }
  }

  uoc_value <- metric_value_for_display(uoc_raw, metric_name)

  opp_raw <- NA_real_

  if (isTRUE(show_opponent) && !is.null(opp_row) && nrow(opp_row)) {
    opp_raw <- as.numeric(opp_row$metric_value[1])
    if (grepl("^Aced %$", metric_name, ignore.case = TRUE) &&
        all(c("numerator", "denominator") %in% names(opp_row))) {
      num <- suppressWarnings(as.numeric(opp_row$numerator[1]))
      den <- suppressWarnings(as.numeric(opp_row$denominator[1]))
      if (is.finite(num) && is.finite(den) && den > 0) {
        opp_raw <- 100 * num / den
      }
    }
  }

  opp_value <- if (!is.na(opp_raw)) {
    metric_value_for_display(opp_raw, metric_name)
  } else {
    NA_real_
  }

  bar_colour <- if (
    grepl("^Aced %$", metric_name, ignore.case = TRUE)
  ) {
    "#E34D40"
  } else {
    "#159B69"
  }

  uoc_width <- metric_scale_value(uoc_raw, metric_name) * 100

  opp_left <- if (is.na(opp_raw)) {
    NA_real_
  } else {
    metric_scale_value(opp_raw, metric_name) * 100
  }

  value_text <- metric_display_text(uoc_raw, metric_name)

  opp_text <- if (is.na(opp_raw)) {
    ""
  } else {
    paste0(
      as.character(opp_row$team[1]), " ",
      metric_display_text(opp_raw, metric_name)
    )
  }

  div(
    class = "metric-line",

    div(
      div(class = "metric-name", metric_name),
      div(
        class = "metric-formula",
        if ("metric_formula" %in% names(uoc_row)) {
          as.character(uoc_row$metric_formula[1])
        } else {
          ""
        }
      )
    ),

    # Metric value now matches its bar colour.
    div(
      class = "metric-number",
      style = paste0("color:", bar_colour, ";"),
      value_text
    ),

    div(
      class = "metric-track-wrap",

      div(
        class = "metric-track",
        div(
          class = "metric-fill",
          style = paste0(
            "width:", uoc_width, "%;background:", bar_colour, ";"
          )
        ),

        if (!is.na(opp_left)) {
          div(
            class = "opponent-marker",
            style = paste0("left:", opp_left, "%;")
          )
        } else NULL
      ),

      if (!is.na(opp_left)) {
        div(
          class = "opponent-label",
          style = paste0("left:", opp_left, "%;"),
          opp_text
        )
      } else NULL
    )
  )
}

metric_section_ui <- function(data, show_opponent = TRUE) {

  if (is.null(data) || !nrow(data)) {
    return(
      div(
        class = "empty-message",
        "No key metrics are available for this match."
      )
    )
  }

  uoc <- data[
    grepl("University of Calgary", data$team, ignore.case = TRUE),
    ,
    drop = FALSE
  ]

  opp <- data[
    !grepl("University of Calgary", data$team, ignore.case = TRUE),
    ,
    drop = FALSE
  ]

  if (!nrow(uoc)) {
    return(
      div(
        class = "empty-message",
        "University of Calgary metrics could not be identified in this file."
      )
    )
  }

  categories <- unique(as.character(uoc$metric_category))

  tagList(
    lapply(categories, function(cat) {

      cat_rows <- uoc[
        uoc$metric_category == cat,
        ,
        drop = FALSE
      ]

      div(
        class = "metric-category",

        div(class = "metric-category-title", toupper(cat)),

        lapply(seq_len(nrow(cat_rows)), function(i) {

          opp_row <- opp[
            opp$metric_name == cat_rows$metric_name[i],
            ,
            drop = FALSE
          ]

          metric_row_ui(
            cat_rows[i, , drop = FALSE],
            if (nrow(opp_row)) opp_row[1, , drop = FALSE] else NULL,
            show_opponent = show_opponent
          )
        })
      )
    })
  )
}


# ----------------------------- ATTACK MAP DATA -----------------------------

find_attack_map_files <- function() {
  if (!dir.exists(MATCH_ROOT)) return(character(0))
  list.files(
    MATCH_ROOT,
    pattern = "ATTACK_MAP_EVENTS.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}

load_attack_map_events <- function(ids = NULL) {
  files <- find_attack_map_files()
  if (!length(files)) return(data.frame())

  out <- lapply(files, function(f) {
    d <- read_csv_safe(f)
    if (is.null(d) || !nrow(d)) return(NULL)

    # Accept the standard pipeline column names without changing source labels.
    required <- c("Match_ID", "Attack_Code", "Attack_Type", "Attack_Outcome",
                  "Evaluation", "Start_Zone", "End_Zone")
    if (!all(required %in% names(d))) return(NULL)

    d$source_path <- f
    d$match_folder <- basename(dirname(f))
    d
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(data.frame())
  d <- do.call(rbind, out)

  if (!is.null(ids) && length(ids)) {
    ids <- as.character(ids)
    folders <- character(0)
    if (exists("match_index", inherits = TRUE) && nrow(match_index) &&
        "match_id" %in% names(match_index) && "match_folder" %in% names(match_index)) {
      folders <- as.character(match_index$match_folder[
        as.character(match_index$match_id) %in% ids
      ])
    }
    keep <- as.character(d$Match_ID) %in% ids
    if (length(folders)) keep <- keep | as.character(d$match_folder) %in% folders
    d <- d[keep, , drop = FALSE]
  }

  if (!nrow(d)) return(d)
  d$Start_Zone <- suppressWarnings(as.numeric(d$Start_Zone))
  d$End_Zone <- suppressWarnings(as.numeric(d$End_Zone))
  d$Attack_Code <- trimws(as.character(d$Attack_Code))
  d$Attack_Type <- trimws(as.character(d$Attack_Type))
  d$Attack_Outcome <- trimws(as.character(d$Attack_Outcome))
  d$Evaluation <- trimws(as.character(d$Evaluation))
  d
}

ATTACK_MAP_DEFINITIONS <- list(
  right = list(title = "RIGHT SIDE", codes = c("XV", "X8"), labels = c("Red (XV)", "Five (X8)")),
  left = list(title = "LEFT SIDE", codes = c("XX", "X1"), labels = c("Go (XX)", "Hut (X1)")),
  gap_slide = list(title = "GAP & SLIDE", codes = c("C2", "C8"), labels = c("Gap (C2)", "Slide (C8)")),
  middle = list(title = "MIDDLE VARIATIONS", codes = c("C6", "C5", "CH"), labels = c("C6", "C5", "CH")),
  backrow = list(title = "BACK ROW ATTACKS", codes = c("V7"), labels = c("BIC (V7)", "Pipe"))
)

attack_map_ui <- function() {
  map_card <- function(key, def) {
    choices <- stats::setNames(def$codes, def$labels[seq_along(def$codes)])
    if (identical(key, "backrow")) choices <- c("BIC (V7)" = "V7", "Pipe" = "PIPE")
    tagList(
      div(class = "attack-map-card",
        div(class = "attack-map-heading", def$title),
        div(class = "attack-map-controls",
          div(class = "attack-type-toggles",
            lapply(seq_along(choices), function(i) {
              code_value <- unname(choices[i])
              checkboxInput(
                paste0("attack_toggle_", key, "_", code_value),
                names(choices)[i],
                value = TRUE
              )
            })
          ),
          checkboxInput(paste0("attack_paths_", key), "Attack paths", value = TRUE),
          radioButtons(paste0("attack_outcome_", key), NULL,
            choices = c("All" = "All", "Kills" = "Kills", "In play" = "In play"),
            selected = "All", inline = TRUE)
        ),
        plotOutput(paste0("attack_plot_", key), height = "340px"),
        uiOutput(paste0("attack_summary_", key))
      )
    )
  }
  tagList(
    div(class = "attack-maps-section",
      div(class = "attack-maps-title", "ATTACK MAPS"),
      div(class = "attack-map-legend",
        span(class = "legend-kill", "●  KILL"),
        span(class = "legend-inplay", "○  IN PLAY"),
        span(class = "legend-line", "—  ATTACK PATH"),
        span(class = "legend-touch", "■  BLOCK TOUCH"),
        span(class = "legend-stuffed", "■  STUFFED"),
        span(class = "legend-no-touch", "■  NO RECORDED BLOCK TOUCH")
      ),
      map_card("right", ATTACK_MAP_DEFINITIONS$right),
      map_card("left", ATTACK_MAP_DEFINITIONS$left),
      div(class = "attack-map-subsection-label", "MIDDLE ATTACKS"),
      map_card("gap_slide", ATTACK_MAP_DEFINITIONS$gap_slide),
      map_card("middle", ATTACK_MAP_DEFINITIONS$middle),
      div(class = "attack-map-subsection-label", "BACK ROW ATTACKS"),
      map_card("backrow", ATTACK_MAP_DEFINITIONS$backrow)
    )
  )
}

attack_map_outcome_class <- function(d) {
  outcome <- tolower(trimws(as.character(d$Attack_Outcome)))
  evaluation <- tolower(trimws(as.character(d$Evaluation)))
  ifelse(outcome == "kill", "Kill",
    ifelse(outcome %in% c("in play", "positive", "negative") |
             evaluation == "blocked for reattack", "In Play", "Other"))
}

attack_zone_xy <- function(zone, subzone = NULL, is_origin = FALSE) {
  # Court orientation follows the supplied scouting diagram:
  # far row 1-6-5, middle row 7-8-9, net row 2-3-4.
  x_map <- c(`1` = 0.5, `6` = 1.5, `5` = 2.5,
             `7` = 0.5, `8` = 1.5, `9` = 2.5,
             `2` = 0.5, `3` = 1.5, `4` = 2.5)
  y_map <- c(`1` = 2.5, `6` = 2.5, `5` = 2.5,
             `7` = 1.5, `8` = 1.5, `9` = 1.5,
             `2` = 0.5, `3` = 0.5, `4` = 0.5)
  z <- as.character(suppressWarnings(as.integer(zone)))
  x <- unname(x_map[z])
  y <- unname(y_map[z])

  if (!is_origin && !is.null(subzone)) {
    # Each zone is divided into four subzones, matching the diagram:
    # A = upper-left, B = lower-left, C = lower-right, D = upper-right.
    sz <- toupper(trimws(as.character(subzone)))
    dx <- ifelse(sz %in% c("A", "B"), -0.25,
          ifelse(sz %in% c("C", "D"), 0.25, 0))
    dy <- ifelse(sz %in% c("A", "D"), 0.25,
          ifelse(sz %in% c("B", "C"), -0.25, 0))
    x <- x + dx
    y <- y + dy
  }

  if (is_origin) {
    # Own zone 2 is the right-side approach; own zone 4 is the left-side approach.
    x <- 3 - x
    y <- rep(-0.18, length(zone))
  }
  list(x = x, y = y)
}

draw_attack_map <- function(d, title = "ATTACK MAP", show_paths = TRUE) {
  par(mar = c(0.6, 0.6, 1.8, 0.6), xaxs = "i", yaxs = "i")
  plot(NA, xlim = c(-0.35, 3.35), ylim = c(-0.35, 3.25),
       asp = 1, axes = FALSE, xlab = "", ylab = "", main = "")
  # The card header already names the map; keep the court graphic uncluttered.

  # Light run-off area and clean half-court. Zone numbers are intentionally
  # omitted; the CSV End_Zone values determine endpoint locations.
  rect(-0.18, -0.18, 3.18, 3.08, col = "#eaf1f5", border = NA)
  rect(0, 0, 3, 3, col = "#f7f9fa", border = "#344653", lwd = 2)
  segments(1, 0, 1, 3, col = "#d2dce3", lwd = 1.15)
  segments(2, 0, 2, 3, col = "#d2dce3", lwd = 1.15)
  segments(0, 1, 3, 1, col = "#d2dce3", lwd = 1.15)
  segments(0, 2, 3, 2, col = "#d2dce3", lwd = 1.15)
  # Fine dashed lines divide each numbered zone into A/B/C/D subzones.
  for (x_sub in c(0.5, 1.5, 2.5)) {
    segments(x_sub, 0, x_sub, 3,
             col = grDevices::adjustcolor("#d2dce3", alpha.f = 0.75),
             lwd = 0.7, lty = 3)
  }
  for (y_sub in c(0.5, 1.5, 2.5)) {
    segments(0, y_sub, 3, y_sub,
             col = grDevices::adjustcolor("#d2dce3", alpha.f = 0.75),
             lwd = 0.7, lty = 3)
  }
  # Net / front boundary
  segments(0, 0, 3, 0, col = "#344653", lwd = 4)
  # Side and end lines
  segments(0, 3, 3, 3, col = "#344653", lwd = 1.8)
  segments(0, 0, 0, 3, col = "#344653", lwd = 1.8)
  segments(3, 0, 3, 3, col = "#344653", lwd = 1.8)

  if (!nrow(d)) {
    text(1.5, 1.5, "No attack events for this selection",
         cex = 0.9, col = "#7b8996")
    return(invisible(NULL))
  }

  valid <- !is.na(d$Start_Zone) & !is.na(d$End_Zone) &
    d$End_Zone %in% 1:9 & d$Start_Zone %in% 1:9
  d <- d[valid, , drop = FALSE]
  if (!nrow(d)) {
    text(1.5, 1.5, "No valid start/end zones",
         cex = 0.9, col = "#7b8996")
    return(invisible(NULL))
  }

  start <- attack_zone_xy(d$Start_Zone, is_origin = TRUE)
  end <- attack_zone_xy(d$End_Zone, subzone = d$End_Subzone)
  cls <- attack_map_outcome_class(d)
  marker_fill <- ifelse(cls == "Kill", "#263746",
                 ifelse(cls == "In Play", "#159b70", "#df4b40"))
  marker_pch <- ifelse(cls == "Other", 4, 21)

  if (isTRUE(show_paths)) {
    segments(start$x, start$y, end$x, end$y,
             col = grDevices::adjustcolor("#52616b", alpha.f = 0.62),
             lwd = 1.15)
  }
  points(end$x, end$y, pch = marker_pch, bg = marker_fill,
         col = marker_fill, cex = 1.15, lwd = 1.5)
  # Attack origins are positioned by their actual Start_Zone.
  points(start$x, start$y, pch = 21, bg = "#C8102E",
         col = "white", cex = 0.85, lwd = 1)
  invisible(NULL)
}

attack_map_summary_ui <- function(d) {
  if (is.null(d) || !nrow(d)) return(div(class = "attack-map-summary", "No attack events found for this map."))
  outcome <- tolower(trimws(as.character(d$Attack_Outcome)))
  eval <- tolower(trimws(as.character(d$Evaluation)))
  kills <- sum(outcome == "kill", na.rm = TRUE)
  in_play <- sum(outcome %in% c("in play", "positive", "negative") |
                   eval == "blocked for reattack", na.rm = TRUE)
  stuffed <- sum(eval == "blocked", na.rm = TRUE)
  touched_in_play <- sum(eval == "blocked for reattack", na.rm = TRUE)
  block_touches <- stuffed + touched_in_play
  total <- nrow(d)
  efficiency <- if (total) {
    round((kills - sum(outcome == "error", na.rm = TRUE)) / total, 3)
  } else 0
  div(class = "attack-map-summary",
    div(class = "attack-summary-stats",
      div(span("SWINGS"), strong(total)),
      div(span("KILLS"), strong(kills)),
      div(span("IN PLAY"), strong(in_play)),
      div(span("ATTACK EFF."), strong(sprintf("%.3f", efficiency)))
    ),
    div(class = "attack-net-bar",
      div(class = "net-bar-title", "BLOCK CONTACT NEAR THE NET"),
      div(class = "net-bar-track",
        div(class = "net-bar-touch",
          style = paste0("width:", if (total) touched_in_play / total * 100 else 0, "%"),
          if (touched_in_play > 0) span(touched_in_play)
        ),
        div(class = "net-bar-stuffed",
          style = paste0("width:", if (total) stuffed / total * 100 else 0, "%"),
          if (stuffed > 0) span(stuffed)
        ),
        div(class = "net-bar-none",
          style = paste0("width:", if (total) max(0, total - block_touches) / total * 100 else 0, "%"),
          if (total - block_touches > 0) span(max(0, total - block_touches))
        )
      )
    )
  )
}

# ----------------------------- SETTING MAPS -----------------------------

# Reads the match-specific setting analysis exports. These files retain every
# setter event, including sets that could not be linked to an attack. Linked
# attacks are used only for attack outcome and efficiency calculations.
find_setting_analysis_files <- function(ids = NULL) {
  if (!dir.exists(MATCH_ROOT)) return(character(0))

  files <- list.files(
    MATCH_ROOT,
    pattern = "SETTING_ANALYSIS\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (!length(files)) return(character(0))
  if (is.null(ids) || !length(ids)) return(files)

  folders <- match_folders_for_ids(ids)
  if (length(folders)) {
    keep <- basename(dirname(files)) %in% folders
    selected <- files[keep]
    if (length(selected)) return(selected)
  }

  dates <- match_dates_for_ids(ids)
  if (!length(dates)) return(character(0))
  files[vapply(files, function(f) {
    any(vapply(dates, function(dt) grepl(gsub("-", "[_-]", dt), f), logical(1)))
  }, logical(1))]
}

load_setting_analysis <- function(ids = NULL) {
  files <- find_setting_analysis_files(ids)
  if (!length(files)) return(data.frame())

  out <- lapply(files, read_csv_safe)
  out <- Filter(function(d) !is.null(d) && nrow(d), out)
  if (!length(out)) return(data.frame())
  d <- do.call(rbind, out)

  required <- c(
    "Team", "Setter_Name", "Setter_Number", "Setting_Destination_Zone",
    "Setting_Destination_Subzone", "Setting_Phase", "Linked_Attack",
    "Attack_Kill", "Attack_Error", "Attack_Unknown_Outcome"
  )
  missing <- setdiff(required, names(d))
  if (length(missing)) {
    warning(paste("Setting analysis export is missing columns:", paste(missing, collapse = ", ")))
    return(data.frame())
  }

  d <- d[grepl("University of Calgary", d$Team, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(d)) return(d)

  d$Setter_Name <- trimws(as.character(d$Setter_Name))
  d$Setter_Number <- as.character(d$Setter_Number)
  d$Setting_Destination_Zone <- suppressWarnings(as.integer(d$Setting_Destination_Zone))
  d$Setting_Destination_Subzone <- toupper(trimws(as.character(d$Setting_Destination_Subzone)))
  d$Setting_Phase <- trimws(as.character(d$Setting_Phase))
  d$Linked_Attack <- tolower(as.character(d$Linked_Attack)) %in% c("true", "t", "1", "yes")
  for (nm in c("Attack_Kill", "Attack_Error", "Attack_Unknown_Outcome")) {
    d[[nm]] <- tolower(as.character(d[[nm]])) %in% c("true", "t", "1", "yes")
  }
  d
}

setting_zone_cell <- function(zone, subzone) {
  # Reference court orientation: NET at top; zones 4-3-2 / 7-8-9 / 5-6-1.
  zone_layout <- matrix(c(4, 3, 2,
                          7, 8, 9,
                          5, 6, 1), nrow = 3, byrow = TRUE)
  zone <- suppressWarnings(as.integer(as.character(zone)))
  subzone <- toupper(trimws(as.character(subzone)))
  out_x <- out_y <- rep(NA_real_, length(zone))
  for (i in seq_along(zone)) {
    pos <- which(zone_layout == zone[i], arr.ind = TRUE)
    if (!nrow(pos)) next
    # Subzone layout within each numbered zone:
    # C B
    # D A
    sub_pos <- switch(subzone[i],
      "C" = c(1, 1), "B" = c(1, 2),
      "D" = c(2, 1), "A" = c(2, 2),
      NULL
    )
    if (is.null(sub_pos)) next
    out_x[i] <- (pos[1, 2] - 1) * 2 + sub_pos[2] - 0.5
    out_y[i] <- 6 - ((pos[1, 1] - 1) * 2 + sub_pos[1] - 0.5)
  }
  list(x = out_x, y = out_y)
}

setting_map_ui <- function() {
  div(
    class = "attack-maps-section setting-maps-section",
    div(class = "attack-maps-title", "SETTING MAPS"),
    div(class = "setting-map-note",
        "University of Calgary setters · Setting destination by zone and subzone · All sets are counted, including unlinked sets"),
    div(class = "setting-map-controls",
      div(class = "setting-filter-block",
        tags$label("SETTER"),
        uiOutput("setting_setter_buttons")
      ),
      div(class = "setting-filter-block setting-phase-block",
        tags$label("PHASE"),
        radioButtons("setting_phase_filter", NULL,
          choices = c("All phases" = "All", "First Ball" = "First Ball", "Transition" = "Transition"),
          selected = "All", inline = TRUE)
      )
    ),
    plotOutput("setting_destination_map", height = "700px"),
    uiOutput("setting_map_summary")
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

      .opponent-marker{position:absolute;top:-7px;height:27px;width:3px;background:#ef4438;
        transform:translateX(-50%);z-index:3;border-radius:2px;}

      .opponent-label{position:absolute;top:2px;transform:translateX(-50%);
        white-space:nowrap;color:#e33e35;font-size:9px;line-height:11px;max-width:170px;
        overflow:hidden;text-overflow:ellipsis;text-align:center;z-index:4;}

      .skill-area-panel{margin:18px 0 22px;background:#e9eef2;border-radius:15px;padding:14px 16px 18px;}
      .skill-area-heading{font-family:'Barlow Condensed';font-size:15px;font-weight:800;
        letter-spacing:1.2px;color:#617383;border-bottom:1px solid #d4dde4;padding:0 0 8px;margin-bottom:10px;}

      .skill-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:12px;}
      .skill-card{background:#fff;border:1px solid #d8e0e6;border-radius:13px;overflow:hidden;
        box-shadow:0 5px 12px rgba(31,49,67,.08);min-width:0;}

      .skill-card-head{height:36px;background:#C8102E;color:#fff;display:flex;align-items:center;
        justify-content:space-between;padding:0 13px;font-family:'Barlow Condensed';font-size:16px;font-weight:800;}

      .skill-card-head span:last-child{font-size:10px;}
      .skill-card-body{min-height:170px;padding:16px 12px 8px;display:grid;
        grid-template-columns:minmax(88px,42%) minmax(0,1fr);gap:10px;align-items:center;}

      .skill-donut-wrap{width:124px;max-width:100%;aspect-ratio:1;display:flex;
        align-items:center;justify-content:center;margin:auto;}

      .skill-donut{width:100%;aspect-ratio:1;border-radius:50%;display:flex;
        align-items:center;justify-content:center;margin:auto;}

      .skill-conversion-ring{width:146px;max-width:100%;aspect-ratio:1;border-radius:50%;
        padding:7px;display:flex;align-items:center;justify-content:center;margin:auto;box-sizing:border-box;}

      .skill-conversion-ring-inner{width:calc(100% - 14px);height:calc(100% - 14px);
        border-radius:50%;background:#fff;padding:7px;display:flex;align-items:center;
        justify-content:center;box-sizing:border-box;}

      .skill-conversion-ring-inner .skill-donut{width:100%;height:100%;}
      .skill-donut-hole{width:66%;height:66%;border-radius:50%;background:#fff;
        display:flex;flex-direction:column;align-items:center;justify-content:center;text-align:center;}

      .skill-donut-hole strong{font-family:'Barlow Condensed';font-size:27px;line-height:1;color:#263746;}
      .skill-donut-hole small{font-size:8px;letter-spacing:.6px;color:#6f7f8d;margin-top:3px;}
      .skill-legend{min-width:0;}

      .skill-legend-row{display:grid;grid-template-columns:9px minmax(0,1fr) auto 22px;
        gap:6px;align-items:center;margin:7px 0;font-size:10px;}

      .skill-swatch{width:9px;height:9px;border-radius:3px;}
      .skill-legend-name{font-weight:700;color:#344454;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}
      .skill-legend-pct{font-family:'Barlow Condensed';font-size:14px;color:#263746;}
      .skill-legend-count{font-size:9px;text-align:right;color:#87939e;}
      .skill-card-empty,.skill-empty{padding:24px 12px;color:#82909d;font-size:12px;text-align:center;}

      .skill-detail{border-top:1px dashed #dce4eb;margin:0 12px 9px;padding:8px 0 2px;
        text-align:center;font-size:9px;color:#718096;line-height:1.3;}

      .skill-benchmark{border-top:1px dashed #dce4eb;margin:0 12px 10px;padding-top:9px;
        display:grid;grid-template-columns:1fr auto;gap:6px;font-size:9px;color:#788999;}

      .skill-benchmark strong{font-family:'Barlow Condensed';font-size:15px;color:#263746;}
      .skill-benchmark-track{grid-column:1/-1;height:13px;border-radius:20px;background:#f3e9e2;
        position:relative;overflow:visible;margin-top:8px;}

      .skill-benchmark-fill{height:13px;background:#159B69;border-radius:20px;min-width:0;}
      .skill-benchmark-label-wrap{grid-column:1/-1;position:relative;height:17px;}
      .skill-opponent-marker{top:-7px;height:27px;width:3px;background:#ef4438;
        transform:translateX(-50%);z-index:3;border-radius:2px;}

      .skill-opponent-label{top:0;max-width:170px;white-space:nowrap;color:#e33e35;
        font-size:9px;line-height:11px;overflow:hidden;text-overflow:ellipsis;text-align:center;z-index:4;}

      .block-detail-section{overflow:hidden;}
      .block-detail-section .detail-section-title{
        background:#C8102E !important;color:#fff !important;border-radius:9px;
        padding:11px 15px;border:0;margin:-18px -20px 14px;
      }
      .block-detail-subtitle{font-size:11px;color:#718096;font-weight:700;margin:2px 0 12px;}
      .block-type-filter{background:#f1f4f6;border-radius:10px;padding:9px 12px 5px;margin:8px 0 14px;}
      .block-type-filter-title{font-size:9px;font-weight:800;letter-spacing:1.2px;color:#718096;margin-bottom:4px;}
      .block-type-filter .form-group{margin:0;}
      .block-type-filter .checkbox-inline{position:relative;display:inline-flex;align-items:center;
        margin:2px 13px 6px 0;padding:0 0 0 38px;min-height:22px;font-size:10px;
        color:#344454;white-space:nowrap;vertical-align:middle;}
      .block-type-filter .checkbox-inline input[type=checkbox]{position:absolute !important;
        opacity:0 !important;width:1px !important;height:1px !important;margin:0 !important;left:0;}
      .block-type-filter .checkbox-inline:before{content:'';position:absolute;left:0;top:3px;
        width:29px;height:15px;border-radius:12px;background:#c5ced6;transition:background .15s;}
      .block-type-filter .checkbox-inline:after{content:'';position:absolute;left:2px;top:5px;
        width:11px;height:11px;border-radius:50%;background:#fff;transition:transform .15s;}
      .block-type-filter .checkbox-inline:has(input:checked):before{background:#C8102E !important;}
      .block-type-filter .checkbox-inline:has(input:checked):after{transform:translateX(14px);}
      .block-type-filter .checkbox-inline:hover{color:#263746;}
      .block-court{border:1px solid #d4e0e8;border-radius:10px;overflow:hidden;background:#edf4f8;margin:12px 0 14px;}
      .block-court-top-label,.block-court-bottom-label{background:#fff;text-align:center;padding:8px;font-size:8px;font-weight:800;letter-spacing:1.5px;color:#718096;}
      .block-court-lanes{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:0;background-color:#edf4f8;background-image:repeating-linear-gradient(to bottom,transparent 0,transparent 48px,#d8e4eb 49px,#d8e4eb 50px);}
      .block-court-lane{min-width:0;display:flex;flex-direction:column;border-right:2px dashed #bfd0dc;}
      .block-court-lane:last-child{border-right:0;}
      .block-opponent-half{height:78px;display:flex;flex-direction:column;align-items:center;justify-content:center;color:#263746;border-bottom:1px solid #d4e0e8;}
      .block-opponent-half strong{font-family:'Barlow Condensed';font-size:22px;font-weight:800;}
      .block-opponent-half span{font-family:'Barlow Condensed';font-size:10px;font-weight:800;color:#536577;}
      .block-court-net{height:10px;background:#263746;color:#fff;text-align:center;font-size:7px;line-height:10px;letter-spacing:1.2px;z-index:2;}
      .block-court-block-half{height:230px;padding:8px 10px 0;display:flex;flex-direction:column;justify-content:flex-end;}
      .block-court-stack{height:195px;display:flex;flex-direction:column;justify-content:flex-end;align-items:stretch;border-bottom:5px solid #344653;}
      .block-stack-segment{display:block;min-height:0;flex:0 0 auto;}
      .block-court-position{text-align:center;font-family:'Barlow Condensed';font-size:12px;font-weight:800;color:#263746;padding:9px 2px 8px;white-space:nowrap;}
      .block-legend{display:flex;gap:18px;flex-wrap:wrap;margin:12px 0;font-size:10px;font-weight:700;color:#536577;}
      .block-legend-item{display:flex;align-items:center;gap:6px;}
      .block-legend-swatch{width:9px;height:9px;border-radius:2px;display:inline-block;}
      .block-table-wrap{overflow-x:auto;}
      .block-detail-table{width:100%;border-collapse:collapse;font-size:11px;text-align:center;}
      .block-detail-table th,.block-detail-table td{text-align:center !important;}
      .block-detail-table th{font-family:'Barlow Condensed';font-size:11px;letter-spacing:.4px;color:#536577;background:#f2f5f7;padding:10px 7px;border-bottom:2px solid #d6dfe6;white-space:nowrap;}
      .block-detail-table td{padding:9px 7px;border-bottom:1px solid #e3e8ed;color:#344454;font-weight:700;}
      .block-detail-table .block-position-name{text-align:center !important;font-weight:900;color:#263746;white-space:nowrap;}
      @media(max-width:700px){.block-court-lanes{grid-template-columns:repeat(2,minmax(0,1fr));}.block-type-filter .checkbox-inline{margin-right:8px;}}
      .empty-message{color:#7c8995;padding:18px 2px;font-size:13px;}

      .detail-section{background:#fff;border:1px solid #dfe6ed;border-radius:16px;
        padding:18px 20px;margin:18px 0;box-shadow:0 5px 16px rgba(31,49,67,.05);overflow:hidden;}

      .report-card-result{margin-top:7px;font-family:Inter,Arial,sans-serif;font-size:12px;
        font-weight:850;letter-spacing:.8px;color:#263746;text-align:left;}

      .detail-section-title{font-family:'Barlow Condensed';font-size:24px;font-weight:800;
        letter-spacing:1px;color:#5b6d7c;border-bottom:1px solid #dce4eb;padding-bottom:9px;margin-bottom:12px;}

      .rotation-table-wrap{overflow-x:auto;}
      .rotation-table{width:max-content;min-width:100%;border-collapse:separate;
        border-spacing:0;table-layout:auto;font-size:12px;text-align:center;font-variant-numeric:tabular-nums;}

      .rotation-table th{font-family:Inter,Arial,sans-serif;font-size:11px;letter-spacing:.25px;
        color:#344454;background:#f2f5f7;padding:10px 9px;border-bottom:1px solid #d6dfe6;white-space:nowrap;
        text-align:center !important;vertical-align:middle;font-weight:800;}

      .rotation-table .rotation-group-head{background:#263746;color:#fff;font-size:11px;
        letter-spacing:.8px;border-right:2px solid #fff;text-align:center !important;vertical-align:middle;}

      .rotation-table td{padding:10px 9px;border-right:1px solid rgba(255,255,255,.8);
        font-family:Inter,Arial,sans-serif;font-size:12px;font-weight:750;color:#243447;
        text-align:center !important;vertical-align:middle;white-space:nowrap;min-width:76px;}

      .rotation-table .rot-label,.rotation-table .rot-avg-label{background:#edf1f5;color:#263746;
        text-align:center !important;padding:10px 12px;font-size:13px;font-weight:900;}
      .rotation-table .rot-avg-cell{font-weight:900;border-top:2px solid #9eabb5;}
      .rotation-table .rot-neutral{background:#f5f7f9;color:#667788;}
      .rotation-table-full .rotation-average-row td{border-top:2px solid #9eabb5;}
      .rotation-average-row td{border-top:2px solid #9eabb5;background-color:#f4f6f8 !important;}

      .attack-filter-row{display:flex;justify-content:flex-end;margin:-5px 0 10px;}
      .attack-filter-row .form-group{margin:0;}
      .attack-filter-row .control-label{display:none;}
      .attack-filter-row .selectize-input{border-color:#d9e1e7;box-shadow:none;font-size:11px;}

      .attack-detail-section .detail-section-title{background:var(--dinos-red);color:#fff;
        border-radius:9px;padding:11px 15px;border:0;margin:-18px -20px 14px;}

      .attack-detail-table{width:100%;min-width:900px;border-collapse:collapse;font-size:11px;}
      .attack-detail-table th{font-family:'Barlow Condensed';font-size:11px;color:#536577;
        text-transform:uppercase;padding:10px 8px;border-bottom:2px solid #d6dfe6;
        text-align:right;white-space:nowrap;}

      .attack-detail-table th:first-child,.attack-detail-table td:first-child{text-align:left;}
      .attack-detail-table td{padding:9px 8px;border-bottom:1px solid #e3e8ed;
        text-align:right;color:#344454;vertical-align:middle;}

      .attack-detail-table .attack-type-cell{font-weight:900;color:#263746;min-width:135px;}
      .attack-detail-table td:nth-child(n+5){font-family:'Barlow Condensed';font-size:14px;font-weight:800;}

      .attack-summary-bars{display:grid;grid-template-columns:1fr;gap:18px;
        margin:22px 0 8px;padding:0 2px;}

      .attack-summary-heading{display:flex;justify-content:space-between;align-items:center;
        margin-bottom:7px;font-size:10px;font-weight:800;letter-spacing:.7px;color:#718096;}

      .attack-summary-heading strong{font-family:'Barlow Condensed';font-size:23px;color:#159B69;}

      .attack-summary-track{
        height:12px;background:#f3e9e2;border-radius:20px;
        position:relative;overflow:visible;isolation:isolate;
      }
      .attack-summary-fill{
        position:relative;height:12px;border-radius:20px;min-width:0;z-index:1;
      }

      /* Opponent / team benchmark tick: rendered above the efficiency bar. */
      .attack-benchmark-tick{
        position:absolute !important;display:block !important;
        top:-8px !important;height:28px !important;width:4px !important;
        background:#E53935 !important;border:1px solid #ffffff;
        border-radius:2px;transform:translateX(-50%);
        z-index:20 !important;pointer-events:auto;
      }

      .attack-summary-benchmark{position:relative;height:17px;font-size:9px;color:#e33e35;font-weight:700;}
      .attack-summary-benchmark span{position:absolute;transform:translateX(-50%);white-space:nowrap;top:1px;}
      .attack-benchmark-note{font-size:9px;color:#e33e35;text-align:right;margin:4px 0 0;font-weight:800;}


      .serving-map-section{overflow:hidden;}
      .serving-map-section .detail-section-title{background:#C8102E;color:#fff;border:0;
        border-radius:9px;padding:11px 15px;margin:-18px -20px 14px;}
      .serve-filter-row{display:flex;align-items:flex-end;gap:12px;flex-wrap:wrap;
        background:#f1f4f6;border-radius:10px;padding:10px 12px;margin:8px 0 14px;}
      .serve-filter-row .form-group{margin:0;}
      .serve-filter-row .shiny-options-group{display:flex;align-items:center;gap:5px;flex-wrap:wrap;}
      .serve-filter-row .checkbox-group{display:flex;align-items:center;gap:6px;flex-wrap:wrap;}
      .serve-filter-row .checkbox-inline{position:relative;display:inline-flex;align-items:center;
        margin:0;padding:6px 12px;border-radius:7px;background:#e3e8ec;color:#536577;
        font-family:'Barlow Condensed';font-size:11px;font-weight:800;letter-spacing:.35px;cursor:pointer;}
      .serve-filter-row .checkbox-inline input{position:absolute;opacity:0;pointer-events:none;}
      .serve-filter-row .checkbox-inline:has(input:checked){background:#C8102E;color:#fff;}
      .serve-target-table th,.serve-target-table td{text-align:center !important;}
      .serve-target-table th:first-child,.serve-target-table td:first-child{text-align:left !important;}
      .serve-target-table tbody tr:first-child{background:#f4f6f8;font-weight:900;}
      .serve-breakdown-table{width:100%;min-width:0 !important;table-layout:fixed;}
      .serve-breakdown-table th,.serve-breakdown-table td{text-align:center !important;
        padding:7px 4px !important;white-space:nowrap;overflow:hidden;text-overflow:clip;
        font-size:9.5px;}
      .serve-breakdown-table th:nth-child(1),.serve-breakdown-table td:nth-child(1){width:13%;}
      .serve-breakdown-table th:nth-child(2),.serve-breakdown-table td:nth-child(2){width:6%;}
      .serve-breakdown-table th:nth-child(3),.serve-breakdown-table td:nth-child(3){width:25%;}
      .serve-breakdown-table th:nth-child(n+4),.serve-breakdown-table td:nth-child(n+4){width:11.2%;}
      .serve-breakdown-table th:first-child,.serve-breakdown-table td:first-child{text-align:left !important;}
      .serve-breakdown-group-title{background:#f2f5f7;color:#596f83;text-align:left !important;
        font-size:10px;letter-spacing:1px;padding:9px 8px !important;}
      .serve-breakdown-label{font-weight:800;color:#30465a;}
      .serve-breakdown-total{font-weight:900;background:#f7f8fa;}
      .serve-breakdown-bar{height:10px;width:100%;min-width:0 !important;display:flex;overflow:hidden;
        border-radius:8px;background:#e9eef2;}
      .serve-breakdown-segment{height:100%;min-width:1px;}
      .serve-breakdown-table .serve-outcome-value{font-size:10px;color:#526477;}
      .serve-breakdown-table tbody tr:not(:first-child) td{border-bottom:1px solid #e2e8ed;}


      .serve-filter-row .radio-inline{position:relative;display:inline-flex;align-items:center;
        margin:0;padding:6px 12px;border-radius:7px;background:#e3e8ec;color:#536577;
        font-family:'Barlow Condensed';font-size:11px;font-weight:800;letter-spacing:.35px;
        cursor:pointer;transition:all .15s ease;}
      .serve-filter-row .radio-inline input{position:absolute;opacity:0;pointer-events:none;}
      .serve-filter-row .radio-inline:has(input:checked){background:#C8102E;color:#fff;
        box-shadow:0 1px 3px rgba(200,16,46,.2);}

      .serve-filter-row label{font-family:'Barlow Condensed';font-size:10px;letter-spacing:.8px;
        color:#536577;font-weight:800;}
      .serve-filter-row .form-control{height:31px;font-size:11px;border:1px solid #d5dfe7;
        box-shadow:none;border-radius:7px;}
      .serve-map-layout{display:grid;grid-template-columns:minmax(320px,1.05fr) minmax(400px,1fr);
        gap:18px;align-items:start;margin-top:12px;}
      .serve-court-column{min-width:0;}
      .serve-court{position:relative;width:100%;height:500px;overflow:hidden;
        border:3px solid #fff;outline:1px solid #d4e0e8;border-radius:3px;
        background:linear-gradient(to bottom,#eee9e2 0%,#eee9e2 80%,#f3f6f8 80%,#f3f6f8 100%);
        box-shadow:0 2px 8px rgba(31,49,67,.12);}
      .serve-zone-cell{position:absolute;width:33.3333%;height:26.6667%;
        border:0 !important;background:transparent;display:flex;align-items:center;
        justify-content:center;z-index:1;}
      .serve-zone-cell strong{font-family:'Barlow Condensed';font-size:36px;color:#c6d2da;
        font-weight:900;z-index:2;text-shadow:none;}
      .serve-trajectory-layer{position:absolute;inset:0;width:100%;height:100%;z-index:2;
        overflow:visible;pointer-events:none;}
      .serve-map-marker{position:absolute;width:11px;height:11px;border:1.5px solid #fff;
        border-radius:50%;z-index:4;transform:translate(-50%,-50%);
        box-shadow:0 1px 3px rgba(0,0,0,.3);cursor:help;}
      .serve-court-net{position:absolute;left:0;right:0;top:80%;height:5px;
        background:#263746;box-shadow:0 1px 2px rgba(0,0,0,.16);font-size:0;z-index:5;}
      .serve-origin{position:absolute;top:86.5%;transform:translateX(-50%);z-index:6;
        display:flex;flex-direction:column;align-items:center;gap:3px;color:#53616b;
        font-family:'Barlow Condensed';font-size:10px;line-height:1;}
      .serve-origin-dot{width:13px;height:13px;border-radius:50%;background:#53616b;
        border:1px solid #fff;box-shadow:0 1px 2px rgba(0,0,0,.15);}
      .serve-origin strong{font-size:11px;color:#53616b;font-weight:900;}
      .serve-origin small{font-size:9px;color:#82909a;font-weight:700;}
      .serve-map-server-label{text-align:center;font-family:'Barlow Condensed';font-size:10px;
        font-weight:800;letter-spacing:1px;color:#718096;margin-top:8px;}
      .serve-summary-column{min-width:0;}
      .serve-legend{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:9px 12px;
        padding:4px 0 14px;border-bottom:1px solid #e0e6eb;}
      .serve-legend-item{display:flex;align-items:center;gap:7px;font-size:10px;
        font-weight:700;color:#536577;min-width:0;}
      .serve-stat-dot{width:10px;height:10px;border-radius:50%;display:inline-block;flex:0 0 10px;}
      .serve-server-summary-title{font-family:'Barlow Condensed';font-size:14px;font-weight:800;
        letter-spacing:1px;color:#5b6d7c;margin:13px 0 7px;}
      .serve-table-wrap{width:100%;min-width:0;overflow-x:hidden;}
      .serve-start-zone-table{width:100%;min-width:0 !important;table-layout:fixed;}
      .serve-start-zone-table th,.serve-start-zone-table td{text-align:center;white-space:nowrap;}
      .serve-start-zone-table th:first-child,.serve-start-zone-table td:first-child{text-align:left;}
      .serve-start-zone-table .serve-breakdown-bar{width:100%;min-width:0 !important;height:11px;}
      .serve-start-zone-table td{padding:8px 4px;}

      .serve-server-table{width:100%;border-collapse:collapse;font-size:10px;text-align:center;}
      .serve-server-table th{background:#f1f4f6;color:#536577;font-family:'Barlow Condensed';
        font-size:10px;padding:8px 5px;white-space:nowrap;border-bottom:2px solid #d6dfe6;}
      .serve-server-table td{padding:8px 5px;border-bottom:1px solid #e3e8ed;
        color:#344454;font-weight:700;white-space:nowrap;}
      .serve-server-table td:first-child{text-align:left;font-weight:800;}
      .serve-outcome-cards{display:grid;grid-template-columns:repeat(5,minmax(0,1fr));
        gap:8px;margin-top:15px;}
      .serve-stat-card{background:#f3f6f8;border:1px solid #e0e6eb;border-radius:9px;
        padding:9px 10px;min-width:0;}
      .serve-stat-top{display:flex;align-items:center;gap:6px;font-size:9px;font-weight:700;
        color:#536577;min-height:25px;line-height:1.15;}
      .serve-stat-card strong{display:block;font-family:'Barlow Condensed';font-size:23px;
        color:#263746;margin-top:4px;}
      .serve-stat-card small{font-size:9px;color:#87939e;}

      .attack-maps-section{background:#fff;border:1px solid #dfe6ed;border-radius:17px;
        padding:16px;margin:18px 0;box-shadow:0 4px 14px rgba(31,49,67,.04);}
      .attack-maps-title,.attack-map-subsection-label{font-family:'Barlow Condensed';font-size:25px;
        font-weight:800;color:#fff;background:#C8102E;padding:9px 14px;border-radius:10px;margin-bottom:10px;}
      .attack-map-subsection-label{margin:18px 0 9px;font-size:19px;background:#C8102E;}
      .attack-map-card{border:1px solid #dce4eb;border-radius:13px;margin:10px 0 15px;overflow:hidden;
        box-shadow:0 4px 12px rgba(31,49,67,.06);}
      .attack-map-heading{background:#C8102E;color:#fff;padding:9px 14px;font-family:'Barlow Condensed';
        font-size:19px;font-weight:800;letter-spacing:.5px;}
      .attack-map-controls{display:flex;justify-content:space-between;align-items:center;gap:12px;
        padding:5px 12px;border-bottom:1px solid #e0e6eb;flex-wrap:wrap;}
      .attack-map-controls .form-group{margin:0;}
      .attack-map-controls .shiny-options-group{display:flex;gap:9px;align-items:center;flex-wrap:wrap;}
      .attack-type-toggles{display:flex;gap:12px;align-items:center;flex-wrap:wrap;}
      .attack-type-toggles .checkbox{margin:0;}
      /* Real Shiny checkbox labels are the clickable switch surface.
         Keep the input above the switch track so pointer clicks update Shiny. */
      .attack-map-controls .checkbox-inline,
      .attack-map-controls .checkbox{margin:0 10px 0 0;padding:0;background:transparent;}
      .attack-map-controls .checkbox label,
      .attack-map-controls .checkbox-inline{
        position:relative;display:inline-flex;align-items:center;min-height:22px;
        padding:0 0 0 39px;font-size:11px;color:#405467;
        cursor:pointer;white-space:nowrap;vertical-align:middle;font-weight:400;}
      .attack-map-controls .checkbox label input[type=checkbox],
      .attack-map-controls .checkbox-inline input[type=checkbox]{
        position:absolute !important;left:0 !important;top:1px !important;
        width:31px !important;height:20px !important;margin:0 !important;
        opacity:0 !important;cursor:pointer !important;pointer-events:auto !important;
        z-index:3 !important;}
      .attack-map-controls .checkbox label:before,
      .attack-map-controls .checkbox-inline:before{
        content:'';position:absolute;left:0;top:3px;width:31px;height:16px;
        border-radius:12px;background:#c5ced6;transition:background .15s;}
      .attack-map-controls .checkbox label:after,
      .attack-map-controls .checkbox-inline:after{
        content:'';position:absolute;left:2px;top:5px;width:12px;height:12px;
        border-radius:50%;background:#fff;transition:transform .15s;pointer-events:none;}
      .attack-map-controls .checkbox label:has(input:checked):before,
      .attack-map-controls .checkbox-inline:has(input:checked):before{background:#C8102E;}
      .attack-map-controls .checkbox label:has(input:checked):after,
      .attack-map-controls .checkbox-inline:has(input:checked):after{transform:translateX(15px);}
      .attack-map-controls .radio-inline{
        position:relative;display:inline-flex;align-items:center;margin:0;
        padding:5px 11px;border:1px solid #d8e0e7;border-right:0;
        background:#fff;color:#536577;font-size:10px;font-weight:800;cursor:pointer;}
      .attack-map-controls .radio-inline:first-child{border-radius:7px 0 0 7px;}
      .attack-map-controls .radio-inline:last-child{border-right:1px solid #d8e0e7;border-radius:0 7px 7px 0;}
      .attack-map-controls .radio-inline input{position:absolute;opacity:0;pointer-events:none;}
      .attack-map-controls .radio-inline:has(input:checked){
        background:#C8102E;color:#fff;border-color:#C8102E;}
      .attack-map-card .shiny-plot-output{width:100%;}
      .attack-map-summary{padding:0 14px 13px;}
      .attack-summary-stats{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:8px;margin-bottom:12px;}
      .attack-summary-stats div{background:#f3f6f8;border-radius:8px;padding:8px 10px;display:flex;
        justify-content:space-between;align-items:center;gap:5px;}
      .attack-summary-stats span{font-size:9px;color:#718096;font-weight:800;letter-spacing:.5px;}
      .attack-summary-stats strong{font-family:'Barlow Condensed';font-size:20px;color:#263746;}
      .net-bar-title{font-size:9px;font-weight:800;color:#526475;letter-spacing:.7px;margin-bottom:5px;}
      .net-bar-track{height:20px;display:flex;background:#263746;border-radius:4px;overflow:hidden;}
      .net-bar-touch,.net-bar-stuffed,.net-bar-none{height:100%;display:flex;align-items:center;
        justify-content:center;min-width:0;color:#fff;font-size:11px;font-weight:800;line-height:1;}
      .net-bar-touch{background:#159b70;}
      .net-bar-stuffed{background:#df4b40;}
      .net-bar-none{background:#263746;}
      .attack-map-legend{display:flex;gap:16px;flex-wrap:wrap;padding:4px 4px 10px;font-size:10px;color:#667789;}
      .legend-kill{color:#263746}.legend-inplay{color:#263746}.legend-touch{color:#159b70}.legend-stuffed{color:#df4b40}.legend-no-touch{color:#263746}
      @media(max-width:600px){.attack-summary-stats{grid-template-columns:repeat(2,minmax(0,1fr));}
        .attack-map-controls{display:block}.attack-map-controls .form-group{margin:5px 0;}}

      @media(max-width:900px){.serve-map-layout{grid-template-columns:1fr;}
        .serve-court{height:450px;}.serve-outcome-cards{grid-template-columns:repeat(3,minmax(0,1fr));}}
      @media(max-width:600px){.serve-court{height:390px;}
        .serve-outcome-cards{grid-template-columns:repeat(2,minmax(0,1fr));}
        .serve-legend{grid-template-columns:1fr;}}


      /* Passing map */
      .passing-map-section .detail-section-title{background:var(--dinos-red);
        color:#fff;border-radius:9px;padding:11px 15px;border:0;
        margin:-18px -20px 14px;}
      .pass-filter-row{display:flex;align-items:flex-end;gap:18px;flex-wrap:wrap;
        background:#f1f4f6;border-radius:11px;padding:12px 14px;margin:12px 0 8px;}
      .pass-filter-row .form-group{margin:0;}
      .pass-filter-row label{font-family:'Barlow Condensed';font-size:11px;
        letter-spacing:.6px;color:#536577;font-weight:800;}
      .pass-filter-row .shiny-options-group{display:flex;gap:8px;flex-wrap:wrap;}
      .pass-filter-row .radio-inline{position:relative;display:inline-flex;align-items:center;
        margin:0 2px 0 0 !important;padding:8px 14px !important;min-height:34px;
        border-radius:10px;background:#e3e8ed;color:#536577 !important;
        font-family:'Barlow Condensed';font-size:12px;font-weight:800;letter-spacing:.25px;
        cursor:pointer;transition:background .15s,color .15s;}
      .pass-filter-row .radio-inline input[type=radio]{position:absolute !important;
        opacity:0 !important;width:1px !important;height:1px !important;margin:0 !important;}
      .pass-filter-row .radio-inline:has(input:checked){
        background:var(--dinos-red) !important;color:#fff !important;}
      .pass-filter-row .radio-inline:hover{filter:brightness(.97);}
      .pass-map-layout{display:grid;grid-template-columns:minmax(420px,1.08fr) minmax(350px,1fr);
        gap:22px;align-items:start;margin-top:12px;}
      .pass-court-column{min-width:0;}
      .pass-court{position:relative;width:100%;aspect-ratio:1/1.02;overflow:hidden;
        background:#f3f7f9;border:1px solid #cbd7df;border-radius:4px;
        box-shadow:0 2px 8px rgba(38,55,70,.08);}
      .pass-zone-label{position:absolute;width:33.3333%;height:33.3333%;
        display:flex;align-items:center;justify-content:center;z-index:1;
        border:0;pointer-events:none;}
      .pass-zone-label strong{font-family:'Barlow Condensed';font-size:clamp(25px,3.2vw,48px);
        color:#c5d3dc;font-weight:800;opacity:.82;}
      .pass-trajectory-layer{position:absolute;inset:0;width:100%;height:100%;
        z-index:2;overflow:visible;pointer-events:none;}
      .pass-map-marker{position:absolute;width:10px;height:10px;border-radius:50%;
        border:1.5px solid #fff;box-shadow:0 1px 3px rgba(20,35,48,.28);
        transform:translate(-50%,-50%);z-index:4;}
      .pass-map-marker.marker-error{border:0;box-shadow:0 1px 3px rgba(20,35,48,.28);}
      .pass-map-marker.marker-overpass{border:1.5px solid #fff;}
      .pass-map-marker.marker-oos{border:1.5px solid #fff;}
      .pass-court-net{position:absolute;left:0;right:0;top:33.3333%;height:5px;
        background:#263746;color:#fff;font-family:'Barlow Condensed';font-size:9px;
        font-weight:800;letter-spacing:1px;text-align:left;padding-left:7px;
        line-height:5px;z-index:5;}
      .pass-court-caption{text-align:center;font-family:'Barlow Condensed';
        font-size:11px;font-weight:800;letter-spacing:1px;color:#536577;padding:8px;}
      .pass-toggle-panel{display:flex;align-items:center;gap:12px 22px;flex-wrap:wrap;
        background:#fff;border:1px solid #e0e6eb;border-radius:9px;padding:9px 12px;
        margin:0 0 14px;}
      .pass-toggle-panel .form-group{margin:0;}
      .pass-toggle-panel label{position:relative;display:inline-flex;align-items:center;
        margin:0;padding:0 0 0 38px;min-height:22px;font-family:'Barlow Condensed';
        font-size:11px;font-weight:800;letter-spacing:.25px;color:#536577;cursor:pointer;}
      .pass-toggle-panel input[type=checkbox]{position:absolute !important;opacity:0 !important;
        width:1px !important;height:1px !important;margin:0 !important;left:0;}
      .pass-toggle-panel label:before{content:'';position:absolute;left:0;top:3px;
        width:29px;height:15px;border-radius:12px;background:#c5ced6;transition:background .15s;}
      .pass-toggle-panel label:after{content:'';position:absolute;left:2px;top:5px;
        width:11px;height:11px;border-radius:50%;background:#fff;transition:transform .15s;}
      .pass-toggle-panel label:has(input:checked):before{background:#159B69;}
      .pass-toggle-panel label:has(#pass_lines_toggle:checked):before{background:#344653;}
      .pass-toggle-panel label:has(#pass_show_good:checked):before{background:#159B69;}
      .pass-toggle-panel label:has(#pass_show_oos:checked):before{background:#E6A11A;}
      .pass-toggle-panel label:has(#pass_show_overpass:checked):before{background:#F06422;}
      .pass-toggle-panel label:has(#pass_show_errors:checked):before{background:#D9433A;}
      .pass-toggle-panel label:has(input:checked):after{transform:translateX(14px);}
      .pass-summary-column{min-width:0;padding:4px 0;}
      .pass-summary-title{font-family:'Barlow Condensed';color:#536577;
        font-size:13px;font-weight:800;letter-spacing:.8px;margin:2px 0 12px;}
      .pass-boundary-key{display:flex;align-items:center;gap:8px;font-size:10px;
        color:#87939e;margin:8px 0 18px;}
      .pass-boundary-swatch{width:18px;border-top:2px dashed #54BE98;}
      .pass-summary-title{border-top:1px solid #dce4eb;padding-top:14px;margin-top:12px;}
      .pass-summary-column{min-width:0;width:100%;}
      .pass-map-layout{min-width:0;}
      @media(max-width:1100px){.pass-map-layout{grid-template-columns:1fr !important;}}
      .pass-summary-table-wrap{width:100%;min-width:0;overflow-x:hidden;}
      .pass-summary-table{width:100%;min-width:0;table-layout:fixed;border-collapse:collapse;
        font-size:clamp(9px,.72vw,11px);text-align:center;}
      .pass-summary-table th{background:#f1f4f6;color:#536577;font-family:'Barlow Condensed';
        font-size:clamp(9px,.7vw,11px);padding:8px 3px;white-space:normal;
        line-height:1.15;overflow-wrap:anywhere;text-align:center;border-bottom:2px solid #d6dfe6;}
      .pass-summary-table td{padding:9px 3px;border-bottom:1px solid #e3e8ed;
        color:#344454;font-weight:700;white-space:normal;overflow-wrap:anywhere;
        text-align:center;vertical-align:middle;}
      .pass-summary-table th:nth-child(1),.pass-summary-table td:nth-child(1){width:10%;}
      .pass-summary-table th:nth-child(2),.pass-summary-table td:nth-child(2){width:13%;}
      .pass-summary-table th:nth-child(3),.pass-summary-table td:nth-child(3){width:20%;}
      .pass-summary-table th:nth-child(n+4),.pass-summary-table td:nth-child(n+4){width:14.25%;}
      .pass-summary-table td:first-child{text-align:center;font-weight:800;}
      .pass-summary-table td:nth-child(2){font-variant-numeric:tabular-nums;}
      .pass-summary-table td:nth-child(3){padding-left:5px;padding-right:5px;}
      .pass-outcome-bar{display:flex;width:100%;height:13px;overflow:hidden;border-radius:8px;
        background:#e7edf1;box-shadow:inset 0 0 0 1px rgba(38,55,70,.04);}
      .pass-outcome-segment{height:100%;min-width:0;}
      .pass-good-segment{background:#159B69;}
      .pass-oos-segment{background:#E6A11A;}
      .pass-error-segment{background:#D9433A;}
      .pass-table-note{font-size:9px;color:#87939e;margin-top:6px;}
      @media(max-width:900px){.pass-map-layout{grid-template-columns:1fr;}
        .pass-court{max-width:650px;margin:0 auto;}}

      .setting-map-controls{display:flex;align-items:flex-start;justify-content:space-between;gap:18px;
        flex-wrap:wrap;padding:10px 12px;border:1px solid #e0e6eb;border-radius:9px;margin:8px 0 12px;}
      .setting-filter-block{min-width:0;}
      .setting-filter-block>label{display:block;font-size:12px;font-weight:800;color:#344653;margin:0 0 7px;}
      .setting-map-controls .form-group{margin:0;}
      .setting-map-controls .shiny-options-group{display:flex;gap:6px;flex-wrap:wrap;}
      .setting-map-controls .radio-inline{margin:0!important;padding:7px 13px;border:1px solid #d7dfe5;
        border-radius:6px;background:#f3f6f8;font-size:12px;font-weight:700;color:#52616b;}
      .setting-map-controls .radio-inline input{display:none;}
      .setting-map-controls .radio-inline:has(input:checked){background:#C8102E;border-color:#C8102E;color:#fff;}
      .setting-map-note{font-size:11px;color:#6f8090;margin:4px 2px 10px;}
      .setting-summary{margin-top:10px;}
      .setting-summary-stats{display:grid;grid-template-columns:repeat(5,minmax(0,1fr));gap:8px;}
      .setting-summary-stats>div{background:#f3f6f8;border:1px solid #e0e6eb;border-radius:9px;padding:9px 10px;}
      .setting-summary-stats span{display:block;font-size:9px;font-weight:800;color:#637486;letter-spacing:.4px;}
      .setting-summary-stats strong{display:block;font-family:'Barlow Condensed';font-size:22px;color:#263746;margin-top:3px;}
      .setting-summary-note{font-size:10px;color:#718091;margin-top:9px;line-height:1.5;}
      @media(max-width:700px){.setting-summary-stats{grid-template-columns:repeat(2,minmax(0,1fr));}}

      .detail-empty{padding:20px;color:#82909d;text-align:center;font-size:12px;}

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
        .top-header{flex-wrap:wrap;padding:18px;gap:12px}
        .dinos-logo{width:100px;height:80px;}
        .brand-copy h1{font-size:38px}
        .header-latest{order:4;width:100%;text-align:left;border-left:0;padding:8px 0;}
        .metric-line{grid-template-columns:minmax(130px,1fr) 75px minmax(120px,1.2fr);gap:9px;}
      }

      @media(max-width:600px){
        .container-fluid{padding:9px}
        .top-header{border-radius:14px;}
        .brand-copy h1{font-size:30px}
        .brand-subtitle{font-size:10px;letter-spacing:1px;}
        .header-nav{width:100%;justify-content:center;gap:4px}
        .header-nav .btn{width:78px;min-width:78px;font-size:12px}
        .period-bar{overflow-x:auto;}
        .skill-grid{grid-template-columns:repeat(2,minmax(0,1fr));}
        .period-bar .btn{min-width:112px;font-size:14px;}
        .metric-line{grid-template-columns:1fr 75px;}
        .metric-track-wrap{grid-column:1/-1;}
        .metric-section{padding:14px}
        .page-heading h2{font-size:32px;}
        .skill-grid{grid-template-columns:1fr;}
      }
    "))
  ),

  uiOutput("app_header"),
  uiOutput("page_body"),

  div(
    class = "footer",
    "DINOS COACHES DASHBOARD · CALGARY · CANADA WEST · 2026/27"
  )
)

# ----------------------------- SERVER -----------------------------

server <- function(input, output, session) {

  page <- reactiveVal("dashboard")
  active_period <- reactiveVal("MOST RECENT MATCH")
  selected_match <- reactiveVal(NULL)
  selected_attack_player <- reactiveVal("All")

  observeEvent(input$nav_dashboard, {
    page("dashboard")
    selected_match(NULL)
    selected_attack_player("All")
  })

  observeEvent(input$nav_athletes, {
    page("athletes")
  })

  observeEvent(input$nav_reports, {
    page("reports")
    selected_match(NULL)
  })

  observeEvent(input$back_reports, {
    selected_match(NULL)
  })

  observeEvent(input$period_recent, {
    active_period("MOST RECENT MATCH")
  })

  observeEvent(input$period_previous, {
    active_period("PREVIOUS MATCH")
  })

  observeEvent(input$period_5, {
    active_period("LAST 5")
  })

  observeEvent(input$period_10, {
    active_period("LAST 10")
  })

  observeEvent(input$period_season, {
    active_period("FULL SEASON")
  })

  output$app_header <- renderUI({

    latest <- if (nrow(match_index)) {
      match_index[1, , drop = FALSE]
    } else {
      NULL
    }

    latest_date <- if (!is.null(latest)) {
      format(as.Date(latest$match_date), "%b %d, %Y")
    } else {
      "No match loaded"
    }

    latest_opp <- if (!is.null(latest)) {
      as.character(latest$opponent)
    } else {
      "Add match exports"
    }

    result_value <- if (!is.null(latest) &&
                        "match_result" %in% names(latest)) {
      as.character(latest$match_result)
    } else {
      NA_character_
    }

    score_value <- if (!is.null(latest) &&
                       "match_set_score" %in% names(latest)) {
      as.character(latest$match_set_score)
    } else {
      NA_character_
    }

    latest_result <- if (!is.na(result_value) && nzchar(result_value)) {
      paste0(
        toupper(result_value),
        if (!is.na(score_value) && nzchar(score_value)) {
          paste0(" · ", score_value)
        } else {
          ""
        }
      )
    } else {
      "RESULT NOT AVAILABLE"
    }

    div(
      class = "top-header",

      if (file.exists("www/dinos_logo.png")) {
        tags$img(src = "dinos_logo.png", class = "dinos-logo")
      } else {
        div(class = "dinos-logo", "DINOS")
      },

      div(
        class = "brand-copy",
        h1("DINOS COACHES DASHBOARD"),
        div(
          class = "brand-subtitle",
          "CALGARY", tags$b("·"), "CANADA WEST", tags$b("·"), "2026/27"
        )
      ),

      div(
        class = "header-nav",

        actionButton(
          "nav_dashboard",
          label = tagList(
            tags$span(class = "glyphicon glyphicon-home nav-icon"),
            tags$span("DASHBOARD")
          ),
          class = if (page() == "dashboard") "btn active" else "btn"
        ),

        actionButton(
          "nav_athletes",
          label = tagList(
            tags$span(class = "glyphicon glyphicon-user nav-icon"),
            tags$span("ATHLETES")
          ),
          class = if (page() == "athletes") "btn active" else "btn"
        ),

        actionButton(
          "nav_reports",
          label = tagList(
            tags$span(class = "glyphicon glyphicon-file nav-icon"),
            tags$span("REPORTS")
          ),
          class = if (page() == "reports") "btn active" else "btn"
        )
      ),

      div(
        class = "header-latest",
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
        return(
          tagList(
            div(
              class = "page-heading",
              div(
                div(class = "eyebrow", "TEAM ROSTER"),
                h2("ATHLETES"),
                p("Roster file missing or expected columns not found.")
              )
            ),
            div(
              class = "white-panel",
              "Check data/reference/UOC_MASTER_ROSTER.csv."
            )
          )
        )
      }

      return(
        tagList(
          div(
            class = "page-heading",
            div(
              div(class = "eyebrow", "TEAM ROSTER"),
              h2("ATHLETES"),
              p("University of Calgary · Women's Volleyball · 2026/27")
            ),
            div(
              class = "count-pill",
              paste(nrow(roster_view), "ROSTERED ATHLETES")
            )
          ),

          div(
            class = "white-panel",
            tags$table(
              class = "roster-table",

              tags$thead(
                tags$tr(
                  tags$th("NUMBER"),
                  tags$th("ATHLETE"),
                  tags$th("ROLE")
                )
              ),

              tags$tbody(
                lapply(seq_len(nrow(roster_view)), function(i) {
                  tags$tr(
                    tags$td(class = "roster-number", roster_view$Number[i]),
                    tags$td(roster_view$Athlete[i]),
                    tags$td(class = "roster-role", roster_view$Role[i])
                  )
                })
              )
            )
          )
        )
      )
    }

    if (page() == "reports") {

      if (is.null(selected_match())) {

        if (!nrow(match_index)) {
          return(
            tagList(
              div(
                class = "page-heading",
                div(
                  div(class = "eyebrow", "MATCH ARCHIVE"),
                  h2("REPORTS"),
                  p("Match reports will appear here once exports are available.")
                )
              ),
              div(class = "white-panel", "No match reports found.")
            )
          )
        }

        return(
          tagList(
            div(
              class = "page-heading",
              div(
                div(class = "eyebrow", "MATCH ARCHIVE"),
                h2("REPORTS"),
                p("Select a match to view its report.")
              )
            ),

            lapply(seq_len(nrow(match_index)), function(i) {

              row <- match_index[i, , drop = FALSE]

              div(
                class = "report-card",

                div(
                  div(
                    class = "report-card-title",
                    paste("University of Calgary vs", row$opponent)
                  ),
                  div(
                    class = "report-card-date",
                    format(as.Date(row$match_date), "%B %d, %Y")
                  ),
                  div(
                    class = "report-card-result",
                    paste0(
                      if (!is.na(row$match_result) && nzchar(as.character(row$match_result))) {
                        if (toupper(as.character(row$match_result)) %in% c("W", "WIN")) "WIN" else if (toupper(as.character(row$match_result)) %in% c("L", "LOSS")) "LOSS" else toupper(as.character(row$match_result))
                      } else "RESULT PENDING",
                      if (!is.na(row$match_set_score) && nzchar(as.character(row$match_set_score))) paste0("  ·  ", row$match_set_score) else ""
                    )
                  )
                ),

                actionButton(
                  paste0("view_report_", i),
                  "VIEW REPORT",
                  class = "btn-dinos"
                )
              )
            })
          )
        )
      }

      mid <- selected_match()

      return(
        tagList(
          actionButton(
            "back_reports",
            "← BACK TO REPORTS",
            class = "btn-dinos back-btn"
          ),

          div(
            class = "page-heading",
            div(
              div(class = "eyebrow", "MATCH REPORT"),
              h2("MATCH ANALYSIS"),
              p("University of Calgary · ", mid)
            )
          ),

          div(
            class = "white-panel",
            metric_section_ui(
              all_metrics[all_metrics$match_id == mid, , drop = FALSE],
              show_opponent = TRUE
            )
          ),

          skill_area_panel_ui(mid, show_opponent = TRUE),
          rotation_table_ui(mid),

          attack_detail_ui(
            mid,
            selected_player = "All",
            show_opponent = TRUE,
            metric_data = all_metrics[
              all_metrics$match_id == mid,
              ,
              drop = FALSE
            ]
          ),
          block_detail_ui(mid, input$block_attack_type_filter),
          setting_map_ui(),
          serving_map_ui(
            mid,
            input$serve_from_filter,
            input$serve_server_filter,
            input$serve_outcome_filter
          ),
          passing_map_ui(
            mid,
            input$pass_from_filter,
            input$pass_passer_filter,
            c(
          if (is.null(input$pass_show_good) || isTRUE(input$pass_show_good)) "Good passes",
          if (is.null(input$pass_show_oos) || isTRUE(input$pass_show_oos)) "OOS passes",
          if (is.null(input$pass_show_overpass) || isTRUE(input$pass_show_overpass)) "Overpasses",
          if (is.null(input$pass_show_errors) || isTRUE(input$pass_show_errors)) "Passing errors"
        ),
            if (is.null(input$pass_lines_toggle)) TRUE else input$pass_lines_toggle
          )
          ,attack_map_ui()
        )
      )
    }

    # ---------------- DASHBOARD PAGE ----------------

    if (!nrow(match_index)) {
      return(
        tagList(
          div(
            class = "page-heading",
            div(
              div(class = "eyebrow", "2026/27 SEASON"),
              h2("TEAM DASHBOARD"),
              p("No match data has been loaded yet.")
            )
          ),
          div(
            class = "white-panel",
            "Run the data pipeline to generate match exports."
          )
        )
      )
    }

    selected_ids <- switch(
      active_period(),

      "MOST RECENT MATCH" = match_index$match_id[1],

      "PREVIOUS MATCH" = if (nrow(match_index) >= 2) {
        match_index$match_id[2]
      } else {
        match_index$match_id[1]
      },

      "LAST 5" = head(match_index$match_id, 5),

      "LAST 10" = head(match_index$match_id, 10),

      "FULL SEASON" = match_index$match_id
    )

    show_opponent <- active_period() %in% c(
      "MOST RECENT MATCH",
      "PREVIOUS MATCH"
    ) && length(selected_ids) == 1

    match_data <- all_metrics[
      all_metrics$match_id %in% selected_ids,
      ,
      drop = FALSE
    ]

    if (length(selected_ids) == 1) {
      match_data <- match_data[
        match_data$match_id == selected_ids,
        ,
        drop = FALSE
      ]
    }

    tagList(

      div(
        class = "page-heading",

        div(
          div(class = "eyebrow", "2026/27 SEASON"),
          h2("TEAM DASHBOARD"),
          p("University of Calgary · Women's Volleyball · Canada West")
        ),

        div(
          class = "count-pill",
          paste(length(unique(as.character(match_index$match_id))), "MATCHES LOADED")
        )
      ),

      div(
        class = "period-bar",

        actionButton(
          "period_recent",
          "MOST RECENT MATCH",
          class = if (active_period() == "MOST RECENT MATCH") {
            "btn active"
          } else {
            "btn"
          }
        ),

        actionButton(
          "period_previous",
          "PREVIOUS MATCH",
          class = if (active_period() == "PREVIOUS MATCH") {
            "btn active"
          } else {
            "btn"
          }
        ),

        actionButton(
          "period_5",
          "LAST 5",
          class = if (active_period() == "LAST 5") {
            "btn active"
          } else {
            "btn"
          }
        ),

        actionButton(
          "period_10",
          "LAST 10",
          class = if (active_period() == "LAST 10") {
            "btn active"
          } else {
            "btn"
          }
        ),

        actionButton(
          "period_season",
          "FULL SEASON",
          class = if (active_period() == "FULL SEASON") {
            "btn active"
          } else {
            "btn"
          }
        )
      ),

      div(
        class = "metric-section",
        div(class = "metric-section-title", "KEY METRICS"),
        metric_section_ui(
          match_data,
          show_opponent = show_opponent
        )
      ),

      skill_area_panel_ui(
        selected_ids,
        show_opponent = show_opponent
      ),

      rotation_table_ui(selected_ids),

      attack_detail_ui(
        selected_ids,
        selected_player = selected_attack_player(),
        show_opponent = show_opponent,
        metric_data = match_data
      ),
      block_detail_ui(selected_ids, input$block_attack_type_filter),
      setting_map_ui(),
      serving_map_ui(
        selected_ids,
        input$serve_from_filter,
        input$serve_server_filter,
        input$serve_outcome_filter
      ),
      passing_map_ui(
        selected_ids,
        input$pass_from_filter,
        input$pass_passer_filter,
        c(
          if (is.null(input$pass_show_good) || isTRUE(input$pass_show_good)) "Good passes",
          if (is.null(input$pass_show_oos) || isTRUE(input$pass_show_oos)) "OOS passes",
          if (is.null(input$pass_show_overpass) || isTRUE(input$pass_show_overpass)) "Overpasses",
          if (is.null(input$pass_show_errors) || isTRUE(input$pass_show_errors)) "Passing errors"
        ),
        if (is.null(input$pass_lines_toggle)) TRUE else input$pass_lines_toggle
      )
      ,attack_map_ui()
    )
  })

  # The dropdown selection is reactive.
  # The page renderer then refreshes both the table and the summary bars.
  observeEvent(input$attack_player_filter, {

    if (is.null(input$attack_player_filter) ||
        !nzchar(input$attack_player_filter)) {
      selected_attack_player("All")
    } else {
      selected_attack_player(input$attack_player_filter)
    }

  }, ignoreInit = TRUE)

  # Match report navigation.
  observe({

    lapply(seq_len(nrow(match_index)), function(i) {

      local({

        idx <- i

        observeEvent(input[[paste0("view_report_", idx)]], {
          selected_match(match_index$match_id[idx])
        }, ignoreInit = TRUE)

      })
    })
  })

  # Setting map data follows the selected match/period or individual report.
  current_setting_match_ids <- reactive({
    if (page() == "reports" && !is.null(selected_match())) return(as.character(selected_match()))
    if (!nrow(match_index)) return(character(0))
    switch(active_period(),
      "MOST RECENT MATCH" = as.character(match_index$match_id[1]),
      "PREVIOUS MATCH" = as.character(if (nrow(match_index) >= 2) match_index$match_id[2] else match_index$match_id[1]),
      "LAST 5" = as.character(head(match_index$match_id, 5)),
      "LAST 10" = as.character(head(match_index$match_id, 10)),
      "FULL SEASON" = as.character(match_index$match_id),
      as.character(match_index$match_id[1])
    )
  })

  setting_data_for_view <- reactive({
    d <- load_setting_analysis(current_setting_match_ids())
    if (!nrow(d)) return(d)
    if (!is.null(input$setting_setter_filter) && input$setting_setter_filter != "All") {
      d <- d[d$Setter_Name == input$setting_setter_filter, , drop = FALSE]
    }
    if (!is.null(input$setting_phase_filter) && input$setting_phase_filter != "All") {
      d <- d[d$Setting_Phase == input$setting_phase_filter, , drop = FALSE]
    }
    d
  })

  output$setting_setter_buttons <- renderUI({
    d <- load_setting_analysis(current_setting_match_ids())
    setters <- if (nrow(d)) sort(unique(d$Setter_Name[!is.na(d$Setter_Name) & nzchar(d$Setter_Name)])) else character(0)
    choices <- c("All setters" = "All", stats::setNames(setters, setters))
    current <- input$setting_setter_filter
    selected <- if (!is.null(current) && current %in% unname(choices)) current else "All"
    radioButtons("setting_setter_filter", NULL, choices = choices,
                 selected = selected, inline = TRUE)
  })

  output$setting_destination_map <- renderPlot({
    d <- setting_data_for_view()
    # Keep the 6 m x 6 m setting grid square like a real volleyball court,
    # rather than stretching it across the full-width Shiny plotting device.
    par(mar = c(1.0, 0.7, 2.0, 0.7), xaxs = "i", yaxs = "i", pty = "s")
    plot(NA, xlim = c(-0.08, 6.08), ylim = c(-0.15, 6.42), asp = 1,
         axes = FALSE, xlab = "", ylab = "")
    # Net is at the top, matching the supplied zone/subzone reference.
    text(3, 6.28, "NET", cex = 0.9, font = 2, col = "#52616b")
    segments(0, 6.12, 6, 6.12, col = "#344653", lwd = 4)

    # Draw all 36 cells with three horizontal two-row bands.
    # From NET toward the back court: light, medium, warm.
    band_cols <- c("#F1F3F4", "#EEE9E4", "#E9DDD3")
    for (x in 0:5) for (y in 0:5) {
      band <- floor((5 - y) / 2) + 1
      rect(x, y, x + 1, y + 1, col = band_cols[band],
           border = "#C7D1D9", lwd = 0.8)
    }
    rect(0, 0, 6, 6, border = "#AEBBC5", lwd = 1.1)

    if (!nrow(d)) {
      text(3, 3, "No setting analysis data for this selection", col = "#7b8996", cex = 0.9)
      return(invisible(NULL))
    }

    valid <- !is.na(d$Setting_Destination_Zone) &
      d$Setting_Destination_Zone %in% 1:9 &
      d$Setting_Destination_Subzone %in% c("A", "B", "C", "D")
    d <- d[valid, , drop = FALSE]
    if (!nrow(d)) {
      text(3, 3, "No valid setting destinations", col = "#7b8996", cex = 0.9)
      return(invisible(NULL))
    }

    key <- paste(d$Setting_Destination_Zone, d$Setting_Destination_Subzone, sep = "_")
    groups <- split(seq_len(nrow(d)), key)
    counts <- data.frame(
      key = names(groups),
      n = vapply(groups, length, integer(1)),
      stringsAsFactors = FALSE
    )
    counts$zone <- as.integer(sub("_.*$", "", counts$key))
    counts$subzone <- sub("^[^_]*_", "", counts$key)
    counts$linked <- vapply(groups, function(ix) sum(d$Linked_Attack[ix], na.rm = TRUE), integer(1))
    counts$kills <- vapply(groups, function(ix) sum(d$Linked_Attack[ix] & d$Attack_Kill[ix], na.rm = TRUE), integer(1))
    counts$errors <- vapply(groups, function(ix) sum(d$Linked_Attack[ix] & d$Attack_Error[ix], na.rm = TRUE), integer(1))
    counts$unknown <- vapply(groups, function(ix) sum(d$Linked_Attack[ix] & d$Attack_Unknown_Outcome[ix], na.rm = TRUE), integer(1))
    counts$known <- pmax(0, counts$linked - counts$unknown)
    counts$eff <- ifelse(counts$known > 0, (counts$kills - counts$errors) / counts$known, NA_real_)
    xy <- setting_zone_cell(counts$zone, counts$subzone)

    for (i in seq_len(nrow(counts))) {
      x <- xy$x[i]; y <- xy$y[i]
      if (is.na(x) || is.na(y)) next
      eff_label <- if (is.na(counts$eff[i])) "—" else sprintf("%.2f", counts$eff[i])
      eff_col <- if (is.na(counts$eff[i])) "#8b98a3" else "#263746"
      text(x, y + 0.16, eff_label, cex = 1.18, font = 2, col = eff_col)
      detail <- if (counts$known[i] > 0) {
        paste0("K ", counts$kills[i], "  E ", counts$errors[i])
      } else {
        "No linked result"
      }
      text(x, y - 0.10, detail, cex = 0.58, font = 2, col = "#71808c")
      text(x, y - 0.34, paste0("n=", counts$n[i]), cex = 0.62,
           font = 2, col = "#8996a1")
    }
    invisible(NULL)
  }, res = 110)

  output$setting_map_summary <- renderUI({
    d <- setting_data_for_view()
    if (!nrow(d)) return(div(class = "detail-empty", "No setting events found for this match selection."))
    total <- nrow(d)
    linked <- sum(d$Linked_Attack, na.rm = TRUE)
    kills <- sum(d$Linked_Attack & d$Attack_Kill, na.rm = TRUE)
    errors <- sum(d$Linked_Attack & d$Attack_Error, na.rm = TRUE)
    unknown <- sum(d$Linked_Attack & d$Attack_Unknown_Outcome, na.rm = TRUE)
    known <- linked - unknown
    eff <- if (known > 0) (kills - errors) / known else NA_real_
    setter_counts <- sort(table(d$Setter_Name), decreasing = TRUE)
    setter_text <- paste(paste0(names(setter_counts), " (", as.integer(setter_counts), ")"), collapse = " · ")
    div(class = "setting-summary",
      div(class = "setting-summary-stats",
        div(span("TOTAL SETS", strong(total))),
        div(span("LINKED ATTACKS", strong(linked))),
        div(span("KILLS", strong(kills))),
        div(span("ATTACK ERRORS", strong(errors))),
        div(span("LINKED ATTACK EFF.", strong(ifelse(is.na(eff), "—", sprintf("%.3f", eff)))))
      ),
      div(class = "setting-summary-note",
        paste0("Setter distribution: ", setter_text,
               " · Efficiency = (kills − attack errors) / linked attacks with a known outcome. Unlinked sets remain in total set counts."))
    )
  })

  # Attack maps: the same five-map structure is used on the dashboard and
  # individual match reports. The selected period/match controls the source rows.
  current_attack_match_ids <- reactive({
    if (page() == "reports" && !is.null(selected_match())) return(as.character(selected_match()))
    if (!nrow(match_index)) return(character(0))
    switch(active_period(),
      "MOST RECENT MATCH" = as.character(match_index$match_id[1]),
      "PREVIOUS MATCH" = as.character(if (nrow(match_index) >= 2) match_index$match_id[2] else match_index$match_id[1]),
      "LAST 5" = as.character(head(match_index$match_id, 5)),
      "LAST 10" = as.character(head(match_index$match_id, 10)),
      "FULL SEASON" = as.character(match_index$match_id),
      as.character(match_index$match_id[1])
    )
  })

  attack_map_keys <- c("right", "left", "gap_slide", "middle", "backrow")
  lapply(attack_map_keys, function(key) {
    local({
      map_key <- key
      output[[paste0("attack_plot_", map_key)]] <- renderPlot({
        ids <- current_attack_match_ids()
        d <- load_attack_map_events(ids)
        def <- ATTACK_MAP_DEFINITIONS[[map_key]]
        if (map_key == "backrow") {
          type_lower <- tolower(d$Attack_Type)
          code_upper <- toupper(d$Attack_Code)
          keep <- code_upper == "V7" | grepl("pipe", type_lower)
          show_v7 <- isTRUE(input[["attack_toggle_backrow_V7"]])
          show_pipe <- isTRUE(input[["attack_toggle_backrow_PIPE"]])
          keep <- (code_upper == "V7" & show_v7) |
                  (grepl("pipe", type_lower) & show_pipe)
          d <- d[keep, , drop = FALSE]
        } else {
          d <- d[d$Attack_Code %in% def$codes, , drop = FALSE]
          selected_codes <- def$codes[vapply(def$codes, function(code) {
            isTRUE(input[[paste0("attack_toggle_", map_key, "_", code)]])
          }, logical(1))]
          d <- d[d$Attack_Code %in% selected_codes, , drop = FALSE]
        }
        outcome_filter <- input[[paste0("attack_outcome_", map_key)]]
        cls <- if (nrow(d)) attack_map_outcome_class(d) else character(0)
        if (identical(outcome_filter, "Kills")) d <- d[cls == "Kill", , drop = FALSE]
        if (identical(outcome_filter, "In play")) d <- d[cls == "In Play", , drop = FALSE]
        show_paths <- is.null(input[[paste0("attack_paths_", map_key)]]) ||
          isTRUE(input[[paste0("attack_paths_", map_key)]])
        draw_attack_map(d, def$title, show_paths = show_paths)
      }, res = 100)

      output[[paste0("attack_summary_", map_key)]] <- renderUI({
        ids <- current_attack_match_ids()
        d <- load_attack_map_events(ids)
        def <- ATTACK_MAP_DEFINITIONS[[map_key]]
        if (map_key == "backrow") {
          type_lower <- tolower(d$Attack_Type)
          code_upper <- toupper(d$Attack_Code)
          keep <- code_upper == "V7" | grepl("pipe", type_lower)
          show_v7 <- isTRUE(input[["attack_toggle_backrow_V7"]])
          show_pipe <- isTRUE(input[["attack_toggle_backrow_PIPE"]])
          keep <- (code_upper == "V7" & show_v7) |
                  (grepl("pipe", type_lower) & show_pipe)
          d <- d[keep, , drop = FALSE]
        } else {
          d <- d[d$Attack_Code %in% def$codes, , drop = FALSE]
          selected_codes <- def$codes[vapply(def$codes, function(code) {
            isTRUE(input[[paste0("attack_toggle_", map_key, "_", code)]])
          }, logical(1))]
          d <- d[d$Attack_Code %in% selected_codes, , drop = FALSE]
        }
        outcome_filter <- input[[paste0("attack_outcome_", map_key)]]
        cls <- if (nrow(d)) attack_map_outcome_class(d) else character(0)
        if (identical(outcome_filter, "Kills")) d <- d[cls == "Kill", , drop = FALSE]
        if (identical(outcome_filter, "In play")) d <- d[cls == "In Play", , drop = FALSE]
        attack_map_summary_ui(d)
      })
    })
  })

}

shinyApp(ui = ui, server = server)
