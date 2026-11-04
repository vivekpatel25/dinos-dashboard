# ============================================================
# DINOS VOLLEYBALL ANALYTICS - HELPER FUNCTIONS
# ============================================================

# Read a CSV safely. Returns NULL when the file is not uploaded yet.
read_dashboard_csv <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = c("", "NA")
    ),
    error = function(e) {
      warning(
        sprintf("Could not read %s: %s", path, conditionMessage(e)),
        call. = FALSE
      )
      NULL
    }
  )
}

# Return a friendly label for a missing dataset.
dataset_status <- function(data) {
  if (is.null(data)) {
    return("Awaiting CSV upload")
  }

  paste(format(nrow(data), big.mark = ","), "rows loaded")
}

# Format numeric values without showing unnecessary decimals.
format_metric <- function(x, digits = 1) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) {
    return("—")
  }

  value <- suppressWarnings(as.numeric(x[1]))

  if (is.na(value)) {
    return(as.character(x[1]))
  }

  if (abs(value - round(value)) < .Machine$double.eps^0.5) {
    return(format(round(value), big.mark = ",", scientific = FALSE))
  }

  format(round(value, digits), big.mark = ",", nsmall = digits)
}

# A reusable empty-state panel for modules that do not have CSV data yet.
empty_state <- function(title, description, expected_files = NULL) {
  tags$div(
    class = "empty-state",
    tags$div(class = "empty-state-icon", icon("file-circle-plus")),
    tags$h3(title),
    tags$p(description),
    if (!is.null(expected_files)) {
      tags$div(
        class = "expected-files",
        tags$strong("Expected data files"),
        tags$ul(lapply(expected_files, tags$li))
      )
    }
  )
}

# Build a reusable KPI card.
kpi_card <- function(label, value = "—", note = "Waiting for data") {
  tags$div(
    class = "metric-card",
    tags$div(class = "metric-label", label),
    tags$div(class = "metric-value", value),
    tags$div(class = "metric-note", note)
  )
}

# Check that all expected columns are present before a module uses a dataset.
has_columns <- function(data, required_columns) {
  !is.null(data) && all(required_columns %in% names(data))
}

# Return distinct non-missing values for filters.
safe_choices <- function(data, column) {
  if (is.null(data) || !column %in% names(data)) {
    return(character(0))
  }

  values <- unique(as.character(data[[column]]))
  values <- values[!is.na(values) & nzchar(values)]
  sort(values)
}

# Return a concise status for the application data folder.
data_inventory <- function(paths) {
  data.frame(
    File = basename(paths),
    Status = vapply(paths, function(path) {
      if (file.exists(path)) {
        paste(format(file.info(path)$size, big.mark = ","), "bytes")
      } else {
        "Not uploaded"
      }
    }, character(1)),
    stringsAsFactors = FALSE
  )
}
