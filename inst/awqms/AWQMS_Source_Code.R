####---------------- Next-Gen AWQMS Helper Script ----------------####
#### Maintains full compatibility with existing codes
#### Optimized for performance, maintainability, and Shiny usage
####
#### CREDENTIALS: Passwords are NEVER stored in this file.
#### They are read from the Windows keyring (via the keyring package)
#### or from environment variables as a fallback.
####
#### To set up credentials on a new machine, run once in R:
####   keyring::key_set(service = "awqms_credentials", username = "oklahomawrb")
####   # A popup will ask for the password — type it and click OK
####
#### Or set environment variables in ~/.Renviron:
####   AWQMS_UID=your_username
####   AWQMS_PWD=your_password

suppressPackageStartupMessages({
  library(DBI)
  library(odbc)
  library(dplyr)
  library(dbplyr)
  library(keyring)
  library(rlang)
})

# ---- Configuration (edit only here) ----
AWQMS <- list(
  service        = "awqms_credentials",
  server         = "owrb.gselements.com",
  port           = 1433,
  database       = NULL,
  default_schema = "ext",
  driver         = "SQL Server",
  default_user   = "oklahomawrb",
  allow_env_fallback = TRUE,
  env_uid_name       = "AWQMS_UID",
  env_pwd_name       = "AWQMS_PWD"
)

resolve_awqms_credentials <- function(
    service = AWQMS$service,
    user = AWQMS$default_user,
    allow_env_fallback = AWQMS$allow_env_fallback,
    env_uid_name = AWQMS$env_uid_name,
    env_pwd_name = AWQMS$env_pwd_name
) {
  pwd <- tryCatch(keyring::key_get(service = service, username = user), error = function(e) NULL)
  if (!is.null(pwd) && nzchar(pwd)) {
    return(list(uid = user, pwd = pwd, source = "keyring"))
  }

  if (isTRUE(allow_env_fallback)) {
    uid2 <- Sys.getenv(env_uid_name)
    pwd2 <- Sys.getenv(env_pwd_name)
    if (nzchar(uid2) && nzchar(pwd2)) {
      return(list(uid = uid2, pwd = pwd2, source = "env"))
    }
  }

  stop("AWQMS credentials unavailable. Set via keyring or environment variables.", call. = FALSE)
}

awqms_connect <- function(
    server = AWQMS$server,
    port = AWQMS$port,
    database = AWQMS$database,
    driver = AWQMS$driver,
    trusted_connection = "No"
) {
  creds <- resolve_awqms_credentials()

  DBI::dbConnect(
    odbc::odbc(),
    Driver = driver,
    Server = server,
    Port = port,
    Database = database,
    UID = creds$uid,
    PWD = creds$pwd,
    Trusted_Connection = trusted_connection
  )
}

.awqms_state <- new.env(parent = emptyenv())

awqms_get_con <- function() {
  if (!exists("con", envir = .awqms_state) || !DBI::dbIsValid(.awqms_state$con)) {
    .awqms_state$con <- awqms_connect()
  }
  .awqms_state$con
}

awqms_disconnect <- function() {
  if (exists("con", envir = .awqms_state) && DBI::dbIsValid(.awqms_state$con)) {
    DBI::dbDisconnect(.awqms_state$con)
  }
  rm(list = "con", envir = .awqms_state)
  invisible(TRUE)
}

awqms_norm <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (is.character(x)) {
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (length(x) == 0) return(NULL)
  }
  x
}

awqms_filter_projects <- function(q, projects, prefix = "project_id", n = 8) {
  projects <- awqms_norm(projects)
  if (is.null(projects)) return(q)

  conds <- lapply(seq_len(n), function(i) expr(.data[[paste0(prefix, i)]] %in% !!projects))
  q %>% dplyr::filter(Reduce(function(a, b) expr((!!a) | (!!b)), conds))
}

awqms_tbl <- function(name, default_schema = AWQMS$default_schema) {
  con <- awqms_get_con()
  if (grepl("\\.", name)) {
    parts <- strsplit(name, "\\.", fixed = FALSE)[[1]]
    schema <- parts[1]
    object <- parts[2]
  } else {
    schema <- default_schema
    object <- name
  }
  tbl(con, dbplyr::in_schema(schema, object))
}

# ---- Date-gated results loader for app usage ----
awqms_get_results_standard_filtered <- function(
    start_date,
    end_date,
    columns = NULL,
    protocol = NULL,
    projects = NULL,
    activity_media = "Habitat",
    monitoring_location_type = "River/Stream",
    project_col_count = 6,
    collect = TRUE
) {
  if (length(start_date) != 1 || is.na(start_date) ||
      length(end_date)   != 1 || is.na(end_date)) {
    stop("No date range specified", call. = FALSE)
  }

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  q <- awqms_tbl("results_standard_vw") %>%
    dplyr::filter(
      .data$activity_media           == activity_media,
      .data$monitoring_location_type == monitoring_location_type,
      .data$activity_start_date      >= start_date,
      .data$activity_start_date      <= end_date
    )

  if (!is.null(columns))
    q <- q %>% dplyr::select(dplyr::any_of(columns))

  if (identical(protocol, "NRSA Protocol"))
    q <- q %>%
      dplyr::filter(.data$sample_collection_method_id %in% c(
        "Wadeable", "Boatable", "DRYVISIT", "INTWADE", "PARBYBOAT", "PARBYWADE"
      ))

  q <- awqms_filter_projects(q, projects, prefix = "project_id", n = project_col_count)

  if (isTRUE(collect)) q %>% dplyr::collect() else q
}
