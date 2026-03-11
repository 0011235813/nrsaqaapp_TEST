# nrsaqaapp <img src="inst/app/www/logo.png" align="right" height="80" alt="NRSA QA App logo"/>

> A Shiny application for quality assurance review of **National Rivers and Streams Assessment (NRSA)** monitoring data.

<!-- badges: start -->
[![R-CMD-check](https://github.com/your-org/nrsaqaapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/your-org/nrsaqaapp/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

`nrsaqaapp` helps state environmental agencies QA-review NRSA field data before submission.  
It supports data loading from **Excel file upload** or a direct **AWQMS ODBC connection**, and provides:

| Tab | What it does |
|-----|-------------|
| **Summary** | Site-visit overview — record counts, transect completeness, date range |
| **Site Map** | Interactive Leaflet map with popup details per site |
| **QA Review** | Per-visit quality score, missing values, outliers, flagged records |
| **Batch QA** | QA metrics across every visit in the filtered dataset at once |

---

## Installation

```r
# Install from GitHub
remotes::install_github("your-org/nrsaqaapp")
```

> **Requirements:** R ≥ 4.1.0, and the packages listed in `DESCRIPTION`.

---

## Quick Start

### File-upload mode (works out of the box)

```r
library(nrsaqaapp)
run_app()
```

Upload one or more `.xlsx` / `.xls` files exported from AWQMS or any system
that follows the `results_standard_vw` schema.

### AWQMS / ODBC mode

1. **Copy the config template** from `inst/app/config.yml` to your project folder.
2. **Add a profile** for your agency:

```yaml
# config.yml
myagency:
  awqms_source: "C:/path/to/your/AWQMS_connection_script.R"
  awqms_table:  "results_standard_vw"
  max_upload_bytes: 5368709120
```

The `awqms_source` script must define two functions:

| Function | Purpose |
|----------|---------|
| `awqms_get_con()` | Returns an open DBI connection |
| `awqms_disconnect()` | Closes the connection |

3. **Launch with your profile:**

```r
Sys.setenv(R_CONFIG_ACTIVE = "myagency")
nrsaqaapp::run_app()
```

---

## Configuration Reference

| Key | Default | Description |
|-----|---------|-------------|
| `awqms_source` | `null` | Path to AWQMS connection script. `null` = file-upload only. |
| `awqms_table` | `"results_standard_vw"` | Database view/table to query. |
| `max_upload_bytes` | `5368709120` (5 GB) | Maximum upload size. |

---

## Data Requirements

The app expects data aligned to the **`results_standard_vw`** schema.  
When uploading Excel files, column names are snake-cased and synonyms are mapped automatically.

### Key columns

| Column | Description |
|--------|-------------|
| `monitoring_location_id` | Site identifier |
| `activity_start_date` | Sample date (ISO, M/D/YYYY, D-Mon-YYYY, or Excel serial) |
| `sampling_component_name` | Transect letter (A–K) |
| `characteristic_name` | Parameter name |
| `result_measure` | Numeric result value |
| `parent_activity_id` | Visit group (derived from `activity_id`) |
| `project_id1` … `project_id6` | Project identifiers |

### NRSA Project IDs

Records must contain at least one of these values across `project_id1`–`project_id6`:

```
FWProb08-12  FWProb13-17  FWProb18-22  FWProb23-27
FWTrend      FWFire2019   FWProbAll    AncillaryHabitat  NRSA-Data
```

---

## Adapting for Your Agency

Most agencies will only need to:

1. Add a config profile in `config.yml`
2. Point `awqms_source` at their connection script

To extend the app (e.g. add a parameter, change QA thresholds):

- **QA ranges** — edit `PARAMETER_QA_RULES` in `R/constants.R`
- **Expected transects** — the default is A–K (11 transects); change the
  `expected` string in `compute_transect_summary()` in `R/fct_qa.R`
- **Project IDs** — edit `NRSA_PROJECT_IDS` in `R/constants.R`
- **Column synonyms** — add entries to `synonym_map` in
  `harmonize_to_odbc_schema()` in `R/fct_data.R`

---

## Running Tests

```r
# From the package root
devtools::test()
```

Tests cover all pure helper functions in `R/fct_data.R`, `R/fct_qa.R`, and `R/utils.R`.

---

## Package Structure

```
nrsaqaapp/
├── R/
│   ├── constants.R          # Column names, project IDs, QA rules
│   ├── utils.R              # %||%, safe_date, safe_min_date, to_snake_case
│   ├── fct_data.R           # Data loading, coercion, harmonization
│   ├── fct_qa.R             # Outlier detection, scoring, QA flags
│   ├── fct_db.R             # AWQMS connection helpers
│   ├── mod_data_source.R    # Module: startup + upload + ODBC modals
│   ├── mod_summary.R        # Module: Summary tab
│   ├── mod_map.R            # Module: Site Map tab
│   ├── mod_qa_review.R      # Module: QA Review tab
│   ├── mod_batch_qa.R       # Module: Batch QA tab
│   ├── app_ui.R             # Top-level UI
│   ├── app_server.R         # Top-level server
│   └── run_app.R            # run_app() entry point
├── inst/app/
│   └── config.yml           # Connection configuration
├── tests/testthat/          # Unit tests
├── DESCRIPTION
└── README.md
```

---

## Contributing

Pull requests are welcome!  
Please open an issue first to discuss any major changes.

When submitting a PR:
- Add or update tests for any new functions in `tests/testthat/`
- Run `devtools::check()` locally — all checks should pass before submitting

---

## License

MIT © Oklahoma Water Resources Board — see [LICENSE](LICENSE).
