# nrsaqaapp News

## 1.0.0 (2025)

### Initial public release

- Shiny app restructured as an installable R package using the golem framework
- Data loading via Excel file upload (`.xlsx` / `.xls`) or AWQMS ODBC connection
- Multi-format date parsing: ISO, M/D/YYYY, D-Mon-YYYY, Excel serial number
- Date parse failure warning modal with continue / cancel options
- Agency-configurable AWQMS connection via `config.yml` profiles
- **Summary tab**: record counts, date range, transect completeness, export buttons
- **Site Map tab**: interactive Leaflet map with per-site popups
- **QA Review tab**: quality score, missing values, outlier detection, QA flag table
- **Batch QA tab**: QA metrics across all site-visits with colour-coded missing-value thresholds
- Dark mode and font-size preferences persisted in browser localStorage
- Keyboard shortcuts: Ctrl+R (Apply Filters), Ctrl+H (Help), Escape (Clear selection)
- Full unit test suite for all pure helper functions
