# ntrdWisconsin 0.0.1

Initial release of `ntrdWisconsin`, an
[`ntrd`](https://github.com/rmtrane/ntrd) extension that integrates the
Wisconsin Alzheimer's Disease Research Center (WADRC) data infrastructure
with the Neuropsychological Test Results Dashboard. The package pulls
neuropsych data from the WADRC REDCap databases (UDS-2, UDS-3, UDS-4) and
biomarker data from the Panda API, then surfaces results inside the `ntrd`
dashboard with a dedicated "Biomarkers" panel featuring population density
plots and threshold-based categorization.

## Data source

* Declares `Config/ntrd/extension: true` so `ntrd` discovers the package
  automatically at app startup.
* `wadrc_source` — S7 data source class (built with
  `ntrd::new_data_source()`) registered under the display name
  "Wisconsin ADRC".
* `data_source_ui()` / `data_source_server()` — Shiny module providing
  password inputs for the three REDCap API tokens and an optional Panda
  API key, with detection of an unreachable Panda server (e.g. when the
  user is off-campus and not on the SMPH VPN).
* `data_load()` — combines the three UDS pulls, fills constant
  participant-level fields across visits, drops rows with missing `SEX`,
  and returns a validated `data_nacc` object.
* `.set_defaults()` (called from `.onLoad()` and `.onAttach()`) sets
  T-score standardization defaults from `ntrsTscores` for the relevant
  neuropsych scores.

## REDCap integration

* `pull_redcap_data()` — exported wrapper around
  `REDCapR::redcap_read_oneshot()` that targets
  `https://redcap.medicine.wisc.edu/api/`, shows Shiny progress
  notifications while pulling and preparing data, and falls back to `cli`
  warnings outside a Shiny context.
* `wadrc_uds2_redcap_fields`, `wadrc_uds3_redcap_fields`,
  `wadrc_uds4_redcap_fields` — bundled character vectors listing the
  REDCap fields pulled from each UDS database.
* `nacc_to_wadrc_uds2` / `wadrc_uds2_to_nacc`,
  `nacc_to_wadrc_uds3` / `wadrc_uds3_to_nacc`,
  `nacc_to_wadrc_uds4` — bundled named character vectors translating
  between NACC and WADRC variable names for each UDS version.
* `wadrc_data_prep()` — applies the WADRC-to-NACC translation and a
  handful of manual coding fixes (e.g. recoding `88` to `-4` for UDS-2
  trail-making and memory-time fields).

## Biomarker integration

* `extension_ui()` / `extension_server()` — Shiny module that injects a
  "Biomarkers" `bslib::nav_panel` into the main `ntrd` dashboard. Fetches
  per-participant biomarker data from the Panda API asynchronously via
  `mirai`, caches results per `NACCID`, and renders an interactive HTML
  table.
* `extension_app()` — standalone Shiny app harness for iterating on the
  biomarker module outside the main dashboard.
* `get_all_values()` — pulls every Panda biomarker table (excluding
  Participants, Appointments, and Visual Rating tables) to support
  population-level summaries.
* `get_all_densities()` — Gaussian kernel density estimates
  (Sheather-Jones bandwidth) for each raw biomarker, used to draw the
  inline density plots.
* `get_all_cuts()` — derives plotting cut-offs either from
  `biomarker_thresholds` when known, or by inferring boundaries between
  categorized bins from the data.
* `bio_tab_to_html_table()` — renders the per-visit biomarker table with
  clickable info icons (source + threshold tooltips) and hover-triggered
  density plots showing where the participant's value sits in the WADRC
  distribution.
* `density_plot()` — exported `plotly` density plot with shaded
  threshold regions and a marker for the observed value.
* Panda JSON query template shipped at `inst/json/panda_template.json`.

## Biomarker thresholds and ratios

* `biomarker_thresholds` — bundled dataset of categorization rules
  indexed by Panda table and biomarker, each carrying a source
  description, reference link, and a `data.frame` of
  `label`/`min`/`max`/`bin` rows.
* `lumipulse_pTau217_ABeta42_ratio()` and
  `lumipulse_ABeta42_ABeta40_ratio()` — compute the Lumipulse ratios
  with value-capping and `NA` rules taken from the corresponding FDA
  review documents.
* `categorize_ratio()` — assigns a label or bin to a numeric value
  using a thresholds table.
* `create_thresholds_table()` — formats a thresholds table for display
  in the biomarker tooltip.

## In-app updates

* `ntrd_update_available()` and `ntrd_update_extension()` exported via
  `ntrd::default_github_update_available("rmtrane/ntrdWisconsin")` and
  `ntrd::default_github_update_extension("rmtrane/ntrdWisconsin")`, so
  the `ntrd` app shows an update banner with a "What's new?" link to
  this `NEWS.md` whenever a newer version is published on GitHub.

## Infrastructure

* MIT licensed.
* R (>= 4.1.0).
* CI on GitHub Actions: R-CMD-check on macOS, Windows, and Ubuntu
  (devel / release / oldrel-1).
* Full extension API walkthrough lives in the "Extension API" vignette
  in the parent `ntrd` package, with `ntrdWisconsin` as the worked
  example.