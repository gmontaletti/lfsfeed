# lfsfeed 0.2.0

* New `get_lfs_metadata(code)` fetches a dataset's titles, dimensions,
  and codelists from Eurostat's SDMX 2.1 dataflow endpoint
  (`?references=descendants`) in a single HTTP call, caches the parsed
  result at `<dest_dir>/metadata/<code>.rds`, and returns a list with
  `$title` (named chr), `$description`, `$dimensions` (tibble), and
  `$codelists` (named list of tibbles, one per dimension).
* Each codelist tibble carries `code`, `label_en`, `label_fr`,
  `label_de`, and `label_it` columns. When the optional `istatlab`
  package (github::gmontaletti/istatlab) is installed, `label_it` is
  populated from ISTAT codelists per a curated mapping
  (`inst/extdata/eurostat-istat-codelists.csv`, override via
  `LFSFEED_MAPPING_CSV`); when `istatlab` is missing or the ISTAT API
  is unavailable, `label_it` falls back to `label_en` and the result
  carries an `"italian_status"` attribute describing why.
* Italian coverage is intentionally partial: only Eurostat codelists
  with an entry in the mapping are attempted, and only codes present
  in the matched ISTAT codelist get a non-fallback label. LFS-specific
  dimensions (`indic_em`, `wstatus`, `nace_r2`, ...) have no ISTAT
  equivalent and remain English.
* `read_lfs_with_labels()` is planned for v0.3.0 — see the README.

# lfsfeed 0.1.0

* Initial release.
* `fetch_lfs_feed()` polls the Eurostat statistics-update RSS feed and
  optionally restricts to Labour Force Survey datasets and to the
  `<category>` values that affect the bulk file.
* `download_lfs_updates()` orchestrates an incremental download of the
  bulk `.tsv.gz` files for every newly-updated LFS dataset, returning a
  manifest tibble.
* `lfs_state_path()` exposes the default state-file location.
* `read_lfs_file()` is a small base-R convenience for loading a
  downloaded `.tsv.gz` into a tibble.
* Ships `inst/scripts/update-lfs.R`, a cron entry point that wraps
  `download_lfs_updates()` with timestamped logging, environment-variable
  configuration, and structured exit codes.
