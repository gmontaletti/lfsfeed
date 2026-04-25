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
