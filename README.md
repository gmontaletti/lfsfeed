# lfsfeed

Monitor the Eurostat **statistics-update** RSS feed, filter for **Labour
Force Survey** datasets, and download their bulk `.tsv.gz` files.

The intended workflow is one call from a script or cron:

```r
m <- lfsfeed::download_lfs_updates(dest_dir = "~/eurostat/lfs")
print(m)
```

* On the first run, every LFS dataset listed in the feed (today: ~50) is
  downloaded to `dest_dir` as `estat_<code>.tsv.gz`.
* On subsequent runs, only items whose `<pubDate>` is strictly newer
  than the last-seen value (recorded in a small JSON state file) are
  re-fetched.
* The function returns a manifest tibble — one row per dataset
  considered — with status `"downloaded"`, `"skipped"`, or `"failed"`.

## Public API

| function                   | purpose                                            |
|----------------------------|----------------------------------------------------|
| `fetch_lfs_feed()`         | Poll the RSS feed; return a tibble of items.       |
| `download_lfs_updates()`   | Orchestrator: feed -> diff -> download -> manifest.|
| `lfs_state_path()`         | Path of the JSON state file (default = user cache).|
| `read_lfs_file()`          | Small base-R reader for a downloaded `.tsv.gz`.    |

For richer parsing of the bulk file (column splitting, factor labels,
caching as RDS), install the rOpenGov
[`eurostat`](https://cran.r-project.org/package=eurostat) package and
call `eurostat::get_eurostat()` with the dataset code.

## URLs

* RSS feed: <https://ec.europa.eu/eurostat/api/dissemination/catalogue/rss/en/statistics-update.rss>
* Bulk file: `https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/{code}/?format=TSV&compressed=true`

## Install

```r
# from the repo root:
devtools::install()
```

## Scheduled updates with cron

The package ships a small Rscript at `inst/scripts/update-lfs.R` that
calls `download_lfs_updates()`, logs the outcome, and returns sensible
exit codes for monitoring tools. After installing the package, find it
with `system.file("scripts/update-lfs.R", package = "lfsfeed")`.

Configure with environment variables:

| variable             | default                                  |
|----------------------|------------------------------------------|
| `LFSFEED_DEST_DIR`   | `~/eurostat/lfs`                         |
| `LFSFEED_STATE_PATH` | `${LFSFEED_DEST_DIR}/state.json`         |
| `LFSFEED_LOG_FILE`   | `${LFSFEED_DEST_DIR}/update.log`         |

Exit codes: `0` success, `1` every download failed, `2` the package is
not installed in `R_LIBS_USER`, `3` an unrecoverable error occurred
(feed unreachable, malformed XML, unwritable destination).

### Example crontab line

Once daily at 02:30 local time, with `flock(1)` to prevent overlap:

```cron
30 2 * * * R_LIBS_USER=$HOME/Rlibs LFSFEED_DEST_DIR=$HOME/eurostat/lfs \
  /usr/bin/flock -n /tmp/lfsfeed.lock \
  /usr/bin/Rscript -e "source(system.file('scripts/update-lfs.R', package='lfsfeed'))"
```

Eurostat publishes most updates at ~23:00 UTC, so a once-daily run
shortly after midnight UTC is enough; tighten to hourly only if you
need lower latency.

## Development

```r
devtools::document()
devtools::test()
devtools::check()
```

The live-feed test (`test-live.R`) is gated `skip_on_cran()` +
`skip_if_offline()` and runs only against a working network.
