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

## Development

```r
devtools::document()
devtools::test()
devtools::check()
```

The live-feed test (`test-live.R`) is gated `skip_on_cran()` +
`skip_if_offline()` and runs only against a working network.
