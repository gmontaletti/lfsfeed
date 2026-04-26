# Offline tests for the v0.2.0 metadata pipeline.

# --- helpers ----------------------------------------------------------------

load_fixture <- function() {
  xml2::read_xml(test_path("fixtures/dataflow-lfsa_egan.xml"))
}

# Bare meta object with two codelists for apply_istat_labels tests.
make_stub_meta <- function() {
  list(
    title       = c(en = "Test", fr = NA_character_, de = NA_character_),
    description = c(en = NA_character_, fr = NA_character_, de = NA_character_),
    dimensions  = tibble::tibble(
      position     = c(1L, 2L),
      dimension_id = c("geo", "indic_em"),
      codelist_id  = c("GEO", "INDIC_EM")
    ),
    codelists = list(
      geo = tibble::tibble(
        code     = c("IT", "FR", "XX"),
        label_en = c("Italy", "France", "Mystery"),
        label_fr = c("Italie", "France", NA_character_),
        label_de = c("Italien", "Frankreich", NA_character_)
      ),
      indic_em = tibble::tibble(
        code     = c("EMP_LFS", "ACT"),
        label_en = c("Employed - LFS", "Active population"),
        label_fr = c(NA_character_, NA_character_),
        label_de = c(NA_character_, NA_character_)
      )
    )
  )
}

stub_mapping_geo <- function() {
  tibble::tibble(
    eurostat_codelist_id = "GEO",
    istat_codelist_id    = "CL_ITTER107",
    join_strategy        = "identity",
    notes                = "test"
  )
}

# --- tests ------------------------------------------------------------------

test_that("parse_eurostat_dataflow_xml extracts the expected shape", {
  doc <- load_fixture()
  out <- parse_eurostat_dataflow_xml(doc)

  expect_named(out, c("title", "description", "dimensions", "codelists"))
  expect_named(out$title, c("en", "fr", "de"))
  expect_true(!is.na(out$title[["en"]]))

  expect_s3_class(out$dimensions, "tbl_df")
  expect_named(out$dimensions, c("position", "dimension_id", "codelist_id"))
  expect_true(all(out$dimensions$dimension_id == tolower(out$dimensions$dimension_id)))
  expect_true("geo" %in% out$dimensions$dimension_id)
  expect_true("sex" %in% out$dimensions$dimension_id)

  expect_type(out$codelists, "list")
  expect_true("geo" %in% names(out$codelists))
  expect_true("sex" %in% names(out$codelists))

  expect_named(out$codelists$geo, c("code", "label_en", "label_fr", "label_de"))
  expect_gte(nrow(out$codelists$geo), 30L)

  it_row <- out$codelists$geo[out$codelists$geo$code == "IT", , drop = FALSE]
  expect_equal(nrow(it_row), 1L)
  expect_equal(it_row$label_en[[1L]], "Italy")
  expect_equal(it_row$label_fr[[1L]], "Italie")
  expect_equal(it_row$label_de[[1L]], "Italien")
})

test_that("parse_eurostat_dataflow_xml errors on non-xml_document input", {
  expect_error(parse_eurostat_dataflow_xml("<rss/>"),
               class = "lfsfeed_metadata_parse_error")
})

test_that("parse_eurostat_dataflow_xml fills NA for missing languages", {
  xml_str <- '<m:Structure xmlns:m="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message"
                          xmlns:s="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure"
                          xmlns:c="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common">
    <m:Structures>
      <s:Dataflows>
        <s:Dataflow id="X"><c:Name xml:lang="en">Eng only</c:Name></s:Dataflow>
      </s:Dataflows>
      <s:DataStructures>
        <s:DataStructure id="DSD_X"><s:DataStructureComponents>
          <s:DimensionList>
            <s:Dimension id="GEO" position="1">
              <s:LocalRepresentation>
                <s:Enumeration><Ref id="GEO"/></s:Enumeration>
              </s:LocalRepresentation>
            </s:Dimension>
          </s:DimensionList>
        </s:DataStructureComponents></s:DataStructure>
      </s:DataStructures>
      <s:Codelists>
        <s:Codelist id="GEO">
          <s:Code id="IT"><c:Name xml:lang="en">Italy</c:Name></s:Code>
        </s:Codelist>
      </s:Codelists>
    </m:Structures>
  </m:Structure>'

  out <- parse_eurostat_dataflow_xml(xml2::read_xml(xml_str))
  expect_equal(out$codelists$geo$label_en, "Italy")
  expect_true(is.na(out$codelists$geo$label_fr))
  expect_true(is.na(out$codelists$geo$label_de))
  expect_true(is.na(out$title[["fr"]]))
})

test_that("apply_istat_labels happy path joins ISTAT names by code", {
  meta <- make_stub_meta()
  resolver <- function() {
    list(CL_ITTER107 = tibble::tibble(
      code    = c("IT", "FR"),
      name_it = c("Italia", "Francia")
    ))
  }
  out <- apply_istat_labels(
    meta,
    mapping         = stub_mapping_geo(),
    .istat_resolver = resolver
  )

  expect_equal(attr(out, "italian_status"), "ok")
  expect_true("geo" %in% attr(out, "italian_matched_codelists"))

  geo <- out$codelists$geo
  expect_equal(geo$label_it[geo$code == "IT"], "Italia")
  expect_equal(geo$label_it[geo$code == "FR"], "Francia")
  # Codes with no IT match fall back to label_en.
  expect_equal(geo$label_it[geo$code == "XX"], "Mystery")

  # Codelists not in the mapping fully fall back.
  ind <- out$codelists$indic_em
  expect_equal(ind$label_it, ind$label_en)
})

test_that("apply_istat_labels falls back when istatlab is missing", {
  meta <- make_stub_meta()
  testthat::local_mocked_bindings(istatlab_available = function() FALSE)
  out <- apply_istat_labels(meta, mapping = stub_mapping_geo())

  expect_match(attr(out, "italian_status"), "^skipped:")
  for (nm in names(out$codelists)) {
    expect_equal(out$codelists[[nm]]$label_it,
                 out$codelists[[nm]]$label_en)
  }
})

test_that("apply_istat_labels falls back when the ISTAT resolver errors", {
  meta <- make_stub_meta()
  resolver <- function() stop("HTTP 503: ISTAT down")
  out <- apply_istat_labels(
    meta,
    mapping         = stub_mapping_geo(),
    .istat_resolver = resolver
  )
  expect_match(attr(out, "italian_status"), "^istat-error:")
  for (nm in names(out$codelists)) {
    expect_equal(out$codelists[[nm]]$label_it,
                 out$codelists[[nm]]$label_en)
  }
})

test_that("get_lfs_metadata caches to RDS and avoids re-fetching", {
  withr::with_tempdir({
    counter <- new.env(parent = emptyenv())
    counter$n <- 0L
    fake_fetch <- function(code, retry_max = 3L, timeout_s = 60L,
                           user_agent = NULL) {
      counter$n <- counter$n + 1L
      list(
        title       = c(en = "Stub", fr = NA_character_, de = NA_character_),
        description = c(en = NA_character_, fr = NA_character_, de = NA_character_),
        dimensions  = tibble::tibble(
          position     = 1L,
          dimension_id = "geo",
          codelist_id  = "GEO"
        ),
        codelists = list(
          geo = tibble::tibble(
            code     = "IT",
            label_en = "Italy",
            label_fr = "Italie",
            label_de = "Italien"
          )
        ),
        source_url = "stub://lfsi_jhh_a",
        fetched_at = Sys.time()
      )
    }

    testthat::local_mocked_bindings(
      fetch_eurostat_dataflow = fake_fetch
    )

    m1 <- get_lfs_metadata("LFSI_JHH_A",
                           dest_dir = "data",
                           lang     = c("en"),
                           quiet    = TRUE)
    expect_equal(counter$n, 1L)
    expect_equal(attr(m1, "schema_version"), 1L)
    expect_equal(attr(m1, "code"), "lfsi_jhh_a")
    expect_true(file.exists(file.path("data", "metadata", "lfsi_jhh_a.rds")))
    expect_true("label_it" %in% names(m1$codelists$geo))
    expect_false("label_fr" %in% names(m1$codelists$geo))

    # Second call should hit the RDS cache.
    m2 <- get_lfs_metadata("lfsi_jhh_a",
                           dest_dir = "data",
                           lang     = c("en"),
                           quiet    = TRUE)
    expect_equal(counter$n, 1L)
    expect_equal(attr(m2, "code"), "lfsi_jhh_a")

    # refresh = TRUE re-fetches.
    m3 <- get_lfs_metadata("lfsi_jhh_a",
                           dest_dir = "data",
                           refresh  = TRUE,
                           lang     = c("en"),
                           quiet    = TRUE)
    expect_equal(counter$n, 2L)
    expect_s3_class(m3$dimensions, "tbl_df")
  })
})

test_that("get_lfs_metadata rejects empty / multi-element code", {
  expect_error(get_lfs_metadata(""),         class = "lfsfeed_metadata_arg_error")
  expect_error(get_lfs_metadata(c("a","b")), class = "lfsfeed_metadata_arg_error")
  expect_error(get_lfs_metadata(123),        class = "lfsfeed_metadata_arg_error")
})

test_that("read_istat_mapping reads the bundled CSV", {
  m <- read_istat_mapping()
  expect_s3_class(m, "tbl_df")
  expect_named(m, c("eurostat_codelist_id", "istat_codelist_id",
                    "join_strategy", "notes"))
  expect_true(all(m$eurostat_codelist_id == toupper(m$eurostat_codelist_id)))
  expect_true("GEO" %in% m$eurostat_codelist_id)
})
