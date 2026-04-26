#' Parse a Eurostat SDMX 2.1 dataflow-with-descendants document.
#'
#' Pulled out of [fetch_eurostat_dataflow()] so the parser can be tested
#' against a committed XML fixture without networking.
#'
#' @param doc an `xml_document` from [xml2::read_xml()].
#' @return a list with elements:
#'   - `title`       named chr (`en`/`fr`/`de`) — the dataflow's name.
#'   - `description` named chr (`en`/`fr`/`de`) — possibly empty.
#'   - `dimensions`  tibble: `position`, `dimension_id` (lowercase),
#'                   `codelist_id` (uppercase, `NA` for the time dim).
#'   - `codelists`   named list of tibbles, one per dimension that has a
#'                   codelist. Each tibble has columns `code`, `label_en`,
#'                   `label_fr`, `label_de`. Missing languages are `NA`.
#' @noRd
parse_eurostat_dataflow_xml <- function(doc) {
  if (!inherits(doc, "xml_document")) {
    rlang::abort(
      "`doc` must be an `xml_document`.",
      class = "lfsfeed_metadata_parse_error"
    )
  }

  ns <- c(
    m = "http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message",
    s = "http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure",
    c = "http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common"
  )

  langs <- c("en", "fr", "de")

  # --- title / description -------------------------------------------------
  pluck_lang_chr <- function(nodes) {
    out <- stats::setNames(rep(NA_character_, length(langs)), langs)
    if (length(nodes) == 0L) return(out)
    for (n in nodes) {
      lang <- xml2::xml_attr(n, "lang")
      if (is.na(lang)) next
      if (lang %in% langs) out[[lang]] <- xml2::xml_text(n)
    }
    out
  }

  df_node <- xml2::xml_find_first(doc, "//s:Dataflows/s:Dataflow", ns = ns)
  title       <- pluck_lang_chr(xml2::xml_find_all(df_node, "./c:Name",        ns = ns))
  description <- pluck_lang_chr(xml2::xml_find_all(df_node, "./c:Description", ns = ns))

  # --- dimensions ---------------------------------------------------------
  dim_nodes <- xml2::xml_find_all(
    doc,
    "//s:DataStructures/s:DataStructure//s:DimensionList/*",
    ns = ns
  )

  if (length(dim_nodes) == 0L) {
    dimensions <- tibble::tibble(
      position     = integer(0),
      dimension_id = character(0),
      codelist_id  = character(0)
    )
  } else {
    pos    <- vapply(dim_nodes, function(n) {
      p <- xml2::xml_attr(n, "position")
      if (is.na(p)) NA_integer_ else as.integer(p)
    }, integer(1))
    dim_id <- vapply(dim_nodes, function(n) tolower(xml2::xml_attr(n, "id")), character(1))
    cl_id  <- vapply(dim_nodes, function(n) {
      ref <- xml2::xml_find_first(n, "./s:LocalRepresentation/s:Enumeration/Ref", ns = ns)
      if (inherits(ref, "xml_missing")) NA_character_
      else toupper(xml2::xml_attr(ref, "id"))
    }, character(1))
    dimensions <- tibble::tibble(
      position     = pos,
      dimension_id = dim_id,
      codelist_id  = cl_id
    )
    # Sort by position when available; keep original order on ties / NAs.
    if (any(!is.na(dimensions$position))) {
      ord <- order(dimensions$position, na.last = TRUE)
      dimensions <- dimensions[ord, , drop = FALSE]
    }
  }

  # --- codelists ----------------------------------------------------------
  cl_nodes <- xml2::xml_find_all(doc, "//s:Codelists/s:Codelist", ns = ns)
  cl_by_id <- stats::setNames(vector("list", length(cl_nodes)),
                              vapply(cl_nodes, function(n) toupper(xml2::xml_attr(n, "id")),
                                     character(1)))

  for (i in seq_along(cl_nodes)) {
    code_nodes <- xml2::xml_find_all(cl_nodes[[i]], "./s:Code", ns = ns)
    n <- length(code_nodes)
    if (n == 0L) {
      cl_by_id[[i]] <- tibble::tibble(
        code     = character(0),
        label_en = character(0),
        label_fr = character(0),
        label_de = character(0)
      )
      next
    }
    code     <- vapply(code_nodes, function(x) xml2::xml_attr(x, "id"), character(1))
    labels_m <- vapply(code_nodes, function(x) {
      pluck_lang_chr(xml2::xml_find_all(x, "./c:Name", ns = ns))
    }, FUN.VALUE = stats::setNames(rep(NA_character_, length(langs)), langs))
    cl_by_id[[i]] <- tibble::tibble(
      code     = code,
      label_en = unname(labels_m["en", ]),
      label_fr = unname(labels_m["fr", ]),
      label_de = unname(labels_m["de", ])
    )
  }

  # Map codelists to dimension ids (lowercased) for stable downstream use.
  codelists <- list()
  for (i in seq_len(nrow(dimensions))) {
    cl_id <- dimensions$codelist_id[i]
    if (is.na(cl_id)) next
    if (cl_id %in% names(cl_by_id)) {
      codelists[[dimensions$dimension_id[i]]] <- cl_by_id[[cl_id]]
    }
  }

  list(
    title       = title,
    description = description,
    dimensions  = dimensions,
    codelists   = codelists
  )
}

#' Fetch and parse the SDMX 2.1 dataflow-with-descendants document for a code.
#'
#' Network call. Errors propagate; caller wraps.
#'
#' @noRd
fetch_eurostat_dataflow <- function(code,
                                    retry_max  = 3L,
                                    timeout_s  = 60L,
                                    user_agent = default_user_agent()) {
  url <- dataflow_url(code)

  req <- httr2::req_retry(
    httr2::req_timeout(
      httr2::req_user_agent(httr2::request(url), user_agent),
      timeout_s
    ),
    max_tries = retry_max,
    backoff   = function(i) min(2 ^ i, 30)
  )
  resp <- httr2::req_perform(req)
  doc  <- xml2::read_xml(httr2::resp_body_raw(resp))

  out <- parse_eurostat_dataflow_xml(doc)
  out$source_url <- url
  out$fetched_at <- Sys.time()
  out
}
