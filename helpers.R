# Shared helper functions used by bot.R (daily broadcast) and reply_bot.R
# (reactive replies about both replications and retractions), so their
# wording and scanning logic stay consistent.

# Trial period: the reactive-reply feature (reply_bot.R) is being evaluated
# for one week before deciding whether to keep it running. After this date,
# the script exits without searching or replying. Remove the check in that
# file (and this constant) once the trial is over and the feature is
# confirmed to stay.
EVALUATION_END_DATE <- as.Date("2026-07-22")

# 1. Helper function: Convert JSON authors to a short citation
get_short_citation <- function(author_json, year) {
  if (is.na(author_json) || author_json == "") return(paste0("Unknown (", year, ")"))

  authors <- tryCatch({
    jsonlite::fromJSON(author_json)
  }, error = function(e) return(NULL))

  if (is.null(authors) || !("family" %in% names(authors))) {
    return(paste0("Unknown (", year, ")"))
  }

  families <- authors$family

  if (length(families) == 0 || is.na(families[1])) {
    return(paste0("Unknown (", year, ")"))
  }

  n <- length(families)

  if (n == 1) {
    name <- families[1]
  } else if (n == 2) {
    name <- paste(families[1], "&", families[2])
  } else {
    name <- paste(families[1], "et al.")
  }

  return(paste0(name, " (", year, ")"))
}

# 2. Helper function: Prioritize link AND strip https:// to bypass the bskyr embed bug!
get_link <- function(doi, primary_url, fallback_url = NA) {
  if (!is.na(doi) && doi != "") {
    return(paste0("doi.org/", doi))
  }
  if (!is.na(primary_url) && primary_url != "") {
    return(gsub("^https?://", "", primary_url))
  }
  if (!is.na(fallback_url) && fallback_url != "") {
    return(gsub("^https?://", "", fallback_url))
  }
  return("No link available")
}

# 3. Helper function: Repro
format_reproduction_outcome <- function(outcome) {
  outcome <- gsub("computionally", "computationally", outcome)
  mapping <- c(
    "computationally successful, robust" = "the reproduction was computationally successful and robust",
    "computationally successful, robustness challenges" = "the reproduction was computationally successful, but had robustness challenges",
    "computationally successful, robustness not checked" = "the reproduction was computationally successful, though robustness was not checked",
    "computational issues, robust" = "there were computational issues but the finding was robust",
    "computational issues, robustness challenges" = "there were computational issues and robustness challenges",
    "computational issues, robustness not checked" = "there were computational issues and robustness was not checked",
    "computation not checked, robust" = "computational reproducibility was not checked but the finding was robust",
    "computation not checked, robustness challenges" = "computational reproducibility was not checked and there were robustness challenges",
    "computation not checked, robustness not checked" = "neither computational reproducibility nor robustness were checked"
  )
  if (outcome %in% names(mapping)) {
    return(mapping[[outcome]])
  } else {
    return(paste("the outcome was:", outcome))
  }
}

# 4. Helper function: Replications
format_replication_outcome <- function(outcome) {
  mapping <- c(
    "successful" = "the replication attempt was successful",
    "failed" = "the replication attempt failed",
    "mixed" = "the replication attempt yielded mixed results",
    "uninformative" = "the replication attempt was uninformative",
    "descriptive only" = "there was no success or failure but the replication was uninformative",
    "statistically successful but flawed" = "the replication attempt was statistically successful but flawed"
  )
  if (outcome %in% names(mapping)) {
    return(mapping[[outcome]])
  } else {
    return(paste("the replication attempt resulted in:", outcome))
  }
}

# 5. Helper function: Normalize a DOI (or a doi.org URL / "doi:" reference) for comparison
normalize_doi <- function(x) {
  x <- trimws(x)
  x <- utils::URLdecode(x)
  x <- sub("^https?://", "", x, ignore.case = TRUE)
  x <- sub("^(dx\\.|www\\.)?doi\\.org/", "", x, ignore.case = TRUE)
  x <- sub("^doi:\\s*", "", x, ignore.case = TRUE)
  x <- sub("[)\\]\\.,;:'\"]+$", "", x)
  x <- sub("/$", "", x)
  tolower(x)
}

# 6. Helper function: Extract a raw DOI-shaped string from a post's plain
# text — matches the bare "10.xxxx/yyyy" pattern whether or not it's preceded
# by a "doi.org/" URL prefix, so both a full doi.org link's visible text AND
# a plain typed-out DOI with no link at all (e.g. "Has this study been
# retracted? 10.1006/obhd.1998.2802") get picked up. This is a fallback in
# extract_dois_from_post() below — Bluesky also truncates the *visible* text
# of any long pasted URL (e.g. "doi.org/10.1207/s153..."), storing the real,
# full URL separately as a rich-text facet, which is checked first.
extract_doi_from_text <- function(text) {
  if (length(text) == 0 || is.na(text) || text == "") return(character())
  m <- regmatches(text, gregexpr("(?i)(?:doi\\.org/)?(10\\.[0-9]{4,9}/[^\\s\"'<>\\)\\],]+)", text, perl = TRUE))[[1]]
  m
}

# 7. Helper function: Pull every candidate doi.org URL out of a raw post
# object (as returned by bs_search_posts(..., clean = FALSE), which mirrors
# the AT Protocol JSON 1:1 — see app.bsky.feed.post lexicon). Checks, in
# order: rich-text link facets (catches truncated-display links, the common
# case for any long pasted URL), the external embed/link-card URI, and
# finally the visible text itself.
extract_dois_from_post <- function(post) {
  candidates <- character()

  facets <- post$record$facets
  if (!is.null(facets)) {
    for (facet in facets) {
      for (feature in facet$features) {
        furi <- feature$uri
        if (!is.null(furi) && grepl("doi\\.org", furi, ignore.case = TRUE)) {
          candidates <- c(candidates, furi)
        }
      }
    }
  }

  embed_uri <- post$record$embed$external$uri
  if (!is.null(embed_uri) && grepl("doi\\.org", embed_uri, ignore.case = TRUE)) {
    candidates <- c(candidates, embed_uri)
  }

  post_text <- post$record$text
  if (!is.null(post_text)) {
    candidates <- c(candidates, extract_doi_from_text(post_text))
  }

  unique(candidates)
}

# 8. Helper function: Flatten the raw (clean = FALSE) bs_search_posts() result
# into a plain list of post objects, regardless of whether it comes back as a
# single page (a list with a $posts field) or a list of multiple such pages.
flatten_search_posts <- function(resp) {
  if (!is.null(resp$posts)) return(resp$posts)
  posts <- list()
  for (page in resp) {
    if (!is.null(page$posts)) posts <- c(posts, page$posts)
  }
  posts
}

# 9. Helper function: Run several bs_search_posts() queries and merge their
# results into one deduplicated (by post uri) candidate list. `queries` is a
# list of list(query = ..., domain = NULL) specs. A domain-filtered search
# for "doi.org" only finds posts with an actual link to that domain; a
# second, domain-less keyword search (e.g. "retracted OR retraction") also
# catches posts that just type out a bare DOI with no link at all, at the
# cost of a noisier candidate pool — harmless here, since candidates still
# have to match a known DOI before anything gets replied to.
collect_candidate_posts <- function(queries, since, limit) {
  posts <- list()
  seen_uris <- character()
  for (q in queries) {
    resp <- tryCatch({
      bs_search_posts(query = q$query, domain = q$domain, sort = "latest", since = since, limit = limit, clean = FALSE)
    }, error = function(e) {
      cat("Warning: bs_search_posts() failed for query '", q$query, "': ", conditionMessage(e), "\n", sep = "")
      NULL
    })
    if (is.null(resp)) next
    for (post in flatten_search_posts(resp)) {
      uri <- post$uri
      if (!is.null(uri) && !(uri %in% seen_uris)) {
        seen_uris <- c(seen_uris, uri)
        posts[[length(posts) + 1]] <- post
      }
    }
  }
  posts
}

# 10. Helper functions: Read/write the dedup log of posts already replied to
# (one shared log for both replication and retraction replies, distinguished
# by the `kind`/`notice_type` columns; `reply_uri` is the bot's own reply
# post, used by index.html to fetch its thread directly rather than hunting
# for the bot in the original post's replies list). `cols` gives the right
# empty-frame shape when the file doesn't exist yet.
load_replied_log <- function(path, cols = c("post_uri", "reply_uri", "doi", "kind", "notice_type", "replied_at")) {
  if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = "")
  } else {
    empty <- as.data.frame(matrix(character(), nrow = 0, ncol = length(cols)))
    names(empty) <- cols
    empty
  }
}

save_replied_log <- function(log, path) {
  write.csv(log, path, row.names = FALSE, na = "")
}
