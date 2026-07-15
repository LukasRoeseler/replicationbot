library(bskyr)

# Shared helpers: format_reproduction_outcome(), format_replication_outcome()
# (also used by bot.R)
source("helpers.R")

# 1. Helper function: Normalize a DOI (or a doi.org URL / "doi:" reference) for comparison
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

# 2. Helper function: Extract a raw doi.org URL from a post's plain text.
# This is a fallback only — Bluesky truncates the *visible* text of any long
# pasted URL (e.g. "doi.org/10.1207/s153...") and stores the real, full URL
# separately as a rich-text facet (see extract_dois_from_post() below), so
# most real posts won't have a usable full DOI in the visible text at all.
extract_doi_from_text <- function(text) {
  if (length(text) == 0 || is.na(text) || text == "") return(character())
  m <- regmatches(text, gregexpr("(?i)doi\\.org/(10\\.[0-9]{4,9}/[^\\s\"'<>\\)\\],]+)", text, perl = TRUE))[[1]]
  m
}

# 3. Helper function: Pull every candidate doi.org URL out of a raw post
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

# 4. Helper function: Flatten the raw (clean = FALSE) bs_search_posts() result
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

# 5. Helper functions: Read/write the dedup log of posts already replied to
load_replied_log <- function(path) {
  if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  } else {
    data.frame(post_uri = character(), doi = character(), replied_at = character(), stringsAsFactors = FALSE)
  }
}

save_replied_log <- function(log, path) {
  write.csv(log, path, row.names = FALSE)
}

# 6. Main process
main <- function() {
  dry_run <- tolower(Sys.getenv("DRY_RUN", "false")) == "true"

  # Load credentials from GitHub Secrets
  bsky_handle <- Sys.getenv("BLUESKY_HANDLE")
  bsky_password <- Sys.getenv("BLUESKY_PASSWORD")

  # Authenticate with Bluesky
  set_bluesky_user(bsky_handle)
  set_bluesky_pass(bsky_password)

  # ---------------------------------------------------------
  # Load the dataset live from GitHub and collect original-study DOIs
  # that are known to have a replication attempt
  # ---------------------------------------------------------
  df <- read.csv("https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

  # Keep the full row per valid doi_o (not just the DOI) so a match can also
  # report that replication's outcome. If a DOI has more than one replication
  # attempt in the dataset, the first matching row is used (same simplification
  # bot.R already makes when picking one row to post about).
  flora_valid <- df[!is.na(df$doi_o) & df$doi_o != "", ]
  flora_valid$doi_o_norm <- normalize_doi(flora_valid$doi_o)
  flora_dois <- unique(flora_valid$doi_o_norm)

  if (length(flora_dois) == 0) {
    stop("Error: No valid original-study DOIs found in flora.csv.")
  }

  # ---------------------------------------------------------
  # Load the dedup log of posts already replied to
  # ---------------------------------------------------------
  log_path <- "replied_posts.csv"
  replied_log <- load_replied_log(log_path)
  already_replied_uris <- replied_log$post_uri

  # ---------------------------------------------------------
  # Search recent posts linking to doi.org. clean = FALSE returns the raw
  # parsed JSON response (a plain nested list mirroring the AT Protocol
  # lexicon exactly), so post facets/embeds can be read reliably below.
  # ---------------------------------------------------------
  since <- format(Sys.time() - as.difftime(25, units = "hours"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  resp <- tryCatch({
    bs_search_posts(query = "doi.org", domain = "doi.org", sort = "latest", since = since, limit = 50, clean = FALSE)
  }, error = function(e) {
    cat("Warning: bs_search_posts() failed:", conditionMessage(e), "\n")
    NULL
  })

  candidates <- if (is.null(resp)) list() else flatten_search_posts(resp)

  if (length(candidates) == 0) {
    cat("No candidate posts found in this run.\n")
    return(invisible(NULL))
  }

  # ---------------------------------------------------------
  # Walk each candidate: skip our own posts, posts already linking FLoRA,
  # and posts already replied to; otherwise try to match a known DOI and reply
  # ---------------------------------------------------------
  new_log_rows <- list()

  for (post in candidates) {
    post_uri <- post$uri
    if (is.null(post_uri) || post_uri %in% already_replied_uris) next

    author_handle <- post$author$handle
    if (!is.null(author_handle) && tolower(author_handle) == tolower(bsky_handle)) next

    post_text <- post$record$text
    if (!is.null(post_text) && grepl("flora-replication-atlas", post_text, ignore.case = TRUE)) next

    raw_dois <- extract_dois_from_post(post)
    if (length(raw_dois) == 0) next

    candidate_dois <- unique(normalize_doi(raw_dois))
    matched_dois <- candidate_dois[candidate_dois %in% flora_dois]
    if (length(matched_dois) == 0) next
    doi <- matched_dois[1]

    row <- flora_valid[flora_valid$doi_o_norm == doi, ][1, ]
    study_type <- ifelse(!is.na(row$type), tolower(row$type), "unknown")
    raw_outcome <- ifelse(!is.na(row$outcome), tolower(row$outcome), "unknown")

    if (study_type == "reproduction") {
      outcome_sentence <- sprintf("According to the reproduction authors, %s.", format_reproduction_outcome(raw_outcome))
    } else {
      outcome_sentence <- sprintf("According to the replication authors, %s.", format_replication_outcome(raw_outcome))
    }

    flora_url <- sprintf("https://forrt.org/flora-replication-atlas/?dois=%s", utils::URLencode(doi, reserved = TRUE))

    short_reply <- sprintf("Did you know there is a replication to this study? %s", flora_url)
    full_reply <- sprintf("Did you know there is a replication to this study? %s\n\n%s", outcome_sentence, flora_url)

    # Bluesky posts cap at 300 characters; fall back to the shorter wording
    # (no outcome sentence) rather than skip the reply or risk truncation.
    reply_text <- if (nchar(full_reply, type = "chars") <= 300) full_reply else short_reply

    cat("Match found - post:", post_uri, "- DOI:", doi, "\n")

    if (dry_run) {
      cat("[DRY RUN] Would reply:", reply_text, "\n\n")
      next
    }

    # embed = FALSE: without this, bs_post() tries to auto-generate a link
    # preview card for the FLoRA Atlas URL in reply_text, which is the same
    # bskyr embed bug bot.R's get_link() already works around — it caused
    # every reply attempt to fail with an HTTP 400.
    result <- tryCatch({
      bs_post(text = reply_text, reply = post_uri, embed = FALSE)
      TRUE
    }, error = function(e) {
      cat("Warning: failed to reply to", post_uri, "-", conditionMessage(e), "\n")
      FALSE
    })

    if (isTRUE(result)) {
      new_log_rows[[length(new_log_rows) + 1]] <- data.frame(
        post_uri = post_uri,
        doi = doi,
        replied_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        stringsAsFactors = FALSE
      )
      cat("Successfully replied!\n\n")
    }
  }

  if (length(new_log_rows) > 0 && !dry_run) {
    updated_log <- do.call(rbind, c(list(replied_log), new_log_rows))
    save_replied_log(updated_log, log_path)
    cat("Logged", length(new_log_rows), "new repl(y/ies) to", log_path, "\n")
  } else {
    cat("No new replies logged in this run.\n")
  }
}

main()
