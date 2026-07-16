library(bskyr)

# Shared helpers: normalize_doi(), extract_dois_from_post(), flatten_search_posts(),
# load_replied_log()/save_replied_log(), get_link(), EVALUATION_END_DATE
# (also used by bot.R / reply_bot.R)
source("helpers.R")

RETRACTION_WATCH_CSV_URL <- "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv?ref_type=heads"

# 1. Helper function: Retraction Watch packs multiple values into some fields
# (OriginalPaperDOI, URLS, Reason) as a ";"-separated list. Take the first.
first_semicolon_field <- function(x) {
  if (is.na(x)) return(NA_character_)
  x <- trimws(sub(";.*$", "", x))
  if (x == "") return(NA_character_)
  x
}

# 2. Helper function: Lowercase the first letter, for mid-sentence use
lowercase_first <- function(x) {
  if (is.na(x) || x == "") return(x)
  paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# 3. Helper function: Map Retraction Watch's "RetractionNature" to the phrasing
# the user asked for. Only "Retraction" and "Expression of concern" are
# handled (deliberately skipping "Correction"/"Reinstatement"/blank rows,
# which aren't what was asked for).
notice_label <- function(nature) {
  if (identical(nature, "Retraction")) return("retraction notice")
  if (identical(nature, "Expression of concern")) return("editorial notice of concern")
  NA_character_
}

# 4. Main process
main <- function() {
  if (Sys.Date() >= EVALUATION_END_DATE) {
    cat("Evaluation period ended on", format(EVALUATION_END_DATE), "- retraction bot is paused.\n")
    return(invisible(NULL))
  }

  dry_run <- tolower(Sys.getenv("DRY_RUN", "false")) == "true"

  # Load credentials from GitHub Secrets
  bsky_handle <- Sys.getenv("BLUESKY_HANDLE")
  bsky_password <- Sys.getenv("BLUESKY_PASSWORD")

  # Authenticate with Bluesky
  set_bluesky_user(bsky_handle)
  set_bluesky_pass(bsky_password)

  # ---------------------------------------------------------
  # Load the Retraction Watch database live from GitLab (updated weekly
  # upstream, so fetching fresh on every once-a-day run keeps this current)
  # and collect original-paper DOIs that were retracted or given an
  # editorial notice of concern.
  # ---------------------------------------------------------
  df <- read.csv(RETRACTION_WATCH_CSV_URL, stringsAsFactors = FALSE, na.strings = c("", "NA"))

  retraction_valid <- df[
    !is.na(df$OriginalPaperDOI) &
      df$RetractionNature %in% c("Retraction", "Expression of concern"),
  ]
  # A handful of rows list more than one original DOI, separated by ";" -
  # take the first (same "pick one" simplification reply_bot.R already makes
  # when a DOI matches multiple flora.csv rows).
  retraction_valid$doi_o_norm <- normalize_doi(sub(";.*$", "", retraction_valid$OriginalPaperDOI))
  retraction_dois <- unique(retraction_valid$doi_o_norm)

  if (length(retraction_dois) == 0) {
    stop("Error: No valid original-paper DOIs found in retraction_watch.csv.")
  }

  # ---------------------------------------------------------
  # Load the dedup log of posts already replied to
  # ---------------------------------------------------------
  log_path <- "retraction_replies.csv"
  log_cols <- c("post_uri", "doi", "notice_type", "replied_at")
  replied_log <- load_replied_log(log_path, cols = log_cols)
  already_replied_uris <- replied_log$post_uri

  # ---------------------------------------------------------
  # Search recent posts linking to doi.org. Runs once a day, so a 25h
  # lookback (not 24h) gives a cushion against cron jitter without risking
  # duplicate replies (dedup is keyed on post_uri).
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
  # Walk each candidate: skip our own posts, posts already linking Retraction
  # Watch, and posts already replied to; otherwise try to match a known DOI
  # and reply
  # ---------------------------------------------------------
  new_log_rows <- list()

  for (post in candidates) {
    post_uri <- post$uri
    if (is.null(post_uri) || post_uri %in% already_replied_uris) next

    author_handle <- post$author$handle
    if (!is.null(author_handle) && tolower(author_handle) == tolower(bsky_handle)) next

    post_text <- post$record$text
    if (!is.null(post_text) && grepl("retractionwatch", post_text, ignore.case = TRUE)) next

    raw_dois <- extract_dois_from_post(post)
    if (length(raw_dois) == 0) next

    candidate_dois <- unique(normalize_doi(raw_dois))
    matched_dois <- candidate_dois[candidate_dois %in% retraction_dois]
    if (length(matched_dois) == 0) next
    doi <- matched_dois[1]

    row <- retraction_valid[retraction_valid$doi_o_norm == doi, ][1, ]
    label <- notice_label(row$RetractionNature)
    if (is.na(label)) next

    reason <- lowercase_first(first_semicolon_field(row$Reason))
    link <- get_link(row$RetractionDOI, first_semicolon_field(row$URLS), NA)

    short_reply <- sprintf("Did you know that there is a %s for this study? %s", label, link)
    full_reply <- if (!is.na(reason)) {
      sprintf("Did you know that there is a %s for this study? Reason: %s. %s", label, reason, link)
    } else {
      short_reply
    }

    # Bluesky posts cap at 300 characters; fall back to the shorter wording
    # (no reason) rather than skip the reply or risk truncation.
    reply_text <- if (nchar(full_reply, type = "chars") <= 300) full_reply else short_reply

    cat("Match found - post:", post_uri, "- DOI:", doi, "- Notice:", row$RetractionNature, "\n")

    if (dry_run) {
      cat("[DRY RUN] Would reply:", reply_text, "\n\n")
      next
    }

    # embed = FALSE: without this, bs_post() tries to auto-generate a link
    # preview card for the notice URL in reply_text, which is the same
    # bskyr embed bug bot.R's get_link() already works around, and which
    # caused every reply_bot.R attempt to fail with an HTTP 400 before that
    # fix was added.
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
        notice_type = row$RetractionNature,
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
