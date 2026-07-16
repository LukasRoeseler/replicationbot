library(bskyr)

# Shared helpers from helpers.R: normalize_doi(), extract_dois_from_post(),
# collect_candidate_posts(), load_replied_log()/save_replied_log(), get_link(),
# format_reproduction_outcome(), format_replication_outcome() (the latter two,
# plus get_short_citation()/get_link(), are also used by bot.R for its daily
# broadcast, so both scripts share the same wording).
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
# the user asked for, distinguishing retractions from editorial notices of
# concern. Only these two are handled (deliberately skipping
# "Correction"/"Reinstatement"/blank rows, which aren't what was asked for).
notice_label <- function(nature) {
  if (identical(nature, "Retraction")) return("retraction notice")
  if (identical(nature, "Expression of concern")) return("editorial notice of concern")
  NA_character_
}

# 4. Main process
main <- function() {
  if (Sys.Date() >= EVALUATION_END_DATE) {
    cat("Evaluation period ended on", format(EVALUATION_END_DATE), "- reply bot is paused.\n")
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
  # Load FLoRA (replications/reproductions) and collect original-study DOIs
  # that are known to have a replication attempt
  # ---------------------------------------------------------
  df_flora <- read.csv("https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

  # Keep the full row per valid doi_o (not just the DOI) so a match can also
  # report that replication's outcome. If a DOI has more than one replication
  # attempt in the dataset, the first matching row is used (same simplification
  # bot.R already makes when picking one row to post about).
  flora_valid <- df_flora[!is.na(df_flora$doi_o) & df_flora$doi_o != "", ]
  flora_valid$doi_o_norm <- normalize_doi(flora_valid$doi_o)
  flora_dois <- unique(flora_valid$doi_o_norm)

  # ---------------------------------------------------------
  # Load Retraction Watch (fresh on every run — updated weekly upstream, and
  # this runs hourly, so freshness is never a concern) and collect
  # original-paper DOIs that were retracted or given an editorial notice of
  # concern.
  # ---------------------------------------------------------
  df_rw <- read.csv(RETRACTION_WATCH_CSV_URL, stringsAsFactors = FALSE, na.strings = c("", "NA"))

  retraction_valid <- df_rw[
    !is.na(df_rw$OriginalPaperDOI) &
      df_rw$RetractionNature %in% c("Retraction", "Expression of concern"),
  ]
  # A handful of rows list more than one original DOI, separated by ";" -
  # take the first (same "pick one" simplification used for flora.csv above).
  retraction_valid$doi_o_norm <- normalize_doi(sub(";.*$", "", retraction_valid$OriginalPaperDOI))
  retraction_dois <- unique(retraction_valid$doi_o_norm)

  if (length(flora_dois) == 0 && length(retraction_dois) == 0) {
    stop("Error: No valid DOIs found in either flora.csv or retraction_watch.csv.")
  }

  # ---------------------------------------------------------
  # Load the dedup log of posts already replied to (shared between
  # replication and retraction replies, distinguished by kind/notice_type)
  # ---------------------------------------------------------
  log_path <- "replied_posts.csv"
  replied_log <- load_replied_log(log_path)
  already_replied_uris <- replied_log$post_uri

  # ---------------------------------------------------------
  # Search recent posts three ways: posts linking to doi.org (catches proper
  # links), and posts using topical keywords for either replications or
  # retractions (catches a bare "10.xxxx/yyyy" DOI typed with no link at all,
  # which the domain filter alone would never surface as a candidate).
  #
  # LOOKBACK_MINUTES/SEARCH_LIMIT are overridable via env vars so the same
  # script can run as the regular hourly scan (defaults: 70 min / 50 posts —
  # a 10-minute cushion over the hourly cadence) or as a one-off wider
  # backfill (e.g. 4 weeks / 1000 posts) without duplicating this logic.
  # Overlap in the lookback window is harmless: dedup is keyed on post_uri.
  # ---------------------------------------------------------
  lookback_minutes <- as.numeric(Sys.getenv("LOOKBACK_MINUTES", "70"))
  search_limit <- as.integer(Sys.getenv("SEARCH_LIMIT", "50"))
  since <- format(Sys.time() - as.difftime(lookback_minutes, units = "mins"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  candidates <- collect_candidate_posts(
    list(
      list(query = "doi.org", domain = "doi.org"),
      list(query = "replicated OR replication OR reproduced OR reproducibility", domain = NULL),
      list(query = "retracted OR retraction OR \"expression of concern\"", domain = NULL)
    ),
    since = since,
    limit = search_limit
  )

  if (length(candidates) == 0) {
    cat("No candidate posts found in this run.\n")
    return(invisible(NULL))
  }

  # ---------------------------------------------------------
  # Walk each candidate: skip our own posts, posts already linking FLoRA or
  # Retraction Watch, and posts already replied to; otherwise try to match a
  # known DOI (replication first, then retraction) and reply
  # ---------------------------------------------------------
  new_log_rows <- list()

  for (post in candidates) {
    post_uri <- post$uri
    if (is.null(post_uri) || post_uri %in% already_replied_uris) next

    author_handle <- post$author$handle
    if (!is.null(author_handle) && tolower(author_handle) == tolower(bsky_handle)) next

    post_text <- post$record$text
    if (!is.null(post_text) && grepl("flora-replication-atlas|retractionwatch", post_text, ignore.case = TRUE)) next

    raw_dois <- extract_dois_from_post(post)
    if (length(raw_dois) == 0) next
    candidate_dois <- unique(normalize_doi(raw_dois))

    matched_flora <- candidate_dois[candidate_dois %in% flora_dois]
    matched_retraction <- candidate_dois[candidate_dois %in% retraction_dois]

    kind <- NA_character_
    notice_type <- NA_character_
    doi <- NA_character_
    reply_text <- NA_character_

    if (length(matched_flora) > 0) {
      doi <- matched_flora[1]
      kind <- "replication"

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
    } else if (length(matched_retraction) > 0) {
      doi <- matched_retraction[1]

      row <- retraction_valid[retraction_valid$doi_o_norm == doi, ][1, ]
      label <- notice_label(row$RetractionNature)
      if (is.na(label)) next

      kind <- "retraction"
      notice_type <- row$RetractionNature

      reason <- lowercase_first(first_semicolon_field(row$Reason))
      # RetractionDOI is the notice's own DOI — link there, not to the
      # retracted paper's OriginalPaperDOI, so the link leads to the notice
      # itself. URLS (a Retraction Watch article about the notice) is only a
      # fallback for the ~0.4% of rows where RetractionDOI is blank.
      link <- get_link(row$RetractionDOI, first_semicolon_field(row$URLS), NA)

      short_reply <- sprintf("Did you know that there is a %s for this study? %s", label, link)
      full_reply <- if (!is.na(reason)) {
        sprintf("Did you know that there is a %s for this study? Reason: %s.\n\n%s", label, reason, link)
      } else {
        short_reply
      }

      # Bluesky posts cap at 300 characters; fall back to the shorter wording
      # (no reason) rather than skip the reply or risk truncation.
      reply_text <- if (nchar(full_reply, type = "chars") <= 300) full_reply else short_reply
    } else {
      next
    }

    cat("Match found - post:", post_uri, "- kind:", kind, "- DOI:", doi, "\n")

    if (dry_run) {
      cat("[DRY RUN] Would reply:", reply_text, "\n\n")
      next
    }

    # embed = FALSE: without this, bs_post() tries to auto-generate a link
    # preview card for the URL in reply_text, which is the same bskyr embed
    # bug bot.R's get_link() already works around — it caused every reply
    # attempt to fail with an HTTP 400 before this fix was added.
    post_result <- tryCatch({
      bs_post(text = reply_text, reply = post_uri, embed = FALSE)
    }, error = function(e) {
      cat("Warning: failed to reply to", post_uri, "-", conditionMessage(e), "\n")
      NULL
    })

    if (!is.null(post_result)) {
      # Capture the reply's own post URI (not just the original post's) so
      # index.html can fetch its thread directly — Bluesky's public API
      # doesn't reliably list a fresh reply in the original post's replies,
      # so hunting for the bot's handle there isn't a robust way to find it.
      reply_uri <- tryCatch(as.character(post_result$uri[[1]]), error = function(e) NA_character_)

      new_log_rows[[length(new_log_rows) + 1]] <- data.frame(
        post_uri = post_uri,
        reply_uri = reply_uri,
        doi = doi,
        kind = kind,
        notice_type = notice_type,
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
