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

# 2. Helper function: Extract a raw DOI-like token from a post's plain text
# Note: only catches doi.org links that appear as visible text in the post body.
# Posts where the doi.org link is only present via an embed/link-card (no visible
# URL text) will not be caught by this v1 approach.
extract_doi_from_text <- function(text) {
  if (length(text) == 0 || is.na(text) || text == "") return(NA_character_)
  m <- regmatches(text, regexpr("(?i)doi\\.org/(10\\.[0-9]{4,9}/[^\\s\"'<>\\)\\],]+)", text, perl = TRUE))
  if (length(m) == 0 || m == "") return(NA_character_)
  m
}

# 3. Helper function: Robustly read a field off a single search-result row,
# regardless of whether bskyr returns nested objects (e.g. `author`, `record`)
# as flattened columns (e.g. `author_handle`) or as list-columns holding a
# nested one-row tibble/list (e.g. `author[[1]]$handle`).
get_post_field <- function(post, obj_col, field, flat_col = paste0(obj_col, "_", field)) {
  if (flat_col %in% names(post)) {
    val <- post[[flat_col]]
    if (length(val) >= 1 && !is.na(val[[1]])) return(as.character(val[[1]]))
  }
  if (obj_col %in% names(post)) {
    obj <- post[[obj_col]]
    if (is.list(obj)) {
      inner <- obj
      if (length(obj) == 1 && is.list(obj[[1]])) inner <- obj[[1]]
      if (field %in% names(inner)) {
        val <- inner[[field]]
        if (length(val) >= 1 && !is.na(val[[1]])) return(as.character(val[[1]]))
      }
    }
  }
  NA_character_
}

# 4. Helper functions: Read/write the dedup log of posts already replied to
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

# 5. Main process
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
  # Search recent posts linking to doi.org
  # ---------------------------------------------------------
  since <- format(Sys.time() - as.difftime(25, units = "hours"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  candidates <- tryCatch({
    bs_search_posts(query = "doi.org", domain = "doi.org", sort = "latest", since = since, limit = 50)
  }, error = function(e) {
    cat("Warning: bs_search_posts() failed:", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(candidates) || nrow(candidates) == 0) {
    cat("No candidate posts found in this run.\n")
    return(invisible(NULL))
  }

  # ---------------------------------------------------------
  # Walk each candidate: skip our own posts, posts already linking FLoRA,
  # and posts already replied to; otherwise try to match a known DOI and reply
  # ---------------------------------------------------------
  new_log_rows <- list()

  for (i in seq_len(nrow(candidates))) {
    post <- candidates[i, ]

    post_uri <- post$uri[[1]]
    if (is.na(post_uri) || post_uri %in% already_replied_uris) next

    author_handle <- get_post_field(post, "author", "handle")
    if (!is.na(author_handle) && tolower(author_handle) == tolower(bsky_handle)) next

    post_text <- get_post_field(post, "record", "text")
    if (!is.na(post_text) && grepl("flora-replication-atlas", post_text, ignore.case = TRUE)) next

    raw_doi <- extract_doi_from_text(post_text)
    if (is.na(raw_doi)) next

    doi <- normalize_doi(raw_doi)
    if (!(doi %in% flora_dois)) next

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

    result <- tryCatch({
      bs_post(text = reply_text, reply = post_uri)
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
