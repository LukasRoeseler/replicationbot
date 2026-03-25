library(bskyr)
library(jsonlite)

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

# 2. Helper function: Prioritize link
get_link <- function(doi, primary_url, fallback_url = NA) {
  if (!is.na(doi) && doi != "") return(paste0("https://doi.org/", doi))
  if (!is.na(primary_url) && primary_url != "") return(primary_url)
  if (!is.na(fallback_url) && fallback_url != "") return(fallback_url)
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

# 5. Main process
main <- function() {
  bsky_handle <- Sys.getenv("BLUESKY_HANDLE")
  bsky_password <- Sys.getenv("BLUESKY_PASSWORD")
  
  set_bluesky_user(bsky_handle)
  set_bluesky_pass(bsky_password)
  
  df <- read.csv("https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
  
  # Filter missing authors
  valid_mask <- mapply(function(author_o, year_o, author_r, year_r) {
    cit_o <- get_short_citation(author_o, year_o)
    cit_r <- get_short_citation(author_r, year_r)
    is_valid_o <- !startsWith(cit_o, "Unknown (")
    is_valid_r <- !startsWith(cit_r, "Unknown (")
    return(is_valid_o && is_valid_r)
  }, df$author_o, df$year_o, df$author_r, df$year_r)
  
  df <- df[valid_mask, ]
  
  if (nrow(df) == 0) stop("Error: No valid rows left after filtering missing authors.")
  
  # Row selection
  bot_start_date <- as.Date("2024-05-23") 
  days_running <- max(0, as.numeric(Sys.Date() - bot_start_date))
  
  set.seed(42) 
  shuffled_indices <- sample(1:nrow(df))
  list_position <- (days_running %% nrow(df)) + 1
  row_index <- shuffled_indices[list_position]
  row <- df[row_index, ]
  
  # Extract data
  title_o <- ifelse(!is.na(row$title_o), row$title_o, "")
  orig_cit <- get_short_citation(row$author_o, row$year_o)
  repl_cit <- get_short_citation(row$author_r, row$year_r)
  
  orig_link <- get_link(row$doi_o, row$oa_url_o, NA)
  repl_link <- get_link(row$doi_r, row$url_r, row$oa_url_r)
  
  study_type <- ifelse(!is.na(row$type), tolower(row$type), "unknown")
  action_verb <- ifelse(study_type == "reproduction", "reproduced", "replicated")
  link_label <- ifelse(study_type == "reproduction", "Reproduction", "Replication")
  raw_outcome <- ifelse(!is.na(row$outcome), tolower(row$outcome), "unknown")
  
  if (study_type == "reproduction") {
    middle_sentence <- sprintf("According to the reproduction authors, %s.", format_reproduction_outcome(raw_outcome))
  } else {
    middle_sentence <- sprintf("According to the replication authors, %s.", format_replication_outcome(raw_outcome))
  }
  
  # Text building - HINWEIS: Leerzeichen nach %s eingefügt, um Facet-Parsing-Fehler zu vermeiden!
  base_text <- sprintf(
    "%s was %s by %s. %s\n\nOriginal: %s \n%s: %s ",
    orig_cit, action_verb, repl_cit, middle_sentence, orig_link, link_label, repl_link
  )
  
  # Limit auf 280 heruntergesetzt für absolute Sicherheit
  available_space <- 280 - nchar(base_text, type = "chars")
  
  if (title_o != "" && available_space > 10) {
    if (nchar(title_o, type = "chars") > available_space) {
      short_title <- paste0(substr(title_o, 1, available_space - 3), "...")
      title_insert <- sprintf(", \"%s\"", short_title)
    } else {
      title_insert <- sprintf(", \"%s\"", title_o)
    }
    
    post_text <- sprintf(
      "%s%s was %s by %s. %s\n\nOriginal: %s \n%s: %s ",
      orig_cit, title_insert, action_verb, repl_cit, middle_sentence, orig_link, link_label, repl_link
    )
  } else {
    post_text <- base_text
  }
  
  cat("Attempting to post the following text (Day", days_running, "- Row", row_index, "):\n", post_text, "\n", "Length:", nchar(post_text), "characters\n\n")
  
  # --- Fehler abfangen und exakten Bluesky Server-Error auslesen ---
  tryCatch({
    bs_post(text = post_text)
    cat("Successfully posted!\n")
  }, error = function(e) {
    cat("!!! POSTING FAILED !!!\n")
    cat("R Error:", e$message, "\n\n")
    
    # Letzte API-Antwort auslesen
    resp <- httr2::last_response()
    if (!is.null(resp)) {
      cat("--- SECRET BLUESKY API ERROR DETAILS ---\n")
      cat(httr2::resp_body_string(resp), "\n")
    }
    quit(save = "no", status = 1)
  })
}

main()
