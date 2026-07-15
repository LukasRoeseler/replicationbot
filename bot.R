library(bskyr)
library(jsonlite)

# Shared helpers: get_short_citation(), get_link(), format_reproduction_outcome(),
# format_replication_outcome() — also used by reply_bot.R
source("helpers.R")

# 5. Main process
main <- function() {
  # Load credentials from GitHub Secrets
  bsky_handle <- Sys.getenv("BLUESKY_HANDLE")
  bsky_password <- Sys.getenv("BLUESKY_PASSWORD")
  
  # Authenticate with Bluesky
  set_bluesky_user(bsky_handle)
  set_bluesky_pass(bsky_password)
  
  # Load the dataset live from GitHub
  df <- read.csv("https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
  
  # ---------------------------------------------------------
  # Filter out rows with missing authors ("Unknown")
  # ---------------------------------------------------------
  valid_mask <- mapply(function(author_o, year_o, author_r, year_r) {
    cit_o <- get_short_citation(author_o, year_o)
    cit_r <- get_short_citation(author_r, year_r)
    
    is_valid_o <- !startsWith(cit_o, "Unknown (")
    is_valid_r <- !startsWith(cit_r, "Unknown (")
    
    return(is_valid_o && is_valid_r)
  }, df$author_o, df$year_o, df$author_r, df$year_r)
  
  df <- df[valid_mask, ]
  
  if (nrow(df) == 0) {
    stop("Error: No valid rows left after filtering missing authors.")
  }
  
  # ---------------------------------------------------------
  # Select today's row (random but consistent across years)
  # ---------------------------------------------------------
  bot_start_date <- as.Date("2024-05-23") 
  days_running <- max(0, as.numeric(Sys.Date() - bot_start_date))
  
  set.seed(42) 
  shuffled_indices <- sample(1:nrow(df))
  list_position <- (days_running %% nrow(df)) + 1
  row_index <- shuffled_indices[list_position]
  row <- df[row_index, ]
  
  # ---------------------------------------------------------
  # Extract and format data
  # ---------------------------------------------------------
  title_o <- ifelse(!is.na(row$title_o), row$title_o, "")
  orig_cit <- get_short_citation(row$author_o, row$year_o)
  repl_cit <- get_short_citation(row$author_r, row$year_r)
  
  orig_link <- get_link(row$doi_o, row$oa_url_o, NA)
  repl_link <- get_link(row$doi_r, row$url_r, row$oa_url_r)
  
  study_type <- ifelse(!is.na(row$type), tolower(row$type), "unknown")
  action_verb <- ifelse(study_type == "reproduction", "reproduced", "replicated")
  link_label <- ifelse(study_type == "reproduction", "Reproduction", "Replication")
  
  raw_outcome <- ifelse(!is.na(row$outcome), tolower(row$outcome), "unknown")
  
  # Apply the correct grammar mapping based on the study type
  if (study_type == "reproduction") {
    middle_sentence <- sprintf("According to the reproduction authors, %s.", format_reproduction_outcome(raw_outcome))
  } else {
    middle_sentence <- sprintf("According to the replication authors, %s.", format_replication_outcome(raw_outcome))
  }
  
  # ---------------------------------------------------------
  # Build post text & check character limit (max 300)
  # ---------------------------------------------------------
  base_text <- sprintf(
    "%s was %s by %s. %s\n\nOriginal: %s\n%s: %s",
    orig_cit, action_verb, repl_cit, middle_sentence, orig_link, link_label, repl_link
  )
  
  available_space <- 300 - nchar(base_text, type = "chars") - 6
  
  if (title_o != "" && available_space > 10) {
    if (nchar(title_o, type = "chars") > available_space) {
      short_title <- paste0(substr(title_o, 1, available_space - 3), "...")
      title_insert <- sprintf(", \"%s\"", short_title)
    } else {
      title_insert <- sprintf(", \"%s\"", title_o)
    }
    
    post_text <- sprintf(
      "%s%s was %s by %s. %s\n\nOriginal: %s\n%s: %s",
      orig_cit, title_insert, action_verb, repl_cit, middle_sentence, orig_link, link_label, repl_link
    )
  } else {
    post_text <- base_text
  }
  
  # ---------------------------------------------------------
  # Send post to Bluesky
  # ---------------------------------------------------------
  cat("Attempting to post the following text (Day", days_running, "- Row", row_index, "):\n", post_text, "\n", "Length:", nchar(post_text), "characters\n\n")
  
  bs_post(text = post_text)
  
  cat("Successfully posted!\n")
}

main()