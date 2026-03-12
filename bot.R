library(bskyr)
library(jsonlite)

# 1. Helper function: Convert JSON authors to a short citation (e.g., "Name et al. (Year)")
get_short_citation <- function(author_json, year) {
  if (is.na(author_json) || author_json == "") return(paste0("Unknown (", year, ")"))
  
  # Catch errors in case the JSON is not formatted perfectly
  authors <- tryCatch({
    jsonlite::fromJSON(author_json)
  }, error = function(e) return(NULL))
  
  if (is.null(authors) || !("family" %in% names(authors))) {
    return(paste0("Unknown (", year, ")"))
  }
  
  families <- authors$family
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

# 2. Helper function: Prioritize link (DOI first, then primary URL, then Open Access URL)
get_link <- function(doi, primary_url, fallback_url = NA) {
  # 1. Check if DOI exists
  if (!is.na(doi) && doi != "") return(paste0("https://doi.org/", doi))
  
  # 2. Check primary URL (e.g., url_r)
  if (!is.na(primary_url) && primary_url != "") return(primary_url)
  
  # 3. Check fallback URL (e.g., oa_url_r)
  if (!is.na(fallback_url) && fallback_url != "") return(fallback_url)
  
  # 4. If everything is missing
  return("No link available")
}

# 3. Helper function: Convert raw reproduction outcomes into readable sentences
format_reproduction_outcome <- function(outcome) {
  # Fix typo in the original FReD dataset ("computionally" -> "computationally")
  outcome <- gsub("computionally", "computationally", outcome)
  
  # Dictionary mapping raw outcomes to full sentences
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
    return(paste("the outcome was:", outcome)) # Fallback for unknown categories
  }
}

# 4. Main process
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
  
  # Create a logical mask checking if both citations are valid (don't start with "Unknown")
  valid_mask <- mapply(function(author_o, year_o, author_r, year_r) {
    cit_o <- get_short_citation(author_o, year_o)
    cit_r <- get_short_citation(author_r, year_r)
    
    is_valid_o <- !startsWith(cit_o, "Unknown (")
    is_valid_r <- !startsWith(cit_r, "Unknown (")
    
    return(is_valid_o && is_valid_r)
  }, df$author_o, df$year_o, df$author_r, df$year_r)
  
  # Keep only valid rows
  df <- df[valid_mask, ]
  
  # Stop if no rows are left (just a safety net)
  if (nrow(df) == 0) {
    stop("Error: No valid rows left after filtering missing authors.")
  }
  
  # ---------------------------------------------------------
  # Select today's row (random but consistent across years)
  # ---------------------------------------------------------
  
  # Set the bot's start date (IMPORTANT: Change this to today's date, e.g., "2024-05-23")
  bot_start_date <- as.Date("2024-05-23") 
  
  # Calculate how many days the bot has been running (max(0) prevents errors before start date)
  days_running <- max(0, as.numeric(Sys.Date() - bot_start_date))
  
  # Fixed seed: The dataset is always shuffled exactly the same way
  set.seed(42) 
  shuffled_indices <- sample(1:nrow(df))
  
  # Calculate position in the shuffled list (modulo ensures it loops back if days > rows)
  list_position <- (days_running %% nrow(df)) + 1
  
  # Select the row
  row_index <- shuffled_indices[list_position]
  row <- df[row_index, ]
  
  # ---------------------------------------------------------
  # Extract and format data
  # ---------------------------------------------------------
  title_o <- ifelse(!is.na(row$title_o), row$title_o, "")
  orig_cit <- get_short_citation(row$author_o, row$year_o)
  repl_cit <- get_short_citation(row$author_r, row$year_r)
  
  # Advanced link generation (DOI -> URL -> OA URL)
  orig_link <- get_link(row$doi_o, row$oa_url_o, NA)
  repl_link <- get_link(row$doi_r, row$url_r, row$oa_url_r)
  
  # Determine study type and verbs
  study_type <- ifelse(!is.na(row$type), tolower(row$type), "unknown")
  action_verb <- ifelse(study_type == "reproduction", "reproduced", "replicated")
  link_label <- ifelse(study_type == "reproduction", "Reproduction", "Replication")
  
  # Determine the outcome and construct the middle sentence
  raw_outcome <- ifelse(!is.na(row$outcome), tolower(row$outcome), "unknown")
  
  if (study_type == "reproduction") {
    middle_sentence <- sprintf("According to the reproduction authors, %s.", format_reproduction_outcome(raw_outcome))
  } else {
    middle_sentence <- sprintf("According to the replication authors, the replication attempt was %s.", raw_outcome)
  }
  
  # ---------------------------------------------------------
  # Build post text & check character limit (max 300)
  # ---------------------------------------------------------
  
  # 1. Generate text WITHOUT the title to measure its length
  base_text <- sprintf(
    "%s was %s by %s. %s\n\nOriginal: %s\n%s: %s",
    orig_cit, action_verb, repl_cit, middle_sentence, orig_link, link_label, repl_link
  )
  
  # 2. Calculate remaining space for the title
  # We subtract 6 characters as a buffer and for the punctuation: `, ""`
  available_space <- 300 - nchar(base_text, type = "chars") - 6
  
  # 3. Insert the title (or truncate it) if there is enough space (> 10 characters)
  if (title_o != "" && available_space > 10) {
    if (nchar(title_o, type = "chars") > available_space) {
      # Title is too long: truncate and append "..."
      short_title <- paste0(substr(title_o, 1, available_space - 3), "...")
      title_insert <- sprintf(", \"%s\"", short_title)
    } else {
      # Title fits completely
      title_insert <- sprintf(", \"%s\"", title_o)
    }
    
    # Generate the final text WITH the title
    post_text <- sprintf(
      "%s%s was %s by %s. %s\n\nOriginal: %s\n%s: %s",
      orig_cit, title_insert, action_verb, repl_cit, middle_sentence, orig_link, link_label, repl_link
    )
  } else {
    # Not enough space for the title, fallback to the base version
    post_text <- base_text
  }
  
  # ---------------------------------------------------------
  # Send post to Bluesky
  # ---------------------------------------------------------
  
  # Print to console (helpful for debugging in GitHub Actions logs)
  cat("Attempting to post the following text (Day", days_running, "- Row", row_index, "):\n", post_text, "\n", "Length:", nchar(post_text), "characters\n\n")
  
  # Send the post
  bs_post(text = post_text)
  
  cat("Successfully posted!\n")
}

# Execute the script
main()