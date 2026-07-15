# Shared helper functions used by both bot.R (daily broadcast) and
# reply_bot.R (reactive replies), so their wording stays consistent.

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
