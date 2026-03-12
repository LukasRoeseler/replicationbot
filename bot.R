library(bskyr)
library(jsonlite)

# 1. Hilfsfunktion: JSON-Autoren in eine Kurzzitation umwandeln (z.B. "Name et al. (Jahr)")
get_short_citation <- function(author_json, year) {
  if (is.na(author_json) || author_json == "") return(paste0("Unknown (", year, ")"))
  
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

# 2. Hilfsfunktion: Link priorisieren (DOI zuerst, wenn nicht vorhanden URL)
get_link <- function(doi, url) {
  if (!is.na(doi) && doi != "") return(paste0("https://doi.org/", doi))
  if (!is.na(url) && url != "") return(url)
  return("No link available")
}

# 3. Hilfsfunktion: Rohe Reproduktions-Outcomes in schöne Sätze verwandeln
format_reproduction_outcome <- function(outcome) {
  # Tippfehler in der Original-CSV abfangen ("computionally" -> "computationally")
  outcome <- gsub("computionally", "computationally", outcome)
  
  # Wörterbuch für die genauen Sätze
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
    return(paste("the outcome was:", outcome)) # Fallback, falls ein neuer Typ in der Datenbank auftaucht
  }
}

# 4. Hauptprozess
main <- function() {
  # Zugangsdaten aus den GitHub Secrets laden
  bsky_handle <- Sys.getenv("BLUESKY_HANDLE")
  bsky_password <- Sys.getenv("BLUESKY_PASSWORD")
  
  # Authentifizierung bei Bluesky
  set_bluesky_user(bsky_handle)
  set_bluesky_pass(bsky_password)
  
  # Datensatz laden (Live von GitHub)
  df <- read.csv("https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
  
  # Tag des Jahres ermitteln (1 bis 365/366)
  day_of_year <- as.numeric(format(Sys.Date(), "%j"))
  
  # Zeile auswählen
  row_index <- (day_of_year %% nrow(df)) + 1
  row <- df[row_index, ]
  
  # Daten extrahieren und formatieren
  orig_cit <- get_short_citation(row$author_o, row$year_o)
  repl_cit <- get_short_citation(row$author_r, row$year_r)
  
  orig_link <- get_link(row$doi_o, row$oa_url_o)
  repl_link <- get_link(row$doi_r, row$url_r)
  
  # Typ bestimmen
  study_type <- ifelse(!is.na(row$type), tolower(row$type), "unknown")
  action_verb <- ifelse(study_type == "reproduction", "reproduced", "replicated")
  link_label <- ifelse(study_type == "reproduction", "Reproduction", "Replication")
  
  # Outcome bestimmen und den mittleren Satz zusammenbauen
  raw_outcome <- ifelse(!is.na(row$outcome), tolower(row$outcome), "unknown")
  
  if (study_type == "reproduction") {
    middle_sentence <- sprintf("According to the reproduction authors, %s.", format_reproduction_outcome(raw_outcome))
  } else {
    middle_sentence <- sprintf("According to the replication authors, the replication attempt was %s.", raw_outcome)
  }
  
  # Post-Text zusammenbauen
  post_text <- sprintf(
    "%s was %s by %s. %s\n\nOriginal: %s\n%s: %s",
    orig_cit, action_verb, repl_cit, middle_sentence, orig_link, link_label, repl_link
  )
  
  # In der Konsole ausgeben (hilfreich für die GitHub Actions Logs)
  cat("Versuche folgenden Text zu posten:\n", post_text, "\n\n")
  
  # Auf Bluesky posten
  bs_post(text = post_text)
  
  cat("Erfolgreich gepostet!\n")
}

# Skript ausführen
main()