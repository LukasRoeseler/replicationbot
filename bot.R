# Benötigte Pakete laden
install.packages("bskyr")
install.packages("jsonlite")

library(bskyr)
library(jsonlite)

# 1. Hilfsfunktion: JSON-Autoren in eine Kurzzitation umwandeln (z.B. "Name et al. (Jahr)")
get_short_citation <- function(author_json, year) {
  if (is.na(author_json) || author_json == "") return(paste0("Unknown (", year, ")"))
  
  # Fehler abfangen, falls das JSON nicht perfekt formatiert ist
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

# 3. Hauptprozess
main <- function() {
  # Zugangsdaten aus den GitHub Secrets laden
  bsky_handle <- Sys.getenv("BLUESKY_HANDLE")
  bsky_password <- Sys.getenv("BLUESKY_PASSWORD")
  
  # Authentifizierung bei Bluesky
  set_bluesky_user(bsky_handle)
  set_bluesky_pass(bsky_password)
  
  # Datensatz laden (Passe den Dateinamen an, falls er anders heißt!)
  df <- read.csv("https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
  
  # Tag des Jahres ermitteln (1 bis 365/366)
  day_of_year <- as.numeric(format(Sys.Date(), "%j"))
  
  # Zeile auswählen (Modulo-Rechnung, falls mehr Tage vergangen sind als Zeilen existieren)
  # R nutzt 1-basierten Index, daher +1
  row_index <- (day_of_year %% nrow(df)) + 1
  row <- df[row_index, ]
  
  # Daten extrahieren und formatieren
  orig_cit <- get_short_citation(row$author_o, row$year_o)
  repl_cit <- get_short_citation(row$author_r, row$year_r)
  
  orig_link <- get_link(row$doi_o, row$oa_url_o)
  repl_link <- get_link(row$doi_r, row$url_r)
  
  # Typ und Outcome bestimmen
  action_verb <- ifelse(!is.na(row$type) && tolower(row$type) == "reproduction", "reproduced", "replicated")
  outcome <- ifelse(!is.na(row$outcome), tolower(row$outcome), "unknown")
  
  # Post-Text zusammenbauen
  post_text <- sprintf(
    "%s was %s by %s. The study was %s.\n\nOriginal: %s\nReplication: %s",
    orig_cit, action_verb, repl_cit, outcome, orig_link, repl_link
  )
  
  # In der Konsole ausgeben (hilfreich für die GitHub Actions Logs)
  cat("Versuche folgenden Text zu posten:\n", post_text, "\n\n")
  
  # Auf Bluesky posten (bskyr erkennt Links automatisch und formatiert sie korrekt)
  bs_post(text = post_text)
  
  cat("Erfolgreich gepostet!\n")
}

# Skript ausführen
main()
