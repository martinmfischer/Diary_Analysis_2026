# ==============================================================================
# 01 Daten einlesen und gemeinsame Analysestichprobe speichern
# ==============================================================================
# Ausführung: im Projekt-Hauptverzeichnis in einer neuen R-Sitzung
# Eingaben: drei rohe Fragebogen-CSV-Dateien und dokumentierte Dateiausschlüsse
# Ausgaben: drei RDS-Dateien in 01_Data und Stichprobenpruefung.csv in 03_Output
# Die manuelle Codierung wird weder erstellt noch verändert

# 01 Pakete und gemeinsame Funktionen -------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, stringr, openxlsx)
source("02_Scripts/funktionen/Daten_und_Statistik.R", encoding = "UTF-8")
dir.create("03_Output", showWarnings = FALSE)

# 02 Dauerhafte Zuordnung der früheren Pretest-Dateien prüfen --------------------

# Die früheren Screenshot-IDs hängen vom damaligen Datenimport ab
# Deshalb wird die bereits gesicherte Zuordnung zu Originaldateien verwendet
# Eine Neuberechnung aus heutigen IDs könnte andere Dateien ausschließen
pretest_map_file <- "06_Coding/pretest_excluded_original_files.csv"
if (!file.exists(pretest_map_file)) {
  stop(
    "Die dokumentierte Pretest-Dateizuordnung fehlt: ", pretest_map_file,
    "\nBitte aus der Datensicherung wiederherstellen; nicht aus neuen IDs ableiten"
  )
}

# 03 Rohdaten einlesen und Personen auswählen -----------------------------------

# Verbindlich: abgeschlossenes Intro und Outro, erfülltes Screening,
# mindestens sieben unterschiedliche Uploads nach Abzug bekannter Duplikate
# Alle Einschlussregeln stehen gemeinsam in analyseschritte/01_Stichprobe.R
source("02_Scripts/analyseschritte/01_Stichprobe.R", encoding = "UTF-8")
screening <- intro_raw %>% filter(participant %in% study_ids)
outro <- outro_raw %>% filter(participant %in% study_ids)

# Auch Tage ohne Upload bleiben als Zeilen erhalten, damit der Kalenderbezug
# bestehen bleibt. Der Screenshot-Index verwendet später nur befüllte Uploadfelder
diary <- upload_raw %>% filter(participant %in% study_ids)

# 04 Aufbereitete Fragebogendaten speichern -------------------------------------

saveRDS(screening, "01_Data/screening-befragung_tagebuchstudie.rds")
saveRDS(diary, "01_Data/taeglicher_fragebogen_screenshot_upload.rds")
saveRDS(outro, "01_Data/abschlussbefragung_tagebuchstudie.rds")
message(
  "Analyse-Import: ", length(study_ids), " Personen; ",
  sum(sample_audit$n_uploads[sample_audit$included]),
  " unterschiedliche Uploads nach bekannten Duplikaten"
)
