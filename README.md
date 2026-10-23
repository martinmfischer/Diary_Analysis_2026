# Auswertung der Social-Media-Tagebuchstudie

Das Projekt enthält die Analysen für das Paper sowie zwei ausgewählte Zusatzanalysen. Code, Kommentare und Ausgaben sind auf Englisch. Diese README und die lokale Übergabeanleitung sind auf Deutsch.

## Einstieg

`Diary_Analysis_2026.Rproj` in RStudio öffnen und eine neue R-Sitzung starten. Alle Aufrufe werden aus dem Projektordner ausgeführt.

```r
source("01_Scripts/01_paper_analysis.R", encoding = "UTF-8")
source("01_Scripts/checks.R", encoding = "UTF-8")
```

Der erste Aufruf berechnet die Paper-Ergebnisse. Der zweite prüft die Auswahlregeln, Bezugsgrößen und zentralen Auswertungsregeln. Pakete werden mit `pacman::p_load()` geladen.

Die Ergebnisse stehen anschließend als R-Objekte und unter `outputs/` bereit:

- `paper_tables.xlsx`: Tabellen in Manuskriptreihenfolge.
- `paper_results.rds`: gespeicherte Ergebnisse für die Berichtserstellung.
- `sample_audit.csv`: Ein- und Ausschlüsse je Person.
- `session_info.txt`: verwendete R- und Paketversionen.

## Reihenfolge der Paper-Analysen

| Abschnitt | Datei unter `01_Scripts/analysis/` | Inhalt |
| --- | --- | --- |
| Methoden und Stichprobe | `01_data_and_sample.R` | Datenimport, Einschluss, Demografie, Teilnahme und gespeicherte Pretest-Reliabilität |
| RQ1: Inhalte | `02_content.R` | Themen, Quellen, Formate, Plattformen, Werbung und Tabellen A1–A2 |
| RQ2a: Kontexte | `03_contexts.R` | Ort und Gesellschaft; Vergleich mit typischer Nutzung im Intro |
| RQ2b: Incidentality | `04_incidentalness.R` | Mittelwertindex, Alpha/Omega, Begegnungsarten, Survey–Diary-Vergleich mit Bootstrapintervallen und zwei Themenmodelle |
| RQ2c: Praktiken | `05_practices.R` | Lesen/Ansehen, Recherche, Interaktion und „Nichts davon“; Anteile je Beitrag und je Person |
| Export | `06_export.R` | Gemeinsame Excel-Datei und gespeicherte Ergebnisse für die Reports |

Die Themenmodelle bleiben erhalten, weil ihre Befunde im aktuellen Manuskript stehen. Nicht berichtete Themenpaarvergleiche und das zusätzliche Themenprofilmodell wurden entfernt. Bootstrapintervalle und Omega gehören jetzt zum normalen Paper-Durchlauf.

## Zwei zusätzliche Explorationen

```r
source("01_Scripts/02_selected_exploration.R", encoding = "UTF-8")
```

1. **Facebook und Instagram bei denselben Personen:** Themen- und Quellenprofile, mindestens drei eingereichte Beiträge je Plattform. Jede Person zählt gleich. Der Vergleich ist beschreibend.
2. **Informationsbedürfnisse und Praktiken:** Drei ausgewählte Zusammenhänge: Problemlösung und Recherche, thematisches Interesse und gezielte Suche, allgemeine Orientierung und Lesen/Ansehen. Je Person ein Wertepaar; Holm-Korrektur über die drei Tests.

Das Skript liest die gemeinsame Datenbasis selbst ein. Es berechnet die Paper-Modelle nicht erneut und verändert die Paper-Ergebnisse nicht. Die Ausgaben heißen `selected_exploration.xlsx` und `selected_exploration.rds`.

## Berichte und Abbildungen

Zuerst die jeweilige Analyse ausführen, danach bei Bedarf:

```r
source("01_Scripts/reporting/01_paper_report.R", encoding = "UTF-8")
source("01_Scripts/reporting/02_exploration_report.R", encoding = "UTF-8")
```

Die Reports lesen gespeicherte Ergebnisse und erzeugen Markdown, PDF und Abbildungen unter `outputs/`. Sie rechnen die Analysen nicht erneut. Für Berichte werden zusätzlich ggplot2, knitr, rmarkdown, Pandoc und XeLaTeX benötigt. Die reine Analyse braucht keine PDF-Software.

Die drei Paper-Abbildungen stehen in `01_Scripts/reporting/paper_figures.R`. Figure 1 hebt die drei häufigsten Themen je Begegnungsart hervor. Einen Report erweitert man durch einen weiteren `report_section()`-Aufruf mit Tabelle, kurzer Zweckangabe und optionalem Bildpfad.

## Benötigte lokale Daten

| Datei | Inhalt |
| --- | --- |
| `01_Data/initial_survey.csv` | Originaler Intro-Export |
| `01_Data/daily_survey.csv` | Originaler Daily-Export |
| `01_Data/closing_survey.csv` | Originaler Outro-Export |
| `06_Coding/coding_sheet.xlsx` | Finale Codierung und Codebook |
| `06_Coding/upload_exclusions.csv` | Vier dokumentierte technische Ausschlüsse |
| `06_Coding/duplicate_uploads.csv` | 23 dokumentierte Duplikate aus der ursprünglichen Coding-Sicherung |
| `07_Pretest/pretest_reliability.xlsx` | Gespeicherte Reliabilitätswerte für den Methodenteil |

Die drei Fragebogen-CSV-Dateien sind mit Semikolon getrennt. Ihre Dateinamen wurden übersetzt; die Inhalte blieben unverändert. Originale Fragebogentexte, Rohvariablen und Bildnamen bleiben erhalten. Die Analyse öffnet keine Teilnehmerbilder und benötigt keine Datei aus `Backups/`.

## Wichtige Auswertungsregeln

- Screening erfüllt, Intro und Outro abgeschlossen (`committed` vorhanden), mindestens sieben unterschiedliche Uploads nach Ausschlüssen. Keine Mindestzahl an Uploadtagen und keine zusätzlichen Abschlussflags.
- Incidentality: Mittelwert aller sechs Items, Item 5 umgepolt, nur vollständige Antworten. Alpha und Omega werden mit `psych` berechnet; Omega behält die im Manuskript verwendete explorative Spezifikation mit drei Gruppenfaktoren.
- Zufallsbegegnungen: Code 3. Begegnungen ohne gezielte Suche: Codes 2 und 3. Diese beiden Zusammenfassungen überlappen.
- Dreimal −1 bei den Praxisantworten bedeutet „Nichts davon“ und wird zu dreimal 0. Echte fehlende Angaben bleiben fehlend.
- Bei mittleren Personenanteilen zählt jede Person gleich. Bei Praktiken nach Begegnungsart gehen nur Personen mit gültigen Beiträgen dieser Art ein; die Gruppen können sich überschneiden und unterschiedlich groß sein.
- Restkategorien bleiben gültige Inhaltskategorien. „Weiß nicht mehr“ und fehlende Kontexte entfallen beim Kontextvergleich auf Personenebene.

## Git und Übergabe

Daten, Outputs, `Documentation/` und `Backups/` werden nicht versioniert. Ein Git-Checkout enthält deshalb nicht alle zum Rechnen benötigten Dateien. Die lokalen Eingaben müssen separat mitgegeben werden.

Die [Übergabeanleitung](Documentation/Handover.md) erklärt die Weiterarbeit mit einem Beispiel. Frühere Skripte und Unterlagen liegen gesichert unter `Backups/English_Refactor_2026-10-23/`. Die alten Verwaltungs-, Vorbereitungs- und umfangreichen Zusatzskripte gehören nicht mehr zum aktiven Ablauf.
