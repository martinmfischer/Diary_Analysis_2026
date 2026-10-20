# Tagebuchstudie: Analyseprojekt

Das Projekt wird über `Diary_Analysis_2026.Rproj` geöffnet. Alle Pfade beziehen sich auf das Projekt-Hauptverzeichnis. Skripte möglichst in einer neuen R-Sitzung ausführen; gespeicherte `.RData`-Dateien sind keine Voraussetzung

## Analyseablauf

| Reihenfolge | Skript | Zweck und Ausgaben |
| --- | --- | --- |
| 01 | [Daten einlesen](02_Scripts/01_Daten_einlesen.R) | Rohe Fragebogenexporte einlesen, gemeinsame Personenbasis bestimmen, drei aufbereitete RDS-Dateien speichern |
| 02 | [Deskriptive Auswertung](02_Scripts/02_Deskriptive_Auswertung.R) | Stichprobe, Fragebogenantworten, codierte Inhalte, Messqualität und Datenabdeckung; Bericht `03_Output/Analyse_Essentials.md` samt PDF und Grafiken |
| 03 | [Explorative Auswertung](02_Scripts/03_Explorative_Auswertung.R) | Zusammenhänge und Vergleiche zwischen Personen, Plattformen, Begegnungsarten und Kontexten; Bericht `03_Output/Exploration.md` samt PDF und Grafiken |

```r
source("02_Scripts/01_Daten_einlesen.R", encoding = "UTF-8")
source("02_Scripts/02_Deskriptive_Auswertung.R", encoding = "UTF-8")
source("02_Scripts/03_Explorative_Auswertung.R", encoding = "UTF-8")
```

Die beiden Berichte können einzeln ausgeführt werden. Beide laden die gemeinsame Stichprobendefinition aus `analyseschritte/01_Stichprobe.R` und verwenden dieselben Ein- und Ausschlussregeln. Der separate Datenimport erzeugt RDS-Dateien für die Codiersheet-Vorbereitung; er ist keine Voraussetzung für die Berichte.

**Verbindliche Auswahl seit 16.10.2026:** Screening erfüllt, Intro und Outro jeweils abgeschlossen (`committed` vorhanden), mindestens sieben unterschiedliche Rohuploads nach Abzug bekannter Duplikate. Keine Mindestzahl an Uploadtagen. Die ergänzende Sensitivitätsanalyse lockert nur die Uploadgrenze auf einen Upload und behält beide Fragebogenabschlüsse bei. Alle Codierphasen im Coding Sheet fließen ein

Vier historisch ungecodete Dateien sind zusätzlich ausdrücklich ausgeschlossen und in `06_Coding/upload_exclusions.csv` dokumentiert. Die Uploadgrenze und die berichteten Uploadzahlen verwenden die danach verbleibenden Dateien. Aktueller finaler Korpus: 105 Personen mit 3.288 berücksichtigten Uploads und 3.288 Codierungen. Die Rohdaten bleiben unverändert

## Verzeichnisse

| Verzeichnis | Inhalt |
| --- | --- |
| `01_Data/` | Rohe Fragebogenexporte, Originaldateien und aufbereitete RDS-Dateien |
| `02_Scripts/` | Die drei Einstiegsskripte in ihrer Ausführungsreihenfolge |
| `02_Scripts/analyseschritte/` | Gemeinsam verwendete Auswertungsschritte; werden an dokumentierter Stelle von den Einstiegsskripten geladen |
| `02_Scripts/funktionen/` | Wiederverwendbare Funktionen ohne Datenimport oder Dateiausgabe |
| `02_Scripts/berichte/` | Darstellung, Erläuterungen und PDF-Erstellung; notwendige LaTeX-/Lua-Dateien unter `vorlagen/` |
| `02_Scripts/vorbereitung/` | Separater Ablauf für Codiersheet-Erstellung und Pretest-Reliabilität |
| `02_Scripts/administration/` | Vergütung; kein Bestandteil der wissenschaftlichen Analyse |
| `02_Scripts/pruefungen/` | Technische Prüfungen von Syntax und Dateiverweisen |
| `03_Output/` | Berichte, Tabellen, Prüfprotokolle und dokumentierte frühere Codieränderungen |
| `04_Figures/` | Vorhandene ältere Grafiken; aktuelle Berichtsgrafiken liegen bei den Berichten |
| `05_Participants/` | Den Codierenden zugeordnete Screenshots; bestehende Pfade bleiben erhalten |
| `06_Coding/` | Maßgebliches manuelles Coding Sheet, Codebook und dokumentierte Ausschlüsse |
| `07_Pretest/`, `07_Pretest_Archiv/` | Pretest-Daten und historische Pretest-Stände |
| `Archive/` | Komprimierte Skripthistorie, alte Arbeitsdateien und historische Protokolle; kein Bestandteil des aktiven Ablaufs |
| `_Backups_Coding/` | Bestehende Sicherungen der manuellen Codierung |
| `tmp/` | Lokale, nicht versionierte Arbeitsdateien und PDF-Zwischendateien |

## Separate Abläufe

- `vorbereitung/01_Codingsheet_erstellen.R` erstellt ein neues Coding Sheet und verteilt Bildkopien auf die Codierenden; vorhandene Codierungen sind durch `overwrite_existing <- FALSE` geschützt
- `vorbereitung/02_Pretest_Reliabilitaet.R` berechnet die Reliabilität aus den drei Pretest-Dateien; dieser Bootstrap ist kein notwendiger Bestandteil jedes Berichtslaufs
- `administration/01_Verguetung.R` erstellt die Vergütungsübersicht; seine Teilnahme- und Vergütungsregeln sind unabhängig von der Analyseentscheidung zu prüfen

Diese Skripte werden bewusst nicht automatisch durch die drei Einstiegsskripte ausgeführt. Die früheren Pretest-Auswahllisten sind im Archiv nachvollziehbar; die aktive Codiersheet-Erstellung verwendet die gespeicherte Zuordnung in `06_Coding/pretest_excluded_original_files.csv`

## Voraussetzungen und Arbeitsstil

- Benötigte R-Pakete sind am Anfang der jeweiligen Einstiegsskripte aufgeführt und werden wie bisher über `pacman` geladen
- Für PDFs werden `rmarkdown`, Pandoc und XeLaTeX benötigt; LaTeX und Lua dienen ausschließlich dem Berichtslayout
- Daten und Coding-Excel-Dateien werden durch `.gitignore` nicht vollständig versioniert; ein Git-Checkout allein enthält deshalb nicht alle erforderlichen Eingaben
- Pro Verarbeitungsschritt eine verständliche Pipe, sprechende Objektnamen, zwei Leerzeichen Einrückung und nummerierte Abschnitte
- Variablennamen aus Fragebögen und Coding Sheet bleiben stabil; Kommentare erläutern Bezugsbasis, fehlende Werte und methodische Entscheidungen
- Teilskripte dokumentieren ihre benötigten Objekte und Rückgaben; für einen vollständigen Lauf immer das Einstiegsskript verwenden

## Wartung

Der ausführliche [Code-Audit](CODE_AUDIT.md) dokumentiert behobene Fehler, offene Risiken und den Vergleich der Ergebnistabellen vor und nach dem Refactor. Ausführbare Prüfungen stehen in `02_Scripts/pruefungen/`: Syntax und Dateiverweise, Inklusionskriterien sowie gemeinsame Funktionen. Die Inklusionsprüfung setzt den Datenimport voraus.

Gemeinsame Daten-, Statistik- und Markdown-Hilfen stehen in `02_Scripts/funktionen/Daten_und_Statistik.R`. Analyseabschnitte liefern benannte Zwischenergebnisse an die beiden Berichtsskripte. Änderungen an Einschlussregeln erfolgen ausschließlich in `analyseschritte/01_Stichprobe.R`; danach beide Berichte neu erzeugen.
