# Tagebuchstudie: Analyseprojekt

Das Projekt in RStudio über `Diary_Analysis_2026.Rproj` öffnen und eine neue R-Sitzung starten. Alle Pfade beziehen sich auf den Projektordner. Eine gespeicherte `.RData` wird nicht benötigt.

Die ausführliche [Übergabeanleitung](Dokumentation/Uebergabe_Codebase.md) erklärt die gesamte aktive Codebase mit Schaubildern und einem geprüften Beispiel für eine zusätzliche Exploration. Eine [bearbeitbare Word-Fassung](Dokumentation/Uebergabe_Codebase.docx) liegt daneben.

## Für die Übergabe: hier anfangen

```r
source("02_Scripts/02_Paper_Analysen.R", encoding = "UTF-8")
```

Dieses Skript ist der Rumpf der Paper-Auswertung. Es liest die Daten selbst ein, wählt die gemeinsame Stichprobe aus und erstellt `03_Output/Paper_Tabellen.xlsx`. Tabellen und aufbereitete Daten stehen anschließend auch als R-Objekte im Environment. Es erzeugt keine Grafiken, Markdown-Berichte oder PDFs. Pandoc und LaTeX sind dafür nicht erforderlich.

Die nummerierten Abschnitte führen durch die Auswertung:

1. Datenbasis und Einschluss prüfen.
2. Stichprobe und Teilnahme beschreiben.
3. Intro: Nutzung, Informationsbedürfnisse, Incidentality-Mittelwertindex und typische Kontexte.
4. Daily: Relevanz, Themen, Quellen, Formate, Kontexte und Praktiken beschreiben.
5. Outro: Items, vollständige Mittelwertindizes und Alpha beschreiben.
6. Intro und Tagebuch auf Personenebene vergleichen.
7. Praktiken nach Begegnungsart und Format gegenüberstellen.
8. Themenunterschiede mit Mehrebenenmodellen und Personen-Bootstrap untersuchen.
9. Paper-Tabellen einschließlich Themen × Quellen, Kontextabgleich und gespeicherter Pretest-Reliabilität exportieren.

Die Modelle und der Bootstrap bleiben im Paper-Ablauf, weil ihre Befunde bereits im Manuskript verwendet werden. Sie sind explorativ; das Skript kennzeichnet ihren Zweck. Ihre längeren Berechnungen stehen unter `02_Scripts/paper/`. Der Hauptablauf zeigt, an welcher Stelle sie benötigt werden. Keine gerichteten Hypothesen nachträglich als konfirmatorisch ausweisen.

## Drei getrennte Aufgaben

| Aufgabe | Einstieg | Ausgabe |
| --- | --- | --- |
| Paper-Analysen | `02_Scripts/02_Paper_Analysen.R` | `03_Output/Paper_Tabellen.xlsx`, R-Ergebnisse und Modellobjekte |
| Weitere Exploration und Methodenprüfungen | `02_Scripts/03_Zusatzanalysen.R` | zusätzlich `03_Output/Zusatzanalysen_Tabellen.xlsx` und Prüfprotokolle |
| Essentials-Bericht | `02_Scripts/berichte/01_Essentials.R` | `03_Output/Analyse_Essentials.md`, PDF und Grafiken |
| Explorationsbericht | `02_Scripts/berichte/02_Exploration.R` | `03_Output/Exploration.md`, PDF und Grafiken |

Jeder Einstieg kann in einer neuen Sitzung direkt ausgeführt werden. Zusatzanalysen und Reports laden ihre Grundlage selbst. Für die Übergabe genügt zunächst der Paper-Einstieg. Die Report-Aufrufe führen auch die jeweils benötigten Analysen aus; ein zusätzlicher vorheriger Lauf ist nicht nötig. Das Paper-Arbeitsbuch wird immer vollständig vom Paper-Ablauf geschrieben. Die Reihenfolge der Reports verändert seinen Inhalt nicht.

Die Zusatzanalysen enthalten Plattformvergleiche bei denselben Personen, Tagesverläufe, zusätzliche Bedürfnis- und Kontextvergleiche, Verarbeitungsmuster, Item- und Omega-Diagnostik, offene Rückmeldungen und die Sensitivität der Uploadgrenze. Die Berichtstexte, ggplot-Abbildungen und PDF-Technik liegen vollständig unter `berichte/`. Beide Berichte behalten ihre bisherigen Inhalte und Ausgabepfade.

Für die Manuskriptrevision vom 20.10.2026 ergänzt `02_Scripts/zusatz/05_Manuskript_Praezision.R` separat die 95%-Bootstrapintervalle der beiden Incidentality-Korrelationen. Das Skript lädt die Paper-Analyse und schreibt `03_Output/Manuskript_Korrelationen_KI.csv`; es verändert weder Stichprobe noch Report-Inhalte.

`02_Scripts/zusatz/06_Manuskript_Ergaenzungen.R` ergänzt Teilnahmeumfang, beitragende Personen je Plattform und eine kleine Reaktivitätsprüfung in `03_Output/Manuskript_Ergaenzungen.xlsx`. Es verwendet den gemeinsamen Datenimport und verändert die Hauptstichprobe nicht. Die englischen Manuskriptgrafiken werden weiterhin unter `berichte/Paper_Abbildungen.R` erstellt; die Übersicht zu Inhalten bleibt dort für ergänzende Berichte verfügbar.

Die bisherigen Aufrufe `02_Deskriptive_Auswertung.R` und `03_Explorative_Auswertung.R` verweisen auf die beiden Report-Skripte. `01_Daten_einlesen.R` speichert die Fragebogendaten für die Codiersheet-Vorbereitung als RDS; es ist keine Voraussetzung für den Paper-Lauf.

## Wo etwas geändert wird

| Datei oder Ordner | Zuständigkeit |
| --- | --- |
| `analyseschritte/01_Stichprobe.R` | Gemeinsame Ein- und Ausschlussregeln, Uploadzählung und Stichprobenprotokoll |
| `paper/01_Datenbasis.R` | Coding Sheet und Codebook lesen, Schlüssel und Codes prüfen, Stichprobe auswählen |
| `02_Paper_Analysen.R` | Deskription, Skalenbildung, Personenanteile und zentrale Vergleiche |
| `paper/02_Themen_und_Begegnungsart.R` | Themenhäufigkeiten, logistische Mehrebenenmodelle und Holm-Korrekturen |
| `paper/03_Themenprofile.R` | Themenprofile, Personen-Bootstrap und simultane Intervalle |
| `paper/04_Ergebnistabellen.R` | Englische Paper-Tabellen, Themen × Quellen, Kontextabgleich und Export |
| `funktionen/` | Kleine wiederverwendete Daten- und Tabellenfunktionen |
| `zusatz/` | Weitere Exploration und Methodenprüfungen |
| `berichte/` | Grafikgestaltung, Berichtstexte und PDF-Erstellung |
| `vorbereitung/`, `administration/` | Codiersheet, Pretest und Vergütung; separat ausführen |

Teilskripte werden an der angegebenen Stelle über `source()` geladen. Sie verwenden die vorher erzeugten, benannten R-Objekte. Für einen vollständigen Lauf immer einen Einstieg verwenden; einzelne Abschnitte können danach in RStudio nachvollzogen werden. Pakete werden über `pacman::p_load()` geladen. Die Datenaufbereitung nutzt vor allem `dplyr` und `tidyr`.

## Fachliche Regeln und Eingaben

- Screening erfüllt, Intro und Outro abgeschlossen (`committed` vorhanden), mindestens sieben unterschiedliche Uploads nach dokumentierten Ausschlüssen. Keine Mindestzahl an Uploadtagen; keine zusätzlichen Abschlussflags.
- Bekannte Bildduplikate werden über die gesicherte Zuordnung aus `Backups/Coding/coding_sheet_backup_2026-10-13_140323.xlsx` abgezogen. Vier weitere ausgeschlossene Dateien stehen in `06_Coding/upload_exclusions.csv`. Aktuell: 105 Personen, 3.288 codierte Uploads, davon 3.074 öffentlich relevant.
- Der Incidentality-Index ist der Mittelwert aller sechs Items, Item 5 umgepolt; nur vollständige Antworten. Im Tagebuch sind Zufall (Code 3) und ungeplant (Codes 2 und 3) getrennte Definitionen.
- Bei der Praxisfrage bedeutet dreimal −1 ausdrücklich „Nichts davon“: alle drei Praktiken werden als Nein ausgewertet. Echte fehlende Werte bleiben fehlend. Die Rohcodes bleiben erhalten.
- Screenshotanteile und mittlere Personenanteile haben unterschiedliche Bezugsgrößen. Nicht beobachtete Kategorien zählen innerhalb einer Person als null; tatsächlich fehlende Codes nicht.

Benötigt werden die drei rohen Fragebogen-CSV-Dateien unter `01_Data/`, das bereinigte `06_Coding/coding_sheet.xlsx` samt Codebook, die dokumentierten Ausschlüsse, die lokale Feedbackzuordnung und die gespeicherte Pretest-Reliabilität unter `07_Pretest/`. Die Zusatzanalysen benötigen außerdem die Pretest-Codebooks und die bestehende Zuordnung der Pretest-Dateien. Die Analysen öffnen keine Teilnehmerbilder und verändern keine Rohdaten oder manuellen Codierungen. Ein Git-Checkout enthält nur Code und diese README. Studiendaten, Codierungen, Backups und Dokumentation werden separat und lokal weitergegeben.

Nur die Report-Aufrufe benötigen zusätzlich `ggplot2`, `knitr`, `rmarkdown`, Pandoc und XeLaTeX. Layoutvorlagen stehen unter `berichte/vorlagen/`. PDF-Zwischendateien liegen unter `tmp/pdfs/`.

## Prüfungen

```r
source("02_Scripts/pruefungen/01_Syntax_und_Verweise.R", encoding = "UTF-8")
source("02_Scripts/pruefungen/03_Gemeinsame_Funktionen.R", encoding = "UTF-8")
source("02_Scripts/pruefungen/04_Paper_Ablauf.R", encoding = "UTF-8")
```

Die letzte Prüfung führt den Paper-Ablauf in einer eigenen R-Umgebung aus und prüft Nenner, Praxisantworten, Indexbildung, Modelltabellen und die Trennung vom Reporting. Die zusätzliche Prüfung `02_Inklusionskriterien.R` setzt den separaten RDS-Import voraus.

## Lokale Dateien und Git

`Dokumentation/` enthält die Übergabeanleitung, ihre Schaubilder und die ursprüngliche Teilnahmeinstruktion. Der Ordner bleibt lokal und wird nicht zu GitHub übertragen. Auch `Backups/` ist vollständig ignoriert; seine eigene `.gitignore` schließt alle Inhalte aus.

`Backups/Coding/coding_sheet_backup_2026-10-13_140323.xlsx` wird weiterhin für die Duplikatzuordnung benötigt. Diese Datei beim Weitergeben der Daten mitkopieren. Weitere Coding- und Pretest-Sicherungen liegen ebenfalls unter `Backups/`. Das einmalige Archiv `Bereinigung_2026-10-23.zip` ermöglicht, die bei dieser Bereinigung entfernten Arbeitsdateien wiederherzustellen.

Die Ausgabeverzeichnisse `03_Output/` und `04_Figures/` sind zunächst leer. Analysen und Reports erzeugen ihre Ergebnisse beim nächsten Lauf neu; Git ignoriert diese Ergebnisse vollständig. Die gespeicherte Reliabilität unter `07_Pretest/` bleibt als Eingabe für die Paper-Tabellen erhalten.

Die Zusatzanalysen lesen die manuelle Erstzuordnung der offenen Antworten aus `06_Coding/feedback_codes.csv`. Der private Exportlink für den separaten Vergütungsablauf steht unter `01_Data/sosci_payment_export_url.txt`. Beide Dateien gehören zur lokalen Übergabe und werden nicht versioniert.
