# Code-Audit und gezielter Refactor

Stand: 20. Oktober 2026. Geprüft wurden alle aktiven R-Skripte unter `02_Scripts/`, ihre Aufrufketten, Paketabhängigkeiten, Dateiausgaben und die PDF-Vorlagen. Historische Archive und Teilnehmerbilder wurden nicht geöffnet. Der Audit umfasst Wartbarkeit, Laufzeitfehler, Datenfluss und Schutz manueller Daten. Er ersetzt keine unabhängige Begutachtung der statistischen Methoden.

## Wichtigste Befunde

| Priorität | Befund | Behandlung |
| --- | --- | --- |
| Hoch | Der vorherige Refactor hat `md_table()` entfernt, aber die Themenmodule 05 und 06 rufen die Funktion weiter auf. Ein Explorationslauf in einer frischen Sitzung bricht ab. | Alle Aufrufe und Modulbeschreibungen auf `markdown_table()` umgestellt; kompletter Explorationslauf geprüft. |
| Hoch | Das Vergütungsskript enthält eine zugangstragende SoSci-Adresse direkt im Quelltext und führt von dort geladenen R-Code aus. | Offen. Zugang in lokale, nicht versionierte Konfiguration auslagern; Berechtigung und gegebenenfalls Erneuerung des Zugangs prüfen. Keine Adresse oder Zugangsdaten in diesem Bericht. Kein Netzwerkaufruf beim Audit. |
| Mittel | Die Exploration benötigt `lme4`, `nnet` und `boot`, führt sie aber bisher nicht im Paketblock auf. Teilmodule prüfen nur, ob sie installiert sind. | Den vorhandenen `pacman::p_load()`-Block vervollständigt. Keine neue Bibliothek eingeführt. |
| Mittel | Polung und Missing-Regel des Incidentality-Index stehen mehrfach in den Analysen. Änderungen können auseinanderlaufen. | Gemeinsame Funktion `incidentality_scores()` für Deskription, Exploration und Uploadschwellenvergleich. Index und vollständige-Fälle-Regel erhalten. |
| Mittel | Spearman-Korrelationen prüfen gültige Werte und Streuung mehrfach lokal. | Gemeinsame Funktion `safe_spearman()` für den Intro-Daily-Abgleich und die theoriebezogene Exploration; Randfälle durch kleine ausführbare Beispiele abgedeckt. Der gesonderte Uploadschwellenvergleich behält seine bestehende Gruppenberechnung. |
| Mittel | Codiersheet-Vorbereitung verwendet eine gröbere Duplikatsuche als die Analysen; Hinweise auf doppelte Teilnahme könnten dort als Bildduplikate gelten. | Vorbereitung verwendet dieselbe bestehende Funktion `is_marked_screenshot_duplicate()`. Keine Bilder oder Codierungen verändert. |
| Mittel | Der gemeinsame Import schreibt das Stichprobenprotokoll, ohne selbst das Ausgabeverzeichnis anzulegen. | `dir.create()` unmittelbar vor der Ausgabe ergänzt. Ein frischer Projektordner benötigt keine vorab vorhandenen Ergebnisse. |
| Mittel | Import, Vorbereitung und unabhängige Prüfung lesen die Duplikatzuordnung aus einer datierten Backup-Excel. | Offen. Die historische Herkunft ist nachvollziehbar, der Dateiname bleibt aber eine betriebliche Voraussetzung. Eine explizite Ausschlussliste mit Herkunftsnachweis wäre der nächste kleine Schritt; aktuelle Ein- und Ausschlüsse nicht umgedeutet. |
| Niedrig | README und Audit-Dokument können durch das allgemeine Muster `*.MD` von Git ausgeschlossen werden. | Beide Dokumente ausdrücklich von der Ignorierregel ausgenommen. |
| Niedrig | Teilskripte übergeben Daten implizit über die R-Arbeitsumgebung; Berichtserläuterungen werden zusätzlich anhand von Überschriften eingefügt. | Beibehalten und dokumentiert. Modulköpfe nennen benötigte und gelieferte Objekte. Weitere Auslagerung in ein eigenes Framework wäre derzeit zusätzlicher Wartungsaufwand. |

## Wo etablierte Pakete bereits die Arbeit übernehmen

| Aufgabe | Verwendete Umsetzung | Bewertung |
| --- | --- | --- |
| Import und Umformung | `readr`, `dplyr`, `tidyr`, `stringr` | Beibehalten. Fehlende Werte und Nenner bleiben im Analyseskript sichtbar. |
| Excel und manuelle Codierung | `openxlsx`; `readxl`/`writexl` im separaten Pretest | Bestehende Arbeitsdateien und Layouts erhalten. Ein Austausch nur zur Vereinheitlichung wäre unnötig. |
| Interne Konsistenz | `psych::alpha`, `psych::omega` | Beibehalten. Warnungen und unzulässige Lösungen weiterhin kenntlich machen. |
| Intercoder-Reliabilität | `icr::krippalpha` | Kein eigener Alpha-Schätzer. Teurer Bootstrap bleibt ein separater Vorbereitungslauf. |
| Mehrebenenmodelle | `lme4::glmer`, `stats::anova`, `stats::p.adjust` | Beibehalten. Keine Modellformeln, Konvergenzkriterien oder Korrekturen verändert. |
| Themenprofile | `nnet::multinom`, `boot::boot` | Beibehalten. Geschlossene Häufigkeitslösung im Bootstrap vermeidet unnötige wiederholte Modelloptimierung. |
| Modellwahrscheinlichkeiten und Kontraste | Kleine Matrixrechnung mit `stats` und Basis-R | Aktuelle Logit-Kontraste sind explizit und durch Ergebnistabellen geprüft. Ein Wechsel auf ein weiteres Paket nur wegen weniger Code wäre keine Verbesserung ohne erneuten Methodenvergleich. |
| Grafiken und Tabellen | `ggplot2::ggsave`, `knitr::kable` | Die gemeinsame Markdown-Funktion verwendet direkt `knitr`. Unterschiedliche Rundungen in Spezialtabellen bleiben erhalten. |
| PDF | `rmarkdown` zur Pandoc-Suche, Pandoc, XeLaTeX | Fast der gesamte Ablauf ist R. LaTeX gestaltet Seiten; der kleine Lua-Filter steuert Tabellenbreiten und Tabellenköpfe innerhalb von Pandoc. Ein Ersatz würde funktionierenden PDF-Satz neu implementieren. |

## Funktionsprüfung

1. Referenzstände der Daten- und Ergebnistabellen beider vollständigen Analysen gespeichert. Für die verwaisten `md_table()`-Aufrufe im alten Stand war ausschließlich im Referenz-Prüflauf eine Kompatibilitätsfunktion nötig.
2. Beide Analysen nach den Änderungen vollständig berechnet, einschließlich Mehrebenenmodellen und des Personen-Cluster-Bootstraps mit 19.999 Wiederholungen. Im Prüflauf werden die fertigen Markdown- und PDF-Dateien nicht ersetzt; CSV-Ausgaben, RDS-Modellergebnisse und Grafiken werden regulär erzeugt.
3. 76 deskriptive und 107 explorative Tabellen mit dem Referenzstand verglichen: keine Abweichungen bei `all.equal(..., tolerance = 1e-12)`. Enthalten sind die Modell- und Bootstrap-Ergebnistabellen. Das ist ein Vergleich mit dem bestehenden Verhalten, kein Beweis methodischer Richtigkeit.
4. Rohdatenimport und vorhandene unabhängige Inklusionsprüfung ausgeführt: 105 Personen, 3.288 unterschiedliche berücksichtigte Uploads und 3.288 Coding-Zeilen einschließlich Pretest bestätigt.
5. Syntax, wörtliche `source()`-Pfade und Vorlagendateien geprüft. Neue dauerhafte Prüfung unter `pruefungen/03_Gemeinsame_Funktionen.R`: Indexpolung, vollständige Werte, unveränderte Eingabedaten, undefinierte Korrelationen, Tabellenformat und Duplikatregel.

Die bestehende Warnung zu einer unzulässigen Omega-Lösung tritt weiterhin auf. Die Auswertung behandelt sie bereits ausdrücklich; der Refactor hat diese Modellentscheidung nicht verändert.

## Grenzen und nächste Schritte

- Vergütung und Codiersheet-Neuaufbau wurden statisch geprüft und nicht ausgeführt. Diese Abläufe greifen auf Verwaltungsdaten zu beziehungsweise verschieben Bildordner und schreiben manuelle Arbeitsdateien. Ein Live-Lauf wäre kein geeigneter Regressionstest.
- Die PDF-Kompilierung wurde bei diesem Audit nicht neu ausgeführt. PDF-Templates und ihre Pfade sind vorhanden; das tatsächliche Rendering ist durch diesen Prüflauf nicht erneut belegt.
- Aktuelle Auswahlregeln aus dem Repository gelten weiter: Screening sowie abgeschlossenes Intro und Outro, mindestens sieben berücksichtigte Rohuploads. Weder die Zahl der Uploadtage noch die Codierphase ersetzt dieses Kriterium. Historische Entscheidungen aus früheren Gesprächsständen wurden nicht zurückgesetzt.
- Zuerst den Zugang im Verwaltungsskript auslagern und die historische Duplikatliste als eigene dokumentierte Eingabe sichern. Danach nur bei konkretem Änderungsbedarf die großen Berichtstexte oder globalen Teilskripte weiter zerlegen. Keine neue Pipeline-Bibliothek oder Paketstruktur eingeführt.
