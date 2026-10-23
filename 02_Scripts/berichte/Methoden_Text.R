# 06 Tabellen und Erläuterungen für den Bericht zusammenstellen -------------------

quality_report <- c(
  "## Einschluss, Abdeckung und Machbarkeit", "",
  "**Zweck:** Rekrutierungsverlauf und Datenausfälle prüfen. Dies folgt dem Machbarkeitsziel der Präregistrierung; es ist kein Theorietest.",
  "",
  "Grundlage sind die rohen CSV-Exporte. Beide Screening-Kriterien, abgeschlossenes Intro und Outro (jeweils committed vorhanden) und mindestens sieben unterschiedliche Screenshot-Dateien sind verpflichtend. Bekannte markierte Duplikate sind abgezogen. Es gibt keine Mindestzahl an Uploadtagen. Die zusätzliche historische Drei-Tage-Regel wird nicht übernommen. Die konkrete Outro-Abschlussanforderung wird gegenüber der Präregistrierung transparent dokumentiert.",
  "",
  as.character(knitr::kable(raw_flow, format = "pipe", row.names = FALSE)), "",
  as.character(knitr::kable(coverage_summary, format = "pipe", row.names = FALSE)), "",
  "Die Einschlussgrenze bezieht sich auf Uploads, nicht auf die Teilmenge im Coding Sheet. Die Inhaltsanalysen verwenden nur vorhandene Codes. Nicht codierte Uploads sind keine irrelevanten Beiträge. Die Ursachen der unterschiedlichen Abdeckung werden unten dateibezogen ausgewiesen. Uploadauswahl und fehlende Codierung können die Befunde verzerren. Coding_Abdeckung.csv dokumentiert die Abdeckung je Person.",
  "",
  "Die Analyse beschränkt sich auf Personen mit abgeschlossenem Intro und Outro sowie mindestens sieben Uploads. Vier historisch wegen fehlendem firstOpened ungecodete Dateien sind gemäß Entscheidung vom 16.10.2026 ausgeschlossen (06_Coding/upload_exclusions.csv). Ein pauschaler firstOpened-Filter auf weitere Diary-Zeilen wird nicht angewandt. Das Coding Sheet umfasst Hauptcodierung, gemeinsame Codierung und Reliabilitäts-Pretest. Die folgende Tabelle beschreibt nur noch Uploads eingeschlossener Personen ohne vorhandene Codierzeile; die Uploadzahlen sind disjunkt.",
  "",
  as.character(knitr::kable(coverage_reason_summary, format = "pipe", row.names = FALSE)), "",
  "Dateilisten ohne Coding werden nur bei vorhandenen Lücken als CSV ausgegeben. Die Pretest-Codierungen wurden bereits übernommen, bei Abweichungen mit LA-Vorrang. Ergänzungen und Anpassungen an das finale Schema sind im Coding Sheet protokolliert. Diese Übernahme ersetzt keine erneute unabhängige Reliabilitätsprüfung.",
  "",
  "**Zweck:** Nachträgliche Selektionsdiagnostik anhand von Alter und Nutzungsintensität. Die beiden Mittelwertvergleiche beschreiben die verfügbare Coding-Basis; sie belegen weder zufällige Ausfälle noch Repräsentativität.",
  "",
  as.character(knitr::kable(selection_summary, format = "pipe", digits = 2, row.names = FALSE)), "",
  "### Codierqualität", "",
  "**Zweck:** Messqualität der Inhaltsanalyse dokumentieren. Dies ist eine Methodenprüfung, kein Theorietest. Die Tabelle übernimmt den vorhandenen Pretest mit drei Codierenden. Konfidenzintervalle stammen aus der gespeicherten Auswertung und wurden hier nicht neu geschätzt.",
  "",
  as.character(knitr::kable(pretest_table, format = "pipe", digits = 3, row.names = FALSE)), "",
  "Krippendorffs Alpha ist nominal. Öffentliche Relevanz und Plattform beziehen sich auf 140 Pretest-Beiträge. Nachgelagerte Variablen verwenden 117 Beiträge, die alle drei Codierenden als öffentlich relevant einstuften. Die Unsicherheit ist insbesondere bei Werbung groß. Die Werte belegen keine Reliabilität der vollständigen Hauptcodierung.",
  "",
  "Vergleich der Codebook-Zeilen (Variable, Code, Bezeichnung) zwischen Pretest-Dateien und aktuellem Coding Sheet:",
  "",
  as.character(knitr::kable(version_table, format = "pipe", row.names = FALSE)), "",
  "Die Codebooks weichen voneinander ab. Der Pretest enthält beispielsweise Mixed media format (Code 4) und Not determinable (−1); das aktuelle Format-Schema verwendet 1 bis 3 sowie 99. Die Pretest-Werte dürfen deshalb nicht als Reliabilität des aktuellen Kategoriensystems ausgegeben werden. Eine erneute unabhängige Codierung mit dem aktuellen System ist für diese Aussage nötig. Eine datierte Dokumentation zu Schulung, Regeländerungen und Konfliktlösung muss das Forschungsteam ergänzen. Die Dateiprüfsumme im Essentials-Bericht identifiziert den analysierten Stand.",
  "",
  "### Offene Rückmeldungen", "",
  "**Zweck:** Hürden und Verbesserungen für digitale Tagebücher mit älteren Personen identifizieren. Qualitative Exploration zum präregistrierten Machbarkeitsziel; die Themen wurden nach Datensichtung gebildet.",
  "",
  sprintf(
    "Basis: %s Antworten von %s eingeschlossenen Personen. Die inhaltliche Erstcodierung ist KI-gestützt und noch nicht unabhängig geprüft. Mehrfachzuordnungen sind möglich. Die prüfbare Zuordnung mit Originaltext steht in Feedback_Erstcodierung.csv. Häufigkeiten beschreiben die Antwortenden, nicht die Verbreitung in der gesamten Stichprobe.",
    nrow(feedback),
    n_distinct(feedback$participant)
  ),
  "",
  as.character(knitr::kable(feedback_summary, format = "pipe", row.names = FALSE)), "",
  "Konkrete Ansatzpunkte sind eine Vorschau hochgeladener Bilder, Korrektur- und Speicherfunktionen sowie klarere Hinweise zum Ablauf und zur Inhaltsauswahl. Positive App-Bewertungen schließen einzelne Bedienprobleme nicht aus.",
  ""
)

# 05 Berichtsteil mit den jeweiligen Bezugsgrößen erstellen ------------------------

upload_sensitivity_report <- c(
  "## Robustheit gegenüber der Uploadschwelle", "",
  "Die Hauptauswertung verlangt erfülltes Screening, abgeschlossenes Intro und Outro sowie mindestens sieben unterschiedliche Uploads nach Abzug bekannter Duplikate. Ergänzend wird die Uploadgrenze auf einen Upload gelockert; Intro und Outro bleiben auch dort verpflichtend. Beide Varianten verwenden dieselben verfügbaren Codierungen einschließlich der übernommenen Pretest-Dateien.",
  "",
  as.character(knitr::kable(threshold_basis, format = "pipe", row.names = FALSE)), "",
  "Ab_sieben bezeichnet die Hauptstichprobe; Alle die ergänzende Vergleichsgruppe ab einem Upload bei ebenfalls abgeschlossenem Intro und Outro. N zählt in der Beitragsebene gültige Beiträge, in den anderen Zeilen Personen. Wert enthält entweder einen Prozentwert oder Spearman-rho. Differenz ist Vergleichsgruppe minus Hauptstichprobe (Alle minus Ab_sieben): bei Anteilen in Prozentpunkten, bei Korrelationen in rho-Einheiten. Intro–Tagebuch korreliert den sechs Items umfassenden Intro-Index mit dem individuellen Zufalls- bzw. Ungeplant-Anteil.",
  "",
  as.character(knitr::kable(threshold_comparison,
    format = "pipe",
    digits = 3,
    row.names = FALSE
  )),
  "",
  "Für Thema, Quelle und Format zeigt die nächste Tabelle die größte absolute Veränderung eines Kategorieanteils über alle Kategorien. Grundlage sind jeweils öffentlich relevante Beiträge mit gültigem Code. Die vollständigen Verteilungen stehen in Uploadschwelle_Kategorien.csv.",
  "",
  as.character(knitr::kable(category_max, format = "pipe", digits = 3, row.names = FALSE)), "",
  "Wichtig: Diese Prüfung betrifft die Uploadschwelle innerhalb des verfügbaren Hauptcodings. Sie zeigt nicht, welche Ergebnisse zusätzliche, bislang nicht im Hauptcoding enthaltene Uploads liefern würden. Personenanteile aus wenigen Beiträgen bleiben unsicher; die Mindestzahl je Plattform oder Vergleichssituation bei gepaarten Analysen ist eine eigenständige, weiterhin explizit ausgewiesene Analyseregel.",
  ""
)
