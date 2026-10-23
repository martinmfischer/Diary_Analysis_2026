# Leseschlüssel für Berichte: explain_report(report, kind)
# Eingabe: Bericht als Zeichenvektor; kind = essentials oder exploration
# Rückgabe: Bericht mit Erläuterungen zu Variablen, Nennern und Vergleichen
# Längere Zeichenketten sind bewusst vollständige Textbausteine

# Verständliche Erläuterungen direkt unter den zugehörigen Berichtsüberschriften.
# Nur Berichtstext: keine Änderung an Stichprobe, Berechnungen oder Tabellen.
explain_report <- function(report, kind) {
  lines <- unlist(lapply(report, function(x) {
    if (!nzchar(x)) "" else strsplit(x, "\n", fixed = TRUE)[[1]]
  }), use.names = FALSE)
  explanations <- list()
  add <- function(heading, text) explanations[[heading]] <<- text
  practices <- paste0("Mit Lesen ist gründliches Lesen oder Anschauen des Beitrags gemeint (interaction_read_code), mit Recherche die Suche nach weiteren Informationen einschließlich eines Linkklicks (interaction_research_code) und mit Interaktion eine berichtete sichtbare Handlung wie Liken, Kommentieren oder Weiterleiten (interaction_engagement_code). Diese drei Angaben stammen aus dem Tagebuchfragebogen, nicht aus einer Beobachtung des tatsächlichen Verhaltens. Die Ausweichoption −1 = Nichts davon ist eine gültige Antwort: Bei drei −1-Codes werden alle drei Praktiken mit 0 ausgewertet, nicht als fehlend. Jede Praxis wird getrennt als 1 = berichtet und 0 = nicht berichtet ausgewertet; fehlende Angaben werden nicht als 0 behandelt.")
  encounters <- "Die Begegnungsart stammt aus incidentality_code: 1 = gezielt nach Thema oder Account gesucht, 2 = dem Account gefolgt, aber nicht gezielt nach diesem Beitrag gesucht, 3 = zufällig auf den Beitrag gestoßen. Zufall meint nur Code 3; Ungeplant fasst Codes 2 und 3 zusammen. Der Nenner eines Personenanteils sind jeweils die öffentlich relevanten Beiträge dieser Person mit gültiger Angabe zur Begegnungsart."
  stats <- "N bzw. Personen bezeichnet die Zahl gültiger Fälle, nicht immer die gesamte eingeschlossene Stichprobe. M/Mittelwert ist der Durchschnitt; SD beschreibt die Streuung; der Median teilt die Werte in zwei Hälften. Q1 und Q3 begrenzen die mittleren 50 Prozent. Fehlend nennt die nicht auswertbaren Antworten. Ein Prozentpunkt ist die Differenz zweier Prozentwerte, keine relative prozentuale Veränderung."
  add(
    "## Einschluss, Abdeckung und Machbarkeit",
    "Die erste Tabelle zählt die aufeinanderfolgenden Teilnahmegruppen aus den Rohdaten: Intro-Zeilen, erfülltes Screening, abgeschlossene Intro- und Outro-Fragebögen, die Hauptstichprobe ab sieben Uploads und verfügbare Outro-Skalenantworten in dieser Hauptstichprobe. Die zweite Tabelle stellt für die eingeschlossenen Personen Uploads und vorhandene Hauptcoding-Zeilen gegenüber. Mit_Coding und Ohne_Coding unterscheiden Personen mit mindestens einem bzw. ohne einen Beitrag im Hauptcoding. Diese Zahlen beschreiben Datenabdeckung, nicht öffentlich relevante gegenüber irrelevanten Inhalten. Die spätere Gruppentabelle vergleicht genau diese beiden Personengruppen hinsichtlich Alter (intro_age_num, Jahre) und selbstberichteter Nutzungsintensität (intro_intensity, 1 = sehr passiv bis 7 = sehr aktiv). Alter_N und Intensitaet_N sind die jeweils gültigen Personenzahlen; Alter_M und Intensitaet_M die getrennten Gruppenmittelwerte. Es wird hier keine Korrelation zwischen Alter und Intensität berechnet."
  )
  add(
    "### Codierqualität",
    "Hier werden die Zuordnungen von drei Codierenden für dieselben Pretest-Screenshots miteinander verglichen, getrennt für jede Coding-Variable. Beurteilbar zählt Einheiten mit genügend Bewertungen für einen Vergleich, Vollstaendig Einheiten mit Bewertungen aller Codierenden. Alpha bezeichnet hier Krippendorffs nominales Alpha: ein Maß der Übereinstimmung über zufällig zu erwartende Übereinstimmung hinaus. KI_unten und KI_oben begrenzen das gespeicherte 95%-Konfidenzintervall. Dieses Alpha ist nicht Cronbachs Alpha aus den Fragebogenskalen. Die anschließende Codebook-Tabelle zählt abweichende Kombinationen aus Variablenname, Code und Kategorienbezeichnung zwischen der jeweiligen Pretest-Datei und dem aktuellen Codebook; sie misst keine Fehlerquote der Codierenden."
  )
  add(
    "### Offene Rückmeldungen",
    "Ausgewertet werden die Freitextfelder outro_problems_free (Probleme/Unsicherheiten) und outro_suggestions_free (Verbesserungsvorschläge) gemeinsam. Die Tabelle zählt je Thema unterschiedliche Personen, die es in mindestens einer der beiden Antworten ansprechen. Eine Person zählt innerhalb eines Themas nur einmal, kann aber in mehreren Themen vorkommen. Deshalb ergeben die Themenhäufigkeiten zusammen nicht die Zahl der Antwortenden. Es werden weder die beiden Fragen statistisch verglichen noch Anteile aller Studienteilnehmenden geschätzt."
  )
  if (kind == "exploration") {
    add(
      "## Datenbasis",
      paste(
        "Alle nachfolgenden Inhalts- und Praxisanalysen verwenden öffentlich relevante, im Hauptcoding vorhandene Screenshots (public_rel_coded = 1). Intro- und Outro-Angaben werden über den Personencode zugeordnet. Ein Screenshot ist ein eingereichter Beitrag, keine vollständige Erfassung des persönlichen Feeds.",
        practices,
        encounters,
        stats
      )
    )
    add(
      "## Erste Befunde",
      "Die folgenden Kurzbefunde werden in den nummerierten Abschnitten vollständig aufgeschlüsselt. Selbstbericht bezeichnet den Mittelwert der sechs Intro-Incidentalness-Items, Tagebuch den individuellen Anteil zufälliger bzw. ungeplanter Begegnungen. Die Plattformunterschiede beziehen sich auf mittlere Themen- bzw. Quellenanteile derselben Personen. Der Recherchevergleich bezieht sich auf den Anteil der Beiträge mit berichteter weiterer Informationssuche innerhalb der jeweiligen Begegnungsart."
    )
    add(
      "## 1. Unterschiede zwischen Personen",
      paste(
        "Hier werden keine zwei Variablen korreliert. Für jede Person wird separat berechnet, bei welchem Anteil ihrer gültigen öffentlich relevanten Beiträge sie Lesen, Recherche, Interaktion, Zufall oder Ungeplant berichtet. Anschließend wird die Verteilung dieser Personenanteile beschrieben. Ein Tabellenmittelwert von beispielsweise 20 bedeutet: Im Durchschnitt entfallen 20 Prozent der jeweils gültigen Beiträge einer Person auf das Merkmal. Jede Person zählt gleich viel, auch bei unterschiedlich vielen Uploads. Personen ohne gültige Werte für das jeweilige Merkmal entfallen nur dort.",
        practices,
        encounters
      )
    )
    add(
      "## 2. Selbstbericht und Tagebuch",
      "Verglichen werden pro Person (1) incidentality_index, der Mittelwert aus intro_incidentality_1 bis _6 mit umgepoltem Item 5, und (2) ihr Tagebuchanteil Zufall bzw. Ungeplant. Es entstehen zwei getrennte Spearman-Korrelationen, jeweils nur mit Personen, für die beide Werte vorliegen. Der Intro-Wert reicht von 1 bis 5; höhere Werte stehen für stärkere berichtete Incidentalness. Der Tagebuchwert reicht von 0 bis 100 Prozent. Ein positives rho bedeutet: Personen mit höherem Intro-Wert haben tendenziell auch einen höheren Tagebuchanteil; ein negatives rho die umgekehrte Rangfolge. Ein rho nahe null zeigt keinen deutlichen monotonen Zusammenhang, nicht automatisch schlechte Messqualität. Die Grafik zeigt pro Person den Intro-Wert auf der x-Achse und den Tagebuchanteil auf der y-Achse; die Punktgröße zeigt die Zahl gültiger Begegnungsangaben, ohne diese Personen in der Korrelation stärker zu gewichten."
    )
    add(
      "## 4. Plattformprofile bei denselben Personen",
      "Die erste Tabelle beschreibt nur die Zahl öffentlich relevanter Beiträge und beitragender Personen pro Plattform. Für die eigentlichen Vergleiche werden Facebook und Instagram gegenübergestellt; die Plattform stammt vorrangig aus platform_coded, ersatzweise aus platform_reported. Eingeschlossen werden Personen mit mindestens drei öffentlich relevanten Beiträgen auf jeder Plattform. Für Thema bzw. Quelle müssen zusätzlich gültige Codes auf beiden Plattformen vorliegen. Der Vergleich betrifft Inhaltsanteile innerhalb dieser Personen, nicht alle Facebook-Nutzenden gegenüber allen Instagram-Nutzenden."
    )
    add(
      "### Themen",
      "Gegenübergestellt werden platform (Facebook/Instagram) und topic_coded (Themenkategorie). Je Person und Plattform wird die Zahl gültig codierter Beiträge einer Themenkategorie durch alle dort gültig themencodierten Beiträge geteilt. Facebook und Instagram in der Tabelle sind die Mittelwerte dieser Personenanteile in Prozent. Differenz_IG_minus_FB zieht den Facebook-Wert vom Instagram-Wert ab: Positive Werte bedeuten einen höheren Themenanteil auf Instagram. Personen bezeichnet die gemeinsame gültige Personenbasis je Kategorie."
    )
    add(
      "### Quellen",
      "Gegenübergestellt werden platform (Facebook/Instagram) und source_coded (Quellentyp). Der Nenner ist je Person und Plattform die Zahl der Beiträge mit gültiger Quellencodierung. Gemittelt werden diese individuellen Quellentypanteile, einschließlich null Prozent bei nicht vorkommenden Kategorien. Positive Differenz_IG_minus_FB bedeutet einen höheren Anteil dieses Quellentyps auf Instagram bei denselben Personen; es handelt sich nicht um einen Vergleich einzelner Accounts oder eine Aussage zur Glaubwürdigkeit."
    )
    add(
      "## 5. Umgang mit Beiträgen",
      paste(
        practices,
        "Für jede der drei Praktiken wird ihr Ja-Anteil getrennt nach Begegnungsart bzw. Beitragsformat berechnet. Gueltige_Posts zählt die gültigen Antworten für genau diese Kombination, Fehlend die fehlenden Praxisangaben. Screenshot_Prozent ist der Ja-Anteil über alle gültigen Beiträge der Gruppe. Personen_Prozent ist dagegen der Durchschnitt der zunächst je Person innerhalb dieser Gruppe berechneten Ja-Anteile; Personen zählt die dafür verfügbaren Personen. Unterschiedliche Gruppen können unterschiedliche Personen enthalten."
      )
    )
    add(
      "### Begegnungsart",
      paste(
        "Verglichen werden die drei Kategorien von incidentality_code mit Lesen, Recherche und Interaktion, also drei getrennte Praxisvariablen je Begegnungskategorie.",
        encounters
      )
    )
    add(
      "### Format",
      "Verglichen wird media_format (codiertes Beitragsformat gemäß Codebook) jeweils mit Lesen, Recherche und Interaktion. Beispielsweise beschreibt eine Zeile Recherche/Video den Anteil der Videobeiträge, zu denen weitere Informationssuche berichtet wurde. Die Tabelle vergleicht keine tatsächlichen Lesezeiten und keine Abspieldauer; fehlende Formatcodes werden ausgeschlossen, ausgewiesene Restkategorien bleiben entsprechend dem Codebook erhalten."
    )
    add(
      "## 6. Verlauf und Robustheit",
      "Gegenübergestellt werden study_day (Studientag 1 bis 7) und die Anteile Zufall, Lesen, Recherche und Interaktion. Zuerst wird je Person und Studientag ein Anteil aus den gültigen öffentlich relevanten Beiträgen berechnet, danach werden die Personenanteile pro Tag gemittelt. Prozent ist dieser Tagesmittelwert; Personen die jeweilige gültige Personenzahl. Alle verfügbaren Personen erlaubt eine täglich wechselnde Zusammensetzung. Angaben an allen 7 Tagen verwendet für jedes Merkmal nur Personen mit gültigem Anteil an jedem Tag; die konstante Gruppe kann daher zwischen den Merkmalen unterschiedlich sein. Der Studientag bezieht sich auf die Tageszuordnung im Datensatz, nicht auf einen direkten Zeitstempel der tatsächlichen Nutzung."
    )
    add(
      "### Fehlende Angaben zu Interaktionspraktiken",
      "Verglichen werden study_day und das Fehlen einer gültigen Angabe in interaction_read_code. Posts zählt alle öffentlich relevanten Beiträge des Tages, Fehlend die darunter fehlenden Leseangaben und Fehlend_Prozent deren Anteil. Hier wird nicht der Anteil der Personen ohne Antwort und auch nicht die Häufigkeit sichtbarer Interaktion berichtet; Lesen dient an dieser Stelle als Indikator für Antwortausfälle."
    )
    add(
      "### Sensitivität gegenüber studienbedingter Suche",
      "Erneut korreliert wird der Intro-Incidentalness-Mittelwert mit dem individuellen Anteil Zufall bzw. Ungeplant, wie in Abschnitt 2. Jetzt werden nur Personen mit Antwort 1 bis 3 auf outro_reactivity_1 berücksichtigt: Sie stimmen der Aussage, gezielt nach hochladbaren Beiträgen gesucht zu haben, nicht zu oder antworten neutral. Antworten 4/5 und fehlende Antworten werden ausgeschlossen. Die Tabelle enthält die beiden neuen Spearman-Werte und die verbleibenden Personenzahlen; verglichen werden diese mit den Hauptwerten aus Abschnitt 2. Es wird nicht die Reaktivitätsantwort selbst mit der Tagebuchnutzung korreliert."
    )
    add(
      "## 7. Informationsbedürfnisse und Nutzungsintensität",
      "Jede Tabellenzeile ist eine eigene Spearman-Korrelation zwischen genau zwei Personenwerten: (1) intro_ib_problem, Wichtigkeit problemlösender Informationen, mit dem Anteil Recherche; (2) intro_ib_thematic, Wichtigkeit persönlicher Themeninteressen, mit dem Anteil gezielter Begegnungen (incidentality_code = 1, berechnet als 1 minus Ungeplant); (3) intro_ib_undirected, Wichtigkeit von Nachrichten und aktuellen Ereignissen, mit dem Anteil Lesen; (4) intro_intensity, allgemeine passive/aktive Nutzung, mit dem Anteil Interaktion. Die Bedürfnisantworten reichen von 1 = gar nicht wichtig bis 5 = sehr wichtig; Intensität von 1 = sehr passiv bis 7 = sehr aktiv. Die Praxisanteile verwenden jeweils nur gültige Tagebuchantworten der Person. intro_ib_social wird in dieser Tabelle nicht korreliert. Ein positives rho bedeutet jeweils: Ein höherer Intro-Wert geht tendenziell mit einem höheren zugeordneten Tagebuchanteil einher. Die Grafik zeigt die jeweilige Intro-Antwort auf x und den zugeordneten Personenanteil auf y; die x-Skalen unterscheiden sich."
    )
    add(
      "## 8. Nutzungskontext innerhalb derselben Personen",
      "Für jede Praxis wird locality_code (1 = zu Hause, 2 = unterwegs) mit dem Anteil Lesen, Recherche oder Interaktion gegenübergestellt. Je Praxis gehen nur Personen mit mindestens drei gültigen Beiträgen an jedem der beiden Orte ein. Mittel_A ist der mittlere Personenanteil zu Hause, Mittel_B der mittlere Personenanteil unterwegs. Differenz_pp ist der Durchschnitt der individuellen Differenzen unterwegs minus zu Hause; Median_Differenz deren Median. Positive Werte bedeuten mehr berichtete Praxis unterwegs. Beispiel: +4 Prozentpunkte bedeutet einen um vier Punkte höheren Ja-Anteil unterwegs, nicht vier zusätzliche Beiträge. Personen zählt die gepaarten Personen, nicht die Posts."
    )
    add(
      "### Sozialer Kontext",
      "Diese Tabelle zählt Beiträge nach situation_code (allein, gemeinsam, weiß nicht mehr bzw. fehlend) sowie unterschiedliche Personen mit mindestens einem Beitrag in der jeweiligen Kategorie. Eine Person kann in mehreren Kategorien vorkommen. Es wird hier keine Praxisvariable gegenübergestellt und keine Differenz in Lesen oder Interaktion zwischen allein und gemeinsam berechnet."
    )
    add(
      "## 9. Ungeplante Begegnungen und Verarbeitung",
      "Ausgewählt werden öffentlich relevante Beiträge mit incidentality_code 2 oder 3. Innerhalb dieser Beiträge werden die drei Ja/Nein-Angaben Lesen, Recherche und Interaktion zu acht möglichen Kombinationen zusammengefasst. Nur Lesen bedeutet read = 1, research = 0, engage = 0; Keine Praxis berichtet verlangt drei gültige Nullen. Sobald eine der drei Angaben fehlt, zählt der Beitrag zu Angaben fehlen. Posts zählt Beiträge je Kombination; Prozent teilt durch alle ungeplanten Beiträge der jeweiligen Basis einschließlich fehlender Praxisangaben. Alle öffentlichen Themen umfasst sämtliche Themen, Politik, Staat und Wahlen nur topic_coded = 1. Die politische Teilmenge ist in der Gesamtgruppe enthalten; es sind keine unabhängigen Vergleichsgruppen. Die Muster beschreiben berichtete Handlungen, keine gemessene Verarbeitungstiefe."
    )
    add(
      "### Gezielte und zufällige Begegnungen bei denselben Personen",
      "Verglichen werden incidentality_code 3 (zufällig, A) und 1 (gezielt, B), getrennt für Lesen, Recherche und Interaktion; Code 2 ist in diesem Vergleich nicht enthalten. Je Praxis werden nur Personen mit mindestens drei gültigen Beiträgen in beiden Begegnungsarten berücksichtigt. Mittel_A und Mittel_B sind die mittleren individuellen Ja-Anteile in Prozent. Differenz_pp und Median_Differenz beschreiben die individuellen Differenzen gezielt minus zufällig. Ein positiver Wert bedeutet häufiger berichtete Praxis bei gezielten Begegnungen derselben Personen."
    )
    add(
      "## 10. Fehlende Angaben nach Begegnungsart",
      "Gegenübergestellt werden die drei Begegnungskategorien und für jede Praxis separat das Vorliegen bzw. Fehlen einer Antwort. Posts ist die gesamte Beitragszahl der Begegnungskategorie. Fehlend_Prozent = fehlende Praxisangaben / Posts × 100. Gueltig_Prozent = berichtete Praxis / Beiträge mit gültiger Praxisangabe × 100. Untergrenze = berichtete Praxis / alle Posts × 100; Obergrenze zählt zusätzlich sämtliche fehlenden Antworten als berichtete Praxis. Die beiden Grenzen verwenden also einen anderen Nenner als Gueltig_Prozent. Alle Werte sind beitragsgewichtet; unterschiedliche Uploadmengen der Personen werden nicht ausgeglichen."
    )
    add(
      "## 11. Incidentality: einzelne Items und Tagebuch",
      "Jedes der sechs Intro-Items wird einzeln mit zwei Tagebuchanteilen derselben Person korreliert: Zufall (nur Begegnungscode 3) und Ungeplant (Codes 2 und 3). Es gibt somit zwölf Spearman-Korrelationen, keine Korrelationen der sechs Items untereinander. Inhaltlich erfassen die Items: 1 sonst verpasste Informationen, 2 sonst nicht gelesene Informationen, 3 neue/interessante Informationen, 4 normalerweise nicht gesuchte Informationen, 5 keine Informationen außerhalb eigener Interessen (hier umgepolt), 6 unbeabsichtigtes Lesen aufgrund eines Posts. Die Beschreibungen sind sinngemäß, nicht der genaue Fragebogenwortlaut. Pro Zeile werden nur Personen mit gültigem Item und Tagebuchanteil verwendet; Personen nennt diese Zahl. Höhere Werte von Item 5 bedeuten nach Umpolung stärkere Offenheit für Informationen außerhalb eigener Interessen."
    )
  } else {
    add(
      "## 1. Datenbasis und Einschluss",
      paste(
        "Die erste Tabelle beschreibt Datenmengen, keine Zusammenhänge zwischen Variablen: eingeschlossene Personen, verfügbare Intro-Daten, Personen mit Hauptcoding, codierte Screenshots, Personen mit Outro-Skalenantworten und öffentlich relevante Screenshots. Gleiche Personenzahlen in zwei Zeilen bedeuten nicht automatisch identische Personengruppen.",
        stats
      )
    )
    add(
      "### Eingabedateien",
      "Datei nennt die jeweilige Datenquelle; MD5 ist ihre technische Prüfsumme zur Identifikation des ausgewerteten Dateistands. Die Prüfsumme ist keine inhaltliche Qualitätsbewertung."
    )
    add(
      "### Alter",
      "Beschrieben wird intro_age_num in Jahren, eine Antwort je eingeschlossener Person. N zählt gültige Altersangaben; Fehlend die übrigen eingeschlossenen Personen. Es wird kein Altersgruppenvergleich vorgenommen."
    )
    add(
      "### Geschlecht",
      "Beschrieben wird intro_gender. n zählt Personen je Antwortkategorie; Prozent bezieht sich auf alle gültigen Geschlechtsangaben (Gueltig_N). Fehlend_N nennt die Anzahl fehlender Angaben, nicht eine zusätzliche Geschlechtskategorie."
    )
    add(
      "### Bildung",
      "Beschrieben wird intro_education, der höchste angegebene Bildungsabschluss. Die Kategorien werden einzeln ausgezählt, nicht zu hoch/mittel/niedrig zusammengefasst. Prozent verwendet alle gültigen Bildungsangaben als Nenner. Hier wird Bildung mit keiner anderen Variable verglichen."
    )
    add(
      "### Teilnahmeumfang",
      "Die erste Tabelle beschreibt je eingeschlossener Person die Anzahl unterschiedlicher Uploads nach Abzug bekannter Duplikate sowie die Zahl aktiver Studientage. N ist hier eine Personenzahl. Die anschließende Studientagstabelle zählt dagegen pro study_day die vorhandenen codierten Screenshots und die unterschiedlichen Personen, die dazu beitragen. Diese zweite Tabelle verwendet die kleinere Hauptcoding-Basis. Die Summe ihrer Personenwerte ist keine Gesamtstichprobengröße, da Personen an mehreren Tagen vorkommen."
    )
    add(
      "### Plattformnutzung",
      "Die vier Variablen intro_freq_facebook, intro_freq_instagram, intro_freq_tiktok und intro_freq_x erfassen die allgemeine Nutzungshäufigkeit der jeweiligen Plattform, unabhängig vom Nutzungszweck. Die Antwortskala reicht von nie bis mehrmals täglich. Jede Plattform wird separat ausgewertet: n zählt Personen mit der jeweiligen Antwort, Prozent bezieht sich auf deren gültige Antworten für diese Plattform. Personen können mehrere Plattformen nutzen; Prozentwerte sind nicht über Plattformen zu addieren."
    )
    add(
      "### Nutzungsintensität, Bedürfnisse und Incidentality",
      "Die erste Tabelle beschreibt für jedes einzelne Intro-Item seine Zahlenwerte; die zweite zählt die Antwortkategorien. intro_intensity reicht von 1 = sehr passiv bis 7 = sehr aktiv. intro_ib_undirected meint Nachrichten/aktuelle Ereignisse, intro_ib_thematic persönliche Themen und Hobbys, intro_ib_social das soziale Umfeld und intro_ib_problem problemlösende Informationen; diese Wichtigkeitsangaben reichen von 1 = gar nicht wichtig bis 5 = sehr wichtig. intro_incidentality_1 bis _6 sind sechs Zustimmungsitems (1 = stimme gar nicht zu, 5 = stimme voll zu): sonst verpasste, sonst nicht gelesene, neue/interessante, normalerweise nicht gesuchte Informationen; keine Informationen außerhalb eigener Interessen; unbeabsichtigtes Lesen durch Posts. Dies sind sinngemäße Inhaltsangaben. In diesen Einzelitemtabellen bleibt Item 5 in seiner ursprünglichen Richtung. Es wird erst für den folgenden Index umgepolt. Die Tabellen berichten keine Zusammenhänge zwischen Bedürfnissen und Nutzungsintensität."
    )
    add(
      "### Incidentality: Mittelwertindex",
      "Beschrieben wird der pro Person gebildete Mittelwert aus intro_incidentality_1 bis _6 nach Umpolung von Item 5. Die erste Tabelle zeigt die Verteilung dieses Personenwerts. Die Reliabilitätstabelle prüft dagegen, wie die sechs Items als Skala zusammenpassen; sie korreliert den Index nicht mit Tagebuchangaben. Cronbachs Alpha, Omega H und Omega total sind unterschiedliche Maße interner Konsistenz. Omega H betrifft den gemeinsamen Generalfaktor; Omega total berücksichtigt zusätzlich Gruppenfaktoren. Hohe interne Konsistenz allein belegt keine inhaltliche Gültigkeit."
    )
    add(
      "### Räumlicher Kontext",
      "intro_context_local erfasst, wo eine Person soziale Medien typischerweise nutzt: zu Hause, unterwegs oder etwa gleich häufig an beiden Orten. Die Tabelle zählt Personen und deren gültige Antworten, keine einzelnen Tagebuchbeiträge."
    )
    add(
      "### Sozialer Kontext",
      "Im Intro-Abschnitt wird intro_context_situation beschrieben: typische Nutzung überwiegend allein, überwiegend gemeinsam oder etwa gleich häufig. Im Daily-Abschnitt wird dagegen situation_code für den jeweiligen Beitrag ausgewertet: allein, gemeinsam oder weiß nicht mehr. Intro-Prozente beziehen sich auf Personen; Daily-Prozente auf gültige Beiträge bzw. auf gemittelte Personenanteile. Der personenbezogene statistische Abgleich folgt unter Typischer Kontext und Tagebuch im Vergleich."
    )
    add(
      "## 4. Daily Coding",
      paste(
        "Jede folgende Tabelle beschreibt zunächst eine einzelne Beitragsvariable, nicht deren Zusammenhang mit anderen Merkmalen. n/Gueltig_N × 100 ergibt den screenshotgewichteten Prozentwert. Mittlerer_Anteil_Prozent ist der Durchschnitt der zuvor innerhalb jeder Person berechneten Kategorieanteile; SD_Prozentpunkte beschreibt deren Streuung zwischen Personen. Personen_N zählt Personen mit gültigen Daten zu diesem Merkmal. Das ist nicht die Zahl der Personen, die mindestens einen Beitrag in genau dieser Kategorie haben: Auch ein individueller Anteil von null geht in den Mittelwert ein.",
        practices
      )
    )
    add(
      "### Öffentliche Relevanz",
      "public_rel_coded klassifiziert alle im Hauptcoding verfügbaren Screenshots der eingeschlossenen Personen. Die erste Verteilung verwendet sämtliche gültigen Relevanzcodes als Nenner, einschließlich unklarer und nicht als Einzelbeitrag erkennbarer Bilder. Die zweite Quote verwendet dagegen nur eindeutig öffentlich relevante oder nicht relevante Beiträge: Zahl öffentlich relevanter Beiträge / (Zahl öffentlich relevanter + Zahl nicht relevanter Beiträge). Deshalb unterscheiden sich die beiden Prozentwerte; beide sind keine Quote aller hochgeladenen Dateien, da Uploads ohne Hauptcoding fehlen."
    )
    add(
      "### Datenqualität",
      "Geprüft wird zunächst, ob Inhaltscodes trotz public_rel_coded ungleich 1 vorliegen. Danach zählt die Tabelle für advertisement_coded, topic_coded, source_coded und media_format fehlende Werte ausschließlich unter öffentlich relevanten Beiträgen. Fehlend ist eine Beitragszahl je Variable; derselbe Beitrag kann bei mehreren Variablen fehlen."
    )
    for (pair in list(
      c(
        "Werbung",
        "advertisement_coded: Werbung/Anzeige, keine Werbung oder Restkategorie"
      ),
      c(
        "Thema",
        "topic_coded: codierte Themenkategorie des Beitrags"
      ),
      c(
        "Quelle",
        "source_coded: codierter Quellentyp, nicht individuelle Accountnamen"
      ),
      c(
        "Format",
        "media_format: Text-, Bild- oder Videoformat bzw. Restkategorie gemäß Codebook"
      ),
      c(
        "Plattform",
        "Plattform: vorrangig platform_coded, ersatzweise platform_reported"
      ),
      c(
        "Ort",
        "locality_code: zu Hause, unterwegs oder weiß nicht mehr"
      )
    )) {
      add(
        paste0(
          "### ",
          pair[1]
        ),
        paste0(
          "Ausgewertet wird ",
          pair[2],
          ". Grundlage sind öffentlich relevante Beiträge mit gültiger Angabe zu diesem Merkmal. Die obere Tabelle zählt Beiträge; die untere mittelt die Kategorieanteile je Person. Die Verteilung wird hier weder mit Begegnungsart noch mit einer Nutzungspraktik gekreuzt. Inhaltlich benannte Restkategorien werden als Kategorien ausgewiesen und sind von fehlenden Werten zu unterscheiden."
        )
      )
    }
    add(
      "### Begegnungsart",
      paste(
        encounters,
        "Die erste Tabelle zählt die drei Kategorien über alle gültigen Beiträge; die zweite mittelt deren Anteile je Person. Hier wird die Begegnungsart nicht mit Lesen oder Interaktion verglichen."
      )
    )
    for (pair in list(
      c(
        "Gruendlich angesehen",
        "interaction_read_code",
        "gründliches Lesen oder Anschauen"
      ),
      c(
        "Weiter recherchiert",
        "interaction_research_code",
        "weitere Informationssuche einschließlich Linkklick"
      ),
      c(
        "Interagiert",
        "interaction_engagement_code",
        "sichtbare Interaktion wie Liken, Kommentieren oder Weiterleiten"
      )
    )) {
      add(
        paste0(
          "### ",
          pair[1]
        ),
        paste0(
          "Die Variable ",
          pair[2],
          " erfasst die folgende selbstberichtete Praxis: ",
          pair[3],
          ". Ja und Nein beziehen sich auf den einzelnen öffentlich relevanten Beitrag. Fehlende Antworten entfallen nur für diese Praxis. Die obere Tabelle zeigt den Anteil über gültige Beiträge, die untere den mittleren Anteil je Person. Da mehrere Praktiken für einen Beitrag möglich sind, ergeben ihre Ja-Anteile zusammen nicht 100 Prozent. Es wird hier noch kein Unterschied nach Format, Begegnungsart oder Ort untersucht."
        )
      )
    }
    add(
      "### Itemverteilungen",
      "Beschrieben werden einzelne Outro-Zustimmungsantworten von 1 = stimme gar nicht zu bis 5 = stimme voll zu. outro_reactivity_1 bis _5 betreffen gezieltes Suchen für die Studie, unveränderte angezeigte Inhalte, unveränderte eigene Nutzung, mehr öffentlich relevante Inhalte und die Alltagstypik der Uploads. outro_ease_1 bis _8 betreffen Benutzerfreundlichkeit, wenige Teilnahmeschritte, Mühelosigkeit, Fehlerbehebung, erfolgreiche Nutzung, Installation, Codeeingabe und Orientierung. Die Tabellen enthalten die ursprünglichen Antworten, noch keine umgepolten Werte; deshalb bedeutet ein hoher Wert nicht bei jedem Reaktivitätsitem mehr Veränderung. Jedes Item wird einzeln beschrieben, nicht mit einer anderen Variable korreliert."
    )
    add(
      "### Vollständige Skalen",
      "Die Tabelle zeigt zwei getrennte Personenmittelwerte: Reaktivität aus outro_reactivity_1 bis _5 und Benutzerfreundlichkeit aus outro_ease_1 bis _8. Ein Wert wird nur berechnet, wenn alle Items der betreffenden Skala beantwortet sind. Bei Reaktivität werden die Items 2, 3 und 5 mit 6 minus Antwort umgepolt; höhere Skalenwerte bedeuten mehr berichtete Veränderung durch die Studie. Höhere Werte der Benutzerfreundlichkeit bedeuten eine positivere Bewertung. Beide Skalen reichen von 1 bis 5. N gilt jeweils für diese Skala. Es wird weder eine Differenz zwischen den beiden Skalen noch deren Korrelation berechnet. Insbesondere die Reaktivitätsskala ist wegen der anschließend dokumentierten Messprobleme vorsichtig zu interpretieren."
    )
    add(
      "### Interne Konsistenz",
      "Verglichen werden innerhalb jeder Skala die Antwortmuster ihrer Items. Alpha meint hier Cronbachs Alpha; Omega H und Omega total beziehen sich auf den angegebenen gemeinsamen Faktor bzw. gemeinsame und gruppenspezifische Faktoren. Faktoren nennt die verwendete Anzahl Gruppenfaktoren, N die vollständigen Personen. Eine unzulässige Lösung wird als NA ausgegeben und darf nicht als niedrige oder hohe Reliabilität gelesen werden. Die Tabelle ist keine Gegenüberstellung von App-Bewertung und tatsächlichem Teilnahmeerfolg."
    )
    add(
      "### Offene Antworten",
      "Gezählt werden nichtleere Antworten auf outro_problems_free und outro_suggestions_free getrennt. Dieselbe Person kann beide Fragen beantworten. Die Summe der Antwortzahlen ist daher nicht die Zahl unterschiedlicher Personen mit Feedback; diese wird im späteren Abschnitt Offene Rückmeldungen angegeben."
    )
    add(
      "### Generalfaktorladungen und Restvarianzen",
      "Jede Zeile bezeichnet ein Item im jeweiligen Omega-Modell. Die Generalfaktorladung beschreibt seine Beziehung zum angenommenen gemeinsamen Faktor; die Restvarianz den vom Modell nicht erklärten Anteil. Negative Restvarianzen sind unzulässige Schätzungen, sehr kleine Werte weisen auf eine grenznahe Lösung hin. Diese Parameter sind weder Mittelwerte der Antworten noch Zusammenhänge mit Tagebuchpraktiken."
    )
  }
  out <- character()
  for (line in lines) {
    out <- c(out, line)
    if (!is.null(explanations[[line]])) out <- c(out, "", explanations[[line]], "")
  }
  # Keine abschließenden Punkte an Stichpunkten oder nummerierten Listen
  bullet <- grepl("^([*+-] |[0-9]+[.] )", out)
  out[bullet] <- sub("[.]$", "", out[bullet])
  out
}
