# Social media diary study: analysis code

This project contains the manuscript analyses and two optional exploratory analyses. Analysis scripts produce Excel tables, saved results and figures. Separate reporting scripts create Markdown reports and PDFs.

## Getting started

Open `Diary_Analysis_2026.Rproj` in RStudio and start a fresh R session. Run all commands from the project root. Place the seven required input files in `01_Data/` before running an analysis; study data are not included in the repository or the OSF code archive.

A fresh checkout needs no existing outputs or saved R workspace. RStudio supplies Pandoc for the optional reports; command-line users can install Quarto instead. The first setup needs internet access for missing R and TeX packages.

```r
source("02_Scripts/01_paper_analysis.R", encoding = "UTF-8")
source("02_Scripts/checks.R", encoding = "UTF-8")
```

The first script runs the manuscript analyses. The second checks sample selection, denominators, practice coding and model diagnostics. Run the checks immediately after the analysis, before loading report scripts.

Missing R packages are installed automatically from `https://cloud.r-project.org`. Package setup is shared in `02_Scripts/packages.R`; installation requires internet access and a writable R package library.

Results are available as R objects and in `outputs/`. Both main scripts print a short list of saved files on completion:

- `paper_tables.xlsx`: tables in manuscript order, survey index/item descriptives and all figure data.
- `paper_results.rds`: saved results for the reporting scripts.
- `sample_audit.csv`: inclusion decisions and exclusion reasons for each participant.
- `session_info.txt`: R and package versions used for the analysis.
- `figures/figure_1_topic_routes.png`, `figure_2_survey_diary.png`, `figure_3_practices.png`: the three manuscript figures.

`Survey_Indices` contains incidentalness (Intro), reactivity and ease of use (Outro), each with N, missing values, mean, SD, median, Q1/Q3, minimum/maximum, alpha, hierarchical/total omega and omega model status. `Survey_Intro_items` and `Survey_Outro_items` describe the original item responses. `Survey_Index_scoring` records item assignments, reversal rules and score interpretation. The four information needs are separate single items, not an additional composite index.

The scripts create `outputs/` when needed. Re-running an analysis replaces its output files. Input files are not modified.

## Manuscript analyses

| Section | File in `02_Scripts/analysis/` | Contents |
| --- | --- | --- |
| Methods and sample | `01_data_and_sample.R` | Data import, eligibility, sample selection, demographics, participation and stored pretest reliability estimates |
| RQ1: content | `02_content.R` | Topics, sources, formats, platforms, advertising and Tables A1-A2 |
| RQ2a: contexts | `03_contexts.R` | Spatial and social contexts; comparison with typical use reported in the initial survey |
| RQ2b: incidentalness | `04_incidentalness.R` | Mean index, alpha and omega, encounter routes, survey-diary correlations with bootstrap intervals and two topic models |
| RQ2c: practices | `05_practices.R` | Reading/viewing, further information seeking, visible interaction and "None of the above"; post-level and participant-level shares |
| Survey measures | `06_survey_measures.R` | All three Intro/Outro indices, reliability, item descriptives and scoring rules |
| Export | `07_export.R` | Excel workbook and saved results for the reports |

Run the entry script rather than sourcing these files individually: each analysis step uses objects prepared by earlier steps. `helpers.R` contains shared data rules and table summaries. The two topic models are exploratory random-intercept logistic models; the manuscript reports their omnibus tests with Holm correction.

## Two optional exploratory analyses

```r
source("02_Scripts/02_selected_exploration.R", encoding = "UTF-8")
```

1. **Facebook and Instagram within the same participants:** topic and source profiles among participants with at least three publicly relevant submissions on each platform. Each participant receives equal weight. The comparison is descriptive.
2. **Information needs and practices:** three selected associations: problem-solving needs and further information seeking; thematic interests and deliberate search; general orientation and thorough reading/viewing. Each analysis uses one pair of values per participant. Holm correction covers the three Spearman tests.

This script runs independently and preserves existing manuscript data objects. It writes `selected_exploration.xlsx`, `selected_exploration.rds` and two figures: `figures/exploration_platforms.png` and `figures/exploration_needs.png`. The workbook includes the data behind both figures.

## Reports and figures

Run the corresponding analysis first, then the required report:

```r
source("02_Scripts/reporting/01_paper_report.R", encoding = "UTF-8")
source("02_Scripts/reporting/02_exploration_report.R", encoding = "UTF-8")
```

Reports read saved results, redraw the figures and create Markdown and PDF in `outputs/`. They do not rerun analyses. Reporting uses knitr, rmarkdown, Pandoc and XeLaTeX; the main analysis scripts need no PDF software. Temporary PDF build files are removed automatically.

The three manuscript figures are defined in `02_Scripts/figures/paper_figures.R`, the exploratory figures in `02_Scripts/figures/exploration_figures.R`. Figure 1 highlights the three most frequent topics within each encounter route. Figures 2 and 3 show the survey-diary comparison and participant-level practice shares by encounter route.

The first report run installs TinyTeX if XeLaTeX is missing, plus the TeX packages required by the report template. Further missing TeX packages are installed during compilation. Installation requires internet access. Existing installations are reused, and XeLaTeX is checked before each build. No R restart is needed. Pandoc is detected through RStudio or an installed Quarto. Setup failures stop the report with an error.

To include a new manuscript table in a report, calculate it in the appropriate analysis step, add it to `paper_tables` in `07_export.R`, and add a `report_section()` call to the report. Each section has a title, a short explanation of the analysis purpose, a table and an optional figure. Rerun the analysis and then the report to update the saved results.

## Required input data

All seven files must be placed directly in `01_Data/`. Both analysis entry points use the same inputs.

| File | Contents |
| --- | --- |
| `01_Data/initial_survey.csv` | Original initial-survey export: screening, demographics, information needs, incidentalness items and typical contexts |
| `01_Data/daily_survey.csv` | Original daily-survey export: uploads and scheduled questionnaire dates |
| `01_Data/closing_survey.csv` | Original closing-survey export: completion status used for sample selection |
| `01_Data/coding_sheet.xlsx` | Final coding and linked daily responses; worksheets `Coding` and `Codebook` |
| `01_Data/upload_exclusions.csv` | Four documented technical exclusions |
| `01_Data/duplicate_uploads.csv` | 23 documented duplicate uploads |
| `01_Data/pretest_reliability.xlsx` | Stored pretest agreement estimates; worksheet `Reliability` |

The three survey CSV files use semicolons as delimiters; the two exclusion lists use commas. Keep the file names and worksheet names unchanged. Original questionnaire wording, raw variable names and image file names are retained in the data. Screenshot files and historical working folders are not required to run the analyses. Pretest estimates are imported, not recalculated.

## Sample and scoring rules

- Inclusion requires passing the screening, completing the initial and closing surveys (`committed` present), and submitting at least seven distinct uploads after exclusions. There is no minimum number of upload days. Additional completion flags are not used.
- The general incidentalness index is the mean of six items, with item 5 reversed. All six responses must be complete. Alpha and omega are calculated with `psych`; omega retains the exploratory specification with three group factors used in the manuscript.
- Reactivity is the mean of five Outro items, with items 2, 3 and 5 reversed as `6 - response`. Ease of use is the mean of eight Outro items without reversal. Both require all items and range from 1 to 5. Higher scores mean greater study reactivity or greater ease of use. Alpha uses complete cases; exploratory omega retains two group factors for reactivity and three for ease of use. Inadmissible omega solutions are marked and exported as missing estimates; near-boundary residual variances are flagged.
- Item directions were checked against the wording in OSF preregistration `qbxh8`. The full item batteries are retained. The preregistration permits removing one item if hierarchical omega is below .7 and removal raises it above .7; it does not require removal.
- Chance encounters are code 3. Encounters without targeted search combine codes 2 and 3. These definitions overlap.
- Three -1 codes in the practice responses indicate the explicit "None of the above" option and are recoded to three zeros. Missing values in the three practice variables remain missing.
- Participant-level means give each person equal weight. For practices by encounter route, participants contribute only to routes for which they have valid submissions. Groups can overlap and differ in size.
- Residual content categories remain valid categories. "Cannot remember" and missing context responses are excluded when calculating participant-level context shares.

## Code structure

- `analysis/`: numbered analysis steps and shared helpers.
- `figures/`: plots from calculated results.
- `reporting/`: Markdown/PDF presentation and two layout templates.
- `checks.R`: syntax, scoring, sample, model and output checks.

Keep code in short, named sections with descriptive variables and explicit intermediate steps. Comments explain study rules or non-obvious decisions. Reuse helpers for repeated logic; avoid wrappers, dependencies and folders without a concrete purpose.

`.gitignore` excludes local data, outputs and working files from Git and the OSF code archive. The repository includes the RStudio project and GPL v3 license; supply the seven input files separately.
