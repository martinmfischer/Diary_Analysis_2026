# Social media diary study: analysis code

This project contains the manuscript analyses and two optional exploratory analyses. Analysis scripts produce tables and saved results. Separate reporting scripts create Markdown reports, PDFs and figures.

## Getting started

Open `Diary_Analysis_2026.Rproj` in RStudio and start a fresh R session. Run all commands from the project root. Place the seven required input files in `01_Data/` before running an analysis; study data are not included in the repository or the OSF code archive.

```r
source("01_Scripts/01_paper_analysis.R", encoding = "UTF-8")
source("01_Scripts/checks.R", encoding = "UTF-8")
```

The first script runs the manuscript analyses. The second checks sample selection, denominators, practice coding and model diagnostics. Run the checks immediately after the analysis, before loading report scripts.

Packages are loaded with `pacman::p_load()`. Missing packages, including `pacman` itself, are installed automatically. An internet connection is required to install them.

Results are available as R objects and in `outputs/`:

- `paper_tables.xlsx`: tables in manuscript order.
- `paper_results.rds`: saved results for the reporting scripts.
- `sample_audit.csv`: inclusion decisions and exclusion reasons for each participant.
- `session_info.txt`: R and package versions used for the analysis.

The scripts create `outputs/` when needed. Re-running an analysis replaces its output files. Input files are not modified.

## Manuscript analyses

| Section | File in `01_Scripts/analysis/` | Contents |
| --- | --- | --- |
| Methods and sample | `01_data_and_sample.R` | Data import, eligibility, sample selection, demographics, participation and stored pretest reliability estimates |
| RQ1: content | `02_content.R` | Topics, sources, formats, platforms, advertising and Tables A1-A2 |
| RQ2a: contexts | `03_contexts.R` | Spatial and social contexts; comparison with typical use reported in the initial survey |
| RQ2b: incidentalness | `04_incidentalness.R` | Mean index, alpha and omega, encounter routes, survey-diary correlations with bootstrap intervals and two topic models |
| RQ2c: practices | `05_practices.R` | Reading/viewing, further information seeking, visible interaction and "None of the above"; post-level and participant-level shares |
| Export | `06_export.R` | Excel workbook and saved results for the reports |

Run the entry script rather than sourcing these files individually: each analysis step uses objects prepared by earlier steps. `helpers.R` contains shared data rules and table summaries. The two topic models are exploratory random-intercept logistic models; the manuscript reports their omnibus tests with Holm correction.

## Two optional exploratory analyses

```r
source("01_Scripts/02_selected_exploration.R", encoding = "UTF-8")
```

1. **Facebook and Instagram within the same participants:** topic and source profiles among participants with at least three publicly relevant submissions on each platform. Each participant receives equal weight. The comparison is descriptive.
2. **Information needs and practices:** three selected associations: problem-solving needs and further information seeking; thematic interests and deliberate search; general orientation and thorough reading/viewing. Each analysis uses one pair of values per participant. Holm correction covers the three Spearman tests.

This script can run independently of the manuscript analysis. It prepares the shared data in a separate environment, preserves existing manuscript data objects and does not refit the manuscript models. Its data objects are named `exploration_intro` and `exploration_posts`. Outputs are `selected_exploration.xlsx` and `selected_exploration.rds`.

## Reports and figures

Run the corresponding analysis first, then the required report:

```r
source("01_Scripts/reporting/01_paper_report.R", encoding = "UTF-8")
source("01_Scripts/reporting/02_exploration_report.R", encoding = "UTF-8")
```

Reports read saved results and create Markdown, PDF and figures in `outputs/`. They do not rerun the analyses. Reporting additionally uses ggplot2, knitr, rmarkdown, Pandoc and XeLaTeX. The analysis scripts do not require PDF software. Temporary PDF build files are removed automatically.

The three manuscript figures are defined in `01_Scripts/reporting/paper_figures.R`. Figure 1 highlights the three most frequent topics within each encounter route. Figures 2 and 3 show the survey-diary comparison and participant-level practice shares by encounter route.

To include a new manuscript table in a report, calculate it in the appropriate analysis step, add it to `paper_tables` in `06_export.R`, and add a `report_section()` call to the report. Each section has a title, a short explanation of the analysis purpose, a table and an optional figure. Rerun the analysis and then the report to update the saved results.

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
- Chance encounters are code 3. Encounters without targeted search combine codes 2 and 3. These definitions overlap.
- Three -1 codes in the practice responses indicate the explicit "None of the above" option and are recoded to three zeros. Missing values in the three practice variables remain missing.
- Participant-level means give each person equal weight. For practices by encounter route, participants contribute only to routes for which they have valid submissions. Groups can overlap and differ in size.
- Residual content categories remain valid categories. "Cannot remember" and missing context responses are excluded when calculating participant-level context shares.

## Repository files

The manuscript analysis requires `01_Scripts/01_paper_analysis.R` and the files in `01_Scripts/analysis/`. `02_selected_exploration.R` adds the optional explorations; `checks.R` checks the manuscript run. Reports additionally require the files in `01_Scripts/reporting/`, including both templates.

`.gitignore` excludes local data and generated files from Git. The RStudio project file sets up the project workspace. This README describes how to run the code; `LICENSE` contains the GNU GPL v3 license text.

Study data, outputs, local documentation, historical working folders and backups are not versioned or included in the OSF code archive. Supply the seven input files separately in `01_Data/` to run the analyses.
