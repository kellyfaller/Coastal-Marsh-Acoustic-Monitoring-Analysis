# BirdNET Results and SALSNET Model Evaluation

This guide covers the code that processes the stock BirdNET results (model **M0**) and
evaluates the saltmarsh sparrow classifiers (models **M1–M5**). It follows the
[**Thesis Analysis Plan: Updated Research Questions and Chapter Design**](../docs/20261024_updated_analysis_plan.docx)
(Kelly Faller, Oct 24, 2026; `docs/20261024_updated_analysis_plan.docx`). Step numbers in
brackets, such as [B1.3], refer to the steps in that plan.

> [!NOTE]
> This code covers **Phase A3** (stock BirdNET as M0) and the **evaluation and comparison** parts of
> **Phase B** (B1.3–B1.5, B2, B5.4–B5.5, B6, B7). Building the clip library (B3), the
> separability check (B4), training the classifiers (B5.1–B5.3) and the Chapter 1 survey
> comparison (Phase C) are not written yet.

Focal species: **SALS** (Saltmarsh Sparrow), **NESP** (Nelson's Sparrow) and **SESP** (Seaside Sparrow).

---

## Workflow at a glance

| Order | Script | Plan step | What it does |
|---|---|---|---|
| 1 | `scripts/01 prep/birdnet_combine_year.R` | A3 | Stacks every station's `BirdNET_CombinedTable` into one file per year |
| 2 | `scripts/02 analysis/04-BirdNET-Raw-Summary.Rmd` | A3 | Summary of the raw M0 results, focused on the three sparrows |
| 3 | `scripts/01 prep/select_test_clips.R` | B1.3 | Picks the test clips to label from M0 scores and writes a label sheet |
| — | *(you, in Raven)* | B1.4 | Label every sound in each test clip |
| — | *(you, BirdNET-Analyzer)* | B5.1–B5.3 | Train M1–M5 and run each on the test audio |
| 4 | `scripts/02 analysis/05-SALSNET-Model-Evaluation.Rmd` | B1.5, B2, B5.4–B5.5, B6, B7 | Scores and compares every model run found so far |

Shared functions live in `R/`:

- `R/birdnet_io.R` reads BirdNET tables from station folders and adds Station, Site and date/time.
- `R/model_evaluation.R` holds all the evaluation metrics and model comparison functions.

All settings are in `configs/project.yml` (see [Settings](#settings)).

---

## 1. Combine the raw BirdNET results [A3]

**Script:** `scripts/01 prep/birdnet_combine_year.R`

For each year in `birdnet_years` (currently 2024, 2025 and 2026), the script looks in
`data/processed/birdNET/<year>/<station>/` and stacks every file named
whose name contains `BirdNET_CombinedTable`, such as `BirdNET_CombinedTable.csv` or
`Unit4_CAT_05_2026_BirdNET_CombinedTable.csv` (searching subfolders too). Years without a folder yet are
skipped, so 2025 is picked up automatically once its BirdNET run is finished.

Columns added:

| Column | Meaning |
|---|---|
| `Station` | ARU station folder name (e.g. `CAT_A1`, `LHC_01`) |
| `Site` | `Cattus_Island` (stations starting with CAT) or `Lighthouse_Center` (starting with LHC) |
| `Date`, `Hour`, `Detection_Time`, `Recording_Start` | From the `YYYYMMDD_HHMMSS` stamp in the audio file name (recorder clock time) |
| `Audio_File`, `Source_Table` | Audio file name and which combined table the row came from |

**Output:** `data/processed/birdNET/birdNET_<year>_combined.csv`

## 2. Summarize the raw results [A3]

**Report:** `scripts/02 analysis/04-BirdNET-Raw-Summary.Rmd`

Knit after step 1. Every count is a raw BirdNET prediction, not a verified detection.

- Overview per station: detections, species, active days and date range
- Confidence distribution, and how many detections remain at each cut-off (`thresholds` in the config)
- Most detected species and species richness by cut-off
- For SALS, NESP and SESP:
  - detection counts by cut-off
  - confidence summaries
  - share of days with a detection
  - daily, monthly and time-of-day patterns
  - segments where BirdNET predicted more than one of the sparrows (likely confusion)
- Clips to listen to: a random sample from each confidence bin, plus the top clip per station

**Outputs:** `outputs/reports/birdnet_raw_summary/` (tables) and
`outputs/figures/birdnet_raw_summary/` (figures)

> [!TIP]
> Nelson's Sparrow is mainly a migrant in New Jersey. Treat mid-summer NESP predictions as
> possible Saltmarsh Sparrows until they are checked.

---

## 3. Pick and label the test clips [B1.1–B1.4]

1. **Choose the test sets [B1.1–B1.2].**
   - `local`: one whole site-year of your ARU data that is never used for training (e.g. Lighthouse 2024).
   - `independent`: partner sites kept out of training. Leave out any clips already on
     Xeno-canto or Macaulay Library.
2. **Run stock BirdNET on the test audio** with `--combine_results` and `--min_conf 0.01`.
   Save the output to
   `data/processed/model_eval/scores/M0/<local or independent>/<station>/BirdNET_CombinedTable.csv`.
   Without the low `min_conf`, BirdNET drops low scores, so the 0–0.1 bin is empty and
   PR-AUC is wrong.
3. **Run `scripts/01 prep/select_test_clips.R` [B1.3].** For each focal species at each
   site, it picks:
   - 1 clip scoring 0–0.1
   - 6 clips spread across 0.1–1.0
   - the highest- and lowest-scoring clips

   A clip chosen for more than one species is listed once. Outputs:
   - `data/processed/model_eval/test_clips.csv` is the clip list, with what each clip was selected for and its M0 scores.
   - `data/processed/model_eval/test_labels.csv` is the label sheet. M0 scores are left
     off so they don't influence what you hear, and an existing sheet is never overwritten.
4. **Label every sound in each clip in Raven [B1.4].** Fill in the `Labels` column with
   codes separated by `;`, for example `SALS;WIND` or `SESP;BOAT;INSECT`.
   - Leave it blank or type `NONE` when no labeled sound is present.
   - Type `UNKNOWN` when the clip can't be identified. Those clips are dropped.

   If you label inside Raven selection tables instead, `raven_to_clip_labels()` in
   `R/model_evaluation.R` converts them to the same format.

## 4. Score the models [B5.5]

Each model's BirdNET-Analyzer output on the test audio goes in its own folder:

```
data/processed/model_eval/
├── test_labels.csv
├── scores/
│   ├── M0/local/<station>/BirdNET_CombinedTable.csv
│   ├── M0/independent/<station>/BirdNET_CombinedTable.csv
│   ├── M1/...                     full models M1–M5
│   └── M1_n016_d03/...            clip-number runs: <model>_n<clips per class>_d<draw> [B5.4]
├── full_run/<best model>/<station>/BirdNET_CombinedTable.csv   (B7, optional)
├── verified_station_days.csv      (B7, optional: Station, Date, Code, Present)
└── iteration_log.csv              (written by the report)
```

Run every model with the same settings on the same test audio (`--combine_results`,
`--min_conf 0.01`, no overlap) so segments line up with the labeled clips.

## 5. The evaluation report

**Report:** `scripts/02 analysis/05-SALSNET-Model-Evaluation.Rmd`

The report runs on whatever has been scored so far, so it can be re-knit after every
training round. Folders that aren't a model listed in the config are skipped.

| Section | Plan step | What it shows |
|---|---|---|
| 1. Test sets | B1.5 | Positive clips per species; species with 5 or fewer are flagged as too rare to score |
| 2. Load model scores | B2.1, B5.5 | Which runs were found on which test set |
| 3. Model scores | B2.1, B5.5 | All metrics at the 3-s clip and station-day level, plus PR curves |
| 4. What causes the errors | B2.2–B2.3 | Each false positive/negative and what was really in the clip; species with M0 AP < 0.8 flagged for training |
| 5. Sparrow confusion matrix | Results table | Which sparrow is mistaken for which |
| 6. Model comparison | B6.1–B6.3 | AP difference from M0 with paired-bootstrap 95% intervals; ranking and best model |
| 7. Score cut-off | B6.4 | Logistic fit of correct ~ score; the score where a detection is 90% likely to be correct |
| 8. Training clips needed | B5.4 | AP vs. clips per class, mean and range across random draws |
| 9. Iteration log | B5.5 | Appends every scored run to `iteration_log.csv` |
| 10. Station-day agreement | B7.2–B7.4 | Best model's station-day presence (≥ 2 detections above the cut-off) vs. verified presence |

**Outputs:** `outputs/model_eval/` (tables and figures) and
`data/processed/model_eval/iteration_log.csv`

### Metrics (the plan's "How results are judged")

| Measure | Function | Notes |
|---|---|---|
| Precision, recall, F1 | `threshold_metrics()` | At `evaluation$threshold` (default 0.5) |
| Average precision (PR-AUC) | `average_precision()` | Main measure; the same step-wise method as scikit-learn |
| ROC-AUC | `roc_auc()` | For comparison with published studies |
| Confusion matrix | `focal_confusion()` | SALS, NESP, SESP, "None" and "Multiple" |
| Cut-off for 90% precision and recall at it | `precision_cutoff()`, `logistic_cutoff()` | Empirical and Wood & Kahl (2024) versions |
| Station-day agreement | `station_day_scores()`, `station_day_agreement()` | Precision, recall, agreement and Cohen's kappa |

### How the best model is picked [B6.1–B6.3]

`rank_models()` follows the plan:

1. Rank models by SALS average precision on the **independent** test set.
2. A model must also beat M0 on the **local** test set: the lower end of its bootstrap interval for
   the AP difference is above 0.
3. Models within `tie_tolerance` (0.02 AP) of the best count as tied. Among tied models,
   the one with fewer training sources wins, because it is cheaper for partners to reproduce.

### Iteration log [B5.5]

Every scored run is added to `iteration_log.csv`, with its model, sources, clips per
class, draw, test set, species and metrics. To record a run that failed to train, add a
row by hand with `Status = failed`. Hand-added rows are kept when the log is rebuilt.

---

## Settings

All in `configs/project.yml`:

| Setting | Default | Plan step |
|---|---|---|
| `paths$birdnet_raw_dir` | `data/processed/birdNET` | A3 |
| `params$birdnet_years` | `[2024, 2025, 2026]` | A3 |
| `paths$model_eval_dir` | `data/processed/model_eval` | B |
| `evaluation$focal_species` | SALS, NESP, SESP | — |
| `evaluation$test_sets` | `local`, `independent` | B1.1–B1.2 |
| `evaluation$test_clip_bins` | 1 clip at 0–0.1, 6 at 0.1–1.0 | B1.3 |
| `evaluation$min_positives` | 5 | B1.5 |
| `evaluation$ap_flag` | 0.8 | B2.3 |
| `evaluation$models` | M0–M5 and their training sources | B5 |
| `evaluation$threshold` | 0.5 | Results table |
| `evaluation$tie_tolerance` | 0.02 | B6.3 |
| `evaluation$target_precision` | 0.9 | B6.4 |
| `evaluation$min_detections_per_day` | 2 | B7.2 |
| `evaluation$n_boot` | 1000 | B6 |

To add or change a model, edit its entry under `evaluation$models`. The number of
`sources` is what B6.3 uses to break ties.

## Caveats

- **Test clips are picked by M0 score, not at random [B1.3].** Precision, recall and F1
  describe the test set, not all of the audio, and models are judged on clips that M0
  selected. This is fine for ranking models and for the logistic cut-off (B6.4), which
  models precision at each score. But recall shouldn't be read as the share of all
  real calls a model finds.
- **Small test sets.** About 9 clips per species per site means few positives, especially
  for NESP. Check section 1 of the report before reading the metrics.
- **Recorder time.** Dates and hours come from file names and are whatever the ARU clock
  was set to.

## Packages

Install once:

```r
install.packages(c("data.table", "dplyr", "tidyr", "ggplot2", "yaml", "here", "knitr", "rmarkdown"))
```

Needs dplyr 1.1.0 or newer and ggplot2 3.4.0 or newer (tested with R 4.3, dplyr 1.1.4,
ggplot2 3.4.4, data.table 1.14.10). Check with `packageVersion("dplyr")`.

Scripts find files relative to the repository root with `here()`. That works as long
as the script is inside your cloned repository folder; you don't need to set a working
directory. Run the `.R` scripts with **Source** in RStudio and the `.Rmd` reports with **Knit**.

## Quick start

Either open each file and click **Source** / **Knit**, or run these from the R console
after setting the working directory to the repository folder
(`setwd("C:/Users/kelly/Documents/GitHub/<repo folder>")`):

```r
source("scripts/01 prep/birdnet_combine_year.R")    # 1. combine (a few seconds)
rmarkdown::render("scripts/02 analysis/04-BirdNET-Raw-Summary.Rmd")   # 2. raw summary
# 3. put M0 scores on the test audio in data/processed/model_eval/scores/M0/<test set>/<station>/
source("scripts/01 prep/select_test_clips.R")       # 4. pick test clips, writes the label sheet
# 5. label the clips in Raven and fill in test_labels.csv
rmarkdown::render("scripts/02 analysis/05-SALSNET-Model-Evaluation.Rmd")  # 6. evaluate
```

Each step stops with a message naming the step to run first if its inputs don't exist yet.

## References

The plan's methods draw on these sources (full citations are in the
[analysis plan](../docs/20261024_updated_analysis_plan.docx)):

- Jacuzzi & Olden (2025) — four-step evaluation workflow, test clip selection, ≥ 2 detections per site
- Wood & Kahl (2024) — logistic score cut-off for a target precision
- Ziegenhorn et al. (2025) — ROC-AUC comparison with stock BirdNET
