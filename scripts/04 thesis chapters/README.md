# Thesis chapter notebooks

Analysis skeleton for the three chapters in the conceptual framework (October 2026). These are separate from the
existing analysis notebooks in `scripts/02 analysis/` and don't change them.

| Step | File | What it does |
|---|---|---|
| Settings | `configs/thesis_framework.yml` | Paths, species lists, thresholds, effort/cost inputs |
| Shared functions | `R/thesis_functions.R` | Arbimon backup import, BirdNET parsing, species crosswalk, ARU-point pairing, accumulation, occupancy and classifier metrics |
| 1. Prep | `scripts/01 prep/Thesis Data Prep.Rmd` | Reads point counts, the Arbimon backup and BirdNET tables; QC; one combined detection table; starts the shared validation set; saves `thesis_data.rds` |
| 2. Chapter 1 | `Ch1-PointCount_vs_ARU.Rmd` | Point counts vs ARUs vs both: species by method, accumulation per sample and per staff hour, detection niche, occupancy (PC only / ARU only / combined), cost-effort table, recommendation |
| 3. Chapter 2 | `Ch2-BirdNET_FewShot.Rmd` | Vocal similarity index (Raven), baseline BirdNET audit, clip library manifest, fine-tuning log, fine-tuned vs stock evaluation |
| 4. Chapter 3 | `Ch3-SALS_Benchmark.Rmd` | Arbimon PM / random forest vs stock vs fine-tuned BirdNET on the same test minutes, false positives, practical dimensions, decision checklist |

Run the prep notebook first, then any chapter. Sections whose inputs don't exist yet print a **To do** note instead of failing.

To keep local paths (e.g. the Arbimon backup on an external drive) out of the repo, copy the YAML somewhere else, edit
the paths, and set `Sys.setenv(THESIS_CONFIG = "path/to/your_copy.yml")` before knitting.

Raw inputs go in (paths set in the YAML):

- `data/arbimon/backup/`: the unzipped Arbimon project backup (all `*.0001.csv` tables)
- `data/birdnet/raw/`: BirdNET-Analyzer combined tables
- `data/validation/validation_set.csv`: shared labelled minutes (a starting version is written by the prep notebook)
- `data/raven_annotations/`, `data/birdnet/embeddings/`, `data/birdnet/finetuned/`: Chapter 2 inputs as they are created
