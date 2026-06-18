# Phase C — reorganization execution plan

Goal: restructure into an AEA- / repo-template-compliant package where the proprietary NielsenIQ
panel can be removed while the package stays reproducible.

> **Do the file MOVES only after the welfare simulation chain finishes** — the running scripts use
> absolute paths to `code/03_welfare/` and `Data/Welfare/`; moving those mid-run breaks them.
> Documentation (README, LICENSE, .gitignore, docs/) is already in place and safe to add now.

## Target structure

```
wine-demand-repo/
├── README.md / README.pdf          # done (export to PDF for AEA)
├── LICENSE                          # done
├── .gitignore                       # done
├── renv.lock                        # TODO: renv::snapshot()
├── code/                            # already in place (master.R, 01_build, 02_demand, 03_welfare)
├── data/
│   ├── raw/                         # panel_wine_module_2021.dta  (gitignored; REMOVED on publish)
│   │   └── README.md                # how to obtain from Kilts
│   ├── public/                      # committed (see codebook.md)
│   │   ├── cpi/ tax/ population/ area/ store_count/ distance/ brand_crosswalk/
│   └── derived/                     # gitignored; regenerated intermediates + Welfare/
├── output/                          # gitignored; tables/ figures/ numbers/
├── docs/                            # paper PDF, codebook.md, verification logs, this plan
├── legacy_stata/                    # original .do files (incl. 1a panel compile)
└── legacy_R/                        # original 3 R scripts
```

## Move mapping (execute post-sim)

| From | To |
|------|----|
| `Data/Wine Master Data/panel_wine_module_2021.dta` | `data/raw/panel_wine_module_2021.dta` |
| `Other Supplement Data/CPI data/CPI_deflator.dta` | `data/public/cpi/` |
| `Other Supplement Data/Tax Data/state_excise_tax.dta` | `data/public/tax/` |
| `Other Supplement Data/MSA_Population/*` | `data/public/population/` |
| `Other Supplement Data/Retail Store Count.../DEC_10_MSAAREA/*` | `data/public/area/` |
| `Other Supplement Data/Retail Store Count.../Stata_Data/*` | `data/public/store_count/` |
| `Other Supplement Data/Distance Instrument/*` | `data/public/distance/` |
| `Data/2a_region_brand.dta`, `Data/2b_external_match_basedonbrands.dta` | `data/public/brand_crosswalk/` |
| `Data/r_*.rds`, `Data/[1-9]_*.dta`, `Data/Welfare/` | `data/derived/` (gitignored) |
| `Results/`, `Elasticity Estimation/` | `output/` (gitignored) |
| `DoFile/` | `legacy_stata/` |
| `Rscripts/` | `legacy_R/` |
| `*.pdf` (paper), `VERIFICATION_LOG.md`, `Rport/R_PORT_LOG.md` | `docs/` |
| `_verify/`, `Rport/`, `resubmission/`, `DescriptiveStats/` | remove (or archive outside package) |

## Path edits required after moves

The R scripts currently hardcode `file.path(ROOT, "Data/...")` and
`file.path(ROOT, "Other Supplement Data/...")`. Update via:
- `_config.R`: set `RAW <- data/raw`, `PUBLIC <- data/public`, `DERIVED <- data/derived`,
  `OUTPUT <- output`. Prefer routing all scripts through these.
- Sweep replacements in `code/**`:
  - `Data/Wine Master Data/panel_wine_module_2021.dta` → `data/raw/panel_wine_module_2021.dta`
  - `Data/r_` → `data/derived/r_`  (intermediates)
  - `Data/Welfare/` → `data/derived/welfare/`
  - `Other Supplement Data/CPI data/` → `data/public/cpi/`  (and the other supplement subpaths)
  - `Data/2a_region_brand.dta` → `data/public/brand_crosswalk/2a_region_brand.dta` (and 2b)
- Re-run `Rscript code/master.R` end-to-end to confirm Tables 2/3/4 still reproduce after the moves.

## Other Phase C tasks
- Add `data/raw/README.md` (Kilts access link + NielsenIQ disclaimer).
- `renv::init()` / `renv::snapshot()` → commit `renv.lock`.
- Correct the stale "2007–17" comment in `legacy_stata/1_capture_geographic_origin_ava.do` and the
  R port to "2007–2019".
- `git init`; commit code + `data/public/` + docs + lockfile; verify `.gitignore` excludes the
  proprietary panel and all derived/output. Confirm `git status` shows no large/proprietary files.
- Export `README.md` → `README.pdf`.
- Final check: with `data/raw/` empty, `Rscript code/master.R` should fail fast with a clear
  "obtain NielsenIQ data from Kilts" message — proving the package is correctly data-removable.
