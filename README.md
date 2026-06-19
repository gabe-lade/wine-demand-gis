# Replication package: "Geographical indications and welfare: Evidence from US wine demand"

Chandra, Raj, GianCarlo Moschini, and Gabriel E. Lade. 2025. "Geographical indications and
welfare: Evidence from US wine demand." *American Journal of Agricultural Economics* 107(2):
670–695. https://doi.org/10.1111/ajae.12499

Corresponding author: Gabriel E. Lade (glade@macalester.edu).

This package reproduces the paper's empirical results — the nested-logit demand estimates
(Table 2), the marginal willingness-to-pay estimates (Table 3), and the welfare counterfactual
(Table 4) — from NielsenIQ Consumer Panel data over 2007–2019.

> **A note on this README.** The README of record for an AEA submission must be a PDF. This
> Markdown file is the source; export it to `README.pdf` before submission.

---

## 1. Data availability statement

The primary data are the **NielsenIQ (formerly Nielsen) Consumer Panel** (Homescan) datasets,
which are **proprietary and confidential**. The authors accessed them through the **Kilts Center
for Marketing** at the University of Chicago Booth School of Business under a data-use agreement
that prohibits redistribution. We therefore **cannot include the NielsenIQ data in this package.**

- Source / access: NielsenIQ Datasets at the Kilts Center for Marketing,
  https://www.chicagobooth.edu/research/kilts/datasets/nielsenIQ-nielsen
  An institutional subscription and a signed data-use agreement are required. Any researcher with
  Kilts access can obtain the same Consumer Panel extracts (HMS annual files, 2007–2019).
- Disclaimer (per the data agreement): "Researcher(s) own analyses calculated (or derived) based
  in part on data from NielsenIQ Consumer LLC and marketing databases provided through the
  NielsenIQ Datasets at the Kilts Center for Marketing Data Center at The University of Chicago
  Booth School of Business. The conclusions drawn from the NielsenIQ data are those of the
  researcher(s) and do not reflect the views of NielsenIQ. NielsenIQ is not responsible for, had
  no role in, and was not involved in analyzing and preparing the results reported herein."
- Retention: the authors will preserve the analysis files and code for at least five years and
  will provide reasonable assistance with replication requests.

**What IS included** (non-proprietary, redistributable; in `data/public/`):

| File | Source |
|------|--------|
| `cpi/CPI_deflator.dta` | Consumer Price Index (BLS), annual deflators to 2019 dollars |
| `tax/state_excise_tax.dta` | State wine excise tax rates (state revenue depts / TTB) |
| `population/Population_2005-09.dta`, `Population_2010-19.dta` | MSA population (US Census) |
| `area/MSA_Area_2010Census.dta` | MSA land area (2010 US Census) |
| `store_count/foodstorecount_2012.dta`, `foodstorecount_2017.dta` | Food/beverage store counts (Census County Business Patterns) |
| `distance/distance.dta` | Market-to-appellation distances (authors' construction) |
| `distance/diesel_price.dta` | Diesel prices (EIA) |

**Restricted inputs that are NOT distributed** (required to run, available from the authors on
request, subject to the NielsenIQ agreement):

- `data/raw/panel_wine_module_2021.dta` (~4.3 GB) — the proprietary compiled NielsenIQ panel. A
  replicator with Kilts access regenerates it from the HMS annual files with
  `legacy_stata/1a_panel_compile_wine_2004_2019.do`.
- `data/public/brand_crosswalk/2a_region_brand.dta`, `2b_external_match_basedonbrands.dta` —
  hand-built brand→region crosswalks. These embed NielsenIQ `brand_descr` strings, so they are
  treated as restricted and are not redistributed; the authors will provide them to researchers
  with Kilts access. (They are used only in `code/01_build/02_varietal_and_supplements.R`.)

---

## 2. Computational requirements

The analysis runs in **R** (no Stata license required). The original Stata code is retained in
`legacy_stata/` for reference.

- R ≥ 4.5 (developed on 4.5.1).
- R packages: `data.table`, `fixest`, `haven`, `readstata13`, `stringr`, `tidyverse`
  (`dplyr`, `tidyr`, `magrittr`). A pinned `renv.lock` is provided.
  - `readstata13` is required to read the one large proprietary `.dta`; `haven`/ReadStat throws a
    spurious out-of-memory error on large release-118 `.dta` files.
- Hardware: developed on macOS with 128 GB RAM. Step 1 loads the 4.3 GB panel (~30 GB peak in R).
  A machine with ≥ 64 GB RAM is recommended.
- Approximate run time (on the development machine):
  - Steps 1–4 (data build): ~30 min total.
  - Step 5 (estimation, Tables 2 & 3): ~2 min.
  - Welfare (marginal cost + two counterfactual simulations + welfare calc, Table 4): **~8 hours**
    (the two equilibrium simulations dominate).

---

## 3. Description of programs

The pipeline is pure R under `code/`, orchestrated by `code/master.R`. Each step writes an
R-native `.rds` intermediate consumed by the next.

```
code/
  _config.R                         # paths, packages, helpers (read_dta_dt, stata_proper, ...)
  master.R                          # runs the full pipeline
  01_build/
    01_capture_geographic_origin.R  # clean panel; extract AVA/state/foreign geographic origin
    02_varietal_and_supplements.R   # extract varietal/type; merge public supplements
    03_bottle_bulk_product.R        # bottle/bulk split; collapse to product level (Table 1)
    04_demand_estimation_data.R     # product-market-quarter dataset; shares; instruments
  02_demand/
    05_demand_estimation.R          # nested-logit IV (two-step GMM); Tables 2 & 3; elasticities
  03_welfare/
    06_welfare_prep.R               # FE4, FE4_counterfactual, marginal-cost input
    06b_marginal_cost.R             # recover marginal cost (Bertrand-Nash FOC)
    06c_build_cf1_input.R           # iteration-1 counterfactual input
    07a_sim_first_iteration.R       # counterfactual equilibrium, iteration 1 (~4 hr)
    07b_akerlof_and_cf2_input.R     # Akerlof adjustment; iteration-2 input
    07c_sim_second_iteration.R      # counterfactual equilibrium, iteration 2 (~4 hr)
    08_welfare_calculation.R        # Table 4 (consumer welfare, industry revenue, prices)
    _demand_functions.R             # nested-logit share/inclusive-value functions
```

The original Stata do-files and the three original R scripts are kept in `legacy_stata/` and
`legacy_R/` for reference.

---

## 4. Instructions to replicators

1. Obtain NielsenIQ Consumer Panel access from the Kilts Center and place the compiled panel at
   `data/raw/panel_wine_module_2021.dta` (or regenerate it with
   `legacy_stata/1a_panel_compile_wine_2004_2019.do`).
2. Install R (≥ 4.5) and restore packages: `renv::restore()`.
3. Run the whole pipeline: `Rscript code/master.R`. Outputs land in `output/tables/`,
   `output/figures/`, and `output/numbers/`.
   - With `data/raw/` empty the build stops with a message pointing to the Kilts data.

---

## 5. List of exhibits and the programs that create them

| Exhibit | Program | Output |
|---------|---------|--------|
| Table 1 (product characteristic counts) | `code/01_build/03_bottle_bulk_product.R` | `output/tables/` |
| Figure 1 (shares & prices by origin) | `code/01_build/03_bottle_bulk_product.R` | `output/figures/` |
| Table 2 (demand parameters, elasticities) | `code/02_demand/05_demand_estimation.R` | `output/tables/` |
| Table 3 (marginal utility & WTP) | `code/02_demand/05_demand_estimation.R` | `output/tables/` |
| Table 4 (welfare, prices) | `code/03_welfare/08_welfare_calculation.R` | `output/tables/welfare_table_4.csv` |

Reproduced values match the paper: Table 2 α = −0.165, σ₁ = 0.648, σ₂ = 0.468, own-price
elasticity −4.76, N = 794,974; Table 3 WTP e.g. Napa Valley $6.18, Anderson Valley $9.83.

---

## 6. References

Chandra, R., G. Moschini, and G. E. Lade. 2025. "Geographical indications and welfare: Evidence
from US wine demand." *American Journal of Agricultural Economics* 107(2): 670–695.
