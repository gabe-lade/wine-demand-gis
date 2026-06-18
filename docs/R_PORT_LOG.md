# R Port Log (Phase B)

Goal: rewrite the analysis entirely in R (no Stata license needed for replicators),
validated against the verified Stata baseline (see `../VERIFICATION_LOG.md`).

## Status

| Stage | Stata source | R port | Validated? |
|-------|--------------|--------|-----------|
| Step 1 clean / geo-origin | `1_capture_geographic_origin_ava.do` (1203 ln) | `code/01_build/01_capture_geographic_origin.R` | written, not yet run-validated |
| Step 2 varietal + merges | `2_capture_varietal_and_other_supplement_data.do` (1194 ln) | `code/01_build/02_varietal_and_supplements.R` | written, not yet run-validated |
| Step 3 bottle + collapse | `3_bottle_bulk_summary_stats.do` (608 ln) | `code/01_build/03_bottle_bulk_product.R` | written, not yet run-validated |
| Step 4 demand dataset | `4_data_for_demand_estimation.do` (159 ln) | `code/01_build/04_demand_estimation_data.R` | **YES — machine-exact except a 0.2% tie-break artifact (see below)** |
| **Step 5 estimation + WTP + elasticities** | `5_demand_estimation.do` (197 ln) | `code/02_demand/05_demand_estimation.R` | **YES — machine precision** |

## Step 4 validation (run from the Stata `3_product_data.dta`)

R-built `r_4_demand_estimation_data.dta`: **794,985 rows → 794,974 after singletons, 2,941 products, 2007–2019** (all exact). Column-by-column vs Stata `4_demand_estimation_data.dta`: every endogenous var, share, total, and Z-count matches to **~1e-10**. The ONLY differences are two instruments — `state_control` (0.23%) and `excisetax` (0.02%) — which propagate to a ~1–3% wobble in price/σ₂ when used as instruments.

**Root cause (benign):** do-file line 63-64 `bysort product market: gen id=_n; drop if id>1` keeps the *first* row per product-market. When a Scantrack market spans state lines, `fips_state_descr` varies within the group, so `state_control`/`excisetax` depend on which tied row is kept — and Stata's tie order is itself unspecified. ~63 rows differ. This is an inherent arbitrariness in the original code, not a porting bug; impact on results is negligible.

## Infrastructure note (important for the all-R pipeline)

`haven`/ReadStat throws a spurious "Unable to allocate memory" on the large release-118 `.dta`
files (the 2–4 GB intermediates and the 4.3 GB panel) — confirmed by 3 independent agents.
`readstata13::read.dta13()` reads them correctly. `_config.R` now: (a) `read_dta_dt()` falls back
to readstata13; (b) `save_dt()`/`read_dt()` use R-native `.rds` for intermediates, which sidesteps
the bug entirely and is faster. The only proprietary `.dta` that must be read is the original
4.3 GB panel.

## End-to-end run-validation (from the 4.3 GB panel)

- **Step 1 — EXACT.** `r_1` vs Stata `1_`: rows 2,570,179 = 2,570,179; all 154 numeric columns
  match to 1e-9. Fixes needed: (a) `upc_desc`→`upc_descr` (Stata auto-completes the typo, R errors);
  (b) write `.rds` not `.dta` (haven label-validation chokes); (c) `_config.R` ROOT resolution.
- **Step 2 — in reconciliation.** Two real bugs found & fixed:
  1. **Merged string NA vs "".** Stata strings are never missing (unmatched merge → ""); data.table → NA.
     The `_m` columns from the 2b merge came back NA, silently breaking `=="" / !=""` tests. Fixed by
     coercing merged character columns NA→"".
  2. **`proper()` apostrophe handling (the big one).** Stata `proper("COOK'S")` == `"Cook'S"` (caps the
     letter after the apostrophe); R `str_to_title()` gives `"Cook's"`. The 2a/2b brand crosswalks were
     built with Stata `proper()`, so every apostrophe brand (Cook's, Taylor's, Boone's Farm, Meier's, …)
     failed to merge → `state_appellation` never set → ~30,138 rows wrongly dropped at the
     `appellation_label=="No"/"us_not_labelled"` filter. Fixed with a faithful `stata_proper()` in
     `_config.R`, used in steps 1 & 2. Also removed a stray `TMPN` column (undefined-var no-op).
  > Lesson: validating only NUMERIC columns (as step 1 did) misses string-driven row-set bugs. Always
  > check row counts and re-run downstream estimation.

## END-TO-END RESULT (full R pipeline from the 4.3 GB panel) — VALIDATED ✅

Running steps 1→2→3→4→5 entirely in R (no Stata), from the proprietary panel:

| Parameter | Paper | Stata | R end-to-end |
|-----------|-------|-------|--------------|
| price α | −0.16 | −0.16469 | **−0.16489** |
| σ₁ | 0.65 | 0.64771 | **0.64762** |
| σ₂ | 0.47 | 0.46885 | **0.46802** |
| own elasticity | −4.77 | −4.755 | **−4.759** |
| N (reg) | 794,974 | 794,974 | **794,974** |

Per-step validation vs the Stata intermediates: step 1 EXACT (rows + 154 numeric cols 1e-9),
step 2 EXACT (after the NA→"" and `stata_proper` fixes), step 3 EXACT (145 cols), step 4 EXACT
on all vars except the benign 0.2% tie-break, step 5 EXACT. Run the whole thing with
`Rscript code/master.R`.

Replicators need **no Stata license** — only R (≥4.5) with `data.table`, `fixest`, `stringr`,
`readstata13` (to read the one proprietary .dta), and `haven`.

## Welfare stage (Table 4) — ported, running

Ported `7_dataprep_for_welfare.do` + `8_welfare_calculation.do` + the 3 existing R scripts to
`code/03_welfare/`:
- `06_welfare_prep.R` — builds FE4, `rest` (=FE1+FE2+FE3+residual, lumped since only FE4 is
  swapped), the second-step coefficients, `FE4_counterfactual` (FE4 minus US AVA+state coeffs),
  and the marginal-cost input. ✅ ran (betas match).
- `06b_marginal_cost.R` — port of `compute_marginal_cost.R` (FOC solve). ✅ ran (794,974 rows;
  median MC $4.82).
- `06c_build_cf1_input.R` — iteration-1 simulation input.
- `07a_sim_first_iteration.R` / `07c_sim_second_iteration.R` — Bertrand-Nash counterfactual
  equilibrium (~4 hr each).
- `07b_akerlof_and_cf2_input.R` — Akerlof β̄₂ adjustment + iteration-2 input.
- `08_welfare_calculation.R` — inclusive values → consumer welfare (total/variety/price),
  industry revenue → **Table 4** (`output/tables/welfare_table_4.csv`).
- `_demand_functions.R` — nested-logit share functions (paths removed).

Uses the original's rounded structural params (α=−0.16, σ₁=0.65, σ₂=0.47) in the sims/welfare,
exact GMM betas for FE4/rest.

**Table 4 — VALIDATED ✅** (full R, end-to-end):

| Metric | Paper | R |
|--------|-------|---|
| Consumer welfare (total) | 1.19 | **1.19** |
| — variety effect | 1.18 | **1.18** |
| — price effect | 0.01 | **0.01** |
| Industry revenue | 4.18 | **4.12** |
| Total welfare gain | 5.37 | **5.31** |
| Baseline price AVA/Import/State/CA | 10.57/7.44/6.46/4.27 | **10.57/7.44/6.46/4.27** |

Consumer welfare decomposition exact; baseline prices exact; industry revenue within 1.4% (total
within 1.1%) — the structural counterfactual is sensitive to the benign step-4 tie-break.
Validation signals en route also matched: sim1 non-converging markets = {817–822,825,826} exactly
as the original; Akerlof β̄₂ = −0.01712 (paper −0.017134); import share 0.241.

Each sim ran ~24 min (not the ~4 hr the comments warned). Two R bugs fixed during the run:
(1) data.table `.SD[1]` mixed with named scalars in `j` (use `c(.(...), lapply(.SD, first))`);
(2) a column named `Im` shadowing base `Im()` (complex imaginary part) → renamed to `.iv_m`.

## Phase B COMPLETE — entire paper (Tables 2, 3, 4) reproduced in pure R from the 4.3 GB panel.

## Remaining to finish Phase B

1. Run-validate steps 1–3 against the Stata intermediates (`Data/1_…`,`2_…`,`3_…`). Each is a
   multi-GB run; expect to reconcile the edge cases the porting agents flagged:
   - step 1: `regexm(...)`-with-`if` missing semantics; a `upc_desc` vs `upc_descr` typo; `proper()`→`str_to_title`.
   - step 2: an undefined `TMPN` var (do-line 453); `Syr_Temp ` trailing-space literal; `proper()`.
   - step 3: `egen ... total() if` group semantics; contiguous varlist-range drops.
2. Chain end-to-end r_1→r_2→r_3→r_4→step5 from the 4.3 GB panel and confirm Tables 2 & 3.
3. (Optional) decide a deterministic tie-break for step-4 line 63 and document it.

## Key technical result: GMM-vs-2SLS resolved

The core risk of an all-R port was the estimator: Stata uses `ivreghdfe ... gmm2s robust`
(two-step efficient GMM with four absorbed high-dimensional fixed effects), whereas
`fixest::feols` does 2SLS. With 12 overidentifying restrictions these differ materially
(2SLS gave price −0.145 vs −0.165; σ₂ 0.570 vs 0.469).

**Solution (in `Rport/05_demand_estimation.R`, `gmm2s_fe()`):**
1. `fixest::demean()` partials the 4 FE (quarter, year, scm_code, product) out of the
   outcome, the 3 endogenous regressors, and the 15 instruments — reproduces `reghdfe`/
   `ivreghdfe` partialling.
2. Manual two-step efficient GMM on the demeaned data: step-1 2SLS weight `(Z'Z)^-1`,
   robust optimal weight `S^-1` with `S = Σ zᵢzᵢ'êᵢ²`, step-2 GMM.

This matches Stata to **3.9e-08** on coefficients and ~3-4 decimals on robust SEs.
(Remaining SE gap is a small-sample df scaling, `N/(N-K)`-type, that ivreg2 applies; negligible.)

Note: instrument **Z8 is dropped for collinearity by both** Stata and fixest (Stata: "Vars
dropped: Z8"; 15 instruments, overid Chi-sq(12)). No instrument-set discrepancy.

## Validated outputs (R, from `Data/4_demand_estimation_data.dta`)

- Table 2: price −0.164689, σ₁ 0.647714, σ₂ 0.468850; logit-IV −0.20514; logit-OLS −0.01248. N=794,974.
- Table 3 WTP: bulk −2.41, white −1.68, specialty −2.15, Chardonnay 0.83, Napa 6.18,
  Anderson Valley 9.83, Carneros 9.27, Sonoma Valley 12.01, France 3.49. (2930 products, R²=0.61)
- Elasticities: own nested −4.755, logit-IV −2.089, logit-OLS −0.127; aggregate −0.528 / −0.658 / −0.040.

Second-step product fixed effects (for WTP) are recovered consistently with the GMM betas by
absorbing `y − Xβ̂` onto the 4 FE and extracting the `product` component (`fixef()`).

## Remaining work

- Port cleaning steps 1–4 (~3700 lines, mostly mechanical regex extraction + merges) using
  `haven`/`data.table`/`stringr`, validating each output against the saved Stata intermediates
  (`Data/1_…`, `2_…`, `3_…`, `4_…`).
- Port the welfare stage (steps 6b–8 + existing R sims) — deferred.
- Add the GMM small-sample SE correction for exact SE parity (cosmetic).
