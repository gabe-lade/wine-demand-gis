# Reproduction Verification Log

**Paper:** Chandra, Moschini & Lade (2025), "Geographical indications and welfare:
Evidence from US wine demand," *American Journal of Agricultural Economics* 107(2):670–695.
DOI 10.1111/ajae.12499.

**Date run:** 2026-06-18
**Machine:** macOS (Darwin 25.5), 128 GB RAM
**Software:** Stata 19.5 MP (StataNow); SSC packages `reghdfe`, `ivreghdfe`, `ivreg2`,
`ranktest`, `ftools`, `estout` (`esttab`/`estfe`), `erepost`.

**Scope of this run:** Demand estimation through Tables 2 & 3 (steps 1–5). The ~8 hr welfare
counterfactual (Table 4, steps 6b–8) is deferred to a later pass.

---

## What was run

Pipeline executed from the proprietary 4.3 GB NielsenIQ panel
(`Data/Wine Master Data/panel_wine_module_2021.dta`), using path-patched copies of the original
do-files (Windows `Z:\...` paths replaced with the local repo path; originals untouched, in `DoFile/`):

| Step | File | Input → Output | Result |
|------|------|----------------|--------|
| 1 | `1_capture_geographic_origin_ava.do` | 4.3 GB panel → `1_capture_geographic_origin.dta` (2.8 GB) | OK |
| 2 | `2_capture_varietal_and_other_supplement_data.do` | + supplement merges → `2_varietal_and_supplement_data.dta` (2.0 GB) | OK |
| 3 | `3_bottle_bulk_summary_stats.do` | → `3_product_data.dta` (2.0 GB) | OK |
| 4 | `4_data_for_demand_estimation.do` | → `4_demand_estimation_data.dta` (626 MB) | OK |
| 5 | `5_demand_estimation.do` | → Table 2, Table 3, elasticities, simulated params | OK |

**Fixes required to run (documented for the reorg):**
1. All ~140 hardcoded Windows `Z:\NIELSENDATA\...\Main\` paths must be repointed (→ a single
   configurable project root in the reorg).
2. `estfe` (table export) depends on `erepost`, which is **not** pulled in by `ssc install estout`.
   It must be installed separately (`ssc install erepost`); otherwise step 5 aborts at the
   first `estfe` with `r(199) command erepost is unrecognized`, before the WTP regression and
   elasticities run.

---

## Sample / observation counts

| Quantity | Paper | Reproduced | Match |
|----------|-------|-----------|-------|
| Sample period | 2007–2019 | 2007–2019 (`year` min 2007, max 2019) | ✓ |
| Product-market observations | 794,985 | 794,985 | ✓ |
| Regression N | 794,974 | 794,974 | ✓ |

> **Note on a stale comment:** `1_capture_geographic_origin_ava.do` line 10 says "Restricting study
> period to 2007-17." This is incorrect/stale — the code only drops `year < 2007`, and the data run
> through 2019. The exact N match (794,974) confirms the realized sample equals the paper's. The
> comment should be corrected to 2007–2019 during the reorg.

---

## Table 2 — Demand parameter estimates (nested logit, preferred)

| Parameter | Paper | Reproduced | Match |
|-----------|-------|-----------|-------|
| Price α (Nested-IV) | −0.16 (0.0071) | −0.1647 (0.0071) | ✓ |
| σ₁ within wine type (Nested-IV) | 0.65 (0.0063) | 0.6477 (0.0060) | ✓ |
| σ₂ across wine types (Nested-IV) | 0.47 (0.024) | 0.4688 (0.0244) | ✓ |
| Price α (Logit-IV) | −0.21 (0.014) | −0.2051 (0.0141) | ✓ |
| Price α (Logit-OLS, no IV) | small, ~0 (attenuated) | −0.012 | ✓ |
| R² (nested) | 0.63 | 0.6325 | ✓ |

## Table 2 — Elasticities

| Elasticity | Paper | Reproduced | Match |
|------------|-------|-----------|-------|
| Own-price (Nested-IV) | −4.77 | −4.755 | ✓ |
| Own-price (Logit-IV) | −2.11 | −2.089 | ✓ |
| Own-price (Logit-OLS) | −0.13 | −0.127 | ✓ |
| Aggregate (Nested-IV) | −0.53 | −0.528 | ✓ |
| Aggregate (Logit-IV) | −0.66 | −0.658 | ✓ |
| Aggregate (Logit-OLS) | −0.04 | −0.040 | ✓ |
| Cross-price within wine type | small (+) | +0.0065 | ✓ |
| Cross-price across wine types | small (+) | +0.0023 | ✓ |

## Table 3 — Marginal WTP ($/bottle), selected

| Attribute | Paper | Reproduced | Match |
|-----------|-------|-----------|-------|
| Bulk (vs bottle) | −2.40 | −2.41 | ✓ |
| Chardonnay | 0.83 | 0.83 | ✓ |
| Anderson Valley | 9.83 | 9.83 | ✓ |
| Carneros | 9.28 | 9.27 | ✓ |
| Napa Valley | 6.18 | 6.18 | ✓ |
| Sonoma Valley | 12.0 | 12.0 | ✓ |
| France | 3.49 | 3.49 | ✓ |

---

## Conclusion

**Tables 2 and 3 reproduce essentially exactly** from the 4.3 GB compiled NielsenIQ panel, once
the Windows paths are repointed and `erepost` is installed. All headline parameters, elasticities,
sample sizes, and WTP estimates match the published values. The code is sound and reproducible.

**Outputs generated:** `Results/table_2.doc`, `Results/table_3.csv`,
`Results/elasticities_table_2.xlsx`, `Data/5_elasticity_simulation_data.dta`,
`Elasticity Estimation/simulated_parameters.dta`.

**Deferred (not run this engagement):** elasticity CIs (`6_elasticity_CI_table_B0.do`), the welfare
counterfactual (`7`, R simulations, `8` → Table 4), and robustness specs.

**Known portability fixes captured for the R rewrite / reorg:** single configurable project root
(no absolute paths); document the `erepost` dependency; correct the 2007–17 → 2007–2019 comment.
