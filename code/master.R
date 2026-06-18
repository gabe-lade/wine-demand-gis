# =============================================================================
# master.R — run the full pure-R analysis pipeline end-to-end.
#
#   Rscript code/master.R
#
# Each step is a standalone script run in its own R session (heavy memory use),
# reading the previous step's .rds output. Step 1 reads the proprietary 4.3 GB
# NielsenIQ panel; steps 2-5 read R-native intermediates under Data/.
#
# Reproduces (validated against the published Stata results and the paper):
#   Table 2  price -0.165, sigma1 0.648, sigma2 0.468; own elast -4.76; N 794,974
#   Table 3  WTP per bottle (Napa 6.18, Anderson Valley 9.83, France 3.49, ...)
# =============================================================================
ROOT <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."))
if (!dir.exists(file.path(ROOT, "code"))) ROOT <- normalizePath(getwd())

steps <- c(
  "code/01_build/01_capture_geographic_origin.R",   # 4.3 GB panel -> r_1  (~10 min)
  "code/01_build/02_varietal_and_supplements.R",    # + supplement merges -> r_2 (~10 min)
  "code/01_build/03_bottle_bulk_product.R",         # bottle/bulk collapse -> r_3 (~3 min)
  "code/01_build/04_demand_estimation_data.R",      # demand dataset      -> r_4 (~3 min)
  "code/02_demand/05_demand_estimation.R"           # Tables 2 & 3 + elasticities
)

Rbin <- file.path(R.home("bin"), "Rscript")
for (s in steps) {
  cat("\n===== RUN", s, "=====\n"); flush.console()
  st <- system2(Rbin, shQuote(file.path(ROOT, s)))
  if (st != 0) stop("step failed: ", s)
}
cat("\nPipeline complete. Outputs in output/ and Results/.\n")
