# =============================================================================
# _config.R  — shared paths, packages, and helpers for the R pipeline.
# Source this at the top of every script:  source("code/_config.R")
# Paths are RELATIVE to the project root (no absolute paths).
# =============================================================================

# Project root. Respect a caller-set ROOT; otherwise find the repo root by walking
# up from this script (or cwd) until we see a marker directory ("DoFile" or "code").
if (!exists("ROOT") || !nzchar(ROOT) || !dir.exists(file.path(ROOT, "code"))) {
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  start <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1])))
           else normalizePath(getwd())
  r <- start
  while (!all(dir.exists(file.path(r, c("code", "data")))) &&
         dirname(r) != r) r <- dirname(r)
  ROOT <- r
}
RAW     <- file.path(ROOT, "data/raw")             # proprietary NielsenIQ panel (gitignored)
PUBLIC  <- file.path(ROOT, "data/public")          # redistributable supplements
DERIVED <- file.path(ROOT, "data/derived")         # regenerated intermediates (gitignored)
OUTPUT  <- file.path(ROOT, "output")               # exhibits (tables/figures/numbers)

suppressMessages({
  library(haven)
  library(data.table)
  library(stringr)
  library(fixest)
})

# ---- small helpers -----------------------------------------------------------
# read a Stata .dta as data.table.
# NOTE: haven/ReadStat throws a spurious "Unable to allocate memory" on the large
# release-118 .dta files in this project (the 2-4 GB intermediates and the 4.3 GB
# panel). readstata13 reads them correctly, so we prefer it for .dta and fall back
# to haven. For the all-R pipeline, intermediates are written as .rds (see save_dt).
read_dta_dt <- function(path, ...) {
  out <- tryCatch(as.data.table(haven::read_dta(path, ...)), error = function(e) NULL)
  if (is.null(out)) {
    if (!requireNamespace("readstata13", quietly = TRUE))
      stop("haven failed and readstata13 is not installed: ", path)
    out <- as.data.table(readstata13::read.dta13(path, convert.factors = FALSE))
  }
  out
}

# read/write R-native intermediates (avoids the ReadStat bug; faster than .dta)
save_dt <- function(d, path) saveRDS(d, sub("\\.dta$", ".rds", path))
read_dt <- function(path) {
  rds <- sub("\\.dta$", ".rds", path)
  if (file.exists(rds)) as.data.table(readRDS(rds)) else read_dta_dt(path)
}

# regexm(x, pat): Stata-style "does x contain regex pat" -> logical/int
rgx <- function(x, pat) as.integer(stringr::str_detect(x, pat))

# stata_proper(x): faithful port of Stata's proper(). Capitalizes the first letter
# of every run of letters (i.e. any letter preceded by start-of-string or a
# non-letter), lowercasing the rest. IMPORTANT: this differs from stringr::str_to_title()
# on apostrophes/digits — Stata proper("COOK'S") == "Cook'S" (not "Cook's"). Brand
# crosswalk files (2a/2b) were built with Stata proper(), so merges require this.
stata_proper <- function(x) gsub("(^|[^[:alpha:]])([[:alpha:]])", "\\1\\U\\2",
                                 tolower(x), perl = TRUE)

# compare a ported variable against the Stata intermediate for validation
check_equal <- function(r_val, stata_val, label, tol = 1e-6) {
  if (is.numeric(r_val) && is.numeric(stata_val)) {
    d <- max(abs(r_val - stata_val), na.rm = TRUE)
    cat(sprintf("  [%s] max abs diff = %.3e %s\n", label, d, ifelse(d < tol, "OK", "*** MISMATCH")))
  } else {
    n <- sum(r_val != stata_val, na.rm = TRUE)
    cat(sprintf("  [%s] mismatched rows = %d %s\n", label, n, ifelse(n == 0, "OK", "*** MISMATCH")))
  }
}
