# =============================================================================
# 06c_build_cf1_input.R — port of 7_dataprep_for_welfare.do Step 4.
# Merges marginal cost into the welfare base and writes the iteration-1
# counterfactual input for the price/share simulation.
# Lumping: FE1=rest (=FE1+FE2+FE3+residual), FE2=FE3=residual_nested=0, FE4 kept.
# =============================================================================
suppressMessages({library(data.table)})
ROOT <- "/Users/lade.10/Library/CloudStorage/Dropbox/Work/RESEARCH/wine-demand-repo"
WF <- file.path(ROOT, "data/derived/welfare")

d  <- as.data.table(readRDS(file.path(WF, "welfare_base.rds")))
mc <- fread(file.path(WF, "2_marginal_cost.csv"))[, .(product, market, marginal_cost)]
d  <- merge(d, mc, by = c("product","market"), all.x = TRUE)

cf1 <- d[, .(year, market, product, price,
             FE1 = rest, FE2 = 0, FE3 = 0, FE4, FE4_counterfactual,
             winetype, brand_name = brand_name_str, marginal_cost,
             share_igm, share_ihgm, share_im, residual_nested = 0)]
fwrite(cf1, file.path(WF, "data_for_counterfactual_first_iteration.csv"))
cat("wrote cf1 input:", nrow(cf1), "rows;", uniqueN(cf1$market), "markets;",
    "MC NA:", sum(is.na(cf1$marginal_cost)), "\n")
