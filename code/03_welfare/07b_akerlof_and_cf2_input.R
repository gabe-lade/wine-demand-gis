# =============================================================================
# 07b_akerlof_and_cf2_input.R — port of 7_dataprep_for_welfare.do Steps 6-8.
# Uses iteration-1 counterfactual shares to compute the Akerlof adjustment
# (beta_bar_2 = share-weighted average MU of US geographic origin), shifts
# FE4_counterfactual, and writes the iteration-2 simulation input.
# =============================================================================
suppressMessages({library(data.table)})
ROOT <- "/Users/lade.10/Library/CloudStorage/Dropbox/Work/RESEARCH/wine-demand-repo"
WF <- file.path(ROOT, "Data/Welfare")
NONCONV <- c(817,818,819,820,821,822,825,826)   # markets that did not converge

base <- as.data.table(readRDS(file.path(WF, "welfare_base.rds")))
par  <- readRDS(file.path(WF, "welfare_params.rds"))
cf   <- par$mu_coef; geo <- par$geo_cf
mc   <- fread(file.path(WF, "2_marginal_cost.csv"))[, .(product, market, marginal_cost)]

# iteration-1 simulated shares
sim1 <- fread(file.path(WF, "simulated_price_share_first_iteration.csv"))[
          , .(product, market, share_im_bn = share_im_sim)]
sim1 <- sim1[!market %in% NONCONV]

d <- merge(base[!market %in% NONCONV], mc, by=c("product","market"), all.x=TRUE)
d <- merge(d, sim1, by=c("product","market"), all.x=TRUE)

domestic_quantity <- sum(d$share_im_bn, na.rm=TRUE)
import_share <- sum(d$share_im_bn[d$appellation_label=="Imported"], na.rm=TRUE)/domestic_quantity
cat(sprintf("import_share = %.4f (paper 0.2411)\n", import_share))

# product-level share weights and Akerlof term
prod <- d[, c(.(FE4 = FE4[1], FE4_counterfactual = FE4_counterfactual[1],
               appellation_label = appellation_label[1],
               product_share = sum(share_im_bn, na.rm=TRUE)/domestic_quantity),
              lapply(.SD, function(x) x[1])), by=product, .SDcols=geo]
gm <- as.matrix(prod[, ..geo])
prod[, beta_2_bar := as.numeric(gm %*% cf[geo]) * product_share / (1 - import_share)]
beta_bar_2_final <- sum(prod$beta_2_bar)
cat(sprintf("beta_bar_2_final = %.6f (paper -0.017134)\n", beta_bar_2_final))
prod[, FE4_cf_2 := FE4_counterfactual + ifelse(appellation_label=="Imported", 0, beta_bar_2_final)]

# build iteration-2 input (FE4_counterfactual <- FE4_cf_2)
d <- merge(d, prod[, .(product, FE4_cf_2)], by="product", all.x=TRUE)
cf2 <- d[, .(year, market, product, price,
             FE1 = rest, FE2 = 0, FE3 = 0, FE4, FE4_counterfactual = FE4_cf_2,
             winetype, brand_name = brand_name_str, marginal_cost,
             share_igm, share_ihgm, share_im, residual_nested = 0)]
fwrite(cf2, file.path(WF, "data_for_counterfactual_second_iteration.csv"))
saveRDS(prod[, .(product, FE4, FE4_counterfactual = FE4_cf_2)], file.path(WF, "product_fe_coeff_2.rds"))
cat("wrote cf2 input:", nrow(cf2), "rows;", uniqueN(cf2$market), "markets\n")
