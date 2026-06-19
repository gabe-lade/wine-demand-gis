# =============================================================================
# 08_welfare_calculation.R — port of 8_welfare_calculation.do (Table 4).
# Computes baseline & counterfactual inclusive values, consumer welfare
# (total / variety / price effects), industry revenue, and average prices.
# Uses rounded structural params (alpha=-0.16, sigma1=0.65, sigma2=0.47), as the original.
# =============================================================================
suppressMessages({library(data.table)})
ROOT <- local({a<-commandArgs(FALSE);f<-grep("^--file=",a,value=TRUE);p<-if(length(f))dirname(normalizePath(sub("^--file=","",f[1])))else normalizePath(getwd());while(!file.exists(file.path(p,"code","_config.R"))&&dirname(p)!=p)p<-dirname(p);p})
WF <- file.path(ROOT, "data/derived/welfare")
OUT <- file.path(ROOT, "output/tables"); dir.create(OUT, showWarnings=FALSE, recursive=TRUE)
NONCONV <- c(817,818,819,820,821,822,825,826)
a <- 0.16; s1 <- 0.65; s2 <- 0.47

d <- as.data.table(readRDS(file.path(WF, "welfare_base.rds")))[!market %in% NONCONV]
sim2 <- fread(file.path(WF, "simulated_price_share_second_iteration.csv"))[
          , .(product, market, price_bn = price_sim, share_im_bn = share_im_sim)]
fe2  <- readRDS(file.path(WF, "product_fe_coeff_2.rds"))     # product, FE4, FE4_counterfactual(=cf_2)
d <- merge(d, sim2, by=c("product","market"), all.x=TRUE)
d[, FE4_counterfactual := NULL]
d <- merge(d, fe2[, .(product, FE4_counterfactual)], by="product", all.x=TRUE)

# inclusive values + predicted shares for a given delta column -> returns per-market inclusive value
# (use safe internal names; `Im` would collide with base::Im, the complex imaginary-part function)
add_iv <- function(d, delta, tag) {
  d[, .eds1 := exp(get(delta)/(1-s1))]
  d[, .ihgm := (1-s1)*log(sum(.eds1)), by=.(market, winetype)]
  mw <- unique(d[, .(market, winetype, .ihgm)])
  mw[, .igm := (1-s2)*log(sum(exp(.ihgm/(1-s2)))), by=market]
  mk <- unique(mw[, .(market, .igm)]); mk[, .iv_m := log(1+exp(.igm))]
  d <- merge(d, mk, by="market", all.x=TRUE)
  d[, (paste0("predict_",tag)) := (.eds1*exp(.ihgm/(1-s2))*exp(.igm)) /
                                   (exp(.ihgm/(1-s1))*exp(.igm/(1-s2))*exp(.iv_m))]
  d[, c(".eds1",".ihgm",".igm",".iv_m") := NULL]
  list(d=d, Im=setNames(mk[, .(market, .iv_m)], c("market", paste0("Im_",tag))))
}

d[, delta_hat     := -a*price    + rest + FE4]
d[, delta_tilda   := -a*price_bn + rest + FE4_counterfactual]
d[, delta_tilda_b := -a*price    + rest + FE4_counterfactual]
r <- add_iv(d, "delta_hat",     "hat");     d <- r$d; Im_hat     <- r$Im
r <- add_iv(d, "delta_tilda",   "tilda");   d <- r$d; Im_tilda   <- r$Im
r <- add_iv(d, "delta_tilda_b", "tilda_b"); d <- r$d; Im_tilda_b <- r$Im

# ---- Consumer welfare (Table 4) ----
mk <- Reduce(function(x,y) merge(x,y,by="market"), list(Im_hat, Im_tilda, Im_tilda_b))
mk <- merge(mk, unique(d[, .(market, market_size)]), by="market")
cw   <- sum((mk$Im_hat - mk$Im_tilda)   / a * mk$market_size)/1e9
ve   <- sum((mk$Im_hat - mk$Im_tilda_b) / a * mk$market_size)/1e9
pe   <- sum((mk$Im_tilda_b - mk$Im_tilda)/ a * mk$market_size)/1e9

# ---- Industry revenue (Table 4) ----
d[, rev_hat   := predict_hat   * price]
d[, rev_tilda := predict_tilda * price_bn]
mr <- d[, .(rev_hat_total=sum(rev_hat), rev_tilda_total=sum(rev_tilda),
            market_size=market_size[1]), by=market]
industry_rev <- sum((mr$rev_hat_total - mr$rev_tilda_total)*mr$market_size)/1e9

# ---- Average prices by appellation (baseline) ----
d[California==1, appellation_label := "California"]
bp <- d[, .(avg_price = sum(sales*price)/sum(sales)), by=appellation_label][order(-avg_price)]

cat("\n================= TABLE 4 (R end-to-end) =================\n")
cat(sprintf("Consumer welfare (total) : %6.2f  $bn   (paper 1.19)\n", cw))
cat(sprintf("   variety effect        : %6.2f  $bn   (paper 1.18)\n", ve))
cat(sprintf("   price effect          : %6.2f  $bn   (paper 0.01)\n", pe))
cat(sprintf("Industry revenue         : %6.2f  $bn   (paper 4.18)\n", industry_rev))
cat(sprintf("TOTAL welfare gain       : %6.2f  $bn   (paper 5.37)\n", cw + industry_rev))
cat("\nBaseline average prices ($/bottle) by appellation:\n"); print(bp)

res <- data.table(metric=c("consumer_welfare","variety_effect","price_effect",
                           "industry_revenue","total"),
                  billions=c(cw, ve, pe, industry_rev, cw+industry_rev))
fwrite(res, file.path(OUT, "welfare_table_4.csv"))
fwrite(bp,  file.path(OUT, "welfare_table_4_baseline_prices.csv"))
cat("\nDONE welfare. Wrote output/tables/welfare_table_4.csv\n")
