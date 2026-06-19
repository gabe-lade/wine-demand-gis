# =============================================================================
# 06b_marginal_cost.R — port of Rscripts/compute_marginal_cost.R (paths fixed).
# Recovers marginal cost per market by solving the Bertrand-Nash FOC.
# Reads data/derived/welfare/1_input_data_for_mc.csv ; writes data/derived/welfare/2_marginal_cost.csv
# =============================================================================
suppressMessages({library(magrittr); library(tidyverse)})
ROOT <- local({a<-commandArgs(FALSE);f<-grep("^--file=",a,value=TRUE);p<-if(length(f))dirname(normalizePath(sub("^--file=","",f[1])))else normalizePath(getwd());while(!file.exists(file.path(p,"code","_config.R"))&&dirname(p)!=p)p<-dirname(p);p})
WF <- file.path(ROOT, "data/derived/welfare")

winedata <- read.csv(file.path(WF, "1_input_data_for_mc.csv")) %>%
  as_tibble() %>% arrange(market, product)
cat("Data loaded:", nrow(winedata), "rows\n")
alphap <- -.16; sigma1 <- .65; sigma2 <- .47

winedata_sim <- NULL
for (market_num in 1:2600) {
  market_winedata <- winedata %>% filter(market == market_num) %>% arrange(market, product)
  if (nrow(market_winedata) == 0) next

  Own_mat <- market_winedata %>%
    select(market, product, brand_name) %>%
    rename(product_i = product) %>% mutate(product_j = product_i) %>%
    rename(brand_name_i = brand_name) %>% mutate(brand_name_j = brand_name_i) %>%
    expand(nesting(product_i, brand_name_i), nesting(product_j, brand_name_j)) %>%
    mutate(same_brand_dummy = if_else(
      (((brand_name_i == brand_name_j) & ((brand_name_i != "Other_brands") | (brand_name_j != "Other_brands")))
       | ((product_i == product_j) & ((brand_name_i == "Other_brands") & (brand_name_j == "Other_brands")))),
      1, 0)) %>%
    select(product_i, product_j, same_brand_dummy) %>%
    spread(product_j, same_brand_dummy) %>%
    column_to_rownames(var = "product_i") %>% data.matrix()

  deriv_mat <- market_winedata %>%
    select(market, product, share_ihgm, winetype, share_igm, share_im) %>%
    mutate(sub_group = 1) %>%
    rename(product_i = product) %>% mutate(product_j = product_i) %>%
    rename(winetype_i = winetype) %>% mutate(winetype_j = winetype_i) %>%
    rename(sub_group_i = sub_group) %>% mutate(sub_group_j = sub_group_i) %>%
    mutate(share_jm = share_im) %>%
    expand(nesting(product_i, winetype_i, sub_group_i, share_im, share_ihgm, share_igm),
           nesting(product_j, winetype_j, sub_group_j, share_jm)) %>%
    mutate(D1_ji = if_else(product_i == product_j, 1, 0)) %>%
    mutate(D2_ji = if_else(winetype_i == winetype_j, 1, 0)) %>%
    mutate(D3_ji = if_else(sub_group_i == sub_group_j, 1, 0)) %>%
    mutate(deriv_ji = (abs(alphap) *
                         (((1/(1-sigma1))*D1_ji)
                          - (((1/(1-sigma1))-(1/(1-sigma2)))*share_ihgm*D2_ji)
                          - ((sigma2/(1-sigma2))*share_igm*D3_ji)
                          - (share_im)) * share_jm)) %>%
    select(product_i, product_j, deriv_ji) %>%
    spread(product_i, deriv_ji) %>%
    column_to_rownames(var = "product_j") %>% data.matrix()

  market_winedata$marginal_cost <- market_winedata$price -
    solve(deriv_mat * Own_mat) %*% (market_winedata$share_im)
  winedata_sim <- bind_rows(winedata_sim, market_winedata %>% select(market, product, marginal_cost))
  if (market_num %% 200 == 0) cat("  market", market_num, "\n")
}
write.csv(winedata_sim, file = file.path(WF, "2_marginal_cost.csv"))
cat("DONE marginal_cost:", nrow(winedata_sim), "rows\n")
