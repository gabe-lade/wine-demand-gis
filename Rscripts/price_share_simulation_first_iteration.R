rm(list = ls())
library(magrittr)
library(tidyverse)
library(dplyr)
setwd ('Z:/NIELSENDATA/consumer_panel/codeSTATA/GianCarlo/Discrete Choice/Main/Data/Welfare')
sink("logfile_simulation_1", append = FALSE, split=TRUE) 

source('Z:/NIELSENDATA/consumer_panel/codeSTATA/GianCarlo/Discrete Choice/Main/Rscripts/Demand_functions.R')

winedata <-read.csv('data_for_counterfactual_first_iteration.csv')
save(winedata, file = "data_for_counterfactual_first_iteration.RData")

load("data_for_counterfactual_first_iteration.RData")
cat('Data Loaded. ', '\n')
start_time <- Sys.time()
winedata <- winedata %>% as_tibble() %>% arrange(market, product)

all_market_list <- winedata %>% 
  select(market) %>%
  distinct() %>%
  as.list()

cat('Entering Loop of all markets. ', '\n')
winedata_sim <- NULL
nonconverging_markets <- c()
for (market_num in 1:2600) {
  tryCatch({
  cat('Executing market = ', market_num,'\n')
  # loop over all the markets
  market_winedata = winedata %>% 
    filter(market == market_num) %>% 
    arrange(market, product)
  
  Own_mat <-market_winedata %>% 
    select(market, product, brand_name) %>%
    rename(product_i = product) %>% 
    mutate(product_j = product_i) %>% 
    rename(brand_name_i = brand_name) %>% 
    mutate(brand_name_j = brand_name_i) %>% 
    expand(nesting(product_i, brand_name_i),nesting(product_j, brand_name_j)) %>% 
    mutate(same_brand_dummy = if_else(
      (
        ((brand_name_i == brand_name_j) & ((brand_name_i != "Other_brands") | (brand_name_j != "Other_brands")))
        |
        ((product_i == product_j) & ((brand_name_i == "Other_brands") & (brand_name_j == "Other_brands")))
      ),
      1, 0)) %>% 
    select(product_i, product_j, same_brand_dummy) %>% 
    spread(product_j, same_brand_dummy) %>% 
    column_to_rownames(var = "product_i") %>% 
    data.matrix()
  
  # 2. Util function parameters 
  alphap <- -.16; sigma1 <- .65; sigma2 <- .47; 
  
  
  # 3. Evaluate FOC and iterate until converence 
  p_old <- market_winedata$marginal_cost
  err <- 2
  #max_err <- 0.5
  max_err <- 10^(-6)
  max_iter <- 200
  while_check = 0
  while (err>max_err) {
    deltaj_cf_next =  deltaj(p=p_old, 
                             alphap = alphap, 
                             FE1 = market_winedata$FE1, 
                             FE2 = market_winedata$FE2,
                             FE3 = market_winedata$FE3,
                             FE4 = market_winedata$FE4_counterfactual,
                             residual = market_winedata$residual_nested)
                             
    
    
    I_hgm_next =  ihgm(deltaj = deltaj_cf_next,
                       sigma1 = sigma1,
                       winetype = market_winedata$winetype,
                       market = market_winedata$market)
    
    #print(I_hgm_next)
    
    I_hgm_vec_next <- market_winedata %>%
      left_join(I_hgm_next, by=c("market", "winetype")) %>%
      select(ihgm)
    
    I_gm_next = igm(ihgm = I_hgm_next$ihgm,
                    sigma2 = sigma2,
                    market = I_hgm_next$market)
    
    #print(I_gm_next)
    
    I_gm_vec_next <- market_winedata %>%
      left_join(I_gm_next, by=c("market")) %>%
      select(igm)
    
    I_m_next = im(igm = I_gm_next$igm,
                  market = I_gm_next$market)
    #print(I_m_next)
    
    I_m_vec_next <- market_winedata %>%
      left_join(I_m_next, by=c("market")) %>% 
      select(im)
    
    
    market_winedata$share_im <- sharei(deltaj = deltaj_cf_next,
                                       sigma1 = sigma1,
                                       sigma2 = sigma2,
                                       ihgm = I_hgm_vec_next$ihgm,
                                       igm = I_gm_vec_next$igm,
                                       im = I_m_vec_next$im,
                                       market = market_winedata$market)
    
    sum_by_winetype_df <- market_winedata %>%
      group_by(market,winetype) %>%
      summarise(sum_by_winetype = sum(share_im))
    
    market_winedata <- market_winedata %>%
      select(-contains("sum_by_winetype")) %>% 
      left_join(sum_by_winetype_df, by=c("market", "winetype")) %>%
      mutate(share_ihgm = share_im/sum_by_winetype)
    
    market_winedata$share_igm_next <-
      sum_by_market_df <- market_winedata %>%
      group_by(market) %>%
      summarise(sum_by_market = sum(share_im))
    
    market_winedata <- market_winedata %>%
      select(-contains("sum_by_market")) %>% 
      left_join(sum_by_market_df, by=c("market")) %>%
      mutate(share_igm = share_im/sum_by_market)
    
    
    deriv_mat <- market_winedata %>% select(market,
                                            product,
                                            share_ihgm,
                                            winetype,
                                            share_igm,
                                            share_im) %>%
      mutate(sub_group = 1) %>%
      rename(product_i = product) %>%
      mutate(product_j = product_i) %>%
      rename(winetype_i = winetype) %>%
      mutate(winetype_j = winetype_i) %>%
      rename(sub_group_i = sub_group) %>%
      mutate(sub_group_j = sub_group_i) %>%
      mutate(share_jm = share_im) %>%
      expand(nesting(product_i, winetype_i, sub_group_i, share_im, share_ihgm, share_igm),
             nesting(product_j, winetype_j, sub_group_j, share_jm)) %>%
      mutate(D1_ij = if_else(product_i == product_j, 1, 0)) %>%
      mutate(D2_ij = if_else(winetype_i == winetype_j, 1, 0)) %>%
      mutate(D3_ij = if_else(sub_group_i == sub_group_j, 1, 0)) %>%
      mutate(deriv_ij = (abs(alphap)*
                           (((1/(1-sigma1))*D1_ij)
                            -(((1/(1-sigma1))-(1/(1-sigma2)))*share_ihgm*D2_ij)
                            -((sigma2/(1-sigma2))*share_igm*D3_ij)
                            -(share_im))*share_jm)) %>%
      select(product_i, product_j, deriv_ij) %>%
      spread(product_i, deriv_ij) %>%
      column_to_rownames(var = "product_j") %>%
      data.matrix()
    
    sh_d <- deriv_mat
    foc <- p_old - market_winedata$marginal_cost - solve(sh_d*Own_mat)%*%(market_winedata$share_im) 
    foc_norm <- norm(foc, type = "2")
    err <- foc_norm
    cat("\n Objective: ", err)
    while_check = while_check+1
    cat("ran while loop = ",while_check," times for market =  " , market_num,"\n")
    p_new <- market_winedata$marginal_cost + solve(sh_d*Own_mat)%*%(market_winedata$share_im)
    p_old <- p_new
    if (while_check >= max_iter){
      nonconverging_markets <- c(nonconverging_markets, market_num)
      break
    }
  }
  market_winedata$price_sim <- p_old
  market_winedata$share_im_sim <- market_winedata$share_im
  market_winedata_sim <- market_winedata %>% select(market, product, price, price_sim, share_im_sim)
  winedata_sim <- winedata_sim %>% bind_rows(market_winedata_sim)
  cat('Finished market = ', market_num,'\n')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

  write.csv(winedata_sim, "simulated_price_share_first_iteration.csv")
  end_time <- Sys.time()
  cat('Time taken = ', (end_time-start_time), '\n')
 
sink()
list(nonconverging_markets)