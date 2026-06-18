rm(list = ls())
library(magrittr)
library(tidyverse)
library(dplyr)
setwd ('Z:/NIELSENDATA/consumer_panel/codeSTATA/GianCarlo/Discrete Choice/Main/Data/Welfare')
# Define functions here
source('Z:/NIELSENDATA/consumer_panel/codeSTATA/GianCarlo/Discrete Choice/Main/Rscripts/Demand_functions.R')
winedata <-read.csv('1_input_data_for_mc.csv')
save(winedata, file = "winedata_mc.RData")
load("winedata_mc.RData")
cat('Data Loaded. ', '\n')
start_time <- Sys.time()
winedata <- winedata %>% as_tibble() %>% arrange(market, product)

all_market_list <- winedata %>% 
  select(market) %>%
  distinct() %>%
  as.list()

cat('Entering Loop of all markets. ', '\n')
winedata_sim <- NULL
for (market_num in 1:2600) {
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
      mutate(D1_ji = if_else(product_i == product_j, 1, 0)) %>%
      mutate(D2_ji = if_else(winetype_i == winetype_j, 1, 0)) %>%
      mutate(D3_ji = if_else(sub_group_i == sub_group_j, 1, 0)) %>%
      mutate(deriv_ji = (abs(alphap)*
                           (((1/(1-sigma1))*D1_ji)
                            -(((1/(1-sigma1))-(1/(1-sigma2)))*share_ihgm*D2_ji)
                            -((sigma2/(1-sigma2))*share_igm*D3_ji)
                            -(share_im))*share_jm)) %>%
      select(product_i, product_j, deriv_ji) %>%
      spread(product_i, deriv_ji) %>%
      column_to_rownames(var = "product_j") %>%
      data.matrix()
    
    sh_d <- deriv_mat
    market_winedata$marginal_cost <- market_winedata$price - solve(sh_d*Own_mat)%*%(market_winedata$share_im)
    market_winedata_sim <- market_winedata %>% select(market, product, marginal_cost)
    winedata_sim <- winedata_sim %>% bind_rows(market_winedata_sim)
    cat('Finished market = ', market_num,'\n')
}
print(winedata_sim)
cat('loopfinished')
write.csv(winedata_sim, file = "2_marginal_cost.csv")
end_time <- Sys.time()
cat('Time taken = ', (end_time-start_time), '\n')

