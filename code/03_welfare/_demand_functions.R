# Nested-logit demand functions (port of Rscripts/Demand_functions.R, paths removed).
# outside option mean utility normalized to 0.
deltaj <- function(p, alphap, FE1, FE2, FE3, FE4, residual) {
  FE1 + FE2 + FE3 + FE4 + residual + (p * alphap)
}
ihgm <- function(deltaj, sigma1, winetype, market) {
  tibble(market = market, winetype = winetype, deltaj = deltaj) %>%
    group_by(market, winetype) %>%
    summarise(sum = sum(exp(deltaj / (1 - sigma1))), .groups = "drop") %>%
    mutate(ihgm = log(sum) * (1 - sigma1)) %>%
    select(market, winetype, ihgm)
}
igm <- function(ihgm, sigma2, market) {
  tibble(market = market, ihgm = ihgm) %>%
    group_by(market) %>%
    summarise(sum = sum(exp(ihgm / (1 - sigma2))), .groups = "drop") %>%
    mutate(igm = log(sum) * (1 - sigma2)) %>%
    select(market, igm)
}
im <- function(igm, market) {
  tibble(market = market, igm = igm) %>%
    mutate(im = log(1 + exp(igm))) %>%
    select(market, im)
}
sharei <- function(deltaj, sigma1, sigma2, ihgm, igm, im, market) {
  (exp(deltaj / (1 - sigma1)) * exp(ihgm / (1 - sigma2)) * exp(igm)) /
    (exp(ihgm / (1 - sigma1)) * exp(igm / (1 - sigma2)) * exp(im))
}
