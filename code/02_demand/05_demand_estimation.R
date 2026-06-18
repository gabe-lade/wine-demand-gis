# =============================================================================
# Step 5 (R port): demand estimation, WTP (Table 3), and elasticities (Table 2)
# Ports DoFile/5_demand_estimation.do. Validated against the Stata baseline.
# =============================================================================
suppressMessages({library(haven); library(data.table); library(fixest)})
options(width = 120)

REPO <- "/Users/lade.10/Library/CloudStorage/Dropbox/Work/RESEARCH/wine-demand-repo"
d <- as.data.table(readRDS(file.path(REPO,"data/derived/r_4_demand_estimation_data.rds")))
nm <- names(d)

# drop 11 product singletons -> N = 794,974 (as ivreghdfe/reghdfe do)
d[, .pc := .N, by = product]; d <- d[.pc > 1]; d[, .pc := NULL]
cat("N =", nrow(d), "\n")

endog <- c("price","lsih","lshg")
instr <- c(paste0("Z",c(1:7,9,10)),"distribution_cost","dis_sum",
           "retail_density_area","retail_density_popu","state_control","excisetax")  # Z8 collinear
fe    <- c("quarter","year","scm_code","product")

# ---- helper: two-step efficient GMM with FE absorption (= ivreghdfe gmm2s robust)
gmm2s_fe <- function(dep, endo, inst, data, fes) {
  M  <- as.matrix(data[, c(dep, endo, inst), with = FALSE])
  Md <- fixest::demean(M, data[, ..fes])
  y <- Md[, dep]; X <- Md[, endo, drop = FALSE]; Z <- Md[, inst, drop = FALSE]
  ZX <- crossprod(Z, X); Zy <- crossprod(Z, y); ZZ <- crossprod(Z)
  b1 <- solve(t(ZX) %*% solve(ZZ) %*% ZX, t(ZX) %*% solve(ZZ) %*% Zy)
  e1 <- as.numeric(y - X %*% b1)
  W  <- solve(crossprod(Z * e1))
  b2 <- solve(t(ZX) %*% W %*% ZX, t(ZX) %*% W %*% Zy)
  V  <- solve(t(ZX) %*% W %*% ZX)
  list(b = setNames(as.numeric(b2), endo), se = setNames(sqrt(diag(V)), endo))
}

# ---- Model 1: nested logit IV (3 endogenous) ; Model 2: logit IV (price only)
m1 <- gmm2s_fe("logshare_diff", endog,      instr, d, fe)
m2 <- gmm2s_fe("logshare_diff", "price",    instr, d, fe)
# ---- Model 3: logit OLS
m3 <- feols(logshare_diff ~ price | quarter + year + scm_code + product, d, vcov = "hetero")

bp1 <- m1$b["price"]; s1 <- m1$b["lsih"]; s2 <- m1$b["lshg"]
bp2 <- m2$b["price"]; bp3 <- coef(m3)["price"]

cat("\n===== TABLE 2: demand parameters (R) =====\n")
t2 <- data.frame(param=c("price","sigma1","sigma2"),
                 nested=c(bp1,s1,s2), nested_se=m1$se,
                 stata=c(-0.1646893,0.647714,0.4688499))
print(t2, row.names=FALSE, digits=6)
cat(sprintf("logit-IV price = %.5f (stata -0.20514) ; logit-OLS price = %.5f (stata -0.01206)\n",
            bp2, bp3))

# =============================================================================
# Second-step: product fixed effects on attributes -> marginal utility & WTP
# =============================================================================
# product FE consistent with GMM betas: absorb (y - X*beta) onto the 4 FE
d[, .u := logshare_diff - price*bp1 - lsih*s1 - lshg*s2]
fe_mod <- feols(.u ~ 1 | quarter + year + scm_code + product, d)
fe4 <- fixef(fe_mod)$product
prod <- d[, .SD[1], by = product]                 # one row per product
prod[, FE4 := fe4[as.character(product)]]

# attribute varlists = dataset-order ranges (mirror Stata varlist a-b syntax)
rng <- function(a, b) nm[which(nm==a):which(nm==b)]
red  <- rng("Cabernet_Sauvignon","Other_Red_Imported")
whit <- rng("Chardonnay","Other_White_Imported")
spec <- rng("Dessert","Other_Specialty_Imported")
avas <- rng("Alexander_Valley","Other_AVAs")
stat <- rng("Florida","Other_States")
ctry <- rng("Argentina","Other_Countries")
attrs <- c(red, whit, spec, avas, stat, ctry)

prod[, size_f := factor(size)]; prod[, winetype_f := factor(winetype)]
rhs <- paste(c("size_f","winetype_f", attrs), collapse = " + ")
f_mu <- as.formula(paste("FE4 ~", rhs, "| brand_name"))
mu_mod <- feols(f_mu, prod, vcov = "hetero")

# WTP = MU / (-alpha): rescale the second-step regression
prod[, WTP := -FE4 / bp1]
wtp_mod <- feols(as.formula(paste("WTP ~", rhs, "| brand_name")), prod, vcov = "hetero")

cat(sprintf("\n===== TABLE 3 (R): N products = %d, R2 = %.3f =====\n", nrow(prod), r2(mu_mod,"r2")))
show <- c("winetype_f2","winetype_f3","size_f2",
          "Chardonnay","Anderson_Valley","Carneros","Napa_Valley","Sonoma_Valley","France")
cm <- coeftable(wtp_mod)
avail <- intersect(show, rownames(cm))
t3 <- data.frame(attr=avail, WTP_R=cm[avail,"Estimate"])
t3$stata <- c(winetype_f2=NA, winetype_f3=NA, size_f2=-2.41, Chardonnay=0.83,
              Anderson_Valley=9.83, Carneros=9.27, Napa_Valley=6.18,
              Sonoma_Valley=12.0, France=3.49)[avail]
print(t3, row.names=FALSE, digits=4)

# =============================================================================
# Elasticities (Table 2)
# =============================================================================
d[, nested_iv := bp1*((1/(1-s1)) - ((1/(1-s1))-(1/(1-s2)))*share_ihgm -
                      (s2/(1-s2))*share_igm - share_im)*price]
d[, logit_iv  := bp2*(1-share_im)*price]
d[, logit_ols := bp3*(1-share_im)*price]
d[, og_nested := -bp1*share_im*price]
own <- d[, lapply(.SD, mean), by=product, .SDcols=c("nested_iv","logit_iv","logit_ols")][
          , lapply(.SD, mean), .SDcols=c("nested_iv","logit_iv","logit_ols")]
d[, conditional_share := (share_im/share_in)*price]
d[, weighted_price := sum(conditional_share), by=market]
d[, sos := sum(og_nested), by=market]
agg_n <- unique(d[, .(market, a=-(share_out/(1-share_out))*sos)])[, mean(a)]
agg_liv <- unique(d[, .(market, a=bp2*share_out*weighted_price)])[, mean(a)]
agg_lols<- unique(d[, .(market, a=bp3*share_out*weighted_price)])[, mean(a)]

cat("\n===== Elasticities (R vs Stata) =====\n")
el <- data.frame(
  metric = c("own nested","own logit-IV","own logit-OLS","agg nested","agg logit-IV","agg logit-OLS"),
  R      = c(own$nested_iv, own$logit_iv, own$logit_ols, agg_n, agg_liv, agg_lols),
  stata  = c(-4.755, -2.089, -0.1271, -0.5283, -0.6581, -0.04004))
el$diff <- abs(el$R - el$stata)
print(el, row.names=FALSE, digits=4)

# ---- write exhibits to output/tables/ ----
OUT <- file.path(REPO, "output/tables"); dir.create(OUT, recursive=TRUE, showWarnings=FALSE)
data.table::fwrite(t2, file.path(OUT, "table_2_demand_params.csv"))
data.table::fwrite(el, file.path(OUT, "table_2_elasticities.csv"))
wtp_full <- data.table::as.data.table(coeftable(wtp_mod), keep.rownames="attribute")
data.table::fwrite(wtp_full, file.path(OUT, "table_3_wtp.csv"))
cat("\nWrote output/tables/{table_2_demand_params, table_2_elasticities, table_3_wtp}.csv\nDONE\n")
