# =============================================================================
# 06_welfare_prep.R  — port of 7_dataprep_for_welfare.do (the Stata/R parts that
# build inputs for the counterfactual). Produces:
#   - data/Welfare/1_input_data_for_mc.csv        (input to marginal-cost solve)
#   - data/Welfare/welfare_base.rds               (base dataset for steps 7b/8)
# Uses the EXACT GMM betas for FE4/rest (as Stata stored them), and the rounded
# alpha=-0.16, sigma1=0.65, sigma2=0.47 in the demand sims/welfare (as the original).
# =============================================================================
ROOT <- "/Users/lade.10/Library/CloudStorage/Dropbox/Work/RESEARCH/wine-demand-repo"
source(file.path(ROOT, "code/_config.R"))
WF <- file.path(ROOT, "Data/Welfare"); dir.create(WF, showWarnings = FALSE, recursive = TRUE)

d <- as.data.table(readRDS(file.path(ROOT, "Data/r_4_demand_estimation_data.rds")))
nm <- names(d)
d[, .pc := .N, by = product]; d <- d[.pc > 1]; d[, .pc := NULL]   # 794,974

endog <- c("price","lsih","lshg")
instr <- c(paste0("Z",c(1:7,9,10)),"distribution_cost","dis_sum",
           "retail_density_area","retail_density_popu","state_control","excisetax")
fe <- c("quarter","year","scm_code","product")

# ---- exact two-step GMM (same as estimation) ----
M  <- as.matrix(d[, c("logshare_diff", endog, instr), with = FALSE])
Md <- fixest::demean(M, d[, ..fe])
y <- Md[,"logshare_diff"]; X <- Md[,endog]; Z <- Md[,instr]
ZX<-crossprod(Z,X); Zy<-crossprod(Z,y); ZZ<-crossprod(Z)
b1<-solve(t(ZX)%*%solve(ZZ)%*%ZX, t(ZX)%*%solve(ZZ)%*%Zy)
e1<-as.numeric(y-X%*%b1); W<-solve(crossprod(Z*e1))
b2<-as.numeric(solve(t(ZX)%*%W%*%ZX, t(ZX)%*%W%*%Zy))
bp<-b2[1]; s1<-b2[2]; s2<-b2[3]
cat(sprintf("betas: price=%.5f sigma1=%.5f sigma2=%.5f\n", bp,s1,s2))

# ---- FE4 (product fixed effect) and rest = FE1+FE2+FE3+residual ----
d[, U := logshare_diff - bp*price - s1*lsih - s2*lshg]   # = FE1+FE2+FE3+FE4+resid
fe_mod <- feols(U ~ 1 | quarter + year + scm_code + product, d)
d[, FE4 := fixef(fe_mod)$product[as.character(product)]]
d[, rest := U - FE4]                                      # lump FE1+FE2+FE3+resid

# ---- second-step regression (Table 3 spec) -> geographic coefficients ----
rng <- function(a,b) nm[which(nm==a):which(nm==b)]
red<-rng("Cabernet_Sauvignon","Other_Red_Imported"); whit<-rng("Chardonnay","Other_White_Imported")
spec<-rng("Dessert","Other_Specialty_Imported"); avas<-rng("Alexander_Valley","Other_AVAs")
stat<-rng("Florida","Other_States"); ctry<-rng("Argentina","Other_Countries")
prod <- d[, .SD[1], by = product]
prod[, size_f := factor(size)]; prod[, winetype_f := factor(winetype)]
rhs <- paste(c("size_f","winetype_f", red,whit,spec,avas,stat,ctry), collapse=" + ")
mu <- feols(as.formula(paste("FE4 ~", rhs, "| brand_name")), prod, vcov="hetero")
cf <- coef(mu)

# ---- FE4_counterfactual = FE4 - sum over US-geographic (AVAs + States) coef*dummy ----
geo_cf <- intersect(c(avas, stat), names(cf))
prodm <- as.matrix(prod[, ..geo_cf]); prod[, FE4_cf := FE4 - as.numeric(prodm %*% cf[geo_cf])]
d <- merge(d, prod[, .(product, FE4_counterfactual = FE4_cf)], by="product", all.x=TRUE)

# ---- build base + MC input ----
d[, brand_name_str := brand_descr]                    # grouped brand string ("Other_brands")
saveRDS(d, file.path(WF, "welfare_base.rds"))
saveRDS(list(bp=bp, s1=s1, s2=s2, mu_coef=cf, geo_cf=geo_cf), file.path(WF, "welfare_params.rds"))

mc_in <- d[, .(product, price, market, share_im, share_ihgm, share_igm,
               winetype, brand_name = brand_name_str)]
fwrite(mc_in, file.path(WF, "1_input_data_for_mc.csv"))
cat("wrote MC input:", nrow(mc_in), "rows;", uniqueN(mc_in$market), "markets\n")
cat("DONE welfare_prep\n")
