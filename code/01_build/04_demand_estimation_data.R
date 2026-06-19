# =============================================================================
# 04_demand_estimation_data.R
# Port of DoFile/4_data_for_demand_estimation.do
# Prepares data at product-market level for demand estimation.
# =============================================================================

ROOT <- local({a<-commandArgs(FALSE);f<-grep("^--file=",a,value=TRUE);p<-if(length(f))dirname(normalizePath(sub("^--file=","",f[1])))else normalizePath(getwd());while(!file.exists(file.path(p,"code","_config.R"))&&dirname(p)!=p)p<-dirname(p);p})
source(file.path(ROOT, "code/_config.R"))
# _config.R derives ROOT from cwd; re-pin to the absolute project root.
ROOT <- local({a<-commandArgs(FALSE);f<-grep("^--file=",a,value=TRUE);p<-if(length(f))dirname(normalizePath(sub("^--file=","",f[1])))else normalizePath(getwd());while(!file.exists(file.path(p,"code","_config.R"))&&dirname(p)!=p)p<-dirname(p);p})

# --- .do line 10: use 3_product_data.dta ------------------------------------
# NOTE: haven/ReadStat (read_dta_dt) fails on this specific 2GB Stata-118 file
# with "Unable to allocate memory" even for a single column. readstata13 reads
# it correctly. convert.factors=FALSE keeps value-labelled vars (e.g. scm_code)
# as their underlying numeric codes, matching the Stata logic.
suppressMessages(library(readstata13))
d <- read_dt(file.path(ROOT, "data/derived/r_3_product_data.dta"))

# --- .do line 12-13: gen date_quarter = qofd(date) --------------------------
# Stata qofd(): number of quarters since 1960q1 = (year-1960)*4 + (quarter-1).
# haven reads %td 'date' as an R Date. Derive year/month robustly.
.dt   <- as.Date(d$date)              # Stata %td daily date -> R Date
.yr   <- as.integer(format(.dt, "%Y"))
.mo   <- as.integer(format(.dt, "%m"))
.qtr  <- (.mo - 1L) %/% 3L + 1L       # quarter of year, 1..4
d[, date_quarter := (.yr - 1960L) * 4L + (.qtr - 1L)]

# --- .do line 15: gen quarter = quarter(date) -------------------------------
d[, quarter := .qtr]

# --- .do line 17-18: sort + egen market = group(scm_code date_quarter) ------
# egen group() numbers in sorted order of the grouping vars.
setorder(d, scm_code, date_quarter)
d[, market := .GRP, by = .(scm_code, date_quarter)]

# Stata `encode` assigns integer codes by levels sorted in ASCII/C order.
# Use a radix (C-locale) sort of unique values so ordering is locale-independent
# and matches Stata exactly.
encode_int <- function(x) {
  lv <- sort(unique(x[!is.na(x)]), method = "radix")
  as.integer(factor(x, levels = lv))
}

# --- .do line 22-23: encode size_category -> size ; drop size_category ------
d[, size := encode_int(size_category)]
d[, size_category := NULL]

# --- .do line 26-27: encode wine_type -> winetype ; drop wine_type ----------
d[, winetype := encode_int(wine_type)]
d[, wine_type := NULL]

# --- .do line 30: encode Varietal -> Var ------------------------------------
d[, Var := encode_int(Varietal)]

# --- .do line 33: encode geographic_label -> geographiclabel ----------------
d[, geographiclabel := encode_int(geographic_label)]

# --- .do line 38: encode brand_descr -> brand_name --------------------------
d[, brand_name := encode_int(brand_descr)]

# --- .do line 42: egen product = group(size winetype Var geographiclabel brand_name)
# egen group() numbers in sorted order of the grouping vars.
setorder(d, size, winetype, Var, geographiclabel, brand_name)
d[, product := .GRP, by = .(size, winetype, Var, geographiclabel, brand_name)]

# --- .do line 45-48: drop singleton products (pc_count==1), drop product ----
d[, pc_count := .N, by = product]
d <- d[pc_count != 1]
d[, pc_count := NULL]
d[, product := NULL]

# --- .do line 50: re-create product group -----------------------------------
setorder(d, size, winetype, Var, geographiclabel, brand_name)
d[, product := .GRP, by = .(size, winetype, Var, geographiclabel, brand_name)]

# --- .do line 52-54: weighted quantities/prices -----------------------------
d[, quantity_bottle_w := quantity_bottle * projection_factor]
d[, final_price_paid_w := final_price_paid * projection_factor]
d[, total_price_paid_w := total_price_paid * projection_factor]

# --- .do line 58-59: revenue / revenue_dis by product market ----------------
d[, revenue     := sum(total_price_paid_w, na.rm = TRUE), by = .(product, market)]
d[, revenue_dis := sum(final_price_paid_w, na.rm = TRUE), by = .(product, market)]

# --- .do line 61: drop if revenue_dis == 0 ----------------------------------
d <- d[revenue_dis != 0]

# --- .do line 62: sales by product market -----------------------------------
d[, sales := sum(quantity_bottle_w, na.rm = TRUE), by = .(product, market)]

# --- .do line 63-64: keep one row per product-market (id = _n ; drop id>1) ---
# bysort product market: gen id = _n  -> sequence within group; keep first.
# 'bysort product market' sorts by product then market within the by-groups;
# _n ordering within a group is row order after that sort. Collapse to first.
setorder(d, product, market)
d[, id := seq_len(.N), by = .(product, market)]
d <- d[id <= 1]
d[, id := NULL]

# --- .do line 67-68: price ---------------------------------------------------
d[, product_price := revenue_dis / sales]
d[, price := product_price / Deflator]

# --- .do line 72-75: market size --------------------------------------------
d[, totalsales := sum(sales, na.rm = TRUE), by = market]
d[, per_capita_cons := totalsales / pop_above_20]
d[, max_percapitacons := max(per_capita_cons, na.rm = TRUE), by = scm_code]
d[, market_size := 1.5 * max_percapitacons * pop_above_20]

# --- .do line 79-84: shares --------------------------------------------------
d[, share_im       := sales / market_size]
d[, logshare       := log(share_im)]
d[, share_in       := sum(share_im, na.rm = TRUE), by = market]
d[, share_out      := 1 - share_in]
d[, logout         := log(share_out)]
d[, logshare_diff  := logshare - logout]

# --- .do line 87-89: subgroup share -----------------------------------------
d[, subgroup_sales := sum(sales, na.rm = TRUE), by = .(market, winetype)]
d[, share_ihgm     := sales / subgroup_sales]
d[, lsih           := log(share_ihgm)]

# --- .do line 92-96: group share --------------------------------------------
d[, group_sales := sum(sales, na.rm = TRUE), by = market]
d[, sharehg     := subgroup_sales / group_sales]
d[, lshg        := log(sharehg)]
d[, share_igm   := sales / group_sales]

# --- .do line 100-119: instruments = count of products minus one ------------
# egen count(product) counts non-missing product values in the by-group; product
# is never missing here, so .N equals the count. Subtract 1 (own product).
d[, Z1  := .N, by = .(market)];                          d[, Z1  := Z1  - 1]
d[, Z2  := .N, by = .(market, winetype)];                d[, Z2  := Z2  - 1]
d[, Z3  := .N, by = .(market, winetype, size)];          d[, Z3  := Z3  - 1]
d[, Z4  := .N, by = .(market, winetype, Var)];           d[, Z4  := Z4  - 1]
d[, Z5  := .N, by = .(market, winetype, geographiclabel)]; d[, Z5 := Z5  - 1]
d[, Z6  := .N, by = .(market, winetype, brand_name)];    d[, Z6  := Z6  - 1]
d[, Z7  := .N, by = .(market, size)];                    d[, Z7  := Z7  - 1]
d[, Z8  := .N, by = .(market, Var)];                     d[, Z8  := Z8  - 1]
d[, Z9  := .N, by = .(market, geographiclabel)];         d[, Z9  := Z9  - 1]
d[, Z10 := .N, by = .(market, brand_name)];              d[, Z10 := Z10 - 1]

# --- .do line 123-126: distance instrument (merge m:1 date_quarter) ---------
diesel <- read_dta_dt(file.path(ROOT, "data/public/distance/diesel_price.dta"))
d <- merge(d, diesel, by = "date_quarter", all.x = TRUE)   # drop _merge
d[, gallons_req       := distance_miles / 25]
d[, distribution_cost := gallons_req * diesel_price]

# --- .do line 128-129: dis_sum = total distance_miles, minus own -------------
d[, dis_sum := sum(distance_miles, na.rm = TRUE), by = market]
d[, dis_sum := dis_sum - distance_miles]

# --- .do line 133-142: store count data -------------------------------------
# merge m:1 scantrack_market_descr ... drop if _merge==2 (using-only obs).
fsc2017 <- read_dta_dt(file.path(ROOT, "data/public/store_count/foodstorecount_2017.dta"))
d <- merge(d, fsc2017, by = "scantrack_market_descr", all.x = TRUE)  # drop _merge==2 == all.x

fsc2012 <- read_dta_dt(file.path(ROOT, "data/public/store_count/foodstorecount_2012.dta"))
d <- merge(d, fsc2012, by = "scantrack_market_descr", all.x = TRUE)

d[, store_count := NA_real_]
d[year > 2012,  store_count := foodandbeveragestorescount_2017]
d[year < 2012,  store_count := foodandbeveragestorescount_2012]
d[year == 2012, store_count := foodandbeveragestorescount_2012]

# --- .do line 143-147: MSA area merge + retail densities --------------------
msa <- read_dta_dt(file.path(ROOT, "data/public/area/MSA_Area_2010Census.dta"))
d <- merge(d, msa, by = "scantrack_market_descr", all.x = TRUE)

d[, retail_density_area := areainsquaremilestotalarea / store_count]
d[, retail_density_popu := (pop_above_20 / store_count) * 1000]

# --- .do line 150-153: state control ----------------------------------------
d[, state_control := NA_real_]
d[fips_state_descr == "UT" | fips_state_descr == "WY" | fips_state_descr == "PA",
  state_control := 1]
d[is.na(state_control), state_control := 0]
d[, one_minus_statecontrol := 1 - state_control]
d[, excisetax := one_minus_statecontrol * excisetx_dollarpergallon]

# --- .do line 156: keep ------------------------------------------------------
# Cabernet_Sauvignon-California is a Stata varlist RANGE (vars from
# Cabernet_Sauvignon through California in dataset order). Reconstruct from the
# original variable order of 3_product_data.dta.
.orig_order <- c(
  "Cabernet_Sauvignon","Cabernet_Blend","Carmenere","Concord","Malbec",
  "Moscato_Red","Petite_Syrah","Pinot_Noir","Syrah","Syrah_Blend","Zinfandel",
  "Other_Red","Other_Red_Imported","Merlot","Chardonnay","Chenin_Blanc",
  "Gewurztraminer","Liebfraumilch","Moscato_White","Pinot_Grigio","Riesling",
  "Viognier","Other_White","Other_White_Imported","Sauvignon_Blanc","Dessert",
  "Flavoured","Sangria","Sparkling","Vermouth","Other_Specialty",
  "Other_Specialty_Imported","Blush_Rose","Argentina","Australia","Chile",
  "France","Germany","Italy","New_Zealand","Portugal","South_Africa","Spain",
  "Other_Countries","Alexander_Valley","Amador_County","Anderson_Valley",
  "Arroyo_Seco","Augusta","Carneros","Central_Coast","Chalk_Hill","Chalone",
  "Clarksburg","Columbia_Valley","Contra_Costa_County","Dry_Creek_Valley",
  "Dunnigan_Hills","Eagle_Peak","Edna_Valley","Eola_Hills","Finger_Lakes",
  "Guenoc","Horse_Heaven_Hills","Knights_Valley","Lake_County","Lake_Erie",
  "Livermore_Valley","Lodi","Mendocino","Mendocino_County","Monterey_County",
  "Napa_County","Napa_Valley","North_Coast","Oakville","Old_Mission_Peninsula",
  "Ohio_River_Valley","Paso_Robles","Red_Hills_Lake_County",
  "Russian_River_Valley","Rutherford","Saint_Lucia_Highlands","San_Antonio",
  "Santa_Barbara_County","Santa_Maria_Valley","Sierra_Foothills",
  "Snake_River_Valley","Sonoma_County","Sonoma_Coast","Sonoma_Mountain",
  "Sonoma_Valley","St_Helena","Texas_High_Plains","Wahluke_Slope","Walla_Walla",
  "Willamette_Valley","Yakima_Valley","Other_AVAs","Florida","Indiana",
  "Michigan","Missouri","Nebraska","New_York","North_Carolina","Ohio","Oregon",
  "Texas","Washington","Other_States","California")

keep_vars <- c(
  "product","logshare_diff","price","lshg","lsih",
  "Z1","Z2","Z3","Z4","Z5","Z6","Z7","Z8","Z9","Z10",
  "dis_sum","distribution_cost","date_quarter","quarter","year","scm_code",
  "retail_density_area","retail_density_popu","share_im","share_ihgm",
  "share_igm","scantrack_market_descr","winetype","geographic_label",
  "appellation_label","geographiclabel","origin_state","brand_name",
  "brand_descr","size","Var","market","market_size","share_in","share_out",
  "sales","revenue_dis","excisetx_dollarpergallon","excisetax","state_control",
  .orig_order, "Import", "Domestic", "product_module_descr")

keep_vars <- keep_vars[keep_vars %in% names(d)]
d <- d[, ..keep_vars]

# --- .do line 158: save ------------------------------------------------------
save_dt(d, file.path(ROOT, "data/derived/r_4_demand_estimation_data.dta"))

cat("Wrote:", file.path(ROOT, "data/derived/r_4_demand_estimation_data.dta"), "\n")
cat("nrow:", nrow(d), "  ncol:", ncol(d), "\n")
