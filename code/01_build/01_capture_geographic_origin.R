# =============================================================================
# 01_capture_geographic_origin.R
#
# R port of DoFile/1_capture_geographic_origin_ava.do (Do File no.1).
# Cleans the 2021 wine panel module and captures geographic-origin information
# (US AVAs, state appellations, origin state, foreign country, foreign region)
# from upc_descr, style_descr, brand_descr and type_descr.
#
# Faithful 1:1 translation of the Stata logic. data.table syntax throughout.
#
# NOTE: This script loads a ~4.3 GB .dta file; do not run casually.
# =============================================================================

ROOT <- local({a<-commandArgs(FALSE);f<-grep("^--file=",a,value=TRUE);p<-if(length(f))dirname(normalizePath(sub("^--file=","",f[1])))else normalizePath(getwd());while(!file.exists(file.path(p,"code","_config.R"))&&dirname(p)!=p)p<-dirname(p);p})
source(file.path(ROOT, "code/_config.R"))

d <- read_dta_dt(file.path(ROOT, "data/raw/panel_wine_module_2021.dta"))

# -----------------------------------------------------------------------------
# DATA CLEANING 1: TIME PERIOD - Restricting study period to 2007-2019
# -----------------------------------------------------------------------------
d <- d[!(panel_year < 2007)]
d <- d[!(year < 2007)]

# most info extracted from upc_descr -> drop obs with missing upc description
d <- d[!(upc_descr == "")]

# -----------------------------------------------------------------------------
# DATA CLEANING 2: unique at hh code, trip code, upc level. Duplicates from deal
# and coupon value; add qty and price paid for such entries and retain coupon
# value / deal information.
# -----------------------------------------------------------------------------
setorder(d, household_code, trip_code_uc, upc)
d[, dup := if (.N == 1) 0L else seq_len(.N), by = .(household_code, trip_code_uc, upc)]

d[dup > 0, quantity_1        := sum(quantity),        by = .(household_code, trip_code_uc, upc)]
d[dup > 0, coupon_value_1    := sum(coupon_value),    by = .(household_code, trip_code_uc, upc)]
d[dup > 0, total_price_paid_1 := sum(total_price_paid), by = .(household_code, trip_code_uc, upc)]
d[, final_price_paid_1 := total_price_paid_1 - coupon_value_1]
d[, final_price_paid   := total_price_paid   - coupon_value]
# to capture transactions made on deal so they aren't lost when dropping dups
d[dup > 0, deal_flag_uc1 := sum(deal_flag_uc), by = .(household_code, trip_code_uc, upc)]
d[is.na(deal_flag_uc1), deal_flag_uc1 := 0]
d[deal_flag_uc1 > 0, deal_flag_uc := 1]

d[dup > 0, quantity         := quantity_1]
d[dup > 0, coupon_value      := coupon_value_1]
d[dup > 0, total_price_paid  := total_price_paid_1]
d[dup > 0, final_price_paid  := final_price_paid_1]
d <- d[!(dup > 1)]
d[, c("quantity_1", "coupon_value_1", "total_price_paid_1", "final_price_paid_1",
      "dup", "deal_flag_uc1") := NULL]

d[, upc_descr := str_trim(upc_descr)]

# -----------------------------------------------------------------------------
# RESTRICTION 1: drop SAKE & Non-alcoholic modules
# -----------------------------------------------------------------------------
d <- d[!(product_module_descr == "WINE-SAKE")]
d <- d[!(product_module_descr == "WINE - NON ALCOHOLIC")]

# -----------------------------------------------------------------------------
# SCANTRACK MARKET
# RESTRICTION 2: keep only major scantrack markets (50; NY markets combined)
# -----------------------------------------------------------------------------
d <- d[!(scantrack_market_code > 50 & panel_year > 2015)]
d <- d[!(scantrack_market_code > 52 & panel_year < 2016)]

# make market description uniform across years
d[scantrack_market_descr == "Buffalo-Rochester",   scantrack_market_descr := "Buffalo - Rochester"]
d[scantrack_market_descr == "Hartford-New Haven",  scantrack_market_descr := "Hartford - New Haven"]
d[scantrack_market_descr == "New Orleans-Mobile",  scantrack_market_descr := "New Orleans - Mobile"]
d[scantrack_market_descr == "Oklahoma City-Tulsa", scantrack_market_descr := "Oklahoma City - Tulsa"]
d[scantrack_market_descr == "Raleigh-Durham",      scantrack_market_descr := "Raleigh - Durham"]
d[scantrack_market_descr == "Salt Lake City-Boise", scantrack_market_descr := "Salt Lake City"]
d[scantrack_market_descr == "St Louis",            scantrack_market_descr := "St. Louis"]
d[scantrack_market_descr == "Richmond",            scantrack_market_descr := "Richmond-Norfolk"]

# RESTRICTION 2.1: combine urban / suburban / exurban New York into one market
d[scantrack_market_descr == "Urban NY" | scantrack_market_descr == "Suburban NY" |
  scantrack_market_descr == "Exurban NY", scantrack_market_descr := "New York"]

# our own uniform scantrack-market code (Nielsen codes differ 2005-15 vs 2016+)
d[, scm_code := as.integer(factor(scantrack_market_descr))]

# RESTRICTION 3: drop observations without price info (Nielsen definition)
d <- d[!(total_price_paid == 0 & deal_flag_uc == 0 & coupon_value == 0)]

# RESTRICTION 5: drop obs without size amount
d <- d[!(is.na(size1_amount))]

# =============================================================================
# Wine appellation are the US defined AVA
# =============================================================================
d[, wine_appellation := ""]

# Alexander Valley
d[, Alexander_Valley := as.integer(style_descr=="A-V"| style_descr=="A-V RS"| style_descr=="A-V SC"| style_descr=="A-V SC RS"| style_descr=="A-V SONOMA"| style_descr=="A-V SONOMA RS"| style_descr=="A-V SPECIAL RS"| style_descr=="A-V W-R"| style_descr=="A-V CA"| style_descr=="A-V BARRELLI CREEK"| style_descr=="A-V CA SC RS"| style_descr=="A-V CA SONOMA RS"| style_descr=="A-V HERITAGE RS"| style_descr=="A-V NORTHERN SONOMA RS"| style_descr=="A-V PVT RS"| style_descr=="A-V RS ALEXANDRE"| style_descr=="A-V S-V"| brand_descr=="ALEXANDER VALLEY VINEYARDS"| style_descr=="A-V CA SC-Alexander"| style_descr=="A-V NAPA COUNTY PVT RS"| style_descr=="A-V CA SC"| style_descr=="A-V NAPA COUNTY PVT RS"| style_descr=="A-V RS ALEXANDRE")]

# Anderson Valley
d[, Anderson_Valley := as.integer(style_descr=="ANDERSON VALLEY MC"| style_descr=="ANDERSON VALLEY MENDOCINO"| style_descr=="ANDERSON VALLEY RS"| style_descr=="ANDERSON VALLEY")]
d[, Anderson := as.integer(rgx(paste0(" ", upc_descr, " "), " (AD-V) ")==1 | rgx(paste0(" ", upc_descr, " "), " (AVM) ")==1)]
d[Anderson==1, Anderson_Valley := 1]
d[, Anderson := NULL]

# Columbia Valley sub-AVA: Ancient Lake of Columbia Valley
d[, ALOCV := as.integer(style_descr=="ALOCV-WS"| style_descr=="ALOCV WASHINGTON STATE"| style_descr=="ANCIENT LAKE OF COLUMBIA VALLE")]

# Horse Heaven Hills
d[, Horse_Heaven_Hills := as.integer(style_descr=="HORSE HEAVEN HILLS RS"| style_descr=="HORSE HEAVENS HILLS"| style_descr=="C-V HORSE HEAVEN HILLS"| style_descr=="HORSE HEAVEN HILLS"| style_descr=="HHHWWW"| style_descr=="HHH-WS"| style_descr=="HHHW"| style_descr=="HHHWSZR"| style_descr=="HHHYV"| upc_descr=="MCR CNY CB-S HHH YV V RED DDT"| upc_descr=="MCR CNY CHRD HHH YV V WT DDT")]

d[, Rattle_Snake_Hills := as.integer(style_descr=="RTLSNK HILLS"| style_descr=="RTLSNK HILLS WASHINGTON"| style_descr=="RTLSNK HILLS WASHINGTON STATE"| style_descr=="RTLSN KHILLS WASHINGTON STATE"| style_descr=="RTLSNK HILLS YAKIMA VALLEY"| style_descr=="OREGON RTLSNK HILLS")]

d[, Red_Mountain := as.integer(style_descr=="RED MOUNTAIN"| style_descr=="RED MOUNTAIN WASHINGTON STATE"| style_descr=="RD MNTN AV WSHNGTN ST")]

# Walla Walla Washington
d[, Walla_Walla := as.integer(style_descr=="WALLA WALLA VALLEY WASHINGTON"| style_descr=="C-V WALLA WALLA WASHINGTON"| style_descr=="WALLA WALLA WASHINGTON"| style_descr=="WALLA WALLA VALLEY"| style_descr=="WALLA WALLA"| style_descr=="WALLA WALLA WASHINGTON STATE"| style_descr=="CVWWW"| style_descr=="CVWWWS"| style_descr=="CVWWWS RS")]

# Yakima Valley
d[, Yakima_Valley := as.integer(style_descr=="WASHINGTON STATE YAKIMA VALLEY"| style_descr=="CHAPEL BLOCK YAKIMA VALLEY"| style_descr=="C-V YAKIMA VALLEY"| style_descr=="YAKIMA VALLEY RS"| style_descr=="YAKIMA VALLEY"| brand_descr=="YAKIMA RIVER"| style_descr=="WASHINGTON YAKIMA VALLEY"| style_descr=="WSHNGTN ST YKM VLY"| style_descr=="ART DEN HOED YAKIMA VALLEY"| style_descr=="WS YAKIMA VALLEY")]

# Wahluke Slope
d[, Wahluke_Slope := as.integer(style_descr=="C-V WAHLUKE SLOPE"| style_descr=="WAHLUKE SLOPE WASHINGTON"| style_descr=="C-V WAHLUKE SLOPE WASHINGTON"| style_descr=="WAHLUKE SLOPE"| style_descr=="C-V WSL"| style_descr=="C-V WSL WASHINGTON"| style_descr=="C-V WSL WASHINGTON STATE"| style_descr=="WAHLUKE SLOPE"| style_descr=="WSL"| style_descr=="WSL WASHINGTON"| style_descr=="WSL WASHINGTON STATE"| style_descr=="C-V WAHLUKE SLOPE"| style_descr=="WSL YAKIMA VALLEY"| style_descr=="MILBRANDT WSL")]

# Columbia Valley
d[, Columbia_Valley := as.integer(style_descr=="C-V"| style_descr=="C-V GOOSE MOUNTAIN RS"| style_descr=="C-V OREGON"| style_descr=="C-V RS"| style_descr=="C-V RS SELECTION"| style_descr=="C-V WASHINGTON"| style_descr=="C-V WASHINGTON RS"| style_descr=="C-V WASHINGTON STATE"| style_descr=="C-V WHITE BLUFFS"| style_descr=="C-V WSH"| style_descr=="Columbia Valley"| style_descr=="COLUMBIA VALLEY"| style_descr=="WASHINGTON C-V"| style_descr=="C-V")]

# Central Coast sub-AVA: Hames Valley
d[, Hames_Valley := as.integer(style_descr=="HAMES VALLEY MTRY"| style_descr=="CA HAMES VALLEY MTRY")]

# San Bernabe
d[, San_Bernabe := as.integer(style_descr=="MTRY SAN BERNABE"| style_descr=="MTRY COUNTY SAN BERNABE")]

# Arroyo Grande
d[, Arroyo_Grande := as.integer(style_descr=="ARROYO GRANDE VALLEY RS"| style_descr=="ARROYO GRANDE VALLEY")]

# Arroyo Seco
d[, Arroyo_Seco := as.integer(style_descr=="ARROYO SECO"| style_descr=="ARROYO SECO MTRY COUNTY"| style_descr=="ARROYO SECO MTRY"| style_descr=="ARROYO SECO MONTEREY"| style_descr=="ARROYO SECO CA"| style_descr=="ARROYO SECO CA MTRY"| style_descr=="ARROYO SECO CANYON"| style_descr=="ARROYO SECO CENTRAL COAST"| style_descr=="ARROYO SECO MTRY"| style_descr=="ARROYO SECO MTRY COUNTY"| style_descr=="ARROYO SECO MTRY RCR"| style_descr=="ARROYO SECO MTRY RIVA RANCH RS"| style_descr=="ARROYO SECO PVT RS"| style_descr=="ARROYO SECO RS")]

d[, Chalone := as.integer(style_descr=="CHALONE APPLELLATION"| style_descr=="CHALONE")]

# Carmel
d[, Carmel_Valley := as.integer(style_descr=="CA CARMEL VALLEY"| style_descr=="CARMEL VALLEY"| style_descr=="CARMEL VALLEY MTRY")]

d[, Cienega_Valley := as.integer(style_descr=="CIENEGA VALLEY")]

# Edna
d[, Edna_Valley := as.integer(style_descr=="CA EV"| style_descr=="EV"| style_descr=="EV RS"| style_descr=="EV RS SELECTION"| style_descr=="EV SLOC"| style_descr=="SLOC"| brand_descr=="EDNA VALLEY VINEYARD")]

# Livermore
d[, Livermore_Valley := as.integer(style_descr=="CA LIVERMORE VALLEY"| style_descr=="CA LIVERMORE VALLEY SFB"| style_descr=="LIVERMORE VALLEY RS"| style_descr=="LIVERMORE VALLEY SFB"| style_descr=="LIVERMORE VALLEY SFB CRR"| style_descr=="LIVERMORE VALLEY SFB CWR"| style_descr=="LIVERMORE VALLEY")]

# San Antonio
d[, San_Antonio := as.integer(style_descr=="SAN ANTONIO DE VALERO"| style_descr=="SAN ANTONIO VALLEY"| brand_descr=="SAN ANTONIO")]

# Santa Clara
d[, Santa_Clara := as.integer(style_descr=="SANTA CLARA VALLEY"| style_descr=="SANTA CLARA VALLEY PVT RS"| style_descr=="CCCSCV")]

# Saint Lucia
d[, Saint_Lucia_Highlands := as.integer(style_descr=="CA SLH"| style_descr=="SLH"| style_descr=="SLH PVT RS"| style_descr=="SLH RS"| style_descr=="SLH RS SELECTION"| style_descr=="SAINT LUCIA HIGHLANDS"| style_descr=="SANTA LUCIA HIGHLANDS"| style_descr=="CENTRAL COAST SLH"| style_descr=="MTRY COUNTY SLH"| style_descr=="MTRY SLH")]

# Santa Maria
d[, Santa_Maria_Valley := as.integer(style_descr=="SMV"| brand_descr=="SANTA MARIA VINEYARD & WINERY"| brand_descr=="SANTA MARIA VINYRD & WNRY VLA!"| brand_descr=="VILLA SANTA MARIA WINERY"| style_descr=="SANTA BARBARA COUNTY SMV"| style_descr=="CA SANTA BARBARA SMV RS")]

# Santa Ynez
d[, Santa_Ynes_Valley := as.integer(style_descr=="RS SANTA YNEZ VALLEY"| style_descr=="SANTA YNEZ VALLEY"| style_descr=="SBC SANTA YNEZ VALLEY")]

d[, Sta_Rita_Hills := as.integer(style_descr=="SANTA RITA HILLS"| style_descr=="SBC SANTA RITA HILLS"| style_descr=="STA RITA HILLS"| style_descr=="STA RITA HILLS CA"| style_descr=="SBC STA RITA HILLS"| style_descr=="CA SANTA RITA HILLS")]

# Monterey
d[, Monterey_County := as.integer(style_descr=="CA MTRY COUNTY"| style_descr=="CA MC"| style_descr=="CA MTRY HIGHLANDS"| style_descr=="CA MTRY"| style_descr=="CA MNDCN MTRY CNTS SNM THE CR"| style_descr=="LC MTRY COUNTY SBC"| style_descr=="MTRY COUNTY PVT RS"| style_descr=="MTRY COUNTY RS"| style_descr=="MTRY COUNTY RS SELECTION"| style_descr=="MTRY COUNTY SBC"| style_descr=="MTRY COUNTY SBC GRAND RS"| style_descr=="MTRY COUNTY SBC SC"| style_descr=="MTRY COUNTY SC"| style_descr=="MTRY COUNTY W-R"| style_descr=="MTRY PINNACLES RANCHES"| style_descr=="MTRY RS"| style_descr=="MTRY SANTA BARBARA SC"| style_descr=="MTRY VERY SPECIAL RS"| style_descr=="MC MTRY COUNTRY"| style_descr=="MC MTRY COUNTY"| style_descr=="MONTEREY COUNTY"| style_descr=="MTRY COUNTY"| style_descr=="MTRY"| style_descr=="MTRY COUNTY NAPA COUNTY SC"| style_descr=="MONTEREY"| style_descr=="MTRY CO"| style_descr=="MTRY COUNTY GRAND RS"| style_descr=="MTRY COUNTY HARVEST RS"| style_descr=="MTRY COUNTY JAMES GANG RS"| style_descr=="MTRY COUNTY NAPA COUNTY SBC"| upc_descr=="TRUFFLE MRLT MTRY V RED DDT"| style_descr=="MTRYCOUNTY"| style_descr=="MTRY COUNTY"| style_descr=="MTRY COUNTY RS"| style_descr=="MTRY PINNACLES RANCHES"| style_descr=="MTRY COUNTY SC"| style_descr=="GLACIER RIDGE MTRY COUNTY"| style_descr=="COASTAL CA MTRY"| style_descr=="MC MTRY COUNTY SC"| style_descr=="MTRY CNTY SBC SC"| style_descr=="MTRY COUNTY SLOC SC"| style_descr=="MTRY SANTA BARBARA SONOMA"| style_descr=="MONTEREY COUNTY NAPA COUNTY"| brand_descr=="MONTEREY VINEYARD"| upc_descr=="AMC THRD P-N CA M-C V RED DDT"| style_descr=="CA MC SBC SC")]

d[, Paicines := as.integer(style_descr=="PAICINES"| style_descr=="CENTRAL COAST PAICINES")]

# Paso Robles
d[, Paso_Robles := as.integer(style_descr=="CA PASO ROBLES"| style_descr=="HUERHUERO PASO ROBLES"| style_descr=="KIARA RS PASO ROBLES"| style_descr=="PASO ROBLES ESTATE RS"| style_descr=="PASO ROBLES JAMES GANG RS"| style_descr=="PASO ROBLES PVT RS"| style_descr=="PASO ROBLES RS"| style_descr=="PASO ROBLES SANTA ROSA"| style_descr=="PASO ROBLES SLOC"| style_descr=="PASO ROBLES TIERRA ROJA"| style_descr=="PASO ROBLES WEST COAST"| style_descr=="PASO ROBLES"| style_descr=="PASO CREEK"| style_descr=="A-C CA PSR SC"| style_descr=="PASO ROBLES PRINTERS ALLEY"| style_descr=="MOSSFIRE RANCH PASO ROBLES"| style_descr=="CA CENTRAL COAST PASO ROBLES"| brand_descr=="EL COTES DU PASO ROBLES")]
d[, pr := rgx(paste0(" ", upc_descr, " "), " (PS-R) ")]
d[pr==1, Paso_Robles := 1]
d[, pr := NULL]

# Central Coast
d[, Central_Coast := as.integer(style_descr=="CA CENTRAL COAST"| style_descr=="CA CENTRAL COAST CA RS"| style_descr=="Central Coast"| style_descr=="CENTRAL COAST HARVEST RS"| style_descr=="CENTRAL COAST"| style_descr=="CA CC"| style_descr=="CENTRAL COAST MTRY COUNTY"| style_descr=="CENTRAL COAST NAPA COUNTY"| style_descr=="CENTRAL COAST PVT RS"| style_descr=="CENTRAL COAST RS"| style_descr=="CENTRAL COAST S-S-R"| style_descr=="CENTRAL COAST SBC"| style_descr=="CENTRAL COAST SACRAMENTO DELTA"| style_descr=="CC"| style_descr=="CCC"| style_descr=="CCC RS SELECTION")]
d[, cc := rgx(paste0(" ", upc_descr, " "), " (CC) ")]
d[brand_descr=="TAYLOR CALIFORNIA CELLARS"| brand_descr=="COCO BAY"| brand_descr=="BISHOP CIDER CO.", cc := 0]
d[cc==1, Central_Coast := 1]
d[, cc := NULL]

# Chalk Hill
d[, Chalk_Hill := as.integer(style_descr=="CHALK HILL"| style_descr=="CHALK HILL RRV"| style_descr=="CHALK HILL SC"| style_descr=="CHALK HILL SONOMA COUNTY RS")]

d[, Chehalem_Mountains := as.integer(style_descr=="CHEHALEM MOUNTAINS")]

# Dry Creek Valley
d[, Dry_Creek_Valley := as.integer(style_descr=="CA DCV"| style_descr=="DCV"| style_descr=="DCV RS"| style_descr=="CA DCV SC"| style_descr=="CA DCV RS SELECTION"| style_descr=="CA DCV RS"| style_descr=="Dry Creek Valley"| style_descr=="DCV SC"| style_descr=="DCV TELDESCHI"| style_descr=="Dry Creek Valley"| style_descr=="DRY GREEK VALLEY SC"| brand_descr=="DRY CREEK VINEYARD")]

# Eagle Peak
d[, Eagle_Peak := as.integer(style_descr=="CA EAGLE PEAK")]

# Clarksburg
d[, Clarksburg := as.integer(style_descr=="CA CLARKSBURG"| style_descr=="CLARKSBURG"| style_descr=="CLARKSBURG SC"| style_descr=="CLARKSBURG LATE HARVEST")]

d[, Knights_Valley := as.integer(style_descr=="CA KNIGHTS VALLEY"| style_descr=="KNIGHTS VALLEY RS"| style_descr=="KNIGHTS VALLEY SC"| style_descr=="KNIGHTS VALLEY")]

# Lodi
d[, Lodi := as.integer(style_descr=="CA LODI"| style_descr=="CA LODI REGION"| style_descr=="LODI style_descr"| style_descr=="LODI RHONE"| style_descr=="LODI RS SELECTION"| style_descr=="LODI WV"| style_descr=="LODI SANTA ROSA"| style_descr=="LODI RS"| style_descr=="LODI CLUB RS"| style_descr=="LODI REGION"| style_descr=="LODI"| style_descr=="LODI NAPA COUNTY"| style_descr=="LC LODI COUNTY NAPA COUNTY"| style_descr=="LODI NAPA"| style_descr=="LODI CA"| style_descr=="ACAMPO LODI"| style_descr=="ACAMPO LODI RS"| style_descr=="CA LDI"| style_descr=="BRAMBLEWOOD LODI"| style_descr=="CA LODI REGION"| style_descr=="LODI SC"| style_descr=="AMADOR CA LODI SONOMA"| upc_descr=="HEAVYWEIGHT CHRD LDI V WT DDT"| style_descr=="CA LODI NAPA")]
d[upc_descr=="ITO RED LDI RHE G RED DDT"| upc_descr=="CTL BR CB-S CA LDI V RED DDT"| upc_descr=="CTL BR RB CA LDI G RED DDT", Lodi := 1]

# Mendocino
d[, Mendocino := as.integer(style_descr=="CA MENDOCINO"| style_descr=="MENDOCINO CA"| style_descr=="MENDOCINO UPLANDS"| style_descr=="MENDOCINO UPPER RUSSIAN RIVER"| style_descr=="MC GRAND RS"| style_descr=="MC UKIAH VALLEY"| style_descr=="MC"| style_descr=="MENDOCINO"| style_descr=="CALIFORNIA MENDOCINO"| style_descr=="MC SBC SC"| style_descr=="LEXIS ESTATE MENDOCINO"| upc_descr=="ATZ ZN MD-C UKHVLY V RED DDT"| style_descr=="MC NCSM")]

# San Lucas
d[, San_Lucas := as.integer(style_descr=="MTRY COUNTY SAN LUCAS")]

# San Benito
d[, San_Benito := as.integer(style_descr=="SAN BENITO"| style_descr=="MTRY COUNTY SAN BENITO SC")]

# Napa Valley sub-AVA
d[, Stags_Leap_District := as.integer(style_descr=="NAPA VALLEY SLD"| style_descr=="CA NAPA VALLEY SLD")]
d[, Spring_Mountain_District := as.integer(style_descr=="NAPA VALLEY SMD"| style_descr=="NAPA VALLEY SPRING MOUNTAIN"| style_descr=="CA NAPA VALLEY SMD")]
d[, Chiles_Valley := as.integer(style_descr=="CHILES VALLEY NAPA")]
d[, Atlas_Peak := as.integer(style_descr=="ATLAS PEAK NAPA VALLEY")]
d[, Calistoga := as.integer(style_descr=="CALISTOGA"| style_descr=="CA CALISTOGA NAPA VALLEY"| style_descr=="CALISTOGA NAPA VALLEY")]
d[, Howell_Mountain := as.integer(style_descr=="HOWELL MOUNTAIN"| style_descr=="HOWELL MOUNTAIN NAPA VALLEY")]

# Carneros
d[, Carneros := as.integer(style_descr=="CARNEROS SC"| style_descr=="CARNEROS SONOMA"| style_descr=="CARNEROS NAPA COUNTY"| style_descr=="CARNEROS"| style_descr=="CA CARNEROS"| style_descr=="CA CARNEROS RS"| style_descr=="LOS CARNEROS"| style_descr=="LOS CARNEROS SC"| style_descr=="RS CARNEROS"| style_descr=="LOS CARNEROS NAPA VALLEY"| style_descr=="CARNEROS DISTRICT"| style_descr=="CARNEROS ESTATE RS"| style_descr=="CARNEROS RS"| style_descr=="CARNEROS NAPA VALLEY"| style_descr=="CARNEROS DISTRICT SC"| style_descr=="RS NAPA CARNEROS"| brand_descr=="CARNEROS CREEK WINERY"| brand_descr=="CARNEROS HIGHWAY"| brand_descr=="DOMAINE CARNEROS"| brand_descr=="BUENA VISTA CARNEROS")]

d[, Oak_Knoll := as.integer(style_descr=="NAPA VALLEY OAK KNOLL"| style_descr=="NAPA VALLEY OAK KNOLL DISTRICT"| style_descr=="NAPA VALLEY OKL-DST")]

# Rutherford
d[, Rutherford := as.integer(style_descr=="CA NAPA COUNTY RUTHERFORD"| style_descr=="NAPA RUTHERFORD APPELLATION RS"| style_descr=="NAPA VALLEY RUTHERFORD"| style_descr=="RUTHERFORD NAPA VALLEY"| style_descr=="RUTHERFORD"| style_descr=="RUTHERFORD RS"| style_descr=="NAPA VALLEY RUTHERFORD RS"| brand_descr=="RUTHERFORD"| brand_descr=="RUTHERFORD ESTATE CELLARS"| brand_descr=="RUTHERFORD RANCH"| brand_descr=="RUTHERFORD VINTNERS"| style_descr=="RUTHERFORD DISTRICT")]

# St Helena
d[, St_Helena := as.integer(style_descr=="CA ST HELENA"| style_descr=="ST HELENA"| style_descr=="NAPA VALLEY ST HELENA"| style_descr=="CARNEROS DISTRICT ST HELENA")]

d[, Yountville := as.integer(style_descr=="YOUNTVILLE"| style_descr=="NAPA VALLEY YOUNTVILLE"| style_descr=="CALIF NAPA VALLEY YOUNTVILLE")]

# Napa Valley
d[, Napa_Valley := as.integer(style_descr=="NAPA VALLEY"| style_descr=="NAPA VALLEY CA"| style_descr=="NAPA VALLEY EVENSTAD RS"| style_descr=="NAPA VALLEY FAMILY RS"| style_descr=="NAPA VALLEY GRAND RS"| style_descr=="NAPA VALLEY MARIOS RESERVE"| style_descr=="NAPA VALLEY PVT RS"| style_descr=="NAPA VALLEY RS"| style_descr=="NAPA VALLEY RS SELECTION"| style_descr=="NAPA VALLEY S-V"| style_descr=="NAPA VALLEY SONOMA COAST SC"| style_descr=="NAPA VALLEY SPECIAL RS"| style_descr=="NAPA VALLEY TPFR"| style_descr=="NAPPA VALLEY RS"| style_descr=="NAPA"| style_descr=="NAPA COUNTY NAPA VALLEY"| style_descr=="NAPA NAPA VALLEY"| brand_descr=="NAPA VALLEY VINEYARDS"| style_descr=="MT VEEDER NAPA VALLEY"| style_descr=="CA NAPA SONOMA"| style_descr=="CA NAPA VALLEY"| style_descr=="CA NAPA VALLEY RS"| style_descr=="CA NAPA VALLEY S-C"| style_descr=="AM CANYON NAPA VALLEY"| style_descr=="ESTHERS RS NAPA VALLEY"| style_descr=="MOUNT VEEDER NAPA VALLEY"| style_descr=="SBRAGIA L-R NAPA VALLEY"| style_descr=="NAPA NORTH COAST"| upc_descr=="B SIDE RED FFSNV G RED DDT"| upc_descr=="DECOY SV-B NV V WT DDT"| style_descr=="CA NAPA"| brand_descr=="NAPA CREEK"| brand_descr=="NAPA LANDING"| brand_descr=="NAPA RIDGE NAPA VALLEY"| style_descr=="NCSJCSC"| style_descr=="ATLAS PARK NAPA VALLEY"| style_descr=="CENTRAL COAST NAPA VALLEY"| style_descr=="CENTRAL COAST NAPA VALLEY S-V"| style_descr=="LODI NAPA VALLEY"| brand_descr=="MUMM NAPA"| brand_descr=="SCREW KAPPA NAPA")]
d[, NV := rgx(paste0(" ", upc_descr, " "), " (NV) ")]
d[brand_descr=="NOVAS", NV := 0]
d[NV==1, Napa_Valley := 1]
d[upc_descr=="CHNDN DM CHM WT NAPA BR XD SP", Napa_Valley := 1]

# Napa County
d[, Napa_County := as.integer(style_descr=="CA NAPA COUNTY"| style_descr=="CA NAPA COUNTY SC"| style_descr=="AM CANYON NAPA COUNTY"| style_descr=="LC NAPA COUNTY SC"| style_descr=="NAPA COUNTY"| style_descr=="NAPA COUNTY RS"| style_descr=="NAPA COUNTY SBC"| style_descr=="NAPA COUNTY SC"| style_descr=="NAPA COUNTY SLD"| style_descr=="NAPA COUNTY SLOC"| style_descr=="NAPA COUNTY SONOMA"| style_descr=="MC NAPA COUNTY SC"| style_descr=="LC MTRY NAPA SANTRA BARBARA"| style_descr=="AMADOR COUNTY MC NAPA COUNTY"| style_descr=="CA LC MTRY NAPA SANTA BARBARA"| style_descr=="MTRY COUNTY NAPA COUNTY SC"| style_descr=="NAPA COUNTY SONOMA COUNTY"| upc_descr=="CTL BR SV-B N-C V WT BB DDT"| upc_descr=="PCFCPK CB-S N-C V RED DDT")]

# North Coast
d[, North_Coast := as.integer(style_descr=="CA NORTH COAST"| style_descr=="CA NORTH COAST CA RS"| style_descr=="CA NORTH COAST CA RS"| style_descr=="CA NORTH COAST SC"| style_descr=="LC MC NORTH COAST SC"| style_descr=="NORTH COAST SC RS"| style_descr=="NORTH COAST VINTNERS BLEND"| style_descr=="NORTH COAST"| style_descr=="CENTRAL COAST LODI NORTH COAST"| style_descr=="MC NAPA COUNTY NORTH COAST SC")]
d[, northcoast := rgx(paste0(" ", upc_descr, " "), " (NC) ")]
d[northcoast==1, North_Coast := 1]
d[, northcoast := NULL]

# Guenoc
d[, Guenoc := as.integer(style_descr=="CA GUENOC VALLEY"| style_descr=="GUENOC VALLEY"| brand_descr=="GUENOC")]

# Russian River Valley
d[, Russian_River_Valley := as.integer(style_descr=="CA RRV"| style_descr=="CA RRV RS"| style_descr=="CA RRV SONOMA"| style_descr=="CA RRV SC RS"| style_descr=="RRV RS"| style_descr=="RRV SC"| style_descr=="RRV SC GRANDE RS"| style_descr=="RRV SMALL LOT RS"| style_descr=="RRV SPECIAL RS"| style_descr=="RRV WEST COAST"| style_descr=="RUSSIAN RIVER"| style_descr=="RRV SC BARREL FERMENTED"| style_descr=="RRV SC RS"| style_descr=="RRV W-R"| style_descr=="RRV SONOMA RS"| style_descr=="RRV"| style_descr=="CA RRV SC"| style_descr=="CA RRV SONOMA RS"| style_descr=="CRRVSC"| style_descr=="GVORRV SC"| style_descr=="NORTHERN SONOMA RRV RS"| style_descr=="ESTATE RRV")]

# Santa Barbara County (Happy canyon of SBC is an AVA - need to check)
d[, Santa_Barbara_County := as.integer(style_descr=="SBC"| style_descr=="SBC VINTNERS RS"| style_descr=="CA SBC"| style_descr=="CA VINO ROSSO DI SANTA BARBARA"| style_descr=="SBC"| brand_descr=="SANTA BARBARA"| upc_descr=="ZINNIA P-N SBC RS V RED DDT"| upc_descr=="TRN FM SV-B SBC V WT DDT"| style_descr=="SANTA BARBARA"| style_descr=="SBC RS"| style_descr=="SBC RS SELECTION"| style_descr=="MALIBU SBC"| style_descr=="SANTA BARBARA COUNTY")]

# Shenandoah
d[, Shenandoah_Valley_Cal := as.integer(style_descr=="CA SHENANDOAH VALLEY"| style_descr=="CA SHENANDOAH VALLEY RS")]
d[, Shenandoah_Valley := as.integer(style_descr=="SHENANDOAH VALLEY"| brand_descr=="SHENANDOAH")]

# Northern Sonoma
d[, Northern_Sonoma := as.integer(style_descr=="NORTHERN SONOMA"| style_descr=="NORTHERN SONOMA RS")]

# Sonoma Mountain
d[, Sonoma_Mountain := as.integer(style_descr=="SONOMA MOUNTAIN")]

# Sonoma Coast
d[, Sonoma_Coast := as.integer(style_descr=="SONOMA COAST"| style_descr=="CA SONOMA COAST"| style_descr=="CALIF SONOMA COAST SC"| style_descr=="CA SONOMA COAST RS"| style_descr=="CA SONOMA COAST RS"| style_descr=="ESTATE SONOMA COAST"| style_descr=="SONOMA COAST"| style_descr=="SONOMA COAST BARREL RS"| style_descr=="SONOMA COAST SC"| style_descr=="LES PIERRES SONOMA COAST"| style_descr=="CA SC SC"| style_descr=="CA SONOMA COAST SC"| style_descr=="LES PIERRES SONOMA COAST"| style_descr=="SONOMA COAST SONOMA COUNTY"| style_descr=="SC SONOMA COUNTY")]

# Sonoma Valley
d[, Sonoma_Valley := as.integer(style_descr=="S-V"| style_descr=="S-V JACK LONDON VINEYARD"| style_descr=="S-V PVT RS"| style_descr=="S-V RS"| style_descr=="CA NORTHERN S-V"| style_descr=="SC S-V")]

# Only Sonoma (not AVA - need to check)
d[, Sonoma := as.integer(style_descr=="CA SONOMA"| style_descr=="CLARK HILL SONOMA COUNTY"| style_descr=="SONOMA COUNTY"| style_descr=="SONOMA RS"| style_descr=="SONOMA VINTNERS SELECT"| style_descr=="SC"| style_descr=="SONOMA"| style_descr=="SONOMA BEACH SC"| style_descr=="SC RS"| style_descr=="SC SONOMA RS"| style_descr=="PVT RS SC"| style_descr=="GRATON SONOMA COUNTY CA"| style_descr=="CA SC"| style_descr=="SAN JOAQUIN COUNTY SC"| style_descr=="SC PVT RS"| style_descr=="SLOC SC"| style_descr=="COUNTY SC"| style_descr=="MC SC"| style_descr=="SC PVT RS"| style_descr=="ANNAPOLIS CA SC")]

# South Coast
d[, South_Coast := as.integer(style_descr=="CA SOUTH COAST RS"| style_descr=="CA SOUTH COAST"| style_descr=="SOUTH COAST"| brand_descr=="SOUTH COAST WINERY")]

# Suisun Valley
d[, Suisun_Valley := as.integer(style_descr=="CA SUISUN VALLEY"| style_descr=="SUISUN VALLEY")]

# Santa Cruz Mountain
d[, Santa_Cruz_Mountain := as.integer(style_descr=="CA SCM"| style_descr=="SANTA CRUZ")]

# Willamette
d[, Willamette_Valley := as.integer(style_descr=="WV"| style_descr=="OREGON WV"| style_descr=="WILLIAMETTE VALLEY RS"| style_descr=="OREGON WV RS"| style_descr=="WILLIAMETTE VALLEY"| style_descr=="OREGON WILLAMETTE VALLEY"| style_descr=="OREGON WLMT VLY"| brand_descr=="WILLAMETTE VALLEY"| style_descr=="OREGON WILLIAMETTE VALLEY")]
d[, match_wv := rgx(paste0(" ", upc_descr, " "), " (WV) ")]
d[brand_descr=="WV", match_wv := 0]
d[match_wv==1, Willamette_Valley := 1]
d[, match_wv := NULL]

# Applegate
d[, Applegate_Valley := as.integer(style_descr=="APLGTE VALLEY")]

# Red Hills Lake
d[, Red_Hills_Lake_County := as.integer(style_descr=="CA CA RED HILLS LAKE COUNTY"| style_descr=="CA RED HILLS LC"| style_descr=="RED HILL LC"| style_descr=="RED HILLS LAKE"| style_descr=="RED HILLS LC"| style_descr=="RED HILLS LAKE COUNTY"| style_descr=="CA RED HILLS LAKE COUNTY")]

d[, Clear_Lake := as.integer(style_descr=="CLEAR LAKE LC"| style_descr=="CLEAR LAKE")]

d[, Dundee_Hills := as.integer(style_descr=="DUNDEE OREGON"| style_descr=="DUNDEE HILLS"| style_descr=="DUNDEE HILLS OREGON")]

d[, Dunnigan_Hills := as.integer(style_descr=="DUNNINGAN HILLS"| style_descr=="DUNNIGAN HILLS MUSQUE CLONE"| style_descr=="DUNNIGAN HILLS")]

d[, El_Dorado := as.integer(style_descr=="EL DORADO"| style_descr=="ELDORADO COUNTRY RS"| style_descr=="ELDORADO COUNTY")]

d[, Eola_Hills := as.integer(style_descr=="EOLA AMITY HILLS"| style_descr=="EOLA AMITY HILLS RS SERIES"| style_descr=="EOLA AMITY HILLS WV"| style_descr=="EOLA HILLS"| brand_descr=="EOLA HILLS")]

d[, Grand_River_Valley := as.integer(style_descr=="GRAND RIVER VALLEY OHIO"| style_descr=="GRAND RIVER VALLEY")]

d[, High_Valley := as.integer(style_descr=="HIGH VALLEY APPELLATION"| style_descr=="HIGH VALLEY LC"| style_descr=="HIGH VALLEY LC RS"| style_descr=="HIGH VALLEY LC TWO BUD BLOCK"| style_descr=="CA HIGH VALLEY LC"| style_descr=="HIGH VALLEY")]

d[, Lake_Michigan_Shore := as.integer(style_descr=="LAKE MICHIGAN SHORE MICHIGAN"| style_descr=="LAKE MICHIGAN SHORE RS"| style_descr=="LAKE MICHIGAN SHORE")]

d[, Long_Island := as.integer(style_descr=="NORTH FORK OF LONG ISLAND"| style_descr=="NORTH FORK OF LONG ISLAND RS"| style_descr=="LONG ISLAND"| style_descr=="THE NORTH FORK OF LONG ISLAND"| style_descr=="LONG ISLAND NEW YORK SAGAPONCK"| style_descr=="LONG ISLAND NEW YORK")]

d[, Umpqua_Valley := as.integer(style_descr=="OREGON UMPQUA VALLEY"| style_descr=="SOUTHERN OREGON UMPQUA VALLEY"| style_descr=="UMPQUA VALLEY")]

d[, Madera := as.integer(style_descr=="MADERA")]

d[, Isle_St_George := as.integer(style_descr=="ISLE ST GEORGE")]

d[, Ohio_River_Valley := as.integer(style_descr=="OHIO RIVER VALLEY")]

d[, Finger_Lakes := as.integer(style_descr=="FINGER LAKES SENECA LAKE"| style_descr=="FINGER LAKES")]

d[, Lake_Erie := as.integer(style_descr=="CHAUTAUQUA REGION LAKE ERIE"| brand_descr=="LAKE ERIE"| style_descr=="LAKE ERIE")]

d[, Yamhill_Carlton := as.integer(style_descr=="YAMHILL VALLEY"| style_descr=="OREGON YAMHILL CARLTON")]

d[, Mcminnville := as.integer(style_descr=="MCMINNVILLE A V A"| style_descr=="MCMINNVILLE A V A OREGON WV"| style_descr=="MCMINNVILLE A V A OREGON")]

d[, Rogue_Valley := as.integer(style_descr=="OREGON ROGUE VALLEY"| style_descr=="ROGUE VALLEY SOUTHERN OREGON"| style_descr=="OREGONS ROGUE VALLEY"| style_descr=="OREGON ROGUE VALLEY"| style_descr=="ROGUE VALLEY"| style_descr=="OREGON ROGUE VALLEY")]

d[, Snake_River_Valley := as.integer(style_descr=="IDAHO SNAKE RIVER VALLEY AVA"| brand_descr=="SNAKE RIVER"| style_descr=="SNAKE RIVER VALLEY")]

d[, Colorado_Grand_Valley := as.integer(style_descr=="COLORADO GRAND VALLEY"| style_descr=="COLORADO EOM GRAND VALLEY")]

d[, Lancaster_Valley := as.integer(style_descr=="LANCASTER VALLEY")]

d[, Lehigh_Va := as.integer(style_descr=="LEHIGH VA"| style_descr=="LEHIGH VALLEY")]

d[, Leelanau_Peninsula := as.integer(style_descr=="LEELANAU PENINSULA"| style_descr=="OLD MISSION PENINSULA"| style_descr=="OLD MISSION PENINSULA RS"| style_descr=="OLD MISSION PENISULA"| style_descr=="MICHIGAN OLD MISSION PENINSULA"| style_descr=="MICHIGAN OLD MISSION PENINSULA")]

d[, Ozark_Hignlands := as.integer(style_descr=="OZARK HIGHLANDS"| style_descr=="OZARK MOUNTAIN")]
d[, Yadkin_Valley := as.integer(style_descr=="YADKIN VALLEY"| style_descr=="NORTH CAROLINA YADKIN VALLEY")]
d[, Texas_High_Plains := as.integer(style_descr=="TEXAS HIGH PLAINS")]
d[, Texas_Hill_Country := as.integer(style_descr=="TEXAS HILL COUNTRY")]
d[, Altus := as.integer(style_descr=="ALTUS"| style_descr=="ALTUS ARKANSAS")]
d[, Monticello := as.integer(style_descr=="MONTICELLO"| style_descr=="MONTICELLO RS")]

d[, Redwood_Valley := as.integer(style_descr=="REDWOOD VALLEY"| style_descr=="MC REDWOOD VALLEY"| style_descr=="MENDOCINO REDWOOD VALLEY")]

d[, Solano_County_Green_Valley := as.integer(style_descr=="GREEN VALLEY SOLANO COUNTY"| style_descr=="GREEN VALLEY SC"| style_descr=="GREEN VALLEY"| style_descr=="CA GREEN VALLEY SOLANO COUNTY"| style_descr=="SOLANO COUNTY")]

d[, Augusta := as.integer(style_descr=="AUGUSTA"| brand_descr=="AUGUSTA")]

d[, Sierra_Foothills := as.integer(style_descr=="SIERRA FOOTHILLS"| style_descr=="AMADOR COUNTY SIERRA FOOTHILLS"| style_descr=="CA SIERRA FOOTHILLS"| style_descr=="PLACER COUNTY SIERRA FOOTHILLS"| style_descr=="SIENA SC"| style_descr=="SIERRA FOOTHILL"| style_descr=="SIERRA FOOTHILL GRAND RS"| brand_descr=="SIERRA COLD"| brand_descr=="SIERRA VISTA")]

d[, Amador_County := as.integer(style_descr=="AMADOR COUNTY CA"| style_descr=="AMADOR COUNTY COUGAR HILL"| style_descr=="AMADOR COUNTY J & S RS"| style_descr=="AMADOR COUNTY MC"| style_descr=="AMADOR COUNTY OLD VINE RS"| style_descr=="AMADOR COUNTY")]

d[, Columbia_Gorge := as.integer(style_descr=="COLUMBIA GORGE")]

d[, Cucamonga_Valley := as.integer(style_descr=="CUCAMONGA VALLEY")]

d[, Fiddletown := as.integer(style_descr=="FIDDLETOWN")]

d[, Lake_Chelan := as.integer(style_descr=="LAKE CHELAN")]

d[, Oakville := as.integer(style_descr=="OAKVILLE"| style_descr=="NAPA VALLEY OAKVILLE"| style_descr=="NAPA VALLEY OAKVILLE RS"| style_descr=="CALIF NAPA VALLEY OAKVILLE"| style_descr=="CA NAPA VALLEY OAKVILLE")]

d[, Rockpile := as.integer(style_descr=="ROCKPILE"| style_descr=="ROCKPILE SC")]

d[, Tracy_Hills := as.integer(style_descr=="TRACY HILLS")]

d[, Yorkville_Highlands := as.integer(style_descr=="YORKVILLE HIGHLANDS"| style_descr=="MENDOCINO YORKVILLE HIGHLANDS")]

d[, Mcdowell_Valley := as.integer(brand_descr=="MCDOWELL VALLEY VINEYARDS")]

d[, Temecula_Valley := as.integer(style_descr=="COASTAL RS TEMECULE VALLEY"| style_descr=="TEMECULA"| style_descr=="TEMECULA VALLEY")]
d[, Southeastern_New_England := as.integer(style_descr=="SOUTHEASTERN NEW ENGLAND")]
d[, Lake_County := as.integer(style_descr=="LC RS"| style_descr=="LC"| style_descr=="CA LC"| style_descr=="CALIF LC SHANNON RIDGE"| style_descr=="LC MC")]
d[, Potter_Valley := as.integer(style_descr=="POTTER VALLEY"| style_descr=="CA POTTER VALLEY")]

# -----------------------------------------------------------------------------
# ava = sum of all AVA dummies (1st pass, used only as scratch then dropped)
# -----------------------------------------------------------------------------
.ava_vars <- c("Alexander_Valley","Anderson_Valley","ALOCV","Horse_Heaven_Hills","Rattle_Snake_Hills","Red_Mountain","Walla_Walla","Yakima_Valley","Wahluke_Slope","Columbia_Valley","Hames_Valley","San_Bernabe","Arroyo_Grande","Arroyo_Seco","Chalone","Carmel_Valley","Cienega_Valley","Edna_Valley","Livermore_Valley","San_Antonio","Santa_Clara","Saint_Lucia_Highlands","Santa_Maria_Valley","Santa_Ynes_Valley","Sta_Rita_Hills","Monterey_County","Paicines","Paso_Robles","Central_Coast","Chalk_Hill","Chehalem_Mountains","Dry_Creek_Valley","Eagle_Peak","Clarksburg","Knights_Valley","Lodi","Mendocino","San_Lucas","San_Benito","Stags_Leap_District","Spring_Mountain_District","Chiles_Valley","Atlas_Peak","Calistoga","Howell_Mountain","Carneros","Oak_Knoll","Rutherford","St_Helena","Yountville","Napa_Valley","Napa_County","North_Coast","Guenoc","Russian_River_Valley","Santa_Barbara_County","Shenandoah_Valley_Cal","Shenandoah_Valley","Northern_Sonoma","Sonoma_Mountain","Sonoma_Coast","Sonoma_Valley","Sonoma","South_Coast","Suisun_Valley","Santa_Cruz_Mountain","Willamette_Valley","Applegate_Valley","Red_Hills_Lake_County","Clear_Lake","Dundee_Hills","Dunnigan_Hills","El_Dorado","Eola_Hills","Grand_River_Valley","High_Valley","Lake_Michigan_Shore","Long_Island","Umpqua_Valley","Madera","Isle_St_George","Ohio_River_Valley","Finger_Lakes","Lake_Erie","Yamhill_Carlton","Mcminnville","Rogue_Valley","Snake_River_Valley","Colorado_Grand_Valley","Lancaster_Valley","Lehigh_Va","Leelanau_Peninsula","Ozark_Hignlands","Yadkin_Valley","Texas_High_Plains","Texas_Hill_Country","Altus","Monticello","Redwood_Valley","Solano_County_Green_Valley","Augusta","Sierra_Foothills","Amador_County","Columbia_Gorge","Cucamonga_Valley","Fiddletown","Lake_Chelan","Oakville","Rockpile","Tracy_Hills","Yorkville_Highlands","Mcdowell_Valley","Temecula_Valley","Southeastern_New_England","Lake_County","Potter_Valley")
d[, ava := rowSums(.SD), .SDcols = .ava_vars]

# Overlap-resolution block
d[Horse_Heaven_Hills==1,      Columbia_Valley := 0]
d[Monterey_County==1,        Central_Coast := 0]
d[Stags_Leap_District==1,    Napa_Valley := 0]
d[Russian_River_Valley==1,   Napa_Valley := 0]
d[Edna_Valley==1,            Central_Coast := 0]
d[Carneros==1,               Napa_Valley := 0]
d[Livermore_Valley==1,       Central_Coast := 0]
d[Rutherford==1,             Napa_Valley := 0]
d[Lake_County==1,            North_Coast := 0]
d[Alexander_Valley==1,       Napa_Valley := 0]
d[Anderson_Valley==1,        Sonoma := 0]
d[Alexander_Valley==1,       Sonoma := 0]
d[Temecula_Valley==1,        South_Coast := 0]
d[Willamette_Valley==1,      Columbia_Valley := 0]
d[Eola_Hills==1,             Willamette_Valley := 0]
d[Arroyo_Seco==1,            Monterey_County := 0]
d[San_Antonio==1,            Paso_Robles := 0]
d[Saint_Lucia_Highlands==1,  Monterey_County := 0]
d[Monterey_County==1,        Napa_County := 0]
d[Paicines==1,               Central_Coast := 0]
d[Central_Coast==1,          Santa_Barbara_County := 0]
d[Dry_Creek_Valley==1,       Sonoma := 0]
d[Russian_River_Valley==1,   Dry_Creek_Valley := 0]
d[Lodi==1,                   Monterey_County := 0]
d[Spring_Mountain_District==1, Napa_Valley := 0]
d[Calistoga==1,              Napa_Valley := 0]
d[Sonoma_Valley==1,          Carneros := 0]
d[Sonoma==1,                 Carneros := 0]
d[St_Helena==1,              Napa_Valley := 0]
d[Napa_Valley==1,            Sonoma := 0]
d[Napa_Valley==1,            North_Coast := 0]
d[Napa_Valley==1,            Lodi := 0]
d[Napa_Valley==1,            Napa_County := 0]
d[Oakville==1,               Napa_Valley := 0]
d[Napa_Valley==1,            Sonoma_Valley := 0]
d[North_Coast==1,            Guenoc := 0]
d[Sonoma_Coast==1,           North_Coast := 0]
d[Lake_County==1,            Guenoc := 0]
d[Russian_River_Valley==1,   Central_Coast := 0]
d[Northern_Sonoma==1,        Sonoma := 0]
d[Sonoma_Mountain==1,        El_Dorado := 0]
d[Sonoma_Mountain==1,        Sonoma := 0]
d[El_Dorado==1,              Sierra_Foothills := 0]
d[Lodi==1,                   Eola_Hills := 0]
d[, ava := NULL]
d[, ava := rowSums(.SD), .SDcols = .ava_vars]
d[ava==2, Central_Coast := 0]   # for last nine overlap

# -----------------------------------------------------------------------------
# wine_appellation labels (last write wins, matching Stata sequential replaces)
# -----------------------------------------------------------------------------
d[Alexander_Valley==1, wine_appellation := "Alexander_Valley"]
d[Anderson_Valley==1, wine_appellation := "Anderson_Valley"]
d[ALOCV==1, wine_appellation := "ALOCV"]
d[Horse_Heaven_Hills==1, wine_appellation := "Horse_Heaven_Hills"]
d[Rattle_Snake_Hills==1, wine_appellation := "Rattle_Snake_Hills"]
d[Red_Mountain==1, wine_appellation := "Red_Mountain"]
d[Walla_Walla==1, wine_appellation := "Walla_Walla"]
d[Yakima_Valley==1, wine_appellation := "Yakima_Valley"]
d[Wahluke_Slope==1, wine_appellation := "Wahluke_Slope"]
d[Columbia_Valley==1, wine_appellation := "Columbia_Valley"]
d[Hames_Valley==1, wine_appellation := "Hames_Valley"]
d[San_Bernabe==1, wine_appellation := "San_Bernabe"]
d[Arroyo_Grande==1, wine_appellation := "Arroyo_Grande"]
d[Arroyo_Seco==1, wine_appellation := "Arroyo_Seco"]
d[Chalone==1, wine_appellation := "Chalone"]
d[Carmel_Valley==1, wine_appellation := "Carmel_Valley"]
d[Cienega_Valley==1, wine_appellation := "Cienega_Valley"]
d[Edna_Valley==1, wine_appellation := "Edna_Valley"]
d[Livermore_Valley==1, wine_appellation := "Livermore_Valley"]
d[San_Antonio==1, wine_appellation := "San_Antonio"]
d[Santa_Clara==1, wine_appellation := "Santa_Clara"]
d[Saint_Lucia_Highlands==1, wine_appellation := "Saint_Lucia_Highlands"]
d[Santa_Maria_Valley==1, wine_appellation := "Santa_Maria_Valley"]
d[Santa_Ynes_Valley==1, wine_appellation := "Santa_Ynes_Valley"]
d[Sta_Rita_Hills==1, wine_appellation := "Sta_Rita_Hills"]
d[Monterey_County==1, wine_appellation := "Monterey_County"]
d[Paicines==1, wine_appellation := "Paicines"]
d[Paso_Robles==1, wine_appellation := "Paso_Robles"]
d[Central_Coast==1, wine_appellation := "Central_Coast"]
d[Chalk_Hill==1, wine_appellation := "Chalk_Hill"]
d[Chehalem_Mountains==1, wine_appellation := "Chehalem_Mountains"]
d[Dry_Creek_Valley==1, wine_appellation := "Dry_Creek_Valley"]
d[Eagle_Peak==1, wine_appellation := "Eagle_Peak"]
d[Clarksburg==1, wine_appellation := "Clarksburg"]
d[Knights_Valley==1, wine_appellation := "Knights_Valley"]
d[Lodi==1, wine_appellation := "Lodi"]
d[Mendocino==1, wine_appellation := "Mendocino"]
d[San_Lucas==1, wine_appellation := "San_Lucas"]
d[San_Benito==1, wine_appellation := "San_Benito"]
d[Stags_Leap_District==1, wine_appellation := "Stags_Leap_District"]
d[Spring_Mountain_District==1, wine_appellation := "Spring_Mountain_District"]
d[Chiles_Valley==1, wine_appellation := "Chiles_Valley"]
d[Atlas_Peak==1, wine_appellation := "Atlas_Peak"]
d[Calistoga==1, wine_appellation := "Calistoga"]
d[Howell_Mountain==1, wine_appellation := "Howell_Mountain"]
d[Carneros==1, wine_appellation := "Carneros"]
d[Oak_Knoll==1, wine_appellation := "Oak_Knoll"]
d[Rutherford==1, wine_appellation := "Rutherford"]
d[St_Helena==1, wine_appellation := "St_Helena"]
d[Yountville==1, wine_appellation := "Yountville"]
d[Napa_Valley==1, wine_appellation := "Napa_Valley"]
d[Napa_County==1, wine_appellation := "Napa_County"]
d[North_Coast==1, wine_appellation := "North_Coast"]
d[Guenoc==1, wine_appellation := "Guenoc"]
d[Russian_River_Valley==1, wine_appellation := "Russian_River_Valley"]
d[Santa_Barbara_County==1, wine_appellation := "Santa_Barbara_County"]
d[Shenandoah_Valley_Cal==1, wine_appellation := "Shenandoah_Valley_Cal"]
d[Shenandoah_Valley==1, wine_appellation := "Shenandoah_Valley"]
d[Northern_Sonoma==1, wine_appellation := "Northern_Sonoma"]
d[Sonoma_Mountain==1, wine_appellation := "Sonoma_Mountain"]
d[Sonoma_Coast==1, wine_appellation := "Sonoma_Coast"]
d[Sonoma_Valley==1, wine_appellation := "Sonoma_Valley"]
d[Sonoma==1, wine_appellation := "Sonoma"]
d[South_Coast==1, wine_appellation := "South_Coast"]
d[Suisun_Valley==1, wine_appellation := "Suisun_Valley"]
d[Santa_Cruz_Mountain==1, wine_appellation := "Santa_Cruz_Mountain"]
d[Willamette_Valley==1, wine_appellation := "Willamette_Valley"]
d[Applegate_Valley==1, wine_appellation := "Applegate_Valley"]
d[Red_Hills_Lake_County==1, wine_appellation := "Red_Hills_Lake_County"]
d[Clear_Lake==1, wine_appellation := "Clear_Lake"]
d[Dundee_Hills==1, wine_appellation := "Dundee_Hills"]
d[Dunnigan_Hills==1, wine_appellation := "Dunnigan_Hills"]
d[El_Dorado==1, wine_appellation := "El_Dorado"]
d[Eola_Hills==1, wine_appellation := "Eola_Hills"]
d[Grand_River_Valley==1, wine_appellation := "Grand_River_Valley"]
d[High_Valley==1, wine_appellation := "High_Valley"]
d[Lake_Michigan_Shore==1, wine_appellation := "Lake_Michigan_Shore"]
d[Long_Island==1, wine_appellation := "Long_Island"]
d[Umpqua_Valley==1, wine_appellation := "Umpqua_Valley"]
d[Madera==1, wine_appellation := "Madera"]
d[Isle_St_George==1, wine_appellation := "Isle_St_George"]
d[Ohio_River_Valley==1, wine_appellation := "Ohio_River_Valley"]
d[Finger_Lakes==1, wine_appellation := "Finger_Lakes"]
d[Lake_Erie==1, wine_appellation := "Lake_Erie"]
d[Yamhill_Carlton==1, wine_appellation := "Yamhill_Carlton"]
d[Mcminnville==1, wine_appellation := "Mcminnville"]
d[Rogue_Valley==1, wine_appellation := "Rogue_Valley"]
d[Snake_River_Valley==1, wine_appellation := "Snake_River_Valley"]
d[Colorado_Grand_Valley==1, wine_appellation := "Colorado_Grand_Valley"]
d[Lancaster_Valley==1, wine_appellation := "Lancaster_Valley"]
d[Lehigh_Va==1, wine_appellation := "Lehigh_Va"]
d[Leelanau_Peninsula==1, wine_appellation := "Leelanau_Peninsula"]
d[Ozark_Hignlands==1, wine_appellation := "Ozark_Hignlands"]
d[Yadkin_Valley==1, wine_appellation := "Yadkin_Valley"]
d[Texas_High_Plains==1, wine_appellation := "Texas_High_Plains"]
d[Texas_Hill_Country==1, wine_appellation := "Texas_Hill_Country"]
d[Altus==1, wine_appellation := "Altus"]
d[Monticello==1, wine_appellation := "Monticello"]
d[Redwood_Valley==1, wine_appellation := "Redwood_Valley"]
d[Solano_County_Green_Valley==1, wine_appellation := "Solano_County_Green_Valley"]
d[Augusta==1, wine_appellation := "Augusta"]
d[Sierra_Foothills==1, wine_appellation := "Sierra_Foothills"]
d[Amador_County==1, wine_appellation := "Amador_County"]
d[Columbia_Gorge==1, wine_appellation := "Columbia_Gorge"]
d[Cucamonga_Valley==1, wine_appellation := "Cucamonga_Valley"]
d[Fiddletown==1, wine_appellation := "Fiddletown"]
d[Lake_Chelan==1, wine_appellation := "Lake_Chelan"]
d[Oakville==1, wine_appellation := "Oakville"]
d[Rockpile==1, wine_appellation := "Rockpile"]
d[Tracy_Hills==1, wine_appellation := "Tracy_Hills"]
d[Yorkville_Highlands==1, wine_appellation := "Yorkville_Highlands"]
d[Mcdowell_Valley==1, wine_appellation := "Mcdowell_Valley"]
d[Temecula_Valley==1, wine_appellation := "Temecula_Valley"]
d[Southeastern_New_England==1, wine_appellation := "Southeastern_New_England"]
d[Lake_County==1, wine_appellation := "Lake_County"]
d[Potter_Valley==1, wine_appellation := "Potter_Valley"]

# =============================================================================
# Capturing State Appellation
# =============================================================================
d[, state_appellation := ""]
d[style_descr=="NEW JERSEY", state_appellation := "NEW JERSEY"]
d[style_descr=="NEW MEXICO"| style_descr=="NEW MEXICO"| style_descr=="NEW MEXICO CELLARMASTERS RS"| style_descr=="NEW MEXICO RS"| style_descr=="NEW MEXICO W-R"| brand_descr=="NEW MEXICO WINE-A-RITA", state_appellation := "NEW MEXICO"]
d[style_descr=="NEW YORK STATE"| style_descr=="NEW YORK STATE NATIVE"| style_descr=="NEW YORK STATE SPECIAL RS"| style_descr=="NEW YORK"| type_descr=="NEW YORK", state_appellation := "NEW YORK"]
d[style_descr=="OHIO", state_appellation := "OHIO"]
d[style_descr=="MARYLAND", state_appellation := "MARYLAND"]
d[style_descr=="COLORADO WESTERN SLOPE"| style_descr=="MESA COUNTY", state_appellation := "COLORADO"]
d[style_descr=="COCHISE COUNTY ARIZONA"| style_descr=="COCHISE COUNTY ARIZONA TOSCANO"| style_descr=="ARIZONA"| style_descr=="ARIZONA COCHISE COUNTY"| brand_descr=="ARIZONA STRONGHOLD"| brand_descr=="ARIZONA ANGEL"| brand_descr=="ARIZONA SUNSET", state_appellation := "ARIZONA"]
d[style_descr=="COMMANCHE COUNTY TEXAS"| brand_descr=="TEXAS COUNTRY CELLARS"| brand_descr=="TEXAS ON THE PLATE"| style_descr=="TEXAS BARREL RS"| style_descr=="TEXAS PVT RS"| style_descr=="TEXAS RS"| style_descr=="TEXAS"| style_descr=="KINSEY TEXAS", state_appellation := "TEXAS"]
d[style_descr=="CONNECTICUT", state_appellation := "CONNECTICUT"]
d[style_descr=="CROW OREGON"| style_descr=="OREGON FOUNDERS RS"| style_descr=="OREGON ROSEBURG"| style_descr=="OREGON RS"| style_descr=="SOUTHERN OREGON"| style_descr=="OREGON"| style_descr=="EAH OREGON", state_appellation := "OREGON"]
d[style_descr=="DOOR COUNTY WISCONSIN"| style_descr=="WISCONSIN"| style_descr=="WISCONSIN RS"| brand_descr=="WISCON", state_appellation := "WISCONSIN"]
d[style_descr=="GEORGIA"| style_descr=="DUNCAN CREEK GEORGIA", state_appellation := "GEORGIA"]
d[style_descr=="ILLINOIS"| style_descr=="ILLINOIS RIVER VALLEY"| brand_descr=="ILLINOIS CELLARS"| brand_descr=="ILLINOIS RIVER WINERY", state_appellation := "ILLINOIS"]
d[style_descr=="IDAHO"| style_descr=="IDAHO RS SERIES", state_appellation := "IDAHO"]
d[style_descr=="INDIANA", state_appellation := "INDIANA"]
d[style_descr=="LOUDOUN COUNTY VIRGINIA"| style_descr=="ORANGE COUNTY VIRGINIA"| style_descr=="VIRGINIA"| style_descr=="VIRGINIA RS"| style_descr=="ALBEMARLE COUNTY VIRGINIA"| style_descr=="RS VIRGINIA"| style_descr=="ORANGE COUNTY VIRGINIA", state_appellation := "VIRGINIA"]
d[style_descr=="MINNESOTA"| style_descr=="MINNESOTA RS", state_appellation := "MINNESOTA"]
d[style_descr=="MISSOURI"| style_descr=="MISSOURI RS"| style_descr=="MISSOURI ST VINCENT", state_appellation := "MISSOURI"]
d[style_descr=="NORTH CAROLINA", state_appellation := "NORTH CAROLINA"]
d[style_descr=="WEST VIRGINIA", state_appellation := "WEST VIRGINIA"]
d[style_descr=="WASHINGTON STATE"| style_descr=="WASHINGTON"| style_descr=="CVISRV WASHINGTON STATE"| style_descr=="PARADISE PEAK WASHINGTON STATE"| style_descr=="RS WASHINGTON"| style_descr==" RS WASHINGTON STATE"| style_descr=="WASHINGTON RS"| style_descr=="WASHINGTON STATE RS"| brand_descr=="WASHINGTON"| brand_descr=="JONES OF WASHINGTON"| brand_descr=="WASHINGTON HILLS", state_appellation := "WASHINGTON"]
d[style_descr=="ARKANSAS", state_appellation := "ARKANSAS"]
d[style_descr=="PENNSYLVANIA", state_appellation := "PENNSYLVANIA"]
d[style_descr=="IDAHO WASHINGTON", state_appellation := "IDAHO WASHINGTON"]
d[style_descr=="COLORADO", state_appellation := "COLORADO"]
d[style_descr=="OREGON WASHINGTON", state_appellation := "OREGON_WASH"]
d[style_descr=="INDIANA MICHIGAN", state_appellation := "INDIANA_MICHIGAN"]
d[brand_descr=="MICHIGAN AWESOME", state_appellation := "MICHIGAN"]
d[style_descr=="IOWA"| style_descr=="CLINTON IOWA", state_appellation := "IOWA"]
d[style_descr=="ALABAMA", state_appellation := "ALABAMA"]
d[style_descr=="NEBRASKA", state_appellation := "NEBRASKA"]
d[style_descr=="NEW HAMPSHIRE"| style_descr=="AMHERST NEW HAMPSHIRE", state_appellation := "NEW HAMPSHIRE"]

d[style_descr=="CA RS"| style_descr=="CA UKIAH VALLEY"| style_descr=="CA VALLEY OAKS"| style_descr=="CA SANTA ROSA"| style_descr=="CA DOOR COUNTY"| upc_descr=="BRFT CB-S CA V RED DDT"| upc_descr=="BRFT P-GR CA V WT DDT"| upc_descr=="BYBRDGV MRLT CA V RED DDT"| upc_descr=="CA' MOMI BNC DI CA V WT DDT"| upc_descr=="CA' MOMI RSO DI CA V RED DDT"| upc_descr=="C-CYN WT ZN CA V BLS BB DDT"| style_descr=="California"| style_descr=="CA CLASSIC"| style_descr=="CA CLASSIC RS"| style_descr=="CA CLASSICS"| style_descr=="CA GLEN ELLEN RS"| style_descr=="CA MASTER LOT RS"| style_descr=="CA MOUNTAIN"| style_descr=="CA VINTNERS RS"| style_descr=="CA PROPRIETORS RS"| style_descr=="CA WILDCREEK CANYON"| style_descr=="VALLEY OAKS CA"| style_descr=="CA VINTNERS BLEND"| style_descr=="CA WILDCREEK CANYON"| style_descr=="CA WILLOW SPRINGS"| style_descr=="CA PREMIUM RS"| style_descr=="CALIFORNIA"| style_descr=="GRANTON CA"| style_descr=="CA COASTAL REGION"| style_descr=="CA NEVADA COUNTY"| style_descr=="CA PVT RS"| style_descr=="CA REPUBLIC"| style_descr=="CA SMALL LOT RS"| style_descr=="CA VINTNERS SELECT"| style_descr=="CA W-R"| style_descr=="CA BIN RS"| style_descr=="CA CARINENA"| style_descr=="CA RS SMALL LOT RS"| style_descr=="NORTHERN CA"| brand_descr=="TAYLOR CALIFORNIA CELLARS"| style_descr=="CA LC"| style_descr=="LC"| style_descr=="CA TIERRA ROJA"| style_descr=="CA"| style_descr=="CA MODESTO"| style_descr=="CA MONARCH ST"| style_descr=="CA GALLAGHER RS"| style_descr=="CA GVSF"| style_descr=="CA FINNEGANS LAKE"| style_descr=="SOUTHERN CA"| brand_descr=="C.A. WINECRAFT"| brand_descr=="CALIFORNIA 37"| brand_descr=="CALIFORNIA CELLARS"| brand_descr=="BERINGER CALIFORNIA CLCTN"| style_descr=="NORTHERN CA"| upc_descr=="APOTHIC INFERNO CA V RED DDT"| upc_descr=="COCOBON RED CA G RED DDT"| upc_descr=="DBL-DD MRLT CA V RED DDT"| upc_descr=="DGLS HL SV-B CA V WT DDT"| upc_descr=="DGLS HL SHZ CA V RED DDT"| upc_descr=="DGLS HL WT ZN CA V BLS DDT"| upc_descr=="HR-G CB-S CA V RED DDT"| style_descr=="CA PALISADE"| style_descr=="LV CLR MSC CA RS V WT DDT"| style_descr=="MALIBU COAST"| style_descr=="LOS ANGELES COUNTY"| upc_descr=="OK-LV CB-S CA V RED C-B DDT"| upc_descr=="OK-LV MRLT CA V RED BB DDT"| upc_descr=="OK-LV P-GR CA V WT C-B DDT"| upc_descr=="OK-LV P-GR CA V WT DDT"| upc_descr=="PCFCPK CB-S CA V RED C-B DDT"| upc_descr=="PCFCPK CB-S CA V RED DDT"| upc_descr=="PCFCPK CHRD CA V WT DDT"| upc_descr=="PINECROFT CB-S CA V RED DDT"| upc_descr=="PROSPECTOR CB-S CA V RED DDT"| upc_descr=="THR WHS CB-S V RED DDT"| upc_descr=="THR WHS MRLT CA V RED DDT"| style_descr=="CONTRA COSTA COUNTY CCC"| style_descr=="CLINE ZN CA V RED DDT"| style_descr=="CCC RS SELECTION"| style_descr=="YOLO COUNTY"| brand_descr=="NEVADA CITY WINERY"| brand_descr=="NEVADA COUNTY WINE GUILD"| upc_descr=="TISDALE P-N CA V RED DDT", state_appellation := "CALIFORNIA"]

d[wine_appellation != "", state_appellation := ""]   # capturing only if we do not know ava

# =============================================================================
# Origin state information (used for DISTANCE INSTRUMENT)
# =============================================================================
d[, origin_state := ""]
d[wine_appellation=="Alexander_Valley"| wine_appellation=="Anderson_Valley"| wine_appellation=="Arroyo_Grande"| wine_appellation=="Amador County"| wine_appellation=="Arroyo Seco"| wine_appellation=="Atlas Peak"| wine_appellation=="Calistoga"| state_appellation=="CALIFORNIA"| wine_appellation=="Carmel_Valley"| wine_appellation=="Carneros"| wine_appellation=="Central_Coast"| wine_appellation=="Chalk_Hill"| wine_appellation=="Chalone"| wine_appellation=="Cienega_Valley"| wine_appellation=="Clarksburg"| wine_appellation=="Clear_Lake"| wine_appellation=="Cucamonga_Valley"| wine_appellation=="Chiles Valley"| wine_appellation=="Dry_Creek_Valley"| wine_appellation=="Dunnigan_Hills"| wine_appellation=="Eagle_Peak"| wine_appellation=="Edna_Valley"| wine_appellation=="El_Dorado"| wine_appellation=="Fiddletown"| wine_appellation=="Guenoc"| wine_appellation=="High_Valley"| wine_appellation=="Howell_Mountain"| wine_appellation=="Hames Valley"| wine_appellation=="Knights_Valley"| wine_appellation=="Livermore_Valley"| wine_appellation=="Lodi"| wine_appellation=="Lake County"| wine_appellation=="Madera"| wine_appellation=="Mcdowell_Valley"| wine_appellation=="Mendocino"| wine_appellation=="Monterey_County"| wine_appellation=="Napa_Valley"| wine_appellation=="Napa_County"| wine_appellation=="North_Coast"| wine_appellation=="Oakville"| wine_appellation=="Oak Knoll"| wine_appellation=="Paicines"| wine_appellation=="Paso_Robles"| wine_appellation=="Red_Hills_Lake_County"| wine_appellation=="Redwood_Valley"| wine_appellation=="Rockpile"| wine_appellation=="Russian_River_Valley"| wine_appellation=="Rutherford"| wine_appellation=="Saint_Lucia_Highlands"| wine_appellation=="San Antonio"| wine_appellation=="Santa_Barbara_County"| wine_appellation=="Santa_Clara"| wine_appellation=="Santa_Cruz_Mountain"| wine_appellation=="Santa_Maria_Valley"| wine_appellation=="Santa_Ynes_Valley"| wine_appellation=="Shenandoah_Valley_Cal"| wine_appellation=="Solano_County_Green_Valley"| wine_appellation=="Sonoma_Coast"| wine_appellation=="Sonoma_Valley"| wine_appellation=="Sonoma"| wine_appellation=="South_Coast"| wine_appellation=="St_Helena"| wine_appellation=="Sta_Rita_Hills"| wine_appellation=="Suisun_Valley"| wine_appellation=="San Benito"| wine_appellation=="San Bernabe"| wine_appellation=="Stags Leap District"| wine_appellation=="Spring Mountain District"| wine_appellation=="Sierra_Foothills"| wine_appellation=="Tracy_Hills"| wine_appellation=="Yorkville_Highlands"| wine_appellation=="Temecula Valley"| wine_appellation=="Yountville"| wine_appellation=="Potter Valley"| wine_appellation=="San Lucas", origin_state := "CALIFORNIA"]

d[style_descr=="YOLO COUNTY"| brand_descr=="DRY CREEK VINEYARD"| style_descr=="CENTRAL VALLEY"| style_descr=="MC UKIAH VALLEY"| style_descr=="ORANGE COUNTY"| brand_descr=="GALLO FAMILY VINEYARDS TWN VLY"| style_descr=="AMADOR COUNTY"| style_descr=="AMADOR COUNTY COUGAR HILL"| style_descr=="AMADOR COUNTY J & S RS"| style_descr=="AMADOR COUNTY MC"| style_descr=="CALAVERAS COUNTY"| style_descr=="CALAVERAS COUNTY RS"| style_descr=="AM CANYON"| style_descr=="WILDCREEK CANYON"| style_descr=="MOUNTAIN NECTAR"| style_descr=="AMADOR COUNTY OLD VINE RS"| style_descr=="CALAVERAS COUNTY"| style_descr=="CALAVERAS COUNTY RS"| style_descr=="CONTRA COSTA COUNTY"| style_descr=="WILLOW SPRINGS"| style_descr=="RUSTY RIDGE SANTA CLARA COUNTY"| style_descr=="SAN JOAQUIN"| style_descr=="SAN JOAQUIN COUNTY SC"| style_descr=="SANTA ROSA"| style_descr=="SBC RS"| style_descr=="SBC RS SELECTION"| style_descr=="SBC VINTNERS RS"| style_descr=="STANISLAUS COUNTY"| style_descr=="TRINITY COUNTY"| style_descr=="WILLOW SPRINGS", origin_state := "CALIFORNIA"]

d[wine_appellation=="Applegate_Valley"| wine_appellation=="Dundee_Hills"| wine_appellation=="Eola_Hills"| wine_appellation=="Umpqua_Valley"| wine_appellation=="Willamette_Valley"| state_appellation=="OREGON"| wine_appellation=="Mcminnville"| wine_appellation=="Rogue_Valley"| brand_descr=="CALLAHAN RIDGE"| brand_descr=="MONTINORE VINEYARD"| wine_appellation=="Yamhill_Carlton"| wine_appellation=="Chehalem Mountains", origin_state := "OREGON"]

d[wine_appellation=="Walla_Walla"| wine_appellation=="Horse_Heaven_Hills"| wine_appellation=="Wahluke_Slope"| wine_appellation=="Yakima_Valley"| brand_descr=="SNOQUALMIE"| brand_descr=="SNOQUALMIE VINEYARDS"| wine_appellation=="Rattle_Snake_Hills"| wine_appellation=="Lake_Chelan"| wine_appellation=="Red_Mountain"| style_descr=="CHELAN COUNTY"| wine_appellation=="ALOCV", origin_state := "WASHINGTON"]

d[wine_appellation=="Columbia_Valley"| wine_appellation=="Columbia_Gorge"| style_descr=="OREGON WASHINGTON", origin_state := "OREGON_WASH"]

d[state_appellation=="ALABAMA", origin_state := "ALABAMA"]
d[state_appellation=="ARIZONA"| style_descr=="COCHISE COUNTY", origin_state := "ARIZONA"]
d[state_appellation=="ARKANSAS"| wine_appellation=="ALTUS", origin_state := "ARKANSAS"]
d[wine_appellation=="WEST ELKS"| state_appellation=="COLORADO"| wine_appellation=="COLORADO GRAND VALLEY", origin_state := "COLORADO"]
d[state_appellation=="CONNECTICUT", origin_state := "CONNECTICUT"]
d[state_appellation=="GEORGIA"| style_descr=="DUNCAN CREEK", origin_state := "GEORGIA"]
d[wine_appellation=="SNAKE RIVER VALLEY", origin_state := "IDAHO_OREGON"]
d[state_appellation=="IDAHO", origin_state := "IDAHO"]
d[state_appellation=="INDIANA", origin_state := "INDIANA"]
d[style_descr=="INDIANA MICHIGAN", origin_state := "INDIANA_MICHIGAN"]
d[style_descr=="IDAHO WASHINGTON", origin_state := "IDAHO_WASHINGTON"]
d[state_appellation=="ILLINOIS"| style_descr=="GREEN COUNTY"| style_descr=="GREENE COUNTY", origin_state := "ILLINOIS"]
d[style_descr=="CLINTON IOWA"| style_descr=="IOWA"| style_descr=="MUSCATINE"| brand_descr=="MADISON COUNTY WINERY", origin_state := "IOWA"]
d[style_descr=="KENTUCKY", origin_state := "KENTUCKY"]
d[style_descr=="LOUISIANA"| style_descr=="LOUISIANA", origin_state := "LOUISIANA"]
d[state_appellation=="MARYLAND", origin_state := "MARYLAND"]
d[state_appellation=="MINNESOTA"| brand_descr=="CANNON RIVER WINERY", origin_state := "MINNESOTA"]
d[wine_appellation=="LAKE MICHIGAN SHORE"| state_appellation=="MICHIGAN"| wine_appellation=="LEELANAU PENINSULA", origin_state := "MICHIGAN"]
d[state_appellation=="MISSOURI"| wine_appellation=="OZARK HIGHLANDS"| wine_appellation=="AUGUSTA", origin_state := "MISSOURI"]
d[state_appellation=="NEBRASKA", origin_state := "NEBRASKA"]
d[wine_appellation=="LONG ISLAND"| state_appellation=="NEW YORK"| wine_appellation=="FINGER LAKES"| brand_descr=="MARTHA CLARA VINEYARDS", origin_state := "NEW YORK"]
d[state_appellation=="NEW JERSEY"| style_descr=="PINELANDS", origin_state := "NEW JERSEY"]
d[state_appellation=="NEW MEXICO", origin_state := "NEW MEXICO"]
d[style_descr=="NEW HAMPSHIRE", origin_state := "NEW HAMPSHIRE"]
d[state_appellation=="NORTH CAROLINA"| wine_appellation=="YADKIN VALLEY", origin_state := "NORTH CAROLINA"]
d[wine_appellation=="GRAND RIVER VALLEY"| wine_appellation=="ISLE ST GEORGE"| wine_appellation=="OHIO RIVER VALLEY"| state_appellation=="OHIO"| style_descr=="CATABWA ISLAND PROPRIETORS RS", origin_state := "OHIO"]
d[wine_appellation=="LANCASTER VALLEY"| state_appellation=="PENNSYLVANIA"| wine_appellation=="LEHIGH VALLEY"| wine_appellation=="LAKE ERIE", origin_state := "PENNSYLVANIA"]
d[style_descr=="RHODE ISLAND", origin_state := "RHODE ISLAND"]
d[style_descr=="COMMANCHE COUNTY"| state_appellation=="TEXAS"| style_descr=="DE VALERO"| style_descr=="SAN ANTONIO VALLEY"| style_descr=="SAN ANTONIO DE VALERO"| style_descr=="SAN ANTONIO VALLEY"| wine_appellation=="TEXAS HIGH PLAINS"| wine_appellation=="TEXAS HILL COUNTRY", origin_state := "TEXAS"]
d[state_appellation=="VIRGINIA"| style_descr=="JAMES RIVER"| wine_appellation=="MONTICELLO", origin_state := "VIRGINIA"]
d[state_appellation=="WEST VIRGINIA"| wine_appellation=="SHENANDOAH VALLEY", origin_state := "WEST VIRGINIA"]
d[style_descr=="GREATER GREEN BAY AREA"| state_appellation=="WISCONSIN"| brand_descr=="BROWN COUNTY WINERY"| style_descr=="DOOR COUNTY"| style_descr=="GREATER GREEN BAY AREA", origin_state := "WISCONSIN"]
d[style_descr=="CAROLINA", origin_state := "CAROLINA"]   # proxy for North and South Carolina
d[wine_appellation=="SOUTHEASTERN NEW ENGLAND", origin_state := "Conn_RI_MS"]
d[state_appellation=="New Hampshire", origin_state := "New Hampshire"]

# =============================================================================
# FOREIGN COUNTRY ORIGIN
# =============================================================================
d[, Imported := as.integer(rgx(paste0(" ", upc_descr, " "), " (IDT|IM) ")==1)]
d[upc_descr=="YAGO IM RED SG", Imported := 0]

# Categorizing different type of appellation
d[, appellation_label := ""]
d[state_appellation != "", appellation_label := "State_Appellation"]
d[state_appellation == "CALIFORNIA", appellation_label := "California"]
d[wine_appellation != "", appellation_label := "AVA"]
d[product_module_code==5052| product_module_code==5059| type_descr=="IMPORTED", appellation_label := "Imported"]
d[Imported==1 & appellation_label=="", appellation_label := "Imported"]
d[appellation_label=="", appellation_label := "No"]
d[, Imported := NULL]

d[upc_descr=="TISDALE P-N CA V RED DDT", appellation_label := "California"]   # captured imported from module but it is domestic

# separating No label between domestic and no info
d[, match_dom := rgx(paste0(" ", upc_descr, " "), " (DM) ")]
d[match_dom==1 & appellation_label=="No", appellation_label := "US"]
d[product_module_descr=="WINE-DOMESTIC DRY TABLE" & appellation_label=="No", appellation_label := "US"]
d[product_module_descr=="WINE-SWEET DESSERT-DOMESTIC" & appellation_label=="No", appellation_label := "US"]
d[type_descr=="DOMESTIC" & appellation_label=="No", appellation_label := "US"]
d[, match_dom := NULL]

d[, oregon := rgx(paste0(" ", upc_descr, " "), " (OG) ")]
d[oregon==1 & appellation_label=="US", state_appellation := "OREGON"]
d[oregon==1 & appellation_label=="No", state_appellation := "OREGON"]
d[state_appellation=="OREGON", appellation_label := "State_Appellation"]
d[state_appellation=="OREGON", origin_state := "OREGON"]
d[, oregon := NULL]

d[, CAL := rgx(paste0(" ", upc_descr, " "), " (CA) ")]
d[CAL==1 & appellation_label=="US", state_appellation := "CALIFORNIA"]
d[CAL==1 & appellation_label=="No", state_appellation := "CALIFORNIA"]
d[state_appellation=="CALIFORNIA", appellation_label := "California"]
d[, CAL := NULL]
d[state_appellation=="CALIFORNIA", origin_state := "CALIFORNIA"]

# Foreign Country
d[, France      := rgx(paste0(" ", upc_descr, " "), " (FR) ")]
d[, Italy       := rgx(paste0(" ", upc_descr, " "), " (IT) ")]
d[, Chile       := rgx(paste0(" ", upc_descr, " "), " (CHL) ")]
d[, Australia    := rgx(paste0(" ", upc_descr, " "), " (AS) ")]
d[, New_Zealand := rgx(paste0(" ", upc_descr, " "), " (NZ) ")]
d[, Argentina   := rgx(paste0(" ", upc_descr, " "), " (ARG) ")]
d[, Germany     := rgx(paste0(" ", upc_descr, " "), " (GM) ")]
d[, South_Africa := rgx(paste0(" ", upc_descr, " "), " (SA) ")]
d[, Spain       := rgx(paste0(" ", upc_descr, " "), " (S) ")]
d[, Portugal    := rgx(paste0(" ", upc_descr, " "), " (PG) ")]
d[, Moldova     := rgx(paste0(" ", upc_descr, " "), " (MOLDOVA) ")]
d[, Greece      := rgx(paste0(" ", upc_descr, " "), " (GK) ")]
d[, Korea       := rgx(paste0(" ", upc_descr, " "), " (KOREA) ")]
d[, Israel      := rgx(paste0(" ", upc_descr, " "), " (IS) ")]
d[, Brazil      := rgx(paste0(" ", upc_descr, " "), " (BRZL) ")]
d[, Slovenia    := rgx(paste0(" ", upc_descr, " "), " (SV) ")]
d[, Austria     := rgx(paste0(" ", upc_descr, " "), " (ASTRA) ")]
d[, China       := rgx(paste0(" ", upc_descr, " "), " (CHINA) ")]
d[, Lebanon     := rgx(paste0(" ", upc_descr, " "), " (LBN) ")]
d[, Hungary     := rgx(paste0(" ", upc_descr, " "), " (HG) ")]
d[product_module_descr=="WINE-DOMESTIC DRY TABLE", Hungary := 0]
d[, Romania     := rgx(paste0(" ", upc_descr, " "), " (RM) ")]

d[, imp := France+ Italy+ Chile+ Australia+ New_Zealand+ Argentina+ Germany+ South_Africa+ Spain+ Portugal+ Moldova+ Greece+ Korea+ Israel+ Slovenia+ Austria+ China+ Lebanon+ Hungary+ Romania]

d[product_module_code==5053 & appellation_label!="Imported", France := 0]
d[upc_descr=="ALM FR COL V WT DDT"| upc_descr=="ARBR-M P-GR WT PEAR GL FR"| upc_descr=="DCC FR COL TX V WT DDT"| upc_descr=="MCNB-R FR COL MDCN V WT DDT"| upc_descr=="FR VRTS ZN LDI V RED DDT"| upc_descr=="MN FR CHRD CA V WT DDT"| upc_descr=="FR VRTS CB-S LDI V RED DDT"| upc_descr=="IGNK FR COL V WT DDT"| upc_descr=="ARBR-M ZN SGRA GL FR"| upc_descr=="PBWY RDA RHU RBY BLND GL FR"| upc_descr=="FR SRD F-RD IT SANG RED BX IDT"| upc_descr=="PCL FR IT BNC WT IDT", France := 0]

# NB: source line 725 reads `upc_desc` (typo); Stata auto-completes the unambiguous
# prefix to upc_descr, so we use upc_descr here to match Stata's behavior.
d[upc_descr=="GM PA MNTY GRAPE O-F SD D"| upc_descr=="MTL GM NON-ALC CDR GL FR"| upc_descr=="WOOD DUCK GM BRD NYS G RED DDT"| upc_descr=="GM F THRS CB-S NV V RED DDT"| upc_descr=="GM F THRS CB-S NV V RED DDT"| upc_descr=="GM F THRS CHRD CC V WT DDT"| upc_descr=="GM F THRS P-N OG V RED DDT"| upc_descr=="GM F THRS RED PSR G RED DDT"| upc_descr=="MTL GM HRD CDR GL FR 12P"| upc_descr=="MTL GM NON-ALC CDR GL FR 4P"| upc_descr=="DMN GM FR LG GS RED IDT"| upc_descr=="DMN GM FR RED RED IDT"| upc_descr=="GM DL IT P-GR WT IDT", Germany := 0]

d[upc_descr=="CTL BR MLBC CHL V RED DDT"| upc_descr=="HATCH RED CHL GL FR"| upc_descr=="PW GV CHL CB-S VC V RED BB DDT"| upc_descr=="PW GV CHL P-N VC V RED BB DDT"| upc_descr=="CHL RCH ARG MLBC RED IDT", Chile := 0]

d[upc_descr=="AS HP CB-S PSR V RED DDT"| upc_descr=="BITCH AS GRN RED IDT", Australia := 0]

d[upc_descr=="PG D-VGNT IM PRSCO SP"| upc_descr=="PG DLS IT CB-S SANG RED IDT"| upc_descr=="PG IT P-GR WT IDT"| upc_descr=="PG D-VGNT IM MSC SPMT SWT SP"| upc_descr=="RAZA PG VVAAT WT IDT"| upc_descr=="ASTROLABE NZ PVC PG WT IDT"| upc_descr=="PG DR S TMPN RED IDT"| upc_descr=="RPLC IT SPGT PG WT IDT"| upc_descr=="VL PG SLV IT CSPNL RED IDT", Portugal := 0]

d[upc_descr=="GFV ARG PK-MR V BLS IDT" & appellation_label!="Imported", Argentina := 0]

d[upc_descr=="DMNE DU SV FR CVRNY WT IDT"| upc_descr=="STDG SV GWRZT V WT DDT"| upc_descr=="STDG SV VDL BL FGR-LK V WT DDT"| upc_descr=="STDG SV VDL-IC FGR-LK V WT DDT"| upc_descr=="TMW-CO SV BL SV-B C-V V WT DDT"| upc_descr=="SV AZD AGL IT WT CLS WT IDT"| upc_descr=="RC SV IT WT CLS WT IDT"| upc_descr=="IN SITU CHL CB SV SNG RED IDT"| upc_descr=="CASA SV CHL CB-S RS RED IDT"| upc_descr=="CASA SV CHL CRMNR RED IDT"| upc_descr=="CASA SV CHL CRMNRE RS RED IDT"| upc_descr=="CASA SV CHL CU-R CR RED IDT"| upc_descr=="CASA SV CHL SAV GRS WT IDT"| upc_descr=="JOHN DALY SA TLN-CB SV RED IDT"| upc_descr=="RC SV IT RPS RED IDT"| upc_descr=="RC SV IT WT-GG CLS WT IDT"| upc_descr=="RC SV IT RSE BLS IDT", Slovenia := 0]

d[upc_descr=="S FR SAV WT IDT"| upc_descr=="7 S RED CSMTSGPVG RED BB IDT"| upc_descr=="DCD LD CVMD S RMA-WS V RED DDT"| upc_descr=="S K N CB-S NV V RED DDT"| upc_descr=="S K N CHRD NV V WT DDT"| upc_descr=="S K N CHRD V WT DDT"| upc_descr=="S K N MRLT NV V RED DDT"| upc_descr=="S K N RSE-PN NV G BLS DDT"| upc_descr=="S K N SV-B V WT DDT"| upc_descr=="WLMS DB HRD AP S CDR CN FR 4P"| upc_descr=="DMN GGN FR CTE S CHRD WT IDT"| upc_descr=="EC LV S NZ SV-B WT IDT"| upc_descr=="S H W AS SV-B WT IDT"| upc_descr=="S & J PORT RUBY SD I", Spain := 0]

d[upc_descr=="GNR LBN GL FR"| upc_descr=="FCH LBN FR SLMS WT IDT", Lebanon := 0]

d[upc_descr=="CHT RM FR GVR MCFS RED IDT"| brand_descr=="RUDOLF MULLER", Romania := 0]
d[, imp := NULL]

d[, Importing_country := ""]
d[France==1,       Importing_country := "France"]
d[Italy==1,        Importing_country := "Italy"]
d[Chile==1,        Importing_country := "Chile"]
d[Australia==1,    Importing_country := "Australia"]
d[New_Zealand==1,  Importing_country := "New_Zealand"]
d[Argentina==1,    Importing_country := "Argentina"]
d[Germany==1,      Importing_country := "Germany"]
d[South_Africa==1, Importing_country := "South_Africa"]
d[Spain==1,        Importing_country := "Spain"]
d[Portugal==1,     Importing_country := "Portugal"]
d[Moldova==1,      Importing_country := "Moldova"]
d[Greece==1,       Importing_country := "Greece"]
d[Korea==1,        Importing_country := "Korea"]
d[Israel==1,       Importing_country := "Israel"]
d[Brazil==1,       Importing_country := "Brazil"]
d[Slovenia==1,     Importing_country := "Slovenia"]
d[Austria==1,      Importing_country := "Austria"]
d[China==1,        Importing_country := "China"]
d[Lebanon==1,      Importing_country := "Lebanon"]
d[Hungary==1,      Importing_country := "Hungary"]
d[Romania==1,      Importing_country := "Romania"]
d[Importing_country=="" & appellation_label=="Imported", Importing_country := "Other"]
# drop France-Romania (contiguous variable range)
d[, c("France","Italy","Chile","Australia","New_Zealand","Argentina","Germany","South_Africa","Spain","Portugal","Moldova","Greece","Korea","Israel","Brazil","Slovenia","Austria","China","Lebanon","Hungary","Romania") := NULL]

# =============================================================================
# Importing Region (if not conditioned on country, capturing only that country)
# =============================================================================

# IGP
d[, CotesDeGascogne := as.integer(rgx(paste0(" ", upc_descr, " "), " (D-G) ")==1 | rgx(paste0(" ", upc_descr, " "), " (GSCGNE) ")==1)]
d[, Vaucluse := rgx(paste0(" ", upc_descr, " "), " (VDP-DV) ")]
d[, Ardeche := rgx(paste0(" ", upc_descr, " "), " (GRD ARD) ")]
d[, Bugey_Cerdon := rgx(paste0(" ", upc_descr, " "), " (BGY-CRDN) ")]

# I. Bordeaux
# `... if Importing_country=="France"` applies to the whole RHS; non-France -> NA -> set 0
d[, Bordeaux := as.integer(rgx(paste0(" ", upc_descr, " "), " (BDX) ")==1 | rgx(paste0(" ", upc_descr, " "), " (RBDX-MCBFCS) ")==1 | rgx(paste0(" ", upc_descr, " "), " (BS-MFCS) ")==1 | rgx(paste0(" ", upc_descr, " "), " (RBDX-M&CS) ")==1)]
d[Importing_country != "France", Bordeaux := NA_integer_]
d[is.na(Bordeaux), Bordeaux := 0]

d[, Saint_Emilion := as.integer(rgx(paste0(" ", upc_descr, " "), " (ST-E) ")==1 | rgx(paste0(" ", upc_descr, " "), " (LSSTE) ")==1 | rgx(paste0(" ", upc_descr, " "), " (ST-EMLN) ")==1 | rgx(paste0(" ", upc_descr, " "), " (SEGC) ")==1)]

d[, Medoc := rgx(paste0(" ", upc_descr, " "), " (MEDOC) ")]
d[Medoc==1, appellation_label := "Imported"]
d[Medoc==1, Importing_country := "France"]

d[, Cru_Bourgeois := rgx(paste0(" ", upc_descr, " "), " (CRU-B) ")]
d[Cru_Bourgeois==1, Medoc := 1]
d[, Cru_Bourgeois := NULL]

d[, Cotes_De_Bourg := rgx(paste0(" ", upc_descr, " "), " (CDBURG) ")]
d[, Lalande_De_Pomerol := rgx(paste0(" ", upc_descr, " "), " (LDP) ")]
d[Lalande_De_Pomerol==1, Importing_country := "France"]

d[, Graves_Bordeaux := as.integer(rgx(paste0(" ", upc_descr, " "), " (GVDBR) ")==1 | rgx(paste0(" ", upc_descr, " "), " (GVDBW) ")==1 | rgx(paste0(" ", upc_descr, " "), " (GVDB) ")==1 | rgx(paste0(" ", upc_descr, " "), " (GVD-BR) ")==1 | rgx(paste0(" ", upc_descr, " "), " (GVDBW) ")==1 | rgx(paste0(" ", upc_descr, " "), " (GVDBR-MCS) ")==1 | rgx(paste0(" ", upc_descr, " "), " (GRAVES) ")==1)]

d[, Margaux := rgx(paste0(" ", upc_descr, " "), " (MRGX) ")]

d[, Sauternes := rgx(paste0(" ", upc_descr, " "), " (SAUT) ")]
d[Importing_country != "France", Sauternes := NA_integer_]
d[is.na(Sauternes), Sauternes := 0]
d[, Pauillac := rgx(paste0(" ", upc_descr, " "), " (PLC) ")]
d[Importing_country != "France", Pauillac := NA_integer_]
d[is.na(Pauillac), Pauillac := 0]
d[, Saint_Estephe := rgx(paste0(" ", upc_descr, " "), " (ST ESTEPHE) ")]

# II. Burgundy/Bourgogne
d[, Bourgogne := as.integer(rgx(paste0(" ", upc_descr, " "), " (BOUR) ")==1 | rgx(paste0(" ", upc_descr, " "), " (RGVDB) ")==1)]
d[upc_descr=="DMNE-DR CHRD BOUR V WT DDT", Bourgogne := 0]
d[, Burgandy := as.integer(rgx(paste0(" ", upc_descr, " "), " (BRG) ")==1 | rgx(paste0(" ", upc_descr, " "), " (BURG) ")==1)]
d[Importing_country != "France", Burgandy := NA_integer_]
d[is.na(Burgandy), Burgandy := 0]
d[Burgandy==1, Bourgogne := 1]
d[, Burgandy := NULL]
d[brand_descr=="BOURGOGNE", Bourgogne := 1]
d[, Macon_Chardonnay := rgx(paste0(" ", upc_descr, " "), " (MCN CHRD) ")]
d[, Saint_Veran := rgx(paste0(" ", upc_descr, " "), " (ST VRN) ")]
d[, Macon_Villages := as.integer(rgx(paste0(" ", upc_descr, " "), " (MCN BL VLG) ")==1 | rgx(paste0(" ", upc_descr, " "), " (MCN VLG) ")==1)]

d[, Chablis := as.integer(rgx(paste0(" ", upc_descr, " "), " (CHB) ")==1 | rgx(paste0(" ", upc_descr, " "), " (PT-CHB) ")==1)]
d[Importing_country != "France", Chablis := NA_integer_]
d[upc_descr=="SMNT-F IM CHB BR SP", Chablis := 1]
d[upc_descr=="SMNT-F IM CHB BR SP", Importing_country := "France"]
d[is.na(Chablis), Chablis := 0]

d[, La_Grange := rgx(paste0(" ", upc_descr, " "), " (LA GRANGE) ")]

d[, Pouilly_Vinzelles := rgx(paste0(" ", upc_descr, " "), " (PLY VNZLS) ")]
d[, Pouilly_Fuisse := as.integer(rgx(paste0(" ", upc_descr, " "), " (PLY FS) ")==1 | rgx(paste0(" ", upc_descr, " "), " (VDOPF) ")==1)]
d[, Macon_Uchizy := rgx(paste0(" ", upc_descr, " "), " (MCN-UCZY) ")]
d[, Macon_Lugny := as.integer(rgx(paste0(" ", upc_descr, " "), " (MCN LUGNY) ")==1 | rgx(paste0(" ", upc_descr, " "), " (MCN-LGNY) ")==1)]
d[, Chalonnaise := as.integer(rgx(paste0(" ", upc_descr, " "), " (CT-CHLNS) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CHALONNAISE) ")==1)]
d[, Cartone := as.integer(rgx(paste0(" ", brand_descr, " "), " (Aloxe-Corton) ")==1 | rgx(paste0(" ", upc_descr, " "), " (ALX-C) ")==1)]
d[, Marsannay := rgx(paste0(" ", upc_descr, " "), " (MARSANNAY) ")]
d[, Haut_CotesDe_Beaune := rgx(paste0(" ", upc_descr, " "), " (BHUCDB) ")]
d[, Puligny_Montrachet := rgx(paste0(" ", upc_descr, " "), " (PLG MNTRC) ")]
d[, Maconnais := rgx(paste0(" ", upc_descr, " "), " (MCN) ")]
d[Macon_Chardonnay==1| Macon_Villages==1| Macon_Lugny==1| Macon_Uchizy==1, Maconnais := 0]

# III. Provence
d[, CotesDeProvence := as.integer(rgx(paste0(" ", upc_descr, " "), " (CDPR) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CTS-D-PRVNC) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CTS-D-PV) ")==1 | rgx(paste0(" ", upc_descr, " "), " (C-D-P) ")==1)]
d[brand_descr=="CASTELLO DEL POGGIO", CotesDeProvence := 0]
d[upc_descr=="LA RUE CDPR G BLS DDT"| upc_descr=="CTL BR CDPR G BLS DDT", appellation_label := "Imported"]
d[upc_descr=="LA RUE CDPR G BLS DDT"| upc_descr=="CTL BR CDPR G BLS DDT", Importing_country := "France"]

# IV. Rhone
d[, Rhone := as.integer(rgx(paste0(" ", upc_descr, " "), " (C-D-R) ")==1 | rgx(paste0(" ", upc_descr, " "), " (RHE) ")==1 | rgx(paste0(" ", upc_descr, " "), " (RHN) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CDRRCGSM) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CDRGCC) ")==1)]
d[Importing_country != "France", Rhone := NA_integer_]
d[is.na(Rhone), Rhone := 0]
d[, Gigondas := rgx(paste0(" ", upc_descr, " "), " (GIGONDAS) ")]
d[, Tavel := as.integer(rgx(paste0(" ", upc_descr, " "), " (TAVEL) ")==1 | rgx(paste0(" ", upc_descr, " "), " (TVL) ")==1)]
d[, Rasteau := as.integer(rgx(paste0(" ", upc_descr, " "), " (RASTEAU) ")==1 | rgx(paste0(" ", brand_descr, " "), " (RASTEAU) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CVE-DR) ")==1)]
d[, Ventoux := as.integer(rgx(paste0(" ", upc_descr, " "), " (CTE DU VNTX) ")==1 | rgx(paste0(" ", upc_descr, " "), " (VNTX) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CSDV-GSCC) ")==1)]
d[, Costieres_De_Nimes := rgx(paste0(" ", upc_descr, " "), " (CDN) ")]
d[brand_descr=="GEORGES DUBOEUF CHATEAU DE NER", Costieres_De_Nimes := 0]
d[, Crozes_Hermitage := rgx(paste0(" ", upc_descr, " "), " (CRZS HRMTG) ")]
d[, Vacqueyras := as.integer(rgx(paste0(" ", upc_descr, " "), " (VACQUEYRAS) ")==1 | rgx(paste0(" ", upc_descr, " "), " (VQYS) ")==1)]
d[, Rhone_Villages := as.integer(rgx(paste0(" ", upc_descr, " "), " (CDRV) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CDRVGSY) ")==1)]
d[Rhone_Villages==1, Rhone := 1]
d[, Rhone_Villages := NULL]
d[, Luberon := as.integer(rgx(paste0(" ", upc_descr, " "), " (CTE DU LBRN) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CTE DU LBVN) ")==1)]
d[, Chateauneuf_du_pape := as.integer(rgx(paste0(" ", upc_descr, " "), " (CDP) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CDLDP) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CDPGS) ")==1)]
d[brand_descr=="CLOS DE L'ORATOIRE DES PAPES"| brand_descr=="CAVES DES PAPES", Chateauneuf_du_pape := 0]

# V. Beaujolais
d[, Beaujolais := as.integer(rgx(paste0(" ", upc_descr, " "), " (BJ-NVU) ")==1 | rgx(paste0(" ", upc_descr, " "), " (BJ NEU) ")==1 | rgx(paste0(" ", upc_descr, " "), " (BJ) ")==1 | rgx(paste0(" ", upc_descr, " "), " (NOUVEAU) ")==1 | rgx(paste0(" ", upc_descr, " "), " (BJ-V-NU) ")==1 | rgx(paste0(" ", upc_descr, " "), " (NEU) ")==1)]
d[Importing_country != "France", Beaujolais := NA_integer_]
d[is.na(Beaujolais), Beaujolais := 0]
d[, Brouilly := as.integer(rgx(paste0(" ", upc_descr, " "), " (BY) ")==1 & Importing_country=="France")]
d[brand_descr=="CIRCUS BY L'OSTAL CAZES", Brouilly := 0]
d[is.na(Brouilly), Brouilly := 0]
d[, Saint_Amour := rgx(paste0(" ", upc_descr, " "), " (ST AMOUR) ")]
d[, Julienas := rgx(paste0(" ", upc_descr, " "), " (JULIENAS) ")]
d[, Morgon := rgx(paste0(" ", upc_descr, " "), " (MORGON) ")]
d[, Fleurie := rgx(paste0(" ", upc_descr, " "), " (FLRE) ")]

# VI. Loire
d[, Anjou := as.integer(rgx(paste0(" ", upc_descr, " "), " (RSE D-ANJ) ")==1 | rgx(paste0(" ", upc_descr, " "), " (RSE D ANJ) ")==1)]
d[, Muscadet := rgx(paste0(" ", upc_descr, " "), " (MSDT) ")]
d[, Cheverny := rgx(paste0(" ", upc_descr, " "), " (CVRNY) ")]
d[, Vouvray := as.integer(rgx(paste0(" ", upc_descr, " "), " (VVRY) ")==1 | rgx(paste0(" ", upc_descr, " "), " (VY-CH-B) ")==1)]
d[Vouvray==1, Importing_country := "France"]
d[, Sancerre := as.integer(rgx(paste0(" ", upc_descr, " "), " (SNC) ")==1 & Importing_country=="France")]
d[, Pouilly_Fume := rgx(paste0(" ", upc_descr, " "), " (PLY-FM) ")]

# VII. South West
d[, CotesDeDuras := rgx(paste0(" ", upc_descr, " "), " (CTSDDUS) ")]
d[, Cahors := rgx(paste0(" ", upc_descr, " "), " (CAHORS) ")]

# VIII. Champagne
d[, Champgane := rgx(paste0(" ", upc_descr, " "), " (CHM) ")]
d[appellation_label != "Imported", Champgane := NA_integer_]
d[is.na(Champgane), Champgane := 0]
d[Champgane==1, Importing_country := "France"]

# IX. Languedoc
d[, Pic_Saint_Loup := rgx(paste0(" ", upc_descr, " "), " (PSLR) ")]
d[, Picpoul_De_Pinet := as.integer(rgx(paste0(" ", upc_descr, " "), " (PD_PNT) ")==1 | rgx(paste0(" ", upc_descr, " "), " (PCPL DE PNT) ")==1 | rgx(paste0(" ", upc_descr, " "), " (PD-PNT) ")==1)]
d[, Corbieres := rgx(paste0(" ", upc_descr, " "), " (CRBRES) ")]
d[, Cotes_du_Roussillon := rgx(paste0(" ", upc_descr, " "), " (C-D-RSLN) ")]
d[, Cabardes := rgx(paste0(" ", upc_descr, " "), " (CBRDS) ")]
d[, CotesDeRose := rgx(paste0(" ", upc_descr, " "), " (CDRR-GCS) ")]
d[, Minervois := as.integer(rgx(paste0(" ", upc_descr, " "), " (MNR-CBSHZ) ")==1 | rgx(paste0(" ", upc_descr, " "), " (MNR-CHVGR) ")==1)]

# ITALY
# I. Veneto
d[, Ripasso := as.integer(rgx(paste0(" ", upc_descr, " "), " (RPS) ")==1 | rgx(paste0(" ", upc_descr, " "), " (RIPASO) ")==1)]
d[, Amarone := as.integer(rgx(paste0(" ", upc_descr, " "), " (AMARONE) ")==1 | rgx(paste0(" ", upc_descr, " "), " (A-D-V) ")==1)]
d[, Valpolicella := rgx(paste0(" ", upc_descr, " "), " (VLPL) ")]

d[, Bardolino := rgx(paste0(" ", upc_descr, " "), " (BRDLNO) ")]
d[Bardolino==1, Importing_country := "Italy"]

d[, Prosecco := as.integer(rgx(paste0(" ", upc_descr, " "), " (PRSCO) ")==1 | rgx(paste0(" ", upc_descr, " "), " (GRBL-P) ")==1)]
d[Prosecco==1, Importing_country := "Italy"]
d[Prosecco==1, appellation_label := "Imported"]

d[, Soave := rgx(paste0(" ", upc_descr, " "), " (SOAVE) ")]

# II. Piedmont
d[, Piemonte := rgx(paste0(" ", upc_descr, " "), " (PIMTE) ")]
d[, Barolo := rgx(paste0(" ", upc_descr, " "), " (BAROLO) ")]
d[Importing_country=="France", Barolo := 0]
d[, Barbaresco := rgx(paste0(" ", upc_descr, " "), " (BRBRSCO) ")]

d[, BarberaDAlba := rgx(paste0(" ", upc_descr, " "), " (BDA) ")]
d[BarberaDAlba==1, Importing_country := "Italy"]

d[, ASTI := as.integer(rgx(paste0(" ", upc_descr, " "), " (ASTI) ")==1 | rgx(paste0(" ", upc_descr, " "), " (AST) ")==1 | rgx(paste0(" ", upc_descr, " "), " (MSC D ASTI) ")==1)]
d[appellation_label != "Imported", ASTI := 0]
d[ASTI==1 & Importing_country=="Other", Importing_country := "Italy"]
d[Importing_country != "Italy", ASTI := 0]

d[, Malvasia_Di_Casorzo_DAsti := rgx(paste0(" ", upc_descr, " "), " (MDCSZ) ")]

# III. Tuscany
d[, Di_Montepulciano := rgx(paste0(" ", upc_descr, " "), " (V-N-D-M) ")]

d[, RossoDiMontalcino := rgx(paste0(" ", upc_descr, " "), " (RSO-DI-MN) ")]
d[, BrunelloDimontalcino := rgx(paste0(" ", upc_descr, " "), " (BR-D-MN) ")]
d[, Vernaccia_Di_SanGimignano := rgx(paste0(" ", upc_descr, " "), " (VDSG) ")]
d[, Tuscany := as.integer(rgx(paste0(" ", upc_descr, " "), " (TUSCANY) ")==1 | rgx(paste0(" ", upc_descr, " "), " (TSCN) ")==1)]
d[, Toscana := as.integer(rgx(paste0(" ", upc_descr, " "), " (TSCNA) ")==1 | rgx(paste0(" ", upc_descr, " "), " (SANG-D-TS) ")==1)]
d[Tuscany==1, Toscana := 0]

d[, Chianti := as.integer(rgx(paste0(" ", upc_descr, " "), " (CHNT) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CHNTI) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CHNT-S) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CHNTI-SC) ")==1)]
d[Importing_country != "Italy", Chianti := NA_integer_]
d[is.na(Chianti), Chianti := 0]

# IV. Puglia
d[, Salice_Salentino := as.integer(rgx(paste0(" ", upc_descr, " "), " (SLNTO) ")==1 | rgx(paste0(" ", upc_descr, " "), " (SLC-SLTN) ")==1)]
d[Salice_Salentino==1, Importing_country := "Italy"]
d[Salice_Salentino==1, appellation_label := "Imported"]
d[, Gravia := rgx(paste0(" ", upc_descr, " "), " (GRAVINA) ")]

# V. Abruzzo
d[, Abruzzo := as.integer(rgx(paste0(" ", upc_descr, " "), " (MDA) ")==1 | rgx(paste0(" ", upc_descr, " "), " (ARBZ) ")==1 | rgx(paste0(" ", upc_descr, " "), " (TRB D ABRZ ) ")==1 | rgx(paste0(" ", upc_descr, " "), " (MNTPL) ")==1)]
d[Importing_country != "Italy", Abruzzo := NA_integer_]
d[is.na(Abruzzo), Abruzzo := 0]
d[upc_descr=="CALDORA IT TRB D ABRZ WT IDT", Abruzzo := 1]

# VI. Lombardi
d[, Sangue_Di_Giuda := as.integer(rgx(paste0(" ", upc_descr, " "), " (SANG D-GD) ")==1 | rgx(paste0(" ", upc_descr, " "), " (SNG D-GD) ")==1)]
d[, Pavia := rgx(paste0(" ", style_descr, " "), " (PROVINCIA DI PAVIA) ")]

# VII. Venezie
d[, Dell_Venezie := rgx(paste0(" ", upc_descr, " "), " (DL VNT) ")]

# VIII. Lazio
d[, Frascati := rgx(paste0(" ", upc_descr, " "), " (FRSCT) ")]

# IX. Emilia IGT
d[, Emilia := rgx(paste0(" ", upc_descr, " "), " (DLL EM) ")]

# X. Emilia Romagna
d[, Romagna := as.integer(rgx(paste0(" ", upc_descr, " "), " (SNG-DR) ")==1 | rgx(paste0(" ", upc_descr, " "), " (CDIROM) ")==1)]

# Umbria
d[, Orvieto := rgx(paste0(" ", upc_descr, " "), " (ORVT CLSC) ")]

# Portugal
# I. DAO
d[, Dao := rgx(paste0(" ", upc_descr, " "), " (DAO) ")]

# II. Vinho Verde
d[, Vinho_Verde := as.integer(rgx(paste0(" ", upc_descr, " "), " (VNHO VERDE) ")==1 | rgx(paste0(" ", upc_descr, " "), " (VV) ")==1)]
d[Importing_country != "Portugal", Vinho_Verde := NA_integer_]
d[upc_descr=="GRINALDA VNHO VERDE KT", Vinho_Verde := 1]
d[is.na(Vinho_Verde), Vinho_Verde := 0]
d[Vinho_Verde==1, Importing_country := "Portugal"]
d[Vinho_Verde==1, appellation_label := "Imported"]

d[, Alvarinho := rgx(paste0(" ", upc_descr, " "), " (ALVARINHO) ")]

# III. Madeira
d[, Madeira := rgx(paste0(" ", upc_descr, " "), " (MADEIRA) ")]
d[type_descr=="MALMSEY", Madeira := 1]
d[Madeira==1 & appellation_label=="Imported", Importing_country := "Portugal"]
d[appellation_label != "Imported", Madeira := 0]

# V. Alentejo
d[, Alentejo := as.integer(rgx(paste0(" ", upc_descr, " "), " (ALENTEJO) ")==1 | rgx(paste0(" ", upc_descr, " "), " (ALNTJNO) ")==1)]

# VI. Douro
d[, Douro := rgx(paste0(" ", upc_descr, " "), " (DOURO) ")]
d[Importing_country != "Portugal", Douro := NA_integer_]
d[is.na(Douro), Douro := 0]

d[, Port := as.integer(rgx(paste0(" ", upc_descr, " "), " (PORTO) ")==1 | rgx(paste0(" ", upc_descr, " "), " (PORT RUBY) ")==1 | rgx(paste0(" ", upc_descr, " "), " (PORTO RUBY) ")==1 | rgx(paste0(" ", upc_descr, " "), " (PORTO TWNY) ")==1 | rgx(paste0(" ", upc_descr, " "), " (PORTA) ")==1 | rgx(paste0(" ", upc_descr, " "), " (PORT) ")==1)]
d[type_descr=="COLHEITA"| type_descr=="FINE TAWNY"| type_descr=="OLD TAWNY"| type_descr=="FINE TAWNY"| type_descr=="FULL RUBY"| type_descr=="RUBY"| type_descr=="RUBY PORTO"| type_descr=="RUBY RESERVA"| type_descr=="RUBY SEC"| type_descr=="TAWNY"| type_descr=="RUBY SWEET RED"| type_descr=="TAWNY"| type_descr=="TAWNY DEMI SEC MEDIUM DRY"| type_descr=="TWNY RED"| type_descr=="TAWNY RESERVE"| type_descr=="TAWNY SPECIAL RESERVE"| type_descr=="TAWNY SWEET RED", Port := 1]
d[brand_descr=="PORTA VITA"| brand_descr=="PORTA SOLE", Port := 0]
d[upc_descr=="S & J PORT TWNY SD I", Importing_country := "Portugal"]
d[appellation_label != "Imported", Port := 0]
d[Port==1, Importing_country := "Portugal"]

# VII.
d[, Bairada := rgx(paste0(" ", upc_descr, " "), " (BAIRRADA) ")]

# SPAIN
d[, Jumilla := rgx(paste0(" ", brand_descr, " "), " (JUMILLA) ")]

# Catalunya
d[, Catalonia := rgx(paste0(" ", upc_descr, " "), " (CAVA) ")]
d[upc_descr=="LINEAS DM CAVA WT SP", Catalonia := 0]
d[Catalonia==1 & appellation_label=="Imported", Importing_country := "Spain"]
d[, Priorat := rgx(paste0(" ", upc_descr, " "), " (PRIORAT) ")]

# Rioja
d[, Rioja := as.integer(rgx(paste0(" ", upc_descr, " "), " (RJA) ")==1 | rgx(paste0(" ", upc_descr, " "), " (RIOJA) ")==1)]
d[brand_descr=="LA RIOJA ALTA, S.A.", Rioja := 1]
d[brand_descr=="RIOJA VEGA"| brand_descr=="RIOJA BORDON", Rioja := 1]

d[, Aragon := rgx(paste0(" ", upc_descr, " "), " (D-ARGN) ")]

# Town in Andalucia
d[, Manzanilla := rgx(paste0(" ", upc_descr, " "), " (MNZNL) ")]
d[type_descr=="MANZANILLA"| type_descr=="MANZANILLA EXTRA DRY"| type_descr=="MANZANILLA RESERVA", Manzanilla := 1]
d[Manzanilla==1, Importing_country := "Spain"]

# Mostly produced in Andalucia (gen Sherry_Andalucia=1 if ... -> else missing -> 0)
d[, Sherry_Andalucia := NA_integer_]
d[type_descr=="AMONTILLADO"| type_descr=="AMONTILLADO DRY RESERVA"| type_descr=="AMONTILLADO MEDIUM"| type_descr=="AMONTILLADO MEDIUM DRY"| type_descr=="FINO AMONTILLADO"| type_descr=="FINO"| type_descr=="FINO DRY"| type_descr=="FINO PALE DRY SPECIAL RESERVE"| type_descr=="FINO SUPERIOR"| type_descr=="OLOROSO"| type_descr=="OLOROSO DON NUNO DRY RESERVA"| type_descr=="OLOROSO FULL DRY"| type_descr=="OLOROSO SWEET", Sherry_Andalucia := 1]
d[is.na(Sherry_Andalucia), Sherry_Andalucia := 0]
d[Sherry_Andalucia==1, Importing_country := "Spain"]

# Town in Castilla
d[, Toro := as.integer(rgx(paste0(" ", upc_descr, " "), " (TDTORO) ")==1 | rgx(paste0(" ", upc_descr, " "), " (TORO) ")==1 | rgx(paste0(" ", upc_descr, " "), " (SDTG) ")==1 | rgx(paste0(" ", upc_descr, " "), " (SAN DE TORO) ")==1)]
d[upc_descr=="EL TORO FLACO IM SG", Toro := 0]
d[Importing_country=="Italy", Toro := 0]

d[Brouilly==1, Beaujolais := 0]
d[upc_descr=="CHT CHB FR SEGC MCSCF RED IDT", Chablis := 0]

# =============================================================================
# Size
# =============================================================================
d[, size_category := ""]
d[size1_units=="LI", size_category := "bulk"]
d[size1_amount==750 & size1_units=="ML", size_category := "bottle"]
d[size1_code_uc==39701 & size1_units=="ML", size_category := "bottle"]
d[size1_amount<750 & size1_units=="ML", size_category := "small"]
d[size1_amount==12 & size1_units=="OZ", size_category := "small"]
d[size1_amount==567 & size1_units=="OZ", size_category := "bulk"]
d[size1_amount==1 & size1_units=="CT", size_category := "bottle"]   # assuming 750ml

d <- d[!(size_category == "small")]

# =============================================================================
# Importing region
# =============================================================================
d[, Importing_region := ""]

# France
d[Anjou==1, Importing_region := "Anjou"]
d[Beaujolais==1| Brouilly==1| Saint_Amour==1| Julienas==1| Morgon==1| Fleurie==1, Importing_region := "Beaujolais"]
d[Bordeaux==1| Saint_Emilion==1| Cotes_De_Bourg==1| Lalande_De_Pomerol==1| Margaux==1| Sauternes==1| Pauillac==1| Saint_Estephe==1| Medoc==1| Graves_Bordeaux==1, Importing_region := "Bordeaux"]
d[Bourgogne==1| La_Grange==1| Chalonnaise==1| Marsannay==1| Haut_CotesDe_Beaune==1| Puligny_Montrachet==1, Importing_region := "Bourgogne"]
d[Macon_Chardonnay==1| Saint_Veran==1| Macon_Villages==1| Pouilly_Vinzelles==1| Pouilly_Fuisse==1| Macon_Uchizy==1| Macon_Lugny==1| Maconnais==1, Importing_region := "Maconnais"]
d[Chablis==1, Importing_region := "Chablis"]
d[Champgane==1, Importing_region := "Champgane"]
d[CotesDeProvence==1, Importing_region := "Provence"]
d[Pic_Saint_Loup==1| Picpoul_De_Pinet==1| Corbieres==1| Cotes_du_Roussillon==1| Cabardes==1| CotesDeRose==1, Importing_region := "Languedoc"]
d[Minervois==1, Importing_region := "Minervois"]
d[Rhone==1| Gigondas==1| Rasteau==1| Costieres_De_Nimes==1| Crozes_Hermitage==1| Vacqueyras==1| Tavel==1, Importing_region := "Rhone"]
d[Ventoux==1, Importing_region := "Ventoux"]
d[Luberon==1, Importing_region := "Luberon"]
d[Chateauneuf_du_pape==1, Importing_region := "Chateauneuf_du_pape"]
d[Cheverny==1| Sancerre==1| Pouilly_Fume==1, Importing_region := "Loire"]
d[Muscadet==1, Importing_region := "Muscadet"]
d[Vouvray==1, Importing_region := "Vouvray"]
d[CotesDeDuras==1| Cahors==1| CotesDeGascogne==1, Importing_region := "South_West_France"]
d[Bugey_Cerdon==1| Vaucluse==1| Ardeche==1, Importing_region := "Rem_France"]

# Italy
d[ASTI==1, Importing_region := "Asti"]
d[Malvasia_Di_Casorzo_DAsti==1, Importing_region := "Asti"]
d[Piemonte==1| Barolo==1| Barbaresco==1| BarberaDAlba==1, Importing_region := "Piedmont"]
d[Di_Montepulciano==1| RossoDiMontalcino==1| BrunelloDimontalcino==1| Vernaccia_Di_SanGimignano==1, Importing_region := "Tuscany"]
d[Tuscany==1| Toscana==1, Importing_region := "Tuscany_IGT"]
d[Chianti==1, Importing_region := "Chianti"]
d[Salice_Salentino==1, Importing_region := "Salice Salentino"]
d[Sangue_Di_Giuda==1, Importing_region := "Sangue Di Giuda"]
d[Pavia==1, Importing_region := "Pavia"]
d[Amarone==1, Importing_region := "Amarone"]
d[Valpolicella==1| Ripasso==1, Importing_region := "Valpolicella"]
d[Bardolino==1, Importing_region := "Bardolino"]
d[Prosecco==1, Importing_region := "Prosecco"]
d[Dell_Venezie==1, Importing_region := "Dell_Venezie"]
d[Frascati==1, Importing_region := "Frascati"]
d[Emilia==1| Romagna==1, Importing_region := "Emilia Romagna"]
d[Orvieto==1, Importing_region := "Orvieto"]
d[Gravia==1, Importing_region := "Rem_Italy"]
d[Abruzzo==1, Importing_region := "Abruzzo"]
d[Soave==1, Importing_region := "Soave"]

# Portugal
d[Dao==1, Importing_region := "Dao"]
d[Vinho_Verde==1, Importing_region := "Vinho_Verde"]
d[Madeira==1, Importing_region := "Madeira"]
d[Douro==1| Port==1, Importing_region := "Douro"]
d[Bairada==1| Alentejo==1, Importing_region := "Rem_Portugal"]

# Spain
d[Jumilla==1| Aragon==1, Importing_region := "Rem_Spain"]
d[Catalonia==1| Priorat==1, Importing_region := "Catalunya"]
d[Rioja==1, Importing_region := "Rioja"]
d[Sherry_Andalucia==1| Manzanilla==1, Importing_region := "Andalucia"]
d[Toro==1, Importing_region := "Toro_Castilla"]

# drop CotesDeGascogne - Toro (contiguous variable range from CotesDeGascogne..Toro)
.range_start <- which(names(d) == "CotesDeGascogne")
.range_end   <- which(names(d) == "Toro")
d[, names(d)[.range_start:.range_end] := NULL]

# Final fixes / overlap corrections
d[upc_descr=="ZAB UCG NC V RED DDT", appellation_label := "AVA"]
d[upc_descr=="ZAB UCG NC V RED DDT", Importing_country := ""]
d[upc_descr=="FZ MRLT CA V RED BB DDT" & style_descr=="SOUTHEASTERN AUSTRALIA", appellation_label := "Imported"]
d[upc_descr=="FZ CHRD CA V WT BB DDT" & style_descr=="SOUTHEASTERN AUSTRALIA", appellation_label := "Imported"]
d[upc_descr=="FZ MRLT CA V RED BB DDT" & style_descr=="SOUTHEASTERN AUSTRALIA", state_appellation := ""]
d[upc_descr=="FZ CHRD CA V WT BB DDT" & style_descr=="SOUTHEASTERN AUSTRALIA", state_appellation := ""]
d[upc_descr=="FZ CHRD CA V WT BB DDT" & style_descr=="SOUTHEASTERN AUSTRALIA", Importing_country := "Australia"]
d[upc_descr=="FZ MRLT CA V RED BB DDT" & style_descr=="SOUTHEASTERN AUSTRALIA", Importing_country := "Australia"]
d[Importing_country=="South_Africa", Importing_country := "South Africa"]
d[Importing_country=="New_Zealand", Importing_country := "New Zealand"]

# proper() -> title case
d[, origin_state := stata_proper(origin_state)]
d[, wine_appellation := stata_proper(wine_appellation)]
d[, state_appellation := stata_proper(state_appellation)]
d[wine_appellation=="Lehigh Va", wine_appellation := "Lehigh Valley"]
d[wine_appellation=="Walla Walla Washington", wine_appellation := "Walla Walla Valley"]
d[upc_descr=="GFVTV IT P-N RED IDT"| upc_descr=="SNTA BRBRA IT AZLENDA WT IDT", origin_state := ""]
d[upc_descr=="GFV ARG PK-MR V BLS IDT" & style_descr != "CA", origin_state := ""]

# Drop unused panelist / product-attribute columns.
# Handles two contiguous variable-range drops via name positions:
#   member_1_birth - member_7_employment
.drop_named <- c("flavor_code","flavor_descr","form_code","form_descr","formula_code","formula_descr",
  "container_code","container_descr","salt_content_code","salt_content_descr","organic_claim_code",
  "organic_claim_descr","usda_organic_seal_code","usda_organic_seal_descr","common_consumer_name_code",
  "common_consumer_name_descr","strength_code","strength_descr","scent_code","scent_descr","dosage_code",
  "dosage_descr","gender_code","gender_descr","target_skin_condition_code","target_skin_condition_descr",
  "use_code","use_descr","department_code","department_descr","dataset_found_uc","household_code",
  "store_code_uc","store_zip3","ym","household_income","household_size","type_of_residence",
  "household_composition","age_and_presence_of_children","female_head_age","male_head_age",
  "male_head_employment","female_head_employment","male_head_education","female_head_education",
  "male_head_occupation","female_head_occupation","male_head_birth","female_head_birth","marital_status",
  "race","hispanic_origin","panelist_zip_code","kitchen_appliances","tv_items",
  "household_internet_connection","wic_indicator_current","wic_indicator_ever_notcurrent")
.drop_named2 <- c("method_of_payment_cd","panelist_zipcd","wic_indicator_ever_not_current")
.mstart <- which(names(d) == "member_1_birth")
.mend   <- which(names(d) == "member_7_employment")
.drop_range <- if (length(.mstart) && length(.mend)) names(d)[.mstart:.mend] else character(0)
.drop_all <- unique(c(.drop_named, .drop_range, .drop_named2))
.drop_all <- intersect(.drop_all, names(d))
d[, (.drop_all) := NULL]

# Write R-native .rds (avoids haven's strict label validation; the all-R pipeline
# uses .rds intermediates — see _config.R save_dt/read_dt).
save_dt(d, file.path(ROOT, "data/derived/r_1_capture_geographic_origin.dta"))

# ***This is end of this do file***
