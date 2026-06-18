# =============================================================================
# 02_varietal_and_supplements.R
# R port of: DoFile/2_capture_varietal_and_other_supplement_data.do
# Extracts varietal info from upc description, makes brand names uniform,
# and merges supplement data (CPI, excise tax, distance, country ranking,
# population). Faithful line-by-line translation of the Stata .do file.
# =============================================================================

ROOT <- "/Users/lade.10/Library/CloudStorage/Dropbox/Work/RESEARCH/wine-demand-repo"
.PROJ_ROOT <- ROOT  # pin root so _config.R resolves to this repo (not the script dir)
source(file.path(ROOT, "code/_config.R"))
ROOT <- "/Users/lade.10/Library/CloudStorage/Dropbox/Work/RESEARCH/wine-demand-repo"

# ---- load input (Stata intermediate from step 1) ----------------------------
d <- read_dt(file.path(ROOT, "data/derived/r_1_capture_geographic_origin.dta"))   # prior step output (.rds)

# Helper to mirror Stata: regexm(" "+upc_descr+" ", " (TOK) ")
ud <- function() paste0(" ", d$upc_descr, " ")

# =============================================================================
# WINE TYPE
# =============================================================================
d[, match_red   := rgx(ud(), " (RED) ")]
d[, match_white := rgx(ud(), " (WT) ")]
d[, match_blush := rgx(ud(), " (BLS) ")]
d[, match_rose  := rgx(ud(), " (RSE) ")]
d[brand_descr == "WOLF BLASS" & match_red == 1,   match_blush := 0L]
d[brand_descr == "WOLF BLASS" & match_white == 1, match_blush := 0L]

d[match_blush == 1 | match_rose == 1, match_red := 0L]
d[match_blush == 1 | match_rose == 1, match_white := 0L]

# fix overlap of type where brand names contain 'red'/'white' words
d[upc_descr %in% c("KIONA RED MT CHN BL V WT DDT","RED BICYCLETTE FR CHRD WT IDT",
                   "RED BUD VGNR V WT DDT","RED CANYON NZ SV-B WT IDT",
                   "RED DMND CHRD WSH V WT DDT","RED IMPULSE CHRD C-V V WT DDT",
                   "RED KNOT AS CHRD WT IDT","RED LION CHRD CA V WT DDT",
                   "RED MUD AS CHRD WT IDT","RED ROVER CHRD V WT DDT",
                   "RED THEORY CHRD C-V V WT DDT","HA-MKT GM RES AL WT RED"),
  match_red := 0L]

d[upc_descr %in% c("S-HM WT ZN CA V RED DDT","SAN CAMILLUS IT P-GR WT RED",
                   "WT OAK VNYD CB-S NV V RED DDT","WT OAK VNYD CB-S V RED DDT",
                   "WT OAK VNYD MRLT NV V RED DDT","WT OAK VNYD ZN V RED DDT",
                   "WT PL ARG CB-S RED IDT","WT PL ARG MLBC CB RED IDT"),
  match_white := 0L]

d[match_rose == 1, match_blush := 0L]

d[, wine_type := ""]
d[match_red == 1,   wine_type := "RED"]
d[match_white == 1, wine_type := "WHITE"]
d[match_blush == 1 | match_rose == 1, wine_type := "OTHER"]
d[wine_type == "", wine_type := "OTHER"]

d[, c("match_red","match_white") := NULL]

# RESTRICTION 1: only table wine categorized red/white; rest is Other
d[product_module_descr != "WINE-DOMESTIC DRY TABLE" &
  product_module_descr != "WINE-IMPORTED DRY TABLE" &
  product_module_descr != "WINE-KOSHER TABLE", wine_type := "OTHER"]

# =============================================================================
# VARIETALS
# =============================================================================
d[, Cab_Sau := as.integer(rgx(ud()," (CB-S) ")==1 | rgx(ud()," (LM-CS) ")==1)]
d[, Chard   := as.integer(rgx(ud()," (CHRD) ")==1 | rgx(ud()," (QN-R-CHRD) ")==1)]
d[upc_descr == "LITTLE ROO AS CHD WT IDT", Chard := 1L]
d[, Pinot_G := as.integer(rgx(ud()," (P-GR) ")==1 | rgx(ud()," (P-GRS) ")==1)]
d[, Riesling := as.integer(rgx(ud()," (RES) ")==1 | rgx(ud()," (RES-SP) ")==1 | rgx(ud()," (P-G-R-S) ")==1)]
d[, Malbec := rgx(ud()," (MLBC) ")]
d[, Zinfandel := as.integer(rgx(ud()," (ZN) ")==1 | rgx(ud()," (TB-ZN) ")==1)]
d[, Merlot := as.integer(rgx(ud()," (MRLT) ")==1 | rgx(ud()," (MBMRLT) ")==1)]
d[, Moscato := as.integer(rgx(ud()," (MSC) ")==1 | rgx(ud()," (MSCT) ")==1 | rgx(ud()," (MSCTL) ")==1 | rgx(ud()," (AR-MS) ")==1 | rgx(ud()," (MUSCATO) ")==1)]
d[, Pinot_N := as.integer(rgx(ud()," (P-N) ")==1 | rgx(ud()," (G2S-PN) ")==1)]
d[, Bonarda := rgx(ud()," (BNRDA) ")]
d[, Sauvignon_Blanc := rgx(ud()," (SV-B) ")]
d[, Macabeo := as.integer(rgx(ud()," (MACABEO) ")==1 | rgx(ud()," (VIURA) ")==1)]
d[, Tempranillo := rgx(ud()," (TMPN) ")]
d[, Pinotage := rgx(ud()," (PNTG) ")]
d[, SHZ := as.integer(rgx(ud()," (SHZ) ")==1 | rgx(ud()," (SHR) ")==1 | rgx(ud()," (RS-SZ) ")==1)]
d[, Syrah := as.integer(rgx(ud()," (SYR) ")==1 | rgx(ud()," (SYR-ME) ")==1)]
d[, Primitivo := rgx(ud()," (PRMTV) ")]
d[, Soave := rgx(ud()," (SOAVE) ")]
d[, Mencia := as.integer(rgx(ud()," (MNC) ")==1 | rgx(ud()," (MENCIA) ")==1)]
d[, Xarel := rgx(ud()," (XAREL) ")]
# Torrontes: only if appellation_label=="Imported"; else missing -> 0
d[, Torrontes := ifelse(appellation_label == "Imported", rgx(ud()," (TRNTS) "), NA_integer_)]
d[is.na(Torrontes), Torrontes := 0L]
d[, Monastrell := as.integer(rgx(ud()," (MNSTRL) ")==1 | rgx(ud()," (RD-MSTL) ")==1)]
d[, Liebfraumilch := as.integer(rgx(ud()," (LIEB) ")==1 | rgx(ud()," (MDNA-LIEB) ")==1)]

# Sangiovese: only if imported from Italy
d[, Sangiovese := ifelse(Importing_country == "Italy", rgx(ud()," (SANG) "), NA_integer_)]
d[is.na(Sangiovese), Sangiovese := 0L]
d[, Bukettraube := rgx(ud()," (BKTR) ")]
d[, Godello := rgx(ud()," (GODELLO) ")]
d[, Nebbiolo := rgx(ud()," (NBLO) ")]
d[, Vermentio := ifelse(Importing_country == "Italy", rgx(ud()," (VRMNT) "), NA_integer_)]
d[is.na(Vermentio), Vermentio := 0L]
d[, Rosato := ifelse(Importing_country == "Italy", rgx(ud()," (RSTO) "), NA_integer_)]
d[is.na(Rosato), Rosato := 0L]
d[, Rosso := ifelse(Importing_country == "Italy", rgx(ud()," (RSO) "), NA_integer_)]
d[is.na(Rosso), Rosso := 0L]
d[, Verdejo := ifelse(Importing_country == "Spain",
                      as.integer(rgx(ud()," (Verdejo) ")==1 | rgx(ud()," (VERDEJO) ")==1 | rgx(ud()," (VRDJOVU) ")==1),
                      NA_integer_)]
d[is.na(Verdejo), Verdejo := 0L]
d[, Pinot_Nero := rgx(ud()," (PNT NERO) ")]
d[, Albarinho := as.integer(rgx(ud()," (ALVARINHO) ")==1 | rgx(ud()," (ALBRNO) ")==1)]
d[, Inzolia := ifelse(Importing_country == "Italy", rgx(ud()," (INZOLIA) "), NA_integer_)]
d[is.na(Inzolia), Inzolia := 0L]
d[, Dornfelder := ifelse(Importing_country == "Germany",
                         as.integer(rgx(ud()," (DNR) ")==1 | rgx(ud()," (DNRFLDR) ")==1),
                         NA_integer_)]
d[is.na(Dornfelder), Dornfelder := 0L]
d[, Negro_Amaro := rgx(ud()," (NG AMARO) ")]
d[, Lambrusco := ifelse(Importing_country == "Italy",
                        as.integer(rgx(ud()," (LMBRSC) ")==1 | rgx(ud()," (LMB) ")==1),
                        NA_integer_)]
d[is.na(Lambrusco), Lambrusco := 0L]
d[, Tarrango := ifelse(Importing_country == "Australia", rgx(ud()," (TARRANGO) "), NA_integer_)]
d[is.na(Tarrango), Tarrango := 0L]
d[, Malavasia := rgx(ud()," (MLVSA) ")]
d[, Teroldego := rgx(ud()," (TEROLDEGO) ")]
d[, Barbera := rgx(ud()," (BARB) ")]
d[, Trebbiano := rgx(ud()," (TRB) ")]
d[, NeroDAvola := rgx(ud()," (NR-DA) ")]
d[, Merlot_NeroDAvola := rgx(ud()," (MRLT NR-DA) ")]
d[Merlot_NeroDAvola == 1, Merlot := 0L]
d[Merlot_NeroDAvola == 1, NeroDAvola := 0L]
d[, Falanghina := rgx(ud()," (FALANGHINA) ")]
d[, Grillo := rgx(ud()," (GRILLO) ")]
# Gamay: regexm(...) & appellation_label=="Imported"
d[, Gamay := as.integer(rgx(ud()," (GMY) ")==1 & appellation_label == "Imported")]
d[is.na(Gamay), Gamay := 0L]
d[, Gruner_Veltliner := rgx(ud()," (GRNR-V) ")]
d[, Piesporter_Michelsberg := rgx(ud()," (PST MC) ")]
d[, Grenache := as.integer(rgx(ud()," (GRNCHA) ")==1 | rgx(ud()," (GRN) ")==1 | rgx(ud()," (VRN-GH) ")==1)]

# ---- Blends ----
# Pinot Gris Chardonnay
d[, PinotG_Chard1 := as.integer(rgx(ud()," (P-GR) ")==1 & rgx(ud()," (CHRD) ")==1)]
d[, PinotG_Chard2 := as.integer(rgx(ud()," (P-GRS) ")==1 & rgx(ud()," (CHRD) ")==1)]
d[, PinotG_Chard3 := rgx(ud()," (CHRD&P-GR) ")]
d[, PinotG_Chard4 := rgx(ud()," (P-G-C) ")]
d[, PinotG_Chard := PinotG_Chard1 + PinotG_Chard2 + PinotG_Chard3 + PinotG_Chard4]
d[PinotG_Chard == 1, Chard := 0L]
d[PinotG_Chard == 1, Pinot_G := 0L]
d[, c("PinotG_Chard1","PinotG_Chard2","PinotG_Chard3","PinotG_Chard4") := NULL]

# Bonarda Blend
d[, Bonarda_Merlot := rgx(ud()," (B-MLT) ")]
d[, Bonarda_Malbec := rgx(ud()," (MLBC&BNRDA) ")]
d[, Bonarda_Syrah := rgx(ud()," (SYR BNRDA) ")]
d[Bonarda_Syrah == 1, Syrah := 0L]
d[Bonarda_Syrah == 1, Bonarda := 0L]

# SauBlanc Semilon: (SV-B & SML) | (SB-SM)  -- Stata precedence: & binds before |
d[, SauBlanc_Semilon := as.integer((rgx(ud()," (SV-B) ")==1 & rgx(ud()," (SML) ")==1) | rgx(ud()," (SB-SM) ")==1)]
d[, Semilon := rgx(ud()," (SML) ")]
d[brand_descr %in% c("SMALL GULLY MR. BLACK'S CNCCTN","SMALL WONDERS","SAMUEL SMITH"), Semilon := 0L]
d[SauBlanc_Semilon == 1, Sauvignon_Blanc := 0L]
d[SauBlanc_Semilon == 1, Semilon := 0L]

d[, Merlot_Pinotage := rgx(ud()," (MRLT PNTG) ")]
d[Merlot_Pinotage == 1, Pinotage := 0L]
d[Merlot_Pinotage == 1, Merlot := 0L]

d[, SHZ_Pinotage := as.integer(rgx(ud()," (PNTG) ")==1 & rgx(ud()," (SHZ) ")==1)]
d[SHZ_Pinotage == 1, Pinotage := 0L]
d[SHZ_Pinotage == 1, SHZ := 0L]

d[, Monas_Tempr := rgx(ud()," (MTL-TMP) ")]

d[, Pinot_Binaco := rgx(ud()," (P-BNC) ")]
d[, Syr_Gr_Mourvedre := rgx(ud()," (SG-MV) ")]

d[, Sang_Cab_Merlot := rgx(ud()," (S-CB-S&MRLT) ")]
d[, Mns_Syrah := rgx(ud()," (MO-S) ")]

d[, Ugni_blanc := rgx(ud()," (UGNI BL) ")]

d[, Sang_Merlot := rgx(ud()," (SANG MRLT) ")]
d[Sang_Merlot == 1, Sangiovese := 0L]
d[Sang_Merlot == 1, Merlot := 0L]

# Cab-Merlot
d[, Cab_Merlo1 := as.integer(rgx(ud()," (CB-M) ")==1 | rgx(ud()," (MCS) ")==1 | rgx(ud()," (RBDX-M&CS) ")==1)]
d[, Cab_Merlo2 := as.integer(rgx(ud()," (MRLT) ")==1 & rgx(ud()," (CB-S) ")==1)]
d[, Cab_Merlo3 := rgx(ud()," (CBSVM) ")]
d[, Cab_Merlo := Cab_Merlo1 + Cab_Merlo2 + Cab_Merlo3]
d[, c("Cab_Merlo1","Cab_Merlo2","Cab_Merlo3") := NULL]
d[Cab_Merlo == 1, Cab_Sau := 0L]
d[Cab_Merlo == 1, Merlot := 0L]

# Cab_SHZ
d[, Cab_SHZ1 := as.integer(rgx(ud()," (CB) ")==1 & rgx(ud()," (SHZ) ")==1)]
d[, Cab_SHZ2 := as.integer(rgx(ud()," (CB-S) ")==1 & rgx(ud()," (SHZ) ")==1)]
d[, Cab_SHZ3 := rgx(ud()," (SHZ CBSVM) ")]
d[, Cab_SHZ4 := rgx(ud()," (CS-SZ) ")]
d[, Cab_SHZ5 := rgx(ud()," (MNR-CBSHZ) ")]
d[, Cab_SHZ6 := rgx(ud()," (PVCSS) ")]
d[, Cab_SHZ := Cab_SHZ1 + Cab_SHZ2 + Cab_SHZ3 + Cab_SHZ4 + Cab_SHZ5 + Cab_SHZ6]
d[Cab_SHZ == 1, Cab_Sau := 0L]
d[Cab_SHZ == 1, SHZ := 0L]
d[, c("Cab_SHZ1","Cab_SHZ2","Cab_SHZ3","Cab_SHZ4","Cab_SHZ5","Cab_SHZ6") := NULL]

# Cab Syrah
d[, Cab_Syr1 := as.integer(rgx(ud()," (CB-S) ")==1 & rgx(ud()," (SYR) ")==1)]
d[, Cab_Syr2 := as.integer(rgx(ud()," (CB) ")==1 & rgx(ud()," (SYR) ")==1)]
d[, Cab_Syr := Cab_Syr1 + Cab_Syr2]
d[Cab_Syr == 1, Syrah := 0L]
d[Cab_Syr == 1, Cab_Sau := 0L]
d[, c("Cab_Syr1","Cab_Syr2") := NULL]

d[, Cab_Syr_Mouv := as.integer(rgx(ud()," (CB-S) ")==1 & rgx(ud()," (SY-M) ")==1)]

# Cab_TMPN
d[, Cab_TMPN := as.integer(rgx(ud()," (CB-S) ")==1 & rgx(ud()," (TMPN) ")==1)]
d[Cab_TMPN == 1, Cab_Sau := 0L]
d[Cab_TMPN == 1, Tempranillo := 0L]

# Cab Malbec
d[, Cab_Malbec1 := as.integer(rgx(ud()," (MLBC) ")==1 & rgx(ud()," (CB-S) ")==1)]
d[, Cab_Malbec2 := rgx(ud()," (CB MLBC) ")]
d[, Cab_Malbec := Cab_Malbec1 + Cab_Malbec2]
d[Cab_Malbec == 1, Cab_Sau := 0L]
d[Cab_Malbec == 1, Malbec := 0L]
d[, c("Cab_Malbec1","Cab_Malbec2") := NULL]

# SHZ_GRE
d[, SHZ_GRE1 := as.integer(rgx(ud()," (SHZ) ")==1 & rgx(ud()," (GRN) ")==1)]
d[, SHZ_GRE2 := as.integer(rgx(ud()," (SYR) ")==1 & rgx(ud()," (GRN) ")==1)]
d[, SHZ_GRE3 := rgx(ud()," (CDRVGSY) ")]
d[, SHZ_GRE4 := rgx(ud()," (GRN&SHZ) ")]
d[, SHZ_GRE5 := rgx(ud()," (G&S) ")]
d[, SHZ_GRE6 := rgx(ud()," (GR-S) ")]
d[, SHZ_GRE7 := rgx(ud()," (LC-GSY) ")]
d[, SHZ_GRE8 := rgx(ud()," (RDGRNSYR) ")]
d[, SHZ_GRE9 := rgx(ud()," (GRNSYR) ")]
d[, SHZ_GRE := SHZ_GRE1 + SHZ_GRE2 + SHZ_GRE3 + SHZ_GRE4 + SHZ_GRE5 + SHZ_GRE6 + SHZ_GRE7 + SHZ_GRE8 + SHZ_GRE9]
d[SHZ_GRE == 1, SHZ := 0L]
d[SHZ_GRE == 1, Grenache := 0L]
d[, c("SHZ_GRE1","SHZ_GRE2","SHZ_GRE3","SHZ_GRE4","SHZ_GRE5","SHZ_GRE6","SHZ_GRE7","SHZ_GRE8","SHZ_GRE9") := NULL]

# SHZ Merlot
d[, SHZ_Merlot := as.integer(rgx(ud()," (SHZ) ")==1 & rgx(ud()," (MRLT) ")==1)]
d[SHZ_Merlot == 1, SHZ := 0L]
d[SHZ_Merlot == 1, Merlot := 0L]

# Syrah-Mourvedre
d[, Syrah_Mourvedre := as.integer(rgx(ud()," (SY-M) ")==1 | rgx(ud()," (SYR-M) ")==1)]

# SHZ MALBEC
d[, SHZ_MLBC1 := as.integer(rgx(ud()," (SHZ) ")==1 & rgx(ud()," (MLBC) ")==1)]
d[, SHZ_MLBC2 := as.integer(rgx(ud()," (SYR) ")==1 & rgx(ud()," (MLBC) ")==1)]
d[, SHZ_Malbec := SHZ_MLBC1 + SHZ_MLBC2]
d[SHZ_Malbec == 1, SHZ := 0L]
d[SHZ_Malbec == 1, Malbec := 0L]
d[, c("SHZ_MLBC1","SHZ_MLBC2") := NULL]

# SHZ Pinot
d[, SHZ_PinotN := as.integer(rgx(ud()," (SYR) ")==1 & rgx(ud()," (P-N) ")==1)]
d[SHZ_PinotN == 1, SHZ := 0L]
d[SHZ_PinotN == 1, Pinot_N := 0L]

# SHZ TMPN
d[, SHZ_TMPN1 := rgx(ud()," (SHZ TMPN) ")]
d[, SHZ_TMPN2 := rgx(ud()," (SYR TMPN) ")]
d[, SHZ_TMPN3 := rgx(ud()," (TMPN SHZ) ")]
d[, SHZ_TMPN := SHZ_TMPN1 + SHZ_TMPN2 + SHZ_TMPN3]
d[SHZ_TMPN == 1, SHZ := 0L]
d[SHZ_TMPN == 1, Tempranillo := 0L]
d[, c("SHZ_TMPN1","SHZ_TMPN2","SHZ_TMPN3") := NULL]

# SHZ ZIN
d[, Zin_SHZ := as.integer(rgx(ud()," (ZN) ")==1 & rgx(ud()," (SHZ) ")==1)]
d[Zin_SHZ == 1, Zinfandel := 0L]
d[Zin_SHZ == 1, SHZ := 0L]

# GRE TMPN
d[, Gren_TMPN := as.integer(rgx(ud()," (GRNCHA) ")==1 & rgx(ud()," (TMPN) ")==1)]
d[Gren_TMPN == 1, Grenache := 0L]
d[Gren_TMPN == 1, Tempranillo := 0L]

# GRE Merlot
d[, Merlot_Gre1 := as.integer(rgx(ud()," (MRLT) ")==1 & rgx(ud()," (GRN) ")==1)]
d[, Merlot_Gre2 := rgx(ud()," (GRN&MRLT) ")]
d[, Merlot_Gre := Merlot_Gre1 + Merlot_Gre2]
d[Merlot_Gre == 1, Merlot := 0L]
d[Merlot_Gre == 1, Grenache := 0L]
d[, c("Merlot_Gre1","Merlot_Gre2") := NULL]

# Merlot Pinot Noir
d[, Merlot_PN := as.integer(rgx(ud()," (MRLT) ")==1 & rgx(ud()," (P-N) ")==1)]
d[Merlot_PN == 1, Pinot_N := 0L]
d[Merlot_PN == 1, Merlot := 0L]

# Merlot Malbec
d[, Merlot_MLBC := as.integer(rgx(ud()," (MRLT) ")==1 & rgx(ud()," (MLBC) ")==1)]
d[Merlot_MLBC == 1, Merlot := 0L]
d[Merlot_MLBC == 1, Malbec := 0L]

# TMPN Malbec
d[, TMPN_MLBC := rgx(ud()," (TMPN MLBC) ")]
d[TMPN_MLBC == 1, Malbec := 0L]
d[TMPN_MLBC == 1, Tempranillo := 0L]

# Zin Grenache
d[, Zin_Gre := as.integer(rgx(ud()," (ZN) ")==1 & rgx(ud()," (GRN) ")==1)]
d[Zin_Gre == 1, Zinfandel := 0L]
d[Zin_Gre == 1, Grenache := 0L]

# Zin Chard
d[, Zin_Chardonnay := as.integer(rgx(ud()," (ZN) ")==1 & rgx(ud()," (CHRD) ")==1)]
d[Zin_Chardonnay == 1, Zinfandel := 0L]
d[Zin_Chardonnay == 1, Chard := 0L]

# Zin Moscato
d[, Zin_Moscato := as.integer(rgx(ud()," (ZN) ")==1 & rgx(ud()," (MSC) ")==1)]
d[Zin_Moscato == 1, Zinfandel := 0L]
d[Zin_Moscato == 1, Moscato := 0L]

# Sau Chard: (SV-B & CHRD) | (CHRD&SV-B)
d[, SB_CHARD := as.integer((rgx(ud()," (SV-B) ")==1 & rgx(ud()," (CHRD) ")==1) | rgx(ud()," (CHRD&SV-B) ")==1)]
d[SB_CHARD == 1, Sauvignon_Blanc := 0L]
d[SB_CHARD == 1, Chard := 0L]

d[, Champgane_other := ifelse(Importing_country != "France", rgx(ud()," (CHM) "), NA_integer_)]
d[is.na(Champgane_other), Champgane_other := 0L]

d[, Chard_Chm := as.integer(rgx(ud()," (CHRD) ")==1 & rgx(ud()," (CHM) ")==1)]
d[Chard_Chm == 1, Chard := 0L]
d[Chard_Chm == 1, Champgane_other := 0L]

d[, MSC_CHM := as.integer(rgx(ud()," (CHM) ")==1 & rgx(ud()," (MSC) ")==1)]
d[MSC_CHM == 1, Moscato := 0L]
d[MSC_CHM == 1, Champgane_other := 0L]

d[, PinotG_CHM := as.integer(rgx(ud()," (CHM) ")==1 & rgx(ud()," (P-GR) ")==1)]
d[PinotG_CHM == 1, Pinot_G := 0L]
d[PinotG_CHM == 1, Champgane_other := 0L]

d[, Zin_CHM := as.integer(rgx(ud()," (CHM) ")==1 & rgx(ud()," (ZN) ")==1)]
d[Zin_CHM == 1, Zinfandel := 0L]
d[Zin_CHM == 1, Champgane_other := 0L]

d[, Carmenere := as.integer(rgx(ud()," (CRMNR) ")==1 | rgx(ud()," (CRMNRE) ")==1)]

d[, Concord1 := rgx(ud()," (CONCORD) ")]
d[, Concord2 := rgx(ud()," (CON) ")]
d[, Concord := Concord1 + Concord2]
d[brand_descr == "CON CARNE", Concord := 0L]
d[, c("Concord1","Concord2") := NULL]

d[, Meritage := rgx(ud()," (MRTG) ")]

d[, Meri_Chard := as.integer(rgx(ud()," (MRTG) ")==1 & rgx(ud()," (CHRD) ")==1)]
d[Meri_Chard == 1, Meritage := 0L]
d[Meri_Chard == 1, Chard := 0L]

d[, Gewur := rgx(ud()," (GWRZT) ")]

d[, P_Sirah := as.integer(rgx(ud()," (P-SRH) ")==1 | rgx(ud()," (P-SYR) ")==1)]

d[, Red_Blend := rgx(ud()," (RB) ")]
d[, White_Blend := rgx(ud()," (WB) ")]
d[, CAVA := rgx(ud()," (CAVA) ")]

d[, Chianti_other := ifelse(Importing_country != "Italy",
                            as.integer(rgx(ud()," (CHNT) ")==1 | rgx(ud()," (CHNTI) ")==1 | rgx(ud()," (CHNT-S) ")==1 | rgx(ud()," (CHNTI-SC) ")==1),
                            NA_integer_)]
d[is.na(Chianti_other), Chianti_other := 0L]

d[, Port := rgx(ud()," (PORT) ")]

d[, Psrh_Port := as.integer(rgx(ud()," (PORT) ")==1 & rgx(ud()," (P-SRH) ")==1)]
d[Psrh_Port == 1, P_Sirah := 0L]
d[Psrh_Port == 1, Port := 0L]

d[, Sherry := rgx(ud()," (SHRY) ")]

d[, Burgandy_other := as.integer(rgx(ud()," (BRG) ")==1 & Importing_country != "France")]
d[is.na(Burgandy_other), Burgandy_other := 0L]

d[, Chablis_other := ifelse(Importing_country != "France",
                            as.integer(rgx(ud()," (CHB) ")==1 | rgx(ud()," (PT-CHB) ")==1),
                            NA_integer_)]
d[is.na(Chablis_other), Chablis_other := 0L]

d[, Rhine := ifelse(Importing_country != "France", rgx(ud()," (RHN) "), NA_integer_)]
d[is.na(Rhine), Rhine := 0L]

d[, Muscadine := rgx(ud()," (MSCDN) ")]
d[, Marsala := rgx(ud()," (MRSL) ")]

d[, Chard_PN := as.integer(rgx(ud()," (P-N) ")==1 & rgx(ud()," (CHRD) ")==1)]
d[Chard_PN == 1, Chard := 0L]
d[Chard_PN == 1, Pinot_N := 0L]

d[, Pinot_Blanc := rgx(ud()," (P-BL) ")]
d[, Norton := rgx(ud()," (NORTON) ")]

d[, CheninB_Chrd := rgx(ud()," (C-B-C) ")]
d[, Chenin_blanc := as.integer(rgx(ud()," (CHN BL) ")==1 | rgx(ud()," (VY-CH-B) ")==1 | rgx(ud()," (P-CHN BL) ")==1 | rgx(ud()," (P-CHN) ")==1 | rgx(ud()," (CH-B) ")==1)]

d[, CheninB_SauB := as.integer(rgx(ud()," (SV-B) ")==1 & rgx(ud()," (CHN) ")==1 & rgx(ud()," (BL) ")==1)]
d[CheninB_SauB == 1, Chenin_blanc := 0L]
d[CheninB_SauB == 1, Sauvignon_Blanc := 0L]

d[upc_descr == "NEDERBURG SA LYR-SBCC WT IDT", Chenin_blanc := 1L]

d[, Grenache_PGR := rgx(ud()," (GR-P-GR) ")]

d[, Chard_Semilon := as.integer(rgx(ud()," (CHRD SML) ")==1 | rgx(ud()," (SML CHRD) ")==1)]
d[Chard_Semilon == 1, Chard := 0L]
d[Chard_Semilon == 1, Semilon := 0L]

d[, Cab_Primitvo := rgx(ud()," (CB-S PRMTV) ")]
d[Cab_Primitvo == 1, Cab_Sau := 0L]
d[Cab_Primitvo == 1, Primitivo := 0L]

d[, Cab_Sang := rgx(ud()," (CB-S SANG) ")]
d[Cab_Sang == 1, Cab_Sau := 0L]
d[Cab_Sang == 1, Sangiovese := 0L]

d[, CabS_Merlot_CabF := as.integer(rgx(ud()," (MCFCS) ")==1 | rgx(ud()," (BS-MCSCF) ")==1 | rgx(ud()," (RMCSCFM) ")==1 | rgx(ud()," (RMCFCS) ")==1 | rgx(ud()," (MCSCF) ")==1 | rgx(ud()," (MCBFC) ")==1 | rgx(ud()," (ST-MCFCS) ")==1 | rgx(ud()," (CSMCF) ")==1 | rgx(ud()," (CBG-CSM) ")==1 | rgx(ud()," (CFCSM) ")==1)]

d[, Mouv_C_GN := rgx(ud()," (M-CB-GN) ")]
d[, Viognier := rgx(ud()," (VGNR) ")]

d[, Chard_Viognier := as.integer(rgx(ud()," (CHRD VGNR) ")==1 | rgx(ud()," (CHRD&VGNR) ")==1 | rgx(ud()," (CHRD&VGNR) ")==1 | rgx(ud()," (MNR-CHVGR) ")==1)]
d[Chard_Viognier == 1, Viognier := 0L]
d[Chard_Viognier == 1, Chard := 0L]

d[, Shz_Viognier := rgx(ud()," (SHZ VGNR) ")]
d[Shz_Viognier == 1, SHZ := 0L]
d[Shz_Viognier == 1, Viognier := 0L]

d[, Gre_Syr_Carig_Cinsault := as.integer(rgx(ud()," (CSDV-GSCC) ")==1 | rgx(ud()," (RGSCC) ")==1 | rgx(ud()," (RGNSCC) ")==1)]
d[, Gre_Syr_Carigan := as.integer(rgx(ud()," (SGC) ")==1 | rgx(ud()," (GCS) ")==1)]
d[, Syr_Carigan := rgx(ud()," (SY-CGN) ")]
d[, Gre_Pn := rgx(ud()," (PN&GY) ")]
d[, Gre_Cab_Syr_Mouv := rgx(ud()," (CSGSMV) ")]
d[, Gre_Syr_Mouv := rgx(ud()," (GSM) ")]
d[brand_descr == "GOSSAMER BAY" & wine_type == "WHITE", Gre_Syr_Mouv := 0L]

d[, Jacquere := rgx(ud()," (WT-JQ) ")]
d[, Gre_Carig := rgx(ud()," (GRN CRIG) ")]
d[, Semilon_Sauv := as.integer(rgx(ud()," (SML SAV) ")==1 | rgx(ud()," (SAV SML) ")==1)]
d[Semilon_Sauv == 1, Semilon := 0L]
d[, TSMR := rgx(ud()," (TSMR) ")]

d[, Carignan := rgx(ud()," (CARIGNAN) ")]
d[, Syr_rose := as.integer(rgx(ud()," (SYR) ")==1 & rgx(ud()," (RSE) ")==1)]
d[Syr_rose == 1, SHZ := 0L]
d[, Sauvignon := ifelse(Importing_country == "France",
                        as.integer(rgx(ud()," (SAV) ")==1 | rgx(ud()," (ED-SUV) ")==1),
                        NA_integer_)]

d[, Chard_Sauv := as.integer(rgx(ud()," (CHRD SAV) ")==1 | rgx(ud()," (CHRDSBV) ")==1)]
d[Chard_Sauv == 1, Chard := 0L]
d[Chard_Sauv == 1, Sauvignon := 0L]

d[, Cab_France := rgx(ud()," (CB FC) ")]

d[, Shz_Mer_CabS := as.integer(rgx(ud()," (SHZ MRLT CB-S) ")==1 | rgx(ud()," (CB SHZ MRLT) ")==1 | rgx(ud()," (SHZ CB-M) ")==1)]
d[Shz_Mer_CabS == 1, SHZ := 0L]
d[Shz_Mer_CabS == 1, Cab_Sau := 0L]
d[Shz_Mer_CabS == 1, Merlot := 0L]

d[, Domina := rgx(ud()," (DOMINA) ")]

d[, Syr_Temp := as.integer(rgx(ud()," (SYR TMPN) ")==1 | rgx(ud()," (TMPN SYR) ")==1 | rgx(ud()," (TMPN SHZ) ")==1)]
d[Syr_Temp == 1, SHZ := 0L]
# Stata source: `replace TMPN=0 if Syr_Temp==1` references an undefined var TMPN.
# It does not exist in the Stata output (no TMPN column), so we make it a true no-op.

d[, Temp_Cbs := as.integer(rgx(ud()," (TMPN CB-S) ")==1 | rgx(ud()," (CB-S TMPN) ")==1 | rgx(ud()," (TMPN CB) ")==1 | rgx(ud()," (TMPN CB-S) ")==1 | rgx(ud()," (CB-S TMPN) ")==1)]
d[Temp_Cbs == 1, Cab_Sau := 0L]
d[Temp_Cbs == 1, Tempranillo := 0L]

d[, Temp_Grn := as.integer(rgx(ud()," (GRNCHA TMPN) ")==1 | rgx(ud()," (TP-GRC) ")==1 | rgx(ud()," (TP-GRC) ")==1 | rgx(ud()," (TMPN GRNCHA) ")==1)]
d[Temp_Grn == 1, Grenache := 0L]
d[Temp_Grn == 1, Tempranillo := 0L]

d[, CabS_Gn_Sb := rgx(ud()," (GRN CB SAV) ")]
d[CabS_Gn_Sb == 1, Sauvignon := 0L]
d[CabS_Gn_Sb == 1, Cab_Sau := 0L]
d[CabS_Gn_Sb == 1, Grenache := 0L]

d[, Merlot_Temp := rgx(ud()," (MRLT TMPN) ")]
d[Merlot_Temp == 1, Merlot := 0L]
d[Merlot_Temp == 1, Tempranillo := 0L]

d[, Cab_Shz_Mstrl := rgx(ud()," (SHZ CB MNSTRL) ")]
d[Cab_Shz_Mstrl == 1, Cab_Sau := 0L]
d[Cab_Shz_Mstrl == 1, SHZ := 0L]
d[Cab_Shz_Mstrl == 1, Monastrell := 0L]

d[, Graciano := rgx(ud()," (GRCN) ")]
d[, Tmpn_Graciano := rgx(ud()," (TMPN-GRCN) ")]
d[Tmpn_Graciano == 1, Tempranillo := 0L]
d[Tmpn_Graciano == 1, Graciano := 0L]

d[, Crnz_Tmpn_CBS := rgx(ud()," (CRNZ TMPN CB-S) ")]
d[Crnz_Tmpn_CBS == 1, Tempranillo := 0L]
d[Crnz_Tmpn_CBS == 1, Cab_Sau := 0L]

# check and fix blended varietals - any upc with more than one varietal
var_mix_cols <- c("Cab_Sau","Chard","Pinot_G","Riesling","Malbec","Zinfandel","Merlot","Moscato",
  "Pinot_N","Bonarda","Sauvignon_Blanc","Macabeo","Tempranillo","Pinotage","SHZ","Syrah","Primitivo",
  "Soave","Mencia","Xarel","Torrontes","Monastrell","Liebfraumilch","Sangiovese","Bukettraube","Godello",
  "Nebbiolo","Vermentio","Rosato","Rosso","Verdejo","Pinot_Nero","Albarinho","Inzolia","Dornfelder",
  "Negro_Amaro","Lambrusco","Tarrango","Malavasia","Teroldego","Barbera","Trebbiano","NeroDAvola",
  "Merlot_NeroDAvola","Falanghina","Grillo","Gamay","Gruner_Veltliner","Piesporter_Michelsberg","Grenache",
  "PinotG_Chard","Bonarda_Merlot","Bonarda_Malbec","Bonarda_Syrah","SauBlanc_Semilon","Semilon",
  "Merlot_Pinotage","SHZ_Pinotage","Monas_Tempr","Pinot_Binaco","Syr_Gr_Mourvedre","Sang_Cab_Merlot",
  "Mns_Syrah","Ugni_blanc","Sang_Merlot","Cab_Merlo","Cab_SHZ","Cab_Syr","Cab_Syr_Mouv","Cab_TMPN",
  "Cab_Malbec","SHZ_GRE","SHZ_Merlot","Syrah_Mourvedre","SHZ_Malbec","SHZ_PinotN","SHZ_TMPN","Zin_SHZ",
  "Gren_TMPN","Merlot_Gre","Merlot_PN","Merlot_MLBC","TMPN_MLBC","Zin_Gre","Zin_Chardonnay","Zin_Moscato",
  "SB_CHARD","Champgane_other","Chard_Chm","MSC_CHM","PinotG_CHM","Zin_CHM","Carmenere","Concord","Meritage",
  "Meri_Chard","Gewur","P_Sirah","Red_Blend","White_Blend","CAVA","Chianti_other","Port","Psrh_Port","Sherry",
  "Burgandy_other","Chablis_other","Rhine","Muscadine","Marsala","Chard_PN","Pinot_Blanc","Norton",
  "CheninB_Chrd","Chenin_blanc","CheninB_SauB","Grenache_PGR","Chard_Semilon","Cab_Primitvo","Cab_Sang",
  "CabS_Merlot_CabF","Viognier","Chard_Viognier","Shz_Viognier","Gre_Syr_Carig_Cinsault","Gre_Syr_Carigan",
  "Syr_Carigan","Gre_Pn","Gre_Cab_Syr_Mouv","Gre_Syr_Mouv","Jacquere","Gre_Carig","Semilon_Sauv","TSMR",
  "Carignan","Syr_rose","Sauvignon","Chard_Sauv","Cab_France","Shz_Mer_CabS","Domina","Syr_Temp","Temp_Cbs",
  "Temp_Grn","CabS_Gn_Sb","Merlot_Temp","Cab_Shz_Mstrl","Graciano","Tmpn_Graciano","Crnz_Tmpn_CBS")
# NOTE: Stata's var_mix sum treats missing as missing; but Sauvignon may be NA (France condition).
# Stata adds (.) producing missing var_mix -> the replace conditions (==2/==3) then are false.
# Replicate by NA-propagating row sum (na.rm=FALSE).
d[, var_mix := rowSums(as.matrix(.SD), na.rm = FALSE), .SDcols = var_mix_cols]

# red blends combined with other varietal - keep as other var
d[var_mix == 2 & Red_Blend == 1, Red_Blend := 0L]
d[var_mix == 2 & White_Blend == 1, White_Blend := 0L]
d[var_mix == 3 & Red_Blend == 1, Red_Blend := 0L]
d[var_mix == 3 & White_Blend == 1, White_Blend := 0L]
d[, var_mix := NULL]

# =============================================================================
# Assign Varietal label
# =============================================================================
d[, Varietal := ""]
d[Cab_Sau == 1, Varietal := "Cabernet Sauvignon"]
d[Chard == 1, Varietal := "Chardonnay"]
d[Pinot_G == 1, Varietal := "Pinot Grigio"]
d[Riesling == 1, Varietal := "Riesling"]
d[Malbec == 1, Varietal := "Malbec"]
d[Zinfandel == 1, Varietal := "Zinfandel"]
d[Merlot == 1, Varietal := "Merlot"]
d[Moscato == 1, Varietal := "Moscato"]
d[Pinot_N == 1, Varietal := "Pinot Noir"]
d[Bonarda == 1, Varietal := "Bonarda"]
d[Sauvignon_Blanc == 1, Varietal := "Sauvignon Blanc"]
d[Macabeo == 1, Varietal := "Macabeo"]
d[Tempranillo == 1, Varietal := "Tempranillo"]
d[Pinotage == 1, Varietal := "Pinotage"]
d[SHZ == 1, Varietal := "Shiraz"]
d[Syrah == 1, Varietal := "Syrah"]
d[Primitivo == 1, Varietal := "Primitivo"]
d[Soave == 1, Varietal := "Soave"]
d[Mencia == 1, Varietal := "Mencia"]
d[Xarel == 1, Varietal := "Xarel"]
d[Torrontes == 1, Varietal := "Torrontes"]
d[Monastrell == 1, Varietal := "Monastrell"]
d[Liebfraumilch == 1, Varietal := "Liebfraumilch"]
d[Sangiovese == 1, Varietal := "Sangiovese"]
d[Bukettraube == 1, Varietal := "Bukettraube"]
# line 525: replace Varietal="Syr_Gr_Mourvedre" if Syr_Gr_Mourvedre  (no ==1; truthy means !=0 & !missing)
d[Syr_Gr_Mourvedre != 0 & !is.na(Syr_Gr_Mourvedre), Varietal := "Syr_Gr_Mourvedre"]
d[Godello == 1, Varietal := "Godello"]
d[Nebbiolo == 1, Varietal := "Nebbiolo"]
d[Vermentio == 1, Varietal := "Vermentio"]
d[Rosato == 1, Varietal := "Rosato"]
d[Rosso == 1, Varietal := "Rosso"]
d[Verdejo == 1, Varietal := "Verdejo"]
d[Pinot_Nero == 1, Varietal := "Pinot_Nero"]
d[Albarinho == 1, Varietal := "Albarinho"]
d[Inzolia == 1, Varietal := "Inzolia"]
d[Dornfelder == 1, Varietal := "Dornfelder"]
d[Negro_Amaro == 1, Varietal := "Negro_Amaro"]
d[Lambrusco == 1, Varietal := "Lambrusco"]
d[Tarrango == 1, Varietal := "Tarrango"]
d[Malavasia == 1, Varietal := "Malavasia"]
d[Teroldego == 1, Varietal := "Teroldego"]
d[Barbera == 1, Varietal := "Barbera"]
d[Trebbiano == 1, Varietal := "Trebbiano"]
d[NeroDAvola == 1, Varietal := "NeroDAvola"]
d[Merlot_NeroDAvola == 1, Varietal := "Merlot_NeroDAvola"]
d[Falanghina == 1, Varietal := "Falanghina"]
d[Ugni_blanc == 1, Varietal := "Ugni_blanc"]
d[Grillo == 1, Varietal := "Grillo"]
d[Gamay == 1, Varietal := "Gamay"]
d[Gruner_Veltliner == 1, Varietal := "Gruner_Veltliner"]
d[Piesporter_Michelsberg == 1, Varietal := "Piesporter_Michelsberg"]
d[Grenache == 1, Varietal := "Grenache"]
d[PinotG_Chard == 1, Varietal := "PinotG_Chard"]
d[Bonarda_Merlot == 1, Varietal := "Bonarda_Merlot"]
d[Bonarda_Malbec == 1, Varietal := "Bonarda_Malbec"]
d[Bonarda_Syrah == 1, Varietal := "Bonarda_Syrah"]
d[SauBlanc_Semilon == 1, Varietal := "SauBlanc_Semilon"]
d[Semilon == 1, Varietal := "Semilon"]
d[Merlot_Pinotage == 1, Varietal := "Merlot_Pinotage"]
d[SHZ_Pinotage == 1, Varietal := "SHZ_Pinotage"]
d[Monas_Tempr == 1, Varietal := "Monas_Tempr"]
d[Pinot_Binaco == 1, Varietal := "Pinot_Binaco"]
d[Syr_Gr_Mourvedre == 1, Varietal := "Syr_Gr_Mourvedre"]
d[Sang_Cab_Merlot == 1, Varietal := "Sang_Cab_Merlot"]
d[Mns_Syrah == 1, Varietal := "Mns_Syrah"]
d[Ugni_blanc == 1, Varietal := "Ugni_blanc"]
d[Sang_Merlot == 1, Varietal := "Sang_Merlot"]
d[Cab_Merlo == 1, Varietal := "Cabernet_Merlot"]
d[Sang_Merlot == 1, Varietal := "Sang_Merlot"]
d[Cab_SHZ == 1 | Cab_Syr == 1, Varietal := "Cabernet_Shiraz"]
d[Cab_Syr_Mouv == 1, Varietal := "Cab_Syr_Mouv"]
d[Cab_TMPN == 1, Varietal := "Cab_TMPN"]
d[Cab_Malbec == 1, Varietal := "Cab_Malbec"]
d[SHZ_GRE == 1, Varietal := "Shiraz_Greenache"]
d[SHZ_Merlot == 1, Varietal := "Shiraz_Merlot"]
d[Syrah_Mourvedre == 1, Varietal := "Syrah_Mourvedre"]
d[SHZ_Malbec == 1, Varietal := "Shiraz_Malbec"]
d[SHZ_PinotN == 1, Varietal := "Shiraz_PinotN"]
d[SHZ_TMPN == 1, Varietal := "Shiraz_Tmpn"]
d[Zin_SHZ == 1, Varietal := "Zinfandel_Shiraz"]
d[Gren_TMPN == 1, Varietal := "Gren_TMPN"]
d[Merlot_Gre == 1, Varietal := "Merlot_Grenache"]
d[Merlot_PN == 1, Varietal := "Merlot_PinotN"]
d[Merlot_MLBC == 1, Varietal := "Merlot_Malbec"]
d[TMPN_MLBC == 1, Varietal := "TMPN_MLBC"]
d[Zin_Gre == 1, Varietal := "Zinfandel_Grenache"]
d[Zin_Chardonnay == 1, Varietal := "Zin_Chardonnay"]
d[Zin_Moscato == 1, Varietal := "Zin_Moscato"]
d[SB_CHARD == 1, Varietal := "SB_Chard"]
d[Champgane_other == 1, Varietal := "Champgane_other"]
d[Chard_Chm == 1, Varietal := "Chard_Champgane"]
d[MSC_CHM == 1, Varietal := "MSC_CHM"]
d[PinotG_CHM == 1, Varietal := "PG_Champgane"]
d[Zin_CHM == 1, Varietal := "Zin_Champgane"]
d[Carmenere == 1, Varietal := "Carmenere"]
d[Concord == 1, Varietal := "Concord"]
d[Meritage == 1, Varietal := "Meriatge"]
d[Meri_Chard == 1, Varietal := "Meritge_Chard"]
d[Gewur == 1, Varietal := "Gewur"]
d[P_Sirah == 1, Varietal := "P_Sirah"]
d[Red_Blend == 1, Varietal := "Red Blend"]
d[White_Blend == 1, Varietal := "White Blend"]
d[CAVA == 1, Varietal := "CAVA"]
d[Chianti_other == 1, Varietal := "Chianti_other"]
d[Port == 1, Varietal := "Port"]
d[Psrh_Port == 1, Varietal := "Psyr_Port"]
d[Sherry == 1, Varietal := "Sherry"]
d[Burgandy_other == 1, Varietal := "Burgandy_other"]
d[Chablis_other == 1, Varietal := "Chablis_other"]
d[Rhine == 1, Varietal := "Rhine"]
d[Muscadine == 1, Varietal := "Muscadine"]
d[Marsala == 1, Varietal := "Marsala"]
d[Chard_PN == 1, Varietal := "Chard_PN"]
d[Pinot_Blanc == 1, Varietal := "Pinot_Blanc"]
d[Norton == 1, Varietal := "Norton"]
d[CheninB_SauB == 1, Varietal := "CheninB_SauB"]
d[Chenin_blanc == 1, Varietal := "Chenin_blanc"]
d[CheninB_Chrd == 1, Varietal := "CheninB_Chrd"]
d[Grenache_PGR == 1, Varietal := "Grenache_PGR"]
d[Chard_Semilon == 1, Varietal := "Chard_Semilon"]
d[Cab_Primitvo == 1, Varietal := "Cab_Primitvo"]
d[Cab_Sang == 1, Varietal := "Cab_Sang"]
d[CabS_Merlot_CabF == 1, Varietal := "CabS_Merlot_CabF"]
d[Viognier == 1, Varietal := "Viognier"]
d[Chard_Viognier == 1, Varietal := "Chard_Viognier"]
d[Shz_Viognier == 1, Varietal := "Shz_Viognier"]
d[Gre_Syr_Carig_Cinsault == 1, Varietal := "Gre_Syr_Carig_Cinsault"]
d[Gre_Syr_Carigan == 1, Varietal := "Gre_Syr_Carigan"]
d[Syr_Carigan == 1, Varietal := "Syr_Carigan"]
d[Gre_Pn == 1, Varietal := "Gre_Pn"]
d[Gre_Cab_Syr_Mouv == 1, Varietal := "Gre_Cab_Syr_Mouv"]
d[Gre_Syr_Mouv == 1, Varietal := "Gre_Syr_Mouv"]
d[Jacquere == 1, Varietal := "Jacquere"]
d[Gre_Carig == 1, Varietal := "Gre_Carig"]
d[Semilon_Sauv == 1, Varietal := "Semilon_Sauv"]
d[TSMR == 1, Varietal := "TSMR"]
d[Carignan == 1, Varietal := "Carignan"]
d[Syr_rose == 1, Varietal := "Syr_rose"]
d[Sauvignon == 1, Varietal := "Sauvignon"]
d[Chard_Sauv == 1, Varietal := "Chard_Sauv"]
d[Cab_France == 1, Varietal := "Cab_Franc"]
d[Shz_Mer_CabS == 1, Varietal := "Shz_Mer_CabS"]
d[Domina == 1, Varietal := "Domina"]
d[Syr_Temp == 1, Varietal := "Syr_Temp"]
d[Temp_Cbs == 1, Varietal := "Temp_Cbs"]
d[Temp_Grn == 1, Varietal := "Temp_Grn"]
d[CabS_Gn_Sb == 1, Varietal := "CabS_Gn_Sb"]
d[Merlot_Temp == 1, Varietal := "Merlot_Temp"]
d[Cab_Shz_Mstrl == 1, Varietal := "Cab_Shz_Mstrl"]
d[Graciano == 1, Varietal := "Graciano"]
d[Tmpn_Graciano == 1, Varietal := "Tmpn_Graciano"]
d[Crnz_Tmpn_CBS == 1, Varietal := "Crnz_Tmpn_CBS"]

# capturing blush and rose
d[match_blush == 1, Varietal := "Blush"]
d[match_rose == 1, Varietal := "Rose"]
d[, c("match_blush","match_rose") := NULL]

# capturing varietal only for table wines - rest is Nielsen module name
d[product_module_descr == "WINE-FLAVORED/REFRESHMENT", Varietal := "Flavoured"]
d[product_module_descr == "WINE-SANGRIA", Varietal := "Sangria"]
d[product_module_descr == "WINE-SPARKLING", Varietal := "Sparkling"]
d[product_module_descr == "WINE-VERMOUTH", Varietal := "Vermouth"]
d[product_module_descr == "WINE-APERITIFS", Varietal := "Aperitifs"]
d[product_module_descr == "WINE-SWEET DESSERT-IMPORTED" | product_module_descr == "WINE-SWEET DESSERT-DOMESTIC", Varietal := "Dessert"]
d[upc_descr == "MADRIA SANGRIA DM MSC SG", Varietal := "Sangria"]
d[upc_descr == "CTL BR MSC JUICY PCH GL FR" | upc_descr == "A-BLNA MSC LEMONATA GL FR", Varietal := "Flavoured"]

# classifying Kosher in red and white
d[Varietal == "Cabernet Sauvignon" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Cabernet_Shiraz" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Chablis_other" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "WHITE"]
d[Varietal == "Chardonnay" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "WHITE"]
d[Varietal == "Concord" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Malbec" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Merlot" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Cabernet_Merlot" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Moscato" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "WHITE"]
d[Varietal == "P_Sirah" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Pinot Noir" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Pinot Grigio" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "WHITE"]
d[Varietal == "Riesling" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "WHITE"]
d[Varietal == "SB_Chard" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "WHITE"]
d[Varietal == "Sauvignon Blanc" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "WHITE"]
d[Varietal == "Shiraz" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Shiraz_Malbec" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Monastrell" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Nebbiolo" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Primitivo" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Malavasia" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "WHITE"]
d[Varietal == "Zinfandel" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]
d[Varietal == "Marsala" & product_module_descr == "WINE-KOSHER TABLE", wine_type := "RED"]

# Fixing wine type based on varietals captured
d[Varietal == "Barbera", wine_type := "RED"]
d[Varietal == "Bonarda" & wine_type == "OTHER", wine_type := "RED"]
d[upc_descr == "GRNT BRG BRS-VN AS CHRD WT IDT", Varietal := "Chardonnay"]
d[Varietal == "Burgandy_other", wine_type := "RED"]
d[Varietal == "Cabernet Sauvignon", wine_type := "RED"]
d[Varietal == "Carmenere" & wine_type == "OTHER", wine_type := "RED"]
d[Varietal == "Carmenere" & wine_type == "WHITE", wine_type := "RED"]
d[Varietal == "Chablis_other" & wine_type == "OTHER", wine_type := "WHITE"]
d[Varietal == "Chablis_other" & wine_type == "RED", wine_type := "WHITE"]
d[Varietal == "Grenache" & wine_type == "WHITE", Varietal := "Grenache Blanc"]
d[upc_descr == "GFV MLVSA CHRD V WT DDT", Varietal := "Chardonnay"]
d[Varietal == "Malavasia", wine_type := "RED"]
d[Varietal == "Malbec" & wine_type == "WHITE", Varietal := "Malbec_White"]
d[Varietal == "Meriatge" & wine_type == "WHITE", Varietal := "Meriatge_White"]
d[upc_descr == "TERRILOGIO IT S-CB-S MRLT IDT", Varietal := "Cabernet_Merlot"]
d[upc_descr == "TERRILOGIO IT S-CB-S MRLT IDT", wine_type := "RED"]
d[upc_descr == "DE GRAS CHL MRLT IDT", wine_type := "RED"]
d[Varietal == "Moscato" & wine_type == "RED", Varietal := "Moscato_Red"]
d[Varietal == "Moscato" & wine_type == "WHITE", Varietal := "Moscato_White"]
d[Varietal == "Muscadine" & wine_type == "WHITE", Varietal := "Muscadine_White"]
d[Varietal == "Muscadine" & wine_type == "RED", Varietal := "Muscadine_Red"]
d[Varietal == "Pinot Grigio", wine_type := "WHITE"]
d[Varietal == "Sauvignon Blanc", wine_type := "WHITE"]
d[Varietal == "Shiraz", wine_type := "RED"]
d[Varietal == "Vermentio", wine_type := "WHITE"]
d[Varietal == "White Blend" & wine_type == "RED", Varietal := ""]
d[Varietal == "Zinfandel" & wine_type == "WHITE", Varietal := "Zinfandel_White"]
d[upc_descr == "GRNT BRG BRS-VN AS SHZ RED IDT", Varietal := "Shiraz"]
d[Varietal == "Viognier" & wine_type == "RED", wine_type := "WHITE"]

d[wine_type == "RED" & Varietal == "" & appellation_label != "Imported", Varietal := "other_red"]
d[wine_type == "WHITE" & Varietal == "" & appellation_label != "Imported", Varietal := "other_white"]

d[Varietal == "" & wine_type == "RED" & appellation_label == "Imported", Varietal := "other_red_imported"]
d[Varietal == "" & wine_type == "WHITE" & appellation_label == "Imported", Varietal := "other_white_imported"]
d[Varietal == "" & wine_type == "OTHER" & appellation_label == "Imported", Varietal := "otherwinetype_imported"]
d[Varietal == "" & wine_type == "OTHER", Varietal := "otherwinetype"]

d[Varietal == "Shiraz", Varietal := "Syrah"]
d[Varietal == "Syrah", wine_type := "RED"]

# drop all the varietal flag columns (note: Cab_Sau listed twice in Stata)
drop_varietal_cols <- c("Cab_Sau","Chard","Pinot_G","Riesling","Malbec","Zinfandel","Merlot","Moscato",
  "Pinot_N","Bonarda","Sauvignon_Blanc","Macabeo","Tempranillo","Pinotage","SHZ","Syrah","Primitivo","Soave",
  "Mencia","Xarel","Torrontes","Monastrell","Liebfraumilch","Sangiovese","Bukettraube","Godello","Nebbiolo",
  "Vermentio","Rosato","Rosso","Verdejo","Pinot_Nero","Albarinho","Inzolia","Dornfelder","Negro_Amaro",
  "Lambrusco","Tarrango","Malavasia","Teroldego","Barbera","Trebbiano","NeroDAvola","Merlot_NeroDAvola",
  "Falanghina","Grillo","Gamay","Gruner_Veltliner","Piesporter_Michelsberg","Grenache","PinotG_Chard",
  "Bonarda_Merlot","Bonarda_Malbec","Bonarda_Syrah","SauBlanc_Semilon","Semilon","Merlot_Pinotage",
  "SHZ_Pinotage","Monas_Tempr","Pinot_Binaco","Syr_Gr_Mourvedre","Sang_Cab_Merlot","Mns_Syrah","Ugni_blanc",
  "Sang_Merlot","Cab_Merlo","Cab_SHZ","Cab_Syr","Cab_Syr_Mouv","Cab_TMPN","Cab_Malbec","SHZ_GRE","SHZ_Merlot",
  "Syrah_Mourvedre","SHZ_Malbec","SHZ_PinotN","SHZ_TMPN","Zin_SHZ","Gren_TMPN","Merlot_Gre","Merlot_PN",
  "Merlot_MLBC","TMPN_MLBC","Zin_Gre","Zin_Chardonnay","Zin_Moscato","SB_CHARD","Champgane_other","Chard_Chm",
  "MSC_CHM","PinotG_CHM","Zin_CHM","Carmenere","Concord","Meritage","Meri_Chard","Gewur","P_Sirah","Red_Blend",
  "White_Blend","CAVA","Chianti_other","Port","Psrh_Port","Sherry","Burgandy_other","Chablis_other","Rhine",
  "Muscadine","Marsala","Chard_PN","Pinot_Blanc","Norton","CheninB_Chrd","Chenin_blanc","CheninB_SauB",
  "Grenache_PGR","Chard_Semilon","Cab_Primitvo","Cab_Sang","CabS_Merlot_CabF","Mouv_C_GN","Viognier",
  "Chard_Viognier","Shz_Viognier","Gre_Syr_Carig_Cinsault","Gre_Syr_Carigan","Syr_Carigan","Gre_Pn",
  "Gre_Cab_Syr_Mouv","Gre_Syr_Mouv","Jacquere","Gre_Carig","Semilon_Sauv","TSMR","Carignan","Syr_rose",
  "Sauvignon","Chard_Sauv","Cab_France","Shz_Mer_CabS","Domina","Syr_Temp","Temp_Cbs","Temp_Grn","CabS_Gn_Sb",
  "Merlot_Temp","Cab_Shz_Mstrl","Graciano","Tmpn_Graciano","Crnz_Tmpn_CBS")
d[, (intersect(drop_varietal_cols, names(d))) := NULL]

d[Varietal == "Sb_Chard", Varietal := "Chard_Sauv"]

# =============================================================================
# Brand name as uniform
# =============================================================================
d[brand_descr == "VENTANA", brand_descr := "VENTANA VINEYARDS"]
d[brand_descr == "WHITEHALL LANE WINERY", brand_descr := "WHITEHALL LANE WINERY & VINEYA"]
d[brand_descr == "ROSEMOUNT OLD BENSON", brand_descr := "ROSEMOUNT ESTATE"]
d[brand_descr == "WILLIAM HILL", brand_descr := "WILLIAM HILL ESTATE"]
d[brand_descr == "A BY ACACIA", brand_descr := "ACACIA"]
d[brand_descr == "MOUNTAIN VIEW VINTNERS", brand_descr := "MOUNTAIN VIEW"]
d[brand_descr == "GREG NORMAN", brand_descr := "GREG NORMAN ESTATE"]
d[brand_descr == "FATTORIA DI PETROIO", brand_descr := "PETRAIO"]
d[brand_descr %in% c("RENE BARBIER MEDITERRANEAN BRT","RENE BARBIER MEDITERRANEAN WHT","RENE BARBIER MEDITERRANEAN RED","RENE BARBIER","RENE BARBIER MDTRRNE PETILLANT"), brand_descr := "RENE BARBIER MEDITERRANEAN"]
d[brand_descr %in% c("STARMONT","MERRYVALE"), brand_descr := "MERRYVALE STARMONT"]
d[brand_descr == "SNOQUALMIE", brand_descr := "SNOQUALMIE VINEYARDS"]
d[brand_descr == "COLUMBIA", brand_descr := "COLUMBIA WINERY"]
d[brand_descr == "APEX II", brand_descr := "APEX ASCENT"]
d[brand_descr == "BEAULIEU VINEYARD BV CSTL ESTS", brand_descr := "BEAULIEU VINEYARD BV COASTAL"]
d[brand_descr == "GLEN ELLEN RESERVE", brand_descr := "GLEN ELLEN"]
d[brand_descr == "LAURIER", brand_descr := "LAURIER VINEYARDS"]
d[brand_descr == "LA BOCA", brand_descr := "BARRIO LA BOCA"]
d[brand_descr == "NAPA VALLEY NAPA RIDGE", brand_descr := "NAPA RIDGE"]
d[brand_descr == "RUTHERFORD", brand_descr := "RUTHERFORD VINTNERS"]
d[brand_descr == "SEA RIDGE COASTAL", brand_descr := "SEA RIDGE"]
d[brand_descr == "BUENA VISTA", brand_descr := "BUENA VISTA CARNEROS"]
d[brand_descr == "BAROSSA VALLEY ESTATE E MINOR", brand_descr := "BAROSSA VALLEY"]
d[brand_descr == "HOUSE OF NOBILO", brand_descr := "NOBILO"]
d[brand_descr %in% c("SANTA CAROLINA","VINA SANTA CAROLINA VISTANA","SANTA CAROLINA VISTANA"), brand_descr := "VINA SANTA CAROLINA"]
d[brand_descr == "D'AQUINO", brand_descr := "GAETANO D'AQUINO"]
d[brand_descr == "EASLEY", brand_descr := "EASLEY WINERY"]
d[brand_descr == "VALCKENBERG", brand_descr := "P.J. VALCKENBERG"]
d[brand_descr == "CONCHA Y TORO XPLORADOR", brand_descr := "CONCHA Y TORO"]
d[brand_descr == "MOUNT VEEDER", brand_descr := "MOUNT VEEDER WINERY"]
d[brand_descr == "NICOLAS", brand_descr := "MAISON NICOLAS"]
d[brand_descr == "BARON PHILIPPE DE ROTHSCHILD", brand_descr := "BARON PHILIPPE"]
d[brand_descr == "LE GRAND NOIR", brand_descr := "LE GRAND"]
d[brand_descr == "CARL SITTMANN", brand_descr := "SITTMANN"]
d[brand_descr %in% c("LEELANAU","LEELANAU CELLARS WITCHES BREW"), brand_descr := "LEELANAU CELLARS"]
d[brand_descr == "BOONE'S", brand_descr := "BOONE'S FARM"]
d[brand_descr == "FREI BROS. REDWOOD CREEK", brand_descr := "REDWOOD CREEK"]
d[brand_descr == "APOTHIC", brand_descr := "APOTHIC RED"]
d[brand_descr == "BAREFOOT REFRESH", brand_descr := "BAREFOOT"]
d[brand_descr == "FORTANT", brand_descr := "FORTANT DE FRANCE"]
d[brand_descr == "MARC MONDAVI'S THE DIVINING RD", brand_descr := "THE DIVINING ROD"]
d[brand_descr == "FINCA EL PORTILLO", brand_descr := "EL PORTILLO"]
d[brand_descr == "HAHN ESTATES", brand_descr := "HAHN"]
d[brand_descr == "BOLLINGER SPECIAL", brand_descr := "BOLLINGER"]
d[brand_descr == "PEPI", brand_descr := "ROBERT PEPI"]
d[brand_descr == "LIN COURT", brand_descr := "LINCOURT"]
d[brand_descr == "CHATEAU DE MONTFORT", brand_descr := "CHATEAU MONTFOR"]
d[brand_descr == "MADDALENA", brand_descr := "MADDALENA VINEYARD"]
d[brand_descr == "1917 IL CONTE D' ALBA", brand_descr := "1917 IL CONTE"]
d[brand_descr == "CASA LAPOSTOLLE CUVEE ALEXNDRE", brand_descr := "LAPOSTOLLE CUVEE ALEXNDRE"]
d[brand_descr == "SEBASTIANI", brand_descr := "SEBASTIANI SONOMA CASK"]
d[brand_descr == "CAVES D'ESCLANS", brand_descr := "LES CAVES DU CHATEAU D'ESCLANS"]
d[brand_descr == "FRATELLI CELLA", brand_descr := "CELLA"]
d[brand_descr == "STRONGBOW GOLD APPLE", brand_descr := "STRONGBOW"]
d[brand_descr == "SOUVERAIN", brand_descr := "CHATEAU SOUVERAIN"]
d[brand_descr == "DOMAINE STE. MICHELLE", brand_descr := "MICHELLE"]
d[brand_descr == "DONA PAULA", brand_descr := "DONA PAULA LOS CARDOS"]
d[brand_descr == "CASTELLO DI GABBIANO", brand_descr := "GABBIANO"]
d[brand_descr %in% c("BERINGER CALIFORNIA CLCTN","STONE CELLARS BY BERINGER"), brand_descr := "BERINGER"]
d[brand_descr == "TORTOISE CREEK WINES", brand_descr := "TORTOISE CREEK"]
d[brand_descr %in% c("YELLOW BY YELLOWGLEN","PINK BY YELLOWGLEN","PINK"), brand_descr := "YELLOWGLEN"]
d[brand_descr == "GREG NORMAN ESTATES", brand_descr := "GREG NORMAN"]
d[brand_descr %in% c("WHITE TRUCK","RED TRUCK"), brand_descr := "TRUCK"]
d[brand_descr == "GOTT 8", brand_descr := "GOTT"]
d[brand_descr == "MIDNIGHT", brand_descr := "MIDNIGHT CELLARS"]
d[brand_descr == "BRANCOTT ESTATE", brand_descr := "BRANCOTT"]
d[brand_descr == "MONTES ALPHA", brand_descr := "MONTES"]
d[brand_descr == "ANTICA CONTEA DI CASTELVERO", brand_descr := "CASTELVERO"]
d[brand_descr == "TERREDORA DIPAOLO", brand_descr := "TERREDORA"]
d[brand_descr == "BISOL JEIO", brand_descr := "BISOL"]
d[brand_descr %in% c("HESS SELECT","HESS COLLECTION","HESS ESTATE"), brand_descr := "HESS"]
d[brand_descr == "MORRO BAY", brand_descr := "MORRO BAY VINEYARDS"]
d[brand_descr == "KUNDE ESTATE", brand_descr := "KUNDE"]
d[brand_descr %in% c("COOPER MOUNTAIN","COOPER HILL"), brand_descr := "COOPER"]
d[brand_descr == "IL", brand_descr := "MIONETTO IL"]
d[brand_descr == "LAKE SONOMA", brand_descr := "LAKE SONOMA WINERY"]
d[brand_descr == "VIANO", brand_descr := "VIANO VINEYARDS"]
d[brand_descr %in% c("FRANCIS COPPOLA DMND CLTN","FRANCIS COPPOLA DIAMOND CLLCTN","FRANCIS COPPOLA PRESENTS","FRANCIS COPPOLA DIRECTOR'S CUT"), brand_descr := "FRANCIS COPPOLA"]
d[brand_descr == "JAUME SERRA CRISTALINO", brand_descr := "CRISTALINO"]
d[brand_descr == "PERRY CREEK", brand_descr := "PERRY CREEK VINEYARDS"]
d[brand_descr == "OAK GROVE", brand_descr := "OAK GROVE VINEYARDS"]
d[brand_descr == "ABUNDANCE", brand_descr := "ABUNDANCE VINEYARDS"]
d[brand_descr == "ALVEAR'S", brand_descr := "ALVEAR'S"]
d[brand_descr == "YANGARRA PARK", brand_descr := "YANGARRA"]
d[brand_descr == "DOMAINE LAFOND ROC-EPINE", brand_descr := "DOMAINE LAFOND"]
d[brand_descr == "DI MAJO NORANTE SAN GIORGIO", brand_descr := "DI MAJO NORANTE"]
d[brand_descr == "VISTANA", brand_descr := "SANTA CAROLINA VISTANA"]
d[brand_descr == "S K N", brand_descr := "SCREW KAPPA NAPA"]
d[brand_descr == "WWE. DR. H. THANISCH", brand_descr := "DR. H. THANISCH"]
d[brand_descr == "CIELO", brand_descr := "FAMIGLIA CIELO"]
d[brand_descr == "FEUDO ARANCIO STEMMARI", brand_descr := "FEUDO ARANCIO "]
d[brand_descr == "STACKED", brand_descr := "STACK WINES"]
d[brand_descr == "JASPER WINERY J W", brand_descr := "JASPER WINERY J"]
d[brand_descr == "CLIF", brand_descr := "CLIF FAMILY WINERY"]
d[brand_descr == "BILA-HAUT DOMAINE DE", brand_descr := "BILA-HAUT"]
d[brand_descr == "THREE THIEVES BANDIT", brand_descr := "BANDIT"]
d[brand_descr == "NINETY + CELLARS", brand_descr := "90 + CELLARS"]
d[brand_descr == "ARTISAN VINTNERS GUILD", brand_descr := "ARTISAN WINERY"]
d[brand_descr == "BERNARD MAGREZ TRANQUILLITE", brand_descr := "BERNARD MAGREZ"]
d[brand_descr == "FINCA LA LINDA", brand_descr := "LA LINDA"]
d[brand_descr == "1917 IL CONTE D' ALBA", brand_descr := "1917 IL CONTE"]
d[brand_descr == "ADAM CAROLLA'S MANGRIA", brand_descr := "ADAM CAROLLA'S"]
d[brand_descr == "ADEGA COOP. DE BORBA", brand_descr := "ADEGA DE BORBA"]
d[brand_descr == "AIRLIE 7", brand_descr := "AIRLIE 7"]
d[brand_descr == "ALEXANDRIA NICOLE A SQUARED", brand_descr := "ALEXANDRIA NICOLE"]
d[brand_descr == "AIRLIE 7", brand_descr := "AIRLIE"]
d[brand_descr == "ALEXANDRIA NICOLE A SQUARED", brand_descr := "ALEXANDRIA NICOLE"]
d[brand_descr %in% c("ALLEGRINI + RENACER","ALLEGRINI PALAZZO DELLA TORRE"), brand_descr := "ALLEGRINI"]
d[brand_descr == "ANDEAN SKY", brand_descr := "ANDEAN"]
d[brand_descr %in% c("ANGOVE'S BEAR CROSSING","ANGOVE'S NINE VINES","ANGOVE'S RED BELLY BLACK"), brand_descr := "ANGOVE'S"]
d[brand_descr == "ANDES PEAKS", brand_descr := "ANDES PEAK"]
d[brand_descr %in% c("BANFI CENTINE","BANFI CHIANTI","BANFI COL DI SASSO"), brand_descr := "BANFI"]
d[brand_descr %in% c("BAREFOOT CELLARS","BAREFOOT REFRESH","BAREFOOT RESERVE"), brand_descr := "BAREFOOT"]
d[brand_descr %in% c("BEAULIEU VINEYARD BV COASTAL","BEAULIEU VINEYARD BV CSTL ESTS","BEAULIEU VNYRD BV GRGS DE LTR"), brand_descr := "BEAULIEU VINEYARD BV"]
d[brand_descr %in% c("BERINGER CALIFORNIA CLCTN","BERINGER FOUNDERS' ESTATE","BERINGER LUMINUS","BERINGER THIRD CENTURY"), brand_descr := "BERINGER"]
d[brand_descr == "BERNARD MAGREZ TRANQUILLITE", brand_descr := "BERNARD MAGREZ"]
d[brand_descr %in% c("BIG ASS CAB","BIG ASS CHARD","BIG ASS ZIN"), brand_descr := "BIG ASS"]
d[brand_descr %in% c("BILTMORE ESTATE","BILTMORE RESERVE"), brand_descr := "BILTMORE"]
d[brand_descr %in% c("BLOCK 012","BLOCK 052","BLOCK 054","BLOCK 115","BLOCK 13","BLOCK 213","BLOCK 214",
  "BLOCK 303","BLOCK 426","BLOCK 430","BLOCK 478","BLOCK 50","BLOCK 501","BLOCK 503","BLOCK 506","BLOCK 511",
  "BLOCK 512","BLOCK 515","BLOCK 516","BLOCK 523","BLOCK 533","BLOCK 575","BLOCK 577","BLOCK 613","BLOCK 713",
  "BLOCK 823","BLOCK 904","BLOCK 917","BLOCK 945","BLOCK 949"), brand_descr := "BLOCK"]
d[brand_descr == "VIANO VINEYARDS", brand_descr := "VIANO"]
d[brand_descr == "WEINSTOCK CELLAR SELECT", brand_descr := "WEINSTOCK CELLARS"]
d[brand_descr == "WHITEHALL LANE WINERY & VINEYA", brand_descr := "WHITEHALL LANE WINERY"]
d[brand_descr %in% c("WOODBRIDGE BY ROBERT MONDAVI","WOODBRIDGE RBRT MNDV SLT VY SR"), brand_descr := "WOODBRIDGE RBRT MNDV"]
d[brand_descr %in% c("YALUMBA THE SIGNATURE","YALUMBA THE STRAPPER"), brand_descr := "YALUMBA"]
d[brand_descr == "YERING STATION NELL", brand_descr := "YERING STATION"]
d[brand_descr == "ZIMMERMAN", brand_descr := "ZIMMERMAN-GRAEFF"]
d[brand_descr %in% c("ZONIN PRIMO AMORE","ZONIN TERRE PALLADIANE"), brand_descr := "ZONIN"]
d[brand_descr %in% c("ZUCCARDI Q","ZUCCARDI SERIE A","ZUCCARDI ZETA"), brand_descr := "ZUCCARDI"]
d[brand_descr %in% c("GALLO FAMILY VINEYARDS TWN VLY","GALLO FAMILY VNYD TWN VLY"), brand_descr := "GALLO FAMILY VINEYARDS"]

d[brand_descr %in% c("WOODBRIDGE RBRT MNDV","ROBERT MONDAVI PRIVATE SELECTN","LA FAMIGLIA DI ROBERT MONDAVI","WOODBRIDGE","WOODBRIDGE RBRT MNDV SLT VY SR","WOODBRIDGE BY ROBERT MONDAVI"), brand_descr := "ROBERT MONDAVI"]
d[brand_descr == "BAREFOOT BUBBLY", brand_descr := "BAREFOOT"]
d[brand_descr %in% c("PORTICO FROM SUTTER HOME","PORTICO FROM SUTTER HOME"), brand_descr := "SUTTER HOME"]
d[brand_descr %in% c("LOS HERMANOS BY BERINGER","STONE CELLARS BY BERINGER"), brand_descr := "BERINGER"]
d[brand_descr %in% c("GALLO FAMILY VINEYARDS SNMA RS","GALLO SHEFFIELD CELLARS","GALLO SHEFFIELD CLLRS SLVR LNE","GALLO","GALLO FAIRBANKS","ERNEST & JULIO GALLO SONOMA","ERNEST & JULIO GALLO VINEYARDS","GALLO ESTATES","GALLO FAMILY VINEYARDS TWN VLY","GALLO FAMILY VNYD TWN VLY","GALLO FAMILY VINEYARDS TWN VLY"), brand_descr := "GALLO FAMILY VINEYARDS"]

d[brand_descr %in% c("KENDALL-JACKSON AVANT","KENDALL-JACKSON GREAT ESTATES","KENDALL-JACKSON HIGHLAND ESTS","KENDALL-JACKSON JACKSON ESTATE","KENDALL-JACKSON JACKSON HILLS","KENDALL-JACKSON STATURE"), brand_descr := "KENDALL-JACKSON"]

d[brand_descr %in% c("CONCHA Y TORO","CONCHA Y TORO AMELIA","CONCHA Y TORO CSLR DEL DIABLO","CONCHA Y TORO FRONTERA","CONCHA Y TORO MRQS D CASA CNCH","CONCHA Y TORO TERRUNYO","CONCHA Y TORO XPLORADOR"), brand_descr := "CONCHA Y TORO FRONTERA"]

# =============================================================================
# Fix kosher appellation labels
# =============================================================================
d[upc_descr == "VLA SANTERO MSC D ASTI KT", appellation_label := "Imported"]
d[upc_descr == "VLA SANTERO MSC D ASTI KT", Importing_country := "Italy"]
d[upc_descr == "VLA SANTERO MSC D ASTI KT", Importing_region := "Asti"]
d[upc_descr == "CHATEAU CAMPLAY BDX SPRR KT", appellation_label := "Imported"]
d[upc_descr == "CHATEAU CAMPLAY BDX SPRR KT", Importing_country := "France"]
d[upc_descr == "CHATEAU CAMPLAY BDX SPRR KT", Importing_region := "Bordeaux"]  # Stata used upc_desc (==upc_descr typo); same condition
d[upc_descr == "HRZG-SL BDX KT", appellation_label := "Imported"]
d[upc_descr == "HRZG-SL BDX KT", Importing_country := "France"]
d[upc_descr == "HRZG-SL BDX KT", Importing_region := "Bordeaux"]
d[brand_descr %in% c("BAROSSA VALLEY","Thorn-Clarke Terra Barossa","Grant Burge Barossa Vines","Grand Barossa","BAROSSA VALLEY ESTATE","Barossa Jack"), Importing_region := "Barossa"]
d[Importing_region == "Barossa", Importing_country := "Australia"]
d[Importing_region == "Barossa", appellation_label := "Imported"]
d[Importing_region == "Barossa", Importing_region := ""]
d[brand_descr == "HERZOG SELECTION", Importing_country := "France"]
d[brand_descr == "HERZOG SELECTION", appellation_label := "Imported"]
d[upc_descr == "HRZG-SL BDX KT", Importing_region := "Bordeaux"]
d[upc_descr == "ALFASI CHL CB RED IDT", Varietal := "Cabernet Sauvignon"]  # Stata used upc_desc
d[upc_descr == "GRN PTH AS CHRD OR WT BX IDT", Varietal := "Chardonnay"]
d[upc_descr == "GRN TMB WT OR CA G WT DDT", Varietal := "other_white"]
d[upc_descr == "BR-TR T-VN CHRD C-V V RED DDT" | upc_descr == "MARTIN RAY CHRD CRNV V RED DDT", wine_type := "WHITE"]

# formatting: Stata proper() -> title case
proper <- function(x) stata_proper(x)   # Stata proper(): caps letter after any non-letter (incl. apostrophe)
d[, wine_type := proper(wine_type)]
d[, Varietal := proper(Varietal)]
d[, brand_descr := proper(brand_descr)]

d[Varietal == "Red Blend" & appellation_label != "Imported", Varietal := "Other_Red"]
d[Varietal == "Red Blend" & appellation_label == "Imported", Varietal := "Other_Red_Imported"]
d[Varietal == "White Blend" & appellation_label != "Imported", Varietal := "Other_White"]
d[Varietal == "White Blend" & appellation_label == "Imported", Varietal := "Other_White_Imported"]
d[appellation_label == "Imported" & Varietal == "Otherwinetype", Varietal := "Otherwinetype_Imported"]

# =============================================================================
# Converting quantity in 750ml
# =============================================================================
d[, size_ml := size1_amount]
d[size1_units == "ML", size_ml := size1_amount]
d[size1_units == "CT", size_ml := 750]
d[size1_units == "LI", size_ml := size1_amount * 1000]
d[size1_units == "OZ", size_ml := size1_amount * 29.5735]

# converting qty in 750ml bottle equivalent
d <- d[!(is.na(quantity))]                       # drop if quantity==. (one obs)
d[, quantity_total := quantity * multi]          # multi pack
d[, quantity_ml := quantity_total * size_ml]
d[, quantity_bottle := quantity_ml / 750]
d[, quantity_total := NULL]

# Price - per bottle and discounted
d[, price_per_bottle_final := final_price_paid / quantity_bottle]
d[, price_per_bottle := total_price_paid / quantity_bottle]

# merge m:1 year using CPI_deflator ; drop _merge
cpi <- read_dta_dt(file.path(ROOT, "data/public/cpi/CPI_deflator.dta"))
d <- merge(d, cpi, by = "year", all.x = TRUE, all.y = FALSE)   # master kept; using-only rows excluded
d[, final_price_per_bottle_deflated := price_per_bottle_final / Deflator]
d[, price_per_bottle_deflated := price_per_bottle / Deflator]

# summarize (no-op) ; drop if price_per_bottle > 400 (outlier; Stata: missing not > 400 stays)
d <- d[!(price_per_bottle > 400)]   # keeps NA rows (NA > 400 -> NA -> !NA kept), matching Stata "if" semantics

# =============================================================================
# Make varietal name uniform
# =============================================================================
d[Varietal == "Cab_Primitvo", Varietal := "Cabernet_Primitivo"]
d[Varietal == "CabS_Merlot_CabF", Varietal := "Cab_Merlot_CabFranc"]
d[Varietal == "Cab_Franc", Varietal := "Cabernet_Franc"]
d[Varietal == "Cab_Sang", Varietal := "Cabernet_Sangiovese"]
d[Varietal == "Cab_Shz_Mstrl", Varietal := "Cab_Syrah_Mnstrl"]
d[Varietal == "Cabernet_Shiraz", Varietal := "Cabernet_Syrah"]
d[Varietal == "Cab_Malbec", Varietal := "Cabernet_Malbec"]
d[Varietal == "Merlot_Pinotn", Varietal := "Merlot_PinotNoir"]
d[Varietal == "Mns_Syrah", Varietal := "Monastrell_Syrah"]
d[Varietal == "Monas_Tempr", Varietal := "Monastrell_Temprn"]
d[Varietal == "Negro_Amaro", Varietal := "Negroamaro"]
d[Varietal == "SHZ_Pinotage", Varietal := "Syrah_Pinotage"]
d[Varietal == "Shiraz_Greenache", Varietal := "Syrah_Greenache"]
d[Varietal == "Shiraz_Malbec", Varietal := "Syrah_Malbec"]
d[Varietal == "Shiraz_Merlot", Varietal := "Syrah_Merlot"]
d[Varietal == "Shiraz_PinotN", Varietal := "Syrah_PinotNoir"]
d[Varietal == "Shiraz_Tmpn", Varietal := "Syrah_Temprn"]
d[Varietal == "Shz_Mer_CabS", Varietal := "Syrah_Merlot_Cab"]
d[Varietal == "Shz_Viognier", Varietal := "Syrah_Viognier"]
d[Varietal == "Sang_Merlot", Varietal := "Sangiovese_Merlot"]
d[Varietal == "S_Cab_Merlot", Varietal := "Cabernet_Merlot"]
d[Varietal == "Shz_Pinotage", Varietal := "Syrah_Pinotage"]
d[Varietal == "Tmpn_Mlbc", Varietal := "Temprn_Malbec"]
d[Varietal == "Syr_Temp ", Varietal := "Syrah_Temprn"]   # NOTE: trailing space in Stata literal (likely no match)
d[Varietal == "TMPN_MLBC", Varietal := "Temprn_Malbec"]
d[Varietal == "Temp_Cbs", Varietal := "Temprn_Cabernet"]
d[Varietal == "Temp_Grn", Varietal := "Temprn_Grenache"]
d[Varietal == "Zinfandel_Shiraz", Varietal := "Zinfandel_Syrah"]
d[Varietal == "Sauvignon" & wine_type == "White", Varietal := "Sauvignon Blanc"]
d[Varietal == "Rosso", Varietal := "Other_Red_Imported"]
d[Varietal == "Cheninb_Chrd", Varietal := "Chenin_Chardonnay"]
d[Varietal == "Cheninb_Saub", Varietal := "Chenin_SauvignonBlanc"]
d[brand_descr %in% c("Columbia Crest Grand Estates","Columbia Crest Two Vines"), brand_descr := "Columbia Crest"]
d[Varietal == "Blush" | Varietal == "Rose", Varietal := "Blush_Rose"]
d[wine_type == "Other", wine_type := "Specialty"]
d[Varietal == "Otherwinetype", Varietal := "Other_Specialty"]
d[Varietal == "Otherwinetype_Imported", Varietal := "Other_Specialty_Imported"]
d[upc_descr == "PL-ANHSR GM BL-NR P-N WT IDT", Varietal := "Other_White_Imported"]  # Stata used upc_desc
d[upc_descr == "SML W&C AS D/D RED IDT", Varietal := "Other_Red_Imported"]
d[upc_descr == "SML W&C AS CB-S RED IDT", Varietal := "Cabernet Sauvignon"]
d[Varietal == "Cabs_Gn_Sb", Varietal := "Cabernet_Grenache_Sauvignon"]
d[Varietal == "Cabs_Merlot_Cabf", Varietal := "Cabernet_Merlot_CabFranc"]
d[Varietal == "Chard_Pn", Varietal := "Chard_PinotNoir"]
d[Varietal == "Chard_Sauv", Varietal := "Chard_Sauvignon"]
d[Varietal == "Syr_Temp", Varietal := "Syrah_Temprn"]
d[Varietal == "Meriatge" & wine_type == "Red" & appellation_label != "Imported", Varietal := "Other_Red"]
d[Varietal == "Meriatge" & wine_type == "Red" & appellation_label == "Imported", Varietal := "Other_Red_Imported"]
d[Varietal == "Meriatge_White" & appellation_label != "Imported", Varietal := "Other_White"]
d[Varietal == "Meriatge_White" & appellation_label == "Imported", Varietal := "Other_White_Imported"]
d[Varietal == "Meritge_Chard" & appellation_label != "Imported", Varietal := "Other_Red"]
d[Varietal == "Meritge_Chard" & appellation_label == "Imported", Varietal := "Other_Red_Imported"]
d[Varietal == "P_Sirah", Varietal := "Petite_Syrah"]
d[Varietal == "Rhine" & appellation_label != "Imported", Varietal := "Other_White"]
d[Varietal == "Rhine" & appellation_label == "Imported", Varietal := "Other_White_Imported"]
d[Varietal == "Piesporter_Michelsberg", Varietal := "Other_White_Imported"]
d[Varietal == "Syr_Gr_Mourvedre", Varietal := "Gre_Syr_Mouv"]
d[Varietal == "Malavasia", Varietal := "Other_Red"]
d[Varietal == "Marsala", wine_type := "White"]
d[Varietal == "Marsala", Varietal := "Other_White"]
d[Varietal == "Semilon", Varietal := "Semillon"]
d[Varietal == "Gewur", Varietal := "Gewurztraminer"]
d[Varietal == "Shz_Mer_Cabs", Varietal := "Syrah_Merlot_Cabs"]
d[Varietal == "Cabernet_Franc", Varietal := "Cabernet Franc"]
d[Varietal == "Shiraz_Pinotn", Varietal := "Syrah_Pinotage"]
d[Varietal == "Albarinho", Varietal := "Alvarinho"]
d[Varietal == "Chard_Semilon", Varietal := "Chard_Semillon"]
d[Varietal == "Chenin_Chardonnay", Varietal := "Chard_CheninBlanc"]
d[Varietal == "Pinotg_Chard", Varietal := "Chard_Pinotage"]
d[Varietal == "Saublanc_Semilon", Varietal := "SauBlanc_Semillon"]
d[Varietal == "Sb_Chard", Varietal := "Chard_Sauvignon"]
d[Varietal == "Chianti_Other", Varietal := "Other_Red"]
d[Varietal == "Burgandy_Other" & appellation_label == "Imported", Varietal := "Other_Red_Imported"]
d[Varietal == "Burgandy_Other", Varietal := "Other_Red"]
d[Varietal == "Chablis_Other" & appellation_label == "Imported", Varietal := "Other_White_Imported"]
d[Varietal == "Chablis_Other", Varietal := "Other_White"]
d[Varietal == "Pinot_Nero", Varietal := "Pinot Noir"]
d[upc_descr %in% c("SNTA BRBRA IT AZLENDA WT IDT","SN ANT IT LMBRSC RED IDT"), wine_appellation := ""]

# =============================================================================
# Merge country of origin from web search (2a_region_brand)
# =============================================================================
# merge m:1 brand_descr using 2a ; drop _merge (keeps master + matched cols)
rb <- read_dta_dt(file.path(ROOT, "data/public/brand_crosswalk/2a_region_brand.dta"))
d <- merge(d, rb, by = "brand_descr", all.x = TRUE, all.y = FALSE)
# Stata strings are never missing: unmatched merge rows are "" not NA. data.table
# fills unmatched with NA, which silently breaks downstream =="" / !="" tests.
# Coerce the merged-in character columns NA -> "" to match Stata semantics.
for (.c in setdiff(names(rb), "brand_descr"))
  if (is.character(d[[.c]])) d[is.na(get(.c)), (.c) := ""]
d[brand_country == "US" & appellation_label == "No", appellation_label := "US"]
d[Importing_country == "" & brand_country != "US" & appellation_label == "No", Importing_country := brand_country]
d[Importing_country != "", appellation_label := "Imported"]

d[, c("brand_country","brand_state_location") := NULL]
d[, c("price_per_bottle_final","price_per_bottle","final_price_per_bottle_deflated","price_per_bottle_deflated") := NULL]

# =============================================================================
# Merge state/AVA from web search (2b_external_match_basedonbrands)
# =============================================================================
em <- read_dta_dt(file.path(ROOT, "data/public/brand_crosswalk/2b_external_match_basedonbrands.dta"))
d <- merge(d, em, by = "brand_descr", all.x = TRUE, all.y = FALSE)
# coerce merged-in character columns NA -> "" (Stata: unmatched strings are "")
for (.c in setdiff(names(em), "brand_descr"))
  if (is.character(d[[.c]])) d[is.na(get(.c)), (.c) := ""]
d[state_appellation == "" & wine_appellation == "" & Importing_country == "", state_appellation := state_appellation_m]
d[wine_appellation == "" & state_appellation == "" & Importing_country == "", wine_appellation := wine_appellation_m]
d[Importing_country == "" & wine_appellation == "" & state_appellation == "", Importing_country := Importing_country_m]
d[, c("wine_appellation_m","state_appellation_m","Importing_country_m") := NULL]

# =============================================================================
# Excise tax for instruments
# =============================================================================
d[, wineclass := ""]
d[product_module_descr == "WINE-SPARKLING", wineclass := "sparkling"]
d[wineclass == "", wineclass := "table"]
# merge m:1 fips_state_code wineclass using state_excise_tax ; drop if _merge==2 ; drop _merge
# Bring only excisetx_dollarpergallon (fips_state_descr already in master -> Stata keeps master's).
tax <- read_dta_dt(file.path(ROOT, "data/public/tax/state_excise_tax.dta"))
tax <- tax[, c("fips_state_code","wineclass","excisetx_dollarpergallon")]
d <- merge(d, tax, by = c("fips_state_code","wineclass"), all.x = TRUE, all.y = FALSE)  # all.x drops using-only (==2)
d[, wineclass := NULL]

# formatting names
d[state_appellation == "California", appellation_label := "State_Appellation"]
d[state_appellation == "New_York", state_appellation := "New York"]
d[state_appellation == "North_Carolina", state_appellation := "North Carolina"]
d[wine_appellation == "Alexander Valley", wine_appellation := "Alexander_Valley"]
d[wine_appellation == "Lehigh_Va", wine_appellation := "Lehigh Valley"]
d[wine_appellation == "Shenandoah_Valley_Cal", wine_appellation := "Shenandoah_Valley"]
d[wine_appellation == "South Coast", wine_appellation := "South_Coast"]
d[wine_appellation == "Walla Walla", wine_appellation := "Walla_Walla"]
d[state_appellation == "Pennslyvania", state_appellation := "Pennsylvania"]

# classify all geographic information into 4 broader classifications
d[wine_appellation != "", appellation_label := "AVA"]
d[state_appellation != "", appellation_label := "State_Appellation"]
d[Importing_country != "", appellation_label := "Imported"]
d[state_appellation == "us_not_labelled", appellation_label := "us_not_labelled"]
d[state_appellation == "us_not_labelled", state_appellation := ""]
d[appellation_label == "US", appellation_label := "us_not_labelled"]

d <- d[!(appellation_label == "No")]              # drop if appellation_label=="No"
d <- d[!(appellation_label == "us_not_labelled")] # drop if appellation_label=="us_not_labelled"

d[wine_appellation == "Virginia", state_appellation := "Virginia"]
d[wine_appellation == "Virginia", wine_appellation := ""]
d[state_appellation == "Virginia", appellation_label := "State_Appellation"]
d[wine_appellation == "New Mexico", state_appellation := "New Mexico"]
d[wine_appellation == "New Mexico", wine_appellation := ""]
d[state_appellation == "New Mexico", appellation_label := "State_Appellation"]
d[brand_descr %in% c("Veuve Clicquot Ponsardin","La Vieille Ferme"), Importing_country := "France"]
d[brand_descr == "Veuve Clicquot Ponsardin", Importing_region := "Champgane"]
d[brand_descr %in% c("Stella Rosa","Santa Margherita","Ruffino","Roscato","Riunite","Lamarca","Castello Del Poggio","Bolla","Bartenura","Gaetano D'Aquino"), Importing_country := "Italy"]
d[brand_descr == "Stella Rosa", appellation_label := "Imported"]
d[brand_descr == "Stella Rosa", state_appellation := ""]
d[brand_descr %in% c("Jacob'S Creek","Bodega Norton"), Importing_country := "Australia"]
d[brand_descr %in% c("Don Simon","Cristalino"), Importing_country := "Spain"]
d[brand_descr == "Trapiche", Importing_country := "Argentina"]

# =============================================================================
# Country ranking (preserve / collapse-to-unique / save / restore / merge)
# =============================================================================
# bysort Importing_country: egen country_quantity = total(quantity_bottle) if Importing_country!=""
d[Importing_country != "", country_quantity := sum(quantity_bottle, na.rm = TRUE), by = Importing_country]
# (rows with Importing_country=="" get NA for country_quantity, as in Stata)

# preserve: build country_ranking from unique (Importing_country, country_quantity)
cr <- unique(d[, .(Importing_country, country_quantity)])
# egen rank_country = rank(-country_quantity): Stata default rank is average-rank for ties
cr[, rank_country := frank(-country_quantity, ties.method = "average")]
cr <- cr[!(Importing_country == "")]   # drop if Importing_country==""
# save country_ranking.dta (contains Importing_country, country_quantity, rank_country)
write_dta(cr, file.path(ROOT, "data/derived/country_ranking.dta"))
# restore (d unchanged) then merge m:1 Importing_country using country_ranking.
# Master already holds country_quantity; Stata keeps it (using's is identical for matches),
# so merge only rank_country to avoid a .x/.y collision and reproduce the same column set.
d <- merge(d, cr[, .(Importing_country, rank_country)], by = "Importing_country", all.x = TRUE, all.y = FALSE)
d[rank_country > 11 & appellation_label == "Imported", Importing_country := "Other_Countries"]
d[Importing_country == "Other", Importing_country := "Other_Countries"]
d[, c("rank_country","country_quantity") := NULL]

# drop the contiguous AVA dummy block Alexander_Valley ... Potter_Valley
nm <- names(d)
ava_block <- nm[which(nm == "Alexander_Valley"):which(nm == "Potter_Valley")]
d[, (ava_block) := NULL]

d[, geographic_label := ""]
d[, geographic_label := wine_appellation]
d[geographic_label == "" & Importing_country != "", geographic_label := Importing_country]
d[geographic_label == "" & state_appellation != "", geographic_label := state_appellation]

# =============================================================================
# Capture origin_state from geographic_label for distance instrument
# =============================================================================
d[geographic_label %in% c("Lake_County","San_Bernabe","San_Lucas","Santa_Clara","Rogue_Valley",
  "Spring_Mountain_District","Shenandoah_Valley","Yadkin_Valley","Red_Mountain","Monticello","Sta_Rita_Hills",
  "Stags_Leap_District","Altus","Atlas_Peak","Ballard_Canyon","Chehalem_Mountains","Chiles_Valley",
  "Dundee_Hills","El_Dorado","Fort_Ross_Seaview","Hames_Valley","High_Valley","Los_Olivos","Santa_Ynes_Valley",
  "Oak_Knoll","Temecula_Valley","San_Benito","Northern_Sonoma","Yorkville_Highlands","South_Coast",
  "Potter_Valley","Rattle_Snake_Hills"), origin_state := "California"]
d[wine_appellation == "Amador_County", origin_state := "California"]
d[wine_appellation == "Augusta", origin_state := "Missouri"]
d[wine_appellation == "Arroyo_Seco", origin_state := "California"]
d[wine_appellation == "Central_Coast", origin_state := "California"]
d[wine_appellation == "Finger_Lakes", origin_state := "New_York"]
d[wine_appellation == "Mendocino", origin_state := "California"]
d[wine_appellation == "Monterey_County", origin_state := "California"]
d[wine_appellation == "Napa_Valley", origin_state := "California"]
d[wine_appellation == "Ohio_River_Valley", origin_state := "Ohio"]
d[wine_appellation == "San_Antonio", origin_state := "California"]
d[wine_appellation == "Snake_River_Valley", origin_state := "Idaho"]
d[wine_appellation == "Sonoma", origin_state := "California"]
d[wine_appellation == "Willamette_Valley", origin_state := "Oregon"]
d[wine_appellation == "Lake_Erie", origin_state := "New_York"]
d[origin_state == "New York" | geographic_label == "Long_Island", origin_state := "New_York"]
d[wine_appellation == "Colorado_Grand_Valley", origin_state := "Colorado"]
d[geographic_label %in% c("Grand_River_Valley","Isle_St_George"), origin_state := "Ohio"]
d[geographic_label %in% c("Lake_Michigan_Shore","Leelanau_Peninsula"), origin_state := "Michigan"]
d[geographic_label %in% c("Lancaster_Valley","Lehigh Valley"), origin_state := "Pennsylvania"]
d[geographic_label == "Southeastern_New_England", origin_state := "Connecticut"]
d[geographic_label == "Texas_Hill_Country", origin_state := "Texas"]
d[geographic_label == "Ozark_Hignlands", origin_state := "Arkansas"]
d[appellation_label == "Imported" & origin_state == "", origin_state := "Imported"]
d[origin_state == "" & appellation_label == "State_Appellation", origin_state := state_appellation]
d[geographic_label %in% c("Alexander_Valley","Anderson_Valley","Carneros","Chalone","Clarksburg",
  "Contra_Costa_County","Dry_Creek_Valley","Dunnigan_Hills","Edna_Valley","Eola_Hills","Knights_Valley","Lodi",
  "Mendocino County","North_Coast","Oakville","Paso_Robles","Russian_River_Valley","Rutherford",
  "Santa_Barbara_County","Santa_Maria_Valley","Sierra_Foothills","Sonoma County"," Sonoma_Coast",
  "Sonoma_Mountain","Sonoma_Valley","St_Helena","Walla_Walla","Yakima_Valley"), origin_state := "California"]
d[geographic_label == "Columbia_Valley", origin_state := "Washington"]
d[geographic_label == "Texas_High_Plains", origin_state := "Texas"]
d[geographic_label == "Old_Mission_Peninsula", origin_state := "Michigan"]
d[geographic_label == "Sonoma_Coast", origin_state := "California"]

# =============================================================================
# Distance instrument
# =============================================================================
# merge m:1 scantrack_market_descr origin_state using distance ; drop if _merge==2 ; drop _merge
dist <- read_dta_dt(file.path(ROOT, "data/public/distance/distance.dta"))
d <- merge(d, dist, by = c("scantrack_market_descr","origin_state"), all.x = TRUE, all.y = FALSE)
d[state_appellation == "Pureto Rico", state_appellation := "Other_State"]

# =============================================================================
# Merge with Population Data (preserve/restore split by year, then append)
# =============================================================================
# poputemp1: year<=2009 merged with Population_2005-09 (key: scantrack_market_descr)
pop1 <- read_dta_dt(file.path(ROOT, "data/public/population/Population_2005-09.dta"))
temp1 <- d[!(year > 2009)]
temp1 <- merge(temp1, pop1, by = "scantrack_market_descr", all.x = TRUE, all.y = FALSE)

# poputemp2: year>=2010 merged with Population_2010-19 (keys: year, scantrack_market_descr)
pop2 <- read_dta_dt(file.path(ROOT, "data/public/population/Population_2010-19.dta"))
temp2 <- d[!(year < 2010)]
setorder(temp2, year, scantrack_market_descr)
temp2 <- merge(temp2, pop2, by = c("year","scantrack_market_descr"), all.x = TRUE, all.y = FALSE)

# append poputemp1 + poputemp2
d <- rbindlist(list(temp1, temp2), fill = TRUE)

# =============================================================================
# Save output
# =============================================================================
save_dt(d, file.path(ROOT, "data/derived/r_2_varietal_and_supplement_data.dta"))
cat("Done. nrow =", nrow(d), "\n")
