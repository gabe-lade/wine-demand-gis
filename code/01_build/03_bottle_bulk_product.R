# =============================================================================
# 03_bottle_bulk_product.R
# Port of DoFile/3_bottle_bulk_summary_stats.do  (608 lines)
#
# This is do file no.3.
# Selects brand, varietal, geographic origin information for bottle and bulk
# wine, builds the product-level dataset, and exports descriptive-stat tables.
#
# Faithful 1:1 translation of the Stata .do. Stata line numbers are given in
# comments like  # L<nn>  for traceability.
# =============================================================================

ROOT <- "/Users/lade.10/Library/CloudStorage/Dropbox/Work/RESEARCH/wine-demand-repo"
.PROJ_ROOT <- ROOT   # ensure _config.R adopts this ROOT instead of resolving its own
source(file.path(ROOT, "code/_config.R"))
ROOT <- "/Users/lade.10/Library/CloudStorage/Dropbox/Work/RESEARCH/wine-demand-repo"

TABLES <- file.path(ROOT, "output", "tables")
dir.create(TABLES, showWarnings = FALSE, recursive = TRUE)

# Descriptive-stat tables in the .do are written to an Excel workbook ("tables.xlsx")
# via `export excel ... sheet(...) sheetmodify`. We instead write one CSV per sheet
# into output/tables/. The main product dataset is the priority output.
# writexl is optional; if unavailable we fall back silently to CSV-only.
.have_writexl <- requireNamespace("writexl", quietly = TRUE)

write_table <- function(dt, name) {
  # name: base file name (no extension)
  data.table::fwrite(dt, file.path(TABLES, paste0(name, ".csv")))
}

# =============================================================================
# PART 1: BOTTLE DATA                                            # L9-L206
# =============================================================================

d <- read_dt(file.path(ROOT, "Data/r_2_varietal_and_supplement_data.dta"))  # L9

d <- d[size_category == "bottle"]                                              # L11

# generating weighted quantity and sales revenue                              # L13
d[, quantity_bottle_w := quantity_bottle * projection_factor]                 # L14
d[, final_price_paid_w := final_price_paid * projection_factor]               # L15
d[, total_price_paid_w := total_price_paid * projection_factor]               # L16
d[, total_quantity := sum(quantity_bottle_w, na.rm = TRUE)]                    # L17

# Not capturing varietal for France, Spain, Italy, Portugal                   # L20
d[Importing_country == "Spain"    & wine_type == "White",     Varietal := "Other_White_Imported"]      # L21
d[Importing_country == "Spain"    & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L22
d[Importing_country == "Spain"    & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L23
d[Importing_country == "France"   & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L24
d[Importing_country == "France"   & wine_type == "White",     Varietal := "Other_White_Imported"]      # L25
d[Importing_country == "France"   & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L26
d[Importing_country == "Italy"    & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L27
d[Importing_country == "Italy"    & wine_type == "White",     Varietal := "Other_White_Imported"]      # L28
d[Importing_country == "Italy"    & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L29
d[Importing_country == "Portugal" & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L30
d[Importing_country == "Portugal" & wine_type == "White",     Varietal := "Other_White_Imported"]      # L31
d[Importing_country == "Portugal" & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L32

# Defining the blend varietals                                                # L35
d[Varietal %in% c("Cabernet_Malbec","Cabernet_Merlot","Cabernet_Merlot_CabFranc","Cabernet_Syrah"),
  Varietal := "Cabernet_Blend"]                                                                        # L36
d[Varietal %in% c("Merlot_Malbec","Merlot_PinotNoir","Merlot_Pinotage"),
  Varietal := "Merlot_Blend"]                                                                          # L37
d[Varietal %in% c("Syrah_Greenache","Syrah_Malbec","Syrah_Merlot","Syrah_Merlot_Cabs","Syrah_Mourvedre","Syrah_Pinotage","Syrah_Viognier","Zinfandel_Syrah"),
  Varietal := "Syrah_Blend"]                                                                           # L38
d[Varietal %in% c("Gre_Syr_Carigan","Gre_Syr_Mouv"),
  Varietal := "Grenache_Blend"]                                                                        # L39
d[Varietal %in% c("Crnz_Tmpn_Cbs","Merlot_Temp","Syr_Temp","Syrah_Tmpn","Tmpn_Cbs","Tmpn_Grn","Temprn_Cabernet","Temprn_Malbec"),
  Varietal := "Tempranillo"]                                                                           # L40
d[Varietal %in% c("Monast_Temprn","Monast_Syrah","Cab_Syr_Mnstrl"),
  Varietal := "Monastrell_Blend"]                                                                      # L41
d[Varietal %in% c("Bonarda_Malbec","Bonarda_Syrah","Bonarda_Merlot"),
  Varietal := "Bonarda_Blend"]                                                                         # L42

# capturing varietal with less than 50 obs                                    # L44
d[Varietal %in% c("Domina","Tarrango"),   Varietal := "Other_Red_Imported"]   # L45
d[Varietal %in% c("Nebbiolo","Primitivo"), Varietal := "Other_Red"]           # L46

# White Varietal                                                              # L48
# NOTE (L49): Stata list includes " Chard_PinotNoir" (leading space) AND "Chard_PinotNoir"; preserved exactly.
d[Varietal %in% c("Chard_CheninBlanc"," Chard_PinotNoir","Chard_Pinotage","Chard_Sauvignon","Chard_Semillon","Chard_Viognier","Chard_PinotNoir"),
  Varietal := "Chardonnay_Blend"]                                                                      # L49
d[Varietal %in% c("Trebbiano","Grenache Blanc"), Varietal := "Other_White"]   # L50
d[appellation_label == "Imported"  & Varietal == "Alvarinho",    Varietal := "Other_White_Imported"]   # L51
d[appellation_label != "Imported"  & Varietal == "Alvarinho",    Varietal := "Other_White"]            # L52
d[appellation_label == "Imported"  & Varietal == "Malbec_White", Varietal := "Other_White_Imported"]   # L53
d[Varietal == "SauBlanc_Semillon", Varietal := "Semillon"]                    # L54

# Specialty                                                                   # L56
d[appellation_label == "Imported" & Varietal == "Aperitifs", Varietal := "Other_Specialty_Imported"]   # L57
d[appellation_label != "Imported" & Varietal == "Aperitifs", Varietal := "Other_Specialty"]            # L58
d[appellation_label == "Imported" & Varietal == "Sherry",    Varietal := "Other_Specialty_Imported"]   # L59

d[wine_appellation  == "Sonoma", wine_appellation  := "Sonoma County"]        # L61
d[geographic_label  == "Sonoma", geographic_label  := "Sonoma County"]        # L62

# Aggregating Varietals and Geographic Origin Information -- with smaller shares  # L64
d[, geographic_label := NULL]                                                 # L65
d[, geographic_label := ""]                                                   # L66
d[, geographic_label := wine_appellation]                                     # L67
d[geographic_label == "" & Importing_country != "", geographic_label := Importing_country]  # L68
d[geographic_label == "" & state_appellation != "", geographic_label := state_appellation]  # L69

# capture varietal share for Imported wine by country                         # L72
d[, quantity_by_wine_type := sum(quantity_bottle_w, na.rm = TRUE), by = wine_type]                       # L73
d[, country_total := sum(quantity_bottle_w, na.rm = TRUE), by = .(Importing_country, wine_type, Varietal)]  # L74
d[, country_percent := 100 * (country_total / quantity_by_wine_type)]         # L75

d[Importing_country == "Chile" & wine_type == "Red"       & country_percent < 0.2, Varietal := "Other_Red_Imported"]        # L77
d[Importing_country == "Chile" & wine_type == "White"     & country_percent < 0.2, Varietal := "Other_White_Imported"]      # L78
d[Importing_country == "Chile" & wine_type == "Specialty" & country_percent < 0.2, Varietal := "Other_Specialty_Imported"]  # L79

d[Importing_country == "Australia" & wine_type == "Red"   & country_percent < 0.2, Varietal := "Other_Red_Imported"]        # L82
d[Importing_country == "Australia" & wine_type == "White" & country_percent < 0.2, Varietal := "Other_White_Imported"]      # L83
d[Importing_country == "Argentina" & wine_type == "Red"   & country_percent < 0.2, Varietal := "Other_Red_Imported"]        # L84
d[Importing_country == "Argentina" & wine_type == "White" & country_percent < 0.2, Varietal := "Other_White_Imported"]      # L85
# keeping blush as separate for Australia, Argentina                          # L86
d[Importing_country == "Germany" & wine_type == "Red"     & country_percent < 0.2, Varietal := "Other_Red_Imported"]        # L87
d[Importing_country == "Germany" & wine_type == "White"   & country_percent < 0.2, Varietal := "Other_White_Imported"]      # L88
d[Importing_country == "Germany" & wine_type == "Specialty",                        Varietal := "Other_Specialty_Imported"]  # L89

# New Zealand                                                                 # L91
d[Importing_country == "New Zealand" & wine_type == "Red",                          Varietal := "Other_Red_Imported"]        # L92
d[Importing_country == "New Zealand" & wine_type == "White" & country_percent < 3,  Varietal := "Other_White_Imported"]      # L93
d[Importing_country == "New Zealand" & wine_type == "Specialty",                    Varietal := "Other_Specialty_Imported"]  # L94

# South Africa                                                                # L96
d[Importing_country == "South Africa" & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L97
d[Importing_country == "South Africa" & wine_type == "White",     Varietal := "Other_White_Imported"]      # L98
d[Importing_country == "South Africa" & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L99

# Other countries - not capturing varietal                                    # L101
d[Importing_country == "Other_Countries" & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L102
d[Importing_country == "Other_Countries" & wine_type == "White",     Varietal := "Other_White_Imported"]      # L103
d[Importing_country == "Other_Countries" & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L104

d[, c("country_total", "country_percent") := NULL]                            # L106

# capture varietal share for domestic wine                                    # L108
# L109: egen us_total = total(quantity_bottle_w) if appellation_label!="Imported"
#       -> total over non-Imported rows within (Varietal, wine_type); rows where
#          the condition is false get missing (NA), exactly as Stata's egen ... if.
d[, us_total := NA_real_]
d[appellation_label != "Imported",
  us_total := sum(quantity_bottle_w, na.rm = TRUE), by = .(Varietal, wine_type)]                          # L109
d[, us_total_percent := 100 * (us_total / quantity_by_wine_type)]             # L110

d[us_total_percent < 0.2 & wine_type == "Red"       & appellation_label != "Imported", Varietal := "Other_Red"]        # L112
d[us_total_percent < 0.2 & wine_type == "White"     & appellation_label != "Imported", Varietal := "Other_White"]      # L113
d[us_total_percent < 0.2 & wine_type == "Specialty" & appellation_label != "Imported", Varietal := "Other_Specialty"]  # L114
d[Varietal == "Aperitifs" & appellation_label != "Imported", Varietal := "Other_Specialty"]                            # L115
d[, c("us_total", "us_total_percent") := NULL]                                # L116

# --- Appendix Table A6 (preserve...restore -> temp data.table) ---           # L118-L131
{
  t <- copy(d)
  t[Importing_country == "", Importing_country := "Domestic"]                 # L121
  t[, country_total := sum(quantity_bottle_w, na.rm = TRUE), by = .(Importing_country, wine_type, Varietal)]  # L122
  t[, country_percent := 100 * (country_total / quantity_by_wine_type)]       # L123
  t[, country_percent := round(country_percent, 2)]                           # L124
  t <- t[, .(Varietal, wine_type, Importing_country, country_percent)]        # L125
  t <- unique(t)                                                              # L126
  setorder(t, Importing_country, -country_percent)                           # L127
  write_table(t[wine_type == "Red",       .(Importing_country, Varietal, country_percent)], "table_a6_red")        # L128
  write_table(t[wine_type == "White",     .(Importing_country, Varietal, country_percent)], "table_a6_white")      # L129
  write_table(t[wine_type == "Specialty", .(Importing_country, Varietal, country_percent)], "table_a6_specialty")  # L130
}

# --- Capture top brands ---                                                  # L134
d[, annual_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = year]                  # L135
d[, brand_quantity  := sum(quantity_bottle_w, na.rm = TRUE), by = .(brand_descr, year)]  # L136
d[, brand_quantity_percent := 100 * (brand_quantity / annual_quantity)]       # L137

# preserve block -> build brand_share temp; export Table A4; produce brand_average_share lookup  # L138-L152
{
  t <- d[, .(brand_descr, brand_quantity_percent)]                           # L139
  t <- unique(t)                                                             # L140
  t[, grand_total := sum(brand_quantity_percent, na.rm = TRUE), by = brand_descr]  # L141
  t[, brand_average_share := grand_total / 13]                              # L142
  t <- t[, .(brand_descr, brand_average_share)]                             # L143
  t <- unique(t)                                                            # L144
  setorder(t, -brand_average_share)                                        # L145
  t[, id := .I]                                                            # L146
  setorder(t, brand_descr)                                                # L147
  # Table A4: top-50 brand names                                           # L148
  write_table(t[id < 51, .(brand_descr)], "table_a4_brand")               # L149
  t[, id := NULL]                                                         # L150
  brand_share_bottle <<- copy(t)   # L151: save brand_share_bottle.dta (intermediate)
}

# merge m:1 brand_descr using brand_share_bottle                              # L153
d <- merge(d, brand_share_bottle, by = "brand_descr", all.x = TRUE)          # L153
# drop _merge (no _merge col created by merge())                              # L154
d[brand_average_share < 0.10, brand_descr := "Other_brands"]                 # L155
rm(brand_share_bottle)                                                       # L156 (erase tempfile)

# --- DOMESTIC GEOGRAPHIC ORIGIN SHARE ---                                    # L159
d[, appellation_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = geographic_label]  # L160
d[, appellation_share := 100 * (appellation_quantity / total_quantity)]      # L161

# AVAs below cutoff aggregated up; small states -> "Other States"            # L163
d[wine_appellation %in% c("Stags_Leap_District","Atlas_Peak","Yountville","Spring_Mountain_District","Calistoga","Howell_Mountain","Chiles_Valley"),
  wine_appellation := "Napa_Valley"]                                                                    # L164
d[wine_appellation %in% c("Arroyo_Grande_Valley","Arroyo_Grande","Cienega_Valley","Paicines","San_Benito","Santa_Clara","Hames_Valley","Carmel_Valley"),
  wine_appellation := "Central_Coast"]                                                                  # L165
d[wine_appellation %in% c("Solano_County_Green_Valley","Yorkville_Highlands","Suisun_Valley","Clear_Lake","Rockpile"),
  wine_appellation := "North_Coast"]                                                                    # L166
d[wine_appellation %in% c("Dundee_Hills","Mcminnville","Chehalem_Mountains","Yamhill_Carlton"),
  wine_appellation := "Willamette_Valley"]                                                              # L167
d[wine_appellation == "Red_Mountain",     wine_appellation := "Yakima_Valley"]        # L168
d[wine_appellation == "San_Lucas",        wine_appellation := "Monterey_County"]      # L169
d[wine_appellation %in% c("Mcdowell_Valley","Potter_Valley"), wine_appellation := "Mendocino"]  # L170
d[wine_appellation == "Applegate_Valley", wine_appellation := "Rogue_Valley"]         # L171
d[wine_appellation == "Fiddletown",       wine_appellation := "Sierra_Foothills"]     # L172
d[wine_appellation == "Isle_St_George",   wine_appellation := "Lake_Erie"]            # L173
d[wine_appellation == "Ballard_Canyon",   wine_appellation := "Santa_Barbara_County"] # L174
d[wine_appellation == "Alocv",            wine_appellation := "Columbia_Valley"]      # L175
d[appellation_label == "AVA"              & appellation_share < 0.01, wine_appellation  := "Other_AVAs"]    # L176
d[appellation_label == "State_Appellation" & appellation_share < 0.1, state_appellation := "Other_States"] # L177
d[, c("appellation_quantity", "appellation_share") := NULL]                   # L178

# Creating geographic_origin variable with the aggregated category            # L181
d[, geographic_label := NULL]                                                 # L182
d[, geographic_label := ""]                                                   # L183
d[, geographic_label := wine_appellation]                                     # L184
d[geographic_label == "" & Importing_country != "", geographic_label := Importing_country]  # L185
d[geographic_label == "" & state_appellation != "", geographic_label := state_appellation]  # L186

# --- Table A8 (preserve...restore) ---                                       # L188-L201
{
  t <- copy(d)
  t[, appellation_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = geographic_label]  # L191
  t[, appelation_share := 100 * (appellation_quantity / total_quantity)]      # L192
  t <- t[, .(geographic_label, appelation_share, Importing_country, appellation_label)]     # L193
  t <- unique(t)                                                              # L194
  setorder(t, -appelation_share)                                             # L195
  t[, appelation_share := round(appelation_share, 2)]                         # L196
  write_table(t[appellation_label == "AVA",              .(geographic_label, appelation_share)], "table_a8_ava")               # L198
  write_table(t[appellation_label == "State_Appellation", .(geographic_label, appelation_share)], "table_a8_state_applelation") # L199
  write_table(t[appellation_label == "Imported",         .(geographic_label, appelation_share)], "table_a8_imported")          # L200
}

# keep listed vars                                                            # L203
keep_vars_bottle <- c("upc","panel_year","year","date","brand_descr","upc_descr",
  "product_module_code","product_module_descr","total_price_paid","projection_factor",
  "scantrack_market_descr","final_price_paid","scm_code","wine_appellation",
  "state_appellation","origin_state","appellation_label","Importing_country",
  "size_category","Importing_region","wine_type","Varietal","quantity_bottle",
  "Deflator","excisetx_dollarpergallon","distance_from","distance_miles",
  "total_population","pop_above_20","geographic_label","fips_state_descr")
d <- d[, ..keep_vars_bottle]                                                  # L203

# save pre_3_bottle (intermediate data.table)                                 # L206
pre_3_bottle <- copy(d)

# =============================================================================
# PART 2: BULK DATA                                              # L210-L383
# =============================================================================

d <- read_dt(file.path(ROOT, "Data/r_2_varietal_and_supplement_data.dta"))  # L212

d <- d[size_category == "bulk"]                                               # L214

# generating weighted quantity and sales revenue                              # L216
d[, quantity_bottle_w := quantity_bottle * projection_factor]                 # L217
d[, final_price_paid_w := final_price_paid * projection_factor]               # L218
d[, total_price_paid_w := total_price_paid * projection_factor]               # L219
d[, total_quantity := sum(quantity_bottle_w, na.rm = TRUE)]                    # L220

# Capturing Varietal for Italy and France and Spain                           # L223
d[Importing_country == "France" & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L224
d[Importing_country == "France" & wine_type == "White",     Varietal := "Other_White_Imported"]      # L225
d[Importing_country == "France" & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L226
d[Importing_country == "Italy"  & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L227
d[Importing_country == "Italy"  & wine_type == "White",     Varietal := "Other_White_Imported"]      # L228
d[Importing_country == "Italy"  & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L229
d[Importing_country == "Spain"  & wine_type == "White",     Varietal := "Other_White_Imported"]      # L230
d[Importing_country == "Spain"  & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L231
d[Importing_country == "Spain"  & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L232
# Other countries - not capturing varietal                                    # L233
d[Importing_country == "Other_Countries" & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L234
d[Importing_country == "Other_Countries" & wine_type == "White",     Varietal := "Other_White_Imported"]      # L235
d[Importing_country == "Other_Countries" & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L236

# Defining the blend varietals                                                # L239
d[Varietal %in% c("Cabernet_Malbec","Cabernet_Merlot","Cabernet_Merlot_CabFranc","Cabernet_Syrah"),
  Varietal := "Cabernet_Blend"]                                                                        # L240
d[Varietal %in% c("Merlot_Malbec","Merlot_PinotNoir","Merlot_Pinotage"),
  Varietal := "Merlot_Blend"]                                                                          # L241
d[Varietal %in% c("Syrah_Greenache","Syrah_Malbec","Syrah_Merlot","Syrah_Merlot_Cabs","Syrah_Mourvedre","Syrah_Pinotage","Syrah_Viognier","Zinfandel_Syrah"),
  Varietal := "Syrah_Blend"]                                                                           # L242
d[Varietal %in% c("Gre_Syr_Carigan","Gre_Syr_Mouv"),
  Varietal := "Grenache_Blend"]                                                                        # L243
d[Varietal %in% c("Crnz_Tmpn_Cbs","Merlot_Temp","Syr_Temp","Syrah_Tmpn","Tmpn_Cbs","Tmpn_Grn","Temprn_Cabernet","Temprn_Malbec"),
  Varietal := "Tempranillo"]                                                                           # L244
d[Varietal %in% c("Monast_Temprn","Monast_Syrah","Cab_Syr_Mnstrl"),
  Varietal := "Monastrell_Blend"]                                                                      # L245
d[Varietal %in% c("Bonarda_Malbec","Bonarda_Syrah","Bonarda_Merlot"),
  Varietal := "Bonarda_Blend"]                                                                         # L246

# capturing varietal with less than 50 obs                                    # L248
d[Varietal %in% c("Domina","Tarrango"),    Varietal := "Other_Red_Imported"]  # L249
d[Varietal %in% c("Nebbiolo","Primitivo"), Varietal := "Other_Red"]           # L250

# White Varietal                                                              # L252
d[Varietal %in% c("Chard_CheninBlanc"," Chard_PinotNoir","Chard_Pinotage","Chard_Sauvignon","Chard_Semillon","Chard_Viognier","Chard_PinotNoir"),
  Varietal := "Chardonnay_Blend"]                                                                      # L253
d[Varietal %in% c("Trebbiano","Grenache Blanc"), Varietal := "Other_White"]   # L254
d[appellation_label == "Imported" & Varietal == "Alvarinho",    Varietal := "Other_White_Imported"]    # L255
d[appellation_label != "Imported" & Varietal == "Alvarinho",    Varietal := "Other_White"]             # L256
d[appellation_label == "Imported" & Varietal == "Malbec_White", Varietal := "Other_White_Imported"]    # L257
d[Varietal == "SauBlanc_Semillon", Varietal := "Semillon"]                    # L258

# Specialty                                                                   # L260
d[appellation_label == "Imported" & Varietal == "Aperitifs", Varietal := "Other_Specialty_Imported"]   # L261
d[appellation_label != "Imported" & Varietal == "Aperitifs", Varietal := "Other_Specialty"]            # L262
d[appellation_label == "Imported" & Varietal == "Sherry",    Varietal := "Other_Specialty_Imported"]   # L263

d[wine_appellation == "Sonoma", wine_appellation := "Sonoma County"]          # L266
d[geographic_label == "Sonoma", geographic_label := "Sonoma County"]          # L267

d[, geographic_label := NULL]                                                 # L269
d[, geographic_label := ""]                                                   # L270
d[, geographic_label := wine_appellation]                                     # L271
d[geographic_label == "" & Importing_country != "", geographic_label := Importing_country]  # L272
d[geographic_label == "" & state_appellation != "", geographic_label := state_appellation]  # L273

# capture varietal share for Imported wine by country                         # L275
d[, quantity_by_wine_type := sum(quantity_bottle_w, na.rm = TRUE), by = wine_type]                       # L276
d[, country_total := sum(quantity_bottle_w, na.rm = TRUE), by = .(Importing_country, wine_type, Varietal)]  # L277
d[, country_percent := 100 * (country_total / quantity_by_wine_type)]         # L278

# to align with bottle varietals and overall cutoff                           # L281
d[Importing_country == "Chile"     & wine_type == "Red"   & country_percent < 0.6,  Varietal := "Other_Red_Imported"]    # L282
d[Importing_country == "Chile"     & wine_type == "White" & country_percent < 0.3,  Varietal := "Other_White_Imported"]  # L283
d[Importing_country == "Australia" & wine_type == "Red"   & country_percent < 0.2,  Varietal := "Other_Red_Imported"]    # L284
d[Importing_country == "Australia" & wine_type == "White" & country_percent < 0.2,  Varietal := "Other_White_Imported"]  # L285
d[Importing_country == "Argentina" & wine_type == "White" & country_percent < 1,    Varietal := "Other_White_Imported"]  # L286
d[Importing_country == "Argentina" & wine_type == "Red"   & country_percent < 0.55, Varietal := "Other_Red_Imported"]    # L287
d[Importing_country == "Germany"   & wine_type == "Red",                             Varietal := "Other_Red_Imported"]    # L288
d[Importing_country == "Germany"   & wine_type == "White" & country_percent < 0.10, Varietal := "Other_White_Imported"]  # L289
d[Importing_country == "Germany"   & wine_type == "Specialty",                       Varietal := "Other_Specialty_Imported"]  # L290
d[Importing_country == "South Africa" & wine_type == "Red",       Varietal := "Other_Red_Imported"]        # L291
d[Importing_country == "South Africa" & wine_type == "White",     Varietal := "Other_White_Imported"]      # L292
d[Importing_country == "South Africa" & wine_type == "Specialty", Varietal := "Other_Specialty_Imported"]  # L293
d[Varietal == "Gruner_Veltliner" & appellation_label == "Imported", Varietal := "Other_Specialty_Imported"]  # L294
d[, c("country_total", "country_percent") := NULL]                            # L295

# capture varietal share for domestic wine                                    # L297
d[, us_total := NA_real_]
d[appellation_label != "Imported",
  us_total := sum(quantity_bottle_w, na.rm = TRUE), by = .(Varietal, wine_type)]                          # L298
d[, us_total_percent := 100 * (us_total / quantity_by_wine_type)]             # L299

d[us_total_percent < 0.13 & wine_type == "Red",   Varietal := "Other_Red"]    # L301
d[us_total_percent < 0.74 & wine_type == "White", Varietal := "Other_White"]  # L302
d[Varietal == "Aperitifs", Varietal := "Other_Specialty"]                     # L303
d[, c("us_total", "us_total_percent") := NULL]                                # L304

# --- Appendix Table A7 (preserve...restore) ---                              # L306-L319
{
  t <- copy(d)
  t[Importing_country == "", Importing_country := "Domestic"]                 # L309
  t[, country_total := sum(quantity_bottle_w, na.rm = TRUE), by = .(Importing_country, wine_type, Varietal)]  # L310
  t[, country_percent := 100 * (country_total / quantity_by_wine_type)]       # L311
  t[, country_percent := round(country_percent, 2)]                           # L312
  t <- t[, .(Varietal, wine_type, Importing_country, country_percent)]        # L313
  t <- unique(t)                                                              # L314
  setorder(t, Importing_country, -country_percent)                           # L315
  write_table(t[wine_type == "Red",       .(Importing_country, Varietal, country_percent)], "table_a7_red")        # L316
  write_table(t[wine_type == "White",     .(Importing_country, Varietal, country_percent)], "table_a7_white")      # L317
  write_table(t[wine_type == "Specialty", .(Importing_country, Varietal, country_percent)], "table_a7_specialty")  # L318
}

# --- Capture top brands ---                                                  # L322
d[, annual_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = year]                  # L323
d[, brand_quantity  := sum(quantity_bottle_w, na.rm = TRUE), by = .(brand_descr, year)]  # L324
d[, brand_quantity_percent := 100 * (brand_quantity / annual_quantity)]       # L325

{
  t <- d[, .(brand_descr, brand_quantity_percent)]                           # L327
  t <- unique(t)                                                             # L328
  t[, grand_total := sum(brand_quantity_percent, na.rm = TRUE), by = brand_descr]  # L329
  t[, brand_average_share := grand_total / 13]                              # L330
  t <- t[, .(brand_descr, brand_average_share)]                             # L331
  t <- unique(t)                                                            # L332
  setorder(t, -brand_average_share)                                        # L333
  t[, id := .I]                                                            # L334
  setorder(t, brand_descr)                                                # L335
  # Table A5 (commented "Table A4." in .do): top-20 brand names           # L336
  write_table(t[id < 21, .(brand_descr)], "table_a5_brand")               # L337
  t[, id := NULL]                                                         # L338
  brand_share_bulk <<- copy(t)   # L339: save brand_share_bulk.dta (intermediate)
}

d <- merge(d, brand_share_bulk, by = "brand_descr", all.x = TRUE)            # L341
# drop _merge                                                                 # L342
d[brand_average_share < 1, brand_descr := "Other_brands"]                    # L343
rm(brand_share_bulk)                                                         # L344 (erase tempfile)

# --- DOMESTIC GEOGRAPHIC ORIGIN SHARE ---                                    # L348
d[, appellation_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = geographic_label]  # L349
d[, appellation_share := 100 * (appellation_quantity / total_quantity)]      # L350

# to align with top AVAs selected in bottle category                          # L353
d[wine_appellation == "Yadkin_Valley",      wine_appellation := "Other_AVAs"] # L354
d[wine_appellation == "Grand_River_Valley", wine_appellation := "Other_AVAs"] # L355
d[state_appellation %in% c("Idaho","Pennsylvania","New Jersey","Arkansas","Idaho Washington","Georgia","Illinois","Virginia","Rhode Island","Kentucky","Maryland","Maine","Oklahoma","Massachusetts"),
  state_appellation := "Other_States"]                                                                  # L356
d[, c("appellation_share", "appellation_quantity") := NULL]                   # L357

# Creating geographic_origin variable with the aggregated category            # L359
d[, geographic_label := NULL]                                                 # L360
d[, geographic_label := ""]                                                   # L361
d[, geographic_label := wine_appellation]                                     # L362
d[geographic_label == "" & Importing_country != "", geographic_label := Importing_country]  # L363
d[geographic_label == "" & state_appellation != "", geographic_label := state_appellation]  # L364

# --- Table A9 (preserve...restore) ---                                       # L366-L378
{
  t <- copy(d)
  t[, appellation_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = geographic_label]  # L368
  t[, appelation_share := 100 * (appellation_quantity / total_quantity)]      # L369
  t <- t[, .(geographic_label, appelation_share, Importing_country, appellation_label)]     # L370
  t <- unique(t)                                                              # L371
  setorder(t, -appelation_share)                                             # L372
  t[, appelation_share := round(appelation_share, 2)]                         # L373
  write_table(t[appellation_label == "AVA",              .(geographic_label, appelation_share)], "table_a9_ava")               # L375
  write_table(t[appellation_label == "State_Appellation", .(geographic_label, appelation_share)], "table_a9_state_applelation") # L376
  write_table(t[appellation_label == "Imported",         .(geographic_label, appelation_share)], "table_a9_imported")          # L377
}

# keep listed vars (same list as bottle)                                      # L380
d <- d[, ..keep_vars_bottle]                                                  # L380

# save pre_3_bulk (intermediate data.table)                                   # L383
pre_3_bulk <- copy(d)

# =============================================================================
# PART 3: COMBINE BULK AND BOTTLE                                # L387-L606
# =============================================================================

# use pre_3_bottle; append pre_3_bulk                                         # L389-L390
d <- rbindlist(list(pre_3_bottle, pre_3_bulk), use.names = TRUE, fill = TRUE)

# Domestic or Imported                                                        # L392
d[, Import   := as.integer(appellation_label == "Imported")]                  # L393
d[, Domestic := as.integer(appellation_label == "AVA" | appellation_label == "State_Appellation")]  # L394

# Varietals - Red                                                             # L396
d[, Cabernet_Sauvignon   := as.integer(Varietal == "Cabernet Sauvignon")]     # L397
d[, Cabernet_Blend       := as.integer(Varietal == "Cabernet_Blend")]         # L398
d[, Carmenere            := as.integer(Varietal == "Carmenere")]              # L399
d[, Concord              := as.integer(Varietal == "Concord")]               # L400
d[, Malbec               := as.integer(Varietal == "Malbec")]                # L401
d[, Moscato_Red          := as.integer(Varietal == "Moscato_Red")]           # L402
d[, Petite_Syrah         := as.integer(Varietal == "Petite_Syrah")]          # L403
d[, Pinot_Noir           := as.integer(Varietal == "Pinot Noir")]            # L404
d[, Syrah                := as.integer(Varietal == "Syrah")]                 # L405
d[, Syrah_Blend          := as.integer(Varietal == "Syrah_Blend")]          # L406
d[, Zinfandel            := as.integer(Varietal == "Zinfandel")]            # L407
d[, Other_Red            := as.integer(Varietal == "Other_Red")]            # L408
d[, Other_Red_Imported   := as.integer(Varietal == "Other_Red_Imported")]  # L409
d[, Merlot               := as.integer(Varietal == "Merlot")]              # L410

# White                                                                       # L412
d[, Chardonnay           := as.integer(Varietal == "Chardonnay")]          # L413
d[, Chenin_Blanc         := as.integer(Varietal == "Chenin_Blanc")]        # L414
d[, Gewurztraminer       := as.integer(Varietal == "Gewurztraminer")]      # L415
d[, Liebfraumilch        := as.integer(Varietal == "Liebfraumilch")]       # L416
d[, Moscato_White        := as.integer(Varietal == "Moscato_White")]       # L417
d[, Pinot_Grigio         := as.integer(Varietal == "Pinot Grigio")]        # L418
d[, Riesling             := as.integer(Varietal == "Riesling")]            # L419
d[, Viognier             := as.integer(Varietal == "Viognier")]            # L420
d[, Other_White          := as.integer(Varietal == "Other_White")]         # L421
d[, Other_White_Imported := as.integer(Varietal == "Other_White_Imported")] # L422
d[, Sauvignon_Blanc      := as.integer(Varietal == "Sauvignon Blanc")]     # L423

# Specialty                                                                   # L425
d[, Dessert                  := as.integer(Varietal == "Dessert")]         # L426
d[, Flavoured                := as.integer(Varietal == "Flavoured")]       # L427
d[, Sangria                  := as.integer(Varietal == "Sangria")]        # L428
d[, Sparkling                := as.integer(Varietal == "Sparkling")]      # L429
d[, Vermouth                 := as.integer(Varietal == "Vermouth")]       # L430
d[, Other_Specialty          := as.integer(Varietal == "Other_Specialty")] # L431
d[, Other_Specialty_Imported := as.integer(Varietal == "Other_Specialty_Imported")]  # L432
d[, Blush_Rose               := as.integer(Varietal == "Blush_Rose")]     # L433

# 5. Foreign Origin                                                           # L436
d[, Argentina       := as.integer(Importing_country == "Argentina")]       # L437
d[, Australia       := as.integer(Importing_country == "Australia")]       # L438
d[, Chile           := as.integer(Importing_country == "Chile")]           # L439
d[, France          := as.integer(Importing_country == "France")]          # L440
d[, Germany         := as.integer(Importing_country == "Germany")]         # L441
d[, Italy           := as.integer(Importing_country == "Italy")]           # L442
d[, New_Zealand     := as.integer(Importing_country == "New Zealand")]     # L443
d[, Portugal        := as.integer(Importing_country == "Portugal")]        # L444
d[, South_Africa    := as.integer(Importing_country == "South Africa")]    # L445
d[, Spain           := as.integer(Importing_country == "Spain")]           # L446
d[, Other_Countries := as.integer(Importing_country == "Other_Countries")] # L447

# 6. AVAs and state appellation                                              # L450
d[, Alexander_Valley       := as.integer(wine_appellation == "Alexander_Valley")]       # L451
d[, Amador_County          := as.integer(wine_appellation == "Amador_County")]          # L452
d[, Anderson_Valley        := as.integer(wine_appellation == "Anderson_Valley")]        # L453
d[, Arroyo_Seco            := as.integer(wine_appellation == "Arroyo_Seco")]            # L454
d[, Augusta                := as.integer(wine_appellation == "Augusta")]               # L455
d[, Carneros               := as.integer(wine_appellation == "Carneros")]              # L456
d[, Central_Coast          := as.integer(wine_appellation == "Central_Coast")]         # L457
d[, Chalk_Hill             := as.integer(wine_appellation == "Chalk_Hill")]            # L458
d[, Chalone                := as.integer(wine_appellation == "Chalone")]               # L459
d[, Clarksburg             := as.integer(wine_appellation == "Clarksburg")]            # L460
d[, Columbia_Valley        := as.integer(wine_appellation == "Columbia_Valley")]       # L461
d[, Contra_Costa_County    := as.integer(wine_appellation == "Contra_Costa_County")]   # L462
d[, Dry_Creek_Valley       := as.integer(wine_appellation == "Dry_Creek_Valley")]      # L463
d[, Dunnigan_Hills         := as.integer(wine_appellation == "Dunnigan_Hills")]        # L464
d[, Eagle_Peak             := as.integer(wine_appellation == "Eagle_Peak")]            # L465
d[, Edna_Valley            := as.integer(wine_appellation == "Edna_Valley")]           # L466
d[, Eola_Hills             := as.integer(wine_appellation == "Eola_Hills")]            # L467
d[, Finger_Lakes           := as.integer(wine_appellation == "Finger_Lakes")]          # L468
d[, Guenoc                 := as.integer(wine_appellation == "Guenoc")]                # L469
d[, Horse_Heaven_Hills     := as.integer(wine_appellation == "Horse_Heaven_Hills")]    # L470
d[, Knights_Valley         := as.integer(wine_appellation == "Knights_Valley")]        # L471
d[, Lake_County            := as.integer(wine_appellation == "Lake_County")]           # L472
d[, Lake_Erie              := as.integer(wine_appellation == "Lake_Erie")]             # L473
d[, Livermore_Valley       := as.integer(wine_appellation == "Livermore_Valley")]      # L474
d[, Lodi                   := as.integer(wine_appellation == "Lodi")]                  # L475
d[, Mendocino              := as.integer(wine_appellation == "Mendocino")]             # L476
d[, Mendocino_County       := as.integer(wine_appellation == "Mendocino County")]      # L477
d[, Monterey_County        := as.integer(wine_appellation == "Monterey_County")]       # L478
d[, Napa_County            := as.integer(wine_appellation == "Napa_County")]           # L479
d[, Napa_Valley            := as.integer(wine_appellation == "Napa_Valley")]           # L480
d[, North_Coast            := as.integer(wine_appellation == "North_Coast")]           # L481
d[, Oakville               := as.integer(wine_appellation == "Oakville")]              # L482
d[, Old_Mission_Peninsula  := as.integer(wine_appellation == "Old_Mission_Peninsula")] # L483
d[, Ohio_River_Valley      := as.integer(wine_appellation == "Ohio_River_Valley")]     # L484
d[, Paso_Robles            := as.integer(wine_appellation == "Paso_Robles")]           # L485
d[, Red_Hills_Lake_County  := as.integer(wine_appellation == "Red_Hills_Lake_County")] # L486
d[, Russian_River_Valley   := as.integer(wine_appellation == "Russian_River_Valley")]  # L487
d[, Rutherford             := as.integer(wine_appellation == "Rutherford")]            # L488
d[, Saint_Lucia_Highlands  := as.integer(wine_appellation == "Saint_Lucia_Highlands")] # L489
d[, San_Antonio            := as.integer(wine_appellation == "San_Antonio")]           # L490
d[, Santa_Barbara_County   := as.integer(wine_appellation == "Santa_Barbara_County")]  # L491
d[, Santa_Maria_Valley     := as.integer(wine_appellation == "Santa_Maria_Valley")]    # L492
d[, Sierra_Foothills       := as.integer(wine_appellation == "Sierra_Foothills")]      # L493
d[, Snake_River_Valley     := as.integer(wine_appellation == "Snake_River_Valley")]    # L494
d[, Sonoma_County          := as.integer(wine_appellation == "Sonoma County")]         # L495
d[, Sonoma_Coast           := as.integer(wine_appellation == "Sonoma_Coast")]          # L496
d[, Sonoma_Mountain        := as.integer(wine_appellation == "Sonoma_Mountain")]       # L497
d[, Sonoma_Valley          := as.integer(wine_appellation == "Sonoma_Valley")]         # L498
d[, St_Helena              := as.integer(wine_appellation == "St_Helena")]             # L499
d[, Texas_High_Plains      := as.integer(wine_appellation == "Texas_High_Plains")]     # L500
d[, Wahluke_Slope          := as.integer(wine_appellation == "Wahluke_Slope")]         # L501
d[, Walla_Walla            := as.integer(wine_appellation == "Walla_Walla")]           # L502
d[, Willamette_Valley      := as.integer(wine_appellation == "Willamette_Valley")]     # L503
d[, Yakima_Valley          := as.integer(wine_appellation == "Yakima_Valley")]         # L504
d[, Other_AVAs             := as.integer(wine_appellation == "Other_AVAs")]            # L505

# State Appellation                                                           # L507
d[, Florida        := as.integer(state_appellation == "Florida")]          # L508
d[, Indiana        := as.integer(state_appellation == "Indiana")]          # L509
d[, Michigan       := as.integer(state_appellation == "Michigan")]         # L510
d[, Missouri       := as.integer(state_appellation == "Missouri")]         # L511
d[, Nebraska       := as.integer(state_appellation == "Nebraska")]         # L512
d[, New_York       := as.integer(state_appellation == "New York")]         # L513
d[, North_Carolina := as.integer(state_appellation == "North Carolina")]   # L514
d[, Ohio           := as.integer(state_appellation == "Ohio")]             # L515
d[, Oregon         := as.integer(state_appellation == "Oregon")]           # L516
d[, Texas          := as.integer(state_appellation == "Texas")]            # L517
d[, Washington     := as.integer(state_appellation == "Washington")]       # L518
d[, Other_States   := as.integer(state_appellation == "Other_States")]     # L519
d[, California     := as.integer(state_appellation == "California")]        # L520

# generating weighted quantity and sales revenue                              # L522
d[, quantity_bottle_w := quantity_bottle * projection_factor]                 # L523
d[, final_price_paid_w := final_price_paid * projection_factor]               # L524
d[, total_price_paid_w := total_price_paid * projection_factor]               # L525

# Table A1                                                                    # L527
d[, total_quantity := sum(quantity_bottle_w, na.rm = TRUE)]                    # L528
d[, total_sales    := sum(final_price_paid_w, na.rm = TRUE)]                   # L529

d[, total_module_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = product_module_descr]  # L531
d[, total_module_sale     := sum(final_price_paid_w, na.rm = TRUE), by = product_module_descr] # L532
d[, module_quantity_share := 100 * (total_module_quantity / total_quantity)]  # L533
d[, module_sales_share    := 100 * (total_module_sale / total_sales)]         # L534

{
  t <- d[, .(product_module_descr, module_quantity_share, module_sales_share)]  # L537
  t <- unique(t)                                                               # L538
  write_table(t, "table_a1_module_share")                                      # L540
}

# Table A2                                                                     # L544
d[, total_wine_type_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = wine_type]  # L545
d[, total_wine_type_sale     := sum(final_price_paid_w, na.rm = TRUE), by = wine_type] # L546
d[, wine_type_quantity_share := 100 * (total_wine_type_quantity / total_quantity)]  # L547
d[, wine_type_sales_share    := 100 * (total_wine_type_sale / total_sales)]    # L548

{
  t <- d[, .(wine_type, wine_type_quantity_share, wine_type_sales_share)]      # L552
  t <- unique(t)                                                               # L553
  write_table(t, "table_a2_wine_type_share")                                   # L555
}

# Table A3                                                                     # L558
d[, total_size_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = size_category]  # L559
d[, total_size_sale     := sum(final_price_paid_w, na.rm = TRUE), by = size_category] # L560
d[, size_quantity_share := 100 * (total_size_quantity / total_quantity)]      # L561
d[, size_sales_share    := 100 * (total_size_sale / total_sales)]             # L562

{
  t <- d[, .(size_category, size_quantity_share, size_sales_share)]           # L565
  t <- unique(t)                                                              # L566
  write_table(t, "table_a3_size_type_share")                                  # L568
}

# drop total_quantity- size_sales_share  (contiguous variable range)          # L571
# These were created L528-L562 in order; drop the whole contiguous block.
drop_range_L571 <- c("total_quantity","total_sales","total_module_quantity","total_module_sale",
  "module_quantity_share","module_sales_share","total_wine_type_quantity","total_wine_type_sale",
  "wine_type_quantity_share","wine_type_sales_share","total_size_quantity","total_size_sale",
  "size_quantity_share","size_sales_share")
d[, (drop_range_L571) := NULL]                                                # L571

# Table A10 (preserve...restore)                                              # L573-L583
{
  t <- d[appellation_label == "Imported"]                                     # L575
  t[, total_quantity := sum(quantity_bottle_w, na.rm = TRUE)]                  # L576
  t[, total_size_quantity := sum(quantity_bottle_w, na.rm = TRUE), by = .(size_category, Importing_country)]  # L577
  t[, size_quantity_share := 100 * (total_size_quantity / total_quantity)]    # L578
  t <- t[, .(Importing_country, size_category, size_quantity_share)]          # L579
  t <- unique(t)                                                              # L580
  write_table(t, "table_a10_impoting_country")                                # L582
}

# Figure 1a & 1b (preserve...restore)                                         # L586-L602
{
  t <- copy(d)
  t[, quantity_appelation := sum(quantity_bottle_w, na.rm = TRUE), by = .(year, appellation_label)]  # L589
  t[, total_quantity      := sum(quantity_bottle_w, na.rm = TRUE), by = year]  # L590
  t[, appelation_share := 100 * (quantity_appelation / total_quantity)]        # L591
  t[, revenue_appelation := sum(final_price_paid_w, na.rm = TRUE), by = .(year, appellation_label)]  # L593
  t[, average_price := revenue_appelation / quantity_appelation]               # L594
  t[, average_price := average_price / Deflator]                               # L595
  t <- t[, .(appellation_label, year, appelation_share, average_price)]        # L597
  t <- unique(t)                                                               # L598
  write_table(t, "data_for_figure_1")                                          # L600
}

# drop weighted vars                                                          # L604
d[, c("quantity_bottle_w", "final_price_paid_w", "total_price_paid_w") := NULL]  # L604

# save 3_product_data                                                         # L606
save_dt(d, file.path(ROOT, "Data/r_3_product_data.dta"))  # L606

cat(sprintf("DONE. Product dataset rows = %d, cols = %d\n", nrow(d), ncol(d)))

# This is the end of this do file ********************************************
