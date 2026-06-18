********************************************************************************
*This is do file no.3
*This do file selectes brand, varietal, geographic origin information for
*bottle and bulk wine and computes descriptive statistics***********************
********************************************************************************

set more off
clear all
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\2_varietal_and_supplement_data.dta"

keep if size_category=="bottle"

*generating weighted quantity and sales revenue
gen quantity_bottle_w = quantity_bottle*projection_factor
gen final_price_paid_w = final_price_paid*projection_factor
gen total_price_paid_w = total_price_paid*projection_factor
egen total_quantity = total(quantity_bottle_w)


*Not capturing varietal for France, Spain, Italy, Portugal
replace Varietal="Other_White_Imported" if Importing_country=="Spain" & wine_type=="White"
replace Varietal="Other_Red_Imported" if Importing_country=="Spain" & wine_type=="Red"
replace Varietal="Other_Specialty_Imported" if Importing_country=="Spain" & wine_type=="Specialty"
replace Varietal="Other_Red_Imported" if Importing_country=="France" & wine_type=="Red"
replace Varietal="Other_White_Imported" if Importing_country=="France" & wine_type=="White"
replace Varietal="Other_Specialty_Imported" if Importing_country=="France" & wine_type=="Specialty"
replace Varietal="Other_Red_Imported" if Importing_country=="Italy" & wine_type=="Red"
replace Varietal="Other_White_Imported" if Importing_country=="Italy" & wine_type=="White"
replace Varietal="Other_Specialty_Imported" if Importing_country=="Italy" & wine_type=="Specialty"
replace Varietal="Other_Red_Imported" if Importing_country=="Portugal" & wine_type=="Red"
replace Varietal="Other_White_Imported" if Importing_country=="Portugal" & wine_type=="White"
replace Varietal="Other_Specialty_Imported" if Importing_country=="Portugal" & wine_type=="Specialty"


*Defining the blend varietals
replace Varietal="Cabernet_Blend" if Varietal=="Cabernet_Malbec"| Varietal=="Cabernet_Merlot"| Varietal=="Cabernet_Merlot_CabFranc"| Varietal=="Cabernet_Syrah"
replace Varietal="Merlot_Blend" if Varietal=="Merlot_Malbec"| Varietal=="Merlot_PinotNoir"| Varietal=="Merlot_Pinotage"
replace Varietal="Syrah_Blend" if Varietal=="Syrah_Greenache"| Varietal=="Syrah_Malbec"| Varietal=="Syrah_Merlot"| Varietal=="Syrah_Merlot_Cabs"| Varietal=="Syrah_Mourvedre"| Varietal=="Syrah_Pinotage"| Varietal=="Syrah_Viognier"| Varietal=="Zinfandel_Syrah"
replace Varietal="Grenache_Blend" if Varietal=="Gre_Syr_Carigan"| Varietal=="Gre_Syr_Mouv"
replace Varietal="Tempranillo" if Varietal=="Crnz_Tmpn_Cbs"| Varietal=="Merlot_Temp"| Varietal=="Syr_Temp"| Varietal=="Syrah_Tmpn"| Varietal=="Tmpn_Cbs"| Varietal=="Tmpn_Grn"| Varietal=="Temprn_Cabernet"| Varietal=="Temprn_Malbec"    
replace Varietal="Monastrell_Blend" if Varietal=="Monast_Temprn"| Varietal=="Monast_Syrah"| Varietal=="Cab_Syr_Mnstrl"
replace Varietal="Bonarda_Blend" if Varietal=="Bonarda_Malbec"| Varietal=="Bonarda_Syrah"| Varietal=="Bonarda_Merlot"

*capturing varietal with less than 50 obs
replace Varietal="Other_Red_Imported" if Varietal=="Domina"| Varietal=="Tarrango"
replace Varietal="Other_Red" if Varietal=="Nebbiolo"| Varietal=="Primitivo"

*White Varietal
replace Varietal="Chardonnay_Blend" if Varietal=="Chard_CheninBlanc"| Varietal==" Chard_PinotNoir"| Varietal=="Chard_Pinotage"| Varietal=="Chard_Sauvignon"| Varietal=="Chard_Semillon"| Varietal=="Chard_Viognier"| Varietal=="Chard_PinotNoir"
replace Varietal="Other_White" if Varietal=="Trebbiano"| Varietal=="Grenache Blanc" 
replace Varietal="Other_White_Imported" if appellation_label=="Imported" & Varietal=="Alvarinho"
replace Varietal="Other_White" if appellation_label!="Imported" & Varietal=="Alvarinho"
replace Varietal="Other_White_Imported" if appellation_label=="Imported" & Varietal=="Malbec_White"
replace Varietal="Semillon" if Varietal=="SauBlanc_Semillon"

*Specialty
replace Varietal="Other_Specialty_Imported" if appellation_label=="Imported" & Varietal=="Aperitifs"
replace Varietal="Other_Specialty" if appellation_label!="Imported" & Varietal=="Aperitifs"
replace Varietal="Other_Specialty_Imported" if appellation_label=="Imported" & Varietal =="Sherry"

replace wine_appellation="Sonoma County" if wine_appellation=="Sonoma"
replace geographic_label="Sonoma County" if geographic_label=="Sonoma"

****************Aggregating Varietals and Geographic Origin Information--with smaller shares*******
drop geographic_label
gen geographic_label=""
replace geographic_label=wine_appellation
replace geographic_label=Importing_country if geographic_label=="" & Importing_country!=""
replace geographic_label=state_appellation if geographic_label=="" & state_appellation!=""


*capture varietal share for Imported wine by country
bysort wine_type: egen quantity_by_wine_type = total(quantity_bottle_w)
bysort Importing_country wine_type Varietal: egen country_total = total(quantity_bottle_w)
gen country_percent = 100*(country_total/quantity_by_wine_type)

replace Varietal="Other_Red_Imported" if Importing_country=="Chile" & wine_type=="Red" & country_percent<0.2          
replace Varietal="Other_White_Imported" if Importing_country=="Chile" & wine_type=="White" & country_percent<0.2       
replace Varietal="Other_Specialty_Imported" if Importing_country=="Chile" & wine_type=="Specialty" & country_percent<0.2    


replace Varietal="Other_Red_Imported" if Importing_country=="Australia" & wine_type=="Red" & country_percent<.2  
replace Varietal="Other_White_Imported" if Importing_country=="Australia" & wine_type=="White" & country_percent<0.2      
replace Varietal="Other_Red_Imported" if Importing_country=="Argentina" & wine_type=="Red" & country_percent<.2  
replace Varietal="Other_White_Imported" if Importing_country=="Argentina" & wine_type=="White" & country_percent<.2  
*keeping blush as seperate for Australia, Argentina
replace Varietal="Other_Red_Imported" if Importing_country=="Germany" & wine_type=="Red" & country_percent<.2    
replace Varietal="Other_White_Imported" if Importing_country=="Germany" & wine_type=="White" & country_percent<.2   
replace Varietal="Other_Specialty_Imported" if Importing_country=="Germany" & wine_type=="Specialty"   

*New Zealand
replace Varietal="Other_Red_Imported" if Importing_country=="New Zealand" & wine_type=="Red" 
replace Varietal="Other_White_Imported" if Importing_country=="New Zealand" & wine_type=="White" & country_percent <3
replace Varietal="Other_Specialty_Imported" if Importing_country=="New Zealand" & wine_type=="Specialty" 

*South Africa
replace Varietal="Other_Red_Imported" if Importing_country=="South Africa" & wine_type=="Red" 
replace Varietal="Other_White_Imported" if Importing_country=="South Africa" & wine_type=="White" 
replace Varietal="Other_Specialty_Imported" if Importing_country=="South Africa" & wine_type=="Specialty"

*Other countries-not capturing varietal
replace Varietal="Other_Red_Imported" if Importing_country=="Other_Countries" & wine_type=="Red"    
replace Varietal="Other_White_Imported" if Importing_country=="Other_Countries" & wine_type=="White" 
replace Varietal="Other_Specialty_Imported" if Importing_country=="Other_Countries" & wine_type=="Specialty"
       
drop country_total country_percent

*capture varietal share for domestic wine
bysort Varietal wine_type: egen us_total = total(quantity_bottle_w) if appellation_label!="Imported"
gen us_total_percent = 100*(us_total/quantity_by_wine_type)

replace Varietal="Other_Red" if us_total_percent <0.2 & wine_type=="Red" & appellation_label!="Imported" 
replace Varietal="Other_White" if us_total_percent <0.2 & wine_type=="White" & appellation_label!="Imported"
replace Varietal="Other_Specialty" if us_total_percent <0.2 & wine_type=="Specialty" & appellation_label!="Imported"
replace Varietal="Other_Specialty" if Varietal=="Aperitifs" & appellation_label!="Imported"   
drop us_total us_total_percent

*Appendix Table A6. 
preserve
cd "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\DescriptiveStats"
replace Importing_country = "Domestic" if Importing_country==""   
bysort Importing_country wine_type Varietal: egen country_total = total(quantity_bottle_w)
gen country_percent = 100*(country_total/quantity_by_wine_type)
replace country_percent = round(country_percent, .01)
keep Varietal wine_type Importing_country country_percent
duplicates drop
gsort Importing_country -country_percent
export excel Importing_country Varietal country_percent using "tables.xlsx" if wine_type=="Red", sheet("table_a6_red") sheetmodify firstrow(variables) nolabel
export excel Importing_country Varietal country_percent using "tables.xlsx" if wine_type=="White", sheet("table_a6_white") sheetmodify firstrow(variables) nolabel
export excel Importing_country Varietal country_percent using "tables.xlsx" if wine_type=="Specialty", sheet("table_a6_specialty") sheetmodify firstrow(variables) nolabel
restore


***Capture top brands*****          
bysort year: egen annual_quantity = total(quantity_bottle_w)    
bysort brand_descr year: egen brand_quantity = total(quantity_bottle_w)
gen brand_quantity_percent = 100*(brand_quantity/annual_quantity)
preserve
keep brand_descr brand_quantity_percent 
duplicates drop
bysort brand_descr: egen grand_total = total(brand_quantity_percent)
gen brand_average_share = grand_total/13
keep brand_descr brand_average_share
duplicates drop
gsort -brand_average_share
gen id = _n
sort brand_descr
*Table A4. 
export excel brand_descr using "tables.xlsx" if id <51, sheet("table_a4_brand") sheetmodify firstrow(variables) nolabel
drop id
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\brand_share_bottle.dta", replace
restore
merge m:1 brand_descr using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\brand_share_bottle.dta"
drop _merge
replace brand_descr="Other_brands" if brand_average_share <.10
erase "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\brand_share_bottle.dta"


****DOMESTIC GEOGRAPHIC ORIGIN SHARE*********
bysort geographic_label: egen appellation_quantity = total(quantity_bottle_w)
gen appellation_share = 100*(appellation_quantity/total_quantity)

*AVA that do not meet cut off of <0.01 are aggregated to the bigger ava they are part of and States are aggregated under "Other States"
replace wine_appellation="Napa_Valley" if wine_appellation=="Stags_Leap_District"| wine_appellation=="Atlas_Peak"| wine_appellation=="Yountville"| wine_appellation=="Spring_Mountain_District"| wine_appellation=="Calistoga"| wine_appellation=="Howell_Mountain"| wine_appellation=="Chiles_Valley"
replace wine_appellation="Central_Coast" if wine_appellation=="Arroyo_Grande_Valley"| wine_appellation=="Arroyo_Grande"| wine_appellation=="Cienega_Valley"| wine_appellation=="Paicines"| wine_appellation=="San_Benito"| wine_appellation=="Santa_Clara"| wine_appellation=="Hames_Valley"| wine_appellation=="Carmel_Valley"
replace wine_appellation="North_Coast" if wine_appellation=="Solano_County_Green_Valley"| wine_appellation=="Yorkville_Highlands"| wine_appellation=="Suisun_Valley"| wine_appellation=="Clear_Lake"| wine_appellation=="Rockpile"
replace wine_appellation="Willamette_Valley" if wine_appellation=="Dundee_Hills"| wine_appellation=="Mcminnville"| wine_appellation=="Chehalem_Mountains"| wine_appellation=="Yamhill_Carlton"
replace wine_appellation="Yakima_Valley" if wine_appellation=="Red_Mountain"
replace wine_appellation="Monterey_County" if wine_appellation=="San_Lucas"
replace wine_appellation="Mendocino" if wine_appellation=="Mcdowell_Valley"| wine_appellation=="Potter_Valley"
replace wine_appellation="Rogue_Valley" if wine_appellation=="Applegate_Valley"
replace wine_appellation="Sierra_Foothills" if wine_appellation=="Fiddletown"
replace wine_appellation="Lake_Erie" if wine_appellation=="Isle_St_George"
replace wine_appellation="Santa_Barbara_County" if wine_appellation=="Ballard_Canyon"
replace wine_appellation="Columbia_Valley" if wine_appellation=="Alocv"
replace wine_appellation="Other_AVAs" if appellation_label=="AVA" & appellation_share <.01       
replace state_appellation="Other_States" if appellation_label=="State_Appellation" & appellation_share < 0.1
drop appellation_quantity appellation_share


*Creating geographic_origin variable with the aggregated category
drop geographic_label
gen geographic_label=""
replace geographic_label=wine_appellation
replace geographic_label=Importing_country if geographic_label=="" & Importing_country!=""
replace geographic_label=state_appellation if geographic_label=="" & state_appellation!=""

*Table A8. 

preserve
bysort geographic_label: egen appellation_quantity = total(quantity_bottle_w)
gen appelation_share = 100*(appellation_quantity/total_quantity)
keep geographic_label appelation_share Importing_country appellation_label
duplicates drop
gsort -appelation_share
replace appelation_share=round(appelation_share, .01)
cd "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\DescriptiveStats"
export excel geographic_label appelation_share using "tables.xlsx" if appellation_label =="AVA", sheet("table_a8_ava") sheetmodify firstrow(variables) nolabel
export excel geographic_label appelation_share using "tables.xlsx" if appellation_label =="State_Appellation", sheet("table_a8_state_applelation") sheetmodify firstrow(variables) nolabel
export excel geographic_label appelation_share using "tables.xlsx" if appellation_label =="Imported", sheet("table_a8_imported") sheetmodify firstrow(variables) nolabel
restore

keep upc panel_year year date brand_descr upc_descr product_module_code product_module_descr total_price_paid projection_factor scantrack_market_descr final_price_paid scm_code wine_appellation state_appellation origin_state appellation_label Importing_country size_category Importing_region wine_type Varietal quantity_bottle Deflator excisetx_dollarpergallon distance_from distance_miles total_population pop_above_20 geographic_label fips_state_descr


save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\pre_3_bottle.dta", replace



*********************PREPARE BULK DATA******************************************
clear all
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\2_varietal_and_supplement_data.dta"

keep if size_category=="bulk"

*generating weighted quantity and sales revenue
gen quantity_bottle_w = quantity_bottle*projection_factor
gen final_price_paid_w = final_price_paid*projection_factor
gen total_price_paid_w = total_price_paid*projection_factor
egen total_quantity = total(quantity_bottle_w)


*Capturing Varietal for Italy and France and Spain
replace Varietal="Other_Red_Imported" if Importing_country=="France" & wine_type=="Red"
replace Varietal="Other_White_Imported" if Importing_country=="France" & wine_type=="White"
replace Varietal="Other_Specialty_Imported" if Importing_country=="France" & wine_type=="Specialty"
replace Varietal="Other_Red_Imported" if Importing_country=="Italy" & wine_type=="Red"
replace Varietal="Other_White_Imported" if Importing_country=="Italy" & wine_type=="White"
replace Varietal="Other_Specialty_Imported" if Importing_country=="Italy" & wine_type=="Specialty"
replace Varietal="Other_White_Imported" if Importing_country=="Spain" & wine_type=="White"     
replace Varietal="Other_Red_Imported" if Importing_country=="Spain" & wine_type=="Red"
replace Varietal="Other_Specialty_Imported" if Importing_country=="Spain" & wine_type=="Specialty"
*Other countries-not capturing varietal
replace Varietal="Other_Red_Imported" if Importing_country=="Other_Countries" & wine_type=="Red"    
replace Varietal="Other_White_Imported" if Importing_country=="Other_Countries" & wine_type=="White" 
replace Varietal="Other_Specialty_Imported" if Importing_country=="Other_Countries" & wine_type=="Specialty"


*Defining the blend varietals
replace Varietal="Cabernet_Blend" if Varietal=="Cabernet_Malbec"| Varietal=="Cabernet_Merlot"| Varietal=="Cabernet_Merlot_CabFranc"| Varietal=="Cabernet_Syrah"
replace Varietal="Merlot_Blend" if Varietal=="Merlot_Malbec"| Varietal=="Merlot_PinotNoir"| Varietal=="Merlot_Pinotage"
replace Varietal="Syrah_Blend" if Varietal=="Syrah_Greenache"| Varietal=="Syrah_Malbec"| Varietal=="Syrah_Merlot"| Varietal=="Syrah_Merlot_Cabs"| Varietal=="Syrah_Mourvedre"| Varietal=="Syrah_Pinotage"| Varietal=="Syrah_Viognier"| Varietal=="Zinfandel_Syrah"
replace Varietal="Grenache_Blend" if Varietal=="Gre_Syr_Carigan"| Varietal=="Gre_Syr_Mouv"
replace Varietal="Tempranillo" if Varietal=="Crnz_Tmpn_Cbs"| Varietal=="Merlot_Temp"| Varietal=="Syr_Temp"| Varietal=="Syrah_Tmpn"| Varietal=="Tmpn_Cbs"| Varietal=="Tmpn_Grn"| Varietal=="Temprn_Cabernet"| Varietal=="Temprn_Malbec"    
replace Varietal="Monastrell_Blend" if Varietal=="Monast_Temprn"| Varietal=="Monast_Syrah"| Varietal=="Cab_Syr_Mnstrl"
replace Varietal="Bonarda_Blend" if Varietal=="Bonarda_Malbec"| Varietal=="Bonarda_Syrah"| Varietal=="Bonarda_Merlot"

*capturing varietal with less than 50 obs
replace Varietal="Other_Red_Imported" if Varietal=="Domina"| Varietal=="Tarrango"
replace Varietal="Other_Red" if Varietal=="Nebbiolo"| Varietal=="Primitivo"

*White Varietal
replace Varietal="Chardonnay_Blend" if Varietal=="Chard_CheninBlanc"| Varietal==" Chard_PinotNoir"| Varietal=="Chard_Pinotage"| Varietal=="Chard_Sauvignon"| Varietal=="Chard_Semillon"| Varietal=="Chard_Viognier"| Varietal=="Chard_PinotNoir"
replace Varietal="Other_White" if Varietal=="Trebbiano"| Varietal=="Grenache Blanc" 
replace Varietal="Other_White_Imported" if appellation_label=="Imported" & Varietal=="Alvarinho"
replace Varietal="Other_White" if appellation_label!="Imported" & Varietal=="Alvarinho"
replace Varietal="Other_White_Imported" if appellation_label=="Imported" & Varietal=="Malbec_White"
replace Varietal="Semillon" if Varietal=="SauBlanc_Semillon"

*Specialty
replace Varietal="Other_Specialty_Imported" if appellation_label=="Imported" & Varietal=="Aperitifs"
replace Varietal="Other_Specialty" if appellation_label!="Imported" & Varietal=="Aperitifs"
replace Varietal="Other_Specialty_Imported" if appellation_label=="Imported" & Varietal =="Sherry"


replace wine_appellation="Sonoma County" if wine_appellation=="Sonoma"
replace geographic_label="Sonoma County" if geographic_label=="Sonoma"

drop geographic_label
gen geographic_label=""
replace geographic_label=wine_appellation
replace geographic_label=Importing_country if geographic_label=="" & Importing_country!=""
replace geographic_label=state_appellation if geographic_label=="" & state_appellation!=""

*capture varietal share for Imported wine by country
bysort wine_type: egen quantity_by_wine_type = total(quantity_bottle_w)
bysort Importing_country wine_type Varietal: egen country_total = total(quantity_bottle_w)
gen country_percent = 100*(country_total/quantity_by_wine_type)


*to align with bottle varietals and overall cutoff
replace Varietal="Other_Red_Imported" if Importing_country=="Chile" & wine_type=="Red" & country_percent < 0.6        
replace Varietal="Other_White_Imported" if Importing_country=="Chile" & wine_type=="White" & country_percent < 0.3      
replace Varietal="Other_Red_Imported" if Importing_country=="Australia" & wine_type=="Red" & country_percent <.2  
replace Varietal="Other_White_Imported" if Importing_country=="Australia" & wine_type=="White" & country_percent <0.2
replace Varietal="Other_White_Imported" if Importing_country=="Argentina" & wine_type=="White" & country_percent <1
replace Varietal="Other_Red_Imported" if Importing_country=="Argentina" & wine_type=="Red" & country_percent< 0.55
replace Varietal="Other_Red_Imported" if Importing_country=="Germany" & wine_type=="Red"  
replace Varietal="Other_White_Imported" if Importing_country=="Germany" & wine_type=="White" & country_percent<.10 
replace Varietal="Other_Specialty_Imported" if Importing_country=="Germany" & wine_type=="Specialty"
replace Varietal="Other_Red_Imported" if Importing_country=="South Africa" & wine_type=="Red" 
replace Varietal="Other_White_Imported" if Importing_country=="South Africa" & wine_type=="White" 
replace Varietal="Other_Specialty_Imported" if Importing_country=="South Africa" & wine_type=="Specialty"
replace Varietal="Other_Specialty_Imported" if Varietal=="Gruner_Veltliner" & appellation_label =="Imported"  
drop country_total country_percent

*capture varietal share for domestic wine
bysort Varietal wine_type: egen us_total = total(quantity_bottle_w) if appellation_label!="Imported"
gen us_total_percent = 100*(us_total/quantity_by_wine_type)

replace Varietal="Other_Red" if us_total_percent < 0.13 & wine_type=="Red"        
replace Varietal="Other_White" if us_total_percent < 0.74 & wine_type=="White"      
replace Varietal="Other_Specialty" if Varietal=="Aperitifs" 
drop us_total us_total_percent

*Appendix Table A7. 
preserve
cd "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\DescriptiveStats"
replace Importing_country = "Domestic" if Importing_country==""   
bysort Importing_country wine_type Varietal: egen country_total = total(quantity_bottle_w)
gen country_percent = 100*(country_total/quantity_by_wine_type)
replace country_percent = round(country_percent, .01)
keep Varietal wine_type Importing_country country_percent
duplicates drop
gsort Importing_country -country_percent
export excel Importing_country Varietal country_percent using "tables.xlsx" if wine_type=="Red", sheet("table_a7_red") sheetmodify firstrow(variables) nolabel
export excel Importing_country Varietal country_percent using "tables.xlsx" if wine_type=="White", sheet("table_a7_white") sheetmodify firstrow(variables) nolabel
export excel Importing_country Varietal country_percent using "tables.xlsx" if wine_type=="Specialty", sheet("table_a7_specialty") sheetmodify firstrow(variables) nolabel
restore


***Capture top brands*****          
bysort year: egen annual_quantity = total(quantity_bottle_w)    
bysort brand_descr year: egen brand_quantity = total(quantity_bottle_w)
gen brand_quantity_percent = 100*(brand_quantity/annual_quantity)
preserve
keep brand_descr brand_quantity_percent 
duplicates drop
bysort brand_descr: egen grand_total = total(brand_quantity_percent)
gen brand_average_share = grand_total/13
keep brand_descr brand_average_share
duplicates drop
gsort -brand_average_share
gen id = _n
sort brand_descr
*Table A4. 
export excel brand_descr using "tables.xlsx" if id <21, sheet("table_a5_brand") sheetmodify firstrow(variables) nolabel
drop id
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\brand_share_bulk.dta", replace
restore
merge m:1 brand_descr using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\brand_share_bulk.dta"
drop _merge
replace brand_descr="Other_brands" if brand_average_share < 1
erase "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\brand_share_bulk.dta"



****DOMESTIC GEOGRAPHIC ORIGIN SHARE*********
bysort geographic_label: egen appellation_quantity = total(quantity_bottle_w)
gen appellation_share = 100*(appellation_quantity/total_quantity)


*to allign with top AVAs selected in bottle category
replace wine_appellation="Other_AVAs" if wine_appellation=="Yadkin_Valley" 
replace wine_appellation="Other_AVAs" if wine_appellation=="Grand_River_Valley" 
replace state_appellation="Other_States" if state_appellation=="Idaho"| state_appellation=="Pennsylvania"| state_appellation=="New Jersey"| state_appellation =="Arkansas"| state_appellation =="Idaho Washington"| state_appellation=="Georgia"| state_appellation=="Illinois"| state_appellation =="Virginia"| state_appellation=="Rhode Island"| state_appellation =="Kentucky"| state_appellation=="Maryland"| state_appellation=="Maine"| state_appellation=="Oklahoma"| state_appellation=="Massachusetts"  
drop appellation_share appellation_quantity

*Creating geographic_origin variable with the aggregated category
drop geographic_label
gen geographic_label=""
replace geographic_label=wine_appellation
replace geographic_label=Importing_country if geographic_label=="" & Importing_country!=""
replace geographic_label=state_appellation if geographic_label=="" & state_appellation!=""

*Table A9.
preserve
bysort geographic_label: egen appellation_quantity = total(quantity_bottle_w)
gen appelation_share = 100*(appellation_quantity/total_quantity)
keep geographic_label appelation_share Importing_country appellation_label
duplicates drop
gsort -appelation_share
replace appelation_share=round(appelation_share, .01)
cd "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\DescriptiveStats"
export excel geographic_label appelation_share using "tables.xlsx" if appellation_label =="AVA", sheet("table_a9_ava") sheetmodify firstrow(variables) nolabel
export excel geographic_label appelation_share using "tables.xlsx" if appellation_label =="State_Appellation", sheet("table_a9_state_applelation") sheetmodify firstrow(variables) nolabel
export excel geographic_label appelation_share using "tables.xlsx" if appellation_label =="Imported", sheet("table_a9_imported") sheetmodify firstrow(variables) nolabel
restore

keep upc panel_year year date brand_descr upc_descr product_module_code product_module_descr total_price_paid projection_factor scantrack_market_descr final_price_paid scm_code wine_appellation state_appellation origin_state appellation_label Importing_country size_category Importing_region wine_type Varietal quantity_bottle Deflator excisetx_dollarpergallon distance_from distance_miles total_population pop_above_20 geographic_label fips_state_descr


save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\pre_3_bulk.dta", replace

*********************************************************************************

************************Combine Bulk and Botlle Data****************************

use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\pre_3_bottle.dta", clear
append using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\pre_3_bulk.dta"

*Domestic or Imported
gen Import=(appellation_label=="Imported")
gen Domestic=(appellation_label=="AVA"| appellation_label=="State_Appellation")
 
*Varietals- Red 
gen Cabernet_Sauvignon=(Varietal=="Cabernet Sauvignon")
gen Cabernet_Blend=(Varietal=="Cabernet_Blend") 
gen Carmenere=(Varietal=="Carmenere")
gen Concord=(Varietal=="Concord") 
gen Malbec=(Varietal=="Malbec") 
gen Moscato_Red=(Varietal=="Moscato_Red")
gen Petite_Syrah=(Varietal=="Petite_Syrah")
gen Pinot_Noir=(Varietal=="Pinot Noir")
gen Syrah=(Varietal=="Syrah")
gen Syrah_Blend=(Varietal=="Syrah_Blend")
gen Zinfandel=(Varietal=="Zinfandel") 
gen Other_Red=(Varietal=="Other_Red")
gen Other_Red_Imported=(Varietal=="Other_Red_Imported")
gen Merlot=(Varietal=="Merlot")

*White
gen Chardonnay=(Varietal=="Chardonnay")
gen Chenin_Blanc=(Varietal=="Chenin_Blanc")
gen Gewurztraminer=(Varietal=="Gewurztraminer")  
gen Liebfraumilch=(Varietal=="Liebfraumilch") 
gen Moscato_White=(Varietal=="Moscato_White")
gen Pinot_Grigio=(Varietal=="Pinot Grigio")
gen Riesling=(Varietal=="Riesling")
gen Viognier=(Varietal=="Viognier")
gen Other_White=(Varietal=="Other_White")
gen Other_White_Imported=(Varietal=="Other_White_Imported")
gen Sauvignon_Blanc=(Varietal=="Sauvignon Blanc")

*Specialty
gen Dessert=(Varietal=="Dessert")
gen Flavoured=(Varietal=="Flavoured")
gen Sangria=(Varietal=="Sangria")
gen Sparkling=(Varietal=="Sparkling") 
gen Vermouth=(Varietal=="Vermouth")
gen Other_Specialty=(Varietal=="Other_Specialty")
gen Other_Specialty_Imported=(Varietal=="Other_Specialty_Imported")
gen Blush_Rose=(Varietal=="Blush_Rose")


*5. Foreign Origin
gen Argentina=(Importing_country=="Argentina") 
gen Australia=(Importing_country=="Australia") 
gen Chile=(Importing_country=="Chile") 
gen France=(Importing_country=="France") 
gen Germany=(Importing_country=="Germany") 
gen Italy=(Importing_country=="Italy")
gen New_Zealand=(Importing_country=="New Zealand")
gen Portugal=(Importing_country=="Portugal")
gen South_Africa=(Importing_country=="South Africa")
gen Spain=(Importing_country=="Spain")
gen Other_Countries=(Importing_country=="Other_Countries")


*6. AVAs and state appelation
gen Alexander_Valley=(wine_appellation=="Alexander_Valley")
gen Amador_County=(wine_appellation=="Amador_County") 
gen Anderson_Valley=(wine_appellation=="Anderson_Valley") 
gen Arroyo_Seco=(wine_appellation=="Arroyo_Seco")
gen Augusta=(wine_appellation=="Augusta") 
gen Carneros=(wine_appellation=="Carneros") 
gen Central_Coast=(wine_appellation=="Central_Coast")
gen Chalk_Hill=(wine_appellation=="Chalk_Hill")
gen Chalone=(wine_appellation=="Chalone")
gen Clarksburg=(wine_appellation=="Clarksburg")
gen Columbia_Valley=(wine_appellation=="Columbia_Valley") 
gen Contra_Costa_County=(wine_appellation=="Contra_Costa_County") 
gen Dry_Creek_Valley=(wine_appellation=="Dry_Creek_Valley") 
gen Dunnigan_Hills=(wine_appellation=="Dunnigan_Hills") 
gen Eagle_Peak=(wine_appellation=="Eagle_Peak") 
gen Edna_Valley=(wine_appellation=="Edna_Valley")
gen Eola_Hills=(wine_appellation=="Eola_Hills")
gen Finger_Lakes=(wine_appellation=="Finger_Lakes") 
gen Guenoc=(wine_appellation=="Guenoc")
gen Horse_Heaven_Hills=(wine_appellation=="Horse_Heaven_Hills") 
gen Knights_Valley=(wine_appellation=="Knights_Valley")
gen Lake_County=(wine_appellation=="Lake_County")
gen Lake_Erie=(wine_appellation=="Lake_Erie")
gen Livermore_Valley=(wine_appellation=="Livermore_Valley") 
gen Lodi=(wine_appellation=="Lodi")
gen Mendocino=(wine_appellation=="Mendocino")
gen Mendocino_County=(wine_appellation=="Mendocino County")
gen Monterey_County=(wine_appellation=="Monterey_County") 
gen Napa_County=(wine_appellation=="Napa_County") 
gen Napa_Valley=(wine_appellation=="Napa_Valley")
gen North_Coast=(wine_appellation=="North_Coast")
gen Oakville=(wine_appellation=="Oakville")
gen Old_Mission_Peninsula=(wine_appellation=="Old_Mission_Peninsula")
gen Ohio_River_Valley=(wine_appellation=="Ohio_River_Valley")
gen Paso_Robles=(wine_appellation=="Paso_Robles") 
gen Red_Hills_Lake_County=(wine_appellation=="Red_Hills_Lake_County")
gen Russian_River_Valley=(wine_appellation=="Russian_River_Valley")
gen Rutherford=(wine_appellation=="Rutherford")
gen Saint_Lucia_Highlands=(wine_appellation=="Saint_Lucia_Highlands")
gen San_Antonio=(wine_appellation=="San_Antonio")  
gen Santa_Barbara_County=(wine_appellation=="Santa_Barbara_County")
gen Santa_Maria_Valley=(wine_appellation=="Santa_Maria_Valley")
gen Sierra_Foothills=(wine_appellation=="Sierra_Foothills") 
gen Snake_River_Valley=(wine_appellation=="Snake_River_Valley")
gen Sonoma_County=(wine_appellation=="Sonoma County")
gen Sonoma_Coast=(wine_appellation=="Sonoma_Coast")
gen Sonoma_Mountain=(wine_appellation=="Sonoma_Mountain")
gen Sonoma_Valley=(wine_appellation=="Sonoma_Valley")
gen St_Helena=(wine_appellation=="St_Helena")
gen Texas_High_Plains=(wine_appellation=="Texas_High_Plains")
gen Wahluke_Slope=(wine_appellation=="Wahluke_Slope")
gen Walla_Walla=(wine_appellation=="Walla_Walla")
gen Willamette_Valley=(wine_appellation=="Willamette_Valley") 
gen Yakima_Valley=(wine_appellation=="Yakima_Valley") 
gen Other_AVAs=(wine_appellation=="Other_AVAs")

*State Appellation
gen Florida=(state_appellation=="Florida") 
gen Indiana=(state_appellation=="Indiana") 
gen Michigan=(state_appellation=="Michigan")
gen Missouri=(state_appellation=="Missouri")
gen Nebraska=(state_appellation=="Nebraska")
gen New_York=(state_appellation=="New York")
gen North_Carolina=(state_appellation=="North Carolina")
gen Ohio=(state_appellation=="Ohio")
gen Oregon=(state_appellation=="Oregon") 
gen Texas=(state_appellation=="Texas")
gen Washington=(state_appellation=="Washington")
gen Other_States=(state_appellation=="Other_States")
gen California=(state_appellation=="California") 

*generating weighted quantity and sales revenue
gen quantity_bottle_w = quantity_bottle*projection_factor
gen final_price_paid_w = final_price_paid*projection_factor
gen total_price_paid_w = total_price_paid*projection_factor

*Table A1
egen total_quantity = total(quantity_bottle_w)
egen total_sales = total(final_price_paid_w)

bysort product_module_descr: egen total_module_quantity = total(quantity_bottle_w)
bysort product_module_descr: egen total_module_sale = total(final_price_paid_w)
gen module_quantity_share = 100*(total_module_quantity/total_quantity)
gen module_sales_share = 100*(total_module_sale/total_sales)

preserve
keep product_module_descr module_quantity_share module_sales_share
duplicates drop
cd "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\DescriptiveStats"
export excel product_module_descr module_quantity_share module_sales_share using "tables.xlsx", sheet("table_a1_module_share") sheetmodify firstrow(variables) nolabel
restore


*Table A2. 
bysort wine_type: egen total_wine_type_quantity = total(quantity_bottle_w)
bysort wine_type: egen total_wine_type_sale = total(final_price_paid_w)
gen wine_type_quantity_share = 100*(total_wine_type_quantity/total_quantity)
gen wine_type_sales_share = 100*(total_wine_type_sale/total_sales)


preserve
keep wine_type wine_type_quantity_share wine_type_sales_share
duplicates drop
cd "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\DescriptiveStats"
export excel wine_type wine_type_quantity_share wine_type_sales_share using "tables.xlsx", sheet("table_a2_wine_type_share") sheetmodify firstrow(variables) nolabel
restore

*Table A3. 
bysort size_category: egen total_size_quantity = total(quantity_bottle_w)
bysort size_category: egen total_size_sale = total(final_price_paid_w)
gen size_quantity_share = 100*(total_size_quantity/total_quantity)
gen size_sales_share = 100*(total_size_sale/total_sales)

preserve
keep size_category size_quantity_share size_sales_share
duplicates drop
cd "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\DescriptiveStats"
export excel size_category size_quantity_share size_sales_share using "tables.xlsx", sheet("table_a3_size_type_share") sheetmodify firstrow(variables) nolabel
restore

drop total_quantity- size_sales_share

*Table A10.
preserve
keep if appellation_label =="Imported"
egen total_quantity = total(quantity_bottle_w)
bysort size_category Importing_country: egen total_size_quantity = total(quantity_bottle_w)
gen size_quantity_share = 100*(total_size_quantity/total_quantity)
keep Importing_country size_category size_quantity_share
duplicates drop
cd "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\DescriptiveStats"
export excel Importing_country size_category size_quantity_share using "tables.xlsx", sheet("table_a10_impoting_country") sheetmodify firstrow(variables) nolabel
restore


*Figure 1a. & 1b.

preserve
bysort year appellation_label: egen quantity_appelation = total(quantity_bottle_w)
bysort year: egen total_quantity=  total(quantity_bottle_w)
gen appelation_share = 100*(quantity_appelation/total_quantity)

bysort year appellation_label: egen revenue_appelation = total(final_price_paid_w)
gen average_price = revenue_appelation/quantity_appelation
replace average_price = average_price/Deflator

keep appellation_label year appelation_share average_price
duplicates drop

export excel using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\DescriptiveStats\Graphs\data for figure 1.xls", firstrow(variables) replace

restore

drop quantity_bottle_w final_price_paid_w total_price_paid_w

save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\3_product_data.dta", replace


*This is the end of this do file************************************************