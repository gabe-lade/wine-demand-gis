********************************************************************************
*This is Do File no.1
*This do file cleans data and captures geograpihc origin information from 
*upc_descr and style_descr column***********************************************

set more off
clear all
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Wine Master Data\panel_wine_module_2021.dta", clear

*DATA CLEANING 1: TIME PERIOD - Restricting study period to 2007-17
drop if panel_year < 2007
drop if year < 2007

*since most of the information is extracted from upc descr-so drop observations for which upc description is not available. 
drop if upc_descr ==""                            //4,439 obs

*DATA CLEANING 2: Data should be uniquely defined at hh code, trip code and upc level. There are some duplicates coz of deal and coupon value; adding qty and price paid for such entries and retaining the coupon value of deal information. 

sort household_code trip_code_uc upc
quietly by household_code trip_code_uc upc: gen dup = cond(_N==1,0,_n)
bysort household_code trip_code_uc upc: egen byte quantity_1 = total(quantity) if dup>0
bysort household_code trip_code_uc upc: egen float coupon_value_1 = total(coupon_value) if dup >0
bysort household_code trip_code_uc upc: egen total_price_paid_1 = total(total_price_paid) if dup >0
gen float final_price_paid_1 = total_price_paid_1-coupon_value_1
gen float final_price_paid = total_price_paid-coupon_value
bysort household_code trip_code_uc upc: egen deal_flag_uc1 = total(deal_flag_uc) if dup>0    //to capture the transactions made on deal- so that when we drop the duplicates we dont miss this information. 
replace deal_flag_uc1=0 if deal_flag_uc1==.
replace deal_flag_uc=1 if deal_flag_uc1 >0

replace quantity = quantity_1 if dup>0
replace coupon_value = coupon_value_1 if dup>0
replace total_price_paid = total_price_paid_1 if dup>0
replace final_price_paid = final_price_paid_1 if dup>0
drop if dup>1              
drop quantity_1 coupon_value_1 total_price_paid_1 final_price_paid_1 dup deal_flag_uc1

format %60s upc_descr
replace upc_descr =strtrim(upc_descr)

**RESTRICTION1: dropping two modules- SAKE & Non-alcoholic, not considering this in analysis
drop if product_module_descr =="WINE-SAKE"                    
drop if product_module_descr =="WINE - NON ALCOHOLIC"         

**********SCANTRACK MARKET********
*RESTRICTION 2- Dropped all other REM-market: Keeping only major scantrack_market- in total 50 markets--we are combining three different NY markets as one

drop if scantrack_market_code>50 & panel_year>2015
drop if scantrack_market_code>52 & panel_year<2016

*to make the market description uniform across years (mostly varying 2017 onwards)
replace scantrack_market_descr ="Buffalo - Rochester" if scantrack_market_descr =="Buffalo-Rochester"
replace scantrack_market_descr ="Hartford - New Haven" if scantrack_market_descr =="Hartford-New Haven"
replace scantrack_market_descr ="New Orleans - Mobile" if scantrack_market_descr =="New Orleans-Mobile"
replace scantrack_market_descr ="Oklahoma City - Tulsa" if scantrack_market_descr =="Oklahoma City-Tulsa"
replace scantrack_market_descr ="Raleigh - Durham" if scantrack_market_descr =="Raleigh-Durham"
replace scantrack_market_descr ="Salt Lake City" if scantrack_market_descr =="Salt Lake City-Boise"
replace scantrack_market_descr ="St. Louis" if scantrack_market_descr =="St Louis"
replace scantrack_market_descr ="Richmond-Norfolk" if scantrack_market_descr =="Richmond"

*RESTRICTION 2.1- combininng urban and suburban and ex-urban New York in one market- Also from 2016 onwards there is no urban and exurban but only one NY market

replace scantrack_market_descr="New York" if scantrack_market_descr=="Urban NY"| scantrack_market_descr=="Suburban NY"| scantrack_market_descr=="Exurban NY"


*Data Formatting: Keeping it uniform thourghout the years: Generating our own code for scantrack market as Nielsen has different set of codes for 2005-15 and different for 2016 onwards... 

encode scantrack_market_descr, gen(scm_code)


*RESTRICTION 3- drop those observation for which we do not have price info-
drop if total_price_paid ==0 & deal_flag_uc ==0 & coupon_value ==0   //price information missing as per Nielsen Definiton 

*RESTRICTION5- 11 obs without size amount is dropped
drop if size1_amount==.    


*Wine appellation are the US defined AVA
*tab style_descr 
gen wine_appellation=""

*Alexander Valley
gen Alexander_Valley = (style_descr=="A-V"| style_descr=="A-V RS"| style_descr=="A-V SC"| style_descr=="A-V SC RS"| style_descr=="A-V SONOMA"| style_descr=="A-V SONOMA RS"| style_descr=="A-V SPECIAL RS"| style_descr=="A-V W-R"| style_descr=="A-V CA"| style_descr=="A-V BARRELLI CREEK"| style_descr=="A-V CA SC RS"| style_descr=="A-V CA SONOMA RS"| style_descr=="A-V HERITAGE RS"| style_descr=="A-V NORTHERN SONOMA RS"| style_descr=="A-V PVT RS"| style_descr=="A-V RS ALEXANDRE"| style_descr=="A-V S-V"| brand_descr=="ALEXANDER VALLEY VINEYARDS"| style_descr=="A-V CA SC-Alexander"| style_descr=="A-V NAPA COUNTY PVT RS"| style_descr=="A-V CA SC"| style_descr=="A-V NAPA COUNTY PVT RS"| style_descr=="A-V RS ALEXANDRE")


*Anderson Valley
gen Anderson_Valley = (style_descr=="ANDERSON VALLEY MC"| style_descr=="ANDERSON VALLEY MENDOCINO"| style_descr=="ANDERSON VALLEY RS"| style_descr=="ANDERSON VALLEY")
gen Anderson= regexm(" " + upc_descr  + " ", " (AD-V) ")| regexm(" " + upc_descr  + " ", " (AVM) ")
replace Anderson_Valley=1 if Anderson==1
drop Anderson

*Columbia Valley sub- AVA
*Ancient Lake of Columbia Valley
gen ALOCV = (style_descr=="ALOCV-WS"| style_descr=="ALOCV WASHINGTON STATE"|style_descr=="ANCIENT LAKE OF COLUMBIA VALLE")

*Horse Heaven Hills
gen Horse_Heaven_Hills = (style_descr=="HORSE HEAVEN HILLS RS"| style_descr=="HORSE HEAVENS HILLS"| style_descr=="C-V HORSE HEAVEN HILLS"| style_descr=="HORSE HEAVEN HILLS"| style_descr=="HHHWWW"| style_descr=="HHH-WS"| style_descr=="HHHW"| style_descr=="HHHWSZR"| style_descr=="HHHYV"| upc_descr=="MCR CNY CB-S HHH YV V RED DDT"| upc_descr=="MCR CNY CHRD HHH YV V WT DDT")

gen Rattle_Snake_Hills = (style_descr=="RTLSNK HILLS"| style_descr=="RTLSNK HILLS WASHINGTON"| style_descr=="RTLSNK HILLS WASHINGTON STATE"| style_descr=="RTLSN KHILLS WASHINGTON STATE"| style_descr=="RTLSNK HILLS YAKIMA VALLEY"| style_descr=="OREGON RTLSNK HILLS")

gen Red_Mountain = (style_descr=="RED MOUNTAIN"| style_descr=="RED MOUNTAIN WASHINGTON STATE"| style_descr=="RD MNTN AV WSHNGTN ST")

*Walla Walla Washington
gen Walla_Walla = (style_descr=="WALLA WALLA VALLEY WASHINGTON"| style_descr=="C-V WALLA WALLA WASHINGTON"| style_descr=="WALLA WALLA WASHINGTON"| style_descr=="WALLA WALLA VALLEY"| style_descr=="WALLA WALLA"| style_descr=="WALLA WALLA WASHINGTON STATE"| style_descr=="CVWWW"| style_descr=="CVWWWS"| style_descr=="CVWWWS RS")

*YAKIMA VALLEY
gen Yakima_Valley = (style_descr=="WASHINGTON STATE YAKIMA VALLEY"| style_descr=="CHAPEL BLOCK YAKIMA VALLEY"| style_descr=="C-V YAKIMA VALLEY"| style_descr=="YAKIMA VALLEY RS"| style_descr=="YAKIMA VALLEY"| brand_descr=="YAKIMA RIVER"| style_descr=="WASHINGTON YAKIMA VALLEY"| style_descr=="WSHNGTN ST YKM VLY"| style_descr=="ART DEN HOED YAKIMA VALLEY"| style_descr=="WS YAKIMA VALLEY")   

*Wahluke Slope
gen Wahluke_Slope = (style_descr=="C-V WAHLUKE SLOPE"| style_descr=="WAHLUKE SLOPE WASHINGTON"| style_descr=="C-V WAHLUKE SLOPE WASHINGTON"| style_descr=="WAHLUKE SLOPE"| style_descr=="C-V WSL"| style_descr=="C-V WSL WASHINGTON"| style_descr=="C-V WSL WASHINGTON STATE"| style_descr=="WAHLUKE SLOPE"| style_descr=="WSL"| style_descr=="WSL WASHINGTON"| style_descr=="WSL WASHINGTON STATE"| style_descr=="C-V WAHLUKE SLOPE"| style_descr=="WSL YAKIMA VALLEY"| style_descr=="MILBRANDT WSL")

*Columbia Valley
gen Columbia_Valley = (style_descr=="C-V"| style_descr=="C-V GOOSE MOUNTAIN RS"| style_descr=="C-V OREGON"| style_descr=="C-V RS"| style_descr=="C-V RS SELECTION"| style_descr=="C-V WASHINGTON"| style_descr=="C-V WASHINGTON RS"| style_descr=="C-V WASHINGTON STATE"| style_descr=="C-V WHITE BLUFFS"| style_descr=="C-V WSH"| style_descr=="Columbia Valley"| style_descr=="COLUMBIA VALLEY"|  style_descr=="WASHINGTON C-V"| style_descr=="C-V")


*Central Coast sub- AVA
*Hames Valley
gen Hames_Valley = (style_descr=="HAMES VALLEY MTRY"| style_descr=="CA HAMES VALLEY MTRY")

*San Bernabe
gen San_Bernabe = (style_descr=="MTRY SAN BERNABE"| style_descr=="MTRY COUNTY SAN BERNABE")

*Arroyo Grand
gen Arroyo_Grande = (style_descr=="ARROYO GRANDE VALLEY RS"| style_descr=="ARROYO GRANDE VALLEY")

*Arroyo Seco
gen Arroyo_Seco = (style_descr=="ARROYO SECO"| style_descr=="ARROYO SECO MTRY COUNTY"| style_descr=="ARROYO SECO MTRY"| style_descr=="ARROYO SECO MONTEREY"| style_descr=="ARROYO SECO CA"| style_descr=="ARROYO SECO CA MTRY"| style_descr=="ARROYO SECO CANYON"| style_descr=="ARROYO SECO CENTRAL COAST"| style_descr=="ARROYO SECO MTRY"| style_descr=="ARROYO SECO MTRY COUNTY"| style_descr=="ARROYO SECO MTRY RCR"| style_descr=="ARROYO SECO MTRY RIVA RANCH RS"| style_descr=="ARROYO SECO PVT RS"| style_descr=="ARROYO SECO RS")

gen Chalone =(style_descr=="CHALONE APPLELLATION"| style_descr=="CHALONE")

*Carmel
gen Carmel_Valley = (style_descr=="CA CARMEL VALLEY"| style_descr=="CARMEL VALLEY"| style_descr=="CARMEL VALLEY MTRY")

gen Cienega_Valley = (style_descr=="CIENEGA VALLEY")

*Edna
gen Edna_Valley = (style_descr=="CA EV"| style_descr=="EV"| style_descr=="EV RS"| style_descr=="EV RS SELECTION"| style_descr=="EV SLOC"| style_descr=="SLOC"| brand_descr=="EDNA VALLEY VINEYARD")

*LIVERMORE
gen Livermore_Valley = (style_descr=="CA LIVERMORE VALLEY"| style_descr=="CA LIVERMORE VALLEY SFB"| style_descr=="LIVERMORE VALLEY RS"| style_descr=="LIVERMORE VALLEY SFB"| style_descr=="LIVERMORE VALLEY SFB CRR"| style_descr=="LIVERMORE VALLEY SFB CWR"| style_descr=="LIVERMORE VALLEY")

*San Antonio
gen San_Antonio= (style_descr=="SAN ANTONIO DE VALERO"| style_descr=="SAN ANTONIO VALLEY"| brand_descr=="SAN ANTONIO")

*Santa Clara
gen Santa_Clara = (style_descr=="SANTA CLARA VALLEY"| style_descr=="SANTA CLARA VALLEY PVT RS"| style_descr=="CCCSCV")

*SAINT LUCIA
gen Saint_Lucia_Highlands = (style_descr=="CA SLH"| style_descr=="SLH"| style_descr=="SLH PVT RS"| style_descr=="SLH RS"| style_descr=="SLH RS SELECTION"| style_descr=="SAINT LUCIA HIGHLANDS"| style_descr=="SANTA LUCIA HIGHLANDS"| style_descr=="CENTRAL COAST SLH"| style_descr=="MTRY COUNTY SLH"| style_descr=="MTRY SLH")

*Santa Maria
gen Santa_Maria_Valley= (style_descr=="SMV"| brand_descr=="SANTA MARIA VINEYARD & WINERY"| brand_descr=="SANTA MARIA VINYRD & WNRY VLA!"| brand_descr=="VILLA SANTA MARIA WINERY"| style_descr=="SANTA BARBARA COUNTY SMV"| style_descr=="CA SANTA BARBARA SMV RS")


*Santa Yenze
gen Santa_Ynes_Valley = (style_descr=="RS SANTA YNEZ VALLEY"| style_descr=="SANTA YNEZ VALLEY"| style_descr=="SBC SANTA YNEZ VALLEY")

gen Sta_Rita_Hills = (style_descr=="SANTA RITA HILLS"| style_descr=="SBC SANTA RITA HILLS"| style_descr=="STA RITA HILLS"| style_descr=="STA RITA HILLS CA"| style_descr=="SBC STA RITA HILLS"| style_descr=="CA SANTA RITA HILLS")

*Monterey
gen Monterey_County = (style_descr=="CA MTRY COUNTY"| style_descr=="CA MC"| style_descr=="CA MTRY HIGHLANDS"| style_descr=="CA MTRY"| style_descr=="CA MNDCN MTRY CNTS SNM THE CR"| style_descr=="LC MTRY COUNTY SBC"| style_descr=="MTRY COUNTY PVT RS"| style_descr=="MTRY COUNTY RS"| style_descr=="MTRY COUNTY RS SELECTION"| style_descr=="MTRY COUNTY SBC"| style_descr=="MTRY COUNTY SBC GRAND RS"| style_descr=="MTRY COUNTY SBC SC"| style_descr=="MTRY COUNTY SC"| style_descr=="MTRY COUNTY W-R"| style_descr=="MTRY PINNACLES RANCHES"| style_descr=="MTRY RS"| style_descr=="MTRY SANTA BARBARA SC"| style_descr=="MTRY VERY SPECIAL RS"| style_descr=="MC MTRY COUNTRY"| style_descr=="MC MTRY COUNTY"| style_descr=="MONTEREY COUNTY"| style_descr=="MTRY COUNTY"| style_descr=="MTRY"| style_descr=="MTRY COUNTY NAPA COUNTY SC"| style_descr=="MONTEREY"| style_descr=="MTRY CO"| style_descr=="MTRY COUNTY GRAND RS"| style_descr=="MTRY COUNTY HARVEST RS"| style_descr=="MTRY COUNTY JAMES GANG RS"| style_descr=="MTRY COUNTY NAPA COUNTY SBC"| upc_descr=="TRUFFLE MRLT MTRY V RED DDT"| style_descr=="MTRYCOUNTY"| style_descr=="MTRY COUNTY"| style_descr=="MTRY COUNTY RS"|style_descr=="MTRY PINNACLES RANCHES"| style_descr=="MTRY COUNTY SC"| style_descr=="GLACIER RIDGE MTRY COUNTY"| style_descr=="COASTAL CA MTRY"| style_descr=="MC MTRY COUNTY SC"| style_descr=="MTRY CNTY SBC SC"| style_descr=="MTRY COUNTY SLOC SC"| style_descr=="MTRY SANTA BARBARA SONOMA"| style_descr=="MONTEREY COUNTY NAPA COUNTY"| brand_descr=="MONTEREY VINEYARD"| upc_descr=="AMC THRD P-N CA M-C V RED DDT"| style_descr=="CA MC SBC SC")

gen Paicines= (style_descr=="PAICINES"| style_descr=="CENTRAL COAST PAICINES")

*Paso Robles 
gen Paso_Robles = (style_descr=="CA PASO ROBLES"| style_descr=="HUERHUERO PASO ROBLES"| style_descr=="KIARA RS PASO ROBLES"| style_descr=="PASO ROBLES ESTATE RS"| style_descr=="PASO ROBLES JAMES GANG RS"| style_descr=="PASO ROBLES PVT RS"| style_descr=="PASO ROBLES RS"| style_descr=="PASO ROBLES SANTA ROSA"| style_descr=="PASO ROBLES SLOC"| style_descr=="PASO ROBLES TIERRA ROJA"| style_descr=="PASO ROBLES WEST COAST"| style_descr=="PASO ROBLES"| style_descr=="PASO CREEK"| style_descr=="A-C CA PSR SC"| style_descr=="PASO ROBLES PRINTERS ALLEY"| style_descr=="MOSSFIRE RANCH PASO ROBLES"| style_descr=="CA CENTRAL COAST PASO ROBLES"| brand_descr=="EL COTES DU PASO ROBLES")

gen pr=regexm(" " + upc_descr  + " ", " (PS-R) ")
replace Paso_Robles = 1 if pr==1 
drop pr


*Central Coast
gen Central_Coast = (style_descr=="CA CENTRAL COAST"| style_descr=="CA CENTRAL COAST CA RS"| style_descr=="Central Coast"| style_descr=="CENTRAL COAST HARVEST RS"| style_descr=="CENTRAL COAST"| style_descr=="CA CC"| style_descr=="CENTRAL COAST MTRY COUNTY"| style_descr=="CENTRAL COAST NAPA COUNTY"| style_descr=="CENTRAL COAST PVT RS"| style_descr=="CENTRAL COAST RS"| style_descr=="CENTRAL COAST S-S-R"| style_descr=="CENTRAL COAST SBC"| style_descr=="CENTRAL COAST SACRAMENTO DELTA"| style_descr=="CC"| style_descr=="CCC"| style_descr=="CCC RS SELECTION")

gen cc=regexm(" " + upc_descr  + " ", " (CC) ")
replace cc=0 if brand_descr=="TAYLOR CALIFORNIA CELLARS"| brand_descr=="COCO BAY"| brand_descr=="BISHOP CIDER CO."     //capturing abv from brand name
replace Central_Coast =1 if cc==1 
drop cc

*Chalk Hill
gen Chalk_Hill = (style_descr=="CHALK HILL"| style_descr=="CHALK HILL RRV"| style_descr=="CHALK HILL SC"| style_descr=="CHALK HILL SONOMA COUNTY RS")

gen Chehalem_Mountains = (style_descr=="CHEHALEM MOUNTAINS")

*Dry Creek Valley
gen Dry_Creek_Valley = (style_descr=="CA DCV"| style_descr=="DCV"| style_descr=="DCV RS"| style_descr=="CA DCV SC"| style_descr=="CA DCV RS SELECTION"| style_descr=="CA DCV RS"| style_descr=="Dry Creek Valley"| style_descr=="DCV SC"| style_descr=="DCV TELDESCHI"| style_descr=="Dry Creek Valley"| style_descr=="DRY GREEK VALLEY SC"| brand_descr=="DRY CREEK VINEYARD")

*Eagle Peak
gen Eagle_Peak = (style_descr=="CA EAGLE PEAK")

*Clarksburg
gen Clarksburg = (style_descr=="CA CLARKSBURG"| style_descr=="CLARKSBURG"| style_descr=="CLARKSBURG SC"| style_descr=="CLARKSBURG LATE HARVEST")

gen Knights_Valley = (style_descr=="CA KNIGHTS VALLEY"| style_descr=="KNIGHTS VALLEY RS"| style_descr=="KNIGHTS VALLEY SC"| style_descr=="KNIGHTS VALLEY")

*LODI
gen Lodi =  (style_descr=="CA LODI"| style_descr=="CA LODI REGION" | style_descr=="LODI style_descr"| style_descr=="LODI RHONE"| style_descr=="LODI RS SELECTION"| style_descr=="LODI WV"| style_descr=="LODI SANTA ROSA"| style_descr=="LODI RS"| style_descr=="LODI CLUB RS"| style_descr=="LODI REGION"| style_descr=="LODI"| style_descr=="LODI NAPA COUNTY"| style_descr=="LC LODI COUNTY NAPA COUNTY"| style_descr=="LODI NAPA"| style_descr=="LODI CA"| style_descr=="ACAMPO LODI"| style_descr=="ACAMPO LODI RS"| style_descr=="CA LDI"| style_descr=="BRAMBLEWOOD LODI"| style_descr=="CA LODI REGION"| style_descr=="LODI SC"| style_descr=="AMADOR CA LODI SONOMA"| upc_descr=="HEAVYWEIGHT CHRD LDI V WT DDT"| style_descr=="CA LODI NAPA")

replace Lodi=1 if  upc_descr=="ITO RED LDI RHE G RED DDT"| upc_descr=="CTL BR CB-S CA LDI V RED DDT"| upc_descr=="CTL BR RB CA LDI G RED DDT"

*Mendocino
gen Mendocino = (style_descr=="CA MENDOCINO"| style_descr=="MENDOCINO CA"| style_descr=="MENDOCINO UPLANDS"| style_descr=="MENDOCINO UPPER RUSSIAN RIVER"| style_descr=="MC GRAND RS"| style_descr=="MC UKIAH VALLEY"| style_descr=="MC"| style_descr=="MENDOCINO"| style_descr=="CALIFORNIA MENDOCINO"| style_descr=="MC SBC SC"| style_descr=="LEXIS ESTATE MENDOCINO"| upc_descr=="ATZ ZN MD-C UKHVLY V RED DDT"| style_descr=="MC NCSM")

*san lucas
gen San_Lucas = (style_descr=="MTRY COUNTY SAN LUCAS")

*SAN BENITO
gen San_Benito = (style_descr=="SAN BENITO"| style_descr=="MTRY COUNTY SAN BENITO SC")

*Napa vally sub- ava
gen Stags_Leap_District = (style_descr=="NAPA VALLEY SLD"| style_descr=="CA NAPA VALLEY SLD")
gen Spring_Mountain_District = (style_descr=="NAPA VALLEY SMD"| style_descr=="NAPA VALLEY SPRING MOUNTAIN"| style_descr=="CA NAPA VALLEY SMD")
gen Chiles_Valley = (style_descr=="CHILES VALLEY NAPA")
gen Atlas_Peak = (style_descr=="ATLAS PEAK NAPA VALLEY")
gen Calistoga = (style_descr=="CALISTOGA"| style_descr=="CA CALISTOGA NAPA VALLEY"| style_descr=="CALISTOGA NAPA VALLEY") 
gen Howell_Mountain = (style_descr=="HOWELL MOUNTAIN"| style_descr=="HOWELL MOUNTAIN NAPA VALLEY")

*CARNEROS
gen Carneros = (style_descr=="CARNEROS SC"| style_descr=="CARNEROS SONOMA"| style_descr=="CARNEROS NAPA COUNTY"| style_descr=="CARNEROS"| style_descr=="CA CARNEROS"| style_descr=="CA CARNEROS RS"| style_descr=="LOS CARNEROS"| style_descr=="LOS CARNEROS SC"| style_descr=="RS CARNEROS"| style_descr=="LOS CARNEROS NAPA VALLEY"| style_descr=="CARNEROS DISTRICT"| style_descr=="CARNEROS ESTATE RS"| style_descr=="CARNEROS RS"| style_descr=="CARNEROS NAPA VALLEY"| style_descr=="CARNEROS DISTRICT SC"| style_descr=="RS NAPA CARNEROS"| brand_descr=="CARNEROS CREEK WINERY" | brand_descr=="CARNEROS HIGHWAY"| brand_descr =="DOMAINE CARNEROS"|  brand_descr=="BUENA VISTA CARNEROS" )     //most brand name also has carneros in style descr

gen Oak_Knoll = (style_descr=="NAPA VALLEY OAK KNOLL"| style_descr=="NAPA VALLEY OAK KNOLL DISTRICT"| style_descr=="NAPA VALLEY OKL-DST")

*Rutherford
gen Rutherford = (style_descr=="CA NAPA COUNTY RUTHERFORD"| style_descr=="NAPA RUTHERFORD APPELLATION RS"| style_descr=="NAPA VALLEY RUTHERFORD"| style_descr=="RUTHERFORD NAPA VALLEY"| style_descr=="RUTHERFORD"| style_descr=="RUTHERFORD RS"| style_descr=="NAPA VALLEY RUTHERFORD RS"| brand_descr=="RUTHERFORD"| brand_descr=="RUTHERFORD ESTATE CELLARS"| brand_descr=="RUTHERFORD RANCH"| brand_descr=="RUTHERFORD VINTNERS"| style_descr=="RUTHERFORD DISTRICT")

*ST HELENA
gen St_Helena = (style_descr=="CA ST HELENA"| style_descr=="ST HELENA"| style_descr=="NAPA VALLEY ST HELENA"| style_descr=="CARNEROS DISTRICT ST HELENA")

gen Yountville = (style_descr=="YOUNTVILLE"| style_descr=="NAPA VALLEY YOUNTVILLE"| style_descr=="CALIF NAPA VALLEY YOUNTVILLE")

*Napa Valley
gen Napa_Valley = (style_descr=="NAPA VALLEY"| style_descr=="NAPA VALLEY CA"| style_descr=="NAPA VALLEY EVENSTAD RS"| style_descr=="NAPA VALLEY FAMILY RS"| style_descr=="NAPA VALLEY GRAND RS"| style_descr=="NAPA VALLEY MARIOS RESERVE"| style_descr=="NAPA VALLEY PVT RS"| style_descr=="NAPA VALLEY RS"| style_descr=="NAPA VALLEY RS SELECTION"|  style_descr=="NAPA VALLEY S-V"| style_descr=="NAPA VALLEY SONOMA COAST SC"| style_descr=="NAPA VALLEY SPECIAL RS"| style_descr=="NAPA VALLEY TPFR"| style_descr=="NAPPA VALLEY RS"| style_descr=="NAPA"| style_descr=="NAPA COUNTY NAPA VALLEY"| style_descr=="NAPA NAPA VALLEY"| brand_descr=="NAPA VALLEY VINEYARDS"| style_descr=="MT VEEDER NAPA VALLEY"| style_descr=="CA NAPA SONOMA"| style_descr=="CA NAPA VALLEY"| style_descr=="CA NAPA VALLEY RS"| style_descr=="CA NAPA VALLEY S-C"| style_descr=="AM CANYON NAPA VALLEY"| style_descr=="ESTHERS RS NAPA VALLEY"| style_descr=="MOUNT VEEDER NAPA VALLEY"| style_descr=="SBRAGIA L-R NAPA VALLEY"| style_descr=="NAPA NORTH COAST"| upc_descr=="B SIDE RED FFSNV G RED DDT"| upc_descr=="DECOY SV-B NV V WT DDT"| style_descr=="CA NAPA"| brand_descr=="NAPA CREEK"| brand_descr=="NAPA LANDING"| brand_descr=="NAPA RIDGE NAPA VALLEY"| style_descr=="NCSJCSC"| style_descr=="ATLAS PARK NAPA VALLEY"| style_descr=="CENTRAL COAST NAPA VALLEY"| style_descr=="CENTRAL COAST NAPA VALLEY S-V"| style_descr=="LODI NAPA VALLEY"| brand_descr=="MUMM NAPA"| brand_descr=="SCREW KAPPA NAPA") //      FFSNV-NV is napa, for most entry screw napa has napa valley in style decpr, and mumm napa also capturing from brand name; also most prodcuts are with napavalley

gen NV= regexm(" " + upc_descr  + " ", " (NV) ")
replace NV=0 if brand_descr=="NOVAS"
replace Napa_Valley =1 if NV==1          //checked - all capturing domestic and no other word in brand descr that would capture NV
replace Napa_Valley =1 if upc_descr=="CHNDN DM CHM WT NAPA BR XD SP"

*Napa County
gen Napa_County = (style_descr=="CA NAPA COUNTY"| style_descr=="CA NAPA COUNTY SC"| style_descr=="AM CANYON NAPA COUNTY"| style_descr=="LC NAPA COUNTY SC"| style_descr=="NAPA COUNTY"| style_descr=="NAPA COUNTY RS"| style_descr=="NAPA COUNTY SBC"| style_descr=="NAPA COUNTY SC"| style_descr=="NAPA COUNTY SLD"| style_descr=="NAPA COUNTY SLOC"| style_descr=="NAPA COUNTY SONOMA"| style_descr=="MC NAPA COUNTY SC"| style_descr=="LC MTRY NAPA SANTRA BARBARA"| style_descr=="AMADOR COUNTY MC NAPA COUNTY"| style_descr=="CA LC MTRY NAPA SANTA BARBARA"| style_descr=="MTRY COUNTY NAPA COUNTY SC"| style_descr=="NAPA COUNTY SONOMA COUNTY"| upc_descr=="CTL BR SV-B N-C V WT BB DDT"| upc_descr=="PCFCPK CB-S N-C V RED DDT")

*North Coast
gen North_Coast = (style_descr=="CA NORTH COAST"| style_descr=="CA NORTH COAST CA RS"| style_descr=="CA NORTH COAST CA RS"| style_descr=="CA NORTH COAST SC"| style_descr=="LC MC NORTH COAST SC"| style_descr=="NORTH COAST SC RS"| style_descr=="NORTH COAST VINTNERS BLEND"| style_descr=="NORTH COAST"| style_descr=="CENTRAL COAST LODI NORTH COAST"| style_descr=="MC NAPA COUNTY NORTH COAST SC" )

gen northcoast= regexm(" " + upc_descr  + " ", " (NC) ")
replace North_Coast = 1 if northcoast==1 
drop northcoast

*Guenoc
gen Guenoc = (style_descr=="CA GUENOC VALLEY"| style_descr=="GUENOC VALLEY"| brand_descr=="GUENOC")

*Russian River Valley
gen Russian_River_Valley = (style_descr=="CA RRV"| style_descr=="CA RRV RS"| style_descr=="CA RRV SONOMA"| style_descr=="CA RRV SC RS"| style_descr=="RRV RS"| style_descr=="RRV SC"| style_descr=="RRV SC GRANDE RS"| style_descr=="RRV SMALL LOT RS"| style_descr=="RRV SPECIAL RS"| style_descr=="RRV WEST COAST"| style_descr=="RUSSIAN RIVER"| style_descr=="RRV SC BARREL FERMENTED"| style_descr=="RRV SC RS"| style_descr=="RRV W-R"| style_descr=="RRV SONOMA RS"| style_descr=="RRV"| style_descr=="CA RRV SC"| style_descr=="CA RRV SONOMA RS"| style_descr=="CRRVSC"| style_descr=="GVORRV SC"| style_descr=="NORTHERN SONOMA RRV RS"| style_descr=="ESTATE RRV")

*Happy canyon of SBC is a AVA - need to check
gen Santa_Barbara_County = (style_descr=="SBC"| style_descr=="SBC VINTNERS RS"| style_descr=="CA SBC"| style_descr=="CA VINO ROSSO DI SANTA BARBARA"| style_descr=="SBC"| brand_descr=="SANTA BARBARA"| upc_descr=="ZINNIA P-N SBC RS V RED DDT"| upc_descr=="TRN FM SV-B SBC V WT DDT"| style_descr=="SANTA BARBARA"| style_descr=="SBC RS"| style_descr=="SBC RS SELECTION"| style_descr=="MALIBU SBC"| style_descr=="SANTA BARBARA COUNTY")
 
*Shenandoah
gen Shenandoah_Valley_Cal = (style_descr=="CA SHENANDOAH VALLEY"| style_descr=="CA SHENANDOAH VALLEY RS")
gen Shenandoah_Valley =  (style_descr=="SHENANDOAH VALLEY"| brand_descr=="SHENANDOAH")

*Northern Sonoma
gen Northern_Sonoma = (style_descr=="NORTHERN SONOMA"| style_descr=="NORTHERN SONOMA RS")

*Sonmoa Mountian
gen Sonoma_Mountain = (style_descr=="SONOMA MOUNTAIN")

*Sonoma Coast
gen Sonoma_Coast = (style_descr=="SONOMA COAST"| style_descr=="CA SONOMA COAST"| style_descr=="CALIF SONOMA COAST SC"| style_descr=="CA SONOMA COAST RS"| style_descr=="CA SONOMA COAST RS"| style_descr=="ESTATE SONOMA COAST"| style_descr=="SONOMA COAST"| style_descr=="SONOMA COAST BARREL RS"| style_descr=="SONOMA COAST SC"| style_descr=="LES PIERRES SONOMA COAST"| style_descr=="CA SC SC"| style_descr=="CA SONOMA COAST SC"| style_descr=="LES PIERRES SONOMA COAST"| style_descr=="SONOMA COAST SONOMA COUNTY"| style_descr=="SC SONOMA COUNTY")

*Sonoma Valley
gen Sonoma_Valley = (style_descr=="S-V"| style_descr=="S-V JACK LONDON VINEYARD"| style_descr=="S-V PVT RS"| style_descr=="S-V RS"| style_descr=="CA NORTHERN S-V"| style_descr=="SC S-V")

*Only Sonoma- Not ava-NEED TO CHECK
gen Sonoma = (style_descr=="CA SONOMA"| style_descr=="CLARK HILL SONOMA COUNTY"| style_descr=="SONOMA COUNTY"| style_descr=="SONOMA RS"| style_descr=="SONOMA VINTNERS SELECT"| style_descr=="SC"| style_descr=="SONOMA"| style_descr=="SONOMA BEACH SC"| style_descr=="SC RS"| style_descr=="SC SONOMA RS"| style_descr=="PVT RS SC"| style_descr=="GRATON SONOMA COUNTY CA"| style_descr=="CA SC"| style_descr=="SAN JOAQUIN COUNTY SC"| style_descr=="SC PVT RS"| style_descr=="SLOC SC"| style_descr=="COUNTY SC"| style_descr=="MC SC"| style_descr=="SC PVT RS"| style_descr=="ANNAPOLIS CA SC")


*SOUTH COAST
gen South_Coast = (style_descr=="CA SOUTH COAST RS"| style_descr=="CA SOUTH COAST"| style_descr=="SOUTH COAST"| brand_descr=="SOUTH COAST WINERY")

*SUISUN VALLEY
gen Suisun_Valley = (style_descr=="CA SUISUN VALLEY"| style_descr=="SUISUN VALLEY")

*SANTA CRUZ MOUNTIAN  
gen Santa_Cruz_Mountain = (style_descr=="CA SCM"| style_descr=="SANTA CRUZ")

*WILLAMETTE 
gen Willamette_Valley = (style_descr=="WV"| style_descr=="OREGON WV"| style_descr=="WILLIAMETTE VALLEY RS"| style_descr=="OREGON WV RS"| style_descr=="WILLIAMETTE VALLEY"| style_descr=="OREGON WILLAMETTE VALLEY"| style_descr=="OREGON WLMT VLY"| brand_descr=="WILLAMETTE VALLEY"| style_descr=="OREGON WILLIAMETTE VALLEY")

gen match_wv= regexm(" " + upc_descr  + " ", " (WV) ")
replace match_wv=0 if brand_descr=="WV"    //replace Williamtte valley =0 if WV is for brand name (wine at versailles)
replace Willamette_Valley=1 if match_wv==1 
drop match_wv

*Applegate
gen Applegate_Valley = (style_descr=="APLGTE VALLEY")

*Red Hills Lake
gen Red_Hills_Lake_County = (style_descr=="CA CA RED HILLS LAKE COUNTY"| style_descr=="CA RED HILLS LC"| style_descr=="RED HILL LC"| style_descr=="RED HILLS LAKE"| style_descr=="RED HILLS LC"| style_descr=="RED HILLS LAKE COUNTY"| style_descr=="CA RED HILLS LAKE COUNTY")

gen Clear_Lake = (style_descr=="CLEAR LAKE LC"| style_descr=="CLEAR LAKE")

gen Dundee_Hills = (style_descr=="DUNDEE OREGON"| style_descr=="DUNDEE HILLS"| style_descr=="DUNDEE HILLS OREGON")

gen Dunnigan_Hills = (style_descr=="DUNNINGAN HILLS"| style_descr=="DUNNIGAN HILLS MUSQUE CLONE"| style_descr=="DUNNIGAN HILLS")

gen El_Dorado = (style_descr=="EL DORADO"| style_descr=="ELDORADO COUNTRY RS"| style_descr=="ELDORADO COUNTY")

gen Eola_Hills = (style_descr=="EOLA AMITY HILLS"| style_descr=="EOLA AMITY HILLS RS SERIES"| style_descr=="EOLA AMITY HILLS WV"| style_descr=="EOLA HILLS"| brand_descr=="EOLA HILLS")

gen Grand_River_Valley = (style_descr=="GRAND RIVER VALLEY OHIO"| style_descr=="GRAND RIVER VALLEY")

gen High_Valley =  (style_descr=="HIGH VALLEY APPELLATION"| style_descr=="HIGH VALLEY LC"| style_descr=="HIGH VALLEY LC RS"| style_descr=="HIGH VALLEY LC TWO BUD BLOCK"| style_descr=="CA HIGH VALLEY LC"| style_descr=="HIGH VALLEY")

gen Lake_Michigan_Shore = (style_descr=="LAKE MICHIGAN SHORE MICHIGAN"| style_descr=="LAKE MICHIGAN SHORE RS"| style_descr=="LAKE MICHIGAN SHORE")

gen Long_Island = (style_descr=="NORTH FORK OF LONG ISLAND"| style_descr=="NORTH FORK OF LONG ISLAND RS"| style_descr=="LONG ISLAND"| style_descr=="THE NORTH FORK OF LONG ISLAND"| style_descr=="LONG ISLAND NEW YORK SAGAPONCK"| style_descr=="LONG ISLAND NEW YORK")

gen Umpqua_Valley = (style_descr=="OREGON UMPQUA VALLEY"| style_descr=="SOUTHERN OREGON UMPQUA VALLEY"| style_descr=="UMPQUA VALLEY")

gen Madera = (style_descr=="MADERA")

gen Isle_St_George = (style_descr=="ISLE ST GEORGE")

gen Ohio_River_Valley = (style_descr=="OHIO RIVER VALLEY")

gen Finger_Lakes = (style_descr=="FINGER LAKES SENECA LAKE"|  style_descr=="FINGER LAKES")

gen Lake_Erie = (style_descr=="CHAUTAUQUA REGION LAKE ERIE"| brand_descr=="LAKE ERIE"| style_descr=="LAKE ERIE")

gen Yamhill_Carlton = (style_descr=="YAMHILL VALLEY"| style_descr=="OREGON YAMHILL CARLTON")

gen Mcminnville = (style_descr=="MCMINNVILLE A V A"| style_descr=="MCMINNVILLE A V A OREGON WV"| style_descr=="MCMINNVILLE A V A OREGON")

gen Rogue_Valley = (style_descr=="OREGON ROGUE VALLEY"| style_descr=="ROGUE VALLEY SOUTHERN OREGON"| style_descr=="OREGONS ROGUE VALLEY"| style_descr=="OREGON ROGUE VALLEY"| style_descr=="ROGUE VALLEY"| style_descr=="OREGON ROGUE VALLEY")

gen Snake_River_Valley = (style_descr=="IDAHO SNAKE RIVER VALLEY AVA"| brand_descr=="SNAKE RIVER"| style_descr=="SNAKE RIVER VALLEY")

gen Colorado_Grand_Valley = (style_descr=="COLORADO GRAND VALLEY"| style_descr=="COLORADO EOM GRAND VALLEY")

gen Lancaster_Valley = (style_descr=="LANCASTER VALLEY")

gen Lehigh_Va = (style_descr=="LEHIGH VA"| style_descr=="LEHIGH VALLEY")

gen Leelanau_Peninsula = (style_descr=="LEELANAU PENINSULA"| style_descr=="OLD MISSION PENINSULA"| style_descr=="OLD MISSION PENINSULA RS"| style_descr=="OLD MISSION PENISULA"| style_descr=="MICHIGAN OLD MISSION PENINSULA"| style_descr=="MICHIGAN OLD MISSION PENINSULA")

gen Ozark_Hignlands =  (style_descr=="OZARK HIGHLANDS"| style_descr=="OZARK MOUNTAIN")
gen Yadkin_Valley = (style_descr=="YADKIN VALLEY"| style_descr=="NORTH CAROLINA YADKIN VALLEY")
gen Texas_High_Plains =  (style_descr=="TEXAS HIGH PLAINS")
gen Texas_Hill_Country = (style_descr=="TEXAS HILL COUNTRY")
gen Altus = (style_descr=="ALTUS"| style_descr=="ALTUS ARKANSAS")
gen Monticello = (style_descr=="MONTICELLO"| style_descr=="MONTICELLO RS")

gen Redwood_Valley =  (style_descr=="REDWOOD VALLEY"| style_descr=="MC REDWOOD VALLEY"| style_descr=="MENDOCINO REDWOOD VALLEY")

gen Solano_County_Green_Valley = (style_descr=="GREEN VALLEY SOLANO COUNTY"| style_descr=="GREEN VALLEY SC"| style_descr=="GREEN VALLEY"| style_descr=="CA GREEN VALLEY SOLANO COUNTY"| style_descr=="SOLANO COUNTY")

gen Augusta = (style_descr=="AUGUSTA"| brand_descr=="AUGUSTA")

gen Sierra_Foothills = (style_descr=="SIERRA FOOTHILLS"| style_descr=="AMADOR COUNTY SIERRA FOOTHILLS"| style_descr=="CA SIERRA FOOTHILLS"| style_descr=="PLACER COUNTY SIERRA FOOTHILLS"| style_descr=="SIENA SC"| style_descr=="SIERRA FOOTHILL"| style_descr=="SIERRA FOOTHILL GRAND RS"| brand_descr=="SIERRA COLD"| brand_descr=="SIERRA VISTA")

gen Amador_County = (style_descr=="AMADOR COUNTY CA"| style_descr=="AMADOR COUNTY COUGAR HILL"| style_descr=="AMADOR COUNTY J & S RS"| style_descr=="AMADOR COUNTY MC"| style_descr=="AMADOR COUNTY OLD VINE RS"| style_descr=="AMADOR COUNTY")  

gen Columbia_Gorge = (style_descr=="COLUMBIA GORGE")

gen Cucamonga_Valley = (style_descr=="CUCAMONGA VALLEY")

gen Fiddletown = (style_descr=="FIDDLETOWN")

gen Lake_Chelan = (style_descr=="LAKE CHELAN")

gen Oakville = (style_descr=="OAKVILLE"| style_descr=="NAPA VALLEY OAKVILLE"| style_descr=="NAPA VALLEY OAKVILLE RS"| style_descr=="CALIF NAPA VALLEY OAKVILLE"| style_descr=="CA NAPA VALLEY OAKVILLE")

gen Rockpile = (style_descr=="ROCKPILE"| style_descr=="ROCKPILE SC")

gen Tracy_Hills = (style_descr=="TRACY HILLS")

gen Yorkville_Highlands = (style_descr=="YORKVILLE HIGHLANDS"| style_descr=="MENDOCINO YORKVILLE HIGHLANDS")

gen Mcdowell_Valley = (brand_descr=="MCDOWELL VALLEY VINEYARDS")

gen Temecula_Valley =  (style_descr=="COASTAL RS TEMECULE VALLEY"| style_descr=="TEMECULA"| style_descr=="TEMECULA VALLEY")
gen Southeastern_New_England = (style_descr=="SOUTHEASTERN NEW ENGLAND")
gen Lake_County = (style_descr=="LC RS"| style_descr=="LC"| style_descr=="CA LC"| style_descr=="CALIF LC SHANNON RIDGE"| style_descr=="LC MC")
gen Potter_Valley = (style_descr=="POTTER VALLEY"| style_descr=="CA POTTER VALLEY")

gen ava = Alexander_Valley+ Anderson_Valley+ ALOCV+ Horse_Heaven_Hills+ Rattle_Snake_Hills+ Red_Mountain+ Walla_Walla+ Yakima_Valley+ Wahluke_Slope+ Columbia_Valley+ Hames_Valley+ San_Bernabe+ Arroyo_Grande+ Arroyo_Seco+ Chalone+ Carmel_Valley+ Cienega_Valley+ Edna_Valley+ Livermore_Valley+ San_Antonio+ Santa_Clara+ Saint_Lucia_Highlands+ Santa_Maria_Valley+ Santa_Ynes_Valley+ Sta_Rita_Hills+ Monterey_County+ Paicines+ Paso_Robles+ Central_Coast+ Chalk_Hill+ Chehalem_Mountains+ Dry_Creek_Valley+ Eagle_Peak+ Clarksburg+ Knights_Valley+ Lodi+ Mendocino+ San_Lucas+ San_Benito+ Stags_Leap_District+ Spring_Mountain_District+ Chiles_Valley+ Atlas_Peak+ Calistoga+ Howell_Mountain+ Carneros+ Oak_Knoll+ Rutherford+ St_Helena+ Yountville+ Napa_Valley+ Napa_County+ North_Coast+ Guenoc+ Russian_River_Valley+ Santa_Barbara_County+ Shenandoah_Valley_Cal+ Shenandoah_Valley+ Northern_Sonoma+ Sonoma_Mountain+ Sonoma_Coast+ Sonoma_Valley+ Sonoma+ South_Coast+ Suisun_Valley+ Santa_Cruz_Mountain+ Willamette_Valley+ Applegate_Valley+ Red_Hills_Lake_County+ Clear_Lake+ Dundee_Hills+ Dunnigan_Hills+ El_Dorado+ Eola_Hills+ Grand_River_Valley+ High_Valley+ Lake_Michigan_Shore+ Long_Island+ Umpqua_Valley+ Madera+ Isle_St_George+ Ohio_River_Valley+ Finger_Lakes+ Lake_Erie+ Yamhill_Carlton+ Mcminnville+ Rogue_Valley+ Snake_River_Valley+ Colorado_Grand_Valley+ Lancaster_Valley+ Lehigh_Va+ Leelanau_Peninsula+ Ozark_Hignlands+ Yadkin_Valley+ Texas_High_Plains+ Texas_Hill_Country+ Altus+ Monticello+ Redwood_Valley+ Solano_County_Green_Valley+ Augusta+ Sierra_Foothills+ Amador_County+ Columbia_Gorge+ Cucamonga_Valley+ Fiddletown+ Lake_Chelan+ Oakville+ Rockpile+ Tracy_Hills+ Yorkville_Highlands+ Mcdowell_Valley+ Temecula_Valley+ Southeastern_New_England+ Lake_County+ Potter_Valley

replace Columbia_Valley = 0 if Horse_Heaven_Hills ==1
replace Central_Coast = 0 if Monterey_County ==1
replace Napa_Valley =0 if Stags_Leap_District==1
replace Napa_Valley=0 if Russian_River_Valley ==1
replace Central_Coast =0 if Edna_Valley ==1
replace Napa_Valley=0 if Carneros ==1
replace Central_Coast =0 if Livermore_Valley ==1
replace Napa_Valley =0 if Rutherford ==1
replace North_Coast =0 if Lake_County  ==1
replace Napa_Valley =0 if Alexander_Valley ==1
replace Sonoma=0 if Anderson_Valley ==1
replace Sonoma=0 if Alexander_Valley  ==1
replace South_Coast =0 if Temecula_Valley ==1
replace Columbia_Valley =0 if Willamette_Valley ==1
replace Willamette_Valley=0 if Eola_Hills ==1
replace Monterey_County =0 if Arroyo_Seco ==1
replace Paso_Robles =0 if San_Antonio ==1
replace Monterey_County =0 if Saint_Lucia_Highlands ==1
replace Napa_County =0 if Monterey_County ==1
replace Central_Coast =0 if Paicines ==1
replace Santa_Barbara_County =0 if Central_Coast ==1
replace Sonoma=0 if Dry_Creek_Valley ==1
replace Dry_Creek_Valley =0 if Russian_River_Valley ==1
replace Monterey_County =0 if Lodi ==1
replace Napa_Valley =0 if Spring_Mountain_District ==1
replace Napa_Valley =0 if Calistoga ==1
replace Carneros =0 if Sonoma_Valley ==1
replace Carneros =0 if Sonoma==1
replace Napa_Valley =0 if St_Helena ==1
replace Sonoma=0 if Napa_Valley ==1
replace North_Coast =0 if Napa_Valley ==1
replace Lodi =0 if Napa_Valley ==1
replace Napa_County =0 if Napa_Valley ==1
replace Napa_Valley =0 if Oakville ==1
replace Sonoma_Valley =0 if Napa_Valley ==1
replace Guenoc =0 if North_Coast ==1
replace North_Coast =0 if Sonoma_Coast ==1
replace Guenoc =0 if Lake_County ==1
replace Central_Coast =0 if Russian_River_Valley ==1
replace Sonoma=0 if Northern_Sonoma ==1
replace El_Dorado =0 if Sonoma_Mountain ==1
replace Sonoma =0 if Sonoma_Mountain ==1
replace Sierra_Foothills =0 if El_Dorado ==1
replace Eola_Hills =0 if Lodi ==1
drop ava
gen ava = Alexander_Valley+ Anderson_Valley+ ALOCV+ Horse_Heaven_Hills+ Rattle_Snake_Hills+ Red_Mountain+ Walla_Walla+ Yakima_Valley+ Wahluke_Slope+ Columbia_Valley+ Hames_Valley+ San_Bernabe+ Arroyo_Grande+ Arroyo_Seco+ Chalone+ Carmel_Valley+ Cienega_Valley+ Edna_Valley+ Livermore_Valley+ San_Antonio+ Santa_Clara+ Saint_Lucia_Highlands+ Santa_Maria_Valley+ Santa_Ynes_Valley+ Sta_Rita_Hills+ Monterey_County+ Paicines+ Paso_Robles+ Central_Coast+ Chalk_Hill+ Chehalem_Mountains+ Dry_Creek_Valley+ Eagle_Peak+ Clarksburg+ Knights_Valley+ Lodi+ Mendocino+ San_Lucas+ San_Benito+ Stags_Leap_District+ Spring_Mountain_District+ Chiles_Valley+ Atlas_Peak+ Calistoga+ Howell_Mountain+ Carneros+ Oak_Knoll+ Rutherford+ St_Helena+ Yountville+ Napa_Valley+ Napa_County+ North_Coast+ Guenoc+ Russian_River_Valley+ Santa_Barbara_County+ Shenandoah_Valley_Cal+ Shenandoah_Valley+ Northern_Sonoma+ Sonoma_Mountain+ Sonoma_Coast+ Sonoma_Valley+ Sonoma+ South_Coast+ Suisun_Valley+ Santa_Cruz_Mountain+ Willamette_Valley+ Applegate_Valley+ Red_Hills_Lake_County+ Clear_Lake+ Dundee_Hills+ Dunnigan_Hills+ El_Dorado+ Eola_Hills+ Grand_River_Valley+ High_Valley+ Lake_Michigan_Shore+ Long_Island+ Umpqua_Valley+ Madera+ Isle_St_George+ Ohio_River_Valley+ Finger_Lakes+ Lake_Erie+ Yamhill_Carlton+ Mcminnville+ Rogue_Valley+ Snake_River_Valley+ Colorado_Grand_Valley+ Lancaster_Valley+ Lehigh_Va+ Leelanau_Peninsula+ Ozark_Hignlands+ Yadkin_Valley+ Texas_High_Plains+ Texas_Hill_Country+ Altus+ Monticello+ Redwood_Valley+ Solano_County_Green_Valley+ Augusta+ Sierra_Foothills+ Amador_County+ Columbia_Gorge+ Cucamonga_Valley+ Fiddletown+ Lake_Chelan+ Oakville+ Rockpile+ Tracy_Hills+ Yorkville_Highlands+ Mcdowell_Valley+ Temecula_Valley+ Southeastern_New_England+ Lake_County+ Potter_Valley
replace Central_Coast =0 if ava==2       //for last nine overlap

replace wine_appellation="Alexander_Valley" if Alexander_Valley==1
replace wine_appellation="Anderson_Valley" if Anderson_Valley==1 
replace wine_appellation="ALOCV" if ALOCV==1
replace wine_appellation="Horse_Heaven_Hills" if Horse_Heaven_Hills==1 
replace wine_appellation="Rattle_Snake_Hills" if Rattle_Snake_Hills==1
replace wine_appellation="Red_Mountain" if Red_Mountain==1
replace wine_appellation="Walla_Walla" if Walla_Walla==1
replace wine_appellation="Yakima_Valley" if Yakima_Valley==1
replace wine_appellation="Wahluke_Slope" if Wahluke_Slope==1 
replace wine_appellation="Columbia_Valley" if  Columbia_Valley==1
replace wine_appellation="Hames_Valley" if Hames_Valley==1
replace wine_appellation="San_Bernabe" if  San_Bernabe==1
replace wine_appellation="Arroyo_Grande" if Arroyo_Grande==1 
replace wine_appellation="Arroyo_Seco" if  Arroyo_Seco==1
replace wine_appellation="Chalone" if Chalone==1
replace wine_appellation="Carmel_Valley" if Carmel_Valley==1
replace wine_appellation="Cienega_Valley" if Cienega_Valley==1
replace wine_appellation="Edna_Valley" if Edna_Valley==1  
replace wine_appellation="Livermore_Valley" if Livermore_Valley==1
replace wine_appellation="San_Antonio" if San_Antonio==1 
replace wine_appellation="Santa_Clara" if Santa_Clara==1 
replace wine_appellation="Saint_Lucia_Highlands" if Saint_Lucia_Highlands==1
replace wine_appellation="Santa_Maria_Valley" if Santa_Maria_Valley==1
replace wine_appellation="Santa_Ynes_Valley" if Santa_Ynes_Valley==1 
replace wine_appellation="Sta_Rita_Hills" if  Sta_Rita_Hills==1
replace wine_appellation="Monterey_County" if  Monterey_County==1
replace wine_appellation="Paicines" if Paicines==1 
replace wine_appellation="Paso_Robles" if Paso_Robles==1 
replace wine_appellation="Central_Coast" if Central_Coast==1 
replace wine_appellation="Chalk_Hill" if  Chalk_Hill==1
replace wine_appellation="Chehalem_Mountains" if  Chehalem_Mountains==1
replace wine_appellation="Dry_Creek_Valley" if  Dry_Creek_Valley==1
replace wine_appellation="Eagle_Peak" if  Eagle_Peak==1
replace wine_appellation="Clarksburg" if Clarksburg==1
replace wine_appellation="Knights_Valley" if  Knights_Valley==1
replace wine_appellation="Lodi" if  Lodi==1
replace wine_appellation="Mendocino" if  Mendocino==1
replace wine_appellation="San_Lucas" if  San_Lucas==1
replace wine_appellation="San_Benito" if  San_Benito==1
replace wine_appellation="Stags_Leap_District" if  Stags_Leap_District==1
replace wine_appellation="Spring_Mountain_District" if  Spring_Mountain_District==1
replace wine_appellation="Chiles_Valley" if  Chiles_Valley==1
replace wine_appellation="Atlas_Peak" if Atlas_Peak==1
replace wine_appellation="Calistoga" if  Calistoga==1
replace wine_appellation="Howell_Mountain" if Howell_Mountain==1 
replace wine_appellation="Carneros" if Carneros==1 
replace wine_appellation="Oak_Knoll" if Oak_Knoll==1 
replace wine_appellation="Rutherford" if Rutherford==1 
replace wine_appellation="St_Helena" if St_Helena==1
replace wine_appellation="Yountville" if Yountville==1
replace wine_appellation="Napa_Valley" if  Napa_Valley==1
replace wine_appellation="Napa_County" if  Napa_County==1
replace wine_appellation="North_Coast" if  North_Coast==1
replace wine_appellation="Guenoc" if  Guenoc==1
replace wine_appellation="Russian_River_Valley" if Russian_River_Valley==1
replace wine_appellation="Santa_Barbara_County" if Santa_Barbara_County==1
replace wine_appellation="Shenandoah_Valley_Cal" if Shenandoah_Valley_Cal==1
replace wine_appellation="Shenandoah_Valley" if Shenandoah_Valley==1
replace wine_appellation="Northern_Sonoma" if  Northern_Sonoma==1
replace wine_appellation="Sonoma_Mountain" if  Sonoma_Mountain==1
replace wine_appellation="Sonoma_Coast" if Sonoma_Coast==1 
replace wine_appellation="Sonoma_Valley" if Sonoma_Valley==1 
replace wine_appellation="Sonoma" if Sonoma==1
replace wine_appellation="South_Coast" if South_Coast==1
replace wine_appellation="Suisun_Valley" if Suisun_Valley==1
replace wine_appellation="Santa_Cruz_Mountain" if Santa_Cruz_Mountain==1
replace wine_appellation="Willamette_Valley" if Willamette_Valley==1
replace wine_appellation="Applegate_Valley" if Applegate_Valley==1
replace wine_appellation="Red_Hills_Lake_County" if Red_Hills_Lake_County==1
replace wine_appellation="Clear_Lake" if Clear_Lake==1
replace wine_appellation="Dundee_Hills" if Dundee_Hills==1
replace wine_appellation="Dunnigan_Hills" if Dunnigan_Hills==1
replace wine_appellation="El_Dorado" if  El_Dorado==1
replace wine_appellation="Eola_Hills" if  Eola_Hills==1
replace wine_appellation="Grand_River_Valley" if Grand_River_Valley==1
replace wine_appellation="High_Valley" if High_Valley==1 
replace wine_appellation="Lake_Michigan_Shore" if Lake_Michigan_Shore==1
replace wine_appellation="Long_Island" if Long_Island==1
replace wine_appellation="Umpqua_Valley" if Umpqua_Valley==1
replace wine_appellation="Madera" if Madera==1
replace wine_appellation="Isle_St_George" if Isle_St_George==1
replace wine_appellation="Ohio_River_Valley" if Ohio_River_Valley==1
replace wine_appellation="Finger_Lakes" if Finger_Lakes==1
replace wine_appellation="Lake_Erie" if Lake_Erie==1
replace wine_appellation="Yamhill_Carlton" if Yamhill_Carlton==1
replace wine_appellation="Mcminnville" if Mcminnville==1
replace wine_appellation="Rogue_Valley" if Rogue_Valley==1
replace wine_appellation="Snake_River_Valley" if Snake_River_Valley==1
replace wine_appellation="Colorado_Grand_Valley" if Colorado_Grand_Valley==1
replace wine_appellation="Lancaster_Valley" if Lancaster_Valley==1
replace wine_appellation="Lehigh_Va" if Lehigh_Va==1
replace wine_appellation="Leelanau_Peninsula" if Leelanau_Peninsula==1
replace wine_appellation="Ozark_Hignlands" if Ozark_Hignlands==1
replace wine_appellation="Yadkin_Valley" if Yadkin_Valley==1
replace wine_appellation="Texas_High_Plains" if Texas_High_Plains==1
replace wine_appellation="Texas_Hill_Country" if Texas_Hill_Country==1
replace wine_appellation="Altus" if Altus==1
replace wine_appellation="Monticello" if Monticello==1
replace wine_appellation="Redwood_Valley" if Redwood_Valley==1
replace wine_appellation="Solano_County_Green_Valley" if Solano_County_Green_Valley==1 
replace wine_appellation="Augusta" if Augusta==1
replace wine_appellation="Sierra_Foothills" if Sierra_Foothills==1
replace wine_appellation="Amador_County" if Amador_County==1 
replace wine_appellation="Columbia_Gorge" if Columbia_Gorge==1  
replace wine_appellation="Cucamonga_Valley" if Cucamonga_Valley==1
replace wine_appellation="Fiddletown" if Fiddletown==1
replace wine_appellation="Lake_Chelan" if Lake_Chelan==1
replace wine_appellation="Oakville" if Oakville==1
replace wine_appellation="Rockpile" if Rockpile==1
replace wine_appellation="Tracy_Hills" if Tracy_Hills==1
replace wine_appellation="Yorkville_Highlands" if Yorkville_Highlands==1
replace wine_appellation="Mcdowell_Valley" if Mcdowell_Valley==1  
replace wine_appellation="Temecula_Valley" if Temecula_Valley==1 
replace wine_appellation="Southeastern_New_England" if Southeastern_New_England==1 
replace wine_appellation="Lake_County" if Lake_County==1
replace wine_appellation="Potter_Valley" if Potter_Valley==1

*********************************************************************************
*Capturing State Appellation

gen state_appellation=""
replace state_appellation="NEW JERSEY" if style_descr=="NEW JERSEY"
replace state_appellation="NEW MEXICO" if style_descr=="NEW MEXICO"| style_descr=="NEW MEXICO"| style_descr=="NEW MEXICO CELLARMASTERS RS"| style_descr=="NEW MEXICO RS"| style_descr=="NEW MEXICO W-R"| brand_descr=="NEW MEXICO WINE-A-RITA"
replace state_appellation="NEW YORK" if style_descr=="NEW YORK STATE"| style_descr=="NEW YORK STATE NATIVE"| style_descr=="NEW YORK STATE SPECIAL RS"| style_descr=="NEW YORK"| type_descr=="NEW YORK"
replace state_appellation="OHIO" if style_descr=="OHIO"
replace state_appellation="MARYLAND" if style_descr=="MARYLAND"
replace state_appellation="COLORADO" if style_descr=="COLORADO WESTERN SLOPE"| style_descr=="MESA COUNTY"
replace state_appellation="ARIZONA" if style_descr=="COCHISE COUNTY ARIZONA"| style_descr=="COCHISE COUNTY ARIZONA TOSCANO"| style_descr=="ARIZONA"| style_descr=="ARIZONA COCHISE COUNTY"| brand_descr=="ARIZONA STRONGHOLD"| brand_descr=="ARIZONA ANGEL"| brand_descr=="ARIZONA SUNSET"
replace state_appellation="TEXAS" if style_descr=="COMMANCHE COUNTY TEXAS"| brand_descr=="TEXAS COUNTRY CELLARS"| brand_descr=="TEXAS ON THE PLATE"| style_descr=="TEXAS BARREL RS"| style_descr=="TEXAS PVT RS"| style_descr=="TEXAS RS"| style_descr=="TEXAS"| style_descr=="KINSEY TEXAS"
replace state_appellation="CONNECTICUT" if style_descr=="CONNECTICUT"
replace state_appellation="OREGON" if style_descr=="CROW OREGON"| style_descr=="OREGON FOUNDERS RS"| style_descr=="OREGON ROSEBURG"| style_descr=="OREGON RS"| style_descr=="SOUTHERN OREGON"| style_descr=="OREGON"| style_descr=="EAH OREGON"
replace state_appellation="WISCONSIN" if style_descr=="DOOR COUNTY WISCONSIN"| style_descr=="WISCONSIN"| style_descr=="WISCONSIN RS"| brand_descr=="WISCON"
replace state_appellation="GEORGIA" if style_descr=="GEORGIA"| style_descr=="DUNCAN CREEK GEORGIA"
replace state_appellation="ILLINOIS" if style_descr=="ILLINOIS"| style_descr=="ILLINOIS RIVER VALLEY"| brand_descr=="ILLINOIS CELLARS"| brand_descr=="ILLINOIS RIVER WINERY"
replace state_appellation="IDAHO" if style_descr=="IDAHO"| style_descr=="IDAHO RS SERIES"
replace state_appellation="INDIANA" if style_descr=="INDIANA"
replace state_appellation="VIRGINIA" if style_descr=="LOUDOUN COUNTY VIRGINIA"| style_descr=="ORANGE COUNTY VIRGINIA"| style_descr=="VIRGINIA"| style_descr=="VIRGINIA RS"| style_descr=="ALBEMARLE COUNTY VIRGINIA"| style_descr=="RS VIRGINIA"| style_descr=="ORANGE COUNTY VIRGINIA"
replace state_appellation="MINNESOTA" if style_descr=="MINNESOTA"| style_descr=="MINNESOTA RS"
replace state_appellation="MISSOURI" if style_descr=="MISSOURI"| style_descr=="MISSOURI RS"| style_descr=="MISSOURI ST VINCENT"
replace state_appellation="NORTH CAROLINA" if style_descr=="NORTH CAROLINA"
replace state_appellation="WEST VIRGINIA" if style_descr=="WEST VIRGINIA"
replace state_appellation="WASHINGTON" if style_descr=="WASHINGTON STATE"| style_descr=="WASHINGTON"| style_descr=="CVISRV WASHINGTON STATE"| style_descr=="PARADISE PEAK WASHINGTON STATE"| style_descr=="RS WASHINGTON"| style_descr==" RS WASHINGTON STATE"| style_descr=="WASHINGTON RS"| style_descr=="WASHINGTON STATE RS"| brand_descr=="WASHINGTON"| brand_descr=="JONES OF WASHINGTON"| brand_descr=="WASHINGTON HILLS"
replace state_appellation="ARKANSAS" if style_descr=="ARKANSAS"
replace state_appellation="PENNSYLVANIA" if style_descr=="PENNSYLVANIA"
replace state_appellation="IDAHO WASHINGTON" if style_descr=="IDAHO WASHINGTON"
replace state_appellation="COLORADO" if style_descr=="COLORADO"
replace state_appellation="OREGON_WASH" if style_descr=="OREGON WASHINGTON"
replace state_appellation="INDIANA_MICHIGAN" if style_descr=="INDIANA MICHIGAN"
replace state_appellation="MICHIGAN" if brand_descr=="MICHIGAN AWESOME"
replace state_appellation="IOWA" if style_descr=="IOWA"| style_descr=="CLINTON IOWA"
replace state_appellation="ALABAMA" if style_descr=="ALABAMA"
replace state_appellation="NEBRASKA" if style_descr=="NEBRASKA"
replace state_appellation="NEW HAMPSHIRE" if style_descr=="NEW HAMPSHIRE"| style_descr=="AMHERST NEW HAMPSHIRE"

replace state_appellation="CALIFORNIA" if style_descr=="CA RS"| style_descr=="CA UKIAH VALLEY"| style_descr=="CA VALLEY OAKS"| ///
style_descr=="CA SANTA ROSA"| style_descr=="CA DOOR COUNTY"| upc_descr=="BRFT CB-S CA V RED DDT"| upc_descr=="BRFT P-GR CA V WT DDT"| upc_descr=="BYBRDGV MRLT CA V RED DDT"| upc_descr=="CA' MOMI BNC DI CA V WT DDT"| upc_descr=="CA' MOMI RSO DI CA V RED DDT"| upc_descr=="C-CYN WT ZN CA V BLS BB DDT"| style_descr=="California"| style_descr=="CA CLASSIC"| style_descr=="CA CLASSIC RS"| style_descr=="CA CLASSICS"| style_descr=="CA GLEN ELLEN RS"| style_descr=="CA MASTER LOT RS"| style_descr=="CA MOUNTAIN"| style_descr=="CA VINTNERS RS"| style_descr=="CA PROPRIETORS RS"| style_descr=="CA WILDCREEK CANYON"| style_descr=="VALLEY OAKS CA"| style_descr=="CA VINTNERS BLEND"| style_descr=="CA WILDCREEK CANYON"| style_descr=="CA WILLOW SPRINGS"| style_descr=="CA PREMIUM RS"| style_descr=="CALIFORNIA"| style_descr=="GRANTON CA"| style_descr=="CA COASTAL REGION"| style_descr=="CA NEVADA COUNTY"| style_descr=="CA PVT RS"| style_descr=="CA REPUBLIC"| style_descr=="CA SMALL LOT RS"| style_descr=="CA VINTNERS SELECT"| style_descr=="CA W-R"| style_descr=="CA BIN RS"| style_descr=="CA CARINENA"| style_descr=="CA RS SMALL LOT RS"| style_descr=="NORTHERN CA"| brand_descr=="TAYLOR CALIFORNIA CELLARS"| style_descr=="CA LC"| style_descr=="LC"| style_descr=="CA TIERRA ROJA"| style_descr=="CA"| style_descr=="CA MODESTO"| style_descr=="CA MONARCH ST"| style_descr=="CA GALLAGHER RS"| style_descr=="CA GVSF"| style_descr=="CA FINNEGANS LAKE"| style_descr=="SOUTHERN CA"| brand_descr=="C.A. WINECRAFT"| brand_descr=="CALIFORNIA 37"| brand_descr=="CALIFORNIA CELLARS"| brand_descr=="BERINGER CALIFORNIA CLCTN"| style_descr=="NORTHERN CA"| upc_descr=="APOTHIC INFERNO CA V RED DDT"| upc_descr=="COCOBON RED CA G RED DDT"| upc_descr=="DBL-DD MRLT CA V RED DDT"| upc_descr=="DGLS HL SV-B CA V WT DDT"| upc_descr=="DGLS HL SHZ CA V RED DDT"| upc_descr=="DGLS HL WT ZN CA V BLS DDT"| upc_descr=="HR-G CB-S CA V RED DDT"| style_descr=="CA PALISADE"| style_descr=="LV CLR MSC CA RS V WT DDT"| style_descr=="MALIBU COAST"| style_descr=="LOS ANGELES COUNTY"| upc_descr=="OK-LV CB-S CA V RED C-B DDT"| upc_descr=="OK-LV MRLT CA V RED BB DDT"| upc_descr=="OK-LV P-GR CA V WT C-B DDT"| upc_descr=="OK-LV P-GR CA V WT DDT"| upc_descr=="PCFCPK CB-S CA V RED C-B DDT"| upc_descr=="PCFCPK CB-S CA V RED DDT"| upc_descr=="PCFCPK CHRD CA V WT DDT"| upc_descr=="PINECROFT CB-S CA V RED DDT"| upc_descr=="PROSPECTOR CB-S CA V RED DDT"| upc_descr=="THR WHS CB-S V RED DDT"| upc_descr=="THR WHS MRLT CA V RED DDT"| style_descr=="CONTRA COSTA COUNTY CCC" | style_descr=="CLINE ZN CA V RED DDT"| style_descr=="CCC RS SELECTION"| style_descr=="YOLO COUNTY"| brand_descr=="NEVADA CITY WINERY"| brand_descr=="NEVADA COUNTY WINE GUILD"| upc_descr=="TISDALE P-N CA V RED DDT"


replace state_appellation="" if wine_appellation!=""      //capturing only if we do not know ava


********************************************************************************

*Origin state information is used for DISTANCE INSTRUMENT: Note origin state info is collected from AVA/State/County info 

gen origin_state=""
replace origin_state="CALIFORNIA" if wine_appellation=="Alexander_Valley"| wine_appellation=="Anderson_Valley"| wine_appellation=="Arroyo_Grande"| wine_appellation=="Amador County"| wine_appellation=="Arroyo Seco"| wine_appellation=="Atlas Peak"| wine_appellation=="Calistoga"| state_appellation=="CALIFORNIA"| wine_appellation=="Carmel_Valley"| wine_appellation=="Carneros"| wine_appellation=="Central_Coast"| wine_appellation=="Chalk_Hill"| wine_appellation=="Chalone"| wine_appellation=="Cienega_Valley"| wine_appellation=="Clarksburg"| wine_appellation=="Clear_Lake"| wine_appellation=="Cucamonga_Valley"| wine_appellation=="Chiles Valley"| wine_appellation=="Dry_Creek_Valley"| wine_appellation=="Dunnigan_Hills"| wine_appellation=="Eagle_Peak"| wine_appellation=="Edna_Valley"| wine_appellation=="El_Dorado"| wine_appellation=="Fiddletown"| wine_appellation=="Guenoc"| wine_appellation=="High_Valley"| wine_appellation=="Howell_Mountain"| wine_appellation=="Hames Valley"| wine_appellation=="Knights_Valley"| wine_appellation=="Livermore_Valley"| wine_appellation=="Lodi"| wine_appellation=="Lake County"| wine_appellation=="Madera"| wine_appellation=="Mcdowell_Valley"| wine_appellation=="Mendocino"| wine_appellation=="Monterey_County"| wine_appellation=="Napa_Valley"| wine_appellation=="Napa_County"| wine_appellation=="North_Coast"| wine_appellation=="Oakville"| wine_appellation=="Oak Knoll"| wine_appellation=="Paicines"| wine_appellation=="Paso_Robles"| wine_appellation=="Red_Hills_Lake_County"| wine_appellation=="Redwood_Valley"| wine_appellation=="Rockpile"| wine_appellation=="Russian_River_Valley"| wine_appellation=="Rutherford"| wine_appellation=="Saint_Lucia_Highlands"| wine_appellation=="San Antonio"| wine_appellation=="Santa_Barbara_County"| wine_appellation=="Santa_Clara"| wine_appellation=="Santa_Cruz_Mountain"|   wine_appellation=="Santa_Maria_Valley"| wine_appellation=="Santa_Ynes_Valley"| wine_appellation=="Shenandoah_Valley_Cal"| wine_appellation=="Solano_County_Green_Valley"| wine_appellation=="Sonoma_Coast"| wine_appellation=="Sonoma_Valley"| wine_appellation=="Sonoma"| wine_appellation=="South_Coast"| wine_appellation=="St_Helena"| wine_appellation=="Sta_Rita_Hills"| wine_appellation=="Suisun_Valley"| wine_appellation=="San Benito"| wine_appellation=="San Bernabe"| wine_appellation=="Stags Leap District"| wine_appellation=="Spring Mountain District"| wine_appellation=="Sierra_Foothills"| wine_appellation=="Tracy_Hills"| wine_appellation=="Yorkville_Highlands"| wine_appellation =="Temecula Valley"| wine_appellation=="Yountville"| wine_appellation=="Potter Valley"| wine_appellation=="San Lucas"


replace origin_state="CALIFORNIA" if style_descr=="YOLO COUNTY"| brand_descr=="DRY CREEK VINEYARD"| style_descr=="CENTRAL VALLEY"| style_descr=="MC UKIAH VALLEY"|style_descr=="ORANGE COUNTY"| brand_descr=="GALLO FAMILY VINEYARDS TWN VLY"| style_descr=="AMADOR COUNTY"| style_descr=="AMADOR COUNTY COUGAR HILL"| style_descr=="AMADOR COUNTY J & S RS"| style_descr=="AMADOR COUNTY MC"| style_descr=="CALAVERAS COUNTY"| style_descr=="CALAVERAS COUNTY RS"| style_descr=="AM CANYON"| style_descr=="WILDCREEK CANYON"| style_descr=="MOUNTAIN NECTAR"| style_descr=="AMADOR COUNTY OLD VINE RS"| style_descr=="CALAVERAS COUNTY"| style_descr=="CALAVERAS COUNTY RS"| style_descr=="CONTRA COSTA COUNTY"| style_descr=="WILLOW SPRINGS"| style_descr=="RUSTY RIDGE SANTA CLARA COUNTY"| style_descr=="SAN JOAQUIN"| style_descr=="SAN JOAQUIN COUNTY SC"| style_descr=="SANTA ROSA"| style_descr=="SBC RS"| style_descr=="SBC RS SELECTION"| style_descr=="SBC VINTNERS RS"| style_descr=="STANISLAUS COUNTY"| style_descr=="TRINITY COUNTY"| style_descr=="WILLOW SPRINGS"


replace origin_state="OREGON" if wine_appellation=="Applegate_Valley"| wine_appellation=="Dundee_Hills"| wine_appellation=="Eola_Hills"| wine_appellation=="Umpqua_Valley"| wine_appellation=="Willamette_Valley" | state_appellation=="OREGON"| wine_appellation=="Mcminnville"| wine_appellation=="Rogue_Valley"| brand_descr=="CALLAHAN RIDGE"| brand_descr=="MONTINORE VINEYARD"| wine_appellation=="Yamhill_Carlton"| wine_appellation=="Chehalem Mountains"  

replace origin_state="WASHINGTON" if wine_appellation=="Walla_Walla"| wine_appellation=="Horse_Heaven_Hills"| wine_appellation=="Wahluke_Slope"| wine_appellation=="Yakima_Valley"| brand_descr=="SNOQUALMIE"| brand_descr=="SNOQUALMIE VINEYARDS"| wine_appellation=="Rattle_Snake_Hills"| wine_appellation=="Lake_Chelan"| wine_appellation=="Red_Mountain"| style_descr=="CHELAN COUNTY"| wine_appellation=="ALOCV"

replace origin_state="OREGON_WASH" if wine_appellation=="Columbia_Valley"| wine_appellation=="Columbia_Gorge"| style_descr=="OREGON WASHINGTON"

replace origin_state="ALABAMA" if state_appellation=="ALABAMA"
replace origin_state="ARIZONA" if state_appellation=="ARIZONA"| style_descr=="COCHISE COUNTY" 
replace origin_state="ARKANSAS" if state_appellation=="ARKANSAS"| wine_appellation=="ALTUS"
replace origin_state="COLORADO" if wine_appellation=="WEST ELKS"| state_appellation=="COLORADO"| wine_appellation=="COLORADO GRAND VALLEY"
replace origin_state="CONNECTICUT" if state_appellation=="CONNECTICUT"
replace origin_state="GEORGIA" if state_appellation=="GEORGIA"| style_descr=="DUNCAN CREEK"
replace origin_state="IDAHO_OREGON" if wine_appellation=="SNAKE RIVER VALLEY"
replace origin_state ="IDAHO" if state_appellation =="IDAHO"
replace origin_state="INDIANA" if state_appellation=="INDIANA"
replace origin_state="INDIANA_MICHIGAN" if style_descr=="INDIANA MICHIGAN"
replace origin_state="IDAHO_WASHINGTON" if style_descr=="IDAHO WASHINGTON"
replace origin_state="ILLINOIS" if state_appellation=="ILLINOIS"| style_descr=="GREEN COUNTY"| style_descr=="GREENE COUNTY"
replace origin_state="IOWA" if style_descr=="CLINTON IOWA"| style_descr=="IOWA"| style_descr=="MUSCATINE"| brand_descr=="MADISON COUNTY WINERY"
replace origin_state="KENTUCKY" if style_descr=="KENTUCKY"
replace origin_state="LOUISIANA" if style_descr=="LOUISIANA"| style_descr=="LOUISIANA"
replace origin_state="MARYLAND" if state_appellation=="MARYLAND"
replace origin_state="MINNESOTA" if state_appellation=="MINNESOTA"| brand_descr=="CANNON RIVER WINERY"
replace origin_state="MICHIGAN" if wine_appellation=="LAKE MICHIGAN SHORE"| state_appellation=="MICHIGAN"| wine_appellation=="LEELANAU PENINSULA"
replace origin_state="MISSOURI" if state_appellation=="MISSOURI"| wine_appellation=="OZARK HIGHLANDS"| wine_appellation=="AUGUSTA"
replace origin_state="NEBRASKA" if state_appellation=="NEBRASKA"
replace origin_state="NEW YORK" if wine_appellation=="LONG ISLAND"| state_appellation=="NEW YORK"| wine_appellation=="FINGER LAKES"| brand_descr=="MARTHA CLARA VINEYARDS"
replace origin_state="NEW JERSEY" if state_appellation=="NEW JERSEY"| style_descr=="PINELANDS"
replace origin_state="NEW MEXICO" if state_appellation=="NEW MEXICO"
replace origin_state="NEW HAMPSHIRE" if style_descr=="NEW HAMPSHIRE"
replace origin_state="NORTH CAROLINA" if state_appellation=="NORTH CAROLINA"| wine_appellation=="YADKIN VALLEY"
replace origin_state="OHIO" if wine_appellation=="GRAND RIVER VALLEY"| wine_appellation=="ISLE ST GEORGE"| wine_appellation=="OHIO RIVER VALLEY"| state_appellation=="OHIO"| style_descr=="CATABWA ISLAND PROPRIETORS RS"
replace origin_state="PENNSYLVANIA" if wine_appellation=="LANCASTER VALLEY"| state_appellation =="PENNSYLVANIA"| wine_appellation=="LEHIGH VALLEY"| wine_appellation=="LAKE ERIE"
replace origin_state="RHODE ISLAND" if style_descr=="RHODE ISLAND"
replace origin_state="TEXAS" if style_descr=="COMMANCHE COUNTY"| state_appellation=="TEXAS"| style_descr=="DE VALERO"| style_descr=="SAN ANTONIO VALLEY"| style_descr=="SAN ANTONIO DE VALERO"| style_descr=="SAN ANTONIO VALLEY"| wine_appellation=="TEXAS HIGH PLAINS"|wine_appellation=="TEXAS HILL COUNTRY"
replace origin_state="VIRGINIA" if state_appellation=="VIRGINIA"| style_descr=="JAMES RIVER"| wine_appellation=="MONTICELLO"
replace origin_state="WEST VIRGINIA" if state_appellation=="WEST VIRGINIA"| wine_appellation=="SHENANDOAH VALLEY"
replace origin_state="WISCONSIN" if style_descr=="GREATER GREEN BAY AREA"| state_appellation=="WISCONSIN"| brand_descr=="BROWN COUNTY WINERY"| style_descr=="DOOR COUNTY"| style_descr=="GREATER GREEN BAY AREA"
replace origin_state="CAROLINA" if style_descr=="CAROLINA"    //not really state- will use proxy for North and South Carolina
replace origin_state ="Conn_RI_MS" if wine_appellation=="SOUTHEASTERN NEW ENGLAND"
replace origin_state="New Hampshire" if state_appellation=="New Hampshire"


***FOREIGN COUNTRY ORIGIN*******************************************************

*Some of the imported categroy are not captured by the product module- and some of them describe IDT in upc but product module says DDT. Fixing that

gen Imported= regexm(" " + upc_descr  + " ", " (IDT|IM) ")
replace Imported=0 if upc_descr=="YAGO IM RED SG"

*Categorizing different type of appellation
gen appellation_label=""
replace appellation_label="State_Appellation" if state_appellation!=""
replace appellation_label="California" if state_appellation=="CALIFORNIA"
replace appellation_label="AVA" if wine_appellation!=""       
replace appellation_label="Imported" if product_module_code==5052| product_module_code==5059| type_descr=="IMPORTED"     
replace appellation_label="Imported" if Imported==1 & appellation_label==""
replace appellation_label="No" if appellation_label==""
drop Imported


replace appellation_label="California" if upc_descr=="TISDALE P-N CA V RED DDT"     //capturing imported from prodcut module code but it is domestic

*seperating No label between domestic and no info

gen match_dom = regexm(" " + upc_descr  + " ", " (DM) ")
replace appellation_label ="US" if match_dom ==1 & appellation_label=="No"
replace appellation_label ="US" if product_module_descr=="WINE-DOMESTIC DRY TABLE" & appellation_label=="No"
replace appellation_label ="US" if product_module_descr=="WINE-SWEET DESSERT-DOMESTIC" & appellation_label=="No"
replace appellation_label ="US" if type_descr=="DOMESTIC" & appellation_label=="No"
drop match_dom

gen oregon= regexm(" " + upc_descr  + " ", " (OG) ")
replace state_appellation="OREGON" if oregon==1 & appellation_label=="US"
replace state_appellation="OREGON" if oregon==1 & appellation_label=="No"
replace appellation_label="State_Appellation" if state_appellation=="OREGON"
replace origin_state="OREGON" if state_appellation=="OREGON"    //updating origin state
drop oregon

gen CAL = regexm(" " + upc_descr  + " ", " (CA) ")
replace state_appellation="CALIFORNIA" if CAL==1 & appellation_label=="US"
replace state_appellation="CALIFORNIA" if CAL==1 & appellation_label=="No"
replace appellation_label="California" if state_appellation=="CALIFORNIA"
drop CAL
replace origin_state="CALIFORNIA" if state_appellation=="CALIFORNIA"   //updating origin state

*Foreign Country
gen France = regexm(" " + upc_descr  + " ", " (FR) ")
gen Italy = regexm(" " + upc_descr  + " ", " (IT) ")
gen Chile = regexm(" " + upc_descr  + " ", " (CHL) ")
gen Australia = regexm(" " + upc_descr  + " ", " (AS) ")
gen New_Zealand = regexm(" " + upc_descr  + " ", " (NZ) ")
gen Argentina = regexm(" " + upc_descr  + " ", " (ARG) ")
gen Germany = regexm(" " + upc_descr  + " ", " (GM) ")
gen South_Africa = regexm(" " + upc_descr  + " ", " (SA) ")
gen Spain= regexm(" " + upc_descr  + " ", " (S) ")
gen Portugal = regexm(" " + upc_descr  + " ", " (PG) ")
gen Moldova = regexm(" " + upc_descr  + " ", " (MOLDOVA) ")
gen Greece = regexm(" " + upc_descr  + " ", " (GK) ")
gen Korea = regexm(" " + upc_descr  + " ", " (KOREA) ")
gen Israel = regexm(" " + upc_descr  + " ", " (IS) ")
gen Brazil = regexm(" " + upc_descr  + " ", " (BRZL) ")
gen Slovenia = regexm(" " + upc_descr  + " ", " (SV) ")
gen Austria = regexm(" " + upc_descr  + " ", " (ASTRA) ")
gen China=regexm(" " + upc_descr  + " ", " (CHINA) ")
gen Lebanon=regexm(" " + upc_descr  + " ", " (LBN) ")
gen Hungary= regexm(" " + upc_descr  + " ", " (HG) ")    // check
replace Hungary=0 if product_module_descr=="WINE-DOMESTIC DRY TABLE" 
gen Romania= regexm(" " + upc_descr  + " ", " (RM) ")


gen imp= France+ Italy+Chile+ Australia+ New_Zealand+ Argentina +Germany+ South_Africa+ Spain+ Portugal+ Moldova+ Greece+ Korea+ Israel+ Slovenia+ Austria+ China+ Lebanon+ Hungary+ Romania

replace France = 0 if product_module_code==5053 & appellation_label!="Imported"
replace France = 0 if upc_descr=="ALM FR COL V WT DDT"| upc_descr=="ARBR-M P-GR WT PEAR GL FR"| upc_descr=="DCC FR COL TX V WT DDT"| upc_descr=="MCNB-R FR COL MDCN V WT DDT"| upc_descr=="FR VRTS ZN LDI V RED DDT"| upc_descr=="MN FR CHRD CA V WT DDT"| upc_descr=="FR VRTS CB-S LDI V RED DDT"| upc_descr=="IGNK FR COL V WT DDT"| upc_descr=="ARBR-M ZN SGRA GL FR"| upc_descr=="PBWY RDA RHU RBY BLND GL FR"| upc_descr=="FR SRD F-RD IT SANG RED BX IDT"| upc_descr=="PCL FR IT BNC WT IDT"  //french colombar is a grape varietal

replace Germany=0 if upc_desc=="GM PA MNTY GRAPE O-F SD D"| upc_descr=="MTL GM NON-ALC CDR GL FR"| upc_descr=="WOOD DUCK GM BRD NYS G RED DDT"| upc_descr=="GM F THRS CB-S NV V RED DDT"| upc_descr=="GM F THRS CB-S NV V RED DDT"| upc_descr=="GM F THRS CHRD CC V WT DDT"| upc_descr=="GM F THRS P-N OG V RED DDT"| upc_descr=="GM F THRS RED PSR G RED DDT"| upc_descr=="MTL GM HRD CDR GL FR 12P"| upc_descr=="MTL GM NON-ALC CDR GL FR 4P"| upc_descr=="DMN GM FR LG GS RED IDT"| upc_descr=="DMN GM FR RED RED IDT"| upc_descr=="GM DL IT P-GR WT IDT"

replace Chile=0 if upc_descr=="CTL BR MLBC CHL V RED DDT"| upc_descr=="HATCH RED CHL GL FR"| upc_descr=="PW GV CHL CB-S VC V RED BB DDT"| upc_descr=="PW GV CHL P-N VC V RED BB DDT"| upc_descr=="CHL RCH ARG MLBC RED IDT"

replace Australia=0 if upc_descr=="AS HP CB-S PSR V RED DDT"| upc_descr=="BITCH AS GRN RED IDT"

replace Portugal = 0 if upc_descr=="PG D-VGNT IM PRSCO SP"| upc_descr=="PG DLS IT CB-S SANG RED IDT"| upc_descr=="PG IT P-GR WT IDT"| upc_descr=="PG D-VGNT IM MSC SPMT SWT SP"| upc_descr=="RAZA PG VVAAT WT IDT"| upc_descr=="ASTROLABE NZ PVC PG WT IDT"| upc_descr=="PG DR S TMPN RED IDT"| upc_descr=="RPLC IT SPGT PG WT IDT"| upc_descr=="VL PG SLV IT CSPNL RED IDT"   //capturing PG from brand name

replace Argentina = 0 if upc_descr=="GFV ARG PK-MR V BLS IDT" & appellation_label!="Imported"    //style description mentions CA

replace Slovenia =0 if upc_descr=="DMNE DU SV FR CVRNY WT IDT"| upc_descr=="STDG SV GWRZT V WT DDT"| upc_descr=="STDG SV VDL BL FGR-LK V WT DDT"| upc_descr=="STDG SV VDL-IC FGR-LK V WT DDT"| upc_descr=="TMW-CO SV BL SV-B C-V V WT DDT"| upc_descr=="SV AZD AGL IT WT CLS WT IDT"| upc_descr=="RC SV IT WT CLS WT IDT"| upc_descr=="IN SITU CHL CB SV SNG RED IDT"| upc_descr=="CASA SV CHL CB-S RS RED IDT"| upc_descr=="CASA SV CHL CRMNR RED IDT"| upc_descr=="CASA SV CHL CRMNRE RS RED IDT"| upc_descr=="CASA SV CHL CU-R CR RED IDT"| upc_descr=="CASA SV CHL SAV GRS WT IDT"| upc_descr=="JOHN DALY SA TLN-CB SV RED IDT"| upc_descr=="RC SV IT RPS RED IDT"| upc_descr=="RC SV IT WT-GG CLS WT IDT"| upc_descr=="RC SV IT RSE BLS IDT"

replace Spain=0 if upc_descr=="S FR SAV WT IDT"| upc_descr=="7 S RED CSMTSGPVG RED BB IDT"| upc_descr=="DCD LD CVMD S RMA-WS V RED DDT" | upc_descr=="S K N CB-S NV V RED DDT"| upc_descr=="S K N CHRD NV V WT DDT"| upc_descr=="S K N CHRD V WT DDT"| upc_descr=="S K N MRLT NV V RED DDT"| upc_descr=="S K N RSE-PN NV G BLS DDT"| upc_descr=="S K N SV-B V WT DDT"| upc_descr=="WLMS DB HRD AP S CDR CN FR 4P"| upc_descr=="DMN GGN FR CTE S CHRD WT IDT" | upc_descr=="EC LV S NZ SV-B WT IDT"| upc_descr=="S H W AS SV-B WT IDT"| upc_descr=="S & J PORT RUBY SD I" //first two capturing S from brand name

replace Lebanon=0 if upc_descr=="GNR LBN GL FR"| upc_descr=="FCH LBN FR SLMS WT IDT"    //capturing brand name

replace Romania=0 if upc_descr=="CHT RM FR GVR MCFS RED IDT"| brand_descr=="RUDOLF MULLER"
drop imp

gen Importing_country=""
replace Importing_country = "France" if France==1
replace Importing_country = "Italy" if Italy==1
replace Importing_country = "Chile" if Chile==1
replace Importing_country = "Australia" if Australia==1 
replace Importing_country = "New_Zealand" if New_Zealand==1
replace Importing_country = "Argentina" if Argentina==1
replace Importing_country = "Germany" if Germany==1
replace Importing_country = "South_Africa" if South_Africa==1
replace Importing_country = "Spain" if Spain==1
replace Importing_country = "Portugal" if Portugal==1
replace Importing_country = "Moldova" if Moldova==1
replace Importing_country = "Greece" if Greece==1
replace Importing_country = "Korea" if Korea==1
replace Importing_country = "Israel" if Israel==1
replace Importing_country = "Brazil" if Brazil==1
replace Importing_country = "Slovenia" if Slovenia==1
replace Importing_country = "Austria" if Austria==1
replace Importing_country = "China" if China==1
replace Importing_country="Lebanon" if Lebanon==1
replace Importing_country="Hungary" if Hungary==1
replace Importing_country="Romania" if Romania==1
replace Importing_country= "Other" if Importing_country=="" & appellation_label=="Imported"
drop France-Romania


********************************************************************************
*Importing Region- if not conditioned on conutry- capturing only that conutry


*IGP
gen CotesDeGascogne= regexm(" " + upc_descr  + " ", " (D-G) ")| regexm(" " + upc_descr  + " ", " (GSCGNE) ")
gen Vaucluse= regexm(" " + upc_descr  + " ", " (VDP-DV) ")    //vin de pays de vaucluse
gen Ardeche= regexm(" " + upc_descr  + " ", " (GRD ARD) ")    //grand ardeche 
gen Bugey_Cerdon= regexm(" " + upc_descr  + " ", " (BGY-CRDN) ")   


*I. Bordeaux
gen Bordeaux = regexm(" " + upc_descr  + " ", " (BDX) ")| regexm(" " + upc_descr  + " ", " (RBDX-MCBFCS) ")| regexm(" " + upc_descr  + " ", " (BS-MFCS) ")| regexm(" " + upc_descr  + " ", " (RBDX-M&CS) ") if Importing_country =="France"     //mcbfcs- varietal
replace Bordeaux=0 if Bordeaux==.

gen Saint_Emilion=regexm(" " + upc_descr  + " ", " (ST-E) ")| regexm(" " + upc_descr  + " ", " (LSSTE) ")| regexm(" " + upc_descr  + " ", " (ST-EMLN) ")| regexm(" " + upc_descr  + " ", " (SEGC) ")   // - combining Lussac saint emilion segc- se grand cru

gen Medoc= regexm(" " + upc_descr  + " ", " (MEDOC) ") 
replace appellation_label="Imported" if Medoc==1
replace Importing_country="France" if Medoc==1       //one entry product omdule mentions domestic but it is frenchvineyard

gen Cru_Bourgeois=regexm(" " + upc_descr  + " ", " (CRU-B) ")
replace Medoc =1 if Cru_Bourgeois==1            //its is Medoc AOC classification
drop Cru_Bourgeois

gen Cotes_De_Bourg=regexm(" " + upc_descr  + " ", " (CDBURG) ")  
gen Lalande_De_Pomerol=regexm(" " + upc_descr  + " ", " (LDP) ")  
replace Importing_country="France" if Lalande_De_Pomerol==1

gen Graves_Bordeaux=regexm(" " + upc_descr  + " ", " (GVDBR) ")| regexm(" " + upc_descr  + " ", " (GVDBW) ")| regexm(" " + upc_descr  + " ", " (GVDB) ")| regexm(" " + upc_descr  + " ", " (GVD-BR) ")| regexm(" " + upc_descr  + " ", " (GVDBW) ")| regexm(" " + upc_descr  + " ", " (GVDBR-MCS) ")|  regexm(" " + upc_descr  + " ", " (GRAVES) ")

gen Margaux= regexm(" " + upc_descr  + " ", " (MRGX) ")

gen Sauternes= regexm(" " + upc_descr  + " ", " (SAUT) ") if Importing_country=="France" //else capturing domestic
replace Sauternes=0 if missing(Sauternes)
gen Pauillac= regexm(" " + upc_descr  + " ", " (PLC) ") if Importing_country=="France"     //else capturing spain from brand name
replace Pauillac=0 if missing(Pauillac)
gen Saint_Estephe= regexm(" " + upc_descr  + " ", " (ST ESTEPHE) ") 


*II. Burgandy/Bourgogne
gen Bourgogne= regexm(" " + upc_descr  + " ", " (BOUR) ")| regexm(" " + upc_descr  + " ", " (RGVDB) ")       //reserve grand vin de bourgogne    
replace Bourgogne=0 if upc_descr=="DMNE-DR CHRD BOUR V WT DDT"         //domestic 
gen Burgandy= regexm(" " + upc_descr  + " ", " (BRG) ")| regexm(" " + upc_descr  + " ", " (BURG) ") if Importing_country=="France"   //Need to condition on France- else will capture domestic burg, Burgandy
replace Burgandy=0 if Burgandy==.
replace Bourgogne=1 if Burgandy==1  
drop Burgandy
replace Bourgogne=1 if brand_descr=="BOURGOGNE"
gen Macon_Chardonnay=regexm(" " + upc_descr  + " ", " (MCN CHRD) ")    
gen Saint_Veran=regexm(" " + upc_descr  + " ", " (ST VRN) ")  
gen Macon_Villages=regexm(" " + upc_descr  + " ", " (MCN BL VLG) ")| regexm(" " + upc_descr  + " ", " (MCN VLG) ")  
 
gen Chablis=regexm(" " + upc_descr  + " ", " (CHB) ")| regexm(" " + upc_descr  + " ", " (PT-CHB) ") if Importing_country=="France"  //have to condition on france-broader aoc-burgandy; le clos- chablis grand cru les clos
replace Chablis=1 if upc_descr=="SMNT-F IM CHB BR SP"    
replace Importing_country="France" if upc_descr=="SMNT-F IM CHB BR SP"    
replace Chablis=0 if Chablis==.

gen La_Grange=regexm(" " + upc_descr  + " ", " (LA GRANGE) ")

gen Pouilly_Vinzelles=regexm(" " + upc_descr  + " ", " (PLY VNZLS) ")
gen Pouilly_Fuisse=regexm(" " + upc_descr  + " ", " (PLY FS) ")| regexm(" " + upc_descr  + " ", " (VDOPF) ")       //Vallon d'Or
gen Macon_Uchizy= regexm(" " + upc_descr  + " ", " (MCN-UCZY) ")
gen Macon_Lugny= regexm(" " + upc_descr  + " ", " (MCN LUGNY) ")| regexm(" " + upc_descr  + " ", " (MCN-LGNY) ")  //all france
gen Chalonnaise=regexm(" " + upc_descr  + " ", " (CT-CHLNS) ")| regexm(" " + upc_descr  + " ", " (CHALONNAISE) ")
gen Cartone=regexm(" " + brand_descr  + " ", " (Aloxe-Corton) ")| regexm(" " + upc_descr  + " ", " (ALX-C) ") 
gen Marsannay=regexm(" " + upc_descr  + " ", " (MARSANNAY) ")
gen Haut_CotesDe_Beaune= regexm(" " + upc_descr  + " ", " (BHUCDB) ")      //bourg hautes cotes de beanue
gen Puligny_Montrachet= regexm(" " + upc_descr  + " ", " (PLG MNTRC) ")
gen Maconnais=  regexm(" " + upc_descr  + " ", " (MCN) ")
replace Maconnais=0 if Macon_Chardonnay==1| Macon_Villages==1| Macon_Lugny==1| Macon_Uchizy==1

********************************************************************************

*III. Provence
gen CotesDeProvence=regexm(" " + upc_descr  + " ", " (CDPR) ")| regexm(" " + upc_descr  + " ", " (CTS-D-PRVNC) ")| regexm(" " + upc_descr  + " ", " (CTS-D-PV) ")| regexm(" " + upc_descr  + " ", " (C-D-P) ")    
replace CotesDeProvence=0 if brand_descr=="CASTELLO DEL POGGIO"        //Italian wine capturing from brand name                                         

//capturing some entry that is not listed as france, fixing below

replace appellation_label="Imported" if upc_descr=="LA RUE CDPR G BLS DDT" | upc_descr=="CTL BR CDPR G BLS DDT"      //produch description says ddt but brand is french and it has provence label

replace Importing_country="France" if upc_descr=="LA RUE CDPR G BLS DDT" | upc_descr=="CTL BR CDPR G BLS DDT"  



*IV. Rhone
gen Rhone=regexm(" " + upc_descr  + " ", " (C-D-R) ")| regexm(" " + upc_descr  + " ", " (RHE) ")| regexm(" " + upc_descr  + " ", " (RHN) ")| regexm(" " + upc_descr  + " ", " (CDRRCGSM) ")| regexm(" " + upc_descr  + " ", " (CDRGCC) ") if Importing_country=="France"  // vgsy is for villages grenache syrah, need to condition on France else it will capture all other Rhine entries; cdrrcgsm- cotes du rhore rose cinsalut, gre,syrah,mourvedre

replace Rhone=0 if Rhone==.
gen Gigondas= regexm(" " + upc_descr  + " ", " (GIGONDAS) ")
gen Tavel = regexm(" " + upc_descr  + " ", " (TAVEL) ")|  regexm(" " + upc_descr  + " ", " (TVL) ") 
gen Rasteau = regexm(" " + upc_descr  + " ", " (RASTEAU) ")| regexm(" " + brand_descr  + " ", " (RASTEAU) ")| regexm(" " + upc_descr  + " ", " (CVE-DR) ")                  //cve-dr= cave de rasteau
gen Ventoux = regexm(" " + upc_descr  + " ", " (CTE DU VNTX) ")| regexm(" " + upc_descr  + " ", " (VNTX) ")| regexm(" " + upc_descr  + " ", " (CSDV-GSCC) ")              //-   gscc- for varietal
gen Costieres_De_Nimes=regexm(" " + upc_descr  + " ", " (CDN) ") 
replace Costieres_De_Nimes=0 if brand_descr=="GEORGES DUBOEUF CHATEAU DE NER"     //capturing the brand name in cdn
gen Crozes_Hermitage=regexm(" " + upc_descr  + " ", " (CRZS HRMTG) ")    
gen Vacqueyras=regexm(" " + upc_descr  + " ", " (VACQUEYRAS) ")| regexm(" " + upc_descr  + " ", " (VQYS) ")    
gen Rhone_Villages= regexm(" " + upc_descr  + " ", " (CDRV) ")| regexm(" " + upc_descr  + " ", " (CDRVGSY) ") 
replace Rhone=1 if Rhone_Villages==1
drop Rhone_Villages
gen Luberon= regexm(" " + upc_descr  + " ", " (CTE DU LBRN) ")| regexm(" " + upc_descr  + " ", " (CTE DU LBVN) ")
gen Chateauneuf_du_pape= regexm(" " + upc_descr  + " ", " (CDP) ")| regexm(" " + upc_descr  + " ", " (CDLDP) ")| regexm(" " + upc_descr  + " ", " (CDPGS) ")
replace Chateauneuf_du_pape=0 if brand_descr=="CLOS DE L'ORATOIRE DES PAPES"| brand_descr=="CAVES DES PAPES"    //captruing abv from brand name

********************************************************************************

*V. Beaujolais
gen Beaujolais= regexm(" " + upc_descr  + " ", " (BJ-NVU) ")| regexm(" " + upc_descr  + " ", " (BJ NEU) ")| regexm(" " + upc_descr  + " ", " (BJ) ")| regexm(" " + upc_descr  + " ", " (NOUVEAU) ")| regexm(" " + upc_descr  + " ", " (BJ-V-NU) ")| regexm(" " + upc_descr  + " ", " (NEU) ") if Importing_country=="France"       //region should be Beaujolais- capturing domestic wine when not condition on France; Beaujolais Nouveau (NEU) is the name given to Beaujolais and Beaujolais Villages wines which are released almost immediately after harvest

replace Beaujolais=0 if Beaujolais==.
gen Brouilly=regexm(" " + upc_descr  + " ", " (BY) ") & Importing_country=="France"   //Beaujolais- need to condition else BY is capturing many things
replace Brouilly=0 if brand_descr=="CIRCUS BY L'OSTAL CAZES"
replace Brouilly=0 if Brouilly==.
gen Saint_Amour=regexm(" " + upc_descr  + " ", " (ST AMOUR) ")
gen Julienas=regexm(" " + upc_descr  + " ", " (JULIENAS) ") 
gen Morgon=regexm(" " + upc_descr  + " ", " (MORGON) ")    
gen Fleurie= regexm(" " + upc_descr  + " ", " (FLRE) ") 


********************************************************************************

*VI LOIRE
gen Anjou= regexm(" " + upc_descr  + " ", " (RSE D-ANJ) ")| regexm(" " + upc_descr  + " ", " (RSE D ANJ) ")   
gen Muscadet= regexm(" " + upc_descr  + " ", " (MSDT) ")       
gen Cheverny= regexm(" " + upc_descr  + " ", " (CVRNY) ")  
gen Vouvray=regexm(" " + upc_descr  + " ", " (VVRY) ")| regexm(" " + upc_descr  + " ", " (VY-CH-B) ") //Capturing only france except one entry; 
replace Importing_country="France" if Vouvray==1   //two entry was in other import
gen Sancerre= regexm(" " + upc_descr  + " ", " (SNC) ") & Importing_country=="France" //condition on france (6 entries from chile and 2 from US otherwise) 
gen Pouilly_Fume= regexm(" " + upc_descr  + " ", " (PLY-FM) ")

********************************************************************************
*VII- South West
gen CotesDeDuras= regexm(" " + upc_descr  + " ", " (CTSDDUS) ")    //capturing only France; south west france
gen Cahors=regexm(" " + upc_descr  + " ", " (CAHORS) ")

********************************************************************************
*VIII Champgane
gen Champgane=regexm(" " + upc_descr  + " ", " (CHM) ") if appellation_label=="Imported"
replace Champgane =0 if Champgane ==.
replace Importing_country="France" if Champgane==1


********************************************************************************

*IX LANGUEDOC 
gen Pic_Saint_Loup= regexm(" " + upc_descr  + " ", " (PSLR) ")    //PSLR- Rousillion
gen Picpoul_De_Pinet=regexm(" " + upc_descr  + " ", " (PD_PNT) ")| regexm(" " + upc_descr  + " ", " (PCPL DE PNT) ")| regexm(" " + upc_descr  + " ", " (PD-PNT) ")  
gen Corbieres=regexm(" " + upc_descr  + " ", " (CRBRES) ")    
gen Cotes_du_Roussillon= regexm(" " + upc_descr  + " ", " (C-D-RSLN) ")   //capturing all France
gen Cabardes=regexm(" " + upc_descr  + " ", " (CBRDS) ")  
gen CotesDeRose=  regexm(" " + upc_descr  + " ", " (CDRR-GCS) ") //GCS- gre-carignan-syrah
gen Minervois=  regexm(" " + upc_descr  + " ", " (MNR-CBSHZ) ")| regexm(" " + upc_descr  + " ", " (MNR-CHVGR) ")    //CBSz varietal


*************************

*ITALY-
*I.Veneto
gen Ripasso = regexm(" " + upc_descr  + " ", " (RPS) ")|  regexm(" " + upc_descr  + " ", " (RIPASO) ")   
gen Amarone= regexm(" " + upc_descr  + " ", " (AMARONE) ")| regexm(" " + upc_descr  + " ", " (A-D-V) ")   //some entry with name was with name Amarone A-D-V Amarone Di Valpolicella
gen Valpolicella= regexm(" " + upc_descr  + " ", " (VLPL) ")      //Valpolicella is a region and Valpantena is a wine made in that region, all entries are captured by only capturing Valpolicella


gen Bardolino=regexm(" " + upc_descr  + " ", " (BRDLNO) ")  //cpturing 3 more imported. veneto
replace Importing_country="Italy" if Bardolino==1

gen Prosecco=regexm(" " + upc_descr  + " ", " (PRSCO) ")| regexm(" " + upc_descr  + " ", " (GRBL-P) ") //not capturing Italy but capturing all imported except 28 entries, checked, all from Italy even though description says domestic ; Garbel-Prosecco

replace Importing_country="Italy" if Prosecco==1
replace appellation_label="Imported" if Prosecco==1

gen Soave=regexm(" " + upc_descr  + " ", " (SOAVE) ")


*II. Piedmont
gen Piemonte = regexm(" " + upc_descr  + " ", " (PIMTE) ")     
gen Barolo= regexm(" " + upc_descr  + " ", " (BAROLO) ")      
replace Barolo =0 if Importing_country =="France"         //2 entries were capturing France 
gen Barbaresco= regexm(" " + upc_descr  + " ", " (BRBRSCO) ")    //Capturing all Italy; Piedmont region

gen BarberaDAlba= regexm(" " + upc_descr  + " ", " (BDA) ")   
replace Importing_country="Italy" if BarberaDAlba==1         //one entry from Other country, checeked its Italy


gen ASTI= regexm(" " + upc_descr  + " ", " (ASTI) ")| regexm(" " + upc_descr  + " ", " (AST) ")| regexm(" " + upc_descr  + " ", " (MSC D ASTI) ")   //capturing some non Itlian wine - replacing country as Italy if ASti =1 and wine is imported but not from any other country 
replace ASTI=0 if appellation_label!="Imported"
replace Importing_country="Italy" if ASTI==1 & Importing_country=="Other"
replace ASTI=0 if Importing_country!="Italy"

gen Malvasia_Di_Casorzo_DAsti= regexm(" " + upc_descr  + " ", " (MDCSZ) ")  //all Italy- Piedmont region

 
*III. Tuscany
gen Di_Montepulciano=regexm(" " + upc_descr  + " ", " (V-N-D-M) ")     //sub region of Tuscany-capturing all montepu in one-seperating aburozo- Caputring only Italy (except one for Spain) without conditiong as well; V-N-D-M- Vino Nobile Di Montepulciano

gen RossoDiMontalcino=regexm(" " + upc_descr  + " ", " (RSO-DI-MN) ")   
gen BrunelloDimontalcino=regexm(" " + upc_descr  + " ", " (BR-D-MN) ")  
gen Vernaccia_Di_SanGimignano= regexm(" " + upc_descr  + " ", " (VDSG) ")  
gen Tuscany= regexm(" " + upc_descr  + " ", " (TUSCANY) ")| regexm(" " + upc_descr  + " ", " (TSCN) ") 
gen Toscana= regexm(" " + upc_descr  + " ", " (TSCNA) ")| regexm(" " + upc_descr  + " ", " (SANG-D-TS) ")
replace Toscana=0 if Tuscany==1    //cos some entry has both tscna and tscn
gen Chianti=regexm(" " + upc_descr  + " ", " (CHNT) ")| regexm(" " + upc_descr  + " ", " (CHNTI) ")| regexm(" " + upc_descr  + " ", " (CHNT-S) ")| regexm(" " + upc_descr  + " ", " (CHNTI-SC) ") if Importing_country=="Italy"      //else capturing cal and france and argentina
replace Chianti=0 if Chianti==.


*IV- Puglia
gen Salice_Salentino= regexm(" " + upc_descr  + " ", " (SLNTO) ")| regexm(" " + upc_descr  + " ", " (SLC-SLTN) ") 
replace Importing_country="Italy" if Salice_Salentino==1  //all Italy (one in KT fixing here) 
replace appellation_label="Imported" if Salice_Salentino==1
gen Gravia= regexm(" " + upc_descr  + " ", " (GRAVINA) ")

*V-Abruzzo
gen Abruzzo=regexm(" " + upc_descr  + " ", " (MDA) ")| regexm(" " + upc_descr  + " ", " (ARBZ) ")| regexm(" " + upc_descr  + " ", " (TRB D ABRZ ) ")| regexm(" " + upc_descr  + " ", " (MNTPL) ") if Importing_country=="Italy"        //MNTPL-checked with brand names- montepulciano d'abruzzo;  //also called TrebbianoDabruzzo-condition on Italy else will capture some brand abreviation
replace Abruzzo=0 if Abruzzo==.
replace Abruzzo=1 if upc_descr=="CALDORA IT TRB D ABRZ WT IDT"


*VI Lombardi
gen Sangue_Di_Giuda=regexm(" " + upc_descr  + " ", " (SANG D-GD) ")| regexm(" " + upc_descr  + " ", " (SNG D-GD) ")  
gen Pavia= regexm(" " + style_descr  + " ", " (PROVINCIA DI PAVIA) ")


*VII- Venezie
gen Dell_Venezie= regexm(" " + upc_descr  + " ", " (DL VNT) ") 


*VIII- Lazio
gen Frascati=regexm(" " + upc_descr  + " ", " (FRSCT) ")   //capturing all Italy  //Lazio


*IX- Emilia IGT
gen Emilia=regexm(" " + upc_descr  + " ", " (DLL EM) ")    // all Italy  Dell' Emilia 

*X-Emilia Romanga
gen Romagna= regexm(" " + upc_descr  + " ", " (SNG-DR) ")| regexm(" " + upc_descr  + " ", " (CDIROM) ")  // all Italy- Sangiovese di Romagna; cdirom-cagina di romanga

*Umbria
gen Orvieto= regexm(" " + upc_descr  + " ", " (ORVT CLSC) ")


********************************************************************************

*Portugal
*I DAO
gen Dao= regexm(" " + upc_descr  + " ", " (DAO) ")         //all portugal

*II Vinho Verde
gen Vinho_Verde=regexm(" " + upc_descr  + " ", " (VNHO VERDE) ")| regexm(" " + upc_descr  + " ", " (VV) ") if Importing_country=="Portugal"    //else will capture non verde wine; 
replace Vinho_Verde=1 if upc_descr=="GRINALDA VNHO VERDE KT"   //as it does not capture Portugal 
replace Vinho_Verde=0 if Vinho_Verde==.
replace Importing_country="Portugal" if Vinho_Verde==1      //some not capturing conutry
replace appellation_label="Imported" if Vinho_Verde==1

gen Alvarinho= regexm(" " + upc_descr  + " ", " (ALVARINHO) ")   //not a region- but mostly produced in vinho verde-should we include

      

*III Madeira
gen Madeira= regexm(" " + upc_descr  + " ", " (MADEIRA) ")
replace Madeira=1 if type_descr=="MALMSEY"
replace Importing_country="Portugal" if Madeira==1 & appellation_label=="Imported"
replace Madeira=0 if appellation_label!="Imported"

*V Alentejo
gen Alentejo= regexm(" " + upc_descr  + " ", " (ALENTEJO) ")| regexm(" " + upc_descr  + " ", " (ALNTJNO) ")

*VI Douro
gen Douro= regexm(" " + upc_descr  + " ", " (DOURO) ") if Importing_country=="Portugal"
replace Douro=0 if Douro==.

gen Port= regexm(" " + upc_descr  + " ", " (PORTO) ")| regexm(" " + upc_descr  + " ", " (PORT RUBY) ")| regexm(" " + upc_descr  + " ", " (PORTO RUBY) ")| regexm(" " + upc_descr  + " ", " (PORTO TWNY) ")| regexm(" " + upc_descr  + " ", " (PORTA) ")| regexm(" " + upc_descr  + " ", " (PORT) ")

replace Port=1 if type_descr=="COLHEITA"| type_descr=="FINE TAWNY"| type_descr=="OLD TAWNY"| type_descr=="FINE TAWNY"| type_descr=="FULL RUBY"| type_descr=="RUBY"| type_descr=="RUBY PORTO"| type_descr=="RUBY RESERVA"| type_descr=="RUBY SEC"| type_descr=="TAWNY"| type_descr=="RUBY SWEET RED"| type_descr=="TAWNY"| type_descr=="TAWNY DEMI SEC MEDIUM DRY"| type_descr=="TWNY RED"| type_descr=="TAWNY RESERVE"| type_descr=="TAWNY SPECIAL RESERVE"| type_descr=="TAWNY SWEET RED"

replace Port=0 if brand_descr=="PORTA VITA"| brand_descr=="PORTA SOLE"  // Italian wine capturing from brand name

replace Importing_country="Portugal" if upc_descr=="S & J PORT TWNY SD I"    //capturing S from brand name as spain (SAVORY & JAMES-Port from Portugal and Sherry from Spain, so do not change for all entries under this brand name )

replace Port=0 if appellation_label!="Imported"      //checked with brand name as well, all domestic and data also mentions domestic
replace Importing_country="Portugal" if Port==1

*VII
gen Bairada= regexm(" " + upc_descr  + " ", " (BAIRRADA) ")

*********************************************************************************

*SPAIN

gen Jumilla= regexm(" " + brand_descr  + " ", " (JUMILLA) ")    //Murcia

*Catalunya
gen Catalonia= regexm(" " + upc_descr  + " ", " (CAVA) ") //cpturing all spain
replace Catalonia=0 if upc_descr=="LINEAS DM CAVA WT SP"      //data mentions it as domestic wine

replace Importing_country="Spain" if Catalonia==1 & appellation_label=="Imported"
gen Priorat= regexm(" " + upc_descr  + " ", " (PRIORAT) ")

*Rioja
gen Rioja=regexm(" " + upc_descr  + " ", " (RJA) ")| regexm(" " + upc_descr  + " ", " (RIOJA) ")    
replace Rioja=1 if brand_descr=="LA RIOJA ALTA, S.A."   
replace Rioja=1 if brand_descr=="RIOJA VEGA"| brand_descr=="RIOJA BORDON"


gen Aragon= regexm(" " + upc_descr  + " ", " (D-ARGN) ") 

*Town in Andalucia
gen Manzanilla= regexm(" " + upc_descr  + " ", " (MNZNL) ")
replace Manzanilla=1 if type_descr=="MANZANILLA"| type_descr=="MANZANILLA EXTRA DRY"| type_descr=="MANZANILLA RESERVA"
replace Importing_country="Spain" if Manzanilla==1      // not capturing spain in conutry-

*Mostly produced in Andalucia
gen Sherry_Andalucia=1 if type_descr=="AMONTILLADO"| type_descr=="AMONTILLADO DRY RESERVA"| type_descr=="AMONTILLADO MEDIUM"| type_descr=="AMONTILLADO MEDIUM DRY"| type_descr=="FINO AMONTILLADO"| type_descr=="FINO"| type_descr=="FINO DRY"| type_descr=="FINO PALE DRY SPECIAL RESERVE"| type_descr=="FINO SUPERIOR"|type_descr=="OLOROSO"| type_descr=="OLOROSO DON NUNO DRY RESERVA"| type_descr=="OLOROSO FULL DRY"| type_descr=="OLOROSO SWEET" 

replace Sherry_Andalucia=0 if Sherry_Andalucia==.
replace Importing_country="Spain" if Sherry_Andalucia==1     //some are not captured in spain

*Town in Castilla
gen Toro= regexm(" " + upc_descr  + " ", " (TDTORO) ")| regexm(" " + upc_descr  + " ", " (TORO) ")| regexm(" " + upc_descr  + " ", " (SDTG) ")| regexm(" " + upc_descr  + " ", " (SAN DE TORO) ")
replace Toro=0 if upc_descr=="EL TORO FLACO IM SG"
replace Toro=0 if Importing_country=="Italy"


replace Beaujolais=0 if Brouilly==1 //capturing both
replace Chablis=0 if upc_descr=="CHT CHB FR SEGC MCSCF RED IDT"

********************************************************************************

*Size  
gen size_category=""
replace size_category="bulk" if size1_units=="LI"
replace size_category="bottle" if size1_amount==750 & size1_units=="ML"
replace size_category="bottle" if size1_code_uc ==39701 & size1_units=="ML"     
replace size_category="small" if size1_amount<750 & size1_units=="ML"
replace size_category="small" if size1_amount==12 & size1_units=="OZ"   
replace size_category="bulk" if size1_amount==567 & size1_units=="OZ"
replace size_category="bottle" if size1_amount==1 & size1_units=="CT"     //assuming 750 ml, as ct is for co ; 723 obs-

drop if size_category =="small"



gen Importing_region=""

*France
replace Importing_region="Anjou" if Anjou==1
replace Importing_region="Beaujolais" if Beaujolais==1| Brouilly==1| Saint_Amour==1| Julienas==1| Morgon==1| Fleurie==1
replace Importing_region="Bordeaux" if Bordeaux==1| Saint_Emilion==1| Cotes_De_Bourg==1| Lalande_De_Pomerol==1| Margaux==1| Sauternes==1| Pauillac==1| Saint_Estephe==1| Medoc==1| Graves_Bordeaux==1
replace Importing_region="Bourgogne" if Bourgogne==1| La_Grange==1| Chalonnaise==1| Marsannay==1| Haut_CotesDe_Beaune==1| Puligny_Montrachet==1
replace Importing_region="Maconnais" if Macon_Chardonnay==1| Saint_Veran==1| Macon_Villages==1| Pouilly_Vinzelles==1| Pouilly_Fuisse==1| Macon_Uchizy==1| Macon_Lugny==1|Maconnais==1
replace Importing_region="Chablis" if Chablis==1
replace Importing_region="Champgane" if Champgane==1
replace Importing_region="Provence" if CotesDeProvence==1
replace Importing_region="Languedoc" if Pic_Saint_Loup==1| Picpoul_De_Pinet==1| Corbieres==1| Cotes_du_Roussillon==1| Cabardes==1| CotesDeRose==1
replace Importing_region="Minervois" if Minervois==1
replace Importing_region="Rhone" if Rhone==1| Gigondas==1| Rasteau==1| Costieres_De_Nimes==1| Crozes_Hermitage==1| Vacqueyras==1| Tavel==1
replace Importing_region="Ventoux" if Ventoux==1
replace Importing_region="Luberon" if Luberon==1
replace Importing_region="Chateauneuf_du_pape" if Chateauneuf_du_pape==1
replace Importing_region="Loire" if Cheverny==1| Sancerre==1| Pouilly_Fume==1
replace Importing_region="Muscadet" if Muscadet==1
replace Importing_region="Vouvray" if Vouvray==1
replace Importing_region="South_West_France" if CotesDeDuras==1| Cahors==1| CotesDeGascogne==1
replace Importing_region="Rem_France" if Bugey_Cerdon==1| Vaucluse==1| Ardeche==1


*Italy-
replace Importing_region="Asti" if ASTI==1
replace Importing_region="Asti" if Malvasia_Di_Casorzo_DAsti==1
replace Importing_region="Piedmont" if Piemonte==1| Barolo==1| Barbaresco==1| BarberaDAlba==1
replace Importing_region="Tuscany" if Di_Montepulciano==1| RossoDiMontalcino==1|BrunelloDimontalcino==1| Vernaccia_Di_SanGimignano==1  
replace Importing_region="Tuscany_IGT" if Tuscany==1| Toscana==1
replace Importing_region="Chianti" if Chianti==1
replace Importing_region="Salice Salentino" if Salice_Salentino==1
replace Importing_region="Sangue Di Giuda" if Sangue_Di_Giuda==1
replace Importing_region="Pavia" if Pavia==1
replace Importing_region="Amarone" if Amarone==1
replace Importing_region="Valpolicella" if Valpolicella==1| Ripasso==1
replace Importing_region="Bardolino" if Bardolino==1
replace Importing_region="Prosecco" if Prosecco==1
replace Importing_region="Dell_Venezie" if Dell_Venezie==1
replace Importing_region="Frascati" if Frascati==1
replace Importing_region="Emilia Romagna" if Emilia==1| Romagna==1
replace Importing_region="Orvieto" if Orvieto==1
replace Importing_region="Rem_Italy" if Gravia==1
replace Importing_region="Abruzzo" if Abruzzo==1
replace Importing_region="Soave" if Soave==1

*Portugal
replace Importing_region="Dao" if Dao==1
replace Importing_region="Vinho_Verde" if Vinho_Verde==1
replace Importing_region="Madeira" if Madeira==1
replace Importing_region="Douro" if Douro==1| Port==1
replace Importing_region="Rem_Portugal" if Bairada==1| Alentejo==1


*Spain-
replace Importing_region="Rem_Spain" if Jumilla==1| Aragon==1
replace Importing_region="Catalunya" if Catalonia==1| Priorat==1
replace Importing_region="Rioja" if Rioja==1
replace Importing_region="Andalucia" if Sherry_Andalucia==1| Manzanilla==1
replace Importing_region="Toro_Castilla" if Toro==1

drop CotesDeGascogne - Toro


*check if regions are captured accurately and there is no overlap-- tab appleation label, importing country and wine appellation and cross check if the categorization is correct and exhaustive and exclusie: ex- number of obs in Importing_country and under Imported in appellation label should be equal ii) wine appellation and other ava iii) improting country and importing region

replace appellation_label="AVA" if upc_descr =="ZAB UCG NC V RED DDT"         //product module says imported but upc dec says domestic and north coast
replace Importing_country="" if upc_descr=="ZAB UCG NC V RED DDT" 
replace appellation_label="Imported" if upc_descr=="FZ MRLT CA V RED BB DDT" & style_descr=="SOUTHEASTERN AUSTRALIA"
replace appellation_label="Imported" if upc_descr=="FZ CHRD CA V WT BB DDT"  & style_descr=="SOUTHEASTERN AUSTRALIA"  //although upc says cal and doemstics, style_descr says southeastern aus 
replace state_appellation="" if upc_descr=="FZ MRLT CA V RED BB DDT"  & style_descr=="SOUTHEASTERN AUSTRALIA"
replace state_appellation="" if upc_descr=="FZ CHRD CA V WT BB DDT" & style_descr=="SOUTHEASTERN AUSTRALIA"
replace Importing_country="Australia" if upc_descr=="FZ CHRD CA V WT BB DDT" & style_descr=="SOUTHEASTERN AUSTRALIA"
replace Importing_country="Australia" if upc_descr=="FZ MRLT CA V RED BB DDT"  & style_descr=="SOUTHEASTERN AUSTRALIA"
replace Importing_country ="South Africa" if Importing_country =="South_Africa"
replace Importing_country ="New Zealand" if Importing_country =="New_Zealand"


replace origin_state=proper(origin_state)
replace wine_appellation=proper(wine_appellation)
replace state_appellation=proper(state_appellation)
replace wine_appellation="Lehigh Valley" if wine_appellation=="Lehigh Va"
replace wine_appellation="Walla Walla Valley" if wine_appellation=="Walla Walla Washington"
replace origin_state="" if upc_descr=="GFVTV IT P-N RED IDT"| upc_descr=="SNTA BRBRA IT AZLENDA WT IDT"   //imported
replace origin_state="" if upc_descr=="GFV ARG PK-MR V BLS IDT" & style_descr!="CA"    //from Argentina


drop flavor_code flavor_descr form_code form_descr formula_code formula_descr container_code container_descr salt_content_code salt_content_descr organic_claim_code organic_claim_descr usda_organic_seal_code usda_organic_seal_descr common_consumer_name_code common_consumer_name_descr strength_code strength_descr scent_code scent_descr dosage_code dosage_descr gender_code gender_descr target_skin_condition_code target_skin_condition_descr use_code use_descr department_code department_descr dataset_found_uc household_code store_code_uc store_zip3 ym household_income household_size type_of_residence household_composition age_and_presence_of_children female_head_age male_head_age male_head_employment female_head_employment male_head_education female_head_education male_head_occupation female_head_occupation male_head_birth female_head_birth marital_status race hispanic_origin panelist_zip_code kitchen_appliances tv_items household_internet_connection wic_indicator_current wic_indicator_ever_notcurrent member_1_birth - member_7_employment method_of_payment_cd panelist_zipcd wic_indicator_ever_not_current

save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\1_capture_geographic_origin.dta", replace

***This is end of this do file***

