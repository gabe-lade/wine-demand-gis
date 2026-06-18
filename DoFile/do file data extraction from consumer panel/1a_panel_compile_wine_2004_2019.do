*This file creates the wine data for 2004-2016 from Nielsen HMS extracts

*Header Steps
*Must save scanner data to CARD server per data agreement
clear all
capture log close
set more off
 
*OUTPUTDATE
global outputdate "2021"


*****************************************************************
*CONSUMER PANEL DATA COMPILE
*****************************************************************
global user 1
global rc "Z:\NIELSENDATA\consumer_panel\"

if $user ==1 cd $rc

*FILEPATHS
global panel "dataRAW\Consumer_Panel_Data_2004_2019\Consumer_Panel_Data_2004_2019\nielsen_extracts\HMS\"
global stata "codeSTATA\GianCarlo\Discrete Choice\Main\Data\Wine Master Data\"


foreach year in "2004" "2005" "2006" "2007" "2008" "2009" "2010" "2011" "2012" ///
                "2013" "2014" "2015" "2016" "2017" "2018" "2019"{
**
*Panelist Characteristics
import delimited "$panel\`year'\Annual_Files\panelists_`year'.tsv", clear

save $stata\panel_wine_module_`year'_$outputdate, replace

**
*Trips
import delimited "$panel\`year'\Annual_Files\trips_`year'.tsv", clear

gen year=substr(purchase_date,1,4)	
destring year, replace
gen month=substr(purchase_date,6,2)
destring month, replace
gen dom=substr(purchase_date,9,2)
destring dom, replace

gen ym=ym(year,month)
gen date=mdy(month,dom,year)
	format date %td
drop month dom purchase_date
compress

merge m:1 household_code panel_year using $stata\panel_wine_module_`year'_$outputdate
drop _merge

save $stata\panel_wine_module_`year'_$outputdate, replace

**
*Purchases
import delimited "$panel\`year'\Annual_Files\purchases_`year'.tsv", clear

keep trip_code_uc upc upc_ver_uc quantity total_price_paid coupon_value deal_flag_uc

merge m:1 trip_code_uc using $stata\panel_wine_module_`year'_$outputdate
drop if _merge==2
drop _merge
save $stata\panel_wine_module_`year'_$outputdate, replace

**
*Product Characteristics
import delimited "$panel\Master_Files\Latest\products.tsv", clear
				 
merge 1:m upc upc_ver_uc using $stata\panel_wine_module_`year'_$outputdate
drop if _merge==1
drop _merge

save $stata\panel_wine_module_`year'_$outputdate, replace

**
*Retailer Characteristics
import delimited "$panel\Master_Files\Latest\retailers.tsv", clear

merge 1:m retailer_code using $stata\panel_wine_module_`year'_$outputdate
drop if _merge==1
drop _merge

save $stata\panel_wine_module_`year'_$outputdate, replace

**
*Brand Characteristics
import delimited "$panel\Master_Files\Latest\brand_variations.tsv", clear

*Collapsing 
* Notes: - brand code uc in itself captures any variation in brand description, so collapse it to first description
collapse (first) brand_descr brand_descr_alternative, by(brand_code_uc)
				 			 
merge 1:m brand_code_uc using $stata\panel_wine_module_`year'_$outputdate
drop if _merge==1
drop _merge

save $stata\panel_wine_module_`year'_$outputdate, replace

**
*Products Extra
import delimited "$panel\`year'\Annual_Files\products_extra_`year'.tsv", clear
 		 	 
merge 1:m upc upc_ver_uc panel_year using $stata\panel_wine_module_`year'_$outputdate
drop if _merge==1
drop _merge

save $stata\panel_wine_module_`year'_$outputdate, replace


****
*Keeping relevant product groups  
gen wine=0
	replace wine=1 if product_group_code == 5003  //Wine
drop if wine==0

****
*Keeping relevant modules groups  
gen wine_module=0
 	replace wine_module=1 if product_module_code==5050| ///
							  product_module_code==5052| product_module_code==5041| ///
							  product_module_code==5049| product_module_code==5053| ///
							  product_module_code==5054| product_module_code==5055| ///
							  product_module_code==5056| product_module_code==5057| ///
							  product_module_code==5058| product_module_code==5059| ///
							  product_module_code==5060 //Wine
keep if wine_module==1


save $stata\panel_wine_module_`year'_$outputdate, replace
}
*

*Compiling files
use $stata\panel_wine_module_2004_$outputdate, clear
foreach year in "2005" "2006" "2007" "2008" "2009" "2010" "2011" "2012" ///
                "2013" "2014" "2015" "2016" "2017" "2018" "2019"{
append using $stata\panel_wine_module_`year'_$outputdate, force
}
*
*Dropping a few duplicates
duplicates drop
save $stata\panel_wine_module_$outputdate, replace


*Erasing year files
foreach year in "2004" "2005" "2006" "2007" "2008" "2009" "2010" "2011" ///
                "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"{
erase $stata\panel_wine_module_`year'_$outputdate.dta
}
*