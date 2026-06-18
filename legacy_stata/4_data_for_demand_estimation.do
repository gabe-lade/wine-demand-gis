********************************************************************************
**Do file no. 4
*This do file prepares data at prodcut market level for demand estimation
********************************************************************************
set more off
clear all
set matsize 11000
set maxvar 32767

use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\3_product_data.dta", clear

gen date_quarter = qofd(date)
format date_quarter %tq

gen quarter = quarter(date)      
       
sort scm_code date_quarter
egen market = group(scm_code date_quarter)

*5 features that defines product
*1. Size
encode size_category, gen(size)
drop size_category

*2. Wine type
encode wine_type, gen(winetype)
drop wine_type

*3. Varietal
encode Varietal, gen(Var)  

*4. Geographic label
encode  geographic_label, gen(geographiclabel)  
*drop geographic_label


*5. Brand
encode brand_descr, gen(brand_name)
*drop brand_descr

*Define Product
egen product=group(size winetype Var geographiclabel brand_name)


bysort product: egen pc_count = count(product)     //removing singletons
drop if pc_count==1                                // 145 products
drop pc_count
drop product

egen product=group(size winetype Var geographiclabel brand_name)

gen quantity_bottle_w = quantity_bottle*projection_factor
gen final_price_paid_w = final_price_paid*projection_factor
gen total_price_paid_w = total_price_paid*projection_factor


***********************CREATING DATA AT PRODUCT MARKET LEVEL********************
bysort product market: egen revenue = total(total_price_paid_w)
bysort product market: egen revenue_dis = total(final_price_paid_w) 

drop if revenue_dis == 0    
bysort product market: egen sales = total(quantity_bottle_w)  
bysort product market: gen id =_n
drop if id>1

*Price
gen double product_price = revenue_dis/sales      
gen double price = product_price/Deflator          


*Market size
bysort market: egen totalsales = total(sales)   
gen per_capita_cons= totalsales/pop_above_20
bysort scm_code: egen max_percapitacons = max(per_capita_cons)
gen market_size = 1.5*max_percapitacons*pop_above_20


*SHARE
gen double share_im = sales/market_size
gen double logshare = log(share_im)
bysort market: egen double share_in = total(share_im)
gen double share_out = 1-share_in
gen double logout = log(share_out)
gen double logshare_diff = logshare - logout

*subgroup share
bysort market winetype: egen double subgroup_sales = total(sales)
gen double share_ihgm = sales/subgroup_sales
gen double lsih = log(share_ihgm)

*group share
bysort market: egen double group_sales = total(sales)
gen double sharehg = subgroup_sales/group_sales
gen double lshg = log(sharehg)

gen double share_igm = sales/group_sales 

*INSTRUMENTS
*Count of products
bysort market: egen Z1 = count(product)
replace Z1= Z1-1
bysort market winetype: egen Z2 = count(product)
replace Z2= Z2-1
bysort market winetype size: egen Z3 = count(product)
replace Z3= Z3-1
bysort market winetype Var: egen Z4 = count(product)
replace Z4= Z4-1
bysort market winetype geographiclabel: egen Z5 =  count(product)
replace Z5= Z5-1
bysort market winetype brand_name: egen Z6 =  count(product)
replace Z6= Z6-1
bysort market size: egen Z7 = count(product)
replace Z7= Z7-1
bysort market Var: egen Z8 = count(product)
replace Z8= Z8-1
bysort market geographiclabel: egen Z9 =  count(product)
replace Z9= Z9-1
bysort market brand_name: egen Z10 =  count(product)
replace Z10= Z10-1


*distance instrument
merge m:1 date_quarter using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\Distance Instrument\diesel_price.dta"
drop _merge
gen gallons_req= distance_miles/25
gen distribution_cost= gallons_req*diesel_price  

bysort market: egen dis_sum = total(distance_miles) 
replace dis_sum = dis_sum-distance_miles


*store count data
merge m:1 scantrack_market_descr using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\Retail Store Count_County Business Profile\Stata_Data\foodstorecount_2017.dta"
drop if _merge==2
drop _merge
merge m:1 scantrack_market_descr using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\Retail Store Count_County Business Profile\Stata_Data\foodstorecount_2012.dta"
drop if _merge==2
drop _merge
gen store_count=.
replace store_count=foodandbeveragestorescount_2017 if year>2012
replace store_count=foodandbeveragestorescount_2012 if year<2012
replace store_count=foodandbeveragestorescount_2012 if year==2012
merge m:1 scantrack_market_descr using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\Retail Store Count_County Business Profile\DEC_10_MSAAREA\MSA_Area_2010Census.dta"
drop if _merge==2
drop _merge
gen retail_density_area = areainsquaremilestotalarea/store_count
gen retail_density_popu = (pop_above_20/store_count)*1000

*state control 
gen double state_control = 1 if fips_state_descr=="UT"| fips_state_descr=="WY"| fips_state_descr=="PA"
replace state_control=0 if state_control==.
gen double one_minus_statecontrol = 1-state_control
gen excisetax = one_minus_statecontrol*excisetx_dollarpergallon  


keep product logshare_diff price lshg lsih Z1-Z10 dis_sum distribution_cost date_quarter quarter year scm_code retail_density_area retail_density_popu share_im share_ihgm share_igm scantrack_market_descr winetype geographic_label appellation_label geographiclabel origin_state brand_name brand_descr size Var market market_size share_in share_out sales revenue_dis excisetx_dollarpergallon excisetax state_control Cabernet_Sauvignon-California Import Domestic product_module_descr 

save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\4_demand_estimation_data.dta",replace

*This is the end of this do file************************************************