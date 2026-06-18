********************************************************************************
**Do file no. 5--RC
*This do file prepares data at prodcut market level for demand estimation and 
*estimates demand model with tripple market size for roubstness check
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

*5 feature that defines product
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
gen market_size_double = 3*max_percapitacons*pop_above_20

*SHARE -market size double
gen double share_im = sales/market_size_double
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


*To create DISTANCE INSTRUMENT
merge m:1 date_quarter using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\Distance Instrument\diesel_price.dta"
drop _merge
gen gallons_req= distance_miles/25
gen distribution_cost= gallons_req*diesel_price  

bysort market: egen dis_sum = total(distance_miles) 
replace dis_sum = dis_sum-distance_miles


*merge with the store count data-
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

gen double share_igm = sales/group_sales 
gen double state_control = 1 if fips_state_descr=="UT"| fips_state_descr=="WY"| fips_state_descr=="PA"
replace state_control=0 if state_control==.

gen double one_minus_statecontrol = 1-state_control
gen excisetax = one_minus_statecontrol*excisetx_dollarpergallon  


*Demand Estimation 

*Nested Logit-IV
ivreghdfe logshare_diff (price lsih lshg  = Z1-Z10 distribution_cost dis_sum retail_density_area retail_density_popu state_control excisetax), a(quarter year scm_code product) gmm2s robust 
estimate store A_1
local coeff_price_m1 = e(b)["y1","price"]
local coeff_sigma1_m1 = e(b)["y1","lsih"]
local coeff_sigma2_m1 = e(b)["y1","lshg"]

label var price "price"
label var logshare_diff "share diff"
label var lsih "subgroup"
label var lshg "group"

estfe A_*, labels(quarter "Quarter FE" year "Year FE" scm_code "Region FE" product "Product FE" IV "YES")
return list 


esttab A_* using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\table_B1.doc", append /// 
	b(a2) ///
	nonumbers mtitles("Nested Logit Tripple Market Size") ///	title("\textsc{Demand Parameter Estimates}") ///
	se ///
	label ///
	indicate(`r(indicate_fe)') ///
	compress ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	
estfe A_* , restore


*********************************Elaticity Estimation***************************

*calculate share and weighted price for aggregate elasticity
gen double conditional_share = (share_im/share_in)*price
bysort market: egen weighted_price = total(conditional_share)

*own price elasticity
gen nested_iv = (`coeff_price_m1')*[(1/(1-`coeff_sigma1_m1'))-((1/(1-`coeff_sigma1_m1'))-(1/(1-`coeff_sigma2_m1')))*share_ihgm-((`coeff_sigma2_m1')/(1-`coeff_sigma2_m1'))*share_igm-share_im]*price  

*wrt to outside good
gen og_nested_iv = -(`coeff_price_m1')*share_im*price

preserve
collapse (mean) nested_iv-og_nested_iv, by(product)       //average across market for each product
collapse (mean) nested_iv-og_nested_iv
mkmat nested_iv, matrix(own_price)
mkmat og_nested_iv, matrix(outside_good)
putexcel set "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\elasticities_table_B1_model_2
putexcel A1 = "own_price_elasticity"
putexcel A2 = matrix(own_price)
putexcel B1 = "outside_good_elasticity"
putexcel B2 =  matrix(outside_good)
restore

*aggregate elasticity 
bysort market: egen sum_os_elasticity = total(og_nested_iv)
gen aggregate_nested_iv = -(share_out/(1-share_out))*sum_os_elas
preserve
keep aggregate_nested_iv market
duplicates drop
collapse (mean) aggregate_nested_iv 
mkmat aggregate_nested_iv, matrix(agg_elasticity)
putexcel C1 = "aggregate_elasticity" 
putexcel C2 = matrix(agg_elasticity) 
restore 



*Cross price elasticity 
keep product price market share_im share_ihgm share_igm winetype
preserve
contract market product winetype
drop _freq
rename product product_j
rename winetype winetype_j
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\market size tripple\market_product_j.dta", replace
restore

rename product product_i
rename winetype winetype_i

joinby market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\market size tripple\market_product_j.dta", unmatched(both)
capture drop _merge
order market product_i product_j

gen D1= 1 if product_i == product_j
gen D2= 1 if winetype_i == winetype_j    
gen D3= 1            
replace D1=0 if D1==.
replace D2=0 if D2==.
replace D3=0 if D3==.

gen nested_iv = (`coeff_price_m1')*[(1/(1-`coeff_sigma1_m1'))*D1-((1/(1-`coeff_sigma1_m1'))-(1/(1-`coeff_sigma2_m1')))*share_ihgm*D2-((`coeff_sigma2_m1')/(1-`coeff_sigma2_m1'))*share_igm*D3-share_im]*price
collapse nested_iv, by(product_i product_j D1 D2 D3)       //averaging across market for each i-j product
drop if D1==1
preserve
collapse (mean) nested_iv if D2==1
mkmat nested_iv, matrix(within_wine_type)
putexcel D1 = "cross_within_wine_type" 
putexcel D2 = matrix(within_wine_type)
restore
collapse (mean) nested_iv if D2==0 & D3==1
mkmat nested_iv, matrix(across_wine_type)
putexcel E1 = "cross_across_wine_type" 
putexcel E2 = matrix(across_wine_type)
clear all
****************This is the end of this do file*********************************

