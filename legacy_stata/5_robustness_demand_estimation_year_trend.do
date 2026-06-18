********************************************************************************
**Do file no. 5--RC
*This do file prepares data at prodcut market level for demand estimation and 
*estimates demand model with year trend specification for roubstness check******

set more off
clear all
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\4_demand_estimation_data.dta", clear

*Nested Logit-IV
ivreghdfe logshare_diff year (price lsih lshg  = Z1-Z10 distribution_cost dis_sum retail_density_area retail_density_popu state_control excisetax), a(quarter scm_code product) resid(residual_nested) gmm2s robust first
estimate store A_1
local coeff_price_m1 = e(b)["y1","price"]
local coeff_sigma1_m1 = e(b)["y1","lsih"]
local coeff_sigma2_m1 = e(b)["y1","lshg"]

*Basic logit IV
ivreghdfe logshare_diff year (price= Z1-Z10 distribution_cost dis_sum retail_density_area retail_density_popu state_control excisetax), a(quarter scm_code product) gmm2s robust  
estimate store A_2 
local coeff_price_m2 = e(b)["y1","price"]

*Basic logit-OLS
reghdfe logshare_diff price year, a(quarter scm_code product) vce(robust) 
estimate store A_3
local coeff_price_m3 = e(b)["y1","price"]

label var price "price"
label var logshare_diff "share diff"
label var lsih "subgroup"
label var lshg "group"

estfe A_*, labels(quarter "Quarter FE" year "Year trend" scm_code "Region FE" product "Product FE" IV "YES")
return list 


esttab A_* using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\table_B2.doc", replace /// 
	b(a2) ///
	title("\textsc{Demand Parameter Estimates}") ///
	nonumbers mtitles("Nested-IV" "Logit-IV" "Logit-OLS") ///
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
gen logit_iv = (`coeff_price_m2')*[1-share_im]*price
gen logit = (`coeff_price_m3')*[1-share_im]*price    

*wrt to outside good
gen og_nested_iv = -(`coeff_price_m1')*share_im*price
gen og_logit_iv = -(`coeff_price_m2')*share_im*price
gen og_logit = -(`coeff_price_m3')*share_im*price

preserve
collapse (mean) nested_iv-og_logit, by(product)       //average across market for each product
collapse (mean) nested_iv-og_logit
export excel nested_iv-og_logit using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\elasticities_table_B2", sheet("own_price_elasticity") sheetmodify firstrow(variables) nolabel keepcellfmt
restore

*aggregate elasticity 
bysort market: egen sum_os_elasticity = total(og_nested_iv)
gen aggregate_nested_iv = -(share_out/(1-share_out))*sum_os_elas
gen aggregate_logit_iv = (`coeff_price_m2')*share_out*weighted_price
gen aggregate_logit = (`coeff_price_m3')*share_out*weighted_price

preserve
keep aggregate_nested_iv aggregate_logit_iv aggregate_logit market
duplicates drop
collapse (mean) aggregate_nested_iv aggregate_logit_iv aggregate_logit
export excel aggregate_nested_iv aggregate_logit_iv aggregate_logit using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\elasticities_table_B2", sheet("aggregate_elasticity") sheetmodify firstrow(variables) nolabel keepcellfmt
restore 


*Cross price elasticity 
keep product price market share_im share_ihgm share_igm winetype
preserve
contract market product winetype
drop _freq
rename product product_j
rename winetype winetype_j
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\year trend model\cross price elasticity\market_product_j.dta", replace
restore

rename product product_i
rename winetype winetype_i

joinby market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\year trend model\cross price elasticity\market_product_j.dta", unmatched(both)
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
collapse (mean) within_wine_type = nested_iv if D2==1
export excel within_wine_type using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\elasticities_table_B2", sheet("cross_price_within_winetype") sheetmodify firstrow(variables) nolabel keepcellfmt
restore
collapse (mean) across_wine_type = nested_iv if D2==0 & D3==1
export excel across_wine_type using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\elasticities_table_B2", sheet("cross_price_across_winetype") sheetmodify firstrow(variables) nolabel keepcellfmt
clear all


****************This is the end of this do file*********************************
