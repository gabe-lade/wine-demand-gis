********************************************************************************
*Do file no. 5
*This do file estimates demand model, WTP, and price elasticities***************

set more off
clear all
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\4_demand_estimation_data.dta", clear

*Nested Logit-IV
ivreghdfe logshare_diff (price lsih lshg  = Z1-Z10 distribution_cost dis_sum retail_density_area retail_density_popu state_control excisetax), a(FE1 = quarter FE2 = year FE3 = scm_code FE4 = product) resid(residual_nested) gmm2s robust first
estimate store A_1
local coeff_price_m1 = e(b)["y1","price"]
local coeff_sigma1_m1 = e(b)["y1","lsih"]
local coeff_sigma2_m1 = e(b)["y1","lshg"]
local se_price_m1  = _se["price"]
local se_lsih_m1 = _se["lsih"]
local se_lshg_m1  = _se["lshg"]
predict yhat, xb
corr logshare_diff yhat
display r(rho)^2
drop yhat


*Basic logit IV
ivreghdfe logshare_diff (price= Z1-Z10 distribution_cost dis_sum retail_density_area retail_density_popu state_control excisetax), a(FE5 =quarter FE6 = year FE7 = scm_code FE8 =product) resid(residual_logit_iv) gmm2s robust  
estimate store A_2 
local coeff_price_m2 = e(b)["y1","price"]
local se_price_m2  = _se["price"]
predict yhat, xb
corr logshare_diff yhat
display r(rho)^2
drop yhat


*Basic logit-OLS
reghdfe logshare_diff price, a(FE9 = quarter FE10 = year FE11 =scm_code FE12 = product) resid(residual_logit) vce(robust) 
estimate store A_3
local coeff_price_m3 = e(b)["y1","price"]
local se_price_m3  = _se["price"]
predict yhat, xb
corr logshare_diff yhat
display r(rho)^2

label var price "price"
label var logshare_diff "share diff"
label var lsih "subgroup"
label var lshg "group"

estfe A_*, labels(quarter "Quarter FE" year "Year FE" scm_code "Region FE" product "Product FE" IV "YES")
return list 


esttab A_* using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\table_2.doc", replace /// 
	b(a2) ///
	title("\textsc{Demand Parameter Estimates}") ///
	nonumbers mtitles("Nested-IV" "Logit-IV" "Logit-OLS") ///
	se ///
	label ///
	indicate(`r(indicate_fe)') ///
	compress ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	
estfe A_* , restore


***************Second Step Regression and WIllingess to Pay*********************

preserve
bysort product: keep if _n==1

reg FE4 i.size i.winetype Cabernet_Sauvignon-Other_Red_Imported Chardonnay-Other_White_Imported Dessert-Other_Specialty_Imported Alexander_Valley-Other_AVAs Florida-Other_States Argentina-Other_Countries, a(brand_name) vce(robust)
estimate store C_1

replace FE4 = -FE4/`coeff_price_m1'
reg FE4 i.size i.winetype Cabernet_Sauvignon-Other_Red_Imported Chardonnay-Other_White_Imported Dessert-Other_Specialty_Imported Alexander_Valley-Other_AVAs Florida-Other_States Argentina-Other_Countries, a(brand_name) vce(robust)
estimate store C_2

estfe C_*
return list 

    esttab C_* using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\table_3.csv", replace ///
	title("{Willingness to pay $ per bottle}") ///
	nonumbers mtitles("MU" "WTP") ///
	b(a2) ///
	wide ///
	se ///
	indicate(`r(indicate_fe)') ///
	label ///
	scalars(r2) ///
	compress ///
	longtable ///
	nogaps ///
	noomitted ///
	nobaselevels ///
	star(* 0.10 ** 0.05 *** 0.01)
	
estfe C_* , restore

restore
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\5_elasticity_simulation_data.dta", replace

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
export excel nested_iv-og_logit using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\elasticities_table_2", sheet("own_price_elasticity") sheetmodify firstrow(variables) nolabel keepcellfmt
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
export excel aggregate_nested_iv aggregate_logit_iv aggregate_logit using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\elasticities_table_2", sheet("aggregate_elasticity") sheetmodify firstrow(variables) nolabel keepcellfmt
restore 


*Cross price elasticity 
keep product price market share_im share_ihgm share_igm winetype
preserve
contract market product winetype
drop _freq
rename product product_j
rename winetype winetype_j
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross price elasticity\market_product_j.dta", replace
restore

rename product product_i
rename winetype winetype_i

joinby market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross price elasticity\market_product_j.dta", unmatched(both)
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
export excel within_wine_type using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\elasticities_table_2", sheet("cross_price_within_winetype") sheetmodify firstrow(variables) nolabel keepcellfmt
restore
collapse (mean) across_wine_type = nested_iv if D2==0 & D3==1
export excel across_wine_type using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\elasticities_table_2", sheet("cross_price_across_winetype") sheetmodify firstrow(variables) nolabel keepcellfmt
clear all

erase "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross price elasticity\market_product_j.dta"

********************************************************************************

*random draw of parameters for elasticity CI:
clear all
set obs 1000

* Create a matrix for the variance-covariance matrix using standard errors
matrix Sigma_nested_1 = (`se_price_m1'^2, 0, 0 \ 0, `se_lsih_m1'^2, 0 \ 0, 0, `se_lshg_m1'^2)

* Generate random samples from the multivariate normal distribution
drawnorm coeff_price_1 coeff_sgimaone_1 coeff_sigmatwo_1, means(`coeff_price_m1' `coeff_sigma1_m1' `coeff_sigma2_m1') cov(Sigma_nested_1)
drawnorm coeff_price_2, means(`coeff_price_m2') sd(`se_price_m2')
drawnorm coeff_price_3, means(`coeff_price_m3') sd(`se_price_m3')

* Calculate and display the mean of each parameter
summarize coeff_price_1 coeff_sgimaone_1 coeff_sigmatwo_1
summarize coeff_price_2
summarize coeff_price_3
gen id = _n

save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\simulated_parameters.dta", replace
clear all
*************************************************************************************************************************
