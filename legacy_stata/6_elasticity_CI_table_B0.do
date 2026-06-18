********************************************************************************
*This is do file no 6. 
*This do file computes shares based on 1000 drawn random parameters for demand 
*estiamtes, calcualtes elasticity and CI. 
********************************************************************************
forvalues i = 1/1000 {
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\5_elasticity_simulation_data.dta", clear
drop if FE4==.          //11 singletons 
gen id = `i'
merge m:1 id using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\simulated_parameters.dta"
keep if _merge ==3
drop _merge

****calculate share with nested logit parameters*****
gen deltajm_hat = coeff_price_1*price + FE1 + FE2 + FE3 + FE4 + residual_nested
gen deltajm_s1_hat = (deltajm_hat)/(1- coeff_sgimaone_1)
gen exp_deltajm_s1_hat = exp(deltajm_s1_hat)                 
bysort market winetype: egen sum_exp_deltajm_s1_hat = total(exp_deltajm_s1_hat)
gen Ihgm_hat = (1- coeff_sgimaone_1)*ln(sum_exp_deltajm_s1_hat)
preserve
keep market market_size winetype Ihgm_hat coeff_sigmatwo_1
duplicates drop
gen Ihgm_s2_hat = exp(Ihgm_hat/(1-coeff_sigmatwo_1))
bysort market: egen summed_Ihgm_s2_hat = total(Ihgm_s2_hat)
gen Igm_hat = (1-coeff_sigmatwo_1)*ln(summed_Ihgm_s2_hat)
keep Igm_hat market market_size
duplicates drop
gen exp_Igm_hat = exp(Igm_hat)                   
gen Im_hat = ln(1+ exp_Igm_hat)
keep Im_hat Igm_hat exp_Igm_hat market market_size 
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\nested_inclusive_value.dta", replace
restore
merge m:1 market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\nested_inclusive_value.dta"
drop _merge
gen Ihgm_s2_hat = exp(Ihgm_hat/(1-coeff_sigmatwo_1))  
gen Ihgm_s1_hat = exp(Ihgm_hat/(1-coeff_sgimaone_1))    
gen Igm_s2_hat = exp(Igm_hat/(1-coeff_sigmatwo_1))     
gen exp_Im_hat = exp(Im_hat)  

gen predict_sjm_hat = (exp_deltajm_s1_hat*Ihgm_s2_hat*exp_Igm_hat)/(Ihgm_s1_hat*Igm_s2_hat*exp_Im_hat)

bysort market: egen double predict_share_in = total(predict_sjm_hat)
gen double predict_share_out = 1-predict_share_in

bysort market winetype: egen double predict_subgroup_share = total(predict_sjm_hat)
gen double predict_share_ihgm = predict_sjm_hat/predict_subgroup_share

bysort market: egen double predict_group_share = total(predict_sjm_hat)
gen double predict_share_igm = predict_sjm_hat/predict_group_share

preserve
rename product product_i
keep market product_i predict_sjm_hat predict_share_in predict_share_out predict_share_ihgm predict_share_igm coeff_price_1 coeff_sgimaone_1 coeff_sigmatwo_1
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\nested_simulated_share_`i'.dta", replace
restore

***predict share with logit IV parameters***
gen deltajm_hat_logit_iv = coeff_price_2*price + FE5 + FE6 + FE7 + FE8 + residual_logit_iv
gen exp_deltajm_hat_logit_iv = exp(deltajm_hat_logit_iv)
bysort market: egen logit_iv_denominator = total(exp_deltajm_hat_logit_iv)
replace logit_iv_denominator = 1+logit_iv_denominator
gen predict_share_im_logit_iv = exp_deltajm_hat_logit_iv/logit_iv_denominator


***predict share with logit ols parameters*****
gen deltajm_hat_logit = coeff_price_3*price + FE9 + FE10 + FE11 + FE12 + residual_logit
gen exp_deltajm_hat_logit = exp(deltajm_hat_logit)
bysort market: egen logit_denominator =  total(exp_deltajm_hat_logit)
replace logit_denominator  = 1 + logit_denominator
gen predict_share_im_logit = exp_deltajm_hat_logit/logit_denominator


bysort market: egen double predict_share_in_logit_iv = total(predict_share_im_logit_iv)
gen double predict_share_out_logit_iv = 1-predict_share_in_logit_iv

bysort market: egen double predict_share_in_logit = total(predict_share_im_logit)
gen double predict_share_out_logit = 1-predict_share_in_logit

*Conditional share weighted price for aggregate elasticity

gen double cond_share_nested = (predict_sjm_hat/predict_share_in)*price
bysort market: egen weighted_price_nested = total(cond_share_nested)

gen double cond_share_logit_iv = (predict_share_im_logit_iv/predict_share_in_logit_iv)*price
bysort market: egen weighted_price_logit_iv = total(cond_share_logit_iv)

gen double cond_share_logit = (predict_share_im_logit/predict_share_in_logit)*price
bysort market: egen weighted_price_logit = total(cond_share_logit)

*own_price_elasticity
gen nested_iv = (coeff_price_1)*[(1/(1-coeff_sgimaone_1))-((1/(1-coeff_sgimaone_1))-(1/(1-coeff_sigmatwo_1)))*predict_share_ihgm-((coeff_sigmatwo_1)/(1-coeff_sigmatwo_1))*predict_share_igm-predict_sjm_hat]*price

gen logit_iv = (coeff_price_2)*[1-predict_share_im_logit_iv]*price
gen logit_ols = (coeff_price_3)*[1-predict_share_im_logit]*price 

*with_outside_good
gen outside_good_nested_iv = -(coeff_price_1*predict_sjm_hat*price)
gen outside_good_logit_iv = -(coeff_price_2*predict_share_im_logit_iv*price)
gen outside_good_logit = -(coeff_price_3*predict_share_im_logit*price)

preserve

collapse (mean) nested_iv-outside_good_logit, by(product)       //average across market for each product
collapse (mean) nested_iv-outside_good_logit
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\own price elasticity\elasticity_simulated_`i'.dta",replace

restore

bysort market: egen sum_outside_elasticity = total(outside_good_nested_iv)
gen aggregate_nested_iv_1 = -(predict_share_out/(1-predict_share_out))*sum_outside_elasticity

gen aggregate_nested_iv_2 = (coeff_price_1)*predict_share_out*weighted_price_nested

gen aggregate_logit_iv = (coeff_price_2)*predict_share_out_logit_iv*weighted_price_logit_iv
gen aggregate_logit_ols = (coeff_price_3)*predict_share_out_logit*weighted_price_logit

keep aggregate_nested_iv_1 aggregate_nested_iv_2 aggregate_logit_iv aggregate_logit_ols market
duplicates drop
collapse (mean) aggregate_nested_iv_1-aggregate_logit_ols
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\aggregate elasticity\aggregate_elasticity_simulated_`i'.dta", replace
}



*own price elasticity
clear all
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\own price elasticity\elasticity_simulated_1.dta", clear
forvalues i =2/1000{
append using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\own price elasticity\elasticity_simulated_`i'.dta" 
}
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\own price elasticity\elasticity_simulated_compiled.dta",replace
ci mean nested_iv logit_iv logit_ols
ci mean outside_good_nested_iv outside_good_logit_iv outside_good_logit


*aggregate elasticity
clear all
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\aggregate elasticity\aggregate_elasticity_simulated_1.dta", clear
forvalues i =2/1000{
append using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\aggregate elasticity\aggregate_elasticity_simulated_`i'.dta"
}
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\aggregate elasticity\aggregate_elasticity_simulated_compiled.dta",replace

ci mean aggregate_nested_iv_1 aggregate_nested_iv_2 aggregate_logit_iv aggregate_logit_ols


***********************************Cross Price Elasticity for Nested Logit******
clear all 
set more off
timer clear
timer on 1
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\5_elasticity_simulation_data.dta", clear
keep product price market share_im share_ihgm share_igm winetype

preserve
timer on 2
contract market product winetype
drop _freq
rename product product_j
rename winetype winetype_j
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\market_product_j.dta", replace
timer off 2
restore

rename product product_i
rename winetype winetype_i
timer on 3

joinby market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\market_product_j.dta", unmatched(both)
capture drop _merge
order market product_i product_j
timer off 3

gen D1= 1 if product_i == product_j
gen D2= 1 if winetype_i == winetype_j    
gen D3= 1            
replace D1=0 if D1==.
replace D2=0 if D2==.
replace D3=0 if D3==.



forvalues i = 1/1000 {
preserve
merge m:1 market product_i using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\nested_simulated_share_`i'.dta"
drop _merge

gen nested_iv = (coeff_price_1)*[(1/(1-coeff_sgimaone_1))*D1-((1/(1-coeff_sgimaone_1))-(1/(1-coeff_sigmatwo_1)))*predict_share_ihgm*D2-((coeff_sigmatwo_1)/(1-coeff_sigmatwo_1))*predict_share_igm*D3-predict_sjm_hat]*price

collapse nested_iv, by(product_i product_j D1 D2 D3)       //averaging across market for each i-j product
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\cross_price_elasticity_`i'.dta", replace
restore
}

forvalues i=1/1000{	
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\cross_price_elasticity_`i'.dta", clear
drop if D1==1
preserve
collapse (mean) subgroup_elasticity = nested_iv if D2==1
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\nested_subgroup\nested_subgroup_elasticity_`i'.dta", replace
restore
collapse (mean) group_elasticity = nested_iv if D2==0 & D3==1
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\nested_group\nested_group_elasticity_`i'.dta", replace
}

use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\nested_subgroup\nested_subgroup_elasticity_1.dta", clear
forvalues i=2/1000{	
	append using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\nested_subgroup\nested_subgroup_elasticity_`i'.dta", 
}
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\nested_subgroup\nested_subgroup_elasticity.dta", replace
ci mean subgroup_elasticity


use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\nested_group\nested_group_elasticity_1.dta", clear
forvalues i=2/1000{	
	append using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\nested_group\nested_group_elasticity_`i'.dta", 
}
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Elasticity Estimation\cross_price_elasticity\nested_group\nested_group_elasticity.dta", replace
ci mean group_elasticity

