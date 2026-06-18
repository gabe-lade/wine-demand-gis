*This is do file number 8. 
*This do files calcualtes the industry revenue, consumer welfare (breaks down in
*price and variety effect                                                                                                        
********************************************************************************
clear all
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\6_welfare_estimation_data.dta", clear
drop if market==817| market==818| market ==819| market ==820| market ==821| market ==822| market ==825| market ==826 //markets that did not converge
merge 1:1 product market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\simulated_price_share_second_iteration.dta"
drop _merge
drop FE4_counterfactual
merge m:1 product using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\product_fe_coeff_2.dta"      
drop _merge 

*Baseline : calculate inclusive values and shares
gen deltajm_hat = -.16*price + FE1 + FE2 + FE3 + FE4 + residual_nested
gen deltajm_s1_hat = (deltajm_hat)/(1-.65)
gen exp_deltajm_s1_hat = exp(deltajm_s1_hat)                 
bysort market winetype: egen sum_exp_deltajm_s1_hat = total(exp_deltajm_s1_hat)
gen Ihgm_hat = (1-.65)*ln(sum_exp_deltajm_s1_hat)

preserve
keep market market_size winetype Ihgm_hat 
duplicates drop
gen Ihgm_s2_hat = exp(Ihgm_hat/(1-.47))
bysort market: egen summed_Ihgm_s2_hat = total(Ihgm_s2_hat)
gen Igm_hat = (1-.47)*ln(summed_Ihgm_s2_hat)
keep Igm_hat market market_size
duplicates drop
gen exp_Igm_hat = exp(Igm_hat)                   
gen Im_hat = ln(1+ exp_Igm_hat)
keep Im_hat Igm_hat exp_Igm_hat market market_size 
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\baseline_welfare.dta", replace
restore

merge m:1 market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\baseline_welfare.dta"
drop _merge
gen Ihgm_s2_hat = exp(Ihgm_hat/(1-.47))  
gen Ihgm_s1_hat = exp(Ihgm_hat/(1-.65))    
gen Igm_s2_hat = exp(Igm_hat/(1-.47))     
gen exp_Im_hat = exp(Im_hat)    
gen predict_sjm_hat = (exp_deltajm_s1_hat*Ihgm_s2_hat*exp_Igm_hat)/(Ihgm_s1_hat*Igm_s2_hat*exp_Im_hat)


*counterfactual scenario : calculate inclusive values and shares
gen deltajm_tilda = -.16*price_bn + FE1 + FE2 + FE3 + FE4_counterfactual + residual_nested
gen deltajm_s1_tilda = (deltajm_tilda)/(1-.65)
gen exp_deltajm_s1_tilda = exp(deltajm_s1_tilda)                 
bysort market winetype: egen sum_exp_deltajm_s1_tilda = total(exp_deltajm_s1_tilda)
gen Ihgm_tilda = (1-.65)*ln(sum_exp_deltajm_s1_tilda)

preserve
keep market market_size winetype Ihgm_tilda
duplicates drop
gen Ihgm_s2_tilda = exp(Ihgm_tilda/(1-.47))
bysort market: egen summed_Ihgm_s2_tilda = total(Ihgm_s2_tilda)
gen Igm_tilda = (1-.47)*ln(summed_Ihgm_s2_tilda)
keep Igm_tilda market market_size
duplicates drop
gen exp_Igm_tilda = exp(Igm_tilda)                   
gen Im_tilda = ln(1+ exp_Igm_tilda)
keep Im_tilda Igm_tilda exp_Igm_tilda market market_size 
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\counterfactual_welfare.dta", replace
restore

merge m:1 market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\counterfactual_welfare.dta"
drop _merge
gen Ihgm_s2_tilda = exp(Ihgm_tilda/(1-.47)) 
gen Ihgm_s1_tilda = exp(Ihgm_tilda/(1-.65))   
gen Igm_s2_tilda = exp(Igm_tilda/(1-.47))    
gen exp_Im_tilda = exp(Im_tilda)    

gen predict_sjm_tilda = (exp_deltajm_s1_tilda*Ihgm_s2_tilda*exp_Igm_tilda)/(Ihgm_s1_tilda*Igm_s2_tilda*exp_Im_tilda)


*capture variety and price effect
gen deltajm_tilda_b = -.16*price + FE1 + FE2 + FE3 + FE4_counterfactual + residual_nested
gen deltajm_s1_tilda_b = (deltajm_tilda_b)/(1-.65)
gen exp_deltajm_s1_tilda_b = exp(deltajm_s1_tilda_b)
bysort market winetype: egen sum_exp_deltajm_s1_tilda_b = total(exp_deltajm_s1_tilda_b)
gen Ihgm_tilda_b = (1- .65)*ln(sum_exp_deltajm_s1_tilda_b)

preserve
keep market market_size winetype Ihgm_tilda_b
duplicates drop
gen Ihgm_s2_tilda_b = exp(Ihgm_tilda_b/(1-.47))
bysort market: egen summed_Ihgm_s2_tilda_b = total(Ihgm_s2_tilda_b)
gen Igm_tilda_b = (1-.47)*ln(summed_Ihgm_s2_tilda_b)
keep Igm_tilda_b market market_size
duplicates drop
gen exp_Igm_tilda_b = exp(Igm_tilda_b)
gen Im_tilda_b = ln(1+ exp_Igm_tilda_b)
keep Im_tilda_b Igm_tilda_b exp_Igm_tilda_b market market_size 
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\price_and_variety_effect.dta",replace
restore

merge m:1 market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\price_and_variety_effect.dta"
drop _merge
gen Ihgm_s2_tilda_b = exp(Ihgm_tilda_b/(1-.47))  
gen Ihgm_s1_tilda_b = exp(Ihgm_tilda_b/(1-.65))    
gen Igm_s2_tilda_b = exp(Igm_tilda_b/(1-.47))    
gen exp_Im_tilda_b = exp(Im_tilda_b)   


*Checking qty predicted
bysort year: egen obs_sales = total(sales)
gen b_sales_qty = predict_sjm_hat*market_size
gen k_sales_qty = predict_sjm_tilda*market_size
bysort year: egen base_sales = total(b_sales_qty)
bysort year: egen k_sales = total(k_sales_qty)
replace base_sales = base_sales/1000000
replace obs_sales  = obs_sales/1000000
replace k_sales  = k_sales/1000000   //after checking- remove line 115-119


*Industry Rev
gen rev_hat = predict_sjm_hat*price
gen rev_tilda = predict_sjm_tilda*price_bn
bysort market: egen rev_hat_total = total(rev_hat)
bysort market: egen rev_tilda_total = total(rev_tilda)

preserve
keep market market_size rev_hat_total rev_tilda_total year  
duplicates drop
gen indutry_revenue = (rev_hat_total-rev_tilda_total)*market_size
bysort year: egen total_indutry_revenue = total(indutry_revenue)
replace total_indutry_revenue = total_indutry_revenue/1000000000
keep year total_indutry_revenue  
duplicates drop
export excel using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\welfare_table_4.xlsx", sheet("industry_revenue", replace) firstrow(variables)
restore 

*Price for baseline and counterfactual
replace appellation_label = "California" if California ==1 
bysort appellation_label: egen baseline_quantity = total(sales)
bysort appellation_label: egen baseline_revenue = total(sales*price)
gen baseline_average_price = baseline_revenue/baseline_quantity

preserve
keep baseline_average_price appellation_label
duplicates drop
export excel using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\welfare_table_4.xlsx", sheet("baseline_price", replace) firstrow(variables)
restore

replace appellation_label = "California" if appellation_label=="AVA"| appellation_label=="State_Appellation"
gen counterfactual_sales = predict_sjm_tilda*market_size
bysort appellation_label: egen counterfactual_quantity = total(counterfactual_sales)
bysort appellation_label: egen counterfactual_revenue = total(counterfactual_sales*price_bn)
gen counterfactual_average_price = counterfactual_revenue/counterfactual_quantity
keep counterfactual_average_price appellation_label
duplicates drop
export excel using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\welfare_table_4.xlsx", sheet("counterfactual_price", replace) firstrow(variables)
clear all


********************************************************************************

*Calcualte Consumer Welfare- table 4

use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\baseline_welfare.dta",clear
merge 1:1 market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\counterfactual_welfare.dta"
drop _merge
merge 1:1 market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\price_and_variety_effect.dta"
drop _merge


gen omega_m = (Im_hat-Im_tilda)/(.16)              //Total CW
gen omega_m_dollar= omega_m*market_size
egen total_cw = total(omega_m_dollar)
gen total_cw_billions = total_cw/1000000000

gen omega_mv = (Im_hat-Im_tilda_b)/(.16)              //Variety Effect
gen omega_mv_dollar = omega_mv*market_size
egen total_variety_effect = total(omega_mv_dollar)
gen total_ve_billions = total_variety_effect/1000000000

gen omega_mp = (Im_tilda_b-Im_tilda)/(.16)              //Price Effect
gen omega_mp_dollar = omega_mp*market_size
egen total_price_effect = total(omega_mp_dollar)
gen total_pe_billions = total_price_effect/1000000000


keep total_cw_billions total_ve_billions total_pe_billions 
duplicates drop

export excel using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Results\Welfare\welfare_table_4.xlsx", sheet("consumer_welfare", replace) firstrow(variables)
clear all
