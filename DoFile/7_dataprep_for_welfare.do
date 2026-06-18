********************************************************************************
*This is do file no. 7
*This do file prepares data for R- to recover MC and compute counterfcatucal price
*and share. Please look at each step as there are steps that requires running 
*R scripts**********************************************************************


********************************************************************************
*Step 1. Prepare data for R to compute marginal cost
********************************************************************************

clear all
set more off
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\5_elasticity_simulation_data.dta", clear
keep product price market share_im share_ihgm share_igm winetype brand_name	
export delimited using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\1_input_data_for_mc.csv", replace
clear all


********************************************************************************
*Step 2: Run compute_marginal_cost.R script to uncover marginal cost
********************************************************************************


********************************************************************************
*Step 3: Use marginal cost data computed using R script above
********************************************************************************
clear all
import delimited "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\2_marginal_cost.csv", encoding(ISO-8859-2)  
drop v1
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\2_marginal_cost.dta", replace
clear all


********************************************************************************
*Step 4:  Generate data for R for counterfactual estimation & merge MC data
********************************************************************************

use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\5_elasticity_simulation_data.dta", clear
drop if FE4==.        
merge 1:1 product market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\2_marginal_cost.dta"
drop if _merge==2        
drop _merge

preserve
bysort product: keep if _n==1

reghdfe FE4 i.size i.winetype Cabernet_Sauvignon-Other_Red_Imported Chardonnay-Other_White_Imported Dessert-Other_Specialty_Imported Alexander_Valley- Other_AVAs Florida-Other_States Argentina-Other_Countries, a(brand_name) vce(robust)

gen FE4_counterfactual = FE4 - _b[Alexander_Valley]*Alexander_Valley -_b[Amador_County]*Amador_County - _b[Anderson_Valley]*Anderson_Valley - _b[Arroyo_Seco]*Arroyo_Seco -_b[Augusta]*Augusta -_b[Carneros]*Carneros -_b[Central_Coast]*Central_Coast -_b[Chalk_Hill]*Chalk_Hill - _b[Chalone]*Chalone - _b[Clarksburg]*Clarksburg -_b[Columbia_Valley]*Columbia_Valley - _b[Contra_Costa_County]*Contra_Costa_County - _b[Dry_Creek_Valley]*Dry_Creek_Valley -_b[Dunnigan_Hills]*Dunnigan_Hills -_b[Eagle_Peak]*Eagle_Peak -_b[Edna_Valley]*Edna_Valley -_b[Eola_Hills]*Eola_Hills -_b[Finger_Lakes]*Finger_Lakes -_b[Guenoc]*Guenoc -_b[Horse_Heaven_Hills]*Horse_Heaven_Hills -_b[Knights_Valley]*Knights_Valley -_b[Lake_County]*Lake_County -_b[Lake_Erie]*Lake_Erie -_b[Livermore_Valley]*Livermore_Valley -_b[Lodi]*Lodi -_b[Mendocino]*Mendocino -_b[Mendocino_County]*Mendocino_County -_b[Monterey_County]*Monterey_County -_b[Napa_County]*Napa_County -_b[Napa_Valley]*Napa_Valley -_b[North_Coast]*North_Coast -_b[Oakville]*Oakville -  _b[Old_Mission_Peninsula]*Old_Mission_Peninsula -_b[Ohio_River_Valley]*Ohio_River_Valley -_b[Paso_Robles]*Paso_Robles -_b[Red_Hills_Lake_County]*Red_Hills_Lake_County -_b[Russian_River_Valley]*Russian_River_Valley -_b[Rutherford]*Rutherford -_b[Saint_Lucia_Highlands]*Saint_Lucia_Highlands -_b[San_Antonio]*San_Antonio -_b[Santa_Barbara_County]*Santa_Barbara_County -_b[Santa_Maria_Valley]*Santa_Maria_Valley -_b[Sierra_Foothills]*Sierra_Foothills -_b[Snake_River_Valley]*Snake_River_Valley -_b[Sonoma_County]*Sonoma_County -_b[Sonoma_Coast]*Sonoma_Coast -_b[Sonoma_Mountain]*Sonoma_Mountain -_b[Sonoma_Valley]*Sonoma_Valley -_b[St_Helena]*St_Helena -_b[Texas_High_Plains]*Texas_High_Plains -_b[Wahluke_Slope]*Wahluke_Slope -_b[Walla_Walla]*Walla_Walla -_b[Willamette_Valley]*Willamette_Valley - _b[Yakima_Valley]*Yakima_Valley - _b[Other_AVAs]*Other_AVAs - _b[Florida]*Florida - _b[Indiana]*Indiana - _b[Michigan]*Michigan - _b[Missouri]*Missouri - _b[Nebraska]*Nebraska - _b[New_York]*New_York - _b[North_Carolina]*North_Carolina - _b[Ohio]*Ohio - _b[Oregon]*Oregon - _b[Texas]*Texas -_b[Washington]*Washington - _b[Other_States]*Other_States 

keep product FE4_counterfactual FE4 
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\product_fe_coeff.dta", replace
restore  

merge m:1 product using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\product_fe_coeff.dta"
drop _merge
drop distribution_cost dis_sum retail_density_area retail_density_popu state_control excisetax 
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\6_welfare_estimation_data.dta", replace  

export delimited year market product price FE1 FE2 FE3 FE4 FE4_counterfactual winetype brand_name marginal_cost share_igm share_ihgm share_im residual_nested using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\data_for_counterfactual_first_iteration.csv", replace
clear all


********************************************************************************
*Step 5: Run R script "price_share_simulation_first_iteration.R"; run time ~4 hrs
*******************************************************************************


********************************************************************************
*Step 6: Use simulated price and share data generated from step 5
********************************************************************************
import delimited "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\simulated_price_share_first_iteration.csv", clear
drop price v1
rename price_sim price_bn
rename share_im_sim share_im_bn
*drop markets that did not converge
drop if market==817| market==818| market ==819| market ==820| market ==821| market ==822| market ==825| market ==826
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\simulated_price_share_first_iteration.dta", replace
clear all

********************************************************************************
*Step 7: For Akerlof, use simulated shares from first iteration (step 6)
*along with MUs, to calculate beta_bar_2:weighted average of estimated MU
*associated with US geographic origin
********************************************************************************
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\6_welfare_estimation_data.dta", clear
drop if market==817| market==818| market ==819| market ==820| market ==821| market ==822| market ==825| market ==826

merge 1:1 product market using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\simulated_price_share_first_iteration.dta"
drop _merge

egen domestic_quantity = total(share_im_bn)
egen import_share = total(share_im_bn) if appellation_label =="Imported"
replace import_share = import_share/domestic_quantity
bysort product: egen product_share = total(share_im_bn)
replace product_share = product_share/domestic_quantity

bysort product: keep if _n==1

reghdfe FE4 i.size i.winetype Cabernet_Sauvignon-Other_Red_Imported Chardonnay-Other_White_Imported Dessert-Other_Specialty_Imported Alexander_Valley- Other_AVAs Florida-Other_States Argentina-Other_Countries, a(brand_name) vce(robust)

//import share is 0.2411

gen beta_2_bar = (_b[Alexander_Valley]*Alexander_Valley*product_share)/(1-0.2411) +(_b[Amador_County]*Amador_County*product_share)/(1-0.2411) + (_b[Anderson_Valley]*Anderson_Valley*product_share)/(1-0.2411) + (_b[Arroyo_Seco]*Arroyo_Seco*product_share)/(1-0.2411)+(_b[Augusta]*Augusta*product_share)/(1-0.2411)+(_b[Carneros]*Carneros*product_share)/(1-0.2411)+(_b[Central_Coast]*Central_Coast*product_share)/(1-0.2411)+(_b[Chalk_Hill]*Chalk_Hill*product_share)/(1-0.2411)+(_b[Chalone]*Chalone*product_share)/(1-0.2411)+(_b[Clarksburg]*Clarksburg*product_share)/(1-0.2411)+(_b[Columbia_Valley]*Columbia_Valley*product_share)/(1-0.2411) + (_b[Contra_Costa_County]*Contra_Costa_County*product_share)/(1-0.2411)+ (_b[Dry_Creek_Valley]*Dry_Creek_Valley*product_share)/(1-0.2411) +(_b[Dunnigan_Hills]*Dunnigan_Hills*product_share)/(1-0.2411)+(_b[Eagle_Peak]*Eagle_Peak*product_share)/(1-0.2411)+(_b[Edna_Valley]*Edna_Valley*product_share)/(1-0.2411)+(_b[Eola_Hills]*Eola_Hills*product_share)/(1-0.2411)+(_b[Finger_Lakes]*Finger_Lakes*product_share)/(1-0.2411)+(_b[Guenoc]*Guenoc*product_share)/(1-0.2411)+(_b[Horse_Heaven_Hills]*Horse_Heaven_Hills*product_share)/(1-0.2411)+(_b[Knights_Valley]*Knights_Valley*product_share)/(1-0.2411)+(_b[Lake_County]*Lake_County*product_share)/(1-0.2411)+(_b[Lake_Erie]*Lake_Erie*product_share)/(1-0.2411)+(_b[Livermore_Valley]*Livermore_Valley*product_share)/(1-0.2411)+(_b[Lodi]*Lodi*product_share)/(1-0.2411)+(_b[Mendocino]*Mendocino*product_share)/(1-0.2411) +(_b[Mendocino_County]*Mendocino_County*product_share)/(1-0.2411)+(_b[Monterey_County]*Monterey_County*product_share)/(1-0.2411)+(_b[Napa_County]*Napa_County*product_share)/(1-0.2411)+(_b[Napa_Valley]*Napa_Valley*product_share)/(1-0.2411)+(_b[North_Coast]*North_Coast*product_share)/(1-0.2411)+(_b[Oakville]*Oakville*product_share)/(1-0.2411)+ (_b[Old_Mission_Peninsula]*Old_Mission_Peninsula*product_share)/(1-0.2411)+ (_b[Ohio_River_Valley]*Ohio_River_Valley*product_share)/(1-0.2411)+(_b[Paso_Robles]*Paso_Robles*product_share)/(1-0.2411)+(_b[Red_Hills_Lake_County]*Red_Hills_Lake_County*product_share)/(1-0.2411)+(_b[Russian_River_Valley]*Russian_River_Valley*product_share)/(1-0.2411)+(_b[Rutherford]*Rutherford*product_share)/(1-0.2411)+(_b[Saint_Lucia_Highlands]*Saint_Lucia_Highlands*product_share)/(1-0.2411)+(_b[San_Antonio]*San_Antonio*product_share)/(1-0.2411)+(_b[Santa_Barbara_County]*Santa_Barbara_County*product_share)/(1-0.2411)+(_b[Santa_Maria_Valley]*Santa_Maria_Valley*product_share)/(1-0.2411)+(_b[Sierra_Foothills]*Sierra_Foothills*product_share)/(1-0.2411)+(_b[Snake_River_Valley]*Snake_River_Valley*product_share)/(1-0.2411) + (_b[Sonoma_County]*Sonoma_County*product_share)/(1-0.2411) + (_b[Sonoma_Coast]*Sonoma_Coast*product_share)/(1-0.2411)+(_b[Sonoma_Valley]*Sonoma_Valley*product_share)/(1-0.2411) +(_b[St_Helena]*St_Helena*product_share)/(1-0.2411) + (_b[Texas_High_Plains]*Texas_High_Plains*product_share)/(1-0.2411)+(_b[Wahluke_Slope]*Wahluke_Slope*product_share)/(1-0.2411)+(_b[Walla_Walla]*Walla_Walla*product_share)/(1-0.2411)+(_b[Willamette_Valley]*Willamette_Valley*product_share)/(1-0.2411)+(_b[Yakima_Valley]*Yakima_Valley*product_share)/(1-0.2411)+(_b[Other_AVAs]*Other_AVAs*product_share)/(1-0.2411)+(_b[Florida]*Florida*product_share)/(1-0.2411)+(_b[Indiana]*Indiana*product_share)/(1-0.2411)+ (_b[Michigan]*Michigan*product_share)/(1-0.2411)+ (_b[Missouri]*Missouri*product_share)/(1-0.2411) + (_b[Nebraska]*Nebraska*product_share)/(1-0.2411) + (_b[New_York]*New_York*product_share)/(1-0.2411)+(_b[North_Carolina]*North_Carolina*product_share)/(1-0.2411)+(_b[Ohio]*Ohio*product_share)/(1-0.2411)+(_b[Oregon]*Oregon*product_share)/(1-0.2411)+(_b[Texas]*Texas*product_share)/(1-0.2411)+(_b[Washington]*Washington*product_share)/(1-0.2411) + (_b[Other_States]*Other_States*product_share)/(1-0.2411)  

egen beta_bar_2_final = total(beta_2_bar)
replace beta_bar_2_final = 0 if appellation_label=="Imported"   //beta_bar_2_fianl = -0.017134
gen FE4_cf_2 = FE4_counterfactual + beta_bar_2_final
keep product FE4 FE4_cf_2
rename FE4_cf_2 FE4_counterfactual
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\product_fe_coeff_2.dta", replace

********************************************************************************
*Step 8: Prepare data for R for iteration 2
********************************************************************************
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\6_welfare_estimation_data.dta", clear
drop if market==817| market==818| market ==819| market ==820| market ==821| market ==822| market ==825| market ==826
drop FE4_counterfactual
merge m:1 product using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\product_fe_coeff_2.dta"
drop _merge
export delimited year market product price FE1 FE2 FE3 FE4 FE4_counterfactual winetype brand_name marginal_cost share_igm share_ihgm share_im residual_nested using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\data_for_counterfactual_second_iteration.csv", replace
clear all

********************************************************************************
*Step 9: Run R script "price_share_simulation_second_iteration.R" (run time ~4 hrs)
********************************************************************************

********************************************************************************
*Step 10:Use simulated price and share data generated in step 9 and prepare final 
*file for welfare calculation
********************************************************************************
import delimited "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\simulated_price_share_second_iteration.csv", clear
drop price v1
rename price_sim price_bn
rename share_im_sim share_im_bn
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\Welfare\simulated_price_share_second_iteration.dta", replace
clear all
**************************************

*This is the end of this do file************************************************