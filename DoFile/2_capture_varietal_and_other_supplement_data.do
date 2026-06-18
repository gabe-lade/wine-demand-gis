*****************************************************************************************************************************
*Do File No. 2
*This do file extracts varietal information from upc description, makes brand name unique and compiles other supplement data
*Supplement Data includes origin information from web search based on brand name, excise tax, adult population data, 
*data for distance instrument
*****************************************************************************************************************************

set more off
clear all

use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\1_capture_geographic_origin.dta", clear

*******************************WINE TYPE***********************************
gen match_red = regexm(" " + upc_descr  + " ", " (RED) ")
gen match_white = regexm(" " + upc_descr  + " ", " (WT) ")
gen match_blush = regexm(" " + upc_descr  + " ", " (BLS) ")
gen match_rose= regexm(" " + upc_descr  + " ", " (RSE) ") 
replace match_blush =0 if brand_descr =="WOLF BLASS" & match_red==1    // capturing the bls from brand name but not really bls
replace match_blush =0 if brand_descr =="WOLF BLASS" & match_white==1   

replace match_red =0 if match_blush ==1| match_rose==1
replace match_white =0 if match_blush ==1| match_rose==1


*to fix the overalp of type as some brand names have 'red' word in it and same for white

replace match_red =0 if upc_descr =="KIONA RED MT CHN BL V WT DDT"| upc_descr=="RED BICYCLETTE FR CHRD WT IDT"| upc_descr=="RED BUD VGNR V WT DDT"| upc_descr=="RED CANYON NZ SV-B WT IDT"| upc_descr=="RED DMND CHRD WSH V WT DDT"| upc_descr=="RED IMPULSE CHRD C-V V WT DDT"| upc_descr=="RED KNOT AS CHRD WT IDT"| upc_descr=="RED LION CHRD CA V WT DDT"| upc_descr=="RED MUD AS CHRD WT IDT"| upc_descr=="RED ROVER CHRD V WT DDT"| upc_descr=="RED THEORY CHRD C-V V WT DDT"| upc_descr=="HA-MKT GM RES AL WT RED"

replace match_white=0 if upc_descr=="S-HM WT ZN CA V RED DDT" | upc_descr=="SAN CAMILLUS IT P-GR WT RED"| upc_descr=="WT OAK VNYD CB-S NV V RED DDT"| upc_descr=="WT OAK VNYD CB-S V RED DDT"| upc_descr=="WT OAK VNYD MRLT NV V RED DDT"| upc_descr=="WT OAK VNYD ZN V RED DDT"| upc_descr=="WT PL ARG CB-S RED IDT"| upc_descr=="WT PL ARG MLBC CB RED IDT" 

replace match_blush=0 if match_rose==1   

gen wine_type=""
replace wine_type="RED" if match_red==1
replace wine_type="WHITE" if match_white==1
replace wine_type="OTHER" if match_blush==1| match_rose==1
replace wine_type="OTHER" if wine_type==""    //for which we do not have information from upc description- 220,738 obs-

drop match_red match_white    

*****RESTRICTION 1

*Categorize only table wine as red or white, rest all other product module is classified as Other type
replace wine_type ="OTHER" if product_module_descr!="WINE-DOMESTIC DRY TABLE" & product_module_descr!="WINE-IMPORTED DRY TABLE" & product_module_descr!="WINE-KOSHER TABLE"


************************VERIETALS************************************
gen Cab_Sau=regexm(" " + upc_descr  + " ", " (CB-S) ")| regexm(" " + upc_descr  + " ", " (LM-CS) ") 
gen Chard= regexm(" " + upc_descr  + " ", " (CHRD) ")| regexm(" " + upc_descr  + " ", " (QN-R-CHRD) ")
replace Chard=1 if upc_descr=="LITTLE ROO AS CHD WT IDT"
gen Pinot_G= regexm(" " + upc_descr  + " ", " (P-GR) ")| regexm(" " + upc_descr  + " ", " (P-GRS) ")
gen Riesling= regexm(" " + upc_descr  + " ", " (RES) ")| regexm(" " + upc_descr  + " ", " (RES-SP) ")| regexm(" " + upc_descr  + " ", " (P-G-R-S) ")    //res-sp- reiseling spatlese; p-g-r-s- rest is region and name
gen Malbec =regexm(" " + upc_descr  + " ", " (MLBC) ")
gen Zinfandel= regexm(" " + upc_descr  + " ", " (ZN) ") | regexm(" " + upc_descr  + " ", " (TB-ZN) ")
gen Merlot= regexm(" " + upc_descr  + " ", " (MRLT) ")| regexm(" " + upc_descr  + " ", " (MBMRLT) ")
gen Moscato= regexm(" " + upc_descr  + " ", " (MSC) ")| regexm(" " + upc_descr  + " ", " (MSCT) ")| regexm(" " + upc_descr  + " ", " (MSCTL) ")| regexm(" " + upc_descr  + " ", " (AR-MS) ")| regexm(" " + upc_descr  + " ", " (MUSCATO) ")   //arien moscato
gen Pinot_N= regexm(" " + upc_descr  + " ", " (P-N) ")| regexm(" " + upc_descr  + " ", " (G2S-PN) ")
gen Bonarda= regexm(" " + upc_descr  + " ", " (BNRDA) ")
gen Sauvignon_Blanc= regexm(" " + upc_descr  + " ", " (SV-B) ")
gen Macabeo = regexm(" " + upc_descr  + " ", " (MACABEO) ")|regexm(" " + upc_descr  + " ", " (VIURA) ")  
gen Tempranillo = regexm(" " + upc_descr  + " ", " (TMPN) ")
gen Pinotage= regexm(" " + upc_descr  + " ", " (PNTG) ")      
gen SHZ=regexm(" " + upc_descr  + " ", " (SHZ) ")| regexm(" " + upc_descr  + " ", " (SHR) ")| regexm(" " + upc_descr  + " ", " (RS-SZ) ")
gen Syrah=regexm(" " + upc_descr  + " ", " (SYR) ")| regexm(" " + upc_descr  + " ", " (SYR-ME) ") 
gen Primitivo= regexm(" " + upc_descr  + " ", " (PRMTV) ")     
gen Soave= regexm(" " + upc_descr  + " ", " (SOAVE) ")      
gen Mencia= regexm(" " + upc_descr  + " ", " (MNC) ")|regexm(" " + upc_descr  + " ", " (MENCIA) ") 
gen Xarel= regexm(" " + upc_descr  + " ", " (XAREL) ")
gen Torrontes= regexm(" " + upc_descr  + " ", " (TRNTS) ") if appellation_label=="Imported"    //maily from argentina-domestic entries were capturing the abb for brand name
replace Torrontes=0 if Torrontes==.      //coz of country condition its creating missing
gen Monastrell= regexm(" " + upc_descr  + " ", " (MNSTRL) ")| regexm(" " + upc_descr  + " ", " (RD-MSTL) ")    //all spain
gen Liebfraumilch= regexm(" " + upc_descr  + " ", " (LIEB) ")| regexm(" " + upc_descr  + " ", " (MDNA-LIEB) ")    //all entry from Germany except 1- germn style wine mostly reiseling

gen Sangiovese= regexm(" " + upc_descr  + " ", " (SANG) ") if Importing_country=="Italy"  //keep only if imported from Italy coz there are domestic entries from California
replace Sangiovese=0 if Sangiovese==.
gen Bukettraube = regexm(" " + upc_descr  + " ", " (BKTR) ")  
gen Godello= regexm(" " + upc_descr  + " ", " (GODELLO) ")   //spain
gen Nebbiolo=regexm(" " + upc_descr  + " ", " (NBLO) ")   
gen Vermentio=regexm(" " + upc_descr  + " ", " (VRMNT) ") if Importing_country=="Italy"
replace Vermentio=0 if Vermentio==.
gen Rosato= regexm(" " + upc_descr  + " ", " (RSTO) ") if Importing_country=="Italy"
replace Rosato=0 if Rosato==.
gen Rosso= regexm(" " + upc_descr  + " ", " (RSO) ") if Importing_country=="Italy"
replace Rosso=0 if Rosso==.
gen Verdejo= regexm(" " + upc_descr  + " ", " (Verdejo) ")| regexm(" " + upc_descr  + " ", " (VERDEJO) ")|regexm(" " + upc_descr  + " ", " (VRDJOVU) ")  if Importing_country=="Spain"
replace Verdejo=0 if Verdejo==.
gen Pinot_Nero= regexm(" " + upc_descr  + " ", " (PNT NERO) ") // capturing only Italy
gen Albarinho=regexm(" " + upc_descr  + " ", " (ALVARINHO) ")| regexm(" " + upc_descr  + " ", " (ALBRNO) ")  
gen Inzolia= regexm(" " + upc_descr  + " ", " (INZOLIA) ") if Importing_country=="Italy"
replace Inzolia=0 if Inzolia==. 
gen Dornfelder=regexm(" " + upc_descr  + " ", " (DNR) ")| regexm(" " + upc_descr  + " ", " (DNRFLDR) ") if Importing_country=="Germany"
replace Dornfelder=0 if Dornfelder==.
gen Negro_Amaro= regexm(" " + upc_descr  + " ", " (NG AMARO) ")
gen Lambrusco=regexm(" " + upc_descr  + " ", " (LMBRSC) ")| regexm(" " + upc_descr  + " ", " (LMB) ") if Importing_country=="Italy"
replace Lambrusco=0 if Lambrusco==.
gen Tarrango=regexm(" " + upc_descr  + " ", " (TARRANGO) ") if Importing_country=="Australia"
replace Tarrango=0 if Tarrango==.
gen Malavasia=regexm(" " + upc_descr  + " ", " (MLVSA) ")  
gen Teroldego=regexm(" " + upc_descr  + " ", " (TEROLDEGO) ")  
gen Barbera=regexm(" " + upc_descr  + " ", " (BARB) ")
gen Trebbiano=regexm(" " + upc_descr  + " ", " (TRB) ") 
gen NeroDAvola=regexm(" " + upc_descr  + " ", " (NR-DA) ")  
gen Merlot_NeroDAvola=regexm(" " + upc_descr  + " ", " (MRLT NR-DA) ")
replace Merlot=0 if Merlot_NeroDAvola==1
replace NeroDAvola=0 if Merlot_NeroDAvola==1
gen Falanghina=regexm(" " + upc_descr  + " ", " (FALANGHINA) ") 
gen Grillo= regexm(" " + upc_descr  + " ", " (GRILLO) ")    
gen Gamay=regexm(" " + upc_descr  + " ", " (GMY) ") & appellation_label=="Imported" //capturing some of the domestic
replace Gamay=0 if Gamay==.
gen Gruner_Veltliner=regexm(" " + upc_descr  + " ", " (GRNR-V) ")    
gen Piesporter_Michelsberg= regexm(" " + upc_descr  + " ", " (PST MC) ")    
gen Grenache=regexm(" " + upc_descr  + " ", " (GRNCHA) ")| regexm(" " + upc_descr  + " ", " (GRN) ")| regexm(" " + upc_descr  + " ", " (VRN-GH) ")

*Blends-
*Pinot Gris Chardonnay
gen PinotG_Chard1=regexm(" " + upc_descr  + " ", " (P-GR) ") & regexm(" " + upc_descr  + " ", " (CHRD) ")
gen PinotG_Chard2=regexm(" " + upc_descr  + " ", " (P-GRS) ") & regexm(" " + upc_descr  + " ", " (CHRD) ")
gen PinotG_Chard3=regexm(" " + upc_descr  + " ", " (CHRD&P-GR) ") 
gen PinotG_Chard4=regexm(" " + upc_descr  + " ", " (P-G-C) ") 
gen PinotG_Chard=PinotG_Chard1+PinotG_Chard2+PinotG_Chard3+PinotG_Chard4
replace Chard=0 if PinotG_Chard==1
replace Pinot_G=0 if PinotG_Chard==1
drop PinotG_Chard1 PinotG_Chard2 PinotG_Chard3 PinotG_Chard4

*Bonarda Blend
gen Bonarda_Merlot= regexm(" " + upc_descr  + " ", " (B-MLT) ")
gen Bonarda_Malbec= regexm(" " + upc_descr  + " ", " (MLBC&BNRDA) ")
gen Bonarda_Syrah= regexm(" " + upc_descr  + " ", " (SYR BNRDA) ")
replace Syrah=0 if Bonarda_Syrah==1
replace Bonarda=0 if Bonarda_Syrah==1

gen SauBlanc_Semilon= regexm(" " + upc_descr  + " ", " (SV-B) ") & regexm(" " + upc_descr  + " ", " (SML) ")| regexm(" " + upc_descr  + " ", " (SB-SM) ")
gen Semilon = regexm(" " + upc_descr  + " ", " (SML) ")
replace Semilon=0 if brand_descr=="SMALL GULLY MR. BLACK'S CNCCTN"| brand_descr=="SMALL WONDERS"| brand_descr=="SAMUEL SMITH" 
replace Sauvignon_Blanc=0 if SauBlanc_Semilon==1
replace Semilon=0 if SauBlanc_Semilon==1

gen Merlot_Pinotage= regexm(" " + upc_descr  + " ", " (MRLT PNTG) ")
replace Pinotage=0 if Merlot_Pinotage==1
replace Merlot=0 if Merlot_Pinotage==1

gen SHZ_Pinotage= regexm(" " + upc_descr  + " ", " (PNTG) ") & regexm(" " + upc_descr  + " ", " (SHZ) ")
replace Pinotage=0 if SHZ_Pinotage==1
replace SHZ=0 if SHZ_Pinotage==1

gen Monas_Tempr=  regexm(" " + upc_descr  + " ", " (MTL-TMP) ")

gen Pinot_Binaco= regexm(" " + upc_descr  + " ", " (P-BNC) ") 
gen Syr_Gr_Mourvedre= regexm(" " + upc_descr  + " ", " (SG-MV) ")   

gen Sang_Cab_Merlot= regexm(" " + upc_descr  + " ", " (S-CB-S&MRLT) ")  
gen Mns_Syrah= regexm(" " + upc_descr  + " ", " (MO-S) ")

gen Ugni_blanc= regexm(" " + upc_descr  + " ", " (UGNI BL) ") 

gen Sang_Merlot=regexm(" " + upc_descr  + " ", " (SANG MRLT) ")
replace Sangiovese=0 if Sang_Merlot==1
replace Merlot=0 if Sang_Merlot==1

*Cab-Merlot
gen Cab_Merlo1= regexm(" " + upc_descr  + " ", " (CB-M) ")| regexm(" " + upc_descr  + " ", " (MCS) ")| regexm(" " + upc_descr  + " ", " (RBDX-M&CS) ")
gen Cab_Merlo2= regexm(" " + upc_descr  + " ", " (MRLT) ") & regexm(" " + upc_descr  + " ", " (CB-S) ")
gen Cab_Merlo3=regexm(" " + upc_descr  + " ", " (CBSVM) ")
gen Cab_Merlo=Cab_Merlo1+Cab_Merlo2+Cab_Merlo3
drop Cab_Merlo1 Cab_Merlo2 Cab_Merlo3
replace Cab_Sau=0 if Cab_Merlo==1
replace Merlot=0 if Cab_Merlo==1

*Cab_SHZ
gen Cab_SHZ1= regexm(" " + upc_descr  + " ", " (CB) ") & regexm(" " + upc_descr  + " ", " (SHZ) ")
gen Cab_SHZ2= regexm(" " + upc_descr  + " ", " (CB-S) ") & regexm(" " + upc_descr  + " ", " (SHZ) ")
gen Cab_SHZ3= regexm(" " + upc_descr  + " ", " (SHZ CBSVM) ")
gen Cab_SHZ4= regexm(" " + upc_descr  + " ", " (CS-SZ) ")
gen Cab_SHZ5= regexm(" " + upc_descr  + " ", " (MNR-CBSHZ) ")      
gen Cab_SHZ6= regexm(" " + upc_descr  + " ", " (PVCSS) ")  
gen Cab_SHZ=Cab_SHZ1+ Cab_SHZ2+ Cab_SHZ3+ Cab_SHZ4+ Cab_SHZ5+Cab_SHZ6
replace Cab_Sau=0 if Cab_SHZ==1
replace SHZ =0 if Cab_SHZ==1
drop Cab_SHZ1 Cab_SHZ2 Cab_SHZ3 Cab_SHZ4 Cab_SHZ5 Cab_SHZ6 


*Cab Syrah
gen Cab_Syr1= regexm(" " + upc_descr  + " ", " (CB-S) ") & regexm(" " + upc_descr  + " ", " (SYR) ")
gen Cab_Syr2= regexm(" " + upc_descr  + " ", " (CB) ") & regexm(" " + upc_descr  + " ", " (SYR) ")
gen Cab_Syr= Cab_Syr1+Cab_Syr2
replace Syrah=0 if Cab_Syr==1
replace Cab_Sau=0 if Cab_Syr==1
drop Cab_Syr1 Cab_Syr2 

gen Cab_Syr_Mouv= regexm(" " + upc_descr  + " ", " (CB-S) ") & regexm(" " + upc_descr  + " ", " (SY-M) ")

*Cab_TMPN
gen Cab_TMPN=regexm(" " + upc_descr  + " ", " (CB-S) ") & regexm(" " + upc_descr  + " ", " (TMPN) ")
replace Cab_Sau=0 if Cab_TMPN==1
replace Tempranillo=0 if Cab_TMPN==1

*Cab Malbec
gen Cab_Malbec1=regexm(" " + upc_descr  + " ", " (MLBC) ") & regexm(" " + upc_descr  + " ", " (CB-S) ")
gen Cab_Malbec2=regexm(" " + upc_descr  + " ", " (CB MLBC) ")
gen Cab_Malbec=Cab_Malbec1+Cab_Malbec2
replace Cab_Sau=0 if Cab_Malbec==1
replace Malbec=0 if Cab_Malbec==1
drop Cab_Malbec1 Cab_Malbec2

*SHZ_GRE
gen SHZ_GRE1=regexm(" " + upc_descr  + " ", " (SHZ) ") & regexm(" " + upc_descr  + " ", " (GRN) ")
gen SHZ_GRE2=regexm(" " + upc_descr  + " ", " (SYR) ") & regexm(" " + upc_descr  + " ", " (GRN) ")
gen SHZ_GRE3=regexm(" " + upc_descr  + " ", " (CDRVGSY) ")    
gen SHZ_GRE4= regexm(" " + upc_descr  + " ", " (GRN&SHZ) ")
gen SHZ_GRE5= regexm(" " + upc_descr  + " ", " (G&S) ")
gen SHZ_GRE6= regexm(" " + upc_descr  + " ", " (GR-S) ")
gen SHZ_GRE7= regexm(" " + upc_descr  + " ", " (LC-GSY) ")
gen SHZ_GRE8= regexm(" " + upc_descr  + " ", " (RDGRNSYR) ")     
gen SHZ_GRE9= regexm(" " + upc_descr  + " ", " (GRNSYR) ")  
gen SHZ_GRE=SHZ_GRE1+SHZ_GRE2+ SHZ_GRE3+SHZ_GRE4+ SHZ_GRE5+ SHZ_GRE6+SHZ_GRE7+ SHZ_GRE8+ SHZ_GRE9
replace SHZ=0 if SHZ_GRE==1
replace Grenache=0 if SHZ_GRE==1
drop SHZ_GRE1 SHZ_GRE2 SHZ_GRE3 SHZ_GRE4 SHZ_GRE5 SHZ_GRE6 SHZ_GRE7 SHZ_GRE8 SHZ_GRE9


*SHZ Merlot
gen SHZ_Merlot=regexm(" " + upc_descr  + " ", " (SHZ) ") & regexm(" " + upc_descr  + " ", " (MRLT) ")
replace SHZ=0 if SHZ_Merlot==1
replace Merlot=0 if SHZ_Merlot==1

*Syrah-Mourvedre
gen Syrah_Mourvedre=regexm(" " + upc_descr  + " ", " (SY-M) ")| regexm(" " + upc_descr  + " ", " (SYR-M) ")

*SHZ MAlBEC
gen SHZ_MLBC1=regexm(" " + upc_descr  + " ", " (SHZ) ") & regexm(" " + upc_descr  + " ", " (MLBC) ")
gen SHZ_MLBC2=regexm(" " + upc_descr  + " ", " (SYR) ") & regexm(" " + upc_descr  + " ", " (MLBC) ")
gen SHZ_Malbec=SHZ_MLBC1+SHZ_MLBC2
replace SHZ=0 if SHZ_Malbec==1
replace Malbec=0 if SHZ_Malbec==1
drop SHZ_MLBC1 SHZ_MLBC2 

*SHZ Pinot
gen SHZ_PinotN=regexm(" " + upc_descr  + " ", " (SYR) ") & regexm(" " + upc_descr  + " ", " (P-N) ")
replace SHZ=0 if SHZ_PinotN==1
replace Pinot_N=0 if SHZ_PinotN==1

*SHZ TMPN
gen SHZ_TMPN1= regexm(" " + upc_descr  + " ", " (SHZ TMPN) ")
gen SHZ_TMPN2= regexm(" " + upc_descr  + " ", " (SYR TMPN) ")
gen SHZ_TMPN3= regexm(" " + upc_descr  + " ", " (TMPN SHZ) ")
gen SHZ_TMPN=SHZ_TMPN1+SHZ_TMPN2+SHZ_TMPN3
replace SHZ=0 if SHZ_TMPN==1
replace Tempranillo=0 if SHZ_TMPN==1
drop SHZ_TMPN1 SHZ_TMPN2 SHZ_TMPN3 

*SHZ ZIN
gen Zin_SHZ= regexm(" " + upc_descr  + " ", " (ZN) ") & regexm(" " + upc_descr  + " ", " (SHZ) ")  
replace Zinfandel=0 if Zin_SHZ==1
replace SHZ=0 if Zin_SHZ==1

*GRE TMPN
gen Gren_TMPN = regexm(" " + upc_descr  + " ", " (GRNCHA) ") & regexm(" " + upc_descr  + " ", " (TMPN) ")
replace Grenache=0 if Gren_TMPN==1
replace Tempranillo=0 if Gren_TMPN==1

*GRE Merlot
gen Merlot_Gre1=regexm(" " + upc_descr  + " ", " (MRLT) ") & regexm(" " + upc_descr  + " ", " (GRN) ")
gen Merlot_Gre2=regexm(" " + upc_descr  + " ", " (GRN&MRLT) ") 
gen Merlot_Gre=Merlot_Gre1+Merlot_Gre2
replace Merlot=0 if Merlot_Gre==1
replace Grenache=0 if Merlot_Gre==1
drop Merlot_Gre1 Merlot_Gre2

*Merlot Pinot Noir
gen Merlot_PN=regexm(" " + upc_descr  + " ", " (MRLT) ") & regexm(" " + upc_descr  + " ", " (P-N) ")
replace Pinot_N=0 if Merlot_PN==1
replace Merlot=0 if Merlot_PN==1

*Merlot Malbec
gen Merlot_MLBC=regexm(" " + upc_descr  + " ", " (MRLT) ") & regexm(" " + upc_descr  + " ", " (MLBC) ")
replace Merlot=0 if Merlot_MLBC==1
replace Malbec=0 if Merlot_MLBC==1

*TMPN Malbec
gen TMPN_MLBC=regexm(" " + upc_descr  + " ", " (TMPN MLBC) ") 
replace Malbec=0 if TMPN_MLBC==1
replace Tempranillo=0 if TMPN_MLBC==1

*Zin Grenache
gen Zin_Gre= regexm(" " + upc_descr  + " ", " (ZN) ") & regexm(" " + upc_descr  + " ", " (GRN) ")
replace Zinfandel=0 if Zin_Gre==1
replace Grenache=0 if Zin_Gre==1

*Zin Chard
gen Zin_Chardonnay=regexm(" " + upc_descr  + " ", " (ZN) ") & regexm(" " + upc_descr  + " ", " (CHRD) ")
replace Zinfandel=0 if Zin_Chardonnay==1
replace Chard=0 if Zin_Chardonnay==1

*Zin Moscato
gen Zin_Moscato=regexm(" " + upc_descr  + " ", " (ZN) ") & regexm(" " + upc_descr  + " ", " (MSC) ")
replace Zinfandel=0 if Zin_Moscato==1
replace Moscato=0 if Zin_Moscato==1

*Suu Chard
gen SB_CHARD=regexm(" " + upc_descr  + " ", " (SV-B) ") & regexm(" " + upc_descr  + " ", " (CHRD) ")| regexm(" " + upc_descr  + " ", " (CHRD&SV-B) ")
replace Sauvignon_Blanc=0 if SB_CHARD==1
replace Chard=0 if SB_CHARD==1

gen Champgane_other=regexm(" " + upc_descr  + " ", " (CHM) ") if Importing_country!="France"
replace Champgane_other=0 if Champgane_other==.

gen Chard_Chm=regexm(" " + upc_descr  + " ", " (CHRD) ") & regexm(" " + upc_descr  + " ", " (CHM) ")   //all dm
replace Chard=0 if Chard_Chm==1
replace Champgane_other=0 if Chard_Chm==1

gen MSC_CHM=regexm(" " + upc_descr  + " ", " (CHM) ") & regexm(" " + upc_descr  + " ", " (MSC) ")     //all dm
replace Moscato=0 if MSC_CHM==1
replace Champgane_other=0 if MSC_CHM==1

gen PinotG_CHM=regexm(" " + upc_descr  + " ", " (CHM) ") & regexm(" " + upc_descr  + " ", " (P-GR) ")    //all dm
replace Pinot_G=0 if PinotG_CHM==1
replace Champgane_other=0 if PinotG_CHM==1

gen Zin_CHM=regexm(" " + upc_descr  + " ", " (CHM) ") & regexm(" " + upc_descr  + " ", " (ZN) ")    //all dm
replace Zinfandel=0 if Zin_CHM==1 
replace Champgane_other=0 if Zin_CHM==1

gen Carmenere=regexm(" " + upc_descr  + " ", " (CRMNR) ")| regexm(" " + upc_descr  + " ", " (CRMNRE) ") 

gen Concord1=regexm(" " + upc_descr  + " ", " (CONCORD) ")
gen Concord2=regexm(" " + upc_descr  + " ", " (CON) ")
gen Concord=Concord1+Concord2
replace Concord=0 if brand_descr=="CON CARNE"
drop Concord1 Concord2

gen Meritage=regexm(" " + upc_descr  + " ", " (MRTG) ")

gen Meri_Chard=regexm(" " + upc_descr  + " ", " (MRTG) ") & regexm(" " + upc_descr  + " ", " (CHRD) ")
replace Meritage=0 if Meri_Chard==1
replace Chard=0 if Meri_Chard==1

gen Gewur=regexm(" " + upc_descr  + " ", " (GWRZT) ")

gen P_Sirah=regexm(" " + upc_descr  + " ", " (P-SRH) ") | regexm(" " + upc_descr  + " ", " (P-SYR) ")

gen Red_Blend=regexm(" " + upc_descr  + " ", " (RB) ")

gen White_Blend=regexm(" " + upc_descr  + " ", " (WB) ")


gen CAVA=regexm(" " + upc_descr  + " ", " (CAVA) ")

gen Chianti_other=regexm(" " + upc_descr  + " ", " (CHNT) ")| regexm(" " + upc_descr  + " ", " (CHNTI) ")| regexm(" " + upc_descr  + " ", " (CHNT-S) ")| regexm(" " + upc_descr  + " ", " (CHNTI-SC) ") if Importing_country!="Italy"
replace Chianti_other=0 if Chianti_other==.

gen Port=regexm(" " + upc_descr  + " ", " (PORT) ")

gen Psrh_Port=regexm(" " + upc_descr  + " ", " (PORT) ") & regexm(" " + upc_descr  + " ", " (P-SRH) ")
replace P_Sirah=0 if Psrh_Port==1
replace Port=0 if Psrh_Port==1

gen Sherry=regexm(" " + upc_descr  + " ", " (SHRY) ")

gen Burgandy_other=regexm(" " + upc_descr  + " ", " (BRG) ") & Importing_country!="France"
replace Burgandy_other=0 if Burgandy_other==.


gen Chablis_other=regexm(" " + upc_descr  + " ", " (CHB) ")| regexm(" " + upc_descr  + " ", " (PT-CHB) ") if Importing_country!="France"
replace Chablis_other=0 if Chablis_other==.


gen Rhine=regexm(" " + upc_descr  + " ", " (RHN) ") if Importing_country!="France"   
replace Rhine=0 if Rhine==.

gen Muscadine=regexm(" " + upc_descr  + " ", " (MSCDN) ")

gen Marsala=regexm(" " + upc_descr  + " ", " (MRSL) ")

gen Chard_PN=regexm(" " + upc_descr  + " ", " (P-N) ") & regexm(" " + upc_descr  + " ", " (CHRD) ")
replace Chard=0 if Chard_PN==1
replace Pinot_N=0 if Chard_PN==1

gen Pinot_Blanc=regexm(" " + upc_descr  + " ", " (P-BL) ")

gen Norton=regexm(" " + upc_descr  + " ", " (NORTON) ")

gen CheninB_Chrd=regexm(" " + upc_descr  + " ", " (C-B-C) ") 
gen Chenin_blanc= regexm(" " + upc_descr  + " ", " (CHN BL) ")| regexm(" " + upc_descr  + " ", " (VY-CH-B) ")| regexm(" " + upc_descr  + " ", " (P-CHN BL) ")| regexm(" " + upc_descr  + " ", " (P-CHN) ")| regexm(" " + upc_descr  + " ", " (CH-B) ")    


gen CheninB_SauB=regexm(" " + upc_descr  + " ", " (SV-B) ") & regexm(" " + upc_descr  + " ", " (CHN) ") & regexm(" " + upc_descr  + " ", " (BL) ")
replace Chenin_blanc=0 if CheninB_SauB==1
replace Sauvignon_Blanc=0 if CheninB_SauB==1

replace Chenin_blanc=1 if upc_descr=="NEDERBURG SA LYR-SBCC WT IDT"

gen Grenache_PGR= regexm(" " + upc_descr  + " ", " (GR-P-GR) ")

gen Chard_Semilon= regexm(" " + upc_descr  + " ", " (CHRD SML) ")| regexm(" " + upc_descr  + " ", " (SML CHRD) ")
replace Chard=0 if Chard_Semilon==1
replace Semilon=0 if Chard_Semilon==1

gen Cab_Primitvo= regexm(" " + upc_descr  + " ", " (CB-S PRMTV) ")
replace Cab_Sau=0 if Cab_Primitvo==1
replace Primitivo=0 if Cab_Primitvo==1

gen Cab_Sang=regexm(" " + upc_descr  + " ", " (CB-S SANG) ")
replace Cab_Sau=0 if Cab_Sang==1
replace Sangiovese=0 if Cab_Sang==1

gen CabS_Merlot_CabF= regexm(" " + upc_descr  + " ", " (MCFCS) ")| regexm(" " + upc_descr  + " ", " (BS-MCSCF) ")| regexm(" " + upc_descr  + " ", " (RMCSCFM) ")| regexm(" " + upc_descr  + " ", " (RMCFCS) ")| regexm(" " + upc_descr  + " ", " (MCSCF) ")| regexm(" " + upc_descr  + " ", " (MCBFC) ")| regexm(" " + upc_descr  + " ", " (ST-MCFCS) ")| regexm(" " + upc_descr  + " ", " (CSMCF) ")| regexm(" " + upc_descr  + " ", " (CBG-CSM) ")| regexm(" " + upc_descr  + " ", " (CFCSM) ")   //cabernet merlot cabernet franc

gen Mouv_C_GN= regexm(" " + upc_descr  + " ", " (M-CB-GN) ")
gen Viognier= regexm(" " + upc_descr  + " ", " (VGNR) ") 

gen Chard_Viognier= regexm(" " + upc_descr  + " ", " (CHRD VGNR) ")| regexm(" " + upc_descr  + " ", " (CHRD&VGNR) ")| regexm(" " + upc_descr  + " ", " (CHRD&VGNR) ")| regexm(" " + upc_descr  + " ", " (MNR-CHVGR) ")  
replace Viognier=0 if Chard_Viognier==1
replace Chard=0 if Chard_Viognier==1

gen Shz_Viognier=regexm(" " + upc_descr  + " ", " (SHZ VGNR) ")
replace SHZ=0 if Shz_Viognier==1
replace Viognier=0 if Shz_Viognier==1

gen Gre_Syr_Carig_Cinsault= regexm(" " + upc_descr  + " ", " (CSDV-GSCC) ")| regexm(" " + upc_descr  + " ", " (RGSCC) ")| regexm(" " + upc_descr  + " ", " (RGNSCC) ")   //csdv is app
gen Gre_Syr_Carigan= regexm(" " + upc_descr  + " ", " (SGC) ")| regexm(" " + upc_descr  + " ", " (GCS) ")
gen Syr_Carigan= regexm(" " + upc_descr  + " ", " (SY-CGN) ")
gen Gre_Pn= regexm(" " + upc_descr  + " ", " (PN&GY) ")
gen Gre_Cab_Syr_Mouv= regexm(" " + upc_descr  + " ", " (CSGSMV) ")
gen Gre_Syr_Mouv= regexm(" " + upc_descr  + " ", " (GSM) ")
replace Gre_Syr_Mouv=0 if brand_descr=="GOSSAMER BAY" & wine_type=="WHITE"

gen Jacquere= regexm(" " + upc_descr  + " ", " (WT-JQ) ")
gen Gre_Carig= regexm(" " + upc_descr  + " ", " (GRN CRIG) ")
gen Semilon_Sauv= regexm(" " + upc_descr  + " ", " (SML SAV) ")| regexm(" " + upc_descr  + " ", " (SAV SML) ") 
replace Semilon=0 if Semilon_Sauv==1
gen TSMR= regexm(" " + upc_descr  + " ", " (TSMR) ")      //tannat syrah merlot rose

gen Carignan=regexm(" " + upc_descr  + " ", " (CARIGNAN) ")
gen Syr_rose= regexm(" " + upc_descr  + " ", " (SYR) ") & regexm(" " + upc_descr  + " ", " (RSE) ")
replace SHZ=0 if Syr_rose==1
gen Sauvignon= regexm(" " + upc_descr  + " ", " (SAV) ")| regexm(" " + upc_descr  + " ", " (ED-SUV) ") if Importing_country=="France"

gen Chard_Sauv= regexm(" " + upc_descr  + " ", " (CHRD SAV) ")| regexm(" " + upc_descr  + " ", " (CHRDSBV) ")
replace Chard=0 if Chard_Sauv==1
replace Sauvignon=0 if Chard_Sauv==1

gen Cab_France= regexm(" " + upc_descr  + " ", " (CB FC) ")   

gen Shz_Mer_CabS=regexm(" " + upc_descr  + " ", " (SHZ MRLT CB-S) ")| regexm(" " + upc_descr  + " ", " (CB SHZ MRLT) ")| regexm(" " + upc_descr  + " ", " (SHZ CB-M) ")
replace SHZ=0 if Shz_Mer_CabS==1
replace Cab_Sau=0 if Shz_Mer_CabS==1
replace Merlot=0 if Shz_Mer_CabS==1

gen Domina=  regexm(" " + upc_descr  + " ", " (DOMINA) ") 

gen Syr_Temp= regexm(" " + upc_descr  + " ", " (SYR TMPN) ")| regexm(" " + upc_descr  + " ", " (TMPN SYR) ")| regexm(" " + upc_descr  + " ", " (TMPN SHZ) ")
replace SHZ=0 if Syr_Temp==1
replace TMPN=0 if Syr_Temp==1

gen Temp_Cbs= regexm(" " + upc_descr  + " ", " (TMPN CB-S) ")| regexm(" " + upc_descr  + " ", " (CB-S TMPN) ")| regexm(" " + upc_descr  + " ", " (TMPN CB) ")|regexm(" " + upc_descr  + " ", " (TMPN CB-S) ")| regexm(" " + upc_descr  + " ", " (CB-S TMPN) ")
replace Cab_Sau=0 if Temp_Cbs==1
replace Tempranillo=0 if Temp_Cbs==1

gen Temp_Grn= regexm(" " + upc_descr  + " ", " (GRNCHA TMPN) ")| regexm(" " + upc_descr  + " ", " (TP-GRC) ")| regexm(" " + upc_descr  + " ", " (TP-GRC) ")| regexm(" " + upc_descr  + " ", " (TMPN GRNCHA) ")
replace Grenache=0 if Temp_Grn==1
replace Tempranillo=0 if Temp_Grn==1


gen CabS_Gn_Sb=regexm(" " + upc_descr  + " ", " (GRN CB SAV) ")
replace Sauvignon=0 if CabS_Gn_Sb==1
replace Cab_Sau=0 if CabS_Gn_Sb==1
replace Grenache=0 if CabS_Gn_Sb==1


gen Merlot_Temp= regexm(" " + upc_descr  + " ", " (MRLT TMPN) ")
replace Merlot=0 if Merlot_Temp==1
replace Tempranillo=0 if Merlot_Temp==1

gen Cab_Shz_Mstrl=regexm(" " + upc_descr  + " ", " (SHZ CB MNSTRL) ")
replace Cab_Sau=0 if Cab_Shz_Mstrl==1
replace SHZ=0 if Cab_Shz_Mstrl==1
replace Monastrell=0 if Cab_Shz_Mstrl==1
 
gen Graciano= regexm(" " + upc_descr  + " ", " (GRCN) ")
gen Tmpn_Graciano= regexm(" " + upc_descr  + " ", " (TMPN-GRCN) ")
replace Tempranillo=0 if Tmpn_Graciano==1
replace Graciano=0 if Tmpn_Graciano==1

gen Crnz_Tmpn_CBS=regexm(" " + upc_descr  + " ", " (CRNZ TMPN CB-S) ")
replace Tempranillo=0 if Crnz_Tmpn_CBS==1
replace Cab_Sau=0 if Crnz_Tmpn_CBS==1


*check and fix the blended varietals-any upc with more thn one varietal*
gen var_mix= Cab_Sau+ Chard+ Pinot_G+ Riesling+ Malbec+ Zinfandel+ Merlot+ Moscato+ Pinot_N+ Bonarda+ Sauvignon_Blanc+ Macabeo+ Tempranillo+ Pinotage+ SHZ+ Syrah+ Primitivo+ Soave+ Mencia+ Xarel+ Torrontes+ Monastrell+ Liebfraumilch+ Sangiovese+ Bukettraube+ Godello+ Nebbiolo+ Vermentio+ Rosato+ Rosso+ Verdejo+ Pinot_Nero+ Albarinho+ Inzolia+ Dornfelder+ Negro_Amaro+ Lambrusco+ Tarrango+ Malavasia+ Teroldego+Barbera+ Trebbiano+ NeroDAvola+ Merlot_NeroDAvola+ Falanghina+ Grillo+ Gamay+ Gruner_Veltliner+ Piesporter_Michelsberg+ Grenache+ PinotG_Chard+ Bonarda_Merlot+ Bonarda_Malbec+ Bonarda_Syrah+ SauBlanc_Semilon+ Semilon+ Merlot_Pinotage+ SHZ_Pinotage+ Monas_Tempr+ Pinot_Binaco+ Syr_Gr_Mourvedre+ Sang_Cab_Merlot+ Mns_Syrah+ Ugni_blanc+ Sang_Merlot+ Cab_Merlo+ Cab_SHZ+ Cab_Syr+ Cab_Syr_Mouv+ Cab_TMPN+ Cab_Malbec+ SHZ_GRE+ SHZ_Merlot+Syrah_Mourvedre+ SHZ_Malbec+ SHZ_PinotN+ SHZ_TMPN+ Zin_SHZ+ Gren_TMPN+ Merlot_Gre+ Merlot_PN+ Merlot_MLBC+ TMPN_MLBC+ Zin_Gre+ Zin_Chardonnay+ Zin_Moscato+ SB_CHARD+ Champgane_other+ Chard_Chm+ MSC_CHM+ PinotG_CHM+ Zin_CHM+ Carmenere+ Concord+ Meritage+ Meri_Chard+ Gewur+ P_Sirah+ Red_Blend+ White_Blend+ CAVA+ Chianti_other+ Port+ Psrh_Port+ Sherry+ Burgandy_other+ Chablis_other+ Rhine+ Muscadine+ Marsala+ Chard_PN+ Pinot_Blanc+ Norton+ CheninB_Chrd+ Chenin_blanc+ CheninB_SauB+ Grenache_PGR+ Chard_Semilon+ Cab_Primitvo+Cab_Sang+CabS_Merlot_CabF+ Viognier+ Chard_Viognier+ Shz_Viognier+ Gre_Syr_Carig_Cinsault+Gre_Syr_Carigan+ Syr_Carigan+ Gre_Pn+Gre_Cab_Syr_Mouv+ Gre_Syr_Mouv+ Jacquere+ Gre_Carig+ Semilon_Sauv+ TSMR+ Carignan+ Syr_rose+ Sauvignon+ Chard_Sauv+ Cab_France+Shz_Mer_CabS+ Domina+ Syr_Temp+ Temp_Cbs+ Temp_Grn+ CabS_Gn_Sb+ Merlot_Temp+ Cab_Shz_Mstrl+ Graciano+ Tmpn_Graciano+ Crnz_Tmpn_CBS

*red blends with combination of other varietal- keep them as other var
replace Red_Blend =0 if var_mix ==2 & Red_Blend ==1
replace White_Blend=0 if var_mix ==2 & White_Blend==1
replace Red_Blend =0 if var_mix ==3 & Red_Blend ==1
replace White_Blend=0 if var_mix ==3 & White_Blend==1
drop var_mix

gen Varietal=""
replace Varietal="Cabernet Sauvignon" if Cab_Sau==1
replace Varietal="Chardonnay" if Chard==1
replace Varietal="Pinot Grigio" if Pinot_G==1
replace Varietal="Riesling" if Riesling==1
replace Varietal="Malbec" if Malbec==1
replace Varietal="Zinfandel" if Zinfandel==1 
replace Varietal="Merlot" if Merlot==1
replace Varietal="Moscato" if Moscato==1
replace Varietal="Pinot Noir" if Pinot_N==1
replace Varietal="Bonarda" if Bonarda==1
replace Varietal="Sauvignon Blanc" if Sauvignon_Blanc==1
replace Varietal="Macabeo" if Macabeo==1
replace Varietal="Tempranillo" if Tempranillo==1
replace Varietal="Pinotage" if Pinotage==1
replace Varietal="Shiraz" if SHZ==1
replace Varietal="Syrah" if Syrah==1
replace Varietal="Primitivo" if Primitivo==1
replace Varietal="Soave" if Soave==1
replace Varietal="Mencia" if Mencia==1
replace Varietal="Xarel" if Xarel==1
replace Varietal="Torrontes" if Torrontes==1
replace Varietal="Monastrell" if Monastrell==1
replace Varietal="Liebfraumilch" if  Liebfraumilch==1
replace Varietal="Sangiovese" if  Sangiovese==1
replace Varietal="Bukettraube" if Bukettraube==1
replace Varietal="Syr_Gr_Mourvedre" if Syr_Gr_Mourvedre
replace Varietal="Godello" if Godello==1
replace Varietal="Nebbiolo" if  Nebbiolo==1
replace Varietal="Vermentio" if Vermentio==1
replace Varietal="Rosato" if Rosato==1 
replace Varietal="Rosso" if Rosso==1
replace Varietal="Verdejo" if Verdejo==1 
replace Varietal="Pinot_Nero" if Pinot_Nero==1 
replace Varietal="Albarinho" if Albarinho==1
replace Varietal="Inzolia" if Inzolia==1
replace Varietal="Dornfelder" if Dornfelder==1
replace Varietal="Negro_Amaro" if Negro_Amaro==1
replace Varietal="Lambrusco" if Lambrusco==1
replace Varietal="Tarrango" if Tarrango==1
replace Varietal="Malavasia" if Malavasia==1
replace Varietal="Teroldego" if Teroldego==1
replace Varietal="Barbera" if Barbera==1
replace Varietal="Trebbiano" if Trebbiano==1
replace Varietal="NeroDAvola" if NeroDAvola==1
replace Varietal="Merlot_NeroDAvola" if Merlot_NeroDAvola==1
replace Varietal="Falanghina" if Falanghina==1
replace Varietal="Ugni_blanc" if Ugni_blanc==1 
replace Varietal="Grillo" if Grillo==1
replace Varietal="Gamay" if Gamay==1 
replace Varietal="Gruner_Veltliner" if Gruner_Veltliner==1 
replace Varietal="Piesporter_Michelsberg" if Piesporter_Michelsberg==1
replace Varietal="Grenache" if Grenache==1
replace Varietal="PinotG_Chard" if PinotG_Chard==1
replace Varietal="Bonarda_Merlot" if Bonarda_Merlot==1
replace Varietal="Bonarda_Malbec" if Bonarda_Malbec==1
replace Varietal="Bonarda_Syrah" if Bonarda_Syrah==1
replace Varietal="SauBlanc_Semilon" if SauBlanc_Semilon==1
replace Varietal="Semilon" if Semilon==1
replace Varietal="Merlot_Pinotage" if  Merlot_Pinotage==1
replace Varietal="SHZ_Pinotage" if SHZ_Pinotage==1
replace Varietal="Monas_Tempr" if  Monas_Tempr==1
replace Varietal="Pinot_Binaco" if Pinot_Binaco==1
replace Varietal="Syr_Gr_Mourvedre" if Syr_Gr_Mourvedre==1
replace Varietal="Sang_Cab_Merlot" if Sang_Cab_Merlot==1 
replace Varietal="Mns_Syrah" if Mns_Syrah==1
replace Varietal="Ugni_blanc" if Ugni_blanc==1 
replace Varietal="Sang_Merlot" if Sang_Merlot==1
replace Varietal="Cabernet_Merlot" if Cab_Merlo==1
replace Varietal="Sang_Merlot" if Sang_Merlot==1
replace Varietal="Cabernet_Shiraz" if Cab_SHZ==1| Cab_Syr==1
replace Varietal="Cab_Syr_Mouv" if Cab_Syr_Mouv==1
replace Varietal="Cab_TMPN" if Cab_TMPN==1
replace Varietal="Cab_Malbec" if Cab_Malbec==1
replace Varietal="Shiraz_Greenache" if SHZ_GRE==1
replace Varietal="Shiraz_Merlot" if SHZ_Merlot==1
replace Varietal="Syrah_Mourvedre" if Syrah_Mourvedre==1
replace Varietal="Shiraz_Malbec" if SHZ_Malbec==1
replace Varietal="Shiraz_PinotN" if SHZ_PinotN==1
replace Varietal="Shiraz_Tmpn" if SHZ_TMPN==1
replace Varietal="Zinfandel_Shiraz" if Zin_SHZ==1
replace Varietal="Gren_TMPN" if Gren_TMPN==1
replace Varietal="Merlot_Grenache" if Merlot_Gre==1
replace Varietal="Merlot_PinotN" if Merlot_PN==1
replace Varietal="Merlot_Malbec" if Merlot_MLBC==1
replace Varietal="TMPN_MLBC" if TMPN_MLBC==1
replace Varietal="Zinfandel_Grenache" if Zin_Gre==1
replace Varietal="Zin_Chardonnay" if Zin_Chardonnay==1 
replace Varietal="Zin_Moscato" if Zin_Moscato==1
replace Varietal="SB_Chard" if SB_CHARD==1
replace Varietal="Champgane_other" if Champgane_other==1 
replace Varietal="Chard_Champgane" if Chard_Chm==1 
replace Varietal="MSC_CHM" if MSC_CHM==1
replace Varietal="PG_Champgane" if PinotG_CHM==1
replace Varietal="Zin_Champgane" if Zin_CHM==1
replace Varietal="Carmenere" if Carmenere==1
replace Varietal="Concord" if Concord==1
replace Varietal="Meriatge" if Meritage==1
replace Varietal="Meritge_Chard" if Meri_Chard==1
replace Varietal="Gewur" if Gewur==1
replace Varietal="P_Sirah" if P_Sirah==1 
replace Varietal="Red Blend" if Red_Blend==1
replace Varietal="White Blend" if White_Blend==1
replace Varietal="CAVA" if CAVA==1
replace Varietal="Chianti_other" if Chianti_other==1
replace Varietal="Port" if Port==1
replace Varietal="Psyr_Port" if Psrh_Port==1
replace Varietal="Sherry" if Sherry==1
replace Varietal="Burgandy_other" if Burgandy_other==1
replace Varietal="Chablis_other" if Chablis_other==1
replace Varietal="Rhine" if Rhine==1
replace Varietal="Muscadine" if Muscadine==1
replace Varietal="Marsala" if Marsala==1
replace Varietal="Chard_PN" if Chard_PN==1
replace Varietal="Pinot_Blanc" if Pinot_Blanc==1
replace Varietal="Norton" if Norton==1
replace Varietal="CheninB_SauB" if CheninB_SauB==1
replace Varietal="Chenin_blanc" if Chenin_blanc==1
replace Varietal="CheninB_Chrd" if CheninB_Chrd==1
replace Varietal="Grenache_PGR" if Grenache_PGR==1
replace Varietal="Chard_Semilon" if Chard_Semilon==1
replace Varietal="Cab_Primitvo" if Cab_Primitvo==1
replace Varietal="Cab_Sang" if Cab_Sang==1
replace Varietal="CabS_Merlot_CabF" if CabS_Merlot_CabF==1
replace Varietal="Viognier" if Viognier==1
replace Varietal="Chard_Viognier" if Chard_Viognier==1
replace Varietal="Shz_Viognier" if Shz_Viognier==1
replace Varietal="Gre_Syr_Carig_Cinsault" if Gre_Syr_Carig_Cinsault==1
replace Varietal="Gre_Syr_Carigan" if Gre_Syr_Carigan==1
replace Varietal="Syr_Carigan" if Syr_Carigan==1
replace Varietal="Gre_Pn" if Gre_Pn==1
replace Varietal="Gre_Cab_Syr_Mouv" if Gre_Cab_Syr_Mouv==1
replace Varietal="Gre_Syr_Mouv" if Gre_Syr_Mouv==1 
replace Varietal="Jacquere" if Jacquere==1
replace Varietal="Gre_Carig" if Gre_Carig==1 
replace Varietal="Semilon_Sauv" if Semilon_Sauv==1 
replace Varietal="TSMR" if TSMR==1
replace Varietal="Carignan" if Carignan==1
replace Varietal="Syr_rose" if Syr_rose==1
replace Varietal="Sauvignon" if Sauvignon==1 
replace Varietal="Chard_Sauv" if Chard_Sauv==1
replace Varietal="Cab_Franc" if Cab_France==1 
replace Varietal="Shz_Mer_CabS" if Shz_Mer_CabS==1
replace Varietal="Domina" if Domina==1
replace Varietal="Syr_Temp" if Syr_Temp==1 
replace Varietal="Temp_Cbs" if Temp_Cbs==1 
replace Varietal="Temp_Grn" if Temp_Grn==1 
replace Varietal="CabS_Gn_Sb" if CabS_Gn_Sb==1
replace Varietal="Merlot_Temp" if Merlot_Temp==1 
replace Varietal="Cab_Shz_Mstrl" if Cab_Shz_Mstrl==1 
replace Varietal="Graciano" if Graciano==1
replace Varietal="Tmpn_Graciano" if Tmpn_Graciano==1
replace Varietal="Crnz_Tmpn_CBS" if Crnz_Tmpn_CBS==1


*capturing blush and rose
replace Varietal="Blush" if match_blush==1
replace Varietal="Rose" if match_rose==1
drop match_blush match_rose

*to make varietal categorization simpler- We are capturing varietal only for table wines- rest is just the Nielsen Module name
replace Varietal ="Flavoured" if product_module_descr =="WINE-FLAVORED/REFRESHMENT"
replace Varietal ="Sangria" if product_module_descr =="WINE-SANGRIA"
replace Varietal ="Sparkling" if product_module_descr=="WINE-SPARKLING"
replace Varietal ="Vermouth" if product_module_descr=="WINE-VERMOUTH"
replace Varietal ="Aperitifs" if product_module_descr=="WINE-APERITIFS"
replace Varietal ="Dessert" if product_module_descr=="WINE-SWEET DESSERT-IMPORTED"| product_module_descr=="WINE-SWEET DESSERT-DOMESTIC"
replace Varietal ="Sangria" if upc_desc=="MADRIA SANGRIA DM MSC SG"
replace Varietal="Flavoured" if upc_descr=="CTL BR MSC JUICY PCH GL FR"| upc_descr=="A-BLNA MSC LEMONATA GL FR"


*classifying Kosher in red and white
replace wine_type="RED" if Varietal=="Cabernet Sauvignon" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Cabernet_Shiraz" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="WHITE" if Varietal=="Chablis_other" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="WHITE" if Varietal=="Chardonnay" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Concord" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Malbec" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Merlot" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Cabernet_Merlot" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="WHITE" if Varietal=="Moscato" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="P_Sirah" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Pinot Noir" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="WHITE" if Varietal=="Pinot Grigio" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="WHITE" if Varietal=="Riesling" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="WHITE" if Varietal=="SB_Chard" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="WHITE" if Varietal=="Sauvignon Blanc" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Shiraz" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Shiraz_Malbec" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Monastrell" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Nebbiolo" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Primitivo" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="WHITE" if Varietal=="Malavasia" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Zinfandel" & product_module_descr=="WINE-KOSHER TABLE"
replace wine_type="RED" if Varietal=="Marsala" & product_module_descr=="WINE-KOSHER TABLE"


****Fixing wine type based on varietals captured
replace wine_type="RED" if Varietal=="Barbera"   
replace wine_type ="RED" if Varietal =="Bonarda" & wine_type =="OTHER"
replace Varietal="Chardonnay" if upc_desc=="GRNT BRG BRS-VN AS CHRD WT IDT"     //was capturing burgandy_other
replace wine_type="RED" if Varietal =="Burgandy_other"            
replace wine_type ="RED" if Varietal =="Cabernet Sauvignon"      
replace wine_type ="RED" if Varietal =="Carmenere" & wine_type =="OTHER"       
replace wine_type ="RED" if Varietal =="Carmenere" & wine_type =="WHITE"       
replace wine_type ="WHITE" if Varietal =="Chablis_other" & wine_type =="OTHER"    
replace wine_type ="WHITE" if Varietal =="Chablis_other" & wine_type =="RED"    
replace Varietal="Grenache Blanc" if Varietal=="Grenache" & wine_type=="WHITE"
replace Varietal="Chardonnay" if  upc_descr=="GFV MLVSA CHRD V WT DDT" 
replace wine_type="RED" if Varietal=="Malavasia"   
replace Varietal ="Malbec_White" if Varietal=="Malbec" & wine_type =="WHITE"  
replace Varietal="Meriatge_White" if Varietal=="Meriatge" & wine_type=="WHITE"
replace Varietal="Cabernet_Merlot" if upc_descr=="TERRILOGIO IT S-CB-S MRLT IDT"
replace wine_type="RED" if upc_descr=="TERRILOGIO IT S-CB-S MRLT IDT"       
replace wine_type="RED" if upc_descr=="DE GRAS CHL MRLT IDT"   
replace Varietal="Moscato_Red" if Varietal=="Moscato" & wine_type =="RED"   
replace Varietal="Moscato_White" if Varietal=="Moscato" & wine_type =="WHITE"
replace Varietal="Muscadine_White" if Varietal=="Muscadine" & wine_type =="WHITE"
replace Varietal="Muscadine_Red" if Varietal=="Muscadine" & wine_type =="RED"
replace wine_type="WHITE" if Varietal=="Pinot Grigio"   
replace wine_type="WHITE" if Varietal=="Sauvignon Blanc"  
replace wine_type="RED" if Varietal=="Shiraz"    
replace wine_type="WHITE" if Varietal=="Vermentio"    
replace Varietal="" if Varietal=="White Blend" & wine_type=="RED"    //capturing wb from brand name- actually red wine
replace Varietal="Zinfandel_White" if Varietal =="Zinfandel" & wine_type =="WHITE" 
replace Varietal ="Shiraz" if upc_descr=="GRNT BRG BRS-VN AS SHZ RED IDT"
replace wine_type="WHITE" if Varietal=="Viognier" & wine_type=="RED"  


replace Varietal="other_red" if wine_type=="RED" & Varietal=="" & appellation_label!="Imported"
replace Varietal="other_white" if wine_type=="WHITE" & Varietal=="" & appellation_label!="Imported" 

replace Varietal="other_red_imported" if Varietal=="" & wine_type=="RED" & appellation_label=="Imported"
replace Varietal="other_white_imported" if Varietal=="" & wine_type=="WHITE" & appellation_label=="Imported"
replace Varietal="otherwinetype_imported" if Varietal=="" & wine_type=="OTHER" & appellation_label=="Imported"
replace Varietal="otherwinetype" if Varietal=="" & wine_type=="OTHER"

replace Varietal="Syrah" if Varietal=="Shiraz"
replace wine_type="RED" if Varietal=="Syrah"     


drop Cab_Sau Cab_Sau Chard Pinot_G Riesling Malbec Zinfandel Merlot Moscato Pinot_N Bonarda Sauvignon_Blanc Macabeo Tempranillo Pinotage SHZ Syrah Primitivo Soave Mencia Xarel Torrontes Monastrell Liebfraumilch Sangiovese Bukettraube Godello Nebbiolo Vermentio Rosato Rosso Verdejo Pinot_Nero Albarinho Inzolia Dornfelder Negro_Amaro Lambrusco Tarrango Malavasia Teroldego Barbera Trebbiano NeroDAvola Merlot_NeroDAvola Falanghina Grillo Gamay Gruner_Veltliner Piesporter_Michelsberg Grenache PinotG_Chard Bonarda_Merlot Bonarda_Malbec Bonarda_Syrah SauBlanc_Semilon Semilon Merlot_Pinotage SHZ_Pinotage Monas_Tempr Pinot_Binaco Syr_Gr_Mourvedre Sang_Cab_Merlot Mns_Syrah Ugni_blanc Sang_Merlot Cab_Merlo Cab_SHZ Cab_Syr Cab_Syr_Mouv Cab_TMPN Cab_Malbec SHZ_GRE SHZ_Merlot Syrah_Mourvedre SHZ_Malbec SHZ_PinotN SHZ_TMPN Zin_SHZ Gren_TMPN Merlot_Gre Merlot_PN Merlot_MLBC TMPN_MLBC Zin_Gre Zin_Chardonnay Zin_Moscato SB_CHARD Champgane_other Chard_Chm MSC_CHM PinotG_CHM Zin_CHM Carmenere Concord Meritage Meri_Chard Gewur P_Sirah Red_Blend White_Blend CAVA Chianti_other Port Psrh_Port Sherry Burgandy_other Chablis_other Rhine Muscadine Marsala Chard_PN Pinot_Blanc Norton CheninB_Chrd Chenin_blanc CheninB_SauB Grenache_PGR Chard_Semilon Cab_Primitvo Cab_Sang CabS_Merlot_CabF Mouv_C_GN Viognier Chard_Viognier Shz_Viognier Gre_Syr_Carig_Cinsault Gre_Syr_Carigan Syr_Carigan Gre_Pn Gre_Cab_Syr_Mouv Gre_Syr_Mouv Jacquere Gre_Carig Semilon_Sauv TSMR Carignan Syr_rose Sauvignon Chard_Sauv Cab_France Shz_Mer_CabS Domina Syr_Temp Temp_Cbs Temp_Grn CabS_Gn_Sb Merlot_Temp Cab_Shz_Mstrl Graciano Tmpn_Graciano Crnz_Tmpn_CBS 

replace Varietal="Chard_Sauv" if Varietal=="Sb_Chard"


****************************Brand name as uniform*******************************

replace brand_descr="VENTANA VINEYARDS" if brand_descr=="VENTANA"
replace brand_descr="WHITEHALL LANE WINERY & VINEYA" if brand_descr=="WHITEHALL LANE WINERY"
replace brand_descr="ROSEMOUNT ESTATE" if brand_descr=="ROSEMOUNT OLD BENSON"
replace brand_descr="WILLIAM HILL ESTATE" if brand_descr=="WILLIAM HILL"
replace brand_descr="ACACIA" if brand_descr=="A BY ACACIA"
replace brand_descr="MOUNTAIN VIEW" if brand_descr=="MOUNTAIN VIEW VINTNERS"
replace brand_descr="GREG NORMAN ESTATE" if brand_descr=="GREG NORMAN"
replace brand_descr="PETRAIO" if brand_descr=="FATTORIA DI PETROIO"
replace brand_descr="RENE BARBIER MEDITERRANEAN" if brand_descr=="RENE BARBIER MEDITERRANEAN BRT"| brand_descr=="RENE BARBIER MEDITERRANEAN WHT"| brand_descr=="RENE BARBIER MEDITERRANEAN RED"| brand_descr=="RENE BARBIER"| brand_descr=="RENE BARBIER MDTRRNE PETILLANT"
replace brand_descr="MERRYVALE STARMONT" if brand_descr=="STARMONT"| brand_descr=="MERRYVALE"
replace brand_descr="SNOQUALMIE VINEYARDS" if brand_descr=="SNOQUALMIE"
replace brand_descr="COLUMBIA WINERY" if brand_descr=="COLUMBIA"
replace brand_descr="APEX ASCENT" if brand_descr=="APEX II"
replace brand_descr="BEAULIEU VINEYARD BV COASTAL" if brand_descr=="BEAULIEU VINEYARD BV CSTL ESTS"
replace brand_descr="GLEN ELLEN" if brand_descr=="GLEN ELLEN RESERVE"
replace brand_descr="LAURIER VINEYARDS" if brand_descr=="LAURIER"
replace brand_descr="BARRIO LA BOCA" if brand_descr=="LA BOCA"
replace brand_descr="NAPA RIDGE" if brand_descr=="NAPA VALLEY NAPA RIDGE"
replace brand_descr="RUTHERFORD VINTNERS" if brand_descr=="RUTHERFORD"
replace brand_descr="SEA RIDGE" if brand_descr=="SEA RIDGE COASTAL" 
replace brand_descr="BUENA VISTA CARNEROS" if brand_descr=="BUENA VISTA"   //checked- all of them have Carneros in upc descrption
replace brand_descr="BAROSSA VALLEY" if brand_descr=="BAROSSA VALLEY ESTATE E MINOR" 
replace brand_descr="NOBILO" if brand_descr=="HOUSE OF NOBILO"
replace brand_descr="VINA SANTA CAROLINA" if brand_descr=="SANTA CAROLINA"| brand_descr=="VINA SANTA CAROLINA VISTANA"| brand_descr=="SANTA CAROLINA VISTANA"
replace brand_descr="GAETANO D'AQUINO" if brand_descr=="D'AQUINO"
replace brand_descr="EASLEY WINERY" if brand_descr=="EASLEY"
replace brand_descr="P.J. VALCKENBERG" if brand_descr=="VALCKENBERG"
replace brand_descr="CONCHA Y TORO" if brand_descr=="CONCHA Y TORO XPLORADOR"
replace brand_descr="MOUNT VEEDER WINERY" if brand_descr=="MOUNT VEEDER"
replace brand_descr="MAISON NICOLAS" if brand_descr=="NICOLAS"
replace brand_descr="BARON PHILIPPE" if brand_descr=="BARON PHILIPPE DE ROTHSCHILD"
replace brand_descr="LE GRAND" if brand_descr=="LE GRAND NOIR"
replace brand_descr="SITTMANN" if brand_descr=="CARL SITTMANN"
replace brand_descr="LEELANAU CELLARS" if brand_descr=="LEELANAU"| brand_descr=="LEELANAU CELLARS WITCHES BREW"
replace brand_descr="BOONE'S FARM" if brand_descr=="BOONE'S"
replace brand_descr="REDWOOD CREEK" if brand_descr=="FREI BROS. REDWOOD CREEK"
replace brand_descr="APOTHIC RED" if brand_descr=="APOTHIC"
replace brand_descr="BAREFOOT" if brand_descr=="BAREFOOT REFRESH"
replace brand_descr="FORTANT DE FRANCE" if brand_descr=="FORTANT"
replace brand_descr="THE DIVINING ROD" if brand_descr=="MARC MONDAVI'S THE DIVINING RD"
replace brand_descr="EL PORTILLO" if brand_descr=="FINCA EL PORTILLO"
replace brand_descr="HAHN" if brand_descr=="HAHN ESTATES"
replace brand_descr="BOLLINGER" if brand_descr=="BOLLINGER SPECIAL"
replace brand_descr="ROBERT PEPI" if brand_descr=="PEPI"
replace brand_descr="LINCOURT" if brand_descr=="LIN COURT"
replace brand_descr="CHATEAU MONTFOR" if brand_descr=="CHATEAU DE MONTFORT"
replace brand_descr="MADDALENA VINEYARD" if brand_descr=="MADDALENA"
replace brand_descr="1917 IL CONTE" if brand_descr=="1917 IL CONTE D' ALBA"
replace brand_descr="LAPOSTOLLE CUVEE ALEXNDRE" if brand_descr=="CASA LAPOSTOLLE CUVEE ALEXNDRE"
replace brand_descr="SEBASTIANI SONOMA CASK" if brand_descr=="SEBASTIANI" 
replace brand_descr="LES CAVES DU CHATEAU D'ESCLANS" if brand_descr=="CAVES D'ESCLANS"
replace brand_descr="CELLA" if brand_descr=="FRATELLI CELLA"
replace brand_descr="STRONGBOW" if brand_descr=="STRONGBOW GOLD APPLE"
replace brand_descr="CHATEAU SOUVERAIN" if brand_descr=="SOUVERAIN"
replace brand_descr="MICHELLE" if brand_descr=="DOMAINE STE. MICHELLE"
replace brand_descr="DONA PAULA LOS CARDOS" if brand_descr=="DONA PAULA"
replace brand_descr="GABBIANO" if brand_descr=="CASTELLO DI GABBIANO"
replace brand_descr="BERINGER" if brand_descr=="BERINGER CALIFORNIA CLCTN"| brand_descr=="STONE CELLARS BY BERINGER"
replace brand_descr="TORTOISE CREEK" if brand_descr=="TORTOISE CREEK WINES"
replace brand_descr="YELLOWGLEN" if brand_descr=="YELLOW BY YELLOWGLEN"| brand_descr=="PINK BY YELLOWGLEN"| brand_descr=="PINK"
replace brand_descr="GREG NORMAN" if brand_descr=="GREG NORMAN ESTATES" 
replace brand_descr="TRUCK" if brand_descr=="WHITE TRUCK" | brand_descr=="RED TRUCK"
replace brand_descr="GOTT" if brand_descr=="GOTT 8" 
replace brand_descr="MIDNIGHT CELLARS" if brand_descr=="MIDNIGHT"
replace brand_descr="BRANCOTT" if brand_descr=="BRANCOTT ESTATE"
replace brand_descr="MONTES" if brand_descr=="MONTES ALPHA"
replace brand_descr="CASTELVERO" if brand_descr=="ANTICA CONTEA DI CASTELVERO"
replace brand_descr="TERREDORA" if brand_descr=="TERREDORA DIPAOLO"
replace brand_descr="BISOL" if brand_descr=="BISOL JEIO"
replace brand_descr="HESS" if brand_descr=="HESS SELECT"| brand_descr=="HESS COLLECTION"| brand_descr=="HESS ESTATE"
replace brand_descr="MORRO BAY VINEYARDS" if brand_descr=="MORRO BAY"
replace brand_descr="KUNDE" if brand_descr=="KUNDE ESTATE"
replace brand_descr="COOPER" if brand_descr=="COOPER MOUNTAIN"| brand_descr=="COOPER HILL"
replace brand_descr="MIONETTO IL" if brand_descr=="IL"
replace brand_descr="LAKE SONOMA WINERY" if brand_descr=="LAKE SONOMA"
replace brand_descr="VIANO VINEYARDS" if brand_descr=="VIANO"
replace brand_descr="FRANCIS COPPOLA" if brand_descr=="FRANCIS COPPOLA DMND CLTN"| brand_descr=="FRANCIS COPPOLA DIAMOND CLLCTN"| brand_descr=="FRANCIS COPPOLA PRESENTS"| brand_descr=="FRANCIS COPPOLA DIRECTOR'S CUT"
replace brand_descr="CRISTALINO" if brand_descr=="JAUME SERRA CRISTALINO"
replace brand_descr="PERRY CREEK VINEYARDS" if brand_descr=="PERRY CREEK"
replace brand_descr="OAK GROVE VINEYARDS" if brand_descr=="OAK GROVE"
replace brand_descr="ABUNDANCE VINEYARDS" if brand_descr=="ABUNDANCE"
replace brand_descr="ALVEAR'S" if brand_descr=="ALVEAR'S"
replace brand_descr="YANGARRA" if brand_descr=="YANGARRA PARK"
replace brand_descr="DOMAINE LAFOND" if brand_descr=="DOMAINE LAFOND ROC-EPINE"
replace brand_descr="DI MAJO NORANTE" if brand_descr=="DI MAJO NORANTE SAN GIORGIO"
replace brand_descr="SANTA CAROLINA VISTANA" if brand_descr=="VISTANA"  
replace brand_descr="SCREW KAPPA NAPA" if brand_descr=="S K N"
replace brand_descr="DR. H. THANISCH" if brand_descr=="WWE. DR. H. THANISCH"
replace brand_descr="FAMIGLIA CIELO" if brand_descr=="CIELO"  
replace brand_descr="FEUDO ARANCIO " if brand_descr=="FEUDO ARANCIO STEMMARI"
replace brand_descr="STACK WINES" if brand_descr=="STACKED"
replace brand_descr="JASPER WINERY J" if brand_descr=="JASPER WINERY J W"
replace brand_descr="CLIF FAMILY WINERY" if brand_descr=="CLIF"
replace brand_descr="BILA-HAUT" if brand_descr=="BILA-HAUT DOMAINE DE"
replace brand_descr="BANDIT" if brand_descr=="THREE THIEVES BANDIT"
replace brand_descr="90 + CELLARS" if brand_descr=="NINETY + CELLARS" 
replace brand_descr="ARTISAN WINERY" if brand_descr=="ARTISAN VINTNERS GUILD"
replace brand_descr="BERNARD MAGREZ" if brand_descr=="BERNARD MAGREZ TRANQUILLITE"
replace brand_descr="LA LINDA" if brand_descr=="FINCA LA LINDA"
replace brand_descr="1917 IL CONTE" if brand_descr=="1917 IL CONTE D' ALBA"
replace brand_descr="ADAM CAROLLA'S" if brand_descr=="ADAM CAROLLA'S MANGRIA"
replace brand_descr="ADEGA DE BORBA" if brand_descr=="ADEGA COOP. DE BORBA"
replace brand_descr="AIRLIE 7" if brand_descr=="AIRLIE 7"
replace brand_descr="ALEXANDRIA NICOLE" if brand_descr=="ALEXANDRIA NICOLE A SQUARED"
replace brand_descr="AIRLIE" if brand_descr=="AIRLIE 7"
replace brand_descr="ALEXANDRIA NICOLE" if brand_descr=="ALEXANDRIA NICOLE A SQUARED"
replace brand_descr="ALLEGRINI" if brand_descr=="ALLEGRINI + RENACER"| brand_descr=="ALLEGRINI PALAZZO DELLA TORRE"
replace brand_descr="ANDEAN" if brand_descr=="ANDEAN SKY"
replace brand_descr="ANGOVE'S" if brand_descr=="ANGOVE'S BEAR CROSSING"| brand_descr=="ANGOVE'S NINE VINES"| brand_descr=="ANGOVE'S RED BELLY BLACK"
replace brand_descr="ANDES PEAK" if brand_descr=="ANDES PEAKS"
replace brand_descr="BANFI" if brand_descr=="BANFI CENTINE"| brand_descr=="BANFI CHIANTI"| brand_descr=="BANFI COL DI SASSO"
replace brand_descr="BAREFOOT" if brand_descr=="BAREFOOT CELLARS"| brand_descr=="BAREFOOT REFRESH"| brand_descr=="BAREFOOT RESERVE"
replace brand_descr="BEAULIEU VINEYARD BV" if brand_descr=="BEAULIEU VINEYARD BV COASTAL"| brand_descr=="BEAULIEU VINEYARD BV CSTL ESTS"|brand_descr=="BEAULIEU VNYRD BV GRGS DE LTR"
replace brand_descr="BERINGER" if brand_descr=="BERINGER CALIFORNIA CLCTN"| brand_descr=="BERINGER FOUNDERS' ESTATE"| brand_descr=="BERINGER LUMINUS"| brand_descr=="BERINGER THIRD CENTURY"
replace brand_descr="BERNARD MAGREZ" if brand_descr=="BERNARD MAGREZ TRANQUILLITE"
replace brand_descr="BIG ASS" if brand_descr=="BIG ASS CAB"| brand_descr=="BIG ASS CHARD"| brand_descr=="BIG ASS ZIN"
replace brand_descr="BILTMORE" if brand_descr=="BILTMORE ESTATE"| brand_descr=="BILTMORE RESERVE"

replace brand_descr="BLOCK" if brand_descr=="BLOCK 012"| brand_descr=="BLOCK 052"| brand_descr=="BLOCK 054"| brand_descr=="BLOCK 115"|brand_descr=="BLOCK 13"| brand_descr=="BLOCK 213"| brand_descr=="BLOCK 214"| brand_descr=="BLOCK 303"| brand_descr=="BLOCK 426"| brand_descr=="BLOCK 430"| brand_descr=="BLOCK 478"| brand_descr=="BLOCK 50"| brand_descr=="BLOCK 501"| brand_descr=="BLOCK 503"| brand_descr=="BLOCK 506"| brand_descr=="BLOCK 511"| brand_descr=="BLOCK 512"| brand_descr=="BLOCK 515"| brand_descr=="BLOCK 516"| brand_descr=="BLOCK 523"| brand_descr=="BLOCK 533"| brand_descr=="BLOCK 575"| brand_descr=="BLOCK 577"| brand_descr=="BLOCK 613"| brand_descr=="BLOCK 713"| brand_descr=="BLOCK 823"| brand_descr=="BLOCK 904"| brand_descr=="BLOCK 917"| brand_descr=="BLOCK 945"| brand_descr=="BLOCK 949"

replace brand_descr="VIANO" if brand_descr=="VIANO VINEYARDS"
replace brand_descr="WEINSTOCK CELLARS" if brand_descr=="WEINSTOCK CELLAR SELECT"
replace brand_descr="WHITEHALL LANE WINERY" if brand_descr=="WHITEHALL LANE WINERY & VINEYA"
replace brand_descr="WOODBRIDGE RBRT MNDV" if brand_descr=="WOODBRIDGE BY ROBERT MONDAVI"| brand_descr=="WOODBRIDGE RBRT MNDV SLT VY SR"
replace brand_descr="YALUMBA" if brand_descr=="YALUMBA THE SIGNATURE"| brand_descr=="YALUMBA THE STRAPPER"
replace brand_descr="YERING STATION" if brand_descr=="YERING STATION NELL"
replace brand_descr="ZIMMERMAN-GRAEFF" if brand_descr=="ZIMMERMAN"
replace brand_descr="ZONIN" if brand_descr=="ZONIN PRIMO AMORE"| brand_descr=="ZONIN TERRE PALLADIANE"
replace brand_descr="ZUCCARDI" if brand_descr=="ZUCCARDI Q"| brand_descr=="ZUCCARDI SERIE A"| brand_descr=="ZUCCARDI ZETA"
replace brand_descr="GALLO FAMILY VINEYARDS" if brand_descr=="GALLO FAMILY VINEYARDS TWN VLY"| brand_descr=="GALLO FAMILY VNYD TWN VLY"


replace brand_descr="ROBERT MONDAVI" if brand_descr=="WOODBRIDGE RBRT MNDV"| brand_descr=="ROBERT MONDAVI PRIVATE SELECTN"| brand_descr=="LA FAMIGLIA DI ROBERT MONDAVI"| brand_descr=="WOODBRIDGE"| brand_descr=="WOODBRIDGE RBRT MNDV SLT VY SR"| brand_descr=="WOODBRIDGE BY ROBERT MONDAVI"
replace brand_descr="BAREFOOT" if brand_descr=="BAREFOOT BUBBLY"
replace brand_descr="SUTTER HOME" if brand_descr=="PORTICO FROM SUTTER HOME" | brand_descr=="PORTICO FROM SUTTER HOME"
replace brand_descr="BERINGER" if brand_descr=="LOS HERMANOS BY BERINGER" | brand_descr=="STONE CELLARS BY BERINGER"
replace brand_descr="GALLO FAMILY VINEYARDS" if brand_descr=="GALLO FAMILY VINEYARDS SNMA RS"| brand_descr=="GALLO SHEFFIELD CELLARS"|brand_descr=="GALLO SHEFFIELD CLLRS SLVR LNE"| brand_descr=="GALLO"| brand_descr=="GALLO FAIRBANKS"| brand_descr=="ERNEST & JULIO GALLO SONOMA"| brand_descr=="ERNEST & JULIO GALLO VINEYARDS"| brand_descr=="GALLO ESTATES"| brand_descr=="GALLO FAMILY VINEYARDS TWN VLY"| brand_descr=="GALLO FAMILY VNYD TWN VLY"| brand_descr=="GALLO FAMILY VINEYARDS TWN VLY"

replace brand_descr="KENDALL-JACKSON" if brand_descr=="KENDALL-JACKSON AVANT"| brand_descr=="KENDALL-JACKSON GREAT ESTATES"| brand_descr=="KENDALL-JACKSON HIGHLAND ESTS"| brand_descr=="KENDALL-JACKSON JACKSON ESTATE"| brand_descr=="KENDALL-JACKSON JACKSON HILLS"| brand_descr=="KENDALL-JACKSON STATURE"

replace brand_descr="CONCHA Y TORO FRONTERA" if brand_descr=="CONCHA Y TORO"| brand_descr=="CONCHA Y TORO AMELIA"| brand_descr=="CONCHA Y TORO CSLR DEL DIABLO"| brand_descr=="CONCHA Y TORO FRONTERA"| brand_descr=="CONCHA Y TORO MRQS D CASA CNCH"| brand_descr=="CONCHA Y TORO TERRUNYO"| brand_descr=="CONCHA Y TORO XPLORADOR"

*************************************************************************************************************************************

*some kosher wine's appellation label is not captured correctly

replace appellation_label="Imported" if upc_descr=="VLA SANTERO MSC D ASTI KT"
replace Importing_country="Italy" if upc_descr=="VLA SANTERO MSC D ASTI KT"
replace Importing_region="Asti" if upc_descr=="VLA SANTERO MSC D ASTI KT"
replace appellation_label="Imported" if upc_descr=="CHATEAU CAMPLAY BDX SPRR KT"
replace Importing_country="France" if upc_descr=="CHATEAU CAMPLAY BDX SPRR KT"
replace Importing_region="Bordeaux" if upc_desc=="CHATEAU CAMPLAY BDX SPRR KT"
replace appellation_label="Imported" if upc_descr=="HRZG-SL BDX KT"
replace Importing_country="France" if upc_descr=="HRZG-SL BDX KT"
replace Importing_region="Bordeaux" if upc_descr=="HRZG-SL BDX KT"
replace Importing_region="Barossa" if brand_descr=="BAROSSA VALLEY"| brand_descr=="Thorn-Clarke Terra Barossa"| brand_descr=="Grant Burge Barossa Vines"| brand_descr=="Grand Barossa"| brand_descr=="BAROSSA VALLEY ESTATE"| brand_descr=="Barossa Jack"
replace Importing_country="Australia" if Importing_region=="Barossa"
replace appellation_label="Imported" if Importing_region=="Barossa"
replace Importing_region="" if Importing_region=="Barossa"          //not capturing region for AUS
replace Importing_country="France" if brand_descr=="HERZOG SELECTION"    //15 entry kosher- all from france
replace appellation_label="Imported" if brand_descr=="HERZOG SELECTION"   
replace Importing_region="Bordeaux" if upc_descr=="HRZG-SL BDX KT"   
replace Varietal="Cabernet Sauvignon" if upc_desc=="ALFASI CHL CB RED IDT"
replace Varietal="Chardonnay" if upc_descr=="GRN PTH AS CHRD OR WT BX IDT"   //captruing as grenache from GRN
replace Varietal="other_white" if upc_descr=="GRN TMB WT OR CA G WT DDT"   //capturing grenache
replace wine_type="WHITE" if upc_descr=="BR-TR T-VN CHRD C-V V RED DDT"| upc_descr=="MARTIN RAY CHRD CRNV V RED DDT"

*formatting
replace wine_type = proper(wine_type)
replace Varietal = proper(Varietal)
replace brand_descr = proper(brand_descr)

replace Varietal="Other_Red" if Varietal=="Red Blend" & appellation_label!="Imported"
replace Varietal="Other_Red_Imported" if Varietal=="Red Blend" & appellation_label=="Imported"
replace Varietal="Other_White" if Varietal=="White Blend" & appellation_label!="Imported"
replace Varietal="Other_White_Imported" if Varietal=="White Blend" & appellation_label=="Imported"
replace Varietal="Otherwinetype_Imported" if appellation_label=="Imported" & Varietal=="Otherwinetype"


*Converting quantity in 750ml
gen size_ml=size1_amount
replace size_ml=size1_amount if size1_units=="ML" 
replace size_ml=750 if size1_units=="CT" 
replace size_ml=size1_amount*1000 if size1_units=="LI"
replace size_ml=size1_amount*29.5735 if size1_units=="OZ"

*converting qty in 750ml bottle equivalent
drop if quantity==.                    // one obs
gen quantity_total=quantity*multi      //to capture multi pack information
gen quantity_ml=quantity_total*size_ml
gen quantity_bottle=quantity_ml/750

*label var size_ml "750 ml bottle"
label var quantity_bottle "quantity 750ml bottle equivalent"
label var quantity_ml "quantity in ml"
drop quantity_total


*Price- per ltr and discounted
gen price_per_bottle_final = final_price_paid/quantity_bottle
gen price_per_bottle = total_price_paid/quantity_bottle

merge m:1 year using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\CPI data\CPI_deflator"  
drop _merge
gen final_price_per_bottle_deflated = price_per_bottle_final/Deflator
gen price_per_bottle_deflated = price_per_bottle/Deflator

summarize price_per_bottle_deflated, detail
drop if price_per_bottle > 400     // 1 observation with considered outlier


****making varietal name uniform****
replace Varietal="Cabernet_Primitivo" if Varietal=="Cab_Primitvo"
replace Varietal="Cab_Merlot_CabFranc" if Varietal=="CabS_Merlot_CabF"
replace Varietal="Cabernet_Franc" if Varietal=="Cab_Franc"
replace Varietal="Cabernet_Sangiovese" if Varietal=="Cab_Sang"
replace Varietal="Cab_Syrah_Mnstrl" if Varietal=="Cab_Shz_Mstrl"
replace Varietal="Cabernet_Syrah" if Varietal=="Cabernet_Shiraz"
replace Varietal="Cabernet_Malbec" if Varietal=="Cab_Malbec"
replace Varietal="Merlot_PinotNoir" if Varietal=="Merlot_Pinotn"
replace Varietal="Monastrell_Syrah" if Varietal=="Mns_Syrah"
replace Varietal="Monastrell_Temprn" if Varietal=="Monas_Tempr"
replace Varietal="Negroamaro" if Varietal=="Negro_Amaro"
replace Varietal="Syrah_Pinotage" if Varietal=="SHZ_Pinotage"
replace Varietal="Syrah_Greenache" if Varietal=="Shiraz_Greenache"
replace Varietal="Syrah_Malbec" if Varietal=="Shiraz_Malbec"
replace Varietal="Syrah_Merlot" if Varietal=="Shiraz_Merlot"
replace Varietal="Syrah_PinotNoir" if Varietal=="Shiraz_PinotN"
replace Varietal="Syrah_Temprn" if Varietal=="Shiraz_Tmpn"
replace Varietal="Syrah_Merlot_Cab" if Varietal=="Shz_Mer_CabS" 
replace Varietal="Syrah_Viognier" if Varietal=="Shz_Viognier"
replace Varietal="Sangiovese_Merlot" if Varietal=="Sang_Merlot"
replace Varietal="Cabernet_Merlot" if Varietal=="S_Cab_Merlot"
replace Varietal="Syrah_Pinotage" if Varietal=="Shz_Pinotage"
replace Varietal="Temprn_Malbec" if Varietal=="Tmpn_Mlbc"
replace Varietal="Syrah_Temprn" if Varietal=="Syr_Temp "
replace Varietal="Temprn_Malbec" if Varietal=="TMPN_MLBC"
replace Varietal="Temprn_Cabernet" if Varietal=="Temp_Cbs"
replace Varietal="Temprn_Grenache" if Varietal=="Temp_Grn"
replace Varietal="Zinfandel_Syrah" if Varietal=="Zinfandel_Shiraz"
replace Varietal="Sauvignon Blanc" if Varietal=="Sauvignon" & wine_type=="White"
replace Varietal="Other_Red_Imported" if Varietal=="Rosso"    // Rosso means red
replace Varietal="Chenin_Chardonnay" if Varietal=="Cheninb_Chrd"
replace Varietal="Chenin_SauvignonBlanc" if Varietal=="Cheninb_Saub"
replace brand_descr ="Columbia Crest" if brand_descr =="Columbia Crest Grand Estates"| brand_descr =="Columbia Crest Two Vines"
replace Varietal="Blush_Rose" if Varietal=="Blush"| Varietal=="Rose"
replace wine_type="Specialty" if wine_type=="Other"
replace Varietal="Other_Specialty" if Varietal=="Otherwinetype"
replace Varietal="Other_Specialty_Imported" if Varietal=="Otherwinetype_Imported"
replace Varietal="Other_White_Imported" if upc_desc=="PL-ANHSR GM BL-NR P-N WT IDT"     //german champagne
replace Varietal="Other_Red_Imported" if upc_descr=="SML W&C AS D/D RED IDT"
replace Varietal="Cabernet Sauvignon" if upc_descr=="SML W&C AS CB-S RED IDT"
replace Varietal="Cabernet_Grenache_Sauvignon" if Varietal=="Cabs_Gn_Sb"
replace Varietal="Cabernet_Merlot_CabFranc" if Varietal=="Cabs_Merlot_Cabf"
replace Varietal="Chard_PinotNoir" if Varietal=="Chard_Pn"
replace Varietal="Chard_Sauvignon" if Varietal=="Chard_Sauv"
replace Varietal="Syrah_Temprn" if Varietal=="Syr_Temp"
replace Varietal="Other_Red" if Varietal=="Meriatge" & wine_type=="Red" & appellation_label!="Imported"
replace Varietal="Other_Red_Imported" if Varietal=="Meriatge" & wine_type=="Red" & appellation_label=="Imported"
replace Varietal="Other_White" if Varietal=="Meriatge_White" & appellation_label!="Imported"
replace Varietal="Other_White_Imported" if Varietal=="Meriatge_White" & appellation_label=="Imported"
replace Varietal="Other_Red" if Varietal=="Meritge_Chard" & appellation_label!="Imported"        //data describes as red
replace Varietal="Other_Red_Imported" if Varietal=="Meritge_Chard" & appellation_label=="Imported"
replace Varietal="Petite_Syrah" if Varietal=="P_Sirah"
replace Varietal="Other_White" if Varietal=="Rhine" & appellation_label!="Imported"
replace Varietal="Other_White_Imported" if Varietal=="Rhine" & appellation_label=="Imported"
replace Varietal="Other_White_Imported" if Varietal=="Piesporter_Michelsberg"
replace Varietal="Gre_Syr_Mouv" if Varietal=="Syr_Gr_Mourvedre"
replace Varietal="Other_Red" if Varietal=="Malavasia"        //most are from Italy not captured as varietal, only 2 domestic entry
replace wine_type="White" if Varietal=="Marsala" 
replace Varietal="Other_White" if Varietal=="Marsala"      //marsala is a way of making wine, mostly consists of white grapes
replace Varietal="Semillon" if Varietal=="Semilon"
replace Varietal="Gewurztraminer" if Varietal=="Gewur"
replace Varietal="Syrah_Merlot_Cabs" if Varietal=="Shz_Mer_Cabs"
replace Varietal="Cabernet Franc" if Varietal=="Cabernet_Franc"
replace Varietal="Syrah_Pinotage" if Varietal=="Shiraz_Pinotn"
replace Varietal="Alvarinho" if Varietal=="Albarinho"
replace Varietal="Chard_Semillon" if Varietal=="Chard_Semilon"
replace Varietal="Chard_CheninBlanc" if Varietal=="Chenin_Chardonnay"
replace Varietal="Chard_Pinotage" if Varietal=="Pinotg_Chard"
replace Varietal="SauBlanc_Semillon" if Varietal=="Saublanc_Semilon"
replace Varietal="Chard_Sauvignon" if Varietal=="Sb_Chard"
replace Varietal="Other_Red" if Varietal=="Chianti_Other"                  //only domestic
replace Varietal="Other_Red_Imported" if Varietal=="Burgandy_Other" & appellation_label =="Imported"     //capturing imported first
replace Varietal="Other_Red" if Varietal=="Burgandy_Other"
replace Varietal="Other_White_Imported" if Varietal=="Chablis_Other" & appellation_label =="Imported"     //capturing imported first
replace Varietal="Other_White" if Varietal=="Chablis_Other"
replace Varietal="Pinot Noir" if Varietal=="Pinot_Nero"
replace wine_appellation="" if upc_descr=="SNTA BRBRA IT AZLENDA WT IDT"| upc_descr=="SN ANT IT LMBRSC RED IDT"

*merge country of origin information based on web-search on basis of brand name--for these entries we did not find any information in the data regarding whether they are imported or domestic :

merge m:1 brand_descr using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\2a_region_brand.dta"
drop _merge
replace appellation_label ="US" if brand_country =="US" & appellation_label=="No"
replace Importing_country = brand_country if Importing_country =="" & brand_country!="US" & appellation_label=="No"
replace appellation_label ="Imported" if Importing_country!=""

drop brand_country brand_state_location
drop price_per_bottle_final price_per_bottle final_price_per_bottle_deflated price_per_bottle_deflated


*merge state and AVA inofmration based on web-search on basis of brand name--for these entries data only tells that these are domestic or imported wine but no specific information on state/country

merge m:1 brand_descr using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\2b_external_match_basedonbrands.dta"

replace state_appellation = state_appellation_m if state_appellation =="" & wine_appellation =="" & Importing_country==""
replace wine_appellation = wine_appellation_m if wine_appellation =="" & state_appellation =="" & Importing_country==""
replace Importing_country = Importing_country_m  if Importing_country =="" & wine_appellation =="" & state_appellation ==""
drop wine_appellation_m state_appellation_m Importing_country_m _merge 

*including excise tax information for instruments
gen wineclass=""
replace wineclass="sparkling" if product_module_descr=="WINE-SPARKLING"
replace wineclass="table" if wineclass==""
merge m:1 fips_state_code wineclass using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\Tax Data\state_excise_tax.dta"
drop if _merge==2
drop _merge
drop wineclass


*formatting names
replace appellation_label="State_Appellation" if state_appellation=="California"
replace state_appellation="New York" if state_appellation=="New_York"
replace state_appellation="North Carolina" if state_appellation=="North_Carolina"
replace wine_appellation="Alexander_Valley" if wine_appellation=="Alexander Valley"
replace wine_appellation="Lehigh Valley" if wine_appellation=="Lehigh_Va"
replace wine_appellation="Shenandoah_Valley" if wine_appellation=="Shenandoah_Valley_Cal"
replace wine_appellation="South_Coast" if wine_appellation=="South Coast"
replace wine_appellation="Walla_Walla" if wine_appellation=="Walla Walla"
replace state_appellation="Pennsylvania" if state_appellation=="Pennslyvania"

*classify all geographic information as 4 broader classification
replace appellation_label="AVA" if wine_appellation!=""
replace appellation_label="State_Appellation" if state_appellation!=""
replace appellation_label="Imported" if Importing_country!=""
replace appellation_label="us_not_labelled" if state_appellation=="us_not_labelled"
replace state_appellation="" if state_appellation=="us_not_labelled"
replace appellation_label="us_not_labelled" if appellation_label=="US"

drop if appellation_label=="No"      //27,834   no info on whether domestic or foreign
drop if appellation_label=="us_not_labelled"  //21,367 no information on 

replace state_appellation="Virginia" if wine_appellation=="Virginia"
replace wine_appellation="" if wine_appellation=="Virginia"
replace appellation_label="State_Appellation" if state_appellation=="Virginia"
replace state_appellation="New Mexico" if wine_appellation=="New Mexico"
replace wine_appellation="" if wine_appellation=="New Mexico"
replace appellation_label="State_Appellation" if state_appellation=="New Mexico"
replace Importing_country="France" if brand_descr=="Veuve Clicquot Ponsardin"| brand_descr=="La Vieille Ferme"
replace Importing_region="Champgane" if brand_descr=="Veuve Clicquot Ponsardin"
replace Importing_country="Italy" if brand_descr=="Stella Rosa" | brand_descr=="Santa Margherita"| brand_descr=="Ruffino"| brand_descr=="Roscato"| brand_descr=="Riunite"| brand_descr=="Lamarca"| brand_descr=="Castello Del Poggio"| brand_descr=="Bolla"| brand_descr=="Bartenura"| brand_descr=="Gaetano D'Aquino"
replace appellation_label="Imported" if brand_descr=="Stella Rosa" 
replace state_appellation="" if brand_descr=="Stella Rosa" 
replace Importing_country="Australia" if brand_descr=="Jacob'S Creek"| brand_descr=="Bodega Norton"
replace Importing_country="Spain" if brand_descr=="Don Simon" | brand_descr=="Cristalino"
replace Importing_country="Argentina" if brand_descr=="Trapiche"


bysort Importing_country: egen country_quantity = total(quantity_bottle) if Importing_country! =""
preserve
keep Importing_country country_quantity
duplicates drop
egen rank_country = rank(-country_quantity)
drop if Importing_country == ""
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\country_ranking.dta", replace
restore
merge m:1 Importing_country using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\country_ranking.dta"
replace Importing_country="Other_Countries" if rank_country >11 & appellation_label=="Imported"         // (basically selecting top 10 countries)
replace Importing_country="Other_Countries" if Importing_country=="Other"
drop _merge rank_country country_quantity


drop Alexander_Valley- Potter_Valley
gen geographic_label=""
replace geographic_label=wine_appellation
replace geographic_label=Importing_country if geographic_label=="" & Importing_country!=""
replace geographic_label=state_appellation if geographic_label=="" & state_appellation!=""

*Capture origin state from diffrent type of geographic lable to create distance intrument

replace origin_state ="California" if geographic_label=="Lake_County"| geographic_label=="San_Bernabe"| geographic_label=="San_Lucas"| geographic_label=="Santa_Clara"| geographic_label=="Rogue_Valley"| geographic_label=="Spring_Mountain_District"| geographic_label=="Shenandoah_Valley"| geographic_label=="Yadkin_Valley"| geographic_label=="Red_Mountain"| geographic_label=="Monticello"| geographic_label=="Sta_Rita_Hills"| geographic_label=="Stags_Leap_District"| geographic_label=="Altus"| geographic_label=="Atlas_Peak"| geographic_label=="Ballard_Canyon"| geographic_label=="Chehalem_Mountains"| geographic_label=="Chiles_Valley"| geographic_label=="Dundee_Hills"| geographic_label=="El_Dorado"| geographic_label=="Fort_Ross_Seaview"| geographic_label=="Hames_Valley"| geographic_label=="High_Valley"| geographic_label=="Los_Olivos"| geographic_label=="Monticello"| geographic_label=="Santa_Ynes_Valley"| geographic_label=="Oak_Knoll"| geographic_label=="Temecula_Valley"| geographic_label=="San_Benito"| geographic_label=="Northern_Sonoma"| geographic_label=="Yorkville_Highlands"| geographic_label=="South_Coast"| geographic_label=="Potter_Valley"| geographic_label=="Rattle_Snake_Hills"
replace origin_state ="California" if wine_appellation =="Amador_County"
replace origin_state ="Missouri" if wine_appellation =="Augusta"
replace origin_state ="California" if wine_appellation =="Arroyo_Seco"
replace origin_state ="California" if wine_appellation =="Central_Coast"
replace origin_state ="New_York" if wine_appellation =="Finger_Lakes"
replace origin_state ="California" if wine_appellation =="Mendocino"
replace origin_state ="California" if wine_appellation =="Monterey_County"
replace origin_state ="California" if wine_appellation =="Napa_Valley"
replace origin_state ="Ohio" if wine_appellation =="Ohio_River_Valley"
replace origin_state ="California" if wine_appellation =="San_Antonio"
replace origin_state ="Idaho" if wine_appellation =="Snake_River_Valley"
replace origin_state ="California" if wine_appellation =="Sonoma"
replace origin_state ="Oregon" if wine_appellation =="Willamette_Valley"
replace origin_state ="New_York" if wine_appellation =="Lake_Erie"
replace origin_state ="New_York" if origin_state =="New York"|geographic_label=="Long_Island"
replace origin_state ="Colorado" if wine_appellation =="Colorado_Grand_Valley"
replace origin_state="Ohio" if geographic_label=="Grand_River_Valley"| geographic_label=="Isle_St_George"
replace origin_state="Michigan" if geographic_label=="Lake_Michigan_Shore"| geographic_label=="Leelanau_Peninsula"
replace origin_state="Pennsylvania" if geographic_label=="Lancaster_Valley"| geographic_label=="Lehigh Valley"
replace origin_state="Connecticut" if geographic_label=="Southeastern_New_England"
replace origin_state="Texas" if geographic_label=="Texas_Hill_Country"
replace origin_state="Arkansas" if geographic_label=="Ozark_Hignlands"
replace origin_state="Imported" if appellation_label=="Imported" & origin_state ==""
replace origin_state=state_appellation if origin_state =="" & appellation_label=="State_Appellation" 
replace origin_state ="California" if geographic_label =="Alexander_Valley"| geographic_label =="Anderson_Valley"| geographic_label =="Carneros"| geographic_label =="Chalone"| geographic_label =="Clarksburg"| geographic_label =="Contra_Costa_County"| geographic_label =="Dry_Creek_Valley" | geographic_label =="Dunnigan_Hills"| geographic_label =="Edna_Valley"| geographic_label =="Eola_Hills"| geographic_label =="Knights_Valley"| geographic_label =="Lodi"| geographic_label =="Mendocino County"| geographic_label =="North_Coast"| geographic_label=="Oakville"| geographic_label =="Paso_Robles"| geographic_label =="Russian_River_Valley"| geographic_label =="Rutherford"| geographic_label =="Santa_Barbara_County"| geographic_label =="Santa_Maria_Valley"| geographic_label =="Sierra_Foothills"| geographic_label =="Sonoma County"| geographic_label ==" Sonoma_Coast"| geographic_label =="Sonoma_Mountain"| geographic_label =="Sonoma_Valley"| geographic_label =="St_Helena"| geographic_label =="Walla_Walla"| geographic_label =="Yakima_Valley"
replace origin_state ="Washington" if geographic_label =="Columbia_Valley"
replace origin_state ="Texas" if geographic_label =="Texas_High_Plains"
replace origin_state ="Michigan" if geographic_label =="Old_Mission_Peninsula"
replace origin_state ="California" if geographic_label =="Sonoma_Coast"



merge m:1 scantrack_market_descr origin_state using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\Distance Instrument\distance.dta"
drop if _merge ==2     //159 obs 
drop _merge
replace state_appellation="Other_State" if state_appellation=="Pureto Rico"     //captured from web search


******************************Merge with Population Data********************************************
preserve
drop if year>2009 
merge m:1 scantrack_market_descr using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\MSA_Population\Population_2005-09.dta"
drop _merge
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\poputemp1.dta", replace
restore 

preserve
drop if year<2010
sort year scantrack_market_descr
merge m:1 year scantrack_market_descr using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Other Supplement Data\MSA_Population\Population_2010-19.dta"
drop _merge
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\poputemp2.dta", replace
restore 

clear all
use "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\poputemp1.dta", clear
append using "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\poputemp2.dta"
save "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\2_varietal_and_supplement_data.dta", replace

erase "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\poputemp1.dta"
erase "Z:\NIELSENDATA\consumer_panel\codeSTATA\GianCarlo\Discrete Choice\Main\Data\poputemp2.dta"


******************************End of DO File*******************************************************


