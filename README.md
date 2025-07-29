# TG_ACF_BD
**********************************************************************************************
************************************** HH information ****************************************
**********************************************************************************************
set more off
clear
cd "E:\TB prevalence_Tea garden\Result findings\Manuscript"

*use hh.dta, clear
use HH_information.dta, clear
replace householder_religion=2 if uniq_id=="b0da9bec-d6e3-485a-a7ad-f9fa22734fbc"
tab1 tea_garden_cat

/**** 
recode age (0/4.7=0 "<5")(5/14=1 "5-14")(15/24=2 "15-24")(25/44=3 "25-44")(45/64=4 "45-64")(65/100=5 "65+"),gen(Age_mem_cat)
asdoc tab Age_mem_cat, chi
asdoc tab gender, chi

** HH religion 
lab def householder_religion 1 "Muslim" 2 "Hindu" 3 "Buddhist" 4 "Christian"  5 "Trival", modify
lab val householder_religion householder_religion
tab householder_religion tea_garden_cat, chi
*/
*** HH Monthly income
desc monthly_income monthly_expenditure
byso tea_garden_cat: summarize monthly_income, detail 

replace monthly_income=. if monthly_income==0|monthly_income==12 
recode monthly_income (500/5000=1 "500-5000")(5001/10000=2 "5001-10000")(10001/20000=3 "10001-20000")(20001/50000=4 "20001-50000")(50001/300000=5 ">50000"), gen(monthly_income_cat)
tab monthly_income_cat tea_garden_cat, m chi
asdoc tab monthly_income_cat tea_garden_cat, col nofreq m
/*** HH Monthly expenditure
replace monthly_expenditure=. if monthly_expenditure==0
recode monthly_expenditure (250/5000=1 "250-5000")(5001/10000=2 "5001-10000")(10001/20000=3 "10001-20000")(20001/50000=4 "20001-50000")(50001/300000=5 ">50000"), gen(monthly_expenditure_cat)
tab monthly_expenditure_cat tea_garden_cat, chi m 
asdoc tab monthly_expenditure_cat tea_garden_cat, col nofreq m
*/
*** HH Toilet facility
lab def toilet_facility_used 1 "Flushed Toilet with septic tank" 2 "Toilet without septic tank" 3 "Improved Pit latrine" 4 "open Pit latrine" 5 "Hanging toilet" 6 "No toilet facility" 77 "Other"
lab val toilet_facility_used toilet_facility_used
*asdoc tabsort toilet_facility_used tea_garden_cat,m save(Manuscript_table)
tabsort toilet_facility_used tea_garden_cat, chi
asdoc tabsort toilet_facility_used tea_garden_cat, col nofreq m

*** HH Room number
recode room_number (1=1 "1")(2=2 "2")(3/4=3 "3-4")(5/43=4 "5+"), gen(room_number_cat)
tab room_number_cat tea_garden_cat,chi
asdoc tab room_number_cat tea_garden_cat, col nofreq m

*** HH drinking water
gen source_of_drinking_water_1=substr(source_of_drinking_water,1,1)
gen source_of_drinking_water_2=substr(source_of_drinking_water,3,1)
*destring source_of_drinking_water_1 source_of_drinking_water_2,replace 
*lab def source_of_drinking_water_1 1 "Piped water into dwelling" 2 "Piped water to Yard" 3 "Public Tap" 4 "Tube well" 5 "Dug well" 6 "Springwater" 7 "Rainwater" 8 "Surface water" 9 "Pond/river/lake/canal" 77 "Other"
*lab val source_of_drinking_water_1 source_of_drinking_water_1
*lab val source_of_drinking_water_2 source_of_drinking_water_1

mrtab source_of_drinking_water_1 source_of_drinking_water_2, by(tea_garden_cat) chi2
mrtab source_of_drinking_water_1 source_of_drinking_water_2, by(tea_garden_cat) includemissing col nofreq 

*** HH Mobile access
format contact_no %10.0f
replace contact_no=0 if contact_no==88|contact_no==88|contact_no==888888|contact_no==888888|contact_no==888888|contact_no==888888|contact_no==4764330959|contact_no==4781656813|contact_no==4795621837|contact_no==4797846896|contact_no==22222222222|contact_no==25252525252|contact_no==99999999999
duplicates report contact_no
sort contact_no
*duplicates drop contact_no, force
*drop if contact_no==0|contact_no==.
gen mobile_Yes=0 if contact_no==.|contact_no==0
replace mobile_Yes=1 if mobile_Yes==.
tab mobile_Yes tea_garden_cat , chi
tab mobile_Yes tea_garden_cat , chi col nofreq
tabsort toilet_facility_used tea_garden_cat,m 
tab toilet_facility_used tea_garden_cat, chi col nofreq m 

****************************************************
*************** Wealth quintile ********************
****************************************************
predict wealth
xtile quint=wealth,nq(4)

summ wealth if quint==1
summ wealth if quint==2
summ wealth if quint==3
summ wealth if quint==4
*summ wealth if quint==5

tab quint 
lab def lquint 1"Poorest" 2"Poor" 3"Medium" 4"Wealthy"
lab val quint lquint
asdoc tab quint,append
tab quint tea_garden_cat, chi 
asdoc tab quint tea_garden_cat, col nofreq

**********************************************************************
*************** Median no of family member per houshold *************
**********************************************************************
use HH_information_member.dta, clear
replace gender=2 if gender==3
sort uniq_id uuid member_id
order uniq_id uuid member_id
ed uniq_id uuid member_id
sort uniq_id member_id 

egen n_members = count(uuid), by(uniq_id)
duplicates drop uniq_id, force
bysort uniq_id (uuid): keep if _n == 1
duplicates report uniq_id
summarize n_members, detail 
byso tea_garden_cat1: asdoc summarize n_members, stat(N mean sd min max p50 p25 p75) dec(2) append

********************** 
*toilet_facility_shared anything_do_for_water_purific electricity solar_electricity television mobile refrigerator almira fan computer other_valuable_goods car cng motor_cycle bicycle rickshaw own_poultry bank_account
*saveold HH_contact.dta, replace 

************************************************************************************************
******************************** HH member information ACF *************************************
************************************************************************************************
use HH_information_member.dta, clear

**** Age category
replace member_age_Y=age if member_age_Y==.
replace member_age_M=0 if member_age_M==.
replace member_age_D=0 if member_age_D==.
gen Age_mem=round(member_age_Y+(member_age_M/12)+(member_age_D/365), .1)

recode Age_mem (0/4.7=0 "<5")(5/14=1 "5-14")(15/24=2 "15-24")(25/44=3 "25-44")(45/64=4 "45-64")(65/100=5 "65+"),gen(Age_mem_cat)
tab Age_mem_cat tea_garden_cat, chi col 
asdoc tab Age_mem_cat tea_garden_cat, chi  

** HH Gender 
tab gender tea_garden_cat, chi col 
tab gender tea_garden_cat, chi col nofreq
asdoc tab gender tea_garden_cat, chi 
/** HH member marital status
lab def marital_status 1 "Married" 2 "Unmarried" 3 "Widow" 4 "Divorced"
lab val marital_status marital_status
tab marital_status tea_garden_cat, chi col
*/
** HH members education
tab education tea_garden_cat 
recode education (0=0 "No education") (1/5=1 "Up to primary") (6/10=2 "Up to SSC")(11/12=3 "HSC")(13/17=4 "Graduation and above"), gen(Edu_cat)
tab Edu_cat tea_garden_cat, chi col
tab Edu_cat tea_garden_cat, chi col nofreq

** HH members Ocupation
tab occupation tea_garden_cat 
lab def occupation 1 "Tea Garden worker" 2 "Unemployed" 3 "Student" 4 "Child" 5 "Homemaker" 77 "Others"
lab val occupation occupation
replace occupation=2 if occupation==5 & gender==1
*replace occupation=2 if occupation==3 
replace occupation=77 if occupation==4
tab occupation tea_garden_cat, chi col

tab history_of_tb tea_garden_cat, chi col 

tab bcg tea_garden_cat, chi col

lab def nutrition 1 "Average" 2 "Over" 3 "Under"
lab val nutrition nutrition
*tab nutrition tea_garden_cat, chi col 
*** Current status of TB 
duplicates report current_TB_status
duplicates tag current_TB_status, gen(Cur_TB_dup)
sort Cur_TB_dup current_TB_status
*ed history_of_tb Cur_TB_dup current_TB_status if history_of_tb==1

tab Curr_stat_TB tea_garden_cat , col chi m 
*tab current_TB_status tea_garden_cat, chi col 
*tab how_long_ago tea_garden_cat 
*tab history_of_smoking tea_garden_cat, chi col 
tab current_smoking tea_garden_cat, chi col 
tab substance_use tea_garden_cat, chi col  
tab alcohol tea_garden_cat, chi col  
*tab chronic_illness tea_garden_cat, chi col  

******* Age distribution by monthly income
replace monthly_income=. if monthly_income==0|monthly_income==12 
recode monthly_income (500/5000=1 "500-5000")(5001/10000=2 "5001-10000")(10001/20000=3 "10001-20000")(20001/50000=4 "20001-50000")(50001/300000=5 ">50000"), gen(monthly_income_cat)
tab monthly_income_cat tea_garden_cat, m chi
*asdoc tab monthly_income_cat tea_garden_cat, col nofreq m
tab Age_mem_cat monthly_income_cat, chi col m

************************************************************************************************
****************************************** Screening ACF ***************************************
************************************************************************************************
set more off
clear
cd "E:\TB prevalence_Tea garden\Result findings\Manuscript"

use Tea_garden_10th_Aug_22.dta, clear
count if merge5==3 & presumptive_by_screener==1
* 159 presumptive_by_screener

gen TB_by_Xray_review_new=1 if TB_by_Xray_review=="Pulmonary CD case by atleast 2 reviewer"|ptb_type=="B(+)"
replace TB_by_Xray_review_new=0 if TB_by_Xray_review_new==. & presumptive_by_screener==1
lab def YN 0 "No" 1 "Yes"
lab val TB_by_Xray_review_new YN
tab TB_by_Xray_review_new tea_garden_cat1 if merge5==3 & presumptive_by_screener==1
count if merge5==3 & presumptive_by_screener==1

replace member_age_Y=age_year if member_age_Y==.
replace member_age_M=0 if member_age_M==.
replace member_age_D=0 if member_age_D==.
gen Age_mem=round(member_age_Y+(member_age_M/12)+(member_age_D/365), .1)

recode Age_mem (0/4.7=0 "<5")(5/14=1 "5-14")(15/24=2 "15-24")(25/44=3 "25-44")(45/64=4 "45-64")(65/100=5 "65+"),gen(Age_mem_cat)
tab Age_mem_cat tea_garden_cat1 if merge5==3 & presumptive_by_screener==1, col chi
* 159 presumptive_by_screener
tab  tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1
/*
recode age_year age_month age_day (.=0)
gen Age_year_cat=round(age_year+(age_month/12)+(age_day/365), .1)

recode Age_year_cat (0/4.7=0 "<5")(5/14=1 "5-14")(15/24=2 "15-24")(25/44=3 "25-44")(45/64=4 "45-64")(65/100=5 "65+"),gen(Age_year_cat2)
*/
** Smptoms distribution among presumptive cases
tab Age_mem_cat tea_garden_cat1 if presumptive_by_screener==1,m col chi 
tab gender tea_garden_cat1 if presumptive_by_screener==1,m col chi 
tab cough tea_garden_cat1 if presumptive_by_screener==1,m col chi
byso tea_garden_cat1: asdoc summarize coughduration if presumptive_by_screener==1 & cough==1, stat(N mean sd min max p50 p25 p75) dec(2) append
tab fever tea_garden_cat1 if presumptive_by_screener==1,m col chi
byso tea_garden_cat1: asdoc summarize Feverduration if presumptive_by_screener==1 & fever==1, stat(N mean sd min max p50 p25 p75) dec(2) append
tab night_sweats tea_garden_cat1 if presumptive_by_screener==1,m col chi
tab weight_loss tea_garden_cat1 if presumptive_by_screener==1,m col chi
tab tb_history tea_garden_cat1 if presumptive_by_screener==1,m col chi
tab lump tea_garden_cat1 if presumptive_by_screener==1,m col chi
*tab fatigue tea_garden_cat1 if presumptive_by_screener==1,m col chi
*tab breathing tea_garden_cat1 if presumptive_by_screener==1,m col chi

** Smptoms distribution among TB diagnosed cases
tab Age_mem_cat tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m col 
tab gender tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m col 

tab cough tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m
byso tea_garden_cat1: asdoc summarize coughduration if presumptive_by_screener==1 & TB_by_Xray_review_new==1, stat(N mean sd min max p50 p25 p75) dec(2) append

tab fever tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m
byso tea_garden_cat1: asdoc summarize Feverduration if presumptive_by_screener==1 & TB_by_Xray_review_new==1, stat(N mean sd min max p50 p25 p75) dec(2) append

tab night_sweats tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m col
tab weight_loss tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m col
tab tb_history tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m col 
tab lump tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m col 
tab fatigue tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m col 
tab breathing tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1,m col 

***** TB type
tab ptb_type tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1

**** Previous history of TB among diagnosed TB cases
tab history_of_tb  if presumptive_by_screener==1 & TB_by_Xray_review_new==1, m

tab history_of_tb tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1, chi col m

** History of smoking
tab history_of_smoking tea_garden_cat if presumptive_by_screener==1 & TB_by_Xray_review_new==1, chi col
tab current_smoking tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1, chi col

*** Substance use 
tab substance_use tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1, chi col m

*** Alcohol drinking
tab alcohol tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1, chi col m

*** Chronic illness
tab chronic_illness tea_garden_cat1 if presumptive_by_screener==1 & TB_by_Xray_review_new==1, chi col m 

****************
asdoc tab tea_garden_cat1 Age_mem_cat if merge5==3 & presumptive_by_screener==1
asdoc tab1 cough fever night_sweats weight_loss tb_history lump fatigue breathing if merge5==3 & presumptive_by_screener==1
asdoc tab tea_garden_cat1 Age_mem_cat if merge5==3 & presumptive_by_screener==1, row nofreq

tab Age_mem_cat tea_garden_cat1 if merge5==3 & presumptive_by_screener==1, chi
tab gender tea_garden_cat1 if merge5==3 & presumptive_by_screener==1, chi

tab Age_mem_cat tea_garden_cat1 if merge5==3 & presumptive_by_screener==1 & diagnosed_tb==1, chi
tab gender tea_garden_cat1 if merge5==3 & presumptive_by_screener==1 & diagnosed_tb==1, chi

**** Age category   
replace age_year=age if age_year==.
replace age_month=0 if age_month==.
replace age_day=0 if age_day==.
gen Age_mem=round(age_year+(age_month/12)+(age_day/365), .1)

recode Age_mem (0/4.7=0 "<5")(5/14=1 "5-14")(15/24=2 "15-24")(25/44=3 "25-44")(45/64=4 "45-64")(65/100=5 "65+"),gen(Age_mem_cat)
asdoc tab Age_mem_cat,chi

**************************************************** graph (HH member) ******************************************
*replace member_age_Y=0 if member_age_Y==.
*replace member_age_M=0 if member_age_M==.
*replace member_age_D=0 if member_age_D==.
*gen Age_mem=round(member_age_Y+(member_age_M/12)+(member_age_D/365), .1)
*lab var Age_mem "Household member's age (in years)"

graph box Age_mem, medt("marker") name(g1, replace) ytitle("Age (in years)") over(tea_garden_cat1) showyvars legend(off)
graph box Age_mem, medt("marker") name(g2, replace) ytitle("Age (in years)") showyvars legend(off)
grc1leg g1 g2, legendfrom(g1)  

stripplot Age_mem g2, box cumul centre vertical height(0.4) xla(, noticks)

graph box Age_mem  
