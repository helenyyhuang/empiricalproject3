* Do-File for Empirical Project 3
* Author: Helen (Yingying) Huang
* Date: 11/20/2023
* Purpose: Analyze CMTO data for housing policy insights
* Dataset: cmto.dta
* Stata Version: 17
version 17

* Change directory to 'project3'
cd "../project3"

* Set relative paths for the data and log files
local datapath "./"
local dataset "cmto.dta"
local logfile "empiricalproject3_helen.log"

* Start log file to record session
log using "`datapath'`logfile'", replace

* Check if 'estout' and 'outreg2' package are installed and install if not present
cap which estout
if _rc == 1 ssc install estout

cap which outreg2
if _rc == 1 ssc install outreg2

* Load the dataset
use "`datapath'`dataset'", clear

* Error checking after loading data to ensure the file exists and is not empty
if _rc {
    di "Error: Dataset could not be loaded. Please check the file path."
    exit 1
}	

* Question 2: Random Assignment Evidence
* -----------------------------------

* Use the collapse command
collapse (mean) hoh_age homeless hh_income black white college_plus, by(treatment_group)

* List the collapsed data
list

* Question 3 Balance Table
* -----------------------------------

* Re-load the full dataset
use "`datapath'`dataset'", clear

* Define a local macro for the relevant variables
local vars hoh_age homeless hh_income black white college_plus

* Create a new matrix to store the results
matrix results = J(6, 4, .)

* Loop over the variables to perform regressions and store the results
local rownum = 1
foreach var in `vars' {
    
	* Get the summary statistics for both control and treatment groups
    quietly sum `var' if treatment_group == 0
    local mean_control = r(mean)
    quietly sum `var' if treatment_group == 1
    local mean_treatment = r(mean)
    
	* Store the means and calculate the difference in means
    matrix results[`rownum', 1] = `mean_control'
    matrix results[`rownum', 2] = `mean_treatment'
    matrix results[`rownum', 3] = `mean_treatment' - `mean_control'
	
    * Run regression to get the standard error of the difference
    quietly reg `var' i.treatment_group
    matrix results[`rownum', 4] = _se[1.treatment_group]

	* Increment the rownum for the next iteration
    local rownum = `rownum' + 1
}

* Name the rows and columns of the results matrix
matrix rownames results = hoh_age homeless hh_income black white college_plus
matrix colnames results = Mean_in_Control_Group Mean_in_Treatment_Group Difference_in_Means SE_of_Difference

* List the results matrix
matrix list results

* Question 5 Compliance Estimate Rate
* -----------------------------------

* Regress 'received_cmto_services' on 'treatment_group' and 'pha' categories
regress received_cmto_services i.treatment_group i.pha

* Question 7 ITT Effect
* -----------------------------------

* Perform regression to assess the effect of 'treatment_group' and 'pha' on 'leased_up_opp'.
regress leased_up_opp i.treatment_group i.pha

* Question 8 TOT Effect
* -----------------------------------

* Estimate 'leased_up_opp' regression and store ITT effect and SE.
regress leased_up_opp treatment_group i.pha
scalar itt_effect = _b[treatment_group]
scalar itt_se = _se[treatment_group]

* Estimate 'received_cmto_services' regression and store compliance rate.
regress received_cmto_services treatment_group i.pha
scalar compliance_rate = _b[treatment_group]

* Display TOT effect and SE.
display "TOT effect = " itt_effect / compliance_rate
display "TOT SE = " itt_se / compliance_rate

* Question 9 Predicted Childhood Change in Environment
* -----------------------------------

* Calculate and store the predicted childhood environmental change
gen predicted_change_in_environment = forecast_kravg30_p25 - origin_forecast_kravg30_p25

* Initialize 'group' variable and assign treatment group as '1'
gen group = 0
replace group = 1 if treatment_group == 1

* Plot density of predicted change for treatment and control groups
twoway (kdensity predicted_change_in_environment if group == 1, lpattern(solid) lcolor(blue)) (kdensity predicted_change_in_environment if group == 0, lpattern(dash) lcolor(red)), legend(label(1 "Treatment") label(2 "Control")) title("Predicted Change in Childhood Environment") xtitle("Predicted Change") ytitle("Density") name(KDEPlot, replace)

* Export the density plot to a PNG image
graph export "`datapath'figures_helen/figure1.png", replace

* Question 10 Heterogeneous Treatment Effects
* -----------------------------------

* Calculate median household income and define higher/lower income groups
egen median_income = median(hh_income)
gen higher_income = hh_income > median_income
gen lower_income = hh_income <= median_income

* Define regression formula
local formula "leased_up_opp treatment_group i.pha"

* Analyze effect on higher income group
regress `formula' if higher_income
scalar itt_effect_higher = _b[treatment_group]
summarize received_cmto_services if treatment_group == 1 & higher_income
scalar compliance_rate_higher = r(mean)
scalar tot_higher_income = itt_effect_higher / compliance_rate_higher

* Analyze effect on lower income group
regress `formula' if lower_income
scalar itt_effect_lower = _b[treatment_group]
summarize received_cmto_services if treatment_group == 1 & lower_income
scalar compliance_rate_lower = r(mean)
scalar tot_lower_income = itt_effect_lower / compliance_rate_lower

* List labels for 'pha'
label list pha

* Analyze effect for KCHA
regress `formula' if pha == 0
scalar itt_effect_kcha = _b[treatment_group]
summarize received_cmto_services if treatment_group == 1 & pha == 0
scalar compliance_rate_kcha = r(mean)
scalar tot_kcha = itt_effect_kcha / compliance_rate_kcha

* Analyze effect for SHA
regress `formula' if pha == 1
scalar itt_effect_sha = _b[treatment_group]
summarize received_cmto_services if treatment_group == 1 & pha == 1
scalar compliance_rate_sha = r(mean)
scalar tot_sha = itt_effect_sha / compliance_rate_sha

* Display Treatment on Treated (TOT) effects for income groups and PHAs
display "TOT for higher income families: " tot_higher_income
display "TOT for lower income families: " tot_lower_income
display "TOT for KCHA: " tot_kcha
display "TOT for SHA: " tot_sha

* Cleanup and close
* -----------------------------------

log close
clear all
