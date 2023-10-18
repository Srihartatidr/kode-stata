// Table 2: TB case definition of study participants by household type
// baseline prevalence
leftalign
browse
keep if eligible==1 & consent==1
keep if subject_cat!=1

// calculating overall consented subject
tab subject_cat consent

// labelling
label define casecatlab 0 "Not TB" 1 "Definite TB" 2 "Probable TB" 3 "Possible TB" 4 "Uncategorized"
label list
label value case_category casecatlab

// calculating TB case definition based on subject_cat
tab case_category subject_cat

*computing ci for proportions
tab case_category subject_cat, m

// PREVALENCE //
*ci and proportions (immediate command) based on tabulation above
*total
cii prop #obs #successes
cii prop 4655 60
cii prop 4655 2
cii prop 4655 152

*hhc
cii prop 514 16
cii prop 514 1
cii prop 514 38

*nc
cii prop 4141 44
cii prop 4141 1
cii prop 4141 114

*another command
ci prop case_category if subject_cat==2 
// not successful

// INCIDENCE //
// deciding population at risk: people who are disease-free at the start of data collection
// excluding tb positive subject at baseline (pcf and acf subject?)

*work with date
*convert string date to integer date
generate date_basei=date(date_baseline, "YMD")
format date_basei %d

generate date_lefti=date(date_left, "YMD")
format date_lefti %d

generate m4_datei=date(m4_date, "YMD")
format m4_datei %d

generate m4_date_lefti=date(m4_date_left, "YMD")
format m4_date_lefti %d

generate m8_datei=date(m8_date, "YMD")
format m8_datei %d

generate m8_date_lefti=date(m8_date_left, "YMD")
format m8_date_lefti %d

generate m12_datei=date(m12_date, "YMD")
format m12_datei %d

generate m12_date_lefti=date(m12_date_left, "YMD")
format m12_date_lefti %d

gsort m12_datei
browse

*make a subset based on baseline date
keep if eligible==1 & consent==1
drop if subject_cat==1
keep if inrange(date_basei, td(12apr2021), td(11oct2022))

*how to replace date -if there are any wrong date-
replace m12_datei= td(12sep2022) in 2584

*generate day_in variable
generate day_in = date_basei if consent==1
format day_in %td

*generate day_out variable
generate day_out=.
format day_out %td
browse subject_id date_basei date_lefti m4_datei m4_date_lefti m8_datei m8_date_lefti m12_datei m12_date_lefti day_in day_out
replace day_out=date_lefti if m4_datei==. & m8_datei==. & m12_datei==.
replace day_out=m8_date_lefti if date_lefti==. & m4_date_lefti==. & m8_date_lefti!=. & m12_date_lefti==.
replace day_out=m12_date_lefti if m8_date_lefti==. & m4_date_lefti==. & date_lefti==. 
replace day_out=m12_datei if date_lefti==. & m4_date_lefti==. & m8_date_lefti==. & m12_date_lefti==.
replace day_out=m8_date_lefti if date_lefti==. & m4_date_lefti==. & m12_datei==. & m12_date_lefti==.
replace day_out=m4_date_lefti if date_lefti!=. & m4_datei!=. & m8_datei==. & m8_date_lefti==. & m12_datei==. & m12_date_lefti==.
replace day_out=m4_date_lefti if date_lefti==. & m4_datei!=. & m8_datei==. & m8_date_lefti==. & m12_datei==. & m12_date_lefti==.
replace day_out=m8_datei if date_lefti==. & m4_date_lefti==. & m12_datei==.
gsort day_out 
sort day_in
codebook day_out

*start with subset with complete day_out
drop if day_out==.
drop if case_category==1

*generate follow-up duration variables
generate fudays=day_out-day_in
generate fuweeks=fudays/7
generate fumonths=fudays/30
generate fuyears=(day_out-day_in)/365.25
sort fumonths

*generate event_tb variable
*a. definite tb
generate def_tb=.
replace def_tb=1 if m4_case_category==1 | m8_case_category==1 | m12_case_category==1
replace def_tb=0 if def_tb!=1
tabulate subject_cat def_tb

*generate new variable: unexposed group(1)=nc(3) and exposed group(2)=hhc(2)
generate exposed_group=.
replace exposed_group=2 if subject_cat==2
replace exposed_group=1 if subject_cat==3

//// DECLARE THE DATA AS A SURVIVAL_TIME DATA ////
*a. use stset command based on fumonths
*a.1. basic stset command (with available length of FU duration)
stset fuyears, failure(def_tb==1)

*a.2. complete stset command (when we have not calculated the length of FU duration)
*person - years*
stset day_out, fail(case_category==1) origin(day_in) enter(day_in) id(subject_id) 

*person - months*
stset day_out, fail(case_category==1) origin(day_in) enter(day_in) id(subject_id) scale(30)

*calculate overall TB incidence rate
di 27/4107.422
// the TB incidence rate was 0.00629 cases per person-years, that is 6.29 in every 1000 people will experience TB in a year.

*estimate TB IR and 95% CI per 1000 person-years
strate, per (1000)
// the TB IR was xxx cases per 1000 person-years
// we are 95% confident that the population TB IR would at minimum be 141 and at maximum 233 cases in every 1000 persons per year.

*estimate TB IR based on group
strate exposed_group, per(1000)
// TB IR was estimated as 133 and 478 cases per 1000 person-years, for the NC and HHC, respectively
// we are 95% confident that the population TB IR for NC lies between 97 and 182 cases per 100000 person-years
// for HHC the population TB IR lies between 318 and 720 cases per 1000 person-years

*compare TB IR among groups
stir exposed_group
// the estimated rate ratio was 3.6 with the 95% CI for the population TB IRR as (2.1, 6.2)
// this implies that the estimated TB IR for HHC is 3.6 times higher than that of NC
// we are 95% confident that at minimum the population TB IR for HHC can be 2.1 times higher and at maximum 6.2 times higher than for NC

*generate event_tb variable
*b. probable tb
generate prob_tb=.
replace prob_tb=1 if m4_case_category==2 | m8_case_category==2 | m12_case_category==2
replace prob_tb=0 if prob_tb!=1
tabulate subject_cat prob_tb

stset fuyears, failure(prob_tb==1)
di 2/4130.204
strate, per (1000)
strate exposed_group, per(1000)

*generate event_tb variable
*c. possible tb
generate poss_tb=.
replace poss_tb=1 if m4_case_category==3 | m8_case_category==3 | m12_case_category==3
replace poss_tb=0 if poss_tb!=1
tabulate subject_cat poss_tb

stset fuyears, failure(poss_tb==1)
di 152/4107.422
strate, per (1000)
strate exposed_group, per(1000)
