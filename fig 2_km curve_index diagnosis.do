keep if eligible ==1 & consent==1

*convertimg string date to integer date
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

*generating new variable --> index_id (grouping based on index_id)
gen akhir = "01"
gen houseid_str = string(house_id)
gen indexid = houseid_str + akhir
replace indexid = neighbour_subject_id if subject_cat==3

*date manipulating
*converting string date to integer date
gen tbdiagdate_i=date(tbdiag_date, "YMD")
format tbdiagdate_i %td

*generating index_diagdate based on index's diagnosis date
gen index_diagdate=.	
format index_diagdate %td
replace index_diagdate = tbdiagdate_i if subject_cat==1

*copying values into group of indexid
sort indexid subject_cat subject_id
browse subject_id subject_cat indexid tbdiag_date tbdiagdate_i index_diagdate date_basei day_in lagtime_days
bysort indexid: replace index_diagdate = index_diagdate[1]
codebook index_diagdate

///////// DAY IN ///////////////////
*generating day_in variable
generate day_in=index_diagdate
format day_in %td

///////// duration from day in to baseline date //////// Sue's request
generate lagtime_days=date_basei-day_in
summarize lagtime_days if subject_cat==2, d
summarize lagtime_days if subject_cat==3, d
sort indexid subject_cat subject_id lagtime_days
sort subject_cat lagtime_days

///////// DAY OUT //////////////////
*generating day_out variable
generate day_out=.
format day_out %td
browse subject_id subject_cat date_baseline indexid tbdiag_date tbdiagdate_i index_diagdate day_in date_basei date_lefti m4_datei m4_date_lefti m8_datei m8_date_lefti m12_datei m12_date_lefti day_out
sort indexid subject_cat date_lefti m4_date_lefti m8_date_lefti m12_date_lefti
replace day_out=date_lefti if m4_datei==. & m8_datei==. & m12_datei==.
replace day_out=m4_date_lefti if date_lefti==. & m4_datei!=. & m8_datei==. & m8_date_lefti==. & m12_datei==. & m12_date_lefti==.
replace day_out=m8_date_lefti if date_lefti==. & m4_date_lefti==. & m12_date_lefti==.
replace day_out=m8_date_lefti if date_lefti==. & m4_date_lefti==. & m12_datei==. & m12_date_lefti==.
replace day_out=m12_date_lefti if m8_date_lefti==. & m4_date_lefti==. & date_lefti==. 
replace day_out=m12_datei if date_lefti==. & m4_date_lefti==. & m8_date_lefti==. & m12_date_lefti==.

gsort day_out date_basei

*start with subset with complete day_out
drop if day_out==.

*generate follow-up duration variables
generate fudays=day_out-day_in
generate fuweeks=fudays/7
generate fumonths=fudays/30
generate fuyears=(day_out-day_in)/365.25
sort fumonths

*generate event_tb variable
generate event_tb=.
replace event_tb=1 if case_category==1 | case_category==2 | m4_case_category==1 | m4_case_category==2 | m8_case_category==1 | m8_case_category==2 | m12_case_category==1 | m12_case_category==2
replace event_tb=0 if event_tb!=1
tabulate subject_cat event_tb

*generate event_death variable
generate event_death=.
replace event_death=1 if left_reason==1 | m4_left_reason==1 | m8_left_reason==1 | m12_left_reason==1
replace event_death=0 if event_death!=1
tabulate subject_cat event_death if consent==1

*create histogram based on fu duration
hist fumonths if subject_cat==2, freq addlabels xlabel(0(1)16)
hist fumonths if subject_cat==3, freq addlabels xlabel(0(1)16)

*generate new variable: unexposed group(1)=nc(3) and exposed group(2)=hhc(2)
generate exposed_group=.
replace exposed_group=2 if subject_cat==2
replace exposed_group=1 if subject_cat==3

browse subject_id event_tb event_death date_basei date_lefti m4_datei m4_date_lefti m8_datei m8_date_lefti m12_datei m12_date_lefti day_in day_out
sort day_in day_out

//// DECLARE THE DATA AS A SURVIVAL_TIME DATA ////
*a. use stset command based on fumonths
*basic stset command (with available length of FU duration)
stset fumonths, failure(event_tb==1)

*complete stset command (when we have not calculated the length of FU duration)
stset day_out, fail(event_tb==1) origin(day_in) enter(day_in) id(subject_id) scale(30)

*calculate TB incidence rate
di 62/34123
// the TB incidence rate was 0.00181 cases per person-years, that is 181 in every 100000 people will experience TB in a year.

*obtain summary statistics, median fu time
stdes
// the median FU time was 12 months

*estimate TB IR and 95% CI
*(i) per person-years
strate
*(ii) per 100000 person-years
strate, per (100000)
// the TB IR was 181 cases per 100000 person-years
// we are 95% confident that the population TB IR would at minimum be 141 and at maximum 233 cases in every 100000 persons per year.

*estimate TB IR based on group
strate exposed_group, per(100000)
// TB IR was estimated as 133 and 478 cases per 100000 person-years, for the NC and HHC, respectively
// we are 95% confident that the population TB IR for NC lies between 97 and 182 cases per 100000 person-years
// for HHC the population TB IR lies between 318 and 720 cases per 100000 person-years

*compare TB IR among groups
stir exposed_group
// the estimated rate ratio was 3.6 with the 95% CI for the population TB IRR as (2.1, 6.2)
// this implies that the estimated TB IR for HHC is 3.6 times higher than that of NC
// we are 95% confident that at minimum the population TB IR for HHC can be 2.1 times higher and at maximum 6.2 times higher than for NC

*stratify rate ratios (trial)
stmh exposed_group smokes

*make a kaplan meier (km) summary table
sts list if exposed_group==2, at (1(1)16)
sts list if exposed_group==1, at (1(1)16)

*make a km curve and 95% CI for each group
sts graph if exposed_group==1, gwood
sts graph if exposed_group==2, gwood

*compare km curves based on fumonths
sts graph, by(exposed_group) xlabel (0(2)16) xtitle("Months") ylab(0 "0" .25 "25" .5 "50" .75 "75" 1 "100") ytitle("Percentage") legend(label(1 "Neighbourhood Contacts") label(2 "Household Contacts")) risktable(, order(1 "Neighbourhood Contacts" 2 "Household Contacts")) text(2 30 "Log-rank p-value<0.001", size(small))
sts test exposed_group, logrank
stmc exposed_group

*drop observation with fumonths more than 13 months
sort fumonths
drop if subject_id==25064704 | subject_id==25002602 | subject_id==25036401

*another approach
*b. use stset function based on fudays
stset fudays, failure(event_tb==1) scale(1)

*compare two groups based on fudays
sts graph, by(exposed_group) xlabel (0(60)420) xtitle("Days") ylab(0 "0" .25 "25" .5 "50" .75 "75" 1 "100") ytitle("Percentage") legend(label(1 "Neighbourhood Contacts") label(2 "Household Contacts")) risktable(, order(1 "Neighbourhood Contacts" 2 "Household Contacts")) text(2 30 "Log-rank p-value<0.001", size(small))
sts test exposed_group, logrank

*how to replace date -if there are any wrong date-
replace tbdiagdate_i= td(20jul2022) in 3124
replace index_diagdate= td(20jul2022) in 727
replace day_in= td(20jul2022) in 727
