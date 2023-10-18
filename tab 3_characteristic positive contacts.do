leftalign

*labelling
label define sexlab 0 "Female" 1 "Male"
label list
label value sex sexlab
label define smokinglab 1 "Current smoker" 2 "Ex-smoker" 3 "Never smoked"
label list
label value smokes smokinglab
label define cxrlab 0 "Normal" 1 "Suggestive active TB" 2 "Suggestive inactive TB" 3 "Abnormal not TB" 4 "Refused chest x-ray"
label list
label value cxr_result cxrlab

label define bcglab 0 "No" 1 "Yes" 2 "Dont know"
label list
label value bcg_scar bcglab
label define tbdiaglab 0 "No" 1 "Yes" 2 "Dont know"
label list
label value tb_diag tbdiaglab
label define hivdiaglab 0 "No" 1 "Yes" 2 "Dont know"
label list
label value hiv_diag hivdiaglab
label define xpertlab 0 "Mtb not detected" 1 "Mtb detected, Rif sen" 2 "Mtb detected, Rif res" 3 "Indeterminate" 4 "Error" 5 "Not done"
label list
label value xpert_result xpertlab 
label define smearlab 0 "Negative" 1 "Scanty" 2 "Positive 1" 3 "Positive 2" 4 "Positive 3" 5 "No result"
label list
label value smear_fin smearlab
label define culturelab 1 "Negative" 2 "Negative contaminated" 3 "Positive" 4 "Positive contaminated" 5 "Indeterminate" 6 "No result"
label list
label value culture1_result culturelab 
label value culture2_result culturelab
label value m4_culture1_result culturelab 
label value m4_culture2_result culturelab
label value m8_culture1_result culturelab 
label value m8_culture2_result culturelab 
label value m12_culture1_result culturelab 
label value m12_culture2_result culturelab

*generating new variable: bmi category
generate bmicat=.
replace bmicat=1 if (bmi<18.5)
replace bmicat=2 if (bmi>=18.5 & bmi<=22.9)
replace bmicat=3 if (bmi>=23.0 & bmi<=24.9)
replace bmicat=4 if (bmi>=25.0 & bmi<.)

*subsetting: keep definite and probable tb from incident and prevalent cases
generate tb_case=.
replace tb_case=1 if (case_category==1 | case_category==2 | m4_case_category==1 | m4_case_category==2 | m8_case_category==1 | m8_case_category==2 | m12_case_category==1 | m12_case_category==2)
replace tb_case=0 if tb_case!=1
keep if eligible==1 & consent==1
tab subject_cat tb_case
keep if tb_case==1
keep if subject_cat!=1

*check case category per follow-up
tab subject_cat case_category
tab subject_cat m4_case_category
tab subject_cat m8_case_category
tab subject_cat m12_case_category
list subject_id if (m4_case_category==1 | m4_case_category==2 | m8_case_category==1 | m8_case_category==2 | m12_case_category==1 | m12_case_category==2)
list subject_id m4_case_category if (m4_case_category==1 | m4_case_category==2)

*table filling (1)
*normalitas checking for variable: age
*visually
histogram age, normal
histogram age, normal by(subject_cat)
*statistically
sktest age
summarize age, d
summarize age if subject_cat==2,d
summarize age if subject_cat==3,d
ranksum age, by (subject_cat)

tabulate sex subject_cat, m col chi2
tabulate smokes subject_cat, m col chi2
tabulate bcg_scar subject_cat, m col chi2
tabulate coughing subject_cat, m col chi2
	histogram cough_dur if coughing==1, normal by(subject_cat)
	sktest cough_dur if coughing==1
	summarize cough_dur if subject_cat==2 & coughing==1,d
	summarize cough_dur if subject_cat==3 & coughing==1,d
	ranksum cough_dur if coughing==1 & subject_cat!=1, by(subject_cat)
tabulate cough_blood subject_cat, m col chi2
tabulate chest_pain subject_cat, m col chi2
tabulate breathless subject_cat, m col chi2
tabulate fever subject_cat, m col chi2
tabulate weightloss subject_cat, m col chi2
tabulate fatigue subject_cat, m col chi2
tabulate bmicat subject_cat, m col chi2
tabulate cxr_result subject_cat, m col chi2


*generating new variable: smear result final
gsort smear1_result smear2_result 
generate smear_fin=.
browse subject_id smear1_result smear2_result case_category m4_smear1_result m4_smear2_result m4_case_category m8_smear1_result m8_smear2_result m8_case_category m12_smear1_result m12_smear2_result m12_case_category smear_fin
gsort case_category m4_case_category m8_case_category m12_case_category
// Negative 
replace smear_fin=0 if smear1_result==0 & smear2_result==0 & case_category==1
replace smear_fin=0 if smear1_result==0 & smear2_result==5 & case_category==1
replace smear_fin=0 if smear1_result==0 & smear2_result==. & case_category==1
// Scanty
replace smear_fin=1 if smear1_result==0 & smear2_result==1 & case_category==1
replace smear_fin=1 if smear1_result==1 & smear2_result==1 & case_category==1
replace smear_fin=1 if smear1_result==5 & smear2_result==1 & case_category==1
// Positive 1
replace smear_fin=2 if smear1_result==0 & smear2_result==2 & case_category==1
replace smear_fin=2 if smear1_result==2 & smear2_result==0 & case_category==1
replace smear_fin=2 if smear1_result==2 & smear2_result==1 & case_category==1
// Positive 2
replace smear_fin=3 if smear1_result==2 & smear2_result==3 & case_category==1
replace smear_fin=3 if smear1_result==3 & smear2_result==2 & case_category==1
replace smear_fin=3 if smear1_result==3 & smear2_result==3 & case_category==1
// Positive 3
replace smear_fin=4 if smear1_result==0 & smear2_result==4 & case_category==1
replace smear_fin=4 if smear1_result==4 & smear2_result==0 & case_category==1
replace smear_fin=4 if smear1_result==4 & smear2_result==3 & case_category==1
replace smear_fin=4 if smear1_result==4 & smear2_result==4 & case_category==1
// Missing
replace smear_fin=. if smear1_result==. & smear2_result==. & case_category==1

*check m4
gsort m4_case_category
replace smear_fin=0 if m4_smear1_result==0 & m4_smear2_result==0 & m4_case_category==1
replace smear_fin=. if m4_smear1_result==. & m4_smear2_result==. & m4_case_category==1

*check m8
gsort m8_case_category
replace smear_fin=. if m8_smear1_result==. & m8_smear2_result==. & m8_case_category==1
replace smear_fin=0 if m8_smear1_result==0 & m8_smear2_result==0 & m8_case_category==1
replace smear_fin=1 if m8_smear1_result==0 & m8_smear2_result==1 & m8_case_category==1

*check m12
gsort m12_case_category
replace smear_fin=0 if m12_smear1_result==0 & m12_smear2_result==0 & m12_case_category==1
replace smear_fin=. if m12_smear1_result==. & m12_smear2_result==. & m12_case_category==1

*generating new variable: culture result final
generate culture_fin=.
browse subject_id smear1_result smear2_result culture1_result culture2_result case_category m4_smear2_result m4_smear2_result m4_culture1_result m4_culture2_result m4_case_category m8_smear2_result m8_smear2_result m8_culture1_result m8_culture2_result m8_case_category m12_smear1_result m12_smear2_result m12_smear2_result m12_culture1_result m12_culture2_result m12_case_category culture_fin
browse subject_id culture1_result culture2_result case_category m4_culture1_result m4_culture2_result m4_case_category m8_culture1_result m8_culture2_result m8_case_category m12_culture1_result m12_culture2_result m12_case_category culture_fin
sort culture1_result culture2_result
label define culfinlab 0 "Negative" 1 "Positive" 2 "Indeterminate"
label list
label value culture_fin culfinlab 
// Negative
replace culture_fin=0 if culture1_result==1 & culture2_result==. & case_category==1
// Positive
replace culture_fin=1 if culture1_result==3 & culture2_result==. & case_category==1
replace culture_fin=1 if culture1_result==3 & culture2_result==3 & case_category==1
// ..
replace culture_fin=. if culture1_result==. & culture2_result==. & case_category==1

sort m4_case_category
replace culture_fin=0 if m4_culture1_result==1 & m4_culture2_result==. & m4_case_category==1
replace culture_fin=1 if m4_culture1_result==3 & m4_culture2_result==. & m4_case_category==1
replace culture_fin=. if m4_culture1_result==. & m4_culture2_result==. & m4_case_category==1

sort m8_case_category
replace culture_fin=0 if m8_culture1_result==1 & m8_culture2_result==. & m8_case_category==1
replace culture_fin=1 if m8_culture1_result==3 & m8_culture2_result==. & m8_case_category==1
replace culture_fin=. if m8_culture1_result==. & m8_culture2_result==. & m8_case_category==1

sort m12_case_category
replace culture_fin=0 if m12_culture1_result==1 & m12_culture2_result==. & m12_case_category==1
replace culture_fin=1 if m12_culture1_result==3 & m12_culture2_result==. & m12_case_category==1
replace culture_fin=. if m12_culture1_result==. & m12_culture2_result==. & m12_case_category==1

tabulate smear_fin subject_cat if smear_fin!=., m col chi2
tabulate smear_fin subject_cat, m col chi2
tabulate xpert_result subject_cat if xpert_result!=5, m col chi2
tabulate xpert_result subject_cat, m col chi2
tabulate culture_fin subject_cat if culture_fin!=., m col chi2
tabulate culture_fin subject_cat, m col chi2
