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
label define culturelab 1 "Negative" 2 "negative contaminated" 3 "Positive" 4 "Positive contaminated" 5 "Indeterminate" 6 "No result"
label list
label value culture1_result culturelab 
label value culture2_result culturelab 

*generating new variable: age category
generate agecat=.
replace agecat=1 if(age>=10 & age<=19)
replace agecat=2 if(age>=20 & age<=29)
replace agecat=3 if(age>=30 & age<=39)
replace agecat=4 if(age>=40 & age<=49)
replace agecat=5 if(age>=50 & age<=59)
replace agecat=6 if(age>=60 & age<.)
codebook agecat

*generating new variable: ethnic group
generate ethnicgroup=.
replace ethnicgroup=0 if(ethnicity!=1) // other
replace ethnicgroup=1 if(ethnicity==1) // sundanese
label define ethniclab 0 "Other" 1 "Sundanese"
label list
label value ethnicgroup ethniclab 

*generating new variable: dm
sort immune_dis
generate dm=.
replace dm=1 if (immune_dis== "1" | immune_dis== "1,4" | immune_dis== "1,4,5" | immune_dis== "1,4,5,7,8" | immune_dis== "1,4,5,8" | immune_dis== "1,4,7" | immune_dis== "1,5" | immune_dis== "1,5,7" | immune_dis== "1,5,8" | immune_dis== "1,7" | immune_dis== "1,8")
replace dm=0 if dm!=1
codebook dm

*generating new variable: bmi category
generate bmicat=.
replace bmicat=1 if (bmi<18.5)
replace bmicat=2 if (bmi>=18.5 & bmi<=22.9)
replace bmicat=3 if (bmi>=23.0 & bmi<=24.9)
replace bmicat=4 if (bmi>=25.0 & bmi<.)

*subsetting: eligible and consented subjects
keep if eligible==1 & consent==1

*table filling (1)
tabulate sex subject_cat, col m
tabulate sex subject_cat if subject_cat!=1, col chi2

*normalitas checking for variable: age
*visually
histogram age, normal
histogram age, normal by(subject_cat)

*statistically
sktest age
summarize age, d
summarize age if subject_cat==1,d
summarize age if subject_cat==2,d
summarize age if subject_cat==3,d
ranksum age if subject_cat!=1, by (subject_cat)

*table filling (2)
tabulate agecat subject_cat, m col
tabulate agecat subject_cat if subject_cat!=1, col chi2
tabulate ethnicgroup subject_cat, m col
tabulate ethnicgroup subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate smokes subject_cat, m col
tabulate smokes subject_cat if subject_cat==2 | subject_cat==3, col chi
tabulate bcg_scar subject_cat, m col
tabulate bcg_scar subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate tb_diag subject_cat, m col
tabulate tb_diag subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate hiv_diag subject_cat, m col
tabulate hiv_diag subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate dm subject_cat, m col
tabulate dm subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate coughing subject_cat, m col
tabulate coughing subject_cat if subject_cat==2 | subject_cat==3, col chi2

*identifying outlier on cough_dur variable
gen id=_n
graph box cough_dur, mark(1,mlabel(id))

*normalitas checking for variable: cough_dur
*visually
histogram cough_dur if coughing==1, bin (30) normal
histogram cough_dur if coughing==1, normal by(subject_cat)
qnorm cough_dur if coughing==1

*statistically
sktest cough_dur if coughing==1
summarize cough_dur if coughing==1,d
summarize cough_dur if subject_cat==1 & coughing==1,d
summarize cough_dur if subject_cat==2 & coughing==1,d
summarize cough_dur if subject_cat==3 & coughing==1,d
ranksum cough_dur if coughing==1 & subject_cat!=1, by(subject_cat)

*table filling (3)
tabulate cough_phlegm subject_cat, m col
tabulate cough_phlegm subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate cough_night subject_cat, m col
tabulate cough_night subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate cough_blood subject_cat, m col
tabulate cough_blood subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate chest_pain subject_cat, m col
tabulate chest_pain subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate breathless subject_cat, m col
tabulate breathless subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate fever subject_cat, m col
tabulate fever subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate nightsweat subject_cat, m col
tabulate nightsweat subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate weightloss subject_cat, m col
tabulate weightloss subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate appetite_loss subject_cat, m col
tabulate appetite_loss subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate dizziness subject_cat, m col
tabulate dizziness subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate fatigue subject_cat, m col
tabulate fatigue subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate bmicat subject_cat, m col
tabulate bmicat subject_cat if subject_cat==2 | subject_cat==3, col chi2
tabulate cxr_result subject_cat, m col
tabulate cxr_result subject_cat if subject_cat==2 & cxr_result!=4 | subject_cat==3 & cxr_result!=4, col chi2
// 11 kasus indeks cxr_result nya missing karena tidak dilakukan, digabung ke not done.
// ID berikut:

subject_id	
	
146.  25005001	
1041.  25005201	
1078.  25005401	
1161.  25004901	
2612.  25001001	
2788.  25005301	
3257.  25006901	
3542.  25001201	
3749.  25001101	
3972.  25007801	
4001.  25000701

*generating new variable: smear result final
gsort smear1_result smear2_result
generate smear_fin=.
browse subject_id smear1_result smear2_result case_category m4_smear1_result m4_smear2_result m4_case_category m8_smear1_result m8_smear2_result m8_case_category m12_smear1_result m12_smear2_result m12_case_category smear_fin
//Negative
replace smear_fin=0 if smear1_result==0 & smear2_result==0
replace smear_fin=0 if smear1_result==0 & smear2_result==5
replace smear_fin=0 if smear1_result==5 & smear2_result==0
replace smear_fin=0 if smear1_result==0 & smear2_result==.
//Scanty
replace smear_fin=1 if smear1_result==0 & smear2_result==1
replace smear_fin=1 if smear1_result==1 & smear2_result==0
replace smear_fin=1 if smear1_result==1 & smear2_result==1
replace smear_fin=1 if smear1_result==5 & smear2_result==1 
//Positive 1
replace smear_fin=2 if smear1_result==0 & smear2_result==2
replace smear_fin=2 if smear1_result==2 & smear2_result==0
replace smear_fin=2 if smear1_result==1 & smear2_result==2
replace smear_fin=2 if smear1_result==2 & smear2_result==1
replace smear_fin=2 if smear1_result==2 & smear2_result==2
replace smear_fin=2 if smear1_result==2 & smear2_result==5
replace smear_fin=2 if smear1_result==2 & smear2_result==.
//Positive 2
replace smear_fin=3 if smear1_result==0 & smear2_result==3
replace smear_fin=3 if smear1_result==1 & smear2_result==3
replace smear_fin=3 if smear1_result==2 & smear2_result==3
replace smear_fin=3 if smear1_result==3 & smear2_result==0
replace smear_fin=3 if smear1_result==3 & smear2_result==1
replace smear_fin=3 if smear1_result==3 & smear2_result==2
replace smear_fin=3 if smear1_result==3 & smear2_result==3 
//Positive 3
replace smear_fin=4 if smear1_result==0 & smear2_result==4
replace smear_fin=4 if smear1_result==2 & smear2_result==4
replace smear_fin=4 if smear1_result==3 & smear2_result==4
replace smear_fin=4 if smear1_result==4 & smear2_result==0
replace smear_fin=4 if smear1_result==4 & smear2_result==1
replace smear_fin=4 if smear1_result==4 & smear2_result==2
replace smear_fin=4 if smear1_result==4 & smear2_result==3
replace smear_fin=4 if smear1_result==4 & smear2_result==4    
//No result (sudah terakomodasi di atas semua)
replace smear_fin=5 if
codebook smear_fin

*generating new variable: culture result final
generate culture_fin=.
sort culture1_result culture2_result
browse smear1_result smear2_result smear_fin culture1_result culture2_result culture_fin
label define culfinlab 0 "Negative" 1 "Positive" 2 "Indeterminate"
label list
label value culture_fin culfinlab 

//Negative
replace culture_fin=0 if culture1_result==1
replace culture_fin=0 if culture1_result==1 & culture2_result==1
replace culture_fin=0 if culture1_result==2
//Positive
replace culture_fin=1 if culture1_result==3
replace culture_fin=1 if culture1_result==3 & culture2_result==3
replace culture_fin=1 if culture1_result==4
//Indeterminate
replace culture_fin=2 if culture1_result==5

*table filling (4)
tabulate smear_fin subject_cat if smear_fin!=., m col
tabulate smear_fin subject_cat if subject_cat==2 & smear_fin!=. | subject_cat==3 & smear_fin!=., col chi2
tabulate xpert_result subject_cat if xpert_result!=5, m col
tabulate xpert_result subject_cat if subject_cat==2 & xpert_result!=5 | subject_cat==3 & xpert_result!=5, col chi2
tabulate culture_fin subject_cat if culture_fin!=., m col
tabulate culture_fin subject_cat if subject_cat==2 & culture_fin!=. | subject_cat==3 & culture_fin!=., col chi2
