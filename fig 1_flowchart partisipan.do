*calculating eligible/identified subject
tabulate eligible subject_cat, m col

*reason for not eligible (hhc) based on age
generate age_cat_eli=.
replace age_cat_eli=1 if (age>=10)
replace age_cat_eli=0 if (age<10)
tabulate eligible age_cat_eli if subject_cat==2
list subject_id if eligible==0 & age_cat_eli==1
list subject_id refuse_reason if eligible==0 & age_cat_eli==1 & subject_cat==2
// terdapat 6 HHC yang eligible dari segi umur tapi tidak dianggap tidak eligible
// yaitu 5 kontak serumah index 25000601 (Pa Wasri)
// dan 1 kontak serumah index 25013701 (Hara)
Tidak dimasukkan ke dalam flowchart

*reason for not eligible (nc) based on age
tabulate eligible age_cat_eli if subject_cat==3
// 969 tidak eligible karena umur < 10 tahun
list subject_id if eligible==0 & age_cat_eli==1 & subject_cat==3
// ada 3 NC yang eligible dari segi umur, tp tidak eligible, yaitu
// 2 subjek not normally living in the hosuehold: id 25163102 cimahi, 25165703 pesantren
// 1 subjek tidak dianggap subjek karena hasil jemput bola (id 25083201), bta dan kultur negatif

*reason if not screened
tabulate consent subject_cat, m col
browse subject_id refuse_reason if consent==0 & subject_cat==2
leftalign
browse subject_id refuse_reason if consent==0 & subject_cat==3
leftalign

*generating new variable: tb positive or tb negative
generate tb_case=.
replace tb_case=1 if case_category==1 | case_category==2
replace tb_case=0 if case_category==0 | case_category==3 | case_category==4
codebook tb_case
tabulate subject_cat tb_case if consent==1
