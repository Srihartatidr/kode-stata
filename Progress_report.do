leftalign
tabulate eligible eligible1, m

*memeriksa konsistensi eligibility di crf1 dan crf2
tabulate eligible1 subject_cat, col
tabulate eligible subject_cat, col

*memeriksa konsistensi consent di crf1 dan crf2
tabulate consent1 subject_cat,m col
tabulate consent subject_cat, m col

*membuat tabel baseline study participants
tabulate eligible subject_cat, m col
tabulate consent subject_cat, m col

*menyeragamkan penulisan kelurahan
replace kelurahan= "Margahayu Utara" in 4706
replace kelurahan= "Margahayu Utara" in 4707
replace kelurahan= "Margahayu Utara" in 4708
replace kelurahan= "Cibaduyut" in 5692
replace kelurahan= "Cibaduyut" in 5693
replace kelurahan= "Cibaduyut" in 5694
replace kelurahan= "Margahayu Utara" in 6227
replace kelurahan= "Margahayu Utara" in 6228
replace kelurahan= "Margahayu Utara" in 6229
replace kelurahan= "Margahayu Utara" in 6230
replace kelurahan= "Sukahaji" in 7644
replace kelurahan= "Sukahaji" in 7645

*membuat hanya ada 6 puskesmas
generate puskesmas=.
replace puskesmas=1 if(kelurahan== "Babakan Ciparay" | kelurahan== "Margahayu Utara")
replace puskesmas=2 if(kelurahan== "Cibaduyut" | kelurahan== "Cibaduyut Kidul")
replace puskesmas=3 if(kelurahan== "Cibaduyut Wetan" | kelurahan== "Mekarwangi")
replace puskesmas=4 if(kelurahan== "Kopo" | kelurahan== "Sukaasih" | kelurahan== "Suka Asih")
replace puskesmas=5 if(kelurahan== "Situsaeur" | kelurahan== "Kebon Lega" | kelurahan== "Kebonlega")
replace puskesmas=6 if(kelurahan== "Sukahaji" | kelurahan== "Babakan")
replace puskesmas=7 if(kelurahan== "Cirangrang" | kelurahan== "Cakuang Kulon" | kelurahan== "Bojongloa Kaler")
codebook puskesmas
tabulate puskesmas subject_cat if consent==1, m
codebook puskesmas
tabulate puskesmas, missing
missings list puskesmas

*membuat tabel case_category per puskesmas
tabulate puskesmas subject_cat if consent==1
tabulate puskesmas case_category if subject_cat==2 & consent==1, row
tabulate puskesmas case_category if subject_cat==3 & consent==1, row

*SLIDE FOLLOW UP FLOW CHART
*menghitung jumlah yang perlu di-fu saat baseline
tabulate consent subject_cat, col
label define leftreasonlab 1 "Death" 2 "Moved" 3 "Lost to FU" 4 "Didnt wish to continue" 5 "Other"
label list
label value left_reason leftreasonlab
label define m4leftreasonlab 1 "Death" 2 "Moved" 3 "Lost to FU" 4 "Didnt wish to continue" 5 "Other"
label list
label value m4_left_reason m4leftreasonlab
label define m8leftreasonlab 1 "Death" 2 "Moved" 3 "Lost to FU" 4 "Didnt wish to continue" 5 "Other"
label list
label value m8_left_reason m8leftreasonlab
label define m12leftreasonlab 1 "Death" 2 "Moved" 3 "Lost to FU" 4 "Didnt wish to continue" 5 "Other"
label list
label value m12_left_reason m12leftreasonlab

*menghitung left_reason saat baseline
tabulate left_reason subject_cat if consent==1

*menghitung case_category
tabulate subject_cat case_category if consent==1

*6 nc didnt wish to continue saat baseline, dipindah ke fu-4
25010601 | 25010901 | 25172201 | 25172202 | 25172203 | 25172204

*mendata tanggal baseline subjek yang left
browse subject_id subject_cat date_baseline left_reason left_other if consent==1 & left==1 & subject_cat!=1
sort subject_cat date_baseline
tabulate left_reason subject_cat if left==1 & subject_cat!=1

*m4 merubah string menjadi numeric variabel m4_date2
*membuat variabel m4_done berdasarkan m4_date2 
encode m4_date, generate (m4_date2)
generate m4_done=0 if m4_date2==.
replace m4_done=1 if m4_date2!=.

*m8 merubah string menjadi numeric variabel m8_date2
*membuat variabel m8_done berdasarkan m8_date2 
encode m8_date, generate (m8_date2)
generate m8_done=0 if m8_date2==.
replace m8_done=1 if m8_date2!=.

*m12 merubah string menjadi numeric variabel m12_date2
*membuat variabel m12_done berdasarkan m12_date2 
encode m12_date, generate (m12_date2)
generate m12_done=0 if m12_date2==.
replace m12_done=1 if m12_date2!=.

*memeriksa apakah ada subjek yang tidak consent tapi di-fu m4
tabulate m4_done consent if subject_cat!=1

*jika ada, list
list subject_id subject_cat left if m4_done==0 & consent==1

*menghitung m4_done 
tabulate subject_cat m4_done if consent==1 & left==0 
tabulate subject_cat m4_left if consent==1 & left==0
// ada 2 NC missed FU-4 yaitu 25116601 dan 25142201

*melihat siapa yang belum di-fu m4
list subject_id subject_cat if consent==1 & left==0 & m4_done==0 & subject_cat==2
list subject_id subject_cat if consent==1 & left==0 & m4_done==0 & subject_cat==3

*menghitung m4_left per subject_cat
tabulate subject_cat m4_left if consent==1 & m4_done==1
tabulate m4_done m4_left_reason if consent==1 & subject_cat==2
tabulate m4_done m4_left_reason if consent==1 & subject_cat==3
list subject_id subject_cat m4_left_reason if consent==1 & m4_left==1 & m4_left_reason==5

*menghitung m4_case_category
tabulate subject_cat m4_case_category if consent==1

*memeriksa apakah ada subjek yang tidak consent tapi di-fu m8
tabulate m8_done consent
tabulate m8_done m4_done

*jika ada, list
list subject_id if subject_cat!=1 & m8_done==1 & consent==0
list subject_id if subject_cat!=1 & m4_done==0 & m8_done==1
// 2 NC yg m4_done==0 & m8_done==1, ID 25116601 dan 25142201

*menghitung m8_done secara keseluruhan
tabulate subject_cat m8_done if consent==1
// hanya lihat yang m8_done=1 untuk dimasukan ke flow chart
tabulate subject_cat m8_done if left==0
tabulate subject_cat m8_done if left==0 | m4_left==0
tabulate subject_cat m8_done if left==0 & m4_left==0
// untuk mendapatkan jumlah yang waiting for fu-8, hasil yang m8_done=0 nya kurangi dulu jumlah left study di fu-4
// ada 2 NCs yang missed FU-4 but attended FU-8
// ada 4 NCs yang missed FU-8 but attended FU-12: 25015002  25015003  25015902  25054601

*melihat siapa yang belum di-fu m8 per subject_cat
browse subject_id date_baseline m4_date if m8_done==0 & subject_cat==2 & left==0
browse subject_id date_baseline m4_date if m8_done==0 & subject_cat==3 & left==0
browse subject_id date_baseline m4_date if m8_done==0 & subject_cat==2 & m4_left==0
browse subject_id date_baseline m4_date if m8_done==0 & subject_cat==3 & m4_left==0

*menghitung m8_left
tabulate m8_done m8_left_reason if consent==1 & subject_cat==2
tabulate m8_done m8_left_reason if consent==1 & subject_cat==3

*menghitung m8_case_category
tabulate subject_cat m8_case_category if consent==1

*mendata tanggal baseline subjek yang left m8
browse subject_id subject_cat date_baseline m8_left_reason if consent==1 & m8_left==1 & subject_cat!=1
sort subject_cat date_baseline
tabulate m8_left_reason subject_cat if m8_left==1 & subject_cat!=1

*memeriksa apakah ada yang tidak consent tapi di-fu m12
tabulate m12_done consent
tabulate m12_done m4_done
tabulate m12_done m8_done

*jika ada, list
list subject_id if consent==0 & m12_done==1
list subject_id if m4_done==0 & m12_done==1
list subject_id subject_cat if m8_done==0 & m12_done==1
// ada 2 NC m4_done=0 tapi attended FU-8 dan FU-12, ID 25116601 (gapapa, sudah masuk di FU-8)
// ada 4 NC missed FU-8 but attended FU-12, ID 25015002  25015003 25015902  25054601

*menghitung m12_done secara keseluruhan
tabulate subject_cat m12_done if consent==1

*menghitung m12_not_done
tabulate subject_cat m12_done if consent==1 & left==0
tabulate subject_cat m12_done if consent==1 & m8_left==0

*melihat siapa subjek yang belum fu-12
browse subject_id date_baseline if m12_done==0 & consent==1 & m8_left==0 & subject_cat==2

*menghitung m12_left
tabulate m12_done m12_left_reason if consent==1 & subject_cat==2
tabulate m12_done m12_left_reason if consent==1 & subject_cat==3

*menghitung m12_case_category
tabulate subject_cat m12_case_category if consent==1

*mendata tanggal baseline subjek yang left m12
browse subject_id subject_cat date_baseline m12_left_reason if consent==1 & m12_left==1 & subject_cat!=1
sort subject_cat date_baseline
tabulate m12_left_reason subject_cat if m12_left==1 & subject_cat!=1
