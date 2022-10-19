** Open directory **
set more off 
cd "/home/eugenio/Dropbox/data media&coordination/cleaneddata"

clear 
import excel "MEG.xlsx", sheet("Foglio1") firstrow

generate sessionID = session*100000
generate subjectID = session*100000 + subject
generate teamID = session*100000 + team  

generate round = 0
replace round = 1 if stage==1&period==1
replace round = 2 if stage==1&period==2
replace round = 3 if stage==1&period==3
replace round = 4 if stage==1&period==4
replace round = 5 if stage==1&period==5
replace round = 6 if stage==1&period==6
replace round = 7 if stage==1&period==7
replace round = 8 if stage==1&period==8
replace round = 9 if stage==1&period==9
replace round = 10 if stage==1&period==10
replace round = 11 if stage==2&period==1
replace round = 12 if stage==2&period==2
replace round = 13 if stage==2&period==3
replace round = 14 if stage==2&period==4
replace round = 15 if stage==2&period==5
replace round = 16 if stage==2&period==6
replace round = 17 if stage==2&period==7
replace round = 18 if stage==2&period==8
replace round = 19 if stage==2&period==9
replace round = 20 if stage==2&period==10
replace round = 21 if stage==3&period==1
replace round = 22 if stage==3&period==2
replace round = 23 if stage==3&period==3
replace round = 24 if stage==3&period==4
replace round = 25 if stage==3&period==5
replace round = 26 if stage==3&period==6
replace round = 27 if stage==3&period==7
replace round = 28 if stage==3&period==8
replace round = 29 if stage==3&period==9
replace round = 30 if stage==3&period==10

generate t = 0
replace t = 1 if treatment==2&order==2
replace t = 3 if treatment==1&order==2
replace t = 2 if treatment==2&order==1
replace t = 4 if treatment==1&order==1

generate t2 = 0
replace t2 = 1 if treatment==2&order==2&change==0
replace t2 = 2 if treatment==1&order==2&change==0
replace t2 = 3 if treatment==2&order==1&change==0
replace t2 = 4 if treatment==1&order==1&change==0
replace t2 = 5 if treatment==2&order==2&change==1
replace t2 = 6 if treatment==1&order==2&change==1
replace t2 = 7 if treatment==2&order==1&change==1
replace t2 = 8 if treatment==1&order==1&change==1
label define treat 0 "baseline" 1 "go/gp" 2 "bo/bp" 3 "gp/go" 4 "bp/bo" 5 "go/bo" 6 "bo/go" 7 "gp/bp" 8 "bp/gp" 
label values t2 treat

save rawdata.dta, replace

clear
import excel DemographicsMEG.xlsx, sheet("Foglio1") firstrow

replace lot2i = 4 - lot2i
replace lot4i = 4 - lot4i
replace lot5i = 4 - lot5i

generate lot = 0
replace lot = lot1 + lot2i + lot3 + lot4i + lot5i + lot6 

drop lot1 lot2i lot3 lot4i lot5i lot6

generate d_lot = 0
egen averagelot = mean(lot)
replace d_lot = 1 if lot>12

generate sessionID = session*100000
generate subjectID = session*100000 + subject

save demographics.dta, replace

clear
use rawdata.dta

**Merge ET data**
merge m:m subjectID using demographics.dta

drop _merge

generate female = 0
replace female = 1 if gender=="Female"

save data.dta, replace

*************************LET'S START THE ANALYSIS******************************
************************* behaviour in the Baseline *******************************

clear
use data.dta


collapse (mean) number smallestnumber news d_lot lot age female economics newsreport t t2 if treatment==0, by(session stage period round teamID subjectID)  

xtset subjectID round

ranksum number if round==10|round==11, by(round)

generate dev = l.number - l.smallestnumber
generate restart = 0
replace restart = 1 if round==11
replace restart = 2 if round==21

eststo clear
eststo: meoprobit number l.number age female lot newsreports economics i.round if round<=10 || session: || subjectID:, vce(cluster session)
eststo: oprobit number age female lot newsreports economics if round==1, vce(cluster session)
esttab using base.rtf, replace p r2 label  star(* 0.1 ** 0.05 *** 0.01)
esttab using base.tex, replace se compress booktabs title("Baseline - Random panel regressions") label star(* 0.1 ** 0.05 *** 0.01)
outreg2 [*] using base, replace excel


**** ranksum test on the first round ***

clear
use data.dta

collapse (mean) number if stage==1&period==1, by(t treatment order subjectID)

tabstat number, by(t) statistics(N mean sd)

ranksum number if (treatment==1&order==2)|(treatment==1&order==1), by(order)
ranksum number if (treatment==2&order==2)|(treatment==2&order==1), by(order)
ranksum number if (treatment==1&order==2)|(treatment==2&order==2), by(treatment)
ranksum number if (treatment==1&order==1)|(treatment==2&order==1), by(treatment)
ranksum number if (treatment==1&order==2)|(treatment==2&order==1), by(treatment)
ranksum number if (treatment==1&order==1)|(treatment==2&order==2), by(treatment)
ranksum number if (treatment==1&order==2)|(treatment==0), by(treatment)
ranksum number if (treatment==1&order==1)|(treatment==0), by(treatment)
ranksum number if (treatment==2&order==2)|(treatment==0), by(treatment)
ranksum number if (treatment==2&order==1)|(treatment==0), by(treatment)
ranksum number if order==1|order==2, by(order)
ranksum number if treatment!=0, by(treatment)

clear
use data.dta

collapse (mean) smallestnumber if stage==1&period==1, by(treatment order teamID)

ranksum smallestnumber if (treatment==1&order==2)|(treatment==1&order==1), by(order)
ranksum smallestnumber if (treatment==2&order==2)|(treatment==2&order==1), by(order)
ranksum smallestnumber if (treatment==1&order==2)|(treatment==2&order==2), by(treatment)
ranksum smallestnumber if (treatment==1&order==1)|(treatment==2&order==1), by(treatment)
ranksum smallestnumber if (treatment==1&order==2)|(treatment==2&order==1), by(treatment)
ranksum smallestnumber if (treatment==1&order==1)|(treatment==2&order==2), by(treatment)
ranksum smallestnumber if (treatment==1&order==2)|(treatment==0), by(treatment)
ranksum smallestnumber if (treatment==1&order==1)|(treatment==0), by(treatment)
ranksum smallestnumber if (treatment==2&order==2)|(treatment==0), by(treatment)
ranksum smallestnumber if (treatment==2&order==1)|(treatment==0), by(treatment)
ranksum smallestnumber if order==1|order==2, by(order)
ranksum smallestnumber if treatment!=0, by(treatment)


**** graph on 1st round

clear
use data.dta

keep if round==1

cibar number, over(t) graphopts(ytitle(Effort) legend(order(1 "Baseline" 2 "GO" 3 "GP" 4 "BO" 5 "BP"))) 
graph export number.png, replace width(8000)

collapse (mean) smallestnumber t t2, by(session treatment order stage change period round teamID)  

cibar smallestnumber, over(t) graphopts(ytitle(Minimum effort) legend(order(1 "Baseline" 2 "GO" 3 "GP" 4 "BO" 5 "BP"))) 
graph export smallest_number.png, replace width(8000)

************* other graphs 

clear
use data.dta
gen dev = number - smallestnumber
collapse dev if round<11, by(t round)
graph twoway (line dev round if t==0) (line dev round if t==1) (line dev round if t==2) (line dev round if t==3) (line dev round if t==4), xlabel(1(1)10) xtitle(Round) ytitle(Effort - Minimum effort) legend(order(1 "Baseline" 2 "GO" 3 "GP" 4 "BO" 5 "BP"))
graph export "convergence.png", replace width(10000)

clear
use data.dta
collapse number if round<11, by(t round)
graph twoway (line number round if t==0) (line number round if t==1) (line number round if t==2) (line number round if t==3) (line number round if t==4), xlabel(1(1)10) xtitle(Round) ytitle(Effort) legend(order(1 "Baseline" 2 "GO" 3 "GP" 4 "BO" 5 "BP") )
graph export "numbers.png", replace  width(10000)

clear
use data.dta
collapse smallestnumber if round<11, by(t round)
graph twoway (line smallestnumber round if t==0) (line smallestnumber round if t==1) (line smallestnumber round if t==2) (line smallestnumber round if t==3) (line smallestnumber round if t==4), xlabel(1(1)10) xtitle(Round) ytitle(Minimum effort) legend(order(1 "Baseline" 2 "GO" 3 "GP" 4 "BO" 5 "BP") )
graph export "smallestnumbers.png", replace  width(10000)

**************** final regressions **********

* 1st round

clear
use data.dta

bysort session: egen dd = count(subjectID)
generate countsubject = dd/30

collapse (mean) number smallestnumber news d_lot lot age female economics newsreport t t2, by(session treatment order stage change period round teamID subjectID)  

xtset subjectID round

oprobit number i.t if round==1, vce(cluster session)
estimates store A
oprobit number i.t age female lot newsreports economics if round==1, vce(cluster session)
estimates store B
oprobit number i.t if round==1&treatment!=0, vce(cluster session)
estimates store C
oprobit number i.t age female lot newsreports economics if round==1&treatment!=0, vce(cluster session)
estimates store D

collapse (mean) smallestnumber countsubject t t2, by(session treatment order stage change period round teamID)  

oprobit smallestnumber i.t if round==1, vce(cluster session)
estimates store E
oprobit smallestnumber i.t if round==1&treatment!=0, vce(cluster session)
estimates store F

label define tr 1 "GO" 2 "GP" 3 "BO" 4 "BP"
label values t tr

coefplot (A, label(Effort - No individual controls)) (B, label(Effort - With individual controls)) (E, label(Minimum effort)), bylabel(Benchmark: baseline) || (C, label(Effort - No individual controls)) (D, label(Effort - With individual controls)) (F, label(Minimum effort)), bylabel(Benchmark: GO)||, drop(age female lot newsreports economics) xline(0) xlabel(-1(0.5)2)
graph export "regressions1stround.png", replace width(9000) 


* all rounds

clear
use data.dta

bysort session: egen dd = count(subjectID)
generate countsubject = dd/30

collapse (mean) number smallestnumber news d_lot lot age female economics newsreport t t2, by(session treatment order stage change period round teamID subjectID)  

xtset subjectID round

meoprobit number i.t if round<11|| session: ||subjectID:, vce(cluster session)
estimates store A
meoprobit number i.t age female lot newsreports economics if round<11|| session: ||subjectID:, vce(cluster session)
estimates store B
meoprobit number i.t if round<11&treatment!=0|| session: ||subjectID:, vce(cluster session)
estimates store C
meoprobit number i.t age female lot newsreports economics if round<11&treatment!=0|| session: ||subjectID:, vce(cluster session)
estimates store D

collapse (mean) smallestnumber countsubject t t2, by(session treatment order stage change period round teamID)  

meoprobit smallestnumber i.t if round<11|| session: , vce(cluster session)
estimates store E
meoprobit smallestnumber i.t if round<11&treatment!=0|| session: , vce(cluster session)
estimates store F

label define tr 1 "GO" 2 "GP" 3 "BO" 4 "BP"
label values t tr

coefplot (A, label(Effort - No individual controls)) (B, label(Effort - With individual controls)) (E, label(Minimum effort)), bylabel(Benchmark: baseline) || (C, label(Effort - No individual controls)) (D, label(Effort - With individual controls)) (F, label(Minimum effort)), bylabel(Benchmark: GO)||, drop(age female lot newsreports economics) xline(0) xlabel(-1(0.5)2)
graph export "regressionsallrounds.png", replace width(9000) 


************* power calculations ************

power twomeans 1.25 (1.4 1.5 1.6 1.7 1.8), sd(0.18) power(0.8 0.9) graph
graph export power_calculations.png, replace width(8000) height(5000)

*********** distribution ********************

clear
cd "/home/eugenio/Dropbox/data media&coordination/cleaneddata"
use data.dta

label define treatments 0 "Baseline" 1 "GO" 2 "GP" 3 "BO" 4 "BP"
label values t treatments
label variable t "Treatments"
hist number if round==1, by(t) xlabel(1(1)7) subtitle(, ring(0) pos(1) nobexpand) percent ylabel(0(10)60)
graph export "distribution.png", replace width(8000) height(5000)

************ LOT **************

label variable lot "LOT"
drop if t==0
graph twoway scatter number lot if round==1 || lfit number lot if round==1, by(t) subtitle(, pos(1) nobexpand) legend(off) ylabel(1(1)7) xlabel(0(6)24)
graph export "scatterbylot.png", replace width(8000) height(5000)



