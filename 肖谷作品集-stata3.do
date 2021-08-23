preserve
xtset code year

tab Indid,gen(indus)
gen did = treat*period
local cons "L.Board L.top10 L.Volati L.Lev L.Cash2Asset L.ROA L.Growth L.Age L.SOE L.Report L.Size"
rename Toverd1 Toverd1_
winsor Toverd1_,gen(Toverd1) p(0.05)
rename Toverd2 Toverd2_
winsor Toverd2_,gen(Toverd2) p(0.05)
//描述性
//logout,save(res/des) word replace:	///
//tabstat Tovery1 Tovery2 `cons' ,s(mean p50 sd min max N) c(s)

//相关性
asdoc pwcorr Tovery1 Tovery2 did `cons',star(0.05)

//主回归
foreach dep of varlist Toverd1 Toverd2{
reghdfe `dep' did,absorb(code year)
esti store reg1_`dep'

reghdfe `dep' did `cons',absorb(code year)
esti store reg2_`dep'

reghdfe `dep' did ,absorb(code year)
esti store reg3_`dep'
}
esttab reg1_Toverd1 reg2_Toverd1 reg3_Toverd1 reg1_Toverd2 reg2_Toverd2 reg3_Toverd2 using res/reg.rtf, stat(r2 fixed N) star(* 0.1 ** 0.05 *** 0.01)

//平行趋势检验
gen time = year - firstin

replace time = -11 if time < -11
replace time = 5 if time > 5
tab year,gen(year)
forvalues i=11(-1)1{
  gen pre`i'=(time == -`i')
}

gen current= (time == 0)

forvalues i=1(1)5{
  gen post`i'=(time==`i')
}

foreach dep of varlist Toverd1 Toverd2{
xtreg `dep' L.pre3 L.pre2 L.pre1 L.current L.post1 L.post2 L.post3 L.post4 L.post5 `cons' year1-year12,fe vce(robust)

coefplot, ///
   keep(L.pre3 L.pre2 L.pre1 L.current L.post1 L.post2 L.post3 L.post4 L.post5)		///
   coeflabels(L.pre3 = "-3"          ///
   L.pre2 = "-2"               ///
   L.pre1 = "-1"               ///
   L.current = "0"               ///
   L.post1 = "1"              ///
   L.post2 = "2"               ///
   L.post3 = "3"               ///
   L.post4 = "4"               ///
   L.post5 = "5")            ///
   vertical                             ///
   yline(0)                             ///
   ytitle("Coef")                 ///
   xtitle("上市公司进入沪深港通前后期数") ///
   addplot(line @b @at)                 ///
   ciopts(recast(rcap))                 ///
   rescale(100)                         ///
   scheme(s1mono)
graph export "res/`dep'.png"
esti store Para_`dep'
}
esttab Para_Toverd1 Para_Toverd2 using res/reg_para.rtf, stat(r2 fixed N) star(* 0.1 ** 0.05 *** 0.01)


//替换变量robust
foreach dep of varlist Tovery1 Tovery2{
reghdfe `dep' L2.did,absorb(code year)
esti store reg1_`dep'

reghdfe `dep' L2.did `cons',absorb(code year)
esti store reg2_`dep'

reghdfe `dep' L2.did `cons',absorb(code year)
esti store reg3_`dep'
}
esttab reg1_Tovery1 reg2_Tovery1 reg3_Tovery1 reg1_Tovery2 reg2_Tovery2 reg3_Tovery2 using res/regr.rtf, stat(r2 fixed N) star(* 0.1 ** 0.05 *** 0.01)

restore