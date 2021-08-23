capt log close
log using median.smcl ,replace
log on 


preserve

//drop if industry_ != "C"
tab year,gen(year)
tab industry,gen(indus)
local cons "Size Age Fixed Value ROA Growth Board Pay Input Subsidy lever Finance GDP lev"
local consL "L.Size L.Age L.Fixed L.Value L.ROA L.Growth L.Board L.Pay L.Input L.Subsidy L.lever L.Finance L.GDP L.lev"
local consF "F.Size F.Age F.Fixed F.Value F.ROA F.Growth F.Board F.Pay F.Input F.Subsidy F.lever F.Finance F.GDP "

/**被解释变量**/
gen y = Patent+Umia+Desia
gen y1 = Patent
gen y2 = Umia+Desia
tab y2
/**解释变量**/
gen x = Disclo
//gen x = Positive+Neutral

/**中介变量**/
//gen m = lloan/loan
//gen m = inter/(sd+ld)-(rs/100)*(sd/(sd+ld))-(rl/100)*(ld/(sd+ld))
//gen m = loan/(flow-sd)
//gen m = overhead
gen m = loan/assets

/**分组变量**/
gen zbld = ln(RDInput/rdstuff)

gen f = 1 if fin_bg >= 1
replace f = 0 if f == .

egen ss = median(Size)
gen s = 1 if Size >= ss
replace s = 0 if s == .

egen ggg = median(Transp)
gen t = 1 if Transp >= ggg
replace f = 0 if t == .

/**控制变量**/
replace Finance = ln(Finance+1)

/**缺失值处理**/
foreach v of varlist y y1 y2  x m `cons'{
by year,sort: egen `v'_ = mean(`v')
replace `v'=`v'_ if `v' == . 
drop `v'_
}
foreach v of varlist y y1 y2 x m `cons'{
egen `v'_ = mean(`v')
replace `v'=`v'_ if `v' == . 
drop `v'_
}

/**异常值处理**/
foreach v of varlist x m `cons'{
//kdensity `v'
//graph export "res/`v'.png"
rename `v' `v'winsor
winsor `v'winsor,gen(`v') p(0.01) 
}

foreach v of varlist y y1 y2 {
//kdensity `v'
//graph export "res/`v'.png"
rename `v' `v'winsor
winsor `v'winsor,gen(`v') p(0.01) 
}


/**工具变量(注意与缺失值和异常值位置)**/
by industry prov year,sort: egen xg = mean(Disclo)
xtset code year
gen Lx = L.x
gen Lxg = L.xg
gen L2xg = L2.xg

xtset code year

/**描述性分析**/
logout,save (res/describe) word replace:	///
tabstat y y1 y2 x m `cons',s(mean p50 sd min max N) c(s)

/**主回归(所有专利+发明+非发明)**/
foreach y of varlist y y1 y2{
qui reghdfe `y' L.x `cons',absorb(code year) vce(robust)
esti sto A`y'
}
esttab Ay Ay1 Ay2 using res/主回归.rtf,s(N r2_a) star(* 0.1 ** 0.05 *** 0.01) compress


/**内生性检验_iv**/
foreach y of varlist y y1 y2{
qui xtivreg2 `y' `cons' year1-year5 (Lx = Lxg L2xg),fe robust
esti sto B1`y'
}
esttab B1y B1y1 B1y2 using res/iv.rtf,s(N r2_a) star(* 0.1 ** 0.05 *** 0.01) drop(year*)


/**内生性检验_gmm**/
foreach y of varlist y y1 y2{
qui xtdpdsys `y' L.x `consL' year1-year5,maxlags(2) pre(Size Age Finance GDP) endogenous(x,lag(0,2)) endogenous(Input,lag(0,2)) endogenous(Subsidy,lag(0,2))
est sto B2`y'
estat sargan
}
esttab B2y B2y1 B2y2 using res/gmm.rtf,s(N r2_a ar1 ar2 sargan) star(* 0.1 ** 0.05 *** 0.01)

/**稳健性检验
foreach y of varlist y y1 y2{
qui xttobit `y' L.x `cons' i.year i.industry,ll(0)
esti sto C`y'
}
esttab Cy Cy1 Cy2 using res/tobit.rtf,s(N r2_a) star(* 0.1 ** 0.05 *** 0.01)
**/


/**机制检验_中介效应-y0.05**/
qui reghdfe F.m x  `consF',absorb(code year) vce(robust)
esti sto My
foreach y of varlist y y1 y2{
qui reghdfe F.`y' x `consF',absorb(code year) vce(robust)
esti sto D`y'a
qui reghdfe F.`y' m x `consF',absorb(code year) vce(robust)
esti sto D`y'b
qui reghdfe F.`y' m `consF',absorb(code year) vce(robust)
esti sto D`y'c
}
esttab My Dya Dyb Dyc Dy1a Dy1b Dy1c Dy2a Dy2b Dy2c using res/median.rtf,s(N r2_a) star(* 0.1 ** 0.05 *** 0.01)



/**机制检验_中介效应_分样本0-y0.01**/
forvalues i = 1(-1)0{
qui reghdfe F.m x  `consF' if t == `i',absorb(code year) vce(robust)
esti sto My
foreach y of varlist y{
qui reghdfe F.`y' x `consF' if t == `i',absorb(code year) vce(robust)
esti sto D`y'a
qui reghdfe F.`y' m x `consF' if t == `i',absorb(code year) vce(robust)
esti sto D`y'b
qui reghdfe F.`y' m `consF' if t == `i',absorb(code year) vce(robust)
esti sto D`y'c
}
esttab My Dya Dyb Dyc using res/median_fin_`i'.rtf,s(N r2_a) star(* 0.1 ** 0.05 *** 0.01)
}


/**机制检验_中介效应_分样本1-y0.003**/
forvalues i = 1(-1)0{
qui reghdfe F.m x  `consF' if s == `i',absorb(code year) vce(robust)
esti sto My
foreach y of varlist y{
qui reghdfe F.`y' x `consF' if s == `i',absorb(code year) vce(robust)
esti sto D`y'a
qui reghdfe F.`y' m x `consF' if s == `i',absorb(code year) vce(robust)
esti sto D`y'b
qui reghdfe F.`y' m `consF' if s == `i',absorb(code year) vce(robust)
esti sto D`y'c
}
esttab My Dya Dyb Dyc using res/median_size_`i'.rtf,s(N r2_a) star(* 0.1 ** 0.05 *** 0.01)
}

/**机制检验_调节效应
//BGD Ys 0.007 0.02
foreach y of varlist y y1 y2{
qui reghdfe F.`y' c.Ana##c.x `cons',absorb(code year) vce(robust)
est sto E`y'
}
esttab Ey Ey1 Ey2 ,s(N r2_a) star(* 0.1 ** 0.05 *** 0.01)
**/

restore
log off


