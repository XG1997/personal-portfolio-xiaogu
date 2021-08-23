capt log close
log using reg,replace
log on

preserve

local consL "L.Size L.Age L.Fixed L.Value L.ROA L.OPE L.Growth L.Board L.Pay L.Inst L.State L.Input L.Subsidy L.Transp"
local cons "Size Age Fixed Value ROA OPE Growth Board Pay Input Subsidy"
local ys "Positive-Negative Disco degree_p"
local x "Patent"
local indus "C"
local y "degree_p"

replace Board = ln(Board)
gen degree_p = (Positive-Negative)/(Disco)
capt tab year,gen(year)
tab industry,gen(indus)
center `x'
center degree_p

xtset code year

//主回归
qui xtdpdsys `y' `x' L.`x' if industry_ == "C", lags(1) maxldep(2) endogenous(`x',lag(0,2)) twostep vce(robust)

estat sargan
estat abond
esti store reg_1

qui xtdpdsys `y' `x' L.`x' `consL' if industry_ == "C", lags(1) maxldep(2) pre(Size Age Fixed Value ROA OPE Growth Board Pay Transp)  endogenous(`x',lag(0,2)) endogenous(Transp,lag(0,3)) endogenous(Input,lag(0,2)) endogenous(Subsidy,lag(0,2)) twostep

estat sargan
estat abond
esti store reg_2

qui xtdpdsys `y' `x' L.`x' L.Size L.Age L.ROA L.Growth L.Inst L.State L.Input L.Subsidy if industry_ == "C", lags(1) maxldep(2) pre(Size Age ROA Growth )  endogenous(`x',lag(0,2)) endogenous(Input,lag(0,2)) endogenous(Subsidy,lag(0,2)) twostep vce(robust)

estat sargan
estat abond
esti store reg_3

esttab reg_1 reg_2 reg_3 using res/regL.rtf,star(* 0.1 ** 0.05 *** 0.01) stats(arm1 arm2 sargan N )

//调节
local j1 "Analyst"		//减弱
local j2 "BGD"
local j3 "Duality"
local j4 "SEO"		//增强
local j5 "Board"		//减弱


center `j1' `j2' `j3' `j4' `j5'

foreach i of varlist `j1' `j5' `j4'{
gen `i'_`x' = `i'*`x'

disp `i'
qui xtdpdsys  `y' `x' L.`x' L.`i' L.`i'_`x' `consL' if industry_ == "C", lags(1) maxldep(2) pre(Size Age Fixed Value ROA OPE Growth Board Pay Transp) endogenous(Transp,lag(0,3)) endogenous(`x',lag(0,2)) endogenous(Input,lag(0,2)) endogenous(Subsidy,lag(0,2)) twostep

estat sargan
estat abond
esti store `y'_`i'
}

esttab  degree_p_`j1' degree_p_`j5' degree_p_`j4' using res/inter.rtf,star(* 0.1 ** 0.05 *** 0.01) stats(arm1 arm2 sargan N )


restore
log close