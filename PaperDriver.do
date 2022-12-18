//clear console
clear
putdocx clear

//calculate growth rate of a variable
capture program drop growthRate
program growthRate
	local i  = 1
	while "``i''" != "" {
		generate L``i'' = L.``i''
		generate ``i''GR = (``i'' - L``i'') / L``i'' 
		//drop ``i''
		drop L``i''
		local ++i
	}
end

//generate ln of a variable function
capture program drop lngen
program lngen
	local i = 1
	while "``i''" != "" {
		gen ln``i'' = ln(``i'')
		//drop ``i''
		local ++i
	}
end


//import data
import excel "C:\Users\Jason\Desktop\EC 200 FINAL !!!\Statedata_US.xlsx", sheet("Sheet1") firstrow

//rename variables
rename Real_GDP_2012_millions GDP
rename Total_Employment_Jobs JOBS
rename Personal_Consumption_Expenditure PCE
rename povrate PR
rename ssirecipients_total SST
rename foodstamp_case FST
rename nslp_total NSLPT
rename Median_HH_Income MEDINC
rename Population POP
rename Labor_Productivity_Growth_Rate LPGR
rename innovatescore_boehmkeskinner STPIN
rename Total_Capital_Stock_Millions CAP


//replace missing values
replace LPGR = LPGR[_n-1] if LPGR >= .
replace STPIN = STPIN[_n-1] if STPIN >= .



//change string to int categorical
egen state = group(State)
drop State


//initialize tsset
tsset state Year
 
//////////////SOLOW GROWTH MODEL//////////////////

// L => JOBS 
rename JOBS L


//SPENDING (Investment) PER STATE = ((Population / average pop per household) * median HH Income) - Personal Consumption Expendtitures

	//average pop per household is ~3.15 over 1998-2021 with almost no variance.
	
growthRate POP


// I
generate I =  (( ( POP / 3.15 ) * MEDINC ) - PCE ) / ( GDP * 1000000 )


// n => POPGR 

generate n = ( 1 + POPGR )
//Technology Growth Rate =  (1 + productivity growth rate) * (1 + state policy productivity score)

// A
generate A = ( 1 + (LPGR/100) ) * ( 1 + STPIN )

//Depreciation Rate = povrate * (ssirecipients_total / Population) * (nslp_total / Population) * (foodstamp_case / Population)

// H
generate H = ( 1 - (PR/100) ) * ( 1 - ( FST / POP ) )

// D = depreciation rate = ratio of Social Security Bennefitters * National School Lunch Signups
generate D =  1 + ( SST / POP ) * ( NSLPT / POP ) 


// Y = Output per Capita

generate Y = ( GDP / POP ) * 1000000


// K = total capital / state population

generate K = CAP / POP

//fill in missing data
by state : replace A = A[_n-1] if A >= .
by state : replace D = D[_n-1] if D >= .
by state : replace H = H[_n-1] if H >= .
by state : replace n = n[_n-1] if n >= .
by state : replace I = I[_n-1] if I >= .
by state : replace K = K[_n-1] if K >= .
by state : replace Y = Y[_n-1] if Y >= .

// ln representations

lngen Y L n A D K I H
growthRate Y L A D K I H
lngen YGR LGR AGR DGR KGR IGR HGR


//fill in missing data
by state : replace lnYGR = lnYGR[_n-1] if lnYGR >= .
by state : replace lnAGR = lnAGR[_n-1] if lnAGR >= .
by state : replace lnDGR = lnDGR[_n-1] if lnDGR >= .
by state : replace lnHGR = lnHGR[_n-1] if lnHGR >= .
by state : replace lnn = lnn[_n-1] if lnn >= .
by state : replace lnIGR = lnIGR[_n-1] if lnIGR >= .




outreg2 using summary-statistics.doc, word replace sum(log)

/////////LINEAR REGRESSION//////////


	// basic regression forms
	regress Y K L I A H D n, vce(robust)
	outreg2 using basic_form_linear_regressions.doc, append ctitle(base) 

	xtreg Y K L I A H D n, fe vce(robust)
	quietly ereturn list
	outreg2  using basic_form_linear_regressions.doc, append ctitle(panel) addstat(Overall R squared, e(r2_o)) 


	ivregress 2sls Y (K L = I A H D n), vce(robust) first
	quietly ereturn list
outreg2 using basic_form_linear_regressions.doc, append ctitle(iv) addstat(Chi Square, e(chi2))
 
	//Growth rate regressions
	regress YGR KGR LGR IGR AGR HGR DGR n, vce(robust)
outreg2 using growth_rate_form_linear_regressions.doc, append ctitle(base)

	xtreg YGR KGR LGR IGR AGR HGR DGR n, fe vce(robust)
	quietly ereturn list
outreg2 using growth_rate_form_linear_regressions.doc, append ctitle(panel) addstat(Overall R squared, e(r2_o)) 

	ivregress 2sls YGR (KGR LGR = IGR AGR HGR DGR n), vce(robust) first
	quietly ereturn list
	outreg2 using growth_rate_form_linear_regressions.doc, append ctitle(iv) addstat(Chi Square, e(chi2))

	// ln regressions
	regress lnY lnK lnL lnI lnA lnH lnD lnn, vce(robust)
outreg2 using ln_basic_form_linear_regressions.doc, append ctitle(base) 

	xtreg lnY lnK lnL lnI lnA lnH lnD lnn, fe vce(robust)
	quietly ereturn list
outreg2 using ln_basic_form_linear_regressions.doc, append ctitle(panel) addstat(Overall R squared, e(r2_o)) 	

	ivregress 2sls lnY (lnK lnL = lnI lnA lnH lnD lnn), vce(robust) first
outreg2 using ln_basic_form_linear_regressions.doc, append ctitle(iv) addstat(Chi Square, e(chi2))

	// ln Growth rate regressions
	regress lnYGR lnKGR lnLGR lnIGR lnAGR lnHGR lnDGR lnn, vce(robust)
outreg2 using ln_growth_rate_form_linear_regressions.doc, append ctitle(base)

	xtreg lnYGR lnKGR lnLGR lnIGR lnAGR lnHGR lnDGR lnn, fe vce(robust)
	quietly ereturn list	
outreg2 using ln_growth_rate_form_linear_regressions.doc, append ctitle(panel) addstat(Overall R squared, e(r2_o)) 

	ivregress 2sls lnYGR (lnKGR lnLGR = lnIGR lnAGR lnHGR lnDGR lnn), vce(robust) first
	quietly ereturn list
outreg2 using ln_growth_rate_form_linear_regressions.doc, append ctitle(iv)	addstat(Chi Square, e(chi2))



	//////////////BEST MODEL//////////////////
	regress lnY lnK lnL lnI lnA lnH lnD lnn, vce(robust)
	quietly ereturn list
	//Bayesian regression of model
	bayesmh lnY lnK lnL lnI lnA lnH lnD lnn, likelihood(normal({sigma2})) prior({sigma2}, igamma(.1, .1)) prior({lnY:_cons}, normal(_b[_cons], _se[_cons])) prior({lnY:lnK}, normal(_b[lnK], _se[lnK])) prior({lnY:lnL}, normal(_b[lnL], _se[lnL])) prior({lnY:lnI}, normal(_b[lnI], _se[lnI])) prior({lnY:lnA}, normal(_b[lnA], _se[lnA])) prior({lnY:lnH}, normal(_b[lnH], _se[lnH])) prior({lnY:lnD}, normal(_b[lnD], _se[lnD])) prior({lnY:lnn}, normal(_b[lnn], _se[lnn])) saving(mcmcR, replace)
	
	putdocx clear
	bayesstats summary
	quietly return list
	matrix r = r(summary)
	putdocx begin
	putdocx table summarize = matrix(r), rownames colnames
	putdocx save bayesian_estimates
	
	//predict outcome
	bayespredict yhatR, mean rseed(16)
	//generate residuals
	generate lnYresidualsR = yhatR - lnY

	// regress residuals on predicted value
	regress lnYresidualsR lnK, vce(robust)
	predict pRK
	graph twoway (scatter lnYresidualsR lnK) (lfitci pRK lnK, sort)  ,saving("basic_residual_regression_K.gph")

	regress lnYresidualsR lnL, vce(robust)
	predict pRL
	graph twoway (scatter lnYresidualsR lnL) (lfitci pRL lnL, sort),saving("basic_residual_regression_L.gph") 
	
	regress lnYresidualsR lnI, vce(robust)
	predict pRI
	graph twoway (scatter lnYresidualsR lnI) (lfitci pRI lnI, sort),saving("basic_residual_regression_I.gph") 
	
	regress lnYresidualsR lnA, vce(robust)
	predict pRA
	graph twoway (scatter lnYresidualsR lnA) (lfitci pRA lnA, sort),saving("basic_residual_regression_A.gph")
	
	regress lnYresidualsR lnH, vce(robust)
	predict pRH
	graph twoway (scatter lnYresidualsR lnH) (lfitci pRH lnH, sort),saving("basic_residual_regression_H.gph") 
	
	regress lnYresidualsR lnn, vce(robust)
	predict pRn
	graph twoway (scatter lnYresidualsR lnn) (lfitci pRn lnn, sort),saving("basic_residual_regression_n.gph") 
	
	regress lnYresidualsR lnD, vce(robust)
	predict pRD
	graph twoway (scatter lnYresidualsR lnD) (lfitci pRD lnD, sort),saving("basic_residual_regression_D.gph") 
	
	
	



	pwcorr lnY yhatR lnYresidualsR 
	pwcorr pRK pRXK 
	pwcorr pRL pRXL 
	pwcorr lnY yhatR lnYresidualsR pRL pRXL pRK pRXK 

	
//Convergence


//Unconditional

//lambda = convergence rate = f(population growth rate, technology growth rate and depreciation rate.)

generate lambda = n + D + A - 3
summarize lambda
lngen lambda

// undoconditional and conditional convergence
regress lnYGR lnlambda, vce(robust)
scalar speed = log(1 + _b[lnlambda])/(2021-1998)
scalar halflife = log(2)/speed
outreg2 using lnlambda_convergence.doc, append ctitle(base) addstat(speed, speed , halflife, halflife)


xtreg lnYGR lnlambda, fe vce(robust)
scalar speed = log(1 + _b[lnlambda])/(2021-1998)
scalar halflife = log(2)/speed
outreg2 using lnlambda_convergence.doc, append ctitle(panel) addstat(speed, speed , halflife, halflife)


//Simulate separate model on each Region

foreach n of numlist 1/8 {
	putdocx clear
	display "Region " +  `n'
	bayes : xtreg lnYGR lnlambda if Region==`n'
	quietly ereturn list
		matrix m = e(mean)
		scalar speed = log(1 + m[1,1])/(2021-1998)
		scalar halflife = log(2)/speed
	putdocx begin
	bayesstats summary
		matrix r = r(summary)
	putdocx table table1 = matrix(r), rownames colnames
	putdocx table table2 = (2,2)
	putdocx table table2(1,1) = ("Convergence Speed (% per year)")
	putdocx table table2(2,1) = ("Halflife Speed (# years)")
	putdocx table table2(1,2) = (speed)
	putdocx table table2(2,2) = (halflife)
	putdocx save region`n'_convergence_lnlambda, replace
}


/* test on each state of region 1, the only consistantly significant region (NEW ENGLAND!), 6 chains are used per region as 50 states / 8 regions ~ 6 states per region (For rounding)
*/
bayes, nchains(6) : xtreg lnYGR lnlambda if Region==1
	quietly ereturn list
		matrix m = e(mean)
		scalar speed = log(1 + m[1,1])/(2021-1998)
		scalar halflife = log(2)/speed
	putdocx clear
	putdocx begin
	bayesstats summary
		matrix r = r(summary)
	putdocx table table1 = matrix(r), rownames colnames
	putdocx table table2 = (2,2)
	putdocx table table2(1,1) = ("Convergence Speed (% per year)")
	putdocx table table2(2,1) = ("Halflife Speed (# years)")
	putdocx table table2(1,2) = (speed)
	putdocx table table2(2,2) = (halflife)
	putdocx save New_England_Convergence_lnlambda
bayesgraph diagnostics {lnYGR:lnlambda}
bayesstats grubin


//Region Converges, coefficients are not at a 95% level (somewhere around 92-93)
bayes, nchains(6) : xtreg lnYGR lnlambda if Region==4
	quietly ereturn list
		matrix m = e(mean)
		scalar speed = log(1 + m[1,1])/(2021-1998)
		scalar halflife = log(2)/speed
	putdocx clear
	putdocx begin
	bayesstats summary
		matrix r = r(summary)
	putdocx table table1 = matrix(r), rownames colnames
	putdocx table table2 = (2,2)
	putdocx table table2(1,1) = ("Convergence Speed (% per year)")
	putdocx table table2(2,1) = ("Halflife Speed (# years)")
	putdocx table table2(1,2) = (speed)
	putdocx table table2(2,2) = (halflife)
	putdocx save Plains_Convergence_lnlambda
bayesgraph diagnostics {lnYGR:lnlambda}
bayesstats grubin

 
 
//Conditional Convergence
regress lnYGR lnlambda lnA lnH lnI lnn, vce(robust)
scalar speed = log(1 + _b[lnlambda])/(2021-1998)
scalar halflife = log(2)/speed
outreg2 using conditional_lnlambda_convergence.doc, append ctitle(base) addstat(speed, speed , halflife, halflife)


xtreg lnYGR lnlambda lnA lnH lnI lnn, fe vce(robust)
scalar speed = log(1 + _b[lnlambda])/(2021-1998)
scalar halflife = log(2)/speed
outreg2 using conditional_lnlambda_convergence.doc, append ctitle(panel) addstat(speed, speed , halflife, halflife)

ivregress 2sls lnYGR (lnlambda = lnA lnH lnI lnn), vce(robust)
scalar speed = log(1 + _b[lnlambda])/(2021-1998)
scalar halflife = log(2)/speed
outreg2 using conditional_lnlambda_convergence.doc, append ctitle(iv) addstat(speed, speed , halflife, halflife)


//Simulate separate model on each Region

foreach n of numlist 1/8 {
	putdocx clear
	display "Region " +  `n'
	bayes : regress lnYGR lnlambda lnA lnH lnI lnn if Region==`n'
	quietly ereturn list
		matrix m = e(mean)
		scalar speed = log(1 + m[1,1])/(2021-1998)
		scalar halflife = log(2)/speed
	putdocx begin
	bayesstats summary
		matrix r = r(summary)
	putdocx table table1 = matrix(r), rownames colnames
	putdocx table table2 = (2,2)
	putdocx table table2(1,1) = ("Convergence Speed (% per year)")
	putdocx table table2(2,1) = ("Halflife Speed (# years)")
	putdocx table table2(1,2) = (speed)
	putdocx table table2(2,2) = (halflife)
	putdocx save Conditional_region`n'_convergence_lnlambda, replace
}


bayes, nchains(6) : xtreg lnYGR lnlambda lnA lnH lnI lnn if Region==1
	quietly ereturn list
		matrix m = e(mean)
		scalar speed = log(1 + m[1,1])/(2021-1998)
		scalar halflife = log(2)/speed
	putdocx clear
	putdocx begin
	bayesstats summary
		matrix r = r(summary)
	putdocx table table1 = matrix(r), rownames colnames
	putdocx table table2 = (2,2)
	putdocx table table2(1,1) = ("Convergence Speed (% per year)")
	putdocx table table2(2,1) = ("Halflife Speed (# years)")
	putdocx table table2(1,2) = (speed)
	putdocx table table2(2,2) = (halflife)
	putdocx save Conditional_New_England_Convergence_lnlambda, replace
bayesgraph diagnostics {lnYGR:lnlambda}
bayesstats grubin


bayes, nchains(6) : xtreg lnYGR lnlambda lnA lnH lnI lnn if Region==4
	quietly ereturn list
		matrix m = e(mean)
		scalar speed = log(1 + m[1,1])/(2021-1998)
		scalar halflife = log(2)/speed
	putdocx clear
	putdocx begin
	bayesstats summary
		matrix r = r(summary)
	putdocx table table1 = matrix(r), rownames colnames
	putdocx table table2 = (2,2)
	putdocx table table2(1,1) = ("Convergence Speed (% per year)")
	putdocx table table2(2,1) = ("Halflife Speed (# years)")
	putdocx table table2(1,2) = (speed)
	putdocx table table2(2,2) = (halflife)
	putdocx save Conditional_Plains_Convergence_lnlambda
bayesgraph diagnostics {lnYGR:lnlambda}
bayesstats grubin

bayes, nchains(6) : xtreg lnYGR lnlambda lnA lnH lnI lnn if Region==6
	quietly ereturn list
		matrix m = e(mean)
		scalar speed = log(1 + m[1,1])/(2021-1998)
		scalar halflife = log(2)/speed
	putdocx clear
	putdocx begin
	bayesstats summary
		matrix r = r(summary)
	putdocx table table1 = matrix(r), rownames colnames
	putdocx table table2 = (2,2)
	putdocx table table2(1,1) = ("Convergence Speed (% per year)")
	putdocx table table2(2,1) = ("Halflife Speed (# years)")
	putdocx table table2(1,2) = (speed)
	putdocx table table2(2,2) = (halflife)
	putdocx save Conditional_SouthWest_Convergence_lnlambda
bayesgraph diagnostics {lnYGR:lnlambda}
bayesstats grubin


  
