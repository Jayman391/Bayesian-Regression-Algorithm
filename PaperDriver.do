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

//continuous variable to binary variable function, 1 if above mean and 0 if equal to or below
capture program drop contToBin
program contToBin
	local i = 1
	while "``i''" != "" {
		summarize ``i''
		local m = r(mean)
		generate ``i''_B = 1 if ``i'' > `m'
		replace ``i''_B = 0 if ``i'' <= `m'
		summarize ``i''_B, detail
		local ++i
	}
end

 
//Interaction Term Program, calculates interaction terms between all independent variables and regresses them against the dependent variable
capture program drop interactions
program interactions
local i = 1
local j = `i' + 1
	while "``i''" != "" {	
	    while "``j''" != "" {
			if `i' != `j' {
				//variables are equivalent, generated 2 for ease of calling interaction terms in regressions
				generate I_``i''_``j'' = ``i'' * ``j''
				generate I_``j''_``i'' = ``i'' * ``j''
			}
			local ++j
		}
		local ++i
	}
end

//Normalize variable function, turns min to 0 and max to 1
capture program drop normalize
program normalize
	summarize `1'
	return list
	local m = r(mean)
	local sd = r(sd)
	local min = r(min)
	local max = r(max)
	generate `1'_norm = (`1'-`min')/(`max'-`min')
	sort `1'
	summarize `1'_norm
	
end

//Standardize variable function, turns mean to 0 and SD to 1
capture program drop standardize
program standardize
	summarize `1'
	return list
	local m = r(mean)
	local sd = r(sd)
	local mean = r(mean)
	local max = r(max)
	generate `1'_stan= (`1'-`mean')/(`sd')
	sort `1'
	summarize `1'_stan
	
end


//display regression data function
capture program drop displayData
program displayData
	quietly return list
	matrix m = r(table)
	scalar c = colsof(m)
	scalar i = 1
	display "Beta, SE, Z, P(X>|Z|), 95% CI"
	while i <= c {
		noi: display el(m,1,i)
		noi: display el(m,2,i)
		noi: display el(m,3,i)
		noi: display el(m,4,i)
		noi: display el(m,5,i)
		noi: display el(m,6,i)

		display "------------"
		scalar i = i + 1
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
generate D =  ( SST / POP ) * ( NSLPT / POP ) 


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

// create binary representations

contToBin lnYGR lnKGR lnn lnAGR lnDGR lnIGR lnHGR lnLGR
contToBin lnY lnA lnD lnK lnI lnH lnL
contToBin Y n A D K I H L
contToBin YGR AGR DGR KGR IGR HGR LGR

summarize Y YGR lnY lnYGR lnY_B lnYGR_B
summarize K KGR lnK lnKGR lnK_B lnKGR_B
summarize L LGR lnL lnLGR lnL_B lnLGR_B
summarize I IGR lnI lnIGR lnI_B lnIGR_B
summarize A AGR lnA lnAGR lnA_B lnAGR_B
summarize H HGR lnH lnHGR lnH_B lnHGR_B
summarize D DGR lnD lnDGR lnD_B lnDGR_B
summarize n lnn lnn lnn_B lnn_B



/////////LINEAR REGRESSION//////////


	// basic regression forms
	regress Y K L I A H D n, vce(robust)
	estimates store YR

	xtreg Y K L I A H D n, fe vce(robust)
	estimates store YXR

	ivregress 2sls Y (K L = I A H D n), vce(robust) first
	estimates store YIR

	//Growth rate regressions
	regress YGR KGR LGR IGR AGR HGR DGR n, vce(robust)
	estimates store YGRR

	xtreg YGR KGR LGR IGR AGR HGR DGR n, fe vce(robust)
	estimates store YGRXR

	ivregress 2sls YGR (KGR LGR = IGR AGR HGR DGR n), vce(robust) first
	estimates store YGRIR

	// ln regressions
	regress lnY lnK lnL lnI lnA lnH lnD lnn, vce(robust)
	estimates store lnYR

	xtreg lnY lnK lnL lnI lnA lnH lnD lnn, fe vce(robust)
	estimates store lnYXR

	ivregress 2sls lnY (lnK lnL = lnI lnA lnH lnD lnn), vce(robust) first
	estimates store lnYIR

	// ln Growth rate regressions
	regress lnYGR lnKGR lnLGR lnIGR lnAGR lnHGR lnDGR lnn, vce(robust)
	estimates store lnYGRR

	xtreg lnYGR lnKGR lnLGR lnIGR lnAGR lnHGR lnDGR lnn, fe vce(robust)
	estimates store lnYGRXR

	ivregress 2sls lnYGR (lnKGR lnLGR = lnIGR lnAGR lnHGR lnDGR lnn), vce(robust) first
	estimates store lnYGRIR
	
	//regression table results
	estimates table YR YXR YIR, stats(r2_a r2_o chi2) star
		putdocx begin
		putdocx table results = etable
		putdocx save "basic_form_linear_regressions.docx"
	estimates table YGRR YGRXR YGRIR, stats(r2_a r2_o chi2) star
		putdocx begin
		putdocx table results = etable
		putdocx save "growth_rate_form_linear_regressions.docx"
	estimates table lnYR lnYXR lnYIR, stats(r2_a r2_o chi2) star
		putdocx begin
		putdocx table results = etable
		putdocx save "ln_basic_form_linear_regressions.docx"
	estimates table lnYGRR lnYGRXR lnYGRIR, stats(r2_a r2_o chi2) star
		putdocx begin
		putdocx table results = etable
		putdocx save "ln_growth_rate_form_linear_regressions.docx"

	//////////////BEST MODEL//////////////////
	regress lnY lnK lnL lnI lnA lnH lnD lnn, vce(robust)
	quietly ereturn list
	//Bayesian regression of model
	bayesmh lnY lnK lnL lnI lnA lnH lnD lnn, likelihood(normal({sigma2})) prior({sigma2}, igamma(.1, .1)) prior({lnY:_cons}, normal(_b[_cons], _se[_cons])) prior({lnY:lnK}, normal(_b[lnK], _se[lnK])) prior({lnY:lnL}, normal(_b[lnL], _se[lnL])) prior({lnY:lnI}, normal(_b[lnI], _se[lnI])) prior({lnY:lnA}, normal(_b[lnA], _se[lnA])) prior({lnY:lnH}, normal(_b[lnH], _se[lnH])) prior({lnY:lnD}, normal(_b[lnD], _se[lnD])) prior({lnY:lnn}, normal(_b[lnn], _se[lnn])) saving(mcmcR, replace)
	//predict outcome
	bayespredict yhatR, mean rseed(16)
	//generate residuals
	generate lnYresidualsR = yhatR - lnY

	// regress residuals on predicted value
	regress lnYresidualsR lnK, vce(robust)
	predict pRK
	graph twoway (scatter lnYresidualsR lnK) (lfitci pRK lnK, sort)  ,saving("basic_residual_regression_K.gph") by(Region)

	regress lnYresidualsR lnL, vce(robust)
	predict pRL
	graph twoway (scatter lnYresidualsR lnL) (lfitci pRL lnL, sort),saving("basic_residual_regression_L.gph") by(Region)

	xtreg lnYresidualsR lnK, vce(robust)
	predict pRXK
	graph twoway (scatter lnYresidualsR lnK) (lfitci pRXK lnK, sort),saving("panel_residual_regression_K.gph") by(Region)

	xtreg lnYresidualsR lnL, vce(robust)
	predict pRXL
	graph twoway (scatter lnYresidualsR lnL) (lfitci pRXL lnL, sort),saving("panel_residual_regression_L.gph") by(Region)


	pwcorr lnY yhatR lnYresidualsR 
	pwcorr pRK pRXK 
	pwcorr pRL pRXL 
	pwcorr lnY yhatR lnYresidualsR pRL pRXL pRK pRXK 

/////////PROBIT REGRESSION//////////

	//basic binary interpretations
	probit Y_B K_B L_B I_B A_B H_B D_B n_B, vce(robust)
	estimates store YB

	xtprobit Y_B K_B L_B I_B A_B H_B D_B n_B, vce(robust)
	estimates store YXB

	ivprobit Y_B n_B (K_B L_B = I_B A_B H_B D_B), vce(robust)
	estimates store YIB

	//binary interpretations of Growth rates
	probit YGR_B KGR_B LGR_B IGR_B AGR_B HGR_B DGR_B n_B, vce(robust)
	estimates store YGRB

	xtprobit YGR_B KGR_B LGR_B LGR_B IGR_B AGR_B HGR_B DGR_B n_B, vce(robust)
	estimates store YGRXB

	ivprobit YGR_B (KGR_B LGR_B = IGR_B AGR_B HGR_B DGR_B LGR_B IGR_B AGR_B HGR_B DGR_B n_B), vce(robust)
	estimates store YGRIB

	//binary interpretations of ln representations
	probit lnY_B lnK_B lnL_B lnn_B lnI_B lnA_B lnH_B lnD_B, vce(robust)
	estimates store lnYB

	xtprobit lnY_B lnK_B lnL_B lnn_B lnI_B lnA_B lnH_B lnD_B , vce(robust)
	estimates store lnYXB

	ivprobit lnY_B (lnK_B lnL_B = lnn_B lnI_B lnA_B lnH_B lnD_B), vce(robust)
	estimates store lnYIB

	//binary interpretations of growth rates of ln representations
	probit lnYGR_B lnKGR_B lnLGR_B lnn_B lnIGR_B lnAGR_B lnHGR_B lnDGR_B, vce(robust)
	estimates store lnYGRB


	xtprobit lnYGR_B lnKGR_B lnLGR_B lnn_B lnIGR_B lnAGR_B lnHGR_B lnDGR_B, vce(robust)	
	estimates store lnYGRXB

	ivprobit lnYGR_B (lnKGR_B lnLGR_B = lnn_B lnIGR_B lnAGR_B lnHGR_B lnDGR_B), vce(robust)
	estimates store lnYGRIB

	estimates table YB YXB YIB, stats(ll chi2) star
		putdocx begin
		putdocx table results = etable
		putdocx save "basic_form_probit_regressions.docx"
	estimates table YGRB YGRXB YGRIB, stats(ll chi2) star
		putdocx begin
		putdocx table results = etable
		putdocx save "growth_rate_form_probit_regressions.docx"
	estimates table lnYB lnYXB lnYIB, stats(ll chi2) star
		putdocx begin
		putdocx table results = etable
		putdocx save "ln_basic_form_probit_regressions.docx"
	estimates table lnYGRB lnYGRXB lnYGRIB, stats(ll chi2) star
		putdocx begin
		putdocx table results = etable
		putdocx save "ln_growth_rate_form_probit_regressions.docx"
	//////////////BEST MODEL//////////////////
	ivprobit YGR_B (KGR_B LGR_B = IGR_B AGR_B HGR_B DGR_B LGR_B IGR_B AGR_B HGR_B DGR_B n_B), vce(robust)

	//Bayesian Probit Regression Model
	bayesmh YGR_B KGR_B LGR_B, likelihood(probit) prior({YGR_B:}, cauchy(0,2.5) ) saving(mcmcP, replace)
	//predict outcome
	bayespredict yhatP, median rseed(16)
	//generate residuals, 0 is correct, 1 is incorrect
	generate lnYresidualsP = yhatP - lnY_B

	probit lnYresidualsP lnK_B, vce(robust)
	predict pPK
	graph twoway (scatter lnYresidualsP lnK_B) (lfitci pPK lnK_B, sort),saving("basic_residual_probit_regression_K.gph") by(Region)

	probit lnYresidualsP lnL_B, vce(robust)
	predict pPL
	graph twoway (scatter lnYresidualsP lnL_B) (lfitci pPL lnL_B, sort),saving("basic_residual_probit_regression_L.gph") by(Region)

	xtprobit lnYresidualsP lnK_B, vce(robust)
	predict pPXK
	graph twoway (scatter lnYresidualsP lnK_B) (lfitci pPXK lnK_B, sort),saving("panel_residual_probit_regression_K.gph") by(Region)

	xtprobit lnYresidualsP lnL_B, vce(robust)
	predict pPXL
	graph twoway (scatter lnYresidualsP lnL_B) (lfitci pPXL lnL_B, sort),saving("panel_residual_probit_regression_L.gph") by(Region)



	pwcorr lnY_B yhatP lnYresidualsP
	pwcorr pPK pPXK
	pwcorr pPL pPXL
	pwcorr lnY_B yhatP lnYresidualsP pPK pPXK pPL pPXL