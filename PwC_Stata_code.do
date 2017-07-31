*** "Paying with Change" Article Stata Code ***

* Marshall A. Taylor *
* Dustin S. Stoltz *

//Note: Seintment variables computed using tm.plugin.sentiment R package. The
	//event clustering variables computed using coreNLP named entity 
	//recognition tool in Bash.
	
******************************
// PwC_Stata_Code.do: "Paying with Change" Article State Code
// Note: Sentiment variables computed using tm.plugin.sentiment R package. The
	//event clustering variables computed using coreNLP named entity 
	//recognition tool in Bash. "Payor" is "Instigator" in the text; "Payee"
	//is "Target" in the text.
// Author: Marshall A. Taylor
******************************
version 13.1
clear all
macro drop _all
log using PwC_Code.log, replace text
set more off  

**************************
***COMMANDS BEGIN HERE ***
**************************
use PwC_Stata_Data, clear

//Descriptives 
univar sqapol Perceptibility Legibility Space Resistance Amount Payor3 Payee if missflag==1
tab Perceptibility Legibility if missflag==1, all

//The models. Using the square root of the absolute value of
	//polarity score as the DV (sqrt(abs(p-n/p+n))).
	//Using logged version of DV b/c of non-normality of identity.
global controls i. Space i.Resistance c.Amount##i.Payor3 i.Payee
permute sqapol _b, seed(50) reps(1000): xtreg sqapol ///
	i.Perceptibility $controls if missflag==1, re
permute sqapol _b, seed(50) reps(1000): xtreg sqapol ///
	i.Legibility $controls if missflag==1, re
permute sqapol _b, seed(50) reps(1000): xtreg sqapol ///
	i.Perceptibility##i.Legibility $controls if missflag==1, re

//Some effect size analyses. 
	//Note: these adjusted predictions were exported into R to make the bar plot
		//in ggplot2.
xtreg sqapol i.Perceptibility##i.Legibility $controls if ///
	missflag==1, re vce(robust) 
contrast r.Legibility@Perceptibility, asobserved 
contrast r.Perceptibility@Legibility, asobserved
margins Legibility, at (Perceptibility=(0(1)1)) asobserved 

//Some diagnostics
	//Hausman specification test
xtreg sqapol i.Perceptibility##i.Legibility $controls if missflag==1, re
est store re
xtreg sqapol i.Perceptibility##i.Legibility $controls if missflag==1, fe
est store fe
hausman fe re

	//LM test
est restore re
xttest0

	//DV is not skewed
sktest sqapol

	//No multicollinearity
collin Perceptibility Legibility Space Resistance Amount payor2 payor3 ///
	payee2-payee4 if missflag==1
	
	//Unconditional model. Plenty of within- and between-event variation to 
	//be explained: about 17% between and about 83% within in sqapol.
mixed sqapol if missflag==1||Event:,

	//Checking for infuential outliers and normal errors. Looks fine.
xtreg sqapol i.Perceptibility##i.Legibility $controls if missflag==1, ///
	re vce(robust)
predict r if missflag==1, e
kdensity r, normal
pnorm r
qnorm r
iqr r
swilk r
predict yhat
scatter r yhat
graph twoway (lfitci sqapol yhat) (scatter sqapol yhat)

	//Comparing sqapol to sqasubject (square root transformation of subject).
		//See footnote 6.
xtreg sqapol i.Perceptibility##i.Legibility $controls if missflag==1, ///
	re vce(robust)
xttest0 //Variance across events--supports use of random effects
xtreg sqasubject i.Perceptibility##i.Legibility $controls if missflag==1, ///
	re vce(robust)
xttest0 //No variance across events--prefers one-level OLS

///Interrater agreement using Cohen's k and 20% random sample
foreach i of varlist Perceptibility Legibility Space Resistance Payor Payee {
	kap `i' `i'2 if `i'2!=.
	}

//Models with asymptotic standard errors (in Appendix C).
xtreg sqapol i.Perceptibility $controls if missflag==1, ///
	re vce(robust)
xtreg sqapol i.Legibility $controls if missflag==1, ///
	re vce(robust)
xtreg sqapol i.Perceptibility##i.Legibility $controls if missflag==1, ///
	re vce(robust)

//
log close
exit 
