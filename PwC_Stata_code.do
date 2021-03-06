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
use PwC_Data, clear

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
margins Legibility, at (Perceptibility=(0(1)1)) asobserved //Highest score = High Leg and High Per,
								//supporting the hypothesis

program define perm1
version 13.1
xtreg sqapol ///
	i.Perceptibility##i.Legibility $controls if missflag==1, re
contrast r.Perceptibility@Legibility, asobserved post
end

	//Perceptibility effect sig. at 1.Legibility
permute sqapol _b, seed(50) reps(1000): perm1 if missflag==1
				//_b[Per]@0.Leg = Not sig. (p = .826)
				//_b[Per]@1.Leg = Sig at p < .01 (p = .002)

program define perm2
version 13.1
xtreg sqapol ///
	i.Perceptibility##i.Legibility $controls if missflag==1, re
contrast r.Legibility@Perceptibility, asobserved post
end

	//Legibility effect (marginally) sig. at 1.Perceptibility (Note that though 
		//the difference between low leg and high leg appears to be only
		//marginally sig in settings where money ideal-typically has high per, this
		//still means that BOTH high leg and high per are needed to maximize
		//the media sentiment score used here: for high per to be meaningfully
		//different from low per, the money needs to ALSO EXHIBIT high leg,
		//even though the difference between high leg and low leg itself is
		//only marginally sig [.065] at higher levels of perceptibility)
permute sqapol _b, seed(50) reps(1000): perm2 if missflag==1 
				//_b[Leg]@0.Per = Not sig. (p = .129)
				//_b[Leg]@1.Per = Marginally sig (p = .065)
				
	//The prediction with high L and High P appears to be the only one that,
		//after 1,000 permutations, doesn't appear to be due to a randomness
		//data-generating process (see "test1" below). 
		
	//Also note:
		//while one may be tempted to use Wald tests with randomization model p-values 
		//to compare the high P by high L prediction with the other three predictions
		//to assess differences, this doesn't work in the present case. This is
		//because all of the predictions generated with "test1" except for the 
		//high P by high L one can easily be accounted for by a random data
		//generating process--i.e., more than 5% of the absolute values of each
		//of those three predictions from the 1,000 permutations are greater than
		//or equal to the absolute value of the observed predictions. For instance,
		//you get a lot of low L by low P predictions that are greater than
		//.44; so, when you perform Wald tests of this difference with the 
		//high P by high L prediction for each permutation, you can easily get
		//a number of a bunch of small differences or even differences going in
		//the opposite direction. This is not because the two predictions are not
		//that different from one another; rather, it is because the absolute 
		//value of the high P by high L prediction is consistently less than the
		//absolute value of the observed prediction (because randomness cannot
		//account for the observed prediction) while the absolute value of the
		//if the low P by low L prediction is consistently GREATER THAN the 
		//absolute value of the observed prediction (because randomness CAN 
		//account for the observed prediction). It seems to me that the only
		//valid way to use a Wald test of prediction differences under the
		//randomization model of inference is to compare predictions that are
		//statistically significant under the randomization model.
program define test1
version 13.1
xtreg sqapol ///
	i.Perceptibility##i.Legibility $controls if missflag==1, re
margins Legibility, at (Perceptibility=(0(1)1)) asobserved post
end

permute sqapol _b, reps(1000) seed(50): test1 if missflag==1

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
		//See footnote 7.
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

/* Why we chose the control variable specification that we did */
	/*It made the most theoretical sense to us. This model also generated the largest between-setting R^2 
	and one of the smallest rmse. Further, looking at p-value CIs, can't rule out a p < .05 for the
	primary interaction term for any of these.
	Root MSE's and between-R^2's for different specifications with control variables:
	(our model) i. Space i.Resistance c.Amount##i.Payor3 i.Payee 
		RMSE = .1734; b-R^2 = .1349
	i.Space##c.Amount i.Resistance i.Payor3 i.Payee 
		RMSE = .1736; b-R^2 = .1283
	i.Space c.Amount i.Resistance i.Payor3 i.Payee 
		RMSE = .1752; b-R^2 = .1128
	i.Space c.Amount##i.Resistance i.Payor3 i.Payee 
		RMSE = .1745; b-R^2 = .1150
	i.Space i.Resistance i.Payor3 c.Amount##i.Payee 
		RMSE = .1720; b-R^2 = .1331
	i.Space i.Resistance i.Payor3 i.Payee c.Amount##c.Amount
		RMSE = .1752; b-R^2 = .1237 */

//What if we log the Amount variable, since it is right-skewed?
gen logamount = log(Amount)
global controls2 i. Space i.Resistance c.logamount##i.Payor3 i.Payee
permute sqapol _b, seed(50) reps(5000): xtreg sqapol ///
	i.Perceptibility##i.Legibility $controls2 if missflag==1, re
	//Results are very similar: Perc = .006, Leg = -.043, and Perc*Leg = .130. High-P and High-L
	//object-settings therefore still produce the largest predicted media sentiment, so the 
	//sign flip for the Perc main effect is inconsequential.
	//1,000 permutations gives a p(Perc*Leg) = .038 with a CI = (.027, .052). We then 
	//tried 5,000 permutations since this produced a boundary p-value, since
	//a larger number of permutations always generate more precise CIs since
	//we get closer to approximating the actual number of possible permutations.
	//5,000 permutations produce a CI = (.032, .043). As such, the "real" p-value
	//across permutations is clearly smaller than .05, so the results are consistent
	//regardless of whether the Amount variable was transformed or not.
	//The effect contrast tests are consistent.

//Supplementary P-Plot
xtreg sqapol i.Perceptibility##i.Legibility $controls if ///
	missflag==1, re vce(robust) 
margins Legibility, at (Perceptibility=(0(1)1)) asobserved

mat p_model = J(4, 3, .)

qui xtreg sqapol i.Perceptibility##i.Legibility $controls if ///
	missflag==1, re vce(robust)
qui margins Legibility, at (Perceptibility=(0(1)1)) asobserved post
mat def beta = e(b)
forvalues k = 1/4 {
	global k`k' = round(beta[1,`k'], .001)
	}
	
permute sqapol _b, reps(1000) seed(50): test1 if missflag==1
mat def m1 = r(p)
mat def m12 = r(ci)
forvalues k = 1/4 {
	mat p_model[`k',1] = m1[1,`k']
	mat p_model[`k',2] = m12[1,`k']
	mat p_model[`k',3] = m12[2,`k']
	}
	
coefplot (matrix(p_model[,1]), ci((p_model[,2] p_model[,3]))), ///
	mlabels(r1=12 ${k1} r2=12 ${k2} r3=12 ${k3} r4=12 ${k4}) mlabcolor(black) ///
	xline(0.05, lcolor(black)) mcol(black) msize(small) mlabsize(2) ///
	graphregion(fcolor(white) lcolor(white) lwidth(thick)) ///
	coeflabels(r1="Less P, Less L" r2="Less P, More L" r3="More P, Less L" ///
	r4="More P, More L", labsize(small)) xtitle("{it:P}-Value") ///
	xsize(1) ysize(1) saving(modelplot.gph, replace)

//
log close
exit  
