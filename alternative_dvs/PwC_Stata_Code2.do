*** "Paying with Change," Alternative DVs ***

* Marshall A. Taylor *
* Dustin S. Stoltz *

******************************
// PwC_Stata_Code2.do: Alternative DVs for "Paying with Change" article
//  Note: The paper uses a (square root transformed) absolute polarity measure
		//as an indicator of media sentiment (see the methods section of 
		//the paper for justifications). As a robustness check, I also 
		//re-ran the analyses with absolute polarity scores from different 
		//R packages. In short, the reported results hold and are supported.
// Author: Marshall A. Taylor
******************************
version 13.1
clear all
macro drop _all
log using PwC_Code2.log, replace text
set more off  

**************************
***COMMANDS BEGIN HERE ***
**************************
use PwC_Stata_Data2, clear //Same as PwC_Stata_Data.dta, except for the 
								//alternative DVs are appended.

//Controls
global controls i. Space i.Resistance c.Amount##i.Payor3 i.Payee

//Create absolute polarity scores from the polarity scores generated in R
	//(see PwC_R_Code2.R for details).
gen absnrc = abs(polnrc)
gen absave = abs(ave_sentiment)
	//"abspolarity" is the absolute polarity that is used in the paper (the
	//square root transformation is used in the paper to curbe issues with
	//skewness). I also ran the models below with square root transformations
	//of these variables, and there were no apreciable differences.

//Model with absolute polarity score using NRC lexicon from tidytext package.
	//Cross-product term is in the same direction and statisticaly sig.
permute absnrc _b, seed(50) reps(1000): xtreg absnrc ///
	i.Perceptibility##i.Legibility $controls if missflag==1, re

//Model with absolute polarity score from qdap package. Different direction of
	//cross-product term, but not significant.
permute absqdap _b, seed(50) reps(1000): xtreg absqdap ///
	i.Perceptibility##i.Legibility $controls if missflag==1, re
	
//What if we create an absolute polarity scale? Using maximum likelihood
	//factor analysis to decompose the communalities between the variables--
	//thus mitigating the bias of measurement error (since these indicators
	//correlate quite poorly).
factor abspolarity absqdap absnrc absave if missflag==1, ml
	//Loadings are in the expected direction except for absqdap. This doesn't
		//appear to be an issue with the fact that the qdap algorithm accounts
		//for negation and amplification, though, because its closely related
		//algorithm from the sentimentr package correlates with abspolarity
		//in the expected direction and also loads on the factor in the 
		//expected direction. Leaving absqdap in as a small correction for 
		//measurement error that there might be across the indicators.
predict fml

	//Again, cross-product term is in the same direction and also statistically
		//significant.
permute fml _b, seed(50) reps(1000): xtreg fml ///
	i.Perceptibility##i.Legibility $controls if missflag==1, re
	
//Could this sentiment variable be better conceptualized as an index of 
	//different "types" of emotional expression? If so, maybe principal
	//components is a better dimension reduction technique.
pca abspolarity absqdap absnrc absave if missflag==1
	//Same loading issue for absqdap. Leaving it in for the same reason
		//behing the factor-analytic version.
predict pca

	//Again, cross-product term is in the same direction and also statistically
		//significant.
permute pca _b, seed(50) reps(1000): xtreg pca ///
	i.Perceptibility##i.Legibility $controls if missflag==1, re

//Like the subjectivity score referenced in footnote 7 of the article,
	//none of the alternative subjectivity scores vary much across settings.
foreach i of valist subjqdap subjnrc {
	qui xtreg `i' i.Perceptibility##i.Legibility $controls if missflag==1, ///
		re vce(robust)
	xttest0
	}

//
log close
exit 
