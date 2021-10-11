/*******************************************************************************
 Replication code for 
 "Improving Health and Economic Security by Reducing Work-Schedule Uncertainty"
  Kristen Harknett, UC San Francisco 
  Daniel Schneider, Harvard University
  Veronique Irwin, UC Berkeley
  Proceedings of the National Academy of Sciences 
  October 2021
 ******************************************************************************/
 
set scheme plotplain			
set matsize 10000 

* SET PATH
* cd "TBD by researcher"

use "Seattle_SSO.dta", clear 

log using "Seattle_SSO.log", replace

* control variables
global controls i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.jobtenure i.subsector

/*******************************************************************************
   Table 1 - means of outcomes for Seattle and Comparison at baseline                                *
*******************************************************************************/
 * Adjusted mean values 
preserve 
keep if seattle_treat==1 | minwage_comp == 1   /* select treatment and main comparison sample */
foreach var in unpred_scale shortnotice timing timingpay oncall clopen cancelpay happy distressed goodsleep hardship { 
  quietly: mi est, esampvaryok: regress `var' i.seattle_treat $controls if period == 0 [pw=acs_weight]
  mimrgns seattle_treat
}
restore

/*******************************************************************************
   Table S1 - means of sociodemographics and sector 
   for Seattle Shift Project sample, unweighted and weighted 
   TBD - NEED TO RETRIEVE THE ACS DESCRIPTIVES DATA AND CODE
*******************************************************************************/
sum ibn.age ibn.race female ibn.educ enrolled ibn.cohabstatus kids ibn.subsector if seattle_treat==1 & _mi_m>0, sep(0)
sum ibn.age ibn.race female ibn.educ enrolled ibn.cohabstatus kids ibn.subsector if seattle_treat==1 & _mi_m>0 [aw=acs_weight], sep(0)
	
/*******************************************************************************
   Table S2 - means of sociodemographics 
   for Seattle and Comparison samples
   weighted and unweighted
   pre- and post-Secure Scheduling Ordinance
*******************************************************************************/
sum ibn.age ibn.race female ibn.educ kids if period==0 & seattle_treat==1 & _mi_m>0, sep(0)
sum ibn.age ibn.race female ibn.educ kids if period==0 & seattle_treat==1 & _mi_m>0 [aw=acs_weight], sep(0)

sum ibn.age ibn.race female ibn.educ kids if inlist(period,1,2) & seattle_treat==1 & _mi_m>0, sep(0)
sum ibn.age ibn.race female ibn.educ kids if inlist(period,1,2) & seattle_treat==1 & _mi_m>0 [aw=acs_weight], sep(0)

sum ibn.age ibn.race female ibn.educ kids if period==0 & minwage_comp==1 & _mi_m>0, sep(0)
sum ibn.age ibn.race female ibn.educ kids if period==0 & minwage_comp==1 & _mi_m>0 [aw=acs_weight], sep(0)

sum ibn.age ibn.race female ibn.educ kids if inlist(period,1,2) & minwage_comp==1 & _mi_m>0, sep(0)
sum ibn.age ibn.race female ibn.educ kids if inlist(period,1,2) & minwage_comp==1 & _mi_m>0 [aw=acs_weight], sep(0)

/*******************************************************************************
   Figures 1a and 1b - DiD estimates for scheduling outcomes
*******************************************************************************/
lab def seattle_treat 0 "comparison workers" 1 "Seattle workers"
lab val seattle_treat seattle_treat 

lab var shortnotice "Less than 2 Weeks' Notice"
lab var timing "Shift Change" 
lab var timingpay "Shift Change without Pay"
lab var oncall "On-Call Shift" 
lab var clopen "Clopening Shift" 
lab var cancelpay "Cancellation without Pay" 
lab var unpred_scale "Unpredictability Scale (0 to 6)"

** ADJUST THE OUTCOME VARIABLES TO PRODUCE MARGINSPLOTS THAT SET BASELINE VALUE TO ZERO
foreach var in shortnotice timing timingpay oncall clopen cancelpay unpred_scale {
 quietly: mi estimate, post: reg `var' period##seattle_treat $controls if (seattle_treat==1 | minwage_comp == 1) [pw=acs_weight], cluster(respondent_id)
  mimrgns period#seattle_treat, cmdmargins
   local precontrol=el(r(b),1,1) 
   local pretreat=el(r(b),1,2) 
   gen     `var'_new = `var' - `precontrol' if seattle_treat == 0 
   replace `var'_new = `var' - `pretreat'   if seattle_treat == 1 
}

** UNPREDICTABILITY SCALE FIGURE 1A
foreach var in unpred_scale  {
quietly: mi estimate, post: reg `var'_new period##seattle_treat $controls if (seattle_treat==1 | minwage_comp == 1) [pw=acs_weight], cluster(respondent_id)
 mimrgns period#seattle_treat, cmdmargins
 marginsplot , ///
  legend(row(2) col(1) pos(6)  symxsize(4) size(medsmall) region(lstyle(none))) ///
  title("") ///
  xtitle("") xline(.5)  ///
  recastci(rarea) ///
  plot1opt(lcolor(gs8)) plot2opt(lcolor(green)) ///
  ci1opt(color(gs8%20)) ci2opt(color(green%10)) ///
  ytitle("")   ysc(alt)  ///
  xlabel(0 "Blin" 1 "Y1" 2 "Y2", labsize(medsmall)) /// 
  note("{bf:Fig. 1A. Impacts of Seattle’s Secure Scheduling Ordinance on Work Schedule Unpredictability Scale (0 to 6)}" ///
       "Notes: Baseline values are set at zero. Y1 and Y2 values are changes relative to baseline."  ///
	   "95% confidence intervals are indicated by green shading for Seattle and grey shading for Comparison cities." ///
	   "Dashed vertical line indicates when the Secure Scheduling Ordinance went into effect.", size(small))  ///
  saving(Replicate_Fig1a, replace)
}

** MARGINSPLOTS OF IMPACTS FOR SIX DICHOTOMOUS SCHEDULING OUTCOMES 
foreach var in shortnotice timing timingpay oncall clopen cancelpay {   
 quietly: mi estimate, post: reg `var'_new period##seattle_treat $controls if (seattle_treat==1 | minwage_comp == 1) [pw=acs_weight], cluster(respondent_id)
 mimrgns period#seattle_treat, cmdmargins 
 marginsplot , ///
  title("`: variable label `var''", span size(medsmall)) ///	                         /* SPAN CENTERS THE TITLE BETTER */						
  legend(row(2) col(1) pos(6)  symxsize(4) size(medsmall) region(lstyle(none))) ///      /* POSITION 6 IS ON THE 1 TO 12 CLOCK POSITIONING SCALE */
  xtitle("") xline(.5)  ///																 /* XLINE(.5) DRAWS LINE AT INTERVENTION TIME POINT BECAUSE IT HAPPENED BETWEEN PERIOD 0 AND 1 */
  recastci(rarea) ///
  plot1opt(lcolor(gs8)) plot2opt(lcolor(green)) ///
  ci1opt(color(gs8%20)) ci2opt(color(green%10)) ///
  ytitle("")  ylabel(-.25 "-25" -.20 "-20" -.15 "-15" -.10 "-10" -.05 "-5" 0 "0" .05 "5") ysc(alt) ///	/* YSC(ALT) PUTS THE Y-AXIS LABELS ON THE RIGHT SIDE */	
  xlabel(0 "Blin" 1 "Y1" 2 "Y2", labsize(medsmall)) /// 
  name(Replicate_`var', replace)
}
grc1leg2 Replicate_shortnotice Replicate_timing Replicate_timingpay Replicate_clopen Replicate_oncall Replicate_cancelpay, rows(2) cols(3) saving(Replicate_Fig1b, replace) ///
       note("{bf:Fig. 1B. Impacts of Seattle’s Secure Scheduling Ordinance on Work Schedules}" ///
            "Notes: Baseline values are set at zero. Y1 and Y2 values are changes relative to baseline."  ///
	   "95% confidence intervals are indicated by green shading for Seattle and grey shading for Comparison cities." ///
	   "Dashed vertical line indicates when the Secure Scheduling Ordinance went into effect.", size(small)) 

/*******************************************************************************
   Figure 2 - DiD estimates for well-being outcomes 
*******************************************************************************/   
** ADJUST THE OUTCOME VARIABLES TO PRODUCE MARGINSPLOTS THAT SET BASELINE VALUE TO ZERO
foreach var in happy distressed goodsleep hardship {
 quietly: mi estimate, post: reg `var' period##seattle_treat $controls if (seattle_treat==1 | minwage_comp == 1) [pw=acs_weight], cluster(respondent_id)
  mimrgns period#seattle_treat, cmdmargins
   local precontrol=el(r(b),1,1) 
   local pretreat=el(r(b),1,2) 
   gen     `var'_new = `var' - `precontrol' if seattle_treat == 0 
   replace `var'_new = `var' - `pretreat'   if seattle_treat == 1 
}

** MARGINSPLOTS OF IMPACTS FOR FOUR DICHOTOMOUS WELL-BEING OUTCOMES 
foreach var in happy distressed goodsleep hardship {
 quietly: mi estimate, post: reg `var'_new period##seattle_treat $controls if (seattle_treat==1 | minwage_comp == 1) [pw=acs_weight], cluster(respondent_id)
 mimrgns period#seattle_treat, cmdmargins
 marginsplot , ///
  title("`: variable label `var''", span size(medium)) ///
  legend(row(2) col(1) pos(6)  symxsize(4) size(medium) region(lstyle(none))) ///
  xtitle("") xline(.5)  ///
  recastci(rarea) ///
  plot1opt(lcolor(gs8)) plot2opt(lcolor(green)) ///
  ci1opt(color(gs8%20)) ci2opt(color(green%10)) ///
  ytitle("") ylabel(-.15 "-15" -.10 "-10" -.05 "-5" 0 "0" .05 "5" .1 "10" .15 "15", labsize(medsmall))  ysc(alt)  ///
  xlabel(0 "Blin" 1 "Y1" 2 "Y2", labsize(medium)) /// 
  name(Replicate_`var', replace)
}
grc1leg2 Replicate_happy Replicate_distressed Replicate_goodsleep Replicate_hardship, rows(2) cols(2) saving(Replicate_Fig2, replace)  ///
  note("{bf:Fig. 2. Impacts of Seattle’s Secure Scheduling Ordinance on Worker Well-being}" ///
        "Notes: Baseline values are set at zero. Y1 and Y2 values are changes relative to baseline."  ///
	   "95% confidence intervals are indicated by green shading for Seattle and grey shading for Comparison cities." ///
	   "Dashed vertical line indicates when the Secure Scheduling Ordinance went into effect.", size(small)) 


/*******************************************************************************
   Table S3 - DiD models for the 7 scheduling outcomes 
   with the 4 different comparison groups        
   weighted and unweighted 
*******************************************************************************/

** MININUM WAGE COMPARISON GROUP 
preserve 
keep if seattle_treat==1 | minwage_comp == 1 
foreach var in shortnotice timing timingpay clopen oncall cancelpay unpred_scale  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls [pw=acs_weight], cluster(respondent_id)
eststo `var'
}
mimrgns seattle_treat#period 
esttab shortnotice timing timingpay clopen oncall cancelpay unpred_scale using "Replicate-TabS3.txt", replace ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept") keep(1.seattle_treat#2*) ///
	mtitle lines title("Min Wage Comparison group")
restore 	

** SCHEDULING CITIES 
preserve 
keep if seattle_treat==1 | schedcity_comp 
foreach var in shortnotice timing timingpay clopen oncall cancelpay unpred_scale  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls [pw=acs_weight] , cluster(respondent_id)
eststo `var'
}
mimrgns seattle_treat#period 
esttab shortnotice timing timingpay clopen oncall cancelpay unpred_scale   using "Replicate-TabS3.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("Scheduling Cities Comparison group")
restore 
		
** SCHEDULING STATES 
preserve 
keep if seattle_treat==1 | schedstate_comp 
foreach var in shortnotice timing timingpay clopen oncall cancelpay unpred_scale   {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls [pw=acs_weight], cluster(respondent_id)
eststo `var'
}
mimrgns seattle_treat#period 
esttab shortnotice timing timingpay clopen oncall cancelpay unpred_scale   using "Replicate-TabS3.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("Scheduling Cities and States Comparison group")
restore 
	
** NEAR SEATTLE 
preserve 	
keep if seattle_treat==1 | nearseattle_comp ==1 
foreach var in shortnotice timing timingpay clopen oncall cancelpay unpred_scale   {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls [pw=acs_weight], cluster(respondent_id)
eststo `var'
}
mimrgns seattle_treat#period 
esttab shortnotice timing timingpay clopen oncall cancelpay unpred_scale   using "Replicate-TabS3.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("Near Seattle Comparison group")
restore 

** UNWEIGHTED MININUM WAGE COMPARISON GROUP 
preserve 
keep if seattle_treat==1 | minwage_comp == 1 
foreach var in shortnotice timing timingpay clopen oncall cancelpay unpred_scale  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls , cluster(respondent_id)
eststo `var'
}
mimrgns seattle_treat#period 
esttab shortnotice timing timingpay clopen oncall cancelpay unpred_scale using "Replicate-TabS3.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept") keep(1.seattle_treat#2*) ///
	mtitle lines title("UNWEIGHTED Min Wage Comparison group")
restore 	

** UNWEIGHTED SCHEDULING CITIES 
preserve 
keep if seattle_treat==1 | schedcity_comp 
foreach var in shortnotice timing timingpay clopen oncall cancelpay unpred_scale  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls , cluster(respondent_id)
eststo `var'
}
esttab shortnotice timing timingpay clopen oncall cancelpay unpred_scale   using "Replicate-TabS3.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("UNWEIGHTED Scheduling Cities Comparison group")
restore 
		
** UNWEIGHTED SCHEDULING STATES 
preserve 
keep if seattle_treat==1 | schedstate_comp 
foreach var in shortnotice timing timingpay clopen oncall cancelpay unpred_scale   {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls , cluster(respondent_id)
eststo `var'
}
esttab shortnotice timing timingpay clopen oncall cancelpay unpred_scale   using "Replicate-TabS3.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("UNWEIGHTED Scheduling Cities and States Comparison group")
restore 
	
** UNWEIGHTED NEAR SEATTLE 
preserve 	
keep if seattle_treat==1 | nearseattle_comp ==1 
foreach var in shortnotice timing timingpay clopen oncall cancelpay unpred_scale   {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls , cluster(respondent_id)
eststo `var'
}
esttab shortnotice timing timingpay clopen oncall cancelpay unpred_scale   using "Replicate-TabS3.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("UNWEIGHTED Near Seattle Comparison group")
restore 

/*******************************************************************************
   Table S4 - DiD models for the 4 well-being outcomes 
   with the 4 different comparison groups        
   weighted and unweighted 
*******************************************************************************/

** MININUM WAGE COMPARISON GROUP 
preserve 
keep if seattle_treat==1 | minwage_comp == 1 
foreach var in  happy distressed goodsleep hardship {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls [pw=acs_weight], cluster(respondent_id)
mimrgns seattle_treat#period 
eststo `var'
}
esttab  happy distressed goodsleep hardship using "Replicate-TabS4.txt", replace ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept") keep(1.seattle_treat#2*) ///
	mtitle lines title("Min Wage Comparison group")
restore 	

** SCHEDULING CITIES 
preserve 
keep if seattle_treat==1 | schedcity_comp 
foreach var in  happy distressed goodsleep hardship  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls [pw=acs_weight], cluster(respondent_id)
mimrgns seattle_treat#period 
eststo `var'
}
esttab  happy distressed goodsleep hardship  using "Replicate-TabS4.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("Scheduling Cities Comparison group")
restore 
		
** SCHEDULING STATES 
preserve 
keep if seattle_treat==1 | schedstate_comp 
foreach var in  happy distressed goodsleep hardship  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls [pw=acs_weight], cluster(respondent_id)
mimrgns seattle_treat#period 
eststo `var'
}
esttab  happy distressed goodsleep hardship  using "Replicate-TabS4.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("Scheduling Cities and States Comparison group")
restore 
	
** NEAR SEATTLE 
preserve 	
keep if seattle_treat==1 | nearseattle_comp ==1 
foreach var in  happy distressed goodsleep hardship  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls [pw=acs_weight], cluster(respondent_id)
mimrgns seattle_treat#period 
eststo `var'
}
esttab  happy distressed goodsleep hardship  using "Replicate-TabS4.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("Near Seattle Comparison group")
restore 


** UNWEIGHTED MININUM WAGE COMPARISON GROUP 
preserve 
keep if seattle_treat==1 | minwage_comp == 1 
foreach var in  happy distressed goodsleep hardship {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls , cluster(respondent_id)
mimrgns seattle_treat#period 
eststo `var'
}
esttab  happy distressed goodsleep hardship using "Replicate-TabS4.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept") keep(1.seattle_treat#2*) ///
	mtitle lines title("Min Wage Comparison group")
restore 	

** UNWEIGHTED SCHEDULING CITIES 
preserve 
keep if seattle_treat==1 | schedcity_comp 
foreach var in  happy distressed goodsleep hardship  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls , cluster(respondent_id)
mimrgns seattle_treat#period 
eststo `var'
}
esttab  happy distressed goodsleep hardship  using "Replicate-TabS4.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("Scheduling Cities Comparison group")
restore 
		
** UNWEIGHTED SCHEDULING STATES 
preserve 
keep if seattle_treat==1 | schedstate_comp 
foreach var in  happy distressed goodsleep hardship  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls , cluster(respondent_id)
mimrgns seattle_treat#period 
eststo `var'
}
esttab  happy distressed goodsleep hardship  using "Replicate-TabS4.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("Scheduling Cities and States Comparison group")
restore 
	
** UNWEIGHTED NEAR SEATTLE 
preserve 	
keep if seattle_treat==1 | nearseattle_comp ==1 
foreach var in  happy distressed goodsleep hardship  {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls , cluster(respondent_id)
mimrgns seattle_treat#period 
eststo `var'
}
esttab  happy distressed goodsleep hardship  using "Replicate-TabS4.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept")  keep(1.seattle_treat#2*) ///
	mtitle lines title("Near Seattle Comparison group")
restore 

/*******************************************************************************
   F-STAT 
   Easy way to get is by mi test seapost 
   or by squaring the t-stat for sea*period==2 coefficient 
*******************************************************************************/
* WITH COVARIATES 
mi est: regress unpred_scale i.age i.race female i.educ enrolled i.cohabstatus kids ///
i.manager i.jobtenure i.subsector seattle_treat##i.period if seattle_treat==1 | minwage_comp==1 [pw=acs_weight], cluster(respondent_id)

/**************************************************************************
* ROBUSTNESS
    PS match as an ALTERNATIVE to WEIGHTED results 
    kmatch is not compatible with imputations, 
	do match then use mi est for impact estimates
    specified to force exact matches on education to address imbalance in college
	matching without replacement
	one to one matching
***************************************************************************/
	
gen subsector2 = subsector 
recode subsector2 (3=2) (5=2) (7/9 = 9) 

gen tenure = jobtenure 
recode tenure (4/7=4)

* use multiply imputed data, generate _weight variable that identifies the matched analysis sample
kmatch ps seattle_treat i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2 ///
      (unpred_scale  = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (shortnotice = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (timing  = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (timingpay  = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (clopen = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (oncall  = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (cancelpay  = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (happy = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (distressed  = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (goodsleep  = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	  (hardship  = i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2) ///
	   if (_mi_m>0 & (inlist(period,0,2))), ematch(educ) over(period) wgen(_weight) nn(1) wor att 
kmatch summarize 

mat M = r(M)
mat V = r(V)
coefplot matrix(M[,3]) matrix(M[,6]) || matrix(V[,3]) matrix(V[,6]) || , keep(*:) ///
bylabels("Std. mean difference" "Variance ratio") ///
noci byopts(xrescale)
addplot 1: , xline(0) norescaling legend(order(1 "Raw" 2 "Matched"))
addplot 2: , xline(1) norescaling

foreach var in unpred_scale shortnotice timing timingpay clopen oncall cancelpay {
quietly: mi est, esampvaryok post: reg `var' i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2 if (inlist(period,0,2)) [pw=_weight], cluster(respondent_id)
eststo `var'
}
esttab unpred_scale shortnotice timing timingpay clopen oncall cancelpay using "Replicate-PSmatch.txt", replace ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept") keep(*seattle_treat*) ///
	mtitle lines title("PS Match weighted - imputed - EXACT MATCH ON EDUCATION ONLY - keep DEM - WOR - ONE TO ONE")	

foreach var in happy distressed goodsleep hardship {
quietly: mi est, esampvaryok post: reg `var' i.seattle_treat##i.period i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.tenure i.subsector2 if (inlist(period,0,2)) [pw=_weight], cluster(respondent_id)
eststo `var'
}
esttab happy distressed goodsleep hardship using "Replicate-PSmatch.txt", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept") keep(*seattle_treat*) ///
	mtitle lines title("PS Match weighted - imputed - EXACT MATCH ON EDUCATION ONLY - keep DEM - WOR - ONE TO ONE")	

/**************************************************************************
* ROBUSTNESS
    DiD Models - new approach with interacting period with all cities 
	Use this to create Figure S3 
	CITY VARIABLE IS NOT AVAILABLE ON DATAFILE TO PROTECT CONFIDENTIALITY
**************************************************************************/

/** MININUM WAGE COMPARISON GROUP 
preserve 
keep if seattle_treat==1 | minwage_comp == 1 

gen city = . 
replace city = 1 if seattle_treat==1 

foreach var in unpred_scale {
quietly: mi estimate, post: reg `var' i.city##period $controls [pw=acs_weight], cluster(respondent_id)
eststo `var'
}
mimrgns city#period 
esttab unpred_scale using "Replicate-all-city", replace ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept") keep(*#2*) ///
	mtitle lines title("CBSA ## period specification - Min Wage Comparison Group")

foreach var in happy distressed goodsleep hardship {
quietly: mi estimate, post: reg `var' i.city##period $controls [pw=acs_weight], cluster(respondent_id)
eststo `var'
}
esttab happy distressed goodsleep hardship   using "Replicate-all-city", append ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept") keep(*#2*) ///
	mtitle lines title("CBSA ## period specification - Min Wage Comparison Group")
restore */

/**************************************************************************
* ROBUSTNESS
    Well-being outcomes as continuous 
**************************************************************************/

** MININUM WAGE COMPARISON GROUP 
preserve 
keep if seattle_treat==1 | minwage_comp == 1 
foreach var in  distress_scale hardship_scale {
quietly: mi estimate, post: reg `var' seattle_treat##period $controls [pw=acs_weight], cluster(respondent_id)
mimrgns seattle_treat#period 
eststo `var'
}
esttab  distress_scale hardship_scale using "Replicate-Continuous.txt", replace ///
	r2 t b(%9.3f) star(+ .1 * .05 ** .01 *** .001) label coeflabels(_cons "Intercept") keep(1.seattle_treat#2*) ///
	mtitle lines title("Wellbeing Continuous")
restore 	

/**************************************************************************
* ROBUSTNESS
    Interact impact with sector 
**************************************************************************/

mi estimate, post: reg unpred_scale seattle_treat##period##ib2.subsector2 i.age i.race i.female i.educ i.enrolled i.cohabstatus i.kids i.manager i.jobtenure if seattle_treat==1 | minwage_comp == 1 [pw=acs_weight], cluster(respondent_id)
mimrgns seattle_treat#period#subsector2  


/****************************************************************
   IVprobit 2SLS ESITMATES 
*****************************************************************/
* create post##seattle indicators 
gen seapost2 = period==2 & seattle_treat==1
gen seapost1 = period==1 & seattle_treat==1


*** THE FOUR IV MODELS - WEIGHTED - STATA 16 ***
foreach sample in minwage_comp {
foreach var in happy distressed goodsleep hardship {
mi estimate, cmdok: ivprobit `var' i.age i.race female i.educ enrolled i.cohabstatus kids ///
manager i.jobtenure i.subsector i.seattle_treat i.period (unpred_scale=seapost1 seapost2) if seattle_treat==1 | `sample'==1  [pw=acs_weight], vce(cluster respondent_id)
}
}

*** THE FOUR IV MODELS - UNWEIGHTED - STATA 16 ***
foreach sample in minwage_comp {
foreach var in happy distressed goodsleep hardship {
mi estimate, cmdok: ivprobit `var' i.age i.race female i.educ enrolled i.cohabstatus kids ///
manager i.jobtenure i.subsector i.seattle_treat i.period (unpred_scale=seapost1 seapost2) if seattle_treat==1 | `sample'==1, vce(cluster respondent_id)
}
}

version 13 // Stata changed the ivprobit margins command after V13. Revert to V13 to generate the desired marginal predicted probabilities.
*** THE FOUR IV MODELS -- WEIGHTED - STATA 13 ***
foreach sample in minwage_comp {
foreach var in happy distressed goodsleep hardship {
mi estimate, cmdok: ivprobit `var' i.age i.race female i.educ enrolled i.cohabstatus kids ///
manager i.jobtenure i.subsector i.seattle_treat i.period (unpred_scale=seapost1 seapost2) if seattle_treat==1 | `sample'==1  [pw=acs_weight], vce(cluster respondent_id)
mimrgns, at(unpred_scale=(0(1)6)) predict(pr) 
}
}

log close
