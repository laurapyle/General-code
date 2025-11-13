/* Sample size calculation for logistic regression */
/* Detecting OR = 1.2 with outcome prevalence = 29.2% */

/* First, calculate p2 from OR and p1 */
data _null_;
   p1 = 0.292;  /* baseline prevalence */
   or = 0.81;    /* odds ratio to detect */
   
   /* Calculate p2 from OR formula: OR = [p2/(1-p2)] / [p1/(1-p1)] */
   odds1 = p1 / (1 - p1);
   odds2 = or * odds1;
   p2 = odds2 / (1 + odds2);
   
   call symputx('p1', p1);
   call symputx('p2', p2);
   
   put "Baseline proportion (p1): " p1;
   put "Expected proportion (p2): " p2;
run;

/* Now use PROC POWER with the two proportions */
proc power;
   twosamplefreq test=pchi
      groupproportions = (&p1 &p2)
      alpha = 0.05
      power = 0.80
      ntotal = .;
run;

/* this is closer - assumes covariate is N(0,1) */
proc power;
	logistic
		vardist("Menarche") = normal(0,1)
		testpredictor = "Menarche"
		responseprob = 0.292
		testoddsratio = 0.81
		alpha = 0.05
		power = 0.8
		ntotal = .;
run;


/* two sample t-test */

proc power;
	twosamplemeans test=diff
	groupmeans = 1.7 | 3.3
	stddev = 1.3
	npergroup = . 
	power = 0.8
	alpha = 0.05;
run;
