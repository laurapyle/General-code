libname data "C:\\";

data wt_4blups;
set data.wt_4blups;
run;

ods graphics on;
PROC MIXED DATA = Wt_4blups plots=RESIDUALPANEL(BLUP); 
Title "All Available Weights, no mc066, no COVID";
WHERE COVID_PT = 0;
CLASS fab_study_id Group (ref = "2") Visit (ref = "1");
MODEL AnalWeight = Group Visit Group*Visit /SOLUTION;
RANDOM Intercept/Subject = fab_study_id;
LSMEANS Group*Visit / CL PDIFF;
RUN;
/* conditional residuals appear normally distributed, so no evidence that the assmption of normality */
/* for the random effects is violated */
