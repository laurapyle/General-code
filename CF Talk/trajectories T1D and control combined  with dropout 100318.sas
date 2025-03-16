libname data 'H:\BDC\CACTI\CACTI twelve year progression\visit 4 data';
*libname data 'C:\For Austria trip\CACTI twelve year progression trajectories\visit 4 data';


proc format;
  value dia 1='T1D'
            0='Control'
  ;
  value $agegroup  	"19-29"="19-29"
					"30-39"="30-39"
					"40-49"="40-49"
					"50-56"="50-56"
				 ;
  value sex 1='Male'
  			2='Female'
  ;
  value race 	1='White'
				2='Black or African American'
				3='American Indian or Alaskan Native '
				4='Asian'
				5='Native Hawaiian or Other Pacific Islander '
				6='Other race'
				9='No Response'
  ;
  value $smkstatus   'Curren'='Current'
  					'Former'='Former'
					'Never'='Never'
  ;
  value yn 0='No'
           1='Yes'
		   .='Missing'
  ;
run;

data alldata;
set data.alldata;
if dia=1 then dianum=1;
else dianum=0;
format dianum yn.;
run;
proc freq data=alldata; tables dianum*dia; run;
proc contents data=alldata; run;

/*  how many ppts in the dataset */
data temp;
set alldata;
keep studyid dia;
run;
proc sort data=temp nodupkey; by studyid; run;
proc freq data=temp; tables dia; run;
/* total of 1416 ppts */

/* create progression variables */
/* define as increase in square root transformed CAC volume of 2.5 or greater compared to baseline */
/* first get baseline CAC */
data basecac;
set alldata;
where visit='1';
basevolavesqrt=volavesqrt;
keep studyid basevolavesqrt;
run;
/* 1416 baseline visits */
proc sort data=basecac; by studyid; run;
proc sort data=alldata; by studyid; run;
data alldata;
merge alldata basecac;
by studyid; 
run;
proc print data=alldata(obs=10); run;
/* calculate change in volavesqrt */
data alldata;
set alldata;
delta_volavesqrt=volavesqrt-basevolavesqrt;
if delta_volavesqrt=. then progression=.;
else if delta_volavesqrt>=2.5 then progression=1;
else progression=0;
run;
proc print data=alldata;
var studyid visit basevolavesqrt volavesqrt delta_volavesqrt progression;
run;

/* testing - are statins associated with progression? */
proc contents data=alldata; run;
proc sort data=alldata; by dia; run;
proc freq data=alldata;
tables progression*onstatinmeds / chisq;
by dia;
run;

/* need to calculate estimated eis variables, agecat, NHW */
data alldata;
set alldata;
tg_hdl= tri/hdlc;
if sex=. then male=.;
else if sex=1 then male=1;
else male=0;
if age = . then agegroup = "";
else if age ge 19 and age lt 30 then agegroup="19-29";
else if age ge 30 and age lt 40 then agegroup="30-39";
else if age ge 40 and age lt 50 then agegroup="40-49";
else if age ge 50 then agegroup="50-56";
if dia=1 then do;
		eIS1 = exp(4.06154 - avewaist*0.01317 - insdoseperkg*1.09615 + adiponectin*0.02027 - tri*0.00307 - avediabp*0.00733);
		eIS2 = exp(4.61476 - avewaist*0.02506 - insdoseperkg*1.53803 );
		eIS3 = exp(4.1075 - avewaist*0.01299 - insdoseperkg*1.05819 - tri*0.00354 - avediabp*0.00802);
end;
if dia=0 then do;
		eIS1 = exp(7.47237 - avewaist*0.01275 + adiponectin*0.01905 - hba1c*0.24990 - glu*0.01983 - tri*0.00324 -avediabp*0.00588);
		eIS2 = exp(6.10604 + 0.21170*male - avewaist*0.02293 -  HbA1c*0.28233 );
		eIS3 = exp(7.19138 + 0.10173*male - avewaist*0.01414 -  HbA1c*0.33308 - glu*0.01290 - tri*0.00316);
end;
pp = avesystbp - avediabp;
if race=. or spanorg=. then nhw=.;
else if race=1 and spanorg=0 then nhw=1;
else nhw=0;
label   age='Age (yr)'
		agegroup='Age group'
		sex='Sex'
		race='Race'
		spanorg='Hispanic origin'
		nhw='Non-Hispanic White'
		yrsschool='Years of school'
		dia='Diabetes status'
		duration='Duration of diabetes (yr)'
		bmi='BMI (kg/m2)'
		avewaist='Waist (cm)'
		ldl='LDL (mg/dl)' 
		hdlc='HDL (mg/dl)'
		tri ='Triglycerides (mg/dl)'
		tg_hdl='Tg:HDL'
		glu ='Fasting glucose (mg/dl)'
		hba1c='HbA1c (%)'
		Insdoseperkg ='Insulin dose (units/kg)'
		avediabp='Diastolic BP (mm/Hg)'
		avesystbp='Systolic BP (mm/Hg)'
		pp='Pulse pressure'
		eis1='eIS 1'
		eis2='eIS 2'
		eis3='eIS 3'
		smkstatus='Smoking status'
		onacemeds ='ACEi use'
		onarbmeds='ARB use'
		onhypermeds='On hypertension meds'
		onlipidmeds='Lipid med use'
		onstatinmeds ='Statin use'
		onantidepressivemeds='On antidepressive meds'
		ac='Albumin:creatinine'
		CKDepi='CKD Epi'
		screat='Serum creatinine (mg/dl)'
		adiponectin='Adiponectin'
		kcal='Kcal of activity per week'
		blocks='Blocks walked per day'
		flights ='Flights of stairs per day'
		modactivity='Min moderate activity per week'
		vigactivity='Min vigorous activity per week'
	;
format agegroup $agegroup.
		sex sex.
		race race.
		spanorg yn.
		nhw yn.
		dia dia.
		smkstatus $smkstatus.
		onacemeds yn.
		onarbmeds yn.
		onhypermeds yn.
		onlipidmeds yn.
		onstatinmeds yn.
		onantidepressivemeds yn.
	;
run;

/* get max and min values */
proc univariate data=alldata;
var volavesqrt;
run;

proc sgplot data=alldata;
series x=visit y=volavesqrt / group=studyid;
scatter x=visit y=volavesqrt;
run;

/* create wide dataset for proc traj */
data keep;
set alldata;
keep studyid visit age volavesqrt progression;
run;
proc print data=keep(obs=10); run;
proc sort data=keep; by studyid visit; run;
data wide;
retain v1-v4 c1-c4 p1-p4 a1-a4;
set keep;
by studyid;
if first.studyid then do;
  v1=.; v2=.; v3=.; v4=.;
  c1=.; c2=.; c3=.; c4=.;
  p1=.; p2=.; p3=.; p4=.;
  a1=.; a2=.; a3=.; a4=.;
end;
if visit='1' then do;
  v1=1;
  c1=volavesqrt;
  p1=progression;
  a1=age;
end;
else if visit='2' then do;
  v2=2;
  c2=volavesqrt;
  p2=progression;
  a2=age;
end;
else if visit='3' then do;
  v3=3;
  c3=volavesqrt;
  p3=progression;
  a3=age;
end;
else if visit='4' then do;
  v4=4;
  c4=volavesqrt;
  p4=progression;
  a4=age;
end;
if last.studyid then output;
keep studyid v1-v4 c1-c4 p1-p4 a1-a4;
run;
proc print data=wide(obs=10); run;

/* save dataset for FCAP */
data data.wide_for_traj;
set wide;
run;

data cactraj;
set wide;
run;

/* Janet's latest code for trajectories */
proc traj data=cactraj outplot=op outstat=os out=of outest=oe itdetail;   
ID studyid; Var c1-c4; Indep v1-v4;
model cnorm; max 10000; ngroups 4; order 1 1 2 2 ;
*risk age_atonset;
*where missingdata=0;
run;
/*proc print data=op;run;*/
/*proc sgplot data=op;*/
/*      series x=t y= avg1 ;*/
/*      series x=t y= avg2 ;*/
/*      series x=t y= avg3 ;*/
/*      series x=t y= avg4 ;*/
/*run;*/
%trajplot(op,os,'No dropout','Cnorm model','Volavesqrt','Visit');

/* adding dropout */
/* first without any covariates for the dropout model */
proc traj data=cactraj outplot=op outstat=os out=of outest=oe itdetail;   
ID studyid; Var c1-c4; Indep v1-v4;
model cnorm; max 10000; ngroups 4; order 1 1 2 2 ;
*risk age_atonset;
*where missingdata=0;
dropout (2 2 2 2);
run;
/*proc print data=op;run;*/
/*proc sgplot data=op;*/
/*      series x=t y= avg1 ;*/
/*      series x=t y= avg2 ;*/
/*      series x=t y= avg3 ;*/
/*      series x=t y= avg4 ;*/
/*run;*/
%trajplot(op,os,'With dropout','Cnorm model','Volavesqrt','Visit');

/* adding covariates */








/* old code */




/*****************************************/
/* MODEL SELECTION FOLLOWING KLIJN ET AL */
/* 1ST SELECT # CLASSES                  */
/* THEN ORDER OF POLYNOMIALS             */
/*****************************************/

/* OUTCOME: VOLAVESQRT */
/* BASED ON FCAP OUTPUT, COULD USE 3, 4, OR 5 GROUPS */


/* 5 classes linear */
proc traj data=wide outplot=op outstat=os out=of outest=oe itdetail;
id studyid; 
var c1-c4;
indep v1-v4;
model cnorm;
min 0;
max 72;
ngroups 5;
order 1 1 1 1 1;
run;
ods pdf file="H:\BDC\CACTI\CACTI twelve year progression\Report\fig1.pdf";
%trajplot(op,os,'Volavesqrt vs. visit, 5 classes, linear','Cnorm model','Volavesqrt','Visit');
ods pdf close;
proc print data=of noobs; run;
proc print data=os; run;
proc print data=oe; run;

/* 4 classes linear */
proc traj data=wide outplot=op outstat=os out=of outest=oe itdetail;
id studyid; 
var c1-c4;
indep v1-v4;
model cnorm;
min 0;
max 72;
ngroups 4;
order 1 1 1 1;
run;
ods pdf file="H:\BDC\CACTI\CACTI twelve year progression\Report\fig1.pdf";
%trajplot(op,os,'Volavesqrt vs. visit, 4 classes, linear','Cnorm model','Volavesqrt','Visit');
ods pdf close;
proc print data=of noobs; run;
proc print data=os; run;
proc print data=oe; run;

/* 3 classes linear */
proc traj data=wide outplot=op outstat=os out=of outest=oe itdetail;
id studyid; 
var c1-c4;
indep v1-v4;
model cnorm;
min 0;
max 72;
ngroups 3;
order 1 1 1;
run;
ods pdf file="H:\BDC\CACTI\CACTI twelve year progression\Report\fig1.pdf";
%trajplot(op,os,'Volavesqrt vs. visit, 3 classes, linear','Cnorm model','Volavesqrt','Visit');
ods pdf close;
proc print data=of noobs; run;

/* OUTCOME: PROGRESSION */
/* BASED ON FCAP OUTPUT, 2 GROUPS IS BEST */

options macrogen symbolgen mprint;
proc print data=wide; run;

/* 2 classes linear */
proc traj data=wide outplot=op outstat=os out=of outest=oe itdetail;
id studyid; 
var p1-p4;
indep v1-v4;
model logit;
ngroups 2;
order 1 1 ;
run;
ods pdf file="H:\BDC\CACTI\CACTI twelve year progression\Report\fig1.pdf";
%trajplot(op,os,'progression vs. visit, 2 classes, linear','Logit model','progression','Visit');
ods pdf close;
proc print data=of noobs; run;

