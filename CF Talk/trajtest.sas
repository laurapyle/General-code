/*--------------------------------------------------------------------
Name: trajtest.sas     
Author: Jones BL, Feb 2004         

2/14/2020 - suppress proc reg output; set Chi-Stat = F-Stat * DF
if quantile call fails due to small p (=> large denominator df)
----------------------------------------------------------------------
----------------------------------------------------------------------
The trajtest macro performs a Wald chi-square test about model 
parameters estimated in a PROC TRAJ procedure call.
 
Each equation specifies a linear hypothesis to be tested. The multiple 
specifications of a joint hypothesis are separated by commas. 

Parameter names are found in the OUTEST= file and work file 'sscpttz'

Examples: 

%trajtest('linear1=linear2=linear3')   
%trajtest('linear1=linear2=0')   
%trajtest('linear1=linear2, quadra1=quadra2')   
---------------------------------------------------------------------*/

%macro trajtest(Test1);

%let lngth=%length(&Test1)-2;
%let tmp=%substr(&Test1,2,&lngth);

proc contents noprint data=sscpttz out=_sscptt_(keep=name);
run;

data _xxyxyy_;
  retain icnt 0;
  set _sscptt_;
  if name="_NAME_" | name="_TYPE_" | name="Intercept" | name="YY" 
	then delete; else icnt=icnt+1; 
  call symput('temp'||left(put(icnt,2.)),put(name,32.));
  call symput('VarCnt',left(put(icnt,2.)));
run;

%let sscpvars=;
%do i=1 %to &VarCnt;
   %let sscpvars=%str(&sscpvars &&temp&i);
%end;

ods exclude all;

proc reg data=sscpttz;
 model yy = &sscpvars / noint;
 test &tmp / print; 
 ODS OUTPUT TestANOVA=_capxrx_;
run;

ods exclude none;

data _capxr1_; set _capxrx_(obs=1);
 ChiValue=quantile('CHISQ',1-ProbF,DF);
 if ( missing(ChiValue) ) then do;
  ChiValue=Fvalue * DF; end;
 ProbChi=ProbF; drop Source MS Control Fvalue ProbF Model Test; 
 Requested_Test=&Test1;
run;

proc print data=_capxr1_ noobs;
run;

%mend trajtest;