/* macro trajplot: plot proc traj results 				*/
/* H. Seltman and J. Lam, 1/19/98  						*/
/* B. Jones 2/23/2020 extend to 20 groups, add colors	*/

/* Parameters:
    Name of outplot= dataset (no quotes)
    Name of outstat= dataset (no quotes)
    Title (in quotes)
    Subtitle (in quotes)
    Label for Y axis (in quotes, default is 'Outcome')
    Label for X axis (in quotes, default is 'T')
*/

/* Sample calls:
    proc traj outplot=op outstat=os;
      ...
    run;
    %trajplot(op,os,'Main Title','Subtitle','y-axis text','x-axis text') 
    %trajplot(op,os,'Main Title',' ','y-axis text','x-axis text') 
    %trajplot(op,os,'Main Title','Subtitle','y-axis text') 
    %trajplot(op,os,'Main Title','Subtitle')
    %trajplot(op,os,'Main Title')
*/

%macro trajplot(PlotFile,StatFile,Title1,Title2,Ylab,Xlab);
	%local Cnt GpPcts;
	%local pi1 pi2 pi3 pi4 pi5 pi6 pi7 pi8 pi9 pi10;
	%local pi11 pi12 pi13 pi14 pi15 pi16 pi17 pi18 pi19 pi20;
	%local maxcolor col1 col2 col3 col4 col5 col6 col7 col8;
	%local i j clr aline pline;

	goptions reset=global gunit=pct cback=white
		colors=(black blue green red orange purple olive vigb) 
		htitle=6 htext=3 ftext=zapf border;  
	%CntPred(&PlotFile)
	%let Cnt=&PredCnt;
 
  /* Table of colors -- cycles back through used colors after maxcolor */
	%let maxcolor=8;
	%let col1=%STR(red);
	%let col2=%STR(green);
	%let col3=%STR(blue);
	%let col4=%STR(black);
	%let col5=%STR(orange);
  	%let col6=%STR(purple);
	%let col7=%STR(olive);
	%let col8=%STR(vigb);

	%DO i=%EVAL(&maxcolor + 1) %TO &Cnt;
		%let j=%EVAL(&i - &maxcolor);
		%let clr=&&col&j;
		%let col&i=&clr;
	%END;

	%DO i=1 %TO &Cnt;
		%let clr=&&col&i;
		symbol&i color=&clr interpol=join value=&i. height=3; 
	%END;
  
	%DO i=1 %TO &Cnt;
		%let clr=&&col&i;
		symbol1&i color=&clr interpol=join line=2;
	%END;

	%if %length(&Ylab)=0 %then %let Ylab='Outcome';
	%if %length(&Xlab)=0 %then %let Xlab='T';

	/* Create avg*t and pred*t lines */
	%LET aline=;
	%LET pline=;
	%DO i=1 %TO &Cnt;
		%LET aline=%STR(&aline avg&i*t);
		%LET pline=%STR(&pline pred&i*t);
	%END;

	/* Get group percentages */
	%GetPIs
	%let GpPcts=;
  
	%do i=1 %to &Cnt;
		%let GpPcts=%str(&GpPcts %'&&pi&i%');
	%end;
	
	%do i=1 %to &Cnt;
		%let GpPcts=%str(&GpPcts %' %');
	%end;
  

	/* Make plots */
	legend1 label=('Group Percents') value=(%unquote(&GpPcts)) across=&Cnt;  
 
	proc gplot data=&PlotFile;
		title1 &Title1;
		title2 &Title2;
		format t 12.2 avg1 12.2;
		plot &aline &pline / overlay legend=legend1;
		label t=&Xlab;
		label avg1=&Ylab;
	run;

	%OUT:
	quit; 
%mend trajplot;


/* Macro to find number of times in plot file */  
%macro CntPred(PltData);
  %global PredCnt;
  %let PredCnt=0;
  proc contents data=&PltData noprint out=CPredTmp(keep=name);
  run;
  data _null_;
    retain icnt 0;
    set CPredTmp;
    if index(name,"PRED")>0 then icnt=icnt+1;
    call symput('PredCnt',left(put(icnt,12.)));
  run;
  proc datasets nolist;
    delete CPredTmp;
  run;
%mend CntPred;


/* Macro to get group percentages from stat file */
%macro GetPIs;
  data _null_;
    set &StatFile;
    call symput('pi'||left(put(_n_,1.)),left(put(pi,4.1)));
  run;
%mend GetPIs;
