/*--------------------------------------------------------------------
Name: dropoutplot.sas     
Author: Jones BL, Feb 2020         
----------------------------------------------------------------------
Plot estimated dropout from a trajectory model with dropout option

Parameters:
    Name of outplot= dataset
    Name of outstat= dataset
    Label for Y axis (quoted, default is 'Dropout')
    Label for X axis (quoted, default is 'T') 

Examples: 
	%dropoutplot(op,os)   
    %dropoutplot(op,os,'Dropout Probability','Age')
---------------------------------------------------------------------*/

%macro dropoutplot(PlotFile,StatFile,Ylab,Xlab);
	
	%local Cnt GpPcts;
	%local pi1 pi2 pi3 pi4 pi5 pi6 pi7 pi8 pi9 pi10;
	%local pi11 pi12 pi13 pi14 pi15 pi16 pi17 pi18 pi19 pi20;
	%local maxcolor col1 col2 col3 col4 col5 col6 col7 col8;
	%local i j clr dline;
	
	goptions reset=global gunit=pct cback=white 
		colors=(black blue green red orange purple olive vigb)
		htitle=6 htext=3 ftext=zapf border;  
  
	data _plotfile_;
		set &PlotFile;
	run;

	%CntObs(&PlotFile)
	%let Cnt=&ObsCnt;
	
/*  Colors -- cycle back after maxcolor */
	
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
	
	%if %length(&Ylab)=0 %then %let Ylab='Dropout';
	%if %length(&Xlab)=0 %then %let Xlab='T';
	
	/* Create dropout * t plot line */
	%LET dline=;
	%DO i=1 %TO &Cnt;
		%LET dline=%STR(&dline dropoutprobg&i*t);
	%END;
    
	/* Get group percentages */
	%GetPIs
	%let GpPcts=;
	%do i=1 %to &Cnt;
		%let GpPcts=%str(&GpPcts %'&&pi&i%');
	%end;
  
	legend1 label=('Group %') value=(%unquote(&GpPcts)) across=&Cnt;  
 
	proc gplot data=_plotfile_;
	    format t 12.2 dropoutprobg1: 4.2;
		plot &dline/ overlay legend=legend1;
		label t=&Xlab;
		label dropoutprobg1=&Ylab;
	run;
	%OUT:
	
	proc datasets nolist;
		delete _plotfile_;
	run;
	
	quit:
%mend dropoutplot;

/* Macro to find number of times in plot file */  
%macro CntObs(PltData);
	%global ObsCnt;
	%let ObsCnt=0;
	proc contents data=&PltData noprint out=CntTmp(keep=name);
	run;
	data _null_;
		retain icnt 0;
		set CntTmp;
		if index(name,"PRED")>0 then icnt=icnt+1;
		call symput('ObsCnt',left(put(icnt,12.)));
	run;
	proc datasets nolist;
		delete CntTmp;
	run;
%mend CntObs;

/* Get group percentage from stat file */
%macro GetPI;
  data _null_;
    set &StatFile;
    call symput('pi'||left(put(_n_,1.)),left(put(pi,4.1)));
  run;
%mend GetPI;