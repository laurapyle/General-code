proc import datafile="C:\Mac\Home\Documents\Temp\trajectories.csv"
        out=data
        dbms=csv
        replace;
run;
proc print;
run;

proc traj data=data outplot=op outstat=os out=of outest=oe itdetail;
ID study_id;
var _0 - _85500;
model cnorm; 
run;
