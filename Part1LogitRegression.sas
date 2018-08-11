%web_drop_table(WORK.BOXING);
FILENAME REFFILE "C:/Users/danie/Documents/GitHub/6372BoxingProject/train.csv";
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.BOXING;
	GETNAMES=YES;
RUN;
PROC PRINT data=boxing;

ods graphics on;
proc corr data=boxing plots=matrix(histogram) PLOTS(MAXPOINTS=9999);
run;
ods graphics off;

proc sgpanel data=boxing; 
panelby stance; 
reg x=WinPA y=WinPB / group=binaryresult alpha = .05 CLM CLI;


PROC logistic data= boxing;
class Stance Over35AgeA	Over35AgeB Over15lbA Over15lbB;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B AdvAgeA AdvHeightA AdvReachA AdvWgtA WinPA WinPB KoAPer KoBPer
/ selection = stepwise;
output out=boxinglogregout predprobs=I p=probpreb;
run;

/* Chosen */
PROC logistic data= boxing plots(only label)=(leverage dpc);
model binaryresult = lost_A lost_B won_B WinPA AdvAgeA /LACKFIT CTABLE;
output out=boxinglogregout predprobs=I p=probpreb resdev=resdev reschi=pearres;
run;

/* Candidate 1  remove age_B */
/* PROC logistic data= boxing; */
/* model binaryresult = lost_A lost_B won_B WinPA /LACKFIT CTABLE; */
/* output out=boxinglogregout predprobs=I p=probpreb resdev=resdev reschi=pearres; */
/* run; */
/*  */
/* Candidate 1.5 add age A */
/* PROC logistic data= boxing; */
/* model binaryresult = age_A age_B lost_A lost_B won_B WinPA /LACKFIT CTABLE; */
/* output out=boxinglogregout predprobs=I p=probpreb resdev=resdev reschi=pearres; */
/* run; */
/*  */
/* Candidate 2 age A and B interaction */
/* PROC logistic data= boxing; */
/* model binaryresult = lost_A lost_B won_B WinPA age_A*age_B  /LACKFIT CTABLE; */
/* output out=boxinglogregout predprobs=I p=probpreb resdev=resdev reschi=pearres; */
/* run; */

proc gplot data=boxinglogregout;
plot resdev*obsno;
plot pearres*obsno;
run; quit;

proc print data=boxinglogregout;
   where pearres > 2.5;
run;

DATA boxinglogregoutAge; 
   SET boxinglogregout;
   KEEP age_A age_B obsno;
RUN;

PROC SORT data=boxinglogregoutage;
BY age_B;
ods graphics off;
title 'Fighter Ages';
proc boxplot data=boxinglogregoutage;
plot age_A*age_B;
   inset min mean max stddev /
      header = 'Overall Statistics'
      pos    = tm;
   insetgroup min max /
      header = 'Extremes by Age';
run;
