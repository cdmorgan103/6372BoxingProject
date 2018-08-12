/* Assumptions */

/* 	Multivariate normal distribution for entire set of variables */
/* 	Univariate normal distribution on response */
/* 	Linear relationships between scores on Y and scores on X for all variables */
/* 	Uniform error variances for response (Y) across all values of X */


%web_drop_table(WORK.BOXING);
FILENAME REFFILE "C:/Users/danie/Documents/GitHub/6372BoxingProject/train.csv";
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.BOXING;
	GETNAMES=YES;
RUN;
PROC PRINT data=boxing;

/* Model Selection */
PROC logistic data= boxing;
class Stance Over35AgeA	Over35AgeB Over15lbA Over15lbB;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B AdvAgeA AdvHeightA AdvReachA AdvWgtA WinPA WinPB KoAPer KoBPer
/ selection = stepwise;
output out=boxinglogregout predprobs=I p=probpreb;
run;

/* Chosen Model*/
PROC logistic data= boxing plots(only label)=(leverage dpc);
model binaryresult = lost_A lost_B won_B WinPA AdvAgeA /LACKFIT CTABLE;
output out=boxinglogregout predprobs=I p=probpreb resdev=resdev reschi=pearres;
run;

proc print data=boxing;
   where obsno = 3426;
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


/* Remove outliers and run again */
DATA boxingRemovedOutliers; 
   SET boxing;
   IF obsno = 3426 THEN DELETE;
   IF obsno = 2901 THEN DELETE;
   IF obsno = 3689 THEN DELETE;
   IF obsno = 2856 THEN DELETE;
   RUN;

PROC logistic data= boxingRemovedOutliers plots(only label)=(leverage dpc);
model binaryresult = lost_A lost_B won_B WinPA AdvAgeA /LACKFIT CTABLE;
output out=boxinglogregoutRemovedOutliers predprobs=I p=probpreb resdev=resdev reschi=pearres;
run;

/* Test */
%web_drop_table(WORK.BOXINGTEST);
FILENAME REFFILE "C:/Users/danie/Documents/GitHub/6372BoxingProject/test.csv";
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.BOXINGTEST;
	GETNAMES=YES;
RUN;

proc logistic data=BOXINGTEST rocoptions(crossvalidate) plots(only)=roc;
         model binaryresult(event="0") = lost_A lost_B won_B WinPA AdvAgeA;
        run;












