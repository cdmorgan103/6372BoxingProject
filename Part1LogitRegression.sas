/* Assumptions */

/* 	Multivariate normal distribution for entire set of variables */
/* 	Univariate normal distribution on response */
/* 	Linear relationships between scores on Y and scores on X for all variables */
/* 	Uniform error variances for response (Y) across all values of X */

/************************* Part 1: Simple Thoughless Model Selection Without Interactions *************************/

%web_drop_table(WORK.BOXING);
FILENAME REFFILE "C:/Users/danie/Documents/GitHub/6372BoxingProject/train.csv";
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.BOXING;
	GETNAMES=YES;
RUN;
/* PROC PRINT data=boxing; */

/* Simple Model Selection (Without Interactions) */
/* Stepwise */
PROC logistic data= boxing;
class Stance Over35AgeA	Over35AgeB Over15lbA Over15lbB;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B 
/ selection = stepwise;
output out=boxinglogregout predprobs=I p=probpreb;
run;
/* Forward */
PROC logistic data= boxing;
class Stance Over35AgeA	Over35AgeB Over15lbA Over15lbB;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B 
/ selection = forward;
output out=boxinglogregout predprobs=I p=probpreb;
run;
/* Backward */
PROC logistic data= boxing;
class Stance Over35AgeA	Over35AgeB Over15lbA Over15lbB;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B 
/ selection = backward;
output out=boxinglogregout predprobs=I p=probpreb;
run;

/* Candidate 1  add the accompanying variables */
PROC logistic data= boxing;
model binaryresult = age_A age_B won_A won_B lost_A lost_B height_A height_B reach_A reach_B /LACKFIT CTABLE;
output out=boxinglogregout predprobs=I p=probpreb resdev=resdev reschi=pearres;
run;


/* Chosen Model with a touch of common sense */
PROC logistic data= boxing plots(only label)=(leverage dpc);
model binaryresult = age_A age_B won_A won_B lost_A lost_B  /LACKFIT CTABLE;
output out=boxinglogregout predprobs=I p=probpreb resdev=resdev reschi=pearres;
run;


/* Analyze outliers */
DATA boxing; 
   SET boxing;
   obsno=_n_;
   RUN;

proc print data=boxing;
   where obsno = 634;
run;
proc print data=boxing;
   where obsno = 1438;
run;
proc print data=boxing;
   where obsno = 523;
run;
proc print data=boxing;
   where obsno = 560;
run;

/* Remove outliers and run again */
DATA boxingRemovedOutliers; 
   SET boxing;
   IF obsno = 634 THEN DELETE;
   IF obsno = 1438 THEN DELETE;
   IF obsno = 523 THEN DELETE;
   IF obsno = 560 THEN DELETE;
   RUN;

/* Choose Model with a touch of common sense */
PROC logistic data= boxingRemovedOutliers plots(only label)=(leverage dpc);
model binaryresult = age_A age_B won_A won_B lost_A lost_B  /LACKFIT CTABLE;
output out=boxinglogregout predprobs=I p=probpreb resdev=resdev reschi=pearres;
run;

/* Test Model */
%web_drop_table(WORK.BOXINGTEST);
FILENAME REFFILE "C:/Users/danie/Documents/GitHub/6372BoxingProject/test.csv";
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.BOXINGTEST;
	GETNAMES=YES;
RUN;

proc logistic data=BOXINGTEST rocoptions(crossvalidate) plots(only)=roc;
         model binaryresult(event="0") = age_A age_B won_A won_B lost_A lost_B;
        run;
