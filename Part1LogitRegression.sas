%web_drop_table(WORK.BOXING);
FILENAME REFFILE "C:/Users/danie/Documents/GitHub/6372BoxingProject/train.csv";
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.BOXING;
	GETNAMES=YES;
RUN;
/* PROC PRINT data=boxing; */

ods graphics on;
proc corr data=boxing plots=matrix(histogram) PLOTS(MAXPOINTS=9999);
run;
ods graphics off;

proc sgpanel data=boxing; 
panelby stance; 
reg x=WinPA y=WinPB / group=binaryresult alpha = .05 CLM CLI;

PROC logistic data= boxing;
class stance;
model binaryresult = AdvAgeA AdvHeightA AdvReachA AdvWgtA WinPA WinPB KoAPer KoBPer KoAPer*KoBPer;
output out=boxinglogregout predprobs=I p=probpreb;
run;

PROC logistic data= boxing;
class stance;
model binaryresult = AdvAgeA WinPA WinPB KoAPer KoBPer KoAPer*KoBPer;
output out=boxinglogregout predprobs=I p=probpreb;
run;

PROC logistic data= boxing;
class Stance Over35AgeA	Over35AgeB Over15lbA Over15lbB;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B AdvAgeA AdvHeightA AdvReachA AdvWgtA WinPA WinPB KoAPer KoBPer
/ selection = stepwise;
output out=boxinglogregout predprobs=I p=probpreb;
run;

PROC logistic data= boxing;
class binaryresult;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B AdvAgeA AdvHeightA AdvReachA AdvWgtA WinPA WinPB KoAPer KoBPer
/ selection = stepwise;
output out=boxinglogregout predprobs=I p=probpreb;
run;

PROC logistic data= boxing;
class Stance Over35AgeA	Over35AgeB Over15lbA Over15lbB;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B AdvAgeA AdvHeightA AdvReachA AdvWgtA WinPA WinPB KoAPer KoBPer
/ selection = backward;
output out=boxinglogregout predprobs=I p=probpreb;
run;

proc gplot data=boxinglogregout;
plot resdev*VAR1;
plot pearres*VAR1;
run; quit;
 