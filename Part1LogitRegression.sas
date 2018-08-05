%web_drop_table(WORK.BOXING);
FILENAME REFFILE "C:/Users/danie/Documents/GitHub/6372BoxingProject/train.csv";
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.BOXING;
	GETNAMES=YES;
RUN;
/* PROC PRINT data=boxing; */

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
class Stance Over35AgeA	Over35AgeB Over15lbA Over15lbB;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B AdvAgeA AdvHeightA AdvReachA AdvWgtA WinPA WinPB KoAPer KoBPer
/ selection = stepwise
slentry=.3
slstay=.35;
output out=boxinglogregout predprobs=I p=probpreb;
run;

PROC logistic data= boxing;
class Stance Over35AgeA	Over35AgeB Over15lbA Over15lbB;
model binaryresult = age_A age_B height_A height_B reach_A reach_B weight_A weight_B won_A won_B lost_A lost_B kos_A kos_B AdvAgeA AdvHeightA AdvReachA AdvWgtA WinPA WinPB KoAPer KoBPer
/ selection = backward;
/* slentry=.3 */
/* slstay=.35; */
output out=boxinglogregout predprobs=I p=probpreb;
run;

proc gplot data=boxinglogregout;
plot resdev*VAR1;
plot pearres*VAR1;
run; quit;
 