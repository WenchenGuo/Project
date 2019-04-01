/* Scoring program */
proc score data=bank.sdtrain8 score=estfile type=parms out=bank.score_sdtrain;
	var pdays2 age1 campaign3 nr_employed1 day_of_week1 day_of_week3 job1 default1 
		contact1 month1 month3--month4;
run;

quit;

/* Score*1000 */
data bank.score_sdtrain;
	set bank.score_sdtrain;
	score=int(score*1000);
run;

quit;



/* Score frequency */
proc format;
	value score 1000-high='1000 or more' 951-1000='951-1000' 901-950='901-950' 
		851-900='851-900' 801-850='801-850' 751-800='751-800' 701-750='701-750' 
		651-700='651-700' 601-650='601-650' 551-600='551-600' 501-550='501-550' 
		451-500='451-500' 401-450='401-450' 351-400='351-400' 301-350='301-350' 
		251-300='251-300' 201-250='201-250' 151-200='151-200' 101-150='101-150' 
		51-100='51-100' 0-50='0-50';
run;

/* Score crosstab */
ODS html file="/home/u35567854/bankproject/12_score(train).html";

proc freq data=bank.score_sdtrain;
	tables score*good/norow nopercent;
	format score score.;
run;

ODS html close;