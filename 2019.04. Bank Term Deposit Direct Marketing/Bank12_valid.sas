/* K-S test */
/* Final score card - Final regression output with parameters */
/* Now apply final regression model on valid data */


/* Sample down no deposit data(good=0), while bank all deposit data(good=1)*/
/* bank.no# = 18423 */
data bank.no;
	set bank.valid;

	if y="yes" then
		delete;
run;

/* bank.yes# = 2311 */
data bank.yes;
	set bank.valid;

	if y="no" then
		delete;
run;


/* bank.nodata# = 2316 bank.yesdata#= 16107 */
data bank.nodata bank.yesdata;
	set bank.no;
	random1=ranuni(14380132);

	if random1<0.127 then
		output bank.nodata;
	else
		output bank.yesdata;
run;


data bank.nodata bank.yesdata;
	set bank.no;
	random1=ranuni(14380132);
	if random1<0.127 then
		output bank.nodata;
	else
		output bank.yesdata;
run;
/* bank.nodata# = 15840 bank.yesdata # =2285(p<0.127) */




/* bank.sdvalid# = 2316 + 2311 = 4627 */
data bank.sdvalid;
	set bank.yes bank.nodata;
run;



/* Dummy coding8 for valid data */
Data bank.sdvalid8;
	set bank.sdvalid;

	if pdays>=26 then
		pdays2=1;
	else
		pdays2=0;

	if age >=0 and age <=31 then
		age1=1;
	else
		age1=0;

	if campaign >=6 then
		campaign3=1;
	else
		campaign3=0;

	if nr_employed<=5076.2 then
		nr_employed1=1;
	else
		nr_employed1=0;

	/* day of week */
	if day_of_week='tue' then
		day_of_week1=1;
	else
		day_of_week1=0;

	if day_of_week='mon' then
		day_of_week3=1;
	else
		day_of_week3=0;

	/* job */
	if job="retired" or job="student" then
		job1=1;
	else
		job1=0;

	/* default */
	if default="no" then
		default1=1;
	else
		default1=0;

	/* contact */
	if contact="cellular" then
		contact1=1;
	else
		contact1=0;

	/* month */
	if month="mar" or month="dec" then
		month1=1;
	else
		month1=0;

	if month="oct" then
		month3=1;
	else
		month3=0;

	if month="apr" then
		month4=1;
	else
		month4=0;
run;

quit;




/* Apply regression on valid data */
proc reg data=bank.sdvalid8 outest=estfile;
	score: model Good=pdays2 age1 campaign3 nr_employed1 day_of_week1 day_of_week3 
		job1 default1 contact1 month1 month3--month4;
	run;
quit;





/* Scoring program */
proc score data=bank.sdvalid8 score=estfile type=parms out=bank.score_sdvalid;
	var pdays2 age1 campaign3 nr_employed1 day_of_week1 day_of_week3 job1 default1 
		contact1 month1 month3--month4;
run;

quit;

/* Score*1000 */
data bank.score_sdvalid;
	set bank.score_sdvalid;
	score=int(score*1000);
run;




/* Score crosstab */
ODS html file="/home/u35567854/bankproject/14_score(valid).html";

proc freq data=bank.score_sdvalid;
	tables score*good/norow nopercent;
	format score score.;
run;

ODS html close;
