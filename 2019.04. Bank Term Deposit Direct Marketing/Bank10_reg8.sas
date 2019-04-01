/* Dummy coding8 */
Data bank.sdtrain8;
	set bank.sdtrain;

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

/* Regression eighth try (this is the final try). */
/* All variables remain p < 0.1. This is our final model.  */
proc reg data=bank.sdtrain8 outest=estfile;
	score: model Good=pdays2 age1 campaign3 nr_employed1 day_of_week1 day_of_week3 
		job1 default1 contact1 month1 month3--month4;
	run;
quit;
