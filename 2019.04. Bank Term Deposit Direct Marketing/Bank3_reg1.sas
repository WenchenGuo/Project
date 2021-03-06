/* Dummy coding1 (50 dummies in total) */
Data bank.sdtrain1;
	set bank.sdtrain;

	if age >=0 and age <=31 then
		age1=1;
	else
		age1=0;
	if age >=55 then
		age2=1;
	else
		age2=0;
		
	if pdays >=10 and pdays <=25 then
		pdays1=1;
	else
		pdays1=0;
	if pdays>=26 then
		pdays2=1;
	else
		pdays2=0;

	if campaign=1 then
		campaign1=1;
	else
		campaign1=0;

	if campaign >=4 and campaign <=5 then
		campaign2=1;
	else
		campaign2=0;

	if campaign >=6 then
		campaign3=1;
	else
		campaign3=0;

	if previous=1 then
		previous1=1;
	else
		previous1=0;

	if previous >=2 then
		previous2=1;
	else
		previous2=0;

	if emp_var_rate>=-0.1 then
		emp_var_rate1=1;
	else
		emp_var_rate1=0;

	if cons_price_idx<=92.843 then
		cons_price_idx1=1;
	else
		cons_price_idx1=0;

	if 92.893<=cons_price_idx<=93.075 then
		cons_price_idx2=1;
	else
		cons_price_idx2=0;

	if 93.2<=cons_price_idx<=93.876 then
		cons_price_idx3=1;
	else
		cons_price_idx3=0;

	if euribor3m<=0.782 then
		euribor3m1=1;
	else
		euribor3m1=0;

	if 0.788<=euribor3m<=1.044 then
		euribor3m2=1;
	else
		euribor3m2=0;

	if 1.046<=euribor3m<=1.264 then
		euribor3m3=1;
	else
		euribor3m3=0;

	if 1.266<=euribor3m<=4.021 then
		euribor3m4=1;
	else
		euribor3m4=0;

	if nr_employed<=5076.2 then
		nr_employed1=1;
	else
		nr_employed1=0;

	if 5099.1<=nr_employed<=5176.3 then
		nr_employed2=1;
	else
		nr_employed2=0;

	/* day of week */
	if day_of_week='tue' then
		day_of_week1=1;
	else
		day_of_week1=0;

	if day_of_week='fri' then
		day_of_week2=1;
	else
		day_of_week2=0;

	if day_of_week='mon' then
		day_of_week3=1;
	else
		day_of_week3=0;

	/* job */
	if job="retired" or job="student" then
		job1=1;
	else
		job1=0;

	if job="admin." or job="housemaid" then
		job2=1;
	else
		job2=0;

	if job="unemployed" then
		job3=1;
	else
		job3=0;

	if job="blue-collar" or job="services" or job="entrepreneur" then
		job4=1;
	else
		job4=0;

	/* marital */
	if marital="single" then
		marital1=1;
	else
		marital1=0;

	if marital="married" then
		marital2=1;
	else
		marital2=0;

	if marital="divorced" then
		marital3=1;
	else
		marital3=0;

	/* education */
	if education="unknown" then
		education1=1;
	else
		education1=0;

	if education="university.degree" then
		education2=1;
	else
		education2=0;

	if education="basic.4y""basic.6y" then
		education3=1;
	else
		education3=0;

	if education="basic.9y" then
		education4=1;
	else
		education4=0;

	/* default */
	if default="no" then
		default1=1;
	else
		default1=0;

	/* housing */
	if housing="yes" then
		housing1=1;
	else
		housing1=0;

	if housing="no" then
		housing2=1;
	else
		housing2=0;

	/* loan */
	if loan="no" then
		loan1=1;
	else
		loan1=0;

	if loan="yes" then
		loan2=1;
	else
		loan2=0;

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

	if month="sep" then
		month2=1;
	else
		month2=0;

	if month="oct" then
		month3=1;
	else
		month3=0;

	if month="apr" then
		month4=1;
	else
		month4=0;

	if month="jul" or month="may" or month="nov" then
		month5=1;
	else
		month5=0;

	/* poutcome */
	if poutcome="success" then
		poutcome1=1;
	else
		poutcome1=0;

	if poutcome="nonexistent" then
		poutcome2=1;
	else
		poutcome2=0;
run;
quit;


/* Regression first try */
proc reg data=bank.sdtrain1 outest=estfile;
	score: model Good = pdays1--pdays2 age1--age2 campaign1--campaign3 
		previous1--previous2 emp_var_rate1 cons_price_idx1--cons_price_idx3 
		euribor3m1--euribor3m4 nr_employed1--nr_employed2 day_of_week1--day_of_week3 
		job1--job4 marital1--marital3 education1--education4 default1 
		housing1--housing2 loan1--loan2 contact1 month1--month5 poutcome1--poutcome2;
	run;
quit;


/* Remove those which are linearly combined to other variables
- 'nr_employed2' 'education3' 'loan2' 'poutcome2' */




