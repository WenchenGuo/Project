/* 1_frequency_tables */
ODS html file="/home/u35567854/bankproject/1_frequency_tables.html";

/* Check frequency tables of all variables */
proc freq data=bank.train (drop=duration y good random1);
run;

ODS html close;



/* format */
proc format;
	value age 0-28='0 to 28' 29-31='29 to 31' 32-33='32 to 33' 34-35='34 to 35' 
		36-38='36 to 38' 39-41='39 to 41' 42-45='42 to 45' 46-49='46 to 49' 
		50-54='50 to 54' 55-HIGH='55 or more';
	value campaign 1='1' 2='2' 3='3' 4-5='4 to 5' 6-HIGH='6 or more';
	value duration 0-36='0 to 36' 37-59='37 to 59' 60-74='60 to 74' 
		75-88='75 to 88' 89-102='89 to 102' 103-116='103 to 116' 117-130='117 to 130' 
		131-145='131 to 145' 146-162='146 to 162' 163-179='163 to 179' 
		180-199='180 to 199' 200-221='200 to 221' 222-248='222 to 248' 
		249-280='249 to 280' 281-319='281 to 319' 320-368='320 to 368' 
		369-438='369 to 438' 439-549='439 to 549' 550-746='550 to 746' 
		747- HIGH='747 or more';
	value pdays 0-3='0 to 3' 4-5='4 to 5' 6-9='6 to 9' 10-25='10 to 25' 
		26-HIGH='26 or more';
	value previous 0='0' 1='1' 2-HIGH='2 or more';
	value emp_var_rate low--3='low to -3'
		-2.9='-2.9'
		-1.8='-1.8'
		-1.7--0.2='-1.7 to -0.2'
		-0.1='-0.1' 1.1='1.1' 1.2-HIGH='1.2 or more';
	value cons_price_idx low-92.843='low to 92.843' 
		92.893-93.075='92.893 to 93.075' 93.2-93.369='93.2 to 93.369' 
		93.444-93.876='93.444 to 93.876' 93.918='93.918' 
		93.994-94.215='93.994 to 94.215' 94.465-HIGH='94.465 or more';
	value cons_conf_idx low--47.1='low to -47.1'
		-46.2--45.9='-46.2 to -45.9'
		-42.7='-42.7' 
		-42='-42'
		-41.8--37.5='41.8 to -37.5'
		-36.4='-36.4'
		-36.1-HIGH='-36.1 or more';
	value euribor3m low-0.782='low to 0.782' 0.788-1.044='0.788 to 1.044' 
		1.046-1.264='1.046 to 1.264' 1.266-1.291='1.266 to 1.291' 
		1.299-1.334='1.299 to 1.334' 1.344-1.406='1.344 to 1.406' 
		1.41-4.021='1.41 to 4.021' 4.076-4.153='4.076 to 4.153' 
		4.191-4.855='4.191 to 4.855' 4.856='4.856' 4.857-4.86='4.857 to 4.86' 
		4.864-4.957='4.864 to 4.957' 4.958-4.959='4.958 to 4.959' 
		4.96-4.961='4.96 to 4.961' 4.962='4.962' 4.963='4.963' 
		4.964-4.966='4.964 to 4.966' 4.967- HIGH='4.967 or more';
	value nr_employed low-5076.2='low to 5076.2' 5099.1-5176.3='5099.1 to 5176.3' 
		5191='5191' 5195.8='5195.8' 5196-HIGH='5196 or more';
run;



/* Apply formats on numeric variables */
/* 2_num_crosstab */
ODS html file="/home/u35567854/bankproject/2_num_crosstab.html";

proc freq data=bank.sdtrain;
	tables duration*good/norow nopercent;
	format duration duration.;
run;

proc freq data=bank.sdtrain;
	tables pdays*good/norow nopercent;
	format pdays pdays.;
run;

proc freq data=bank.sdtrain;
	tables age*good/norow nopercent;
	format age age.;
run;

proc freq data=bank.sdtrain;
	tables campaign*good/norow nopercent;
	format campaign campaign.;
run;

proc freq data=bank.sdtrain;
	tables previous*good/norow nopercent;
	format previous previous.;
run;

proc freq data=bank.sdtrain;
	tables emp_var_rate*good/norow nopercent;
	format emp_var_rate emp_var_rate.;
run;

proc freq data=bank.sdtrain;
	tables cons_price_idx*good/norow nopercent;
	format cons_price_idx cons_price_idx.;
run;

proc freq data=bank.sdtrain;
	tables cons_conf_idx*good/norow nopercent;
	format cons_conf_idx cons_conf_idx.;
run;

proc freq data=bank.sdtrain;
	tables euribor3m*good/norow nopercent;
	format euribor3m euribor3m.;
run;

proc freq data=bank.sdtrain;
	tables nr_employed*good/norow nopercent;
	format nr_employed nr_employed.;
run;

ODS html close;








/* character variables */
/* 3_num_crosstab */
ODS html file="/home/u35567854/bankproject/3_char_crosstab.html";

proc freq data=bank.sdtrain;
	tables job*good/norow nopercent;
run;

proc freq data=bank.sdtrain;
	tables marital*good/norow nopercent;
run;

proc freq data=bank.sdtrain;
	tables education*good/norow nopercent;
run;

proc freq data=bank.sdtrain;
	tables default*good/norow nopercent;
run;

proc freq data=bank.sdtrain;
	tables housing*good/norow nopercent;
run;

proc freq data=bank.sdtrain;
	tables loan*good/norow nopercent;
run;

proc freq data=bank.sdtrain;
	tables contact*good/norow nopercent;
run;

proc freq data=bank.sdtrain;
	tables month*good/norow nopercent;
run;

proc freq data=bank.sdtrain;
	tables day_of_week*good/norow nopercent;
run;

proc freq data=bank.sdtrain;
	tables poutcome*good/norow nopercent;
run;

ODS html close;

