/*Assign libname*/
libname bank "/home/u35567854/bankproject";


proc import datafile="/home/u35567854/bankproject/bank-additional-full.csv" 
	out=bank.data dbms=CSV replace;
	delimiter=";";
	getnames=yes;
	;
run;
/* Import bank term deposit data n = 41188 p = 20 */


proc print data=bank.data(obs=10);
run;
/* Show first 10 observations */



/* ---------------------------------------------------------------------------------------------------- */
data bank.data;
	set bank.data;
	if y="yes" then
		good=1;
	else
		good=0;
run;
/* Transfer campaign results 'yes' or 'no' into 0 and 1 */


data bank.train bank.valid;
	set bank.data;
	random1=ranuni(14380132);
	if random1<0.5 then
		output bank.train;
	else
		output bank.valid;
run;
/* Randomly split data into train and valid sets */
/* bank.train# = 20454 bank.valid# = 20734 */


data bank.no;
	set bank.train;
	if y="yes" then
		delete;
run;
/* Sample down no deposit data(good=0), while bank all deposit data(good=1) */
/* bank.no# = 18125 */

data bank.yes;
	set bank.train;
	if y="no" then
		delete;
run;
/* bank.yes# = 2329 */


data bank.nodata bank.yesdata;
	set bank.no;
	random1=ranuni(14380132);
	if random1<0.127 then
		output bank.nodata;
	else
		output bank.yesdata;
run;
/* bank.nodata# = 2285(p<0.127) bank.yesdata # =1840 */


/* bank.sdtrain# = 2329 + 2285 = 4614 */
data bank.sdtrain;
	set bank.yes bank.nodata;
run;