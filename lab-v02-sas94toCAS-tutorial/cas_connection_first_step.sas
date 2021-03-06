options cashost="viya34smp.nordiclab.sashq-r.openstack.sas.com";
cas casauto;

caslib mycaslib datasource=(srctype="path") path="/sasdata/caslibs/testdata" sessref=casauto;

libname mycas cas caslib=mycaslib;

/* 
Use casutil to:
- drop a table in a caslib
- load a table in a caslib
- save a table in a caslib
- list files in a caslib 
*/
proc casutil incaslib="mycaslib";
   list files; 
   droptable casdata="IRIS" incaslib="mycaslib" quiet;
run;

data mycas.iris;
   set sashelp.iris;
run; 
proc print data=mycas.iris(obs=5) ; run;

proc casutil incaslib="mycaslib" outcaslib="mycaslib"; 
   save casdata="iris" replace; 
run;

proc casutil incaslib="mycaslib";
   list files; 
run;

/* 
Use cas to:
- load a table in a caslib
- call the simple.correlation function
*/
proc cas;
	table.loadtable caslib="mycaslib" path="iris.sashdat" casOut={name="IRIS"};
	simple.correlation result=x table={groupBy={"Species"}, name="IRIS", orderBy={"SepalLength"}};
run;
	print x;
run; 


proc gradboost data=mycas.iris outmodel=mycas.gradboost_model;
	input sepalLength sepalWidth petalLength petalWidth / level = interval;
	target Species /level=nominal;
	output out=mycas.score_at_runtime;
	ods output FitStatistics=fit_at_runtime;
run;