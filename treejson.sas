data cars;
	set sashelp.cars;
	count=1;
	keep _character_ count;
	drop model;
run;

%macro tree_json_data(
			intbl=,
			varseq=,
			sumvar=count,
			outtbl=output
			);
	%let nvarseq=%sysfunc(countw(&varseq.));
	%let aggrelist=;

	proc summary data=&intbl. nway;
		var &sumvar.;
		output out=caraggr0(keep = tot_cnt) sum=tot_cnt;
	run;

	%do k=1 %to &nvarseq.;
		%let aggrelist=&aggrelist. %scan(&varseq., &k.);

		proc summary data=&intbl. nway;
			class &aggrelist.;
			var &sumvar.;
			output out=caraggr&k.(keep = &aggrelist. tot_cnt) sum=tot_cnt;
		run;

	%end;

	data &outtbl.;
		set   caraggr&nvarseq.-caraggr1  caraggr0;
	run;

	proc datasets nolist;
		delete caraggr&nvarseq.-caraggr1  caraggr0;
	quit;

	proc sort  data=&outtbl.;
		by &varseq.;
	run;



%mend;
%let varseq=Origin DriveTrain Make Type;

%tree_json_data(
			intbl=cars,
			varseq=&varseq.,
			sumvar=count,
			outtbl=output
			);


%let jsonheader=flare;
data jsonfile;
	format Jsoncode $2000.;
	set output;
	by &varseq.;
	if _n_=1 then 
		do;
			jsoncode='{';
		 	output;
		    jsoncode=cats('"name":','"',"&jsonheader.",":",tot_cnt,'"');
		    output;	
		end;
	else
		do;
		end;
run;
