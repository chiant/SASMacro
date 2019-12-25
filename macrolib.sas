libname mlib 'D:\Code\SAS\Package';
/****上面为SAS包sasmacr.sas7bcat存放的路径*****/
options mstored sasmstore=mlib;

%macro trimlastspchar(intxt=,trimchar=%str(/)) 
		/ store des='0001.Trim Last Special Character';
	%if "%substr(&intxt.,%length(&intxt.),1)"="&trimchar." %then
		%substr(&intxt.,1,%eval(%length(&intxt.)-1));
	%else &intxt.;
%mend;

%macro del_sysfile(sysfilenm=)
		/ store des='0002.Delete System File';
	%let tmpfilenm=&sysfilenm.;
	filename tmpfile "&tmpfilenm.";

	data _null_;
		rc=fdelete('tmpfile');
	run;

%mend;

%macro Log_Output_Start(logdir=&LogDirNm.,logfilenm=sasrun)
		/ store des='0003.Log Output Start';
	%let logdir=%trimlastspchar(intxt=&logdir.);
	%let lognm=&logdir./&logfilenm..log;
	%del_sysfile(sysfilenm=&lognm.);
	filename logfile "&lognm.";

	proc printto log=logfile;run;quit;
%mend;

%macro Log_Output_End
		/ store des='0004.Log Output End';
	proc printto;run;quit;

%mend;

%macro generate_macro_code(dir=,infile=,outfile=)
		/ store des='0005.Generate Macro Code';
	%let del=/;

	%if &sysscp=WIN %then
		%let del=\;
	%let noval=;

	%if &outfile=&noval. %then
		%let outfile=_&infile._;

	%if &dir=&noval. %then
		%let dir=&del.users&del.jemiolod;

	%del_sysfile(sysfilenm=&dir.&del.&outfile..sas);
	ods listing close;
	filename incode "&dir.&del.&infile..sas" lrecl=2048;
	filename mprint "&dir.&del.&outfile..sas" lrecl=2048;
	options mfile mprint;

	%include incode;
	options nomprint;
	ods listing;
%mend;



%macro create_new_var_nm(
	invarnm=,
	outvarnm=,
	varprefix=,
	varsuffix=,
	varheaderlen=10,
	mode=DATASTEP,
	maxvarlen=28
	)
		/ store des='0006.Create New Variable Name';
	%let invarnm=&invarnm.;
	%let outvarnm=&outvarnm.;
	%let varsuffix=&varsuffix.;
	%let varprefix=&varprefix.;
	%let mode=&mode.;
	%let varlenkeep=%eval(&maxvarlen.-%length(&varsuffix.)-%length(&varprefix.));
	%let varlenshift=%eval(&maxvarlen.-&varheaderlen.-%length(&varsuffix.)-%length(&varprefix.));

	%if %upcase(&mode)=DATASTEP %then
		%do;
			if length(&invarnm.)>&varlenkeep. then
				&outvarnm.="&varprefix."||strip(substr(&invarnm.,1,&varheaderlen.))||'_'||strip(substr(&invarnm.,
				length(&invarnm.)-&varlenshift.+1,&varlenshift))||"&varsuffix.";
			else &outvarnm.="&varprefix."||strip(&invarnm.)||"&varsuffix.";
		%end;

	%if %upcase(&mode)=MACRO %then
		%do;
			%global &outvarnm.;

			%if %length(&invarnm.)>&varlenkeep. %then
				%let &outvarnm.=&varprefix.%substr(&invarnm.,1,&varheaderlen.)_%substr(&invarnm.,%length(&invarnm.)-&varlenshift.+1,&varlenshift.)&varsuffix.;
			%else %let &outvarnm.=&invarnm.&varsuffix.;

			/*			%put ****in macro:&&&outvarnm.************;*/
		%end;

	/*%let thelongnm=aaa1234567890bbb1234567890ccc1234567890;*/
	/*%Create_New_VAR_Nm(invarnm=&thelongnm.,outvarnm=cutnm,varsuffix=_SP,varheaderlen=18,mode=MACRO);*/
	/*%put ****out of macro:&cutnm.************;*/
%mend;

%macro create_tbl_from_str(mstr=,outtbl=_valuelist_,outvarnm=col,outvarfmt=$200.)
		/ store des='0007.Create Table From String';
	%local _ELE_ Ele_Type;
	%let mstr=%sysfunc(compbl(&mstr.));

	%if %substr(&outvarfmt.,1,1)=$ %then
		%let Ele_type=C;
	%else %let Ele_Type=N;

	data &outtbl;
		format &outvarnm. &outvarfmt.;
		%let _ELE_ =1;

		%do %while(%scan(&mstr.,&_ELE_.,%str( ))^=);
			%let _TmpEle_=%scan(&mstr.,&_ELE_.,%str( ));

			%if &Ele_Type=C %then
				%do;
					&outvarnm.="&_TmpEle_.";
				%end;
			%else
				%do;
					&outvarnm.=&_TmpEle_.;
				%end;

			output;
			%let _ELE_ =%eval(&_ele_.+1);
		%end;
	run;

%mend;

%macro cond_delete(intbl=)
		/ store des='0008.Conditional Delete';
	%local droptblindex;
	%let intbl=%sysfunc(compbl(&intbl.));
	%let droptblindex=1;
	%let curdrop=%scan(&intbl.,&droptblindex.,%str( ));

	%do %while (%scan(&intbl.,&droptblindex.,%str( ))^=);
		%let curdrop=%scan(&intbl.,&droptblindex.,%str( ));

		%if %sysfunc(exist(&curdrop.)) %then
			%do;

				proc sql noprint;
					drop table &curdrop.;
				quit;

			%end;

		%let droptblindex=%eval(&droptblindex.+1);
	%end;
%mend;

%macro getOBS(intbl)
		/ store des='0009.Get N of OBS';
	%global _NOBS_;
	%let _NOBS_=0;
	%let fid=%sysfunc(open(&intbl.));
	%let _NOBS_=%sysfunc(attrn(&fid,NOBS));
	%let fid=%sysfunc(close(&fid));
%mend;


%macro getOBS2(intbl)
		/ store des='0010.Get N of OBS 2';
	%global _NOBS_;
	%let _NOBS_=0;
	%let fid=%sysfunc(open(&intbl.));
	%let _NOBS_=%sysfunc(attrn(&fid,NOBS));
	%let fid=%sysfunc(close(&fid));
	&_NOBS_.
%mend;


%macro getVarLabel(intbl=,invar=)
		/ store des='0011.Get Variable Label';
	%global _VLABEL_;
	%let _VLABEL_=;
	%let fid=%sysfunc(open(&intbl.));
	%let _VLABEL_=%sysfunc(varlabel(&fid,%sysfunc(varnum(&fid.,&invar.))));
	%let fid=%sysfunc(close(&fid));
%mend;

%macro getVarType(intbl=,invar=)
		/ store des='0012.Get Variable Type';
	%global _VTYPE_;
	%let _VTYPE_=;
	%let fid=%sysfunc(open(&intbl.));
	%let _VTYPE_=%sysfunc(vartype(&fid,%sysfunc(varnum(&fid.,&invar.))));
	%let fid=%sysfunc(close(&fid));
%mend;

%macro varexist(ds=,var=)
		/ store des='0013.Variable Exist Or Not';
	%local dsid rc;
	%let dsid=%sysfunc(open(&ds));

	%if &dsid. %then
		%do;
			%if %sysfunc(varnum(&dsid.,&var)) %then
				1;
			%else 0;
			%let dsid=%sysfunc(close(&dsid.));
		%end;
	%else 0;
%mend;

%macro getSysColorNm
		/ store des='0014.Get System Color Name';
	proc registry list startat="COLORNAMES";
	run;

%mend;

%macro runtblcode(codetbl=,codevar=,codefilenm=)
		/ store des='0015.Run Code From Table';

	%global_system_initialize;

	%save_options;
	%close_options;

	%if &GLC_Pass. %then
	%do;
		filename &codefilenm. temp lrecl=32767;
		data _null_;
			file &codefilenm.;
			set &codetbl.;
			put &codevar.;
		run;
	%end;
	%restore_options;
%mend;

%macro Auto_Dummy_Variable(
		tablename=,
		variablename=,
		outputtable=,
		MissingDummy=N,
		MaxLevel=10,
		delimiter=,
		keepOriginal=Y
		)/ store des='0016.Auto Dummy Variable';

	%local _N_Vars_ var_name variabletype _Value_Raw_List_ dsid _N_Var_values_ missing_dummy_name missing_dummy_label MissingMark i kkk;
	options nomprint;

	proc contents data=&tablename.(keep=&variablename.) noprint varnum 
		out=_var_list_(keep=name LABEL rename=(name=VariableName ));
	run;

	%if &tablename.^=&outputtable. %then
		%do;
			options mprint;

			data &outputtable.;
				set &tablename.;
			run;

			options nomprint;
		%end;

	%let _N_Vars_=0;
	%let fid=%sysfunc(open(work._var_list_));
	%let _N_Vars_=%sysfunc(attrn(&fid,NOBS));
	%let fid=%sysfunc(close(&fid));

	%if &_N_Vars_>0 %then
		%do;

			Data _null_;
				set _var_list_;
				call symputx('_Var_name_'||left(put(_n_,8.)),VariableName);
				call symputx('_Var_label_'||left(put(_n_,8.)),Label);
			run;

			%do kkk=1 %to &_N_Vars_.;
				%let Var_name=&&_Var_name_&kkk.;
				%let Var_label=&&_Var_label_&kkk.;
				%let var_label=&var_label;

				%if %length(&var_label)>0 %then
					%let dvlabelheader=&Var_name. - &Var_label.;
				%else %let dvlabelheader=&Var_name. -;
				%let dsid=%sysfunc(open(&tablename.,i));
				%let variabletype=%sysfunc(vartype(&dsid,%sysfunc(varnum(&dsid,&var_name.))));
				%let rc=%sysfunc(close(&dsid));
				%put &variabletype.;
				%let _Value_Raw_List_=&tablename.;

				%if %length(&delimiter.)^=0 %then
					%do;
						%let _Value_Raw_List_=_zzz_;

						data _zzz_(drop=_iii_);
							set &tablename.;
							_iii_=1;

							do until(scan(&var_name.,_iii_,"&delimiter.")="");
								_vvv_=scan(&var_name.,_iii_,"&delimiter.");
								output;
								_iii_=_iii_+1;
							end;

							drop &var_name.;
							rename _vvv_=&var_name.;
						run;

					%end;

				/*create list of distinct values*/
				proc sql noprint;
					create table _Value_distinct_List_ as
						select distinct &var_name. as Variable_Value label="Variable Value"
							from &_Value_Raw_List_.;
				quit;

				data _Value_distinct_List_;
					format variable_name $32.;
					set _Value_distinct_List_;
					format dummy_name $32.;
					format dummy_label $1000.;
					label Variable_name="Variable Name"
						dummy_name="Dummy Variable Name"
						dummy_label="Dummy Variable Label";

					%if &variabletype.=C %then
						%do;
							variable_value=propcase(variable_value);
						%end;

					variable_name="&var_name.";
					dummy_name=cats("DV_",substr("&var_name.",1,min(24,length("&var_name."))),_N_);
					dummy_label="&dvlabelheader."||" Value:"||strip(Variable_Value);
					where Variable_Value is not null;
				run;

				%let missing_dummy_name=DV_%substr(&var_name.,1,%sysfunc(min(21,%length(&var_name.))))_Missing;
				%let missing_dummy_label=Dummy Variable||&var_name.: Missing Value;
				%let _N_Var_values_=0;
				%let fid=%sysfunc(open(work._value_distinct_list_));
				%let _N_Var_values_=%sysfunc(attrn(&fid,NOBS));
				%let fid=%sysfunc(close(&fid));
				%put N of levels = &_N_Var_values_.;
				%put Max number of levels considered for coding dummy variables = &maxlevel.;

				%if "&MaxLevel"="" %then
					%let Maxlevel=1000;

				%if &_N_Var_values_.>0 and &_N_Var_values_.<=&maxlevel. %then
					%do;

						Data _null_;
							set _value_distinct_list_;

							%if &variabletype.=N %then
								%do;
									call symputx('_var_value_'||left(put(_n_,8.)),Variable_Value);
								%end;

							%if &variabletype.=C %then
								%do;
									call symputx('_var_value_'||left(put(_n_,8.)),cats('"', Variable_Value, '"'));
								%end;

							call symputx('_dummyname_'||left(put(_n_,8.)),dummy_name);
							call symputx('_dummylabel_'||left(put(_n_,8.)),dummy_label);
						run;

						%if &variabletype.=N %then
							%let MissingMark=.;

						%if &variabletype.=C %then
							%let MissingMark="";
						options mprint;

						Data &outputtable.;
							set &outputtable.;
							_var_temp_=&var_name.;

							%if &variabletype.=C %then
								%do;
									_var_temp_=propcase(_var_temp_);
								%end;

							if _var_temp_^= &MissingMark. then
								do;
									%if %length(&delimiter.)^=0 %then
										%do;
											%do i=1 %to &_N_Var_values_;
												&&_dummyname_&i.=0;
												label &&_dummyname_&i.="&&_dummylabel_&i.";
											%end;

											_iii_=1;

											do until(scan(_var_temp_,_iii_,"&delimiter.")='');
												_jjj_=scan(_var_temp_,_iii_,"&delimiter.");

												%if &variabletype.=C %then
													%do;
														_jjj_=propcase(_jjj_);
													%end;

												%do i=1 %to &_N_Var_values_;
													if _jjj_=&&_var_value_&i then
														&&_dummyname_&i.=1;
												%end;

												_iii_=_iii_+1;
											end;

											drop _iii_ _jjj_;
										%end;
									%else
										%do i=1 %to &_N_Var_values_;
											label &&_dummyname_&i.="&&_dummylabel_&i.";

											if _var_temp_=&&_var_value_&i then
												&&_dummyname_&i.=1;
											Else &&_dummyname_&i.=0;
										%end;
								end;

							%if %upcase(&MissingDummy)=Y %then
								%do;
									if _var_temp_= &MissingMark. then
										do;
											%do i=1 %to &_N_Var_values_;
												if &&_dummyname_&i.=. then
													&&_dummyname_&i.=0;
											%end;

											&missing_dummy_name=1;
										end;
									else &missing_dummy_name=0;
									label &missing_dummy_name.="&missing_dummy_label.";
								%end;

							%if %upcase(&keepOriginal)=N %then
								%do;
									drop &var_name.;
								%end;

							drop _var_temp_;
						run;

						options nomprint;
					%end;

				proc datasets library=work nolist;
					delete _Value_distinct_List_

					%if %length(&delimiter.)^=0 %then
						%do;
							_zzz_
						%end;
					;
				quit;

			%end;
		%end;

	proc datasets library=work nolist;
		delete _var_list_;
	quit;

%mend Auto_Dummy_Variable;

%macro PowerContents(input=,output=,mode=,datarole=)
		/ store des='0017.Calculate Statistics';
	%if %lowcase(&mode)=list %then
		%do;
			%let N_=;datadata _null_;
				set &input.;
				call symputx('lib_'||left(put(_n_,8.)),libname);
				call symputx('table_'||left(put(_n_,8.)),TableName);
				call symputx('var_'||left(put(_n_,8.)),VariableName);
				call symputx('N_',left(put(_n_,8.)));
			run;

			%if %length(&N_.)^=0 %then
				%do _x_=1 %to &N_.;

					proc contents data=&&lib_&_x_...&&table_&_x_ (keep=&&var_&_x_) noprint DETAILS
						out=_tmp_(keep=libname memname name type label length varnum format nobs 
						rename=(varnum=No memname=TableName name=VariableName));
					run;

					%if &_x_.=1 %then
						%do;

							data _variable_infor_;
								set _tmp_;
							run;

						%end;
					%else
						%do;

							data _variable_infor_;
								set _variable_infor_ _tmp_;
							run;

						%end;
				%end;
		%end;
	%else
		%do;

			proc contents data=&input. noprint DETAILS
				out=_variable_infor_(keep=libname memname name type label length varnum format nobs 
				rename=(varnum=No memname=TableName name=VariableName));
			run;

		%end;

	proc sort data=_variable_infor_;
		by no;
	run;

	quit;

	data _variable_infor_;
		retain no;
		set _variable_infor_;
		libname=propcase(libname);
		tablename=propcase(tablename);
		variablename=propcase(variablename);
	run;

	%let N_infor=;

	data _null_;
		set _variable_infor_;
		call symputx('lib_infor'||left(put(_n_,8.)),libname);
		call symputx('table_infor'||left(put(_n_,8.)),TableName);
		call symputx('var_infor'||left(put(_n_,8.)),VariableName);
		call symputx('var_type'||left(put(_n_,8.)),type);
		call symputx('var_format'||left(put(_n_,8.)),upcase(format));
		call symputx('N_infor',left(put(_n_,8.)));
	run;



	%if %length(&N_infor.)^=0 %then
		%do;
			%do _i_=1 %to &N_infor.;
				%put ******Power Content - Variable: &&var_infor&_i_, Task &_i_. of  &N_infor.************;

				data _tmp_var4_analy_;
					set &&lib_infor&_i_...&&table_infor&_i_ (keep = &&var_infor&_i_ );
				run;

				proc freq data=_tmp_var4_analy_ noprint;
					table &&var_infor&_i_/ out=_var_freq_ missing;
				run;

				proc sql noprint;
					create table _var_count_ as
						select 
							missing(&&var_infor&_i_) as Missing,
							max(percent) as MaxPercent, 
							sum(count) as N_obs, 
							count(*) as  N_value
						from _var_freq_
							group by Missing
								order by Missing
					;
				quit;

				proc sort data=_var_freq_;
					by descending count;
				run;

				data _value_list_;
					set _var_freq_;

					if _N_<=10;
					where &&var_infor&_i_ is not null;
				run;

				%let valuesample=;

				proc sql noprint;
					select &&var_infor&_i_. into:valuesample separated by ', '
						from _value_list_;
				quit;

				%let valuesample=&valuesample.;

				/*				%put *****&&var_infor&_i_.****&valuesample.************;*/
				data _var_count2_;
					set _var_count_ nobs=eff;
					format lib_name $32.;
					format table_name $32.;
					format variable_name $32.;
					retain Lib_name Table_name Variable_name 
						N_missing N_Non_missing N_dist_value  N_MaxPercent
						("&&lib_infor&_i_" "&&table_infor&_i_" "&&var_infor&_i_" 0 0 0 0);

					if missing=1 then
						N_missing=N_obs;

					if missing=0 then
						do;
							N_Non_missing=N_obs;
							N_dist_value=N_value;
							N_MaxPercent=MaxPercent;
						end;

					Lib_name=propcase(Lib_name);
					Table_name=propcase(Table_name);
					Variable_name=propcase(variable_name);
					format Value $1000.;
					Value ="&valuesample.";
					format Nmax 8.3;
					format Nmin 8.3;
					format integer_dummy 8.;
					Nmax=.;
					Nmin=.;
					integer_dummy=.;

					if _N_=eff then
						output;
					keep Lib_name Table_name value Variable_name N_Non_missing N_MaxPercent N_dist_value N_missing Nmax Nmin Integer_dummy;
				run;

				%let integer_check=0;

				%if &&var_type&_i_=1 %then %if %length(&&var_format&_i_)=0 %then
					%let integer_check=1;
				%else
					%if  
					%substr(&&var_format&_i_,1,4)=BEST or 
					%substr(&&var_format&_i_,1,1)=8 or 
					%substr(&&var_format&_i_,1,1)=4 or
					%substr(&&var_format&_i_,1,1)=7 or 
					%substr(&&var_format&_i_,1,1)=6 or
					%substr(&&var_format&_i_,1,1)=5 or 
					%substr(&&var_format&_i_,1,1)=3
					%then
					%let integer_check=1;

				%if &integer_check.=1 %then
					%do;

						proc summary data= _tmp_var4_analy_;
							var &&var_infor&_i_;
							output out=_var_stat_  mode=mode mean=mean   std=std  skew=skew kurt=kurt min=min p1=p1 p5=p5 p10=p10 p25=p25 p50=p50 p75=p75 p90=p90 p95=p95 p99=p99 max=max;
						run;

						proc sql noprint;
							create table _var_num_type_ as
								select
									max(&&var_infor&_i_) as Nmax format=8.3,
									min(&&var_infor&_i_) as Nmin format=8.3,
									min(&&var_infor&_i_=round(&&var_infor&_i_)) as integer_dummy
								from _tmp_var4_analy_
							;
						quit;

						data _var_count2_;
							merge _var_count2_(drop=Nmax Nmin integer_dummy) _var_num_type_ _var_stat_;
						run;

						proc sql noprint;
							drop table _var_num_type_, _var_stat_;
					%end;

				%if &_i_.=1 %then
					%do;

						data _tmp_;
							set _var_count2_;
						run;

					%end;
				%else
					%do;

						data _tmp_;
							set _tmp_ _var_count2_;
						run;

					%end;

				proc sql noprint; drop table _tmp_var4_analy_; quit;

			%end;
			
			proc sql noprint;
				create table &output. as
					select A.*,
						B.value,
						B.N_missing, 
						100* B.N_missing/A.nobs as Missing_P label="% of Missing",
						B.N_Non_missing,
						B.N_dist_value,
						B.N_MaxPercent as MaxPercent,
						B.Nmax label="Max(Numberical Variable)",
						B.Nmin label="Min(Numberical Variable)",
						B.integer_dummy label="integer"
						%if %varexist(ds=_tmp_,var=skew) %then
						%do;
						,B.mode
						,B.p50 as Median
						,B.mean
						,B.std
						,B.skew
						,B.kurt
						,B.min
						,B.p1
						,B.p5
						,b.p10
						,b.p25
						,b.p50
						,b.p75
						,b.p90
						,b.p95
						,b.p99
						,b.max
						%end;

					from _variable_infor_ A
						left join _tmp_ B
							on propcase(A.libname)=B.lib_name and
							propcase(A.tablename)=B.table_name and
							propcase(A.variablename)=B.variable_name
				;
			quit;

			data &output.;
				set &output.;
				libname=upcase(libname);
				tablename=upcase(tablename);
				variablename=upcase(variablename);
				format DATAROLE $50.;
				DATAROLE="%upcase(&DATAROLE.)";
			run;



			proc sort data=&output.;
				by no;
			run;

			quit;

			proc sql noprint;
				drop table _var_count_, _value_list_, _var_count2_, _tmp_,_var_freq_,_variable_infor_;
			quit;

		%end;
%mend;

%macro BatchLabel(
		_targetlib=,
		_targettbl=,
		_maplib=,
		_maptbl=,
		_varnm=,
		_labelnm=
		)/ store des='0018.Batch Label Creation';


	data _labelmap;
		set &_maplib..&_maptbl.;
		&_labelnm. = compress(&_labelnm.,'"');
		&_labelnm. = tranwrd(&_labelnm.,"%","百分比");
		re = prxparse('/^([a-zA-Z_])([a-zA-Z0-9_]*)$/');
		pos = prxmatch(re,strip(&_varnm.));

		if pos>0;
	run;

	data _null_;
		set _labelmap;
		call symput('varnm'||left(put(_N_,8.)),strip(&_varnm.));
		call symput('labelnm'||left(put(_N_,8.)),strip(&_labelnm.));
		call symput('NVAR',(put(_N_,8.)));
	run;

	proc datasets library = &_targetlib. nodetails nolist;
		modify &_targettbl.;
		label
			%local i;

		%do i = 1 %to &nvar.;
			%put &_targettbl.***&i.***&&varnm&i.=&&labelnm&i.;
			&&varnm&i. = "%unquote(&&labelnm&i.)"
		%end;
		;
	run;
	quit;

	proc sql noprint;
		drop table _labelmap;
	quit;

%mend;

%macro show_macro_compiled(libnm=work,output=_macrolist)
		/ store des='0019.Show Macro Compiled';

		PROC CATALOG catalog=&libnm..sasmacr ; 
			Contents out=&output.; 
		Run; 
%mend;


%macro save_options
	/ store des='0020.Save SAS System Options';

	%global macrogensave;
	%let macrogensave= %sysfunc(getoption(symbolgen)); 
	options nosymbolgen ;

	%global mlogicsave mprintsave notessave sourcesave source2save;

	%let mlogicsave=%sysfunc(getoption(mlogic)); 
	%let mprintsave=%sysfunc(getoption(mprint)); 
	%let notessave=%sysfunc(getoption(notes)); 
	%let sourcesave=%sysfunc(getoption(nosource)); 
	%let source2save=%sysfunc(getoption(nosource2)); 

	options &macrogensave.;

/*	%put &symbolgensave. &mlogicsave. &mprintsave.;*/
%mend;


%macro restore_options
	/ store des='0021.Restore SAS System Options';
	options nomprint;
	options nosymbolgen;
	options nonotes nomlogic  nosource nosource2;
	
	options  &mlogicsave. &mprintsave. &notessave. &sourcesave. &source2save.;
	%SYMDEL mlogicsave mprintsave notessave sourcesave source2save;

	options &macrogensave.;
	%SYMDEL macrogensave;
%mend;

%macro close_options
	/ store des='0022.Turn off SAS System Options';
	options nomprint;
	options nosymbolgen;
	options nomlogic;
	options nonotes;
	options nosource;
	options nosource2;
%mend;

%macro global_system_initialize
		/ store des='1000.Assign Global System Macro';

	%save_options;
	%close_options;

	%local extian;
	%let extian=21250;

	%global GLC_Pass;
	%let GLC_Pass=0;
	%let curdaynum=%SYSFUNC(date());

	%let serverdomain= %SYSGET(USERDNSDOMAIN);
	%let serverdomain=%upcase(&serverdomain.); 

	%let cur_usernm= %SYSGET(USERNAME);
	%let cur_usernm=%lowcase(&cur_usernm.); 

	%if &curdaynum.<=&extian. 
		and &serverdomain.=MSXF.COM 
		%then 
		%do;
			%if &cur_usernm.=guanghui.sun %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=xi.hu %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=yongying.xue %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=hui.fan %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=lei.deng %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=xi.cao %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=ling.lin %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=guangjun.huo %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=siyuan.yao %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=denis %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=yan.wang02 %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=si.chen01 %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=fan.liu %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=yu.leng %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=yuannian.li %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=kangsheng.huang %then
				%let GLC_Pass=1;
			%else %if &cur_usernm.=yu.gong %then
				%let GLC_Pass=1;
		%end;
	%restore_options;
%mend;

%macro create_model_global_macro
		/ store des='1001.Create Model Global Macro';

	%global ModelLibNm;
	%global ModelNm;
	%global MasterRawNm;
	%global TestRawNm;
	%global MasterNm;
	%global TrainNm;
	%global ValidNm;
	%global TestNm;
	%global RuleNm;
	%global DecisionNm;
	%global StatNm;
	%global BinStatNm;
	%global CorrNm;
	%global EstimateNm;
	%global ScoreParaNm;
	%global PriorNm;
	%global ModelResultNm;

	%global RespYNm;

	%global OverSampFlag;/*是否做Oversampling*/
	%global TrainPercent;/*训练样本抽样比例*/

	%global GSampleSeed; /*全局抽样种子*/

	%global GMaxFreqDropP;/*频数最大值所占百分比*/
	%global GMissingDropP;/*缺失值所占百分比*/
	%global GNumDVLvlN;/*数值变量做虚拟变量的最多取值个数*/
	%global GNumModeDVP;/*数值变量做众数虚拟变量的最少百分比*/
	%global Gmin_distinct_value;/*做平滑变换和极端值处理的最小distinct value个数*/
	%global GMinSkewness;/*需做平滑变换的最小偏度绝对值*/
	%global Gmin_factor;/*upper outlier multiplier*/
	%global Gmax_factor;/*lower outlier multiplier*/
	%global Gmaxpvalue;/*相关性变量筛选考虑Pvalue的最大值*/
	%global GN_Distinct_ChiSQ;/*考虑卡方检验最多的变量值个数*/
	%global GN_Distinct_Corr;/*考虑连续性相关检验最少的变量值个数*/
	%global GMin_IV;/*变量入模考虑最小的IV值*/
	%global GMaxN_Char;/*字符变量考虑入模的最多不同字符数值个数*/
	%global GPureBinIV;/*分箱中如出现纯度为100%的分箱，赋予的IV值*/

	%global AUTO_STEP_ADJ; /*是否自动调整Stepwise变量选择步数*/
	%global STEP_ADJ_MIN_PER; /*自动调整Stepwise步数ValidationASE最小百分比*/

	%global LogDirNm;
	%global LogFileNm;
	%global LogFileNmFul;

	%global ModelTitleNm;
	%global MethodNm;
	%global ModelDSN;
	%global ModelSuffix;

	%global methodid;
	%global method1 method2 method3 method4;

	%global RULE_DSN;

	%global macronullvalue;
	%GLOBAL BINPLOTDISABLE;
	%GLOBAL AUTOBINCALDISABLE;

	%global traindropvar;
	%global dropafterautobin;

	%global CharMiss_Rep;
	%global NumMiss_Rep;


	%global Char_SP_List;
	%global Num_SP_List;
	%global CharSP_Rep;
	%global NumSP_Rep;
	%global CharSP_DV;
	%global NumSP_DV;

	%global GMaxCharValue;
	%global GAutoChar2Num;

	%global GRuleOrder;
	%global GDecisionOrder;
	%global GStepOrder;

	%global PreExcludeVarlist;
	%global G_Common_Reset;
	%global G_Bin_Reset;

	%global common_suffix;

	%global INF_Replacer;

	%global PreIncludeVarList;
	
%mend;


%macro assign_global_model_macro
		/ store des='1002.Assign Model Global Macro';

	%let macronullvalue=;
	%let BINPLOTDISABLE=;
	%let AUTOBINCALDISABLE=;
	
	%let OverSampFlag=N;
	%let TrainPercent=0.5;
	%let GSampleSeed=2015;

	%let GMaxFreqDropP=95;/*频数最大值所占百分比*/
	%let GMissingDropP=95;/*缺失值所占百分比*/
	%let GNumDVLvlN=6;/*数值变量做虚拟变量的最多取值个数*/
	%let GNumModeDVP=50;/*数值变量做众数虚拟变量的最少百分比*/
	%let Gmin_distinct_value=10; /*做平滑变换和极端值处理的最小distinct value个数*/
	%let GMinSkewness=1; /*需做平滑变换的最小偏度绝对值*/
	%let Gmin_factor=0.75; /*upper outlier multiplier*/
	%let Gmax_factor=1.5; /*lower outlier multiplier*/
	%let Gmaxpvalue=0.2; /*相关性变量筛选考虑Pvalue的最大值*/
	%let GN_Distinct_ChiSQ=10; /*考虑卡方检验最多的变量值个数*/
	%let GN_Distinct_Corr=5; /*考虑连续性相关检验最少的变量值个数*/
	%let GMin_IV=0.02;/*变量入模考虑最小的IV值*/
	%let GPureBinIV=5;/*分箱中如出现纯度为100%的分箱，赋予的IV值*/

	%let GMaxN_Char=50;/*字符型变量处理和入模考虑的最多的数值个数*/

	%let AUTO_STEP_ADJ=Y;
	%let STEP_ADJ_MIN_PER=0.03;

	%let CharMiss_Rep=NA;/*做分箱时用的缺失值替代值*/
	%let NumMiss_Rep=-987654321;

	%let Char_SP_List=;
	%let Num_SP_List=;

	%let NumSP_Rep=0;
	%let CharSP_Rep=;

	%let CharSP_DV=Y;
	%let NumSP_DV=Y;
	%let GMaxN_Char=100;
	%let GAutoChar2Num=Y;

	%let INF_Replacer=1000000000;

	%let PreIncludeVarList=;


		/******模型训练全局参数*******/
	%let method1=FBin;
	%let method2=CBin;
	%let method3=DV;
	%let method4=DVBIN;

	%let RULE_DSN=MASTER;

	%let G_Common_Reset=Y;
	%let G_Bin_Reset=N;

	%let common_suffix=_Common;


%mend;

%macro setup_system_environment
		/ store des='1003.Assign Model Global Macro';
	options compress=yes reuse=yes;
	%global_system_initialize;
	%save_options;
	%close_options;
	%if &GLC_Pass. %then
	%do;
		%create_model_global_macro;
		%assign_global_model_macro;
	%end;
	%restore_options;
%mend;


%macro assign_global_model_io_macro(suffix=)
		/ store des='1004.Assign Model Global IO Macro';

	%global_system_initialize;
	%save_options;
	%close_options;

	%if &GLC_Pass. %then
	%do;
		%let MasterNm=Master&suffix.;
		%let TrainNm=Train&suffix.;
		%let ValidNm=Valid&suffix.;
		%let TestNm=Test&suffix.;
		%let RuleNm=Rule&suffix.;
		%let DecisionNm=Decision&suffix.;
		%let StatNm=Stat&suffix.;
		%let BinStatNm=BinStat&suffix.;
		%let CorrNm=Corr&suffix.;
		%let EstimateNm=Estimate&suffix.;
		%let ScoreParaNm=ScorePara&suffix.;
		%let PriorNm=Prior&suffix.;
		%let ModelResultNm=Result&suffix.;
		%let RULE_DSN=%upcase(&RULE_DSN);
	%end;
	%restore_options;

%mend;
%macro assign_local_model_macro
		/ store des='1005.Assign Model Local Macro';

	%global_system_initialize;
	%save_options;
	%close_options;
	%if &GLC_Pass. %then
	%do;
		%let MethodNm=&&Method&methodid.;
		%let modelsuffix=_&ModelDSN._&methodNm.;
		%let LogFileNmFul=&LogFileNm.&modelsuffix.;
		%let ModelTitleNm=&ModelNm &methodid.. &methodNm.;

	%end;
	%restore_options;
%mend;

%macro set_model_parameters
		/ store des='1006.Update Model Environment';
	%assign_local_model_macro;
	%put *************Current Model: &ModelTitleNm. Suffix: &modelsuffix.*******;

	%assign_global_model_io_macro(suffix=&modelsuffix.);
%mend;

%macro show_model_environment
		/ store des='1007.Show Model Environment';
	%put *************Current Model: &ModelTitleNm. Suffix: &modelsuffix.*******;
	%put 日志路径 LogDirNm=&LogDirNm.;
	%put 日志名 LogFileNm=&LogFileNm.;
	%put 日志全名 LogFileNmFul=&LogFileNmFul.;
	%put 模型数据库 ModelLibNm=&ModelLibNm.;
	%put 模型描述名称 ModelNm=&ModelNm.;
	%put 模型描述全名 ModelTitleNm=&ModelTitleNm.;
	%put 模型数据集名称 ModelDSN=&ModelDSN.;
	%put 训练验证宽表原始数据 MasterRawNm=&MasterRawNm.;
	%put 测试集原始数据 TestRawNm=&TestRawNm.;
	%put 因变量 RespYnm=&RespYnm.;
	%put 模型方法论ID methodid=&methodid.;
	%put 模型方法论 MethodNm=&MethodNm.;
	%put 模型数据后缀 modelsuffix=&modelsuffix.;
	%put 是否做Oversampling OverSampFlag=&OverSampFlag.;

	%put 训练集抽样百分比 TrainPercent=&TrainPercent.;
	%put 抽样种子 GSampleSeed=&GSampleSeed.;

	%put 频数最大值所占百分比 GMaxFreqDropP=&GMaxFreqDropP.;
	%put 缺失值所占百分比 GMissingDropP=&GMissingDropP.;
	%put 数值变量做虚拟变量的最多取值个数 GNumDVLvlN=&GNumDVLvlN.;
	%put 数值变量做众数虚拟变量的最少百分比 GNumModeDVP=&GNumModeDVP.;
	%put 做平滑变换和极端值处理的最小distinct Gmin_distinct_value=&Gmin_distinct_value.;
	%put 需做平滑变换的最小偏度绝对值 GMinSkewness=&GMinSkewness.;
	%put 下极端值盖帽因子 Gmin_factor=&Gmin_factor.;
	%put 上极端值盖帽因子 Gmax_factor=&Gmax_factor.;
	%put 相关性变量筛选考虑Pvalue的最大值 Gmaxpvalue=&Gmaxpvalue.;
	%put 考虑卡方检验最多的变量值个数 GN_Distinct_ChiSQ=&GN_Distinct_ChiSQ.;
	%put 考虑连续性相关检验最少的变量值个数 GN_Distinct_Corr=&GN_Distinct_Corr.;
	%put 变量入模考虑最小的IV值 GMin_IV=&GMin_IV.;
	%put 字符变量考虑入模的最多不同字符数值个数 GMaxN_Char=&GMaxN_Char.;

	%put 字符型特殊值列表 Char_SP_List=&Char_SP_List.;
	%put 数值型特殊值列表 Num_SP_List=&Num_SP_List.;

	%put 逐步回归步数自动调整 AUTO_STEP_ADJ=&AUTO_STEP_ADJ.;
	%put 逐步回归步数自动调整敏感度 STEP_ADJ_MIN_PER=&STEP_ADJ_MIN_PER.;

%mend;

%macro Prepare_model_metadata
		/ store des='1008.Create Model Metadata';

	%let ctrldroplist=&ModelLibNm..&DecisionNm. &ModelLibNm..&RuleNm. &ModelLibNm..&StatNm. &ModelLibNm..&BinStatNm. &ModelLibNm..&CorrNm.;

	%cond_delete(intbl=&ctrldroplist.);

	proc sql noprint;
		create table &ModelLibNm..&DecisionNm.(compress=CHAR  bufsize=12288)
			(
		ORDER num format=8. label='运行顺序',
			DATAROLE char(50) format=$50. label='决策数据类型',
			VARIABLENAME char(32) label='变量名称',
			LABEL char(1000) format=$1000. label='变量描述',
			DECISION char(100) format=$100. label='入模决策',
			INPUTMODE char(50) format=$50. label='决策生成模式',
			REASON char(100) format=$100. label='决策原因'
			);
		create table &ModelLibNm..&RuleNm.(compress=CHAR  bufsize=16384)
			(
		ORDER num format=8. label='运行顺序',
			DATAROLE char(50) format=$50. label='决策数据类型',
			VARIABLENAME char(32) format=$32. label='变量名称',
			LABEL char(1000) format=$1000. label='变量描述',
			RULE char(20000) format=$20000. label='规则',
			RULE_DESC char(1000) format=$1000. label='规则描述',
			RULE_RUN_MODE char(20) format=$20. label='规则运行模式',
			INPUTMODE char(50) format=$50. label='规则生成模式',
			TARGETVAR char(40) format=$40. label='输出变量名称',
			TARGETLABEL char(1000) format=$1000. label='输出变量描述'
			);
		create table &ModelLibNm..&StatNm.( compress=CHAR  bufsize=16384 )
			(
		ORDER num format=8. label='运行顺序',
			DATAROLE char(50) format=$50. label='决策数据类型',
			NO num format=8. label='编号',
			VARIABLENAME char(32) label='变量名称',
			LABEL char(256) label='变量描述',
			NOBS num format=8. label='记录数',
			TYPE num format=8. label='变量类型',
			N_DIST_VALUE num label='变量值个数',
			VALUE char(1000) format=$1000. label='变量频数取值前十',
			MODE num format=best32. label='众数',
			MAXPERCENT num format=best32. label='众数比例',
			N_MISSING num format=8. label='缺失值数量',
			MISSING_P num format=best32. label='缺失比例',
			N_NON_MISSING num format=8. label='非缺失值数量',
			INTEGER_DUMMY num format=8. label='整数标签',
			MEDIAN num format=best32.  label='中位数',
			MEAN num format=best32.  label='均值',
			STD num format=best32.  label='标准差',
			SKEW num format=best32. label='偏度',
			KURT num format=best32. label='峰度',
			MIN num format=best32.  label='最小值',
			P1 num format=best32.  label='1分位数',
			P5 num format=best32. label='5分位数',
			P10 num format=best32.  label='10分位数',
			P25 num format=best32.  label='25分位数',
			P50 num format=best32.  label='50分位数',
			P75 num format=best32.  label='75分位数',
			P90 num format=best32.  label='90分位数',
			P95 num format=best32.  label='95分位数',
			P99 num format=best32.  label='99分位数',
			MAX num format=best32.  label='最大值',
			LENGTH num label='变量长度',
			FORMAT char(32) label='变量输出格式'
			);
		create table &ModelLibNm..&CorrNm.( compress=CHAR  bufsize=16384 )
			(
		ORDER num format=8. label='运行顺序',
			DATAROLE char(50) format=$50. label='决策数据类型',
			/*			DECISION char(100) format=$100.,*/
			/*			REASON char(100) format=$100.,*/
			RANK num format=8.,
			VARIABLENAME char(32) label='Variable Name',
			LABEL char(256) label='Variable Label',
			df_chisq num format=2. label='ChiSQ: Degree of Freedom',
			corr_chisq num format=10.4 label='ChiSQ Value',
			pvalue_chisq num format=PVALUE6.4 label='ChiSQ P-Value',
			rank_chisq num,
			corr_Pearson num format=8.3 label='Pearson Correlation',
			pvalue_Pearson num format=PVALUE6.4 label='Pearson p-value',
			rank_Pearson num label='Pearson Rank*of Variables',
			corr_Spearman num format=8.3 label='Spearman Correlation',
			pvalue_Spearman num format=PVALUE6.4 label='Spearman p-value',
			rank_Spearman num label='Spearman Rank*of Variables',
			corr_Hoeffding num format=8.3 label='Hoeffding Correlation',
			pvalue_Hoeffding num format=PVALUE6.4 label='Hoeffding p-value',
			rank_Hoeffding num label='Hoeffding Rank*of Variables',
			INPUTMODE char(50) format=$50.
			);
		create table &ModelLibNm..&BinStatNm.( compress=CHAR  bufsize=16384 )
			(
		ORDER num format=8. label='Run Order',
			DATAROLE char(50) format=$50. label='Data Source',
			IV_RANK num format=8. label='IV Rank',
			RESP_VAR char(40) format=$40. label='Response Name',
			RESP_LAB char(1000) format=$1000. label='Response Label',
			VARIABLENAME char(32) format=$32. label='Variable Name',
			LABEL char(1000) format=$1000. label='Variable Label',
			VARTYPE char(1) format=$1. label='Variable Type',
			MAXPERCENT num format=8.3 label='Max Freq Percent',
			N_MISSING num format=8. label='N of Missing',
			MISSING_P num format=8.3 label='Percent of Missing',
			VALUE char(1000) format=$1000. label='Top 10 Values',
			N_DIST_VALUE num format=8. label='N of Distinct Values',
			N_BIN num format=8. label='N of Bins',
			BIN num format=8. label='Bin Number',
			BinName char(1000) format=$1000. label='Bin Name',

			OBS_P num format=PERCENTN8.2 label='% of OBS in Bin',
			BINY_P num format=PERCENTN8.2 label='Response Rate in Bin',
			N num format=8. label='N of OBS in Bin',
			NY1 num format=8. label='N of Response',
			MIN num format=8.3 label='Min of Bin',
			MAX num format=8.3  label='Max of Bin',
			MEANX num format=8.3 label='Mean of Bin',
			Y1_DIST num format=PERCENTN8.2 label='% of Response in Total Response',
			Y0_DIST num format=PERCENTN8.2 label='% of Non-Response in Total Non-Response',
			ELOGIT num format=8.3 label='ELOGIT',
			WOE num format=8.3 label='WOE',
			BINIV num format=8.4 label='BIN IV',
			MAXBINIV num format=8.3 label='Max Bin IV Value',
			MAXBINIV_P num format=PERCENTN8.2 label='Max Percent of Bin IV Value',
			IV num format=8.4 label='IV',
			BINVALUE char(1000) format=$1000. label='Value List in Bin (Categorical Variable Only)',
			NVALUE num format=8. label='N of Values in Bin (Categorical Variable Only)',
			BinLower num format=8.3 label='Lower Bound (not inclusive)',
			BinUpper num format=8.3 label='Upper Bound (inclusive)'
			);
	quit;

%mend;


%macro model_oversampling(
	indata=,
	outdata=&ModelLibNm..&MasterNm.,
	respvar=&RespYnm.,
	sampleseed=&GSampleSeed.
	)/ store des='1009.Model Oversampling';


	proc sql noprint;
			select sum(&respvar.) into: N_Y_1 from &indata.;
			select sum(1-&respvar.) into: N_Y_0 from &indata.;
	quit;

	%let samplevalueflag=%eval(&N_Y_0.-&N_Y_1.>0);


	data sample1;
		set &indata.;
		where &respvar.=&samplevalueflag.;
	run;

	proc surveyselect data=&indata.(where=(&respvar.=%eval(1-&samplevalueflag.))) noprint
		sampsize=&&N_Y_&samplevalueflag. out =sample2 seed = &sampleseed.;
	run;

	data &outdata.;
		set sample1 sample2;
	run;
	%cond_delete(intbl=sample1 sample2);

%mend;


%macro TrainValidSplit(
			sourcetbl=&ModelLibNm..&MasterNm.,
			DV=&RespYnm.,
			TrainRatio=&TrainPercent.,
			TrainName=&ModelLibNm..&TrainNm., 
			ValidName=&ModelLibNm..&ValidNm., 
			Seed=&GSampleSeed.)
		/ store des='1010.Train Valid Split';


	data _sample_;
		set &sourcetbl.;
	run;

	proc sort data=_sample_;
		by &DV.;
	run;

	proc surveyselect data=_Sample_ noprint
		samprate =&TrainRatio. out =_Sample_flagged_ seed = &Seed. outall;
		strata &DV.;
	run;

	proc freq data=_Sample_flagged_;
		table selected*&DV./ nopercent;
			label selected="1:Training, 0:Validation";
	run;

	data &TrainName.(drop=Selected SelectionProb SamplingWeight) 
		&ValidName.(drop=Selected SelectionProb SamplingWeight);
		set _Sample_flagged_;

		if selected then
			output &TrainName.;
		else output &ValidName.;
		run;

		proc datasets nolist;
			delete _Sample_flagged_ _Sample_ _SampleDV1_;
		quit;


%mend;

%macro Greenacre(intbl=,vary=,varx=,outbl=,nlvl=)
		/ store des='1011.Greenacre Method';
	/*level clustering technique*/

	/*reference:
	1. Greenacre, M. (1988a): Clustering rows and columns of a contingency table. Journal of Classification, 5, 39-51. 
	2. Greenacre, M. (1988b): Correspondence analysis of multivariate categorical data by weighted least squares. Biometrika, 75, 457-467. 
	3. Greenacre, M. (1993): Correspondence Analysis in Practice. Academic Press, London. 
	*/

	/*
	%let fid=%sysfunc(open(&intbl.));
	%let v_loc=%sysfunc(varnum(&fid,&varx.));
	%let varx_typ=%sysfunc(vartype(&fid,&v_loc.));
	%let fid=%sysfunc(close(&fid));
	*/
	proc means data=&intbl. nway maxdec=3 mean n;
		class &varx.;
		var &vary;
		output out=levels mean=prop;
	run;

	ods output clusterhistory=cluster;

	proc cluster data=levels method=ward outtree=fortree;
		freq _freq_;
		var prop;
		id &varx.;
	run;

	proc freq data=&intbl. noprint;
		table &varx.*&vary./chisq;
			output out=chi(keep=_pchi_) chisq;
	run;

	data cutoff;
		if _n_ = 1 then
			set chi;
		set cluster;
		chisquare=_pchi_*rsquared;
		degfree=numberofclusters-1;
		logpvalue=logsdf('CHISQ',chisquare,degfree);
		run;

		symbol i=join c=blue v=star;

		proc gplot data=cutoff;
			plot logpvalue*numberofclusters;
		run;

		%if "&nlvl."="" %then
			%do;

				proc means data=cutoff noprint;
					var logpvalue;
					output out=nlvl minid(logpvalue(numberofclusters))=ncl;
				run;

				data nlvl;
					set nlvl;
					call symput('ncl',ncl);
				run;

			%end;
		%else
			%do;
				%let ncl=&nlvl.;

				data nlvl;
					ncl=&nlvl.;
					output;
				run;

			%end;

		proc print data=nlvl noobs label;
			var ncl;
			label ncl='N of clusters';
		run;

		proc tree data=fortree nclusters=&ncl out=&outbl. h=rsq;
			id &varx;
		run;

		data _map_tmp;
			format DV $32.;
			format IV $32.;
			format value $1000.;
			format newvalue $1000.;
			set &outbl.;
			DV="&vary.";
			IV="&varx.";
			NewValue=cats('G',cluster);
			value= strip(&varx.);
			drop &varx cluster clusname;
			;
		run;

		proc sort data=_map_tmp;
			by newvalue value;
		run;

		data _map_tmp2;
			set _map_tmp;
			by newvalue;
			format longvalue $1000.;
			retain longvalue;

			if first.newvalue then
				do;
					longvalue=catx(' ',newvalue,cats('(',value));
				end;
			else
				do;
					longvalue=catx('|',longvalue,value);
				end;

			if last.newvalue then
				longvalue=cats(longvalue,')');

			if last.newvalue;
			keep longvalue newvalue;
		run;

		proc sql noprint;
			create table &outbl. as
				select A.DV,
					A.IV,
					A.value,
					B.longvalue as newvalue
				from _map_tmp A
					left join _map_tmp2 B
						on A.newvalue=B.newvalue
			;
		quit;

		proc datasets nolist;
			delete nlvl fortree levels cutoff chi cluster _map_tmp _map_tmp2;
		quit;

%mend;

%macro GreenacreVarRecoding(
		indata=,
		outdata=,
		maptbl=,
		summarytbl=,
		keeporginal=Y)
		/ store des='1012.Greenacre Method Recode';

	proc sort data=&maptbl.;
		by dv iv newvalue value;
	run;

	proc sql noprint;
		select distinct iv into: ivlist separated by ' ' 
			from &maptbl.;
	quit;

	proc sql noprint;
		create table _dvivlist_ as
			select distinct dv,iv 
				from &maptbl. 
					order by dv,iv;
	quit;

	data _null_;
		set &maptbl.;
		call symputx('X_var'||left(put(_n_,8.)),IV);
		call symputx('O_value'||left(put(_n_,8.)),value);
		call symputx('N_value'||left(put(_n_,8.)),newvalue);
		call symputx('N_recode',left(put(_n_,8.)));
	run;

	data _null_;
		set _dvivlist_;
		call symputx('X_iv'||left(put(_n_,8.)),IV);
		call symputx('Y_dv'||left(put(_n_,8.)),DV);
		call symputx('N_dviv',left(put(_n_,8.)));
	run;

	proc datasets nolist lib=work;
		delete _dvivlist_;
	quit;

	data &outdata.;
		%if %length(&N_recode.)^=0 and &N_recode.>0 %then
			%do m=1 %to &N_recode.;
				format &&x_var&m.. $1000.;
			%end;

		set &indata.;

		%if %length(&N_recode.)^=0 and &N_recode.>0 %then
			%do m=1 %to &N_recode.;
				format &&x_var&m.._G $1000.;
				%let fid=%sysfunc(open(&indata.));
				%let v_loc=%sysfunc(varnum(&fid,&&x_var&m.));
				%let v_typ=%sysfunc(vartype(&fid,&v_loc.));
				%let fid=%sysfunc(close(&fid));

				%if &v_typ.=C %then
					%let o_exp="&&O_value&m";
				%else %let o_exp=&&O_value&m;

				if &&x_var&m.=&o_exp. then
					&&x_var&m.._G="&&n_value&m.";
			%end;
	run;

	/***************Response Rate Analysis of Variable Recoding**********************/
	%cond_delete(intbl=_dv_stat_);
	%cond_delete(intbl=_dv_stat_o);

	%if %length(&N_dviv.)^=0 and &N_dviv.>0 %then
		%do k=1 %to &N_dviv.;

			proc means data=&outdata. nway mean n sum noprint;
				class &&x_iv&k.._g &&x_iv&k..;
				var &&y_dv&k.;
				output out=_tmp_o(rename=(&&x_iv&k.._g=group &&x_iv&k..=groupvalue) drop=_type_ _freq_) 
					mean=dvmean n=nrec sum=dvsum;
			run;

			proc means data=&outdata. nway mean n sum noprint;
				class &&x_iv&k.._g;
				var &&y_dv&k.;
				output out=_tmp_(rename=(&&x_iv&k.._g=group)) 
					mean=dvmean n=nrec sum=dvsum;
			run;

			data _tmp_;
				format dv $32.;
				format iv $32.;
				format group $1000.;
				format groupvalue $1000.;
				set _tmp_;
				dv="&&y_dv&k.";
				iv="&&x_iv&k";
				group=scan(group,1);
				label group="New Group";
			run;

			data _tmp_o;
				format dv $32.;
				format iv $32.;
				format group $1000.;
				format groupvalue $1000.;
				set _tmp_o;
				dv="&&y_dv&k.";
				iv="&&x_iv&k";
				group=scan(group,1);
				label groupvalue="Value";
				label group="New Group";
			run;

			%if &k.=1 %then
				%do;

					proc datasets nolist lib=work;
						change _tmp_=_dv_stat_;
					quit;

					proc datasets nolist lib=work;
						change _tmp_o=_dv_stat_o;
					quit;

				%end;
			%else
				%do;

					proc append base=_dv_stat_ data=_tmp_ force;
					run;

					quit;

					proc append base=_dv_stat_o data=_tmp_o force;
					run;

					quit;

					proc datasets nolist;
						delete _tmp_o delete _tmp_;
					quit;

				%end;
		%end;

	proc sql noprint;
		create table &summarytbl. as
			select
				A.dv,
				A.iv,
				A.group,
				B.dvmean as gdvmean label="Group Response Rate" format=percentn8.2,
				B.nrec as gnrec label="Group N",
				B.dvsum as gdvsum label="Group N of Response",
				A.groupvalue as groupvalue label="Value",
				A.dvmean label="Response Rate" format=percentn8.2,
				A.nrec label="N",
				A.dvsum label="N of Response"
			from _dv_stat_o A
				left join _dv_stat_ B 
					on A.dv=B.dv and
					A.iv=B.iv and
					A.group=B.group
				order by A.dv, A.iv, A.group
		;
	quit;

	%if %upcase(&keeporginal.)=N  and %length(&N_recode.)^=0 and &N_recode.>0 %then
		%do;

			data &outdata.;
				set &outdata.;

				%do m=1 %to &N_recode.;
					&&x_var&m=&&x_var&m.._G;
				%end;

				drop 
					%do m=1 %to &N_recode.;
				&&x_var&m.._G %str( )
		%end;
	;
			run;

%end;

			proc datasets nolist;
				delete _dv_stat_  _dv_stat_o;
			quit;

%mend;

%macro PrintGreenarce(sumtbl=)
		/ store des='1013.Print Greenacre Output';


	proc report data=&sumtbl. spanrows nowd;
		by iv;
		column  ('Group' group groupvalue) ('Y=1 Rate' gdvmean  dvmean) ('N of Rec' gnrec nrec) ('N (Y=1)' gdvsum dvsum);
		define group /order;
		define gdvmean /order;
		define gnrec /order;
		define gdvsum /order;
		label iv="Bin Variable" group='G' groupvalue='V' gdvmean='G' dvmean='V' gnrec='G' nrec='V' gdvsum='G' dvsum='V';
	run;

%mend;

%macro create_bin(
		indata=,
		outdata=,
		varnm=,
		cutpoint=,
		MissValueSymbol=MISS
		)
		/ store des='1014.Create Bin';
	/*Note: missing bin must be placed at the first place of cutpoint string*/
	%let cutpoint=%sysfunc(compbl(&cutpoint.));
	%let npoint=%sysfunc(countw(&cutpoint.,%str( )));
	%put ****&npoint.****;

	%getVarType(intbl=&indata.,invar=&varnm);
	%let varrawtype=%upcase(&_VTYPE_);
	%let missingbinflag=0;

	data &outdata.;
		set &indata.;
		format bin 8.;
		format &varnm._bin $100.;
		%let pointstart=1;

		%do i=1 %to &npoint.;
			%if %scan(&cutpoint.,&i.,%str( ))=&MissValueSymbol. %then
				%do;
					if missing(&varnm.) then
						do;
							&varnm._bin = "&MissValueSymbol.";
							bin=&i.;
						end;

					%let missingbinflag=1;
				%end;
			%else %if &pointstart=1 %then
				%do;
					%if &missingbinflag. %then
						%do;
							else
						%end;

					if 
						&varnm.<=%scan(&cutpoint.,&i.,%str( )) then
						do;
							&varnm._bin = "(-, %scan(&cutpoint.,&i.,%str( ))]";
							bin=&i.;
						end;

					%let pointstart=0;
				%end;
			%else
				%do;
					else if  &varnm.<=%scan(&cutpoint.,&i.,%str( )) then
						do;
							&varnm._bin = "(%scan(&cutpoint.,%eval(&i.-1),%str( )), %scan(&cutpoint.,&i.,%str( ))]";
							bin=&i.;
						end;
				%end;

			%if &i=&npoint. %then
				%do;
					else
						do;
							&varnm._bin = "(%scan(&cutpoint.,&i.,%str( )), +)";
							bin=%eval(&i.+1);
						end;
				%end;
		%end;
	run;

%mend;

/**/
/*data test;*/
/*set sashelp.class;*/
/*if mod(_N_,3)=0 then weight=.;*/
/*run;*/
/*%create_bin(*/
/*	indata=test,*/
/*	outdata=work.a,*/
/*	varnm=weight,*/
/*	cutpoint=MISS 80 90 100 110 120 );*/
%macro ManuBin(
	intbl=&ModelLibNm..&TrainNm.,
	outstat=,vary=&RespYnm.,
	varx=,
	cutpoint=,
	MissValueSymbol=MISS)
		/ store des='1015.Manually Create Bin';

	%global LastManualBinVar;
	%global AutoBinCalDisable;
	%global BinPlotDisable;
	%let varx=&varx.;
	%let cutpoint=&cutpoint.;
	%let MACRONULLVALUE=;

	%if &outstat.=&macronullvalue. %then
		%let outstat=bin_&varx.;

	%if &cutpoint^=&macronullvalue. %then
		%do;
			%create_bin(
				indata=&intbl.,
				outdata=_binned_,
				varnm=&varx.,
				cutpoint=&cutpoint.,
				MissValueSymbol=&MissValueSymbol.);

			data binrawdata;
				set _binned_;
				keep &varx. &varx._bin &vary. bin;
			run;

			%getVarLabel(intbl=&intbl.,invar=&varx);
			%let xlabel=&_VLABEL_;

			%getVarLabel(intbl=&intbl.,invar=&vary);
			%let ylabel=&_VLABEL_;

			%getVarType(intbl=&intbl.,invar=&varx);
			%let xtype=%upcase(&_VTYPE_);

			proc sql noprint;
				select 	sum(missing(&varx.)) into: MissN from binrawdata;
				select 	100*sum(missing(&varx.))/count(*) into: MissP from binrawdata;
			quit;

			proc sql noprint;
				create table varx as
					select count(distinct &varx.) as disN,
						count(*) as ObsN,
						sum(&vary) as NY1,
						sum(1-&vary) as NY0
					from binrawdata
				;
				select disN into:DistXN from varx;
				select obsN into:RecN from varx;
				select NY1 into:Y1N from varx;
				select NY0 into:Y0N from varx;
				drop table varx;
			quit;

			proc freq data=binrawdata noprint;
				table &varx./ out=_var_freq_ missing;
			run;

			proc sql noprint;
				select max(percent) into: MaxFreqP from _var_freq_;
			quit;

			proc sort data=_var_freq_;
				by descending count;
			run;

			data _value_list_;
				set _var_freq_;

				if _N_<=10;
				where &varx. is not null;
			run;

			%let valuesample=;

			proc sql noprint;
				select &varx. into:valuesample separated by ', '
					from _value_list_;
			quit;

			%let valuesample=&valuesample.;
			%let MaxFreqP=&MaxFreqP.;

			%cond_delete(intbl=_value_list_ _var_freq_ _binned_);
			%let distxn=&distxn.;
			%let recn=&recn.;
			%let y1n=&y1n.;
			%let y0n=&y0n.;
			%put ********Distinct Value of &VarX.: &DistXn.**********;
			%put ********Total Records: &RecN.**********;
			%put ********Total Y=1: &Y1N.**********;
			%put ********Total Y=0: &Y0N.**********;

			proc means data=binrawdata noprint nway maxdec=2;
				class bin &varx._bin;
				var &vary %if &xtype.=N %then

					%do;
						&varx
					%end;
				;
				output out=elogittmp2(drop=_TYPE_ rename=(_FREQ_=N)) 
				%if &xtype.=N %then

					%do;
						min(&varx.)=Min max(&varx.)=Max mean(&varx)=MeanX
					%end;

				sum(&vary)=NY1;
			run;

			proc sort data=elogittmp2;
				by bin;
			run;

			data &outstat.;
				format Resp_Var $40.;
				format Resp_Lab $1000.;
				format VariableName $32.;
				format Label $1000.;
				format VarType $1.;
				set elogittmp2;
				Resp_Var="%UPCASE(&vary)";
				Resp_Lab="&ylabel.";
				VariableName="%UPCASE(&varx)";
				Label="&xlabel.";
				VarType="%UPCASE(&xtype.)";
				format OBS_P percentn8.2;
				format Y1_DIST percentn8.2;
				format Y0_DIST percentn8.2;
				BinY_P=NY1/N;
				OBS_P=N/&RecN.;
				Y1_DIST=NY1/&Y1N.;
				Y0_DIST=(N-NY1)/&Y0N.;
				elogit=log((NY1+(sqrt(N)/2))/
					(N-NY1+(sqrt(N)/2)));
				woe=log(Y1_DIST/Y0_DIST);
				binIV=(Y1_DIST-Y0_DIST)*log(Y1_DIST/Y0_DIST);
				format N_DIST_VALUE 8.;
				format VALUE $1000.;
				format MAXPERCENT 8.3;
				format N_MISSING 8.;
				format MISSING_P 8.3;
				N_DIST_VALUE=&DistXN.;
				VALUE="&valuesample.";
				MAXPERCENT=&MaxFreqP.;
				N_MISSING=&MissN.;
				MISSING_P=&MissP.;
				format BinName $1000.;
				binname=&varx._bin;
				drop &varx._bin;
			run;

			proc sort data=&outstat.;
				by Bin;
			run;

			proc sql noprint;
				create table iv as
					select 
						count(*) as N_BIN,
						max(biniv) as MaxBinIV,
						max(biniv)/sum(biniv) as MaxBinIV_P,
						sum(biniv) as IV from &outstat.;
			quit;

			data &outstat.;
				set &outstat.;

				if _n_=1 then
					set IV;
			run;

			%if &xtype.=N %then
				%do;

					proc sort data=&outstat.;
						by bin;
					run;

					data &outstat.;
						set &outstat. end=lastobs;
						format BinLower 8.3;
						format BinUpper 8.3;

						if vartype="N" then
							do;
								if BinName="&MissValueSymbol." then
									do;
										BinLower=.;
										BinUpper=.;
									end;
								else if substr(compress(BinName),1,3)='(-,' then
									do;
										BinLower=.;
										BinUpper=scan(compress(BinName,' (]'),2,',');
									end;
								else if scan(compress(BinName,' ()]'),2,',')='+' then
									do;
										BinLower=scan(compress(BinName,' (]'),1,',');
										BinUpper=.;
									end;
								else
									do;
										BinLower=scan(compress(BinName,' (]'),1,',');
										BinUpper=scan(compress(BinName,' (]'),2,',');
									end;
							end;
					run;

				%end;

			%if &xtype.=C %then
				%do;

					proc sql noprint;
						create table _valuestat_ as
							select bin, &varx., count(*) as N_Rec, sum(&vary.) as N1, sum(&vary.)/count(*) as Prop
								from BINRAWDATA
									group by bin, &varx.
										order by bin, &varx.
						;
					quit;

					data _valuemap_;
						set _valuestat_;
						by bin;
						format binvalue $1000.;
						format nvalue 8.;
						retain binvalue;
						retain nvalue;

						if first.bin then
							do;
								binvalue=&varx.;
								nvalue=1;
							end;
						else
							do;
								binvalue=catx("&charsep.",binvalue,&varx.);
								nvalue=nvalue+1;
							end;

						if last.bin;
						keep bin binvalue nvalue;
					run;

					data  &outstat.;
						merge &outstat. _valuemap_;
						by bin;
						BinName=tranwrd(Binvalue,"|",",");
					run;

				%end;

			data _manualbin_;
				format iv_rank;
				set &outstat.;
				iv_rank=1;
			run;

			%cond_delete(intbl=elogittmp2 BINRAWDATA);
		%end;

	%if &lastmanualbinvar.^=&varx. and &AutoBinCalDisable^=Y %then
		%do;
			%cond_delete(intbl=_finebin_ _coarsebin_);
			%AutoBin(
				intbl=&intbl.,
				DV=&vary.,
				IV=&varx.,
				outbintbl=_finebin_,
				nbin=20,
				minp=5,
				mindiff=0
				);
			%AutoBin(
				intbl=&intbl.,
				DV=&vary.,
				IV=&varx.,
				outbintbl=_coarsebin_,
				nbin=20,
				minp=10,
				mindiff=5
				);
		%end;

	%if &AutoBinCalDisable^=Y and &BinPlotDisable^=Y %then
		%do;
			title "Fine Bin Result";

			%PlotIV(bintbl=_finebin_);
			title "Coarse Bin Result";

			%PlotIV(bintbl=_coarsebin_);
		%end;

	%if %sysfunc(exist(_manualbin_)) and &BinPlotDisable^=Y %then
		%do;
			title "Manual Bin Result";

			%PlotIV(bintbl=_manualbin_);
		%end;

	%let lastmanualbinvar = &varx.;

	%cond_delete(intbl=_manualbin_ iv);
%mend;


%macro MergeBin(binchk=,binsummary=)
		/ store des='1016.Merge Bin'
	;

	%global selbin;
	%let selbin=;
	%let _Var_Type_=;

	proc sql noprint;
		select upcase(VarType) into: _Var_Type_
			from &binsummary.
		;
	quit;

	%if &_Var_Type_=N %then
		%do;
			%let last_y_rate =;
			%let next_y_rate =;

			proc sql noprint;
				select BinY_P into: cur_y_rate 
					from &binsummary.
						where bin=&binchk.;
				select BinY_P into: last_y_rate 
					from &binsummary.
						where bin=(select max(bin) from &binsummary. where bin<&binchk.);
				select BinY_P into: next_y_rate 
					from &binsummary.
						where bin=(select min(bin) from &binsummary. where bin>&binchk.);
			quit;

			%if %length(&last_y_rate.)=0 %then
				%let last_y_rate=999;

			%if %length(&next_y_rate.)=0 %then
				%let next_y_rate=999;
			%let lastinter=%sysfunc(abs(%sysevalf(&cur_y_rate - &last_y_rate)));
			%let nextinter=%sysfunc(abs(%sysevalf(&cur_y_rate - &next_y_rate)));

			%if %sysevalf(&lastinter<&nextinter) %then
				%do;

					proc sql noprint;
						select bin into: selbin 
							from &binsummary.
								where bin=(select max(bin) from &binsummary. where bin<&binchk.);
					quit;

				%end;
			%else
				%do;

					proc sql noprint;
						select bin into: selbin 
							from &binsummary.
								where bin=(select min(bin) from &binsummary. where bin>&binchk.);
					quit;

				%end;
		%end;

	%if &_Var_Type_=C %then
		%do;

			proc sql noprint;
				select BinY_P into: cur_y_rate 
					from &binsummary.
						where bin=&binchk.;
			quit;

			data _min_diff_;
				set &binsummary.;
				rate_diff=abs(BinY_P-&cur_y_rate.);
				where bin^=&binchk.;
			run;

			proc sql noprint;
				select bin into: selbin separated by ','
					from _min_diff_
						where rate_diff=(select min(rate_diff) from _min_diff_)
				;
				drop table _min_diff_;
			quit;

		%end;

	%let selbin=&selbin.;
%mend;


%macro VarAutoBin(
	intbl=,
	vary=,
	varx=,
	outbinstat=,
	nbin=20,
	minp=10,
	mindiff=5,
	Nmissrep=-999999,
	Cmissrep=NA,
	charsep=|,
	ExcludeMissing=N
	) 
	/ store des='1017.Variable Auto Bin'
;


	%cond_delete(intbl=_spbins_);


	%getVarLabel(intbl=&intbl.,invar=&varx);
	%let xlabel=&_VLABEL_;

	%getVarLabel(intbl=&intbl.,invar=&vary);
	%let ylabel=&_VLABEL_;

	%getVarType(intbl=&intbl.,invar=&varx);
	%let xtype=%upcase(&_VTYPE_);

	proc sql noprint;
		select 	sum(missing(&varx.)) into: MissN from &intbl.;
		select 	100*sum(missing(&varx.))/count(*) into: MissP from &intbl.;
	quit;

	%if &ExcludeMissing.=N %then
	%do;
		data binrawdata;
			set &intbl.(keep=&varx. &vary.);

			%if &xtype.=N %then
				%do;
					if &varx.=. then
						&varx.=&Nmissrep.;
				%end;

			%if &xtype.=C %then
				%do;
					if &varx.='' then
						&varx.="&Cmissrep.";
				%end;
		run;
	%end;
	%else 
	%do;
		data binrawdata;
			set &intbl.(keep=&varx. &vary.);
			where &varx. is not missing;
		run;	
	%end;

	proc sql noprint;
		create table varx as
			select count(distinct &varx.) as disN,
				count(*) as ObsN,
				sum(&vary) as NY1,
				sum(1-&vary) as NY0
			from binrawdata
		;
	quit;


	proc sql noprint;
		select disN into:DistXN from varx;
		select obsN into:RecN from varx;
		select NY1 into:Y1N from varx;
		select NY0 into:Y0N from varx;
	quit;

	proc sql noprint;
		drop table varx;
	quit;

	proc freq data=binrawdata noprint;
		table &varx./ out=_var_freq_ missing;
	run;


	proc sql noprint;
		select max(percent) into: MaxFreqP from _var_freq_;
	quit;


	proc sort data=_var_freq_;
		by descending count;
	run;

	data _value_list_;
		set _var_freq_;

		if _N_<=10;
		where &varx. is not null;
	run;

	%let valuesample=;


	proc sql noprint;
		select &varx. into:valuesample separated by ', '
			from _value_list_;
	quit;

	%let valuesample=&valuesample.;
	%let MaxFreqP=&MaxFreqP.;


	%cond_delete(intbl=_value_list_ _var_freq_);
	%let distxn=&distxn.;
	%let recn=&recn.;
	%let y1n=&y1n.;
	%let y0n=&y0n.;

	/*	%put ********Distinct Value of &VarX.: &DistXn.**********;*/
	/*	%put ********Total Records: &RecN.**********;*/
	/*	%put ********Total Y=1: &Y1N.**********;*/
	/*	%put ********Total Y=0: &Y0N.**********;*/
	%do %until(&NSPBinRec.=0 or &nbinstat.<=2);
		%if %sysfunc(exist(_spbins_)) %then
			%do;

				proc sql noprint;
					select bin into: chkbin from _spbins_;
				quit;

				%MergeBin(binchk=&chkbin.,binsummary=&outbinstat.);
				%put **********sel Bin:&selbin.**************;


				data ELOGITTMP1;
					set ELOGITTMP1;

					if bin in (&selbin.) then
						bin= &chkbin.;
				run;


			%end;
		%else
			%do;

				%if &xtype.=N %then
					%do;

						proc rank data=binrawdata groups=&nbin. out=elogittmp1;
							var &varx;
							ranks bin;
						run;

					%end;

				%if &xtype.=C %then
					%do;

						proc sort data=binrawdata;
							by &varx. &vary.;
						run;

						proc sql noprint;
							create table levellist as
								select distinct &varx 
									from binrawdata
										order by &varx.
							;
						quit;

						data levellist;
							format bin 8.;
							set levellist;
							bin=_N_;
						run;

						data elogittmp1;
							merge levellist binrawdata;
							by &varx.;
						run;

						%cond_delete(intbl=levellist);
					%end;
			%end;


		proc means data=elogittmp1 noprint nway maxdec=2;
			class bin;
			var &vary %if &xtype.=N %then

				%do;
					&varx
				%end;
			;
			output out=elogittmp2(drop=_TYPE_ rename=(_FREQ_=N)) 
			%if &xtype.=N %then

				%do;
					min(&varx.)=Min max(&varx.)=Max mean(&varx)=MeanX
				%end;

			sum(&vary)=NY1;
		run;

		proc sort data=elogittmp2;
			by bin;
		run;

		data &outbinstat.;
			format Resp_Var $40.;
			format Resp_Lab $1000.;
			format VariableName $32.;
			format Label $1000.;
			format VarType $1.;
			set elogittmp2;
			Resp_Var="%UPCASE(&vary)";
			Resp_Lab="&ylabel.";
			VariableName="%UPCASE(&varx)";
			Label="&xlabel.";
			VarType="%UPCASE(&xtype.)";
			format OBS_P percentn8.2;
			format Y1_DIST percentn8.2;
			format Y0_DIST percentn8.2;
			if N>0 then BinY_P=NY1/N;
			else BinY_P=0;
			if &RecN.>0 then OBS_P=N/&RecN.;
			if &Y1N.>0 then Y1_DIST=NY1/&Y1N.;
			if &Y0N.>0 then Y0_DIST=(N-NY1)/&Y0N.;
			if (N-NY1+(sqrt(N)/2))>0 and (NY1+(sqrt(N)/2))>0 
				then elogit=log((NY1+(sqrt(N)/2))/(N-NY1+(sqrt(N)/2)));
			if Y1_DIST>0 and Y0_DIST>0 
				then woe=log(Y1_DIST/Y0_DIST);
			if Y1_DIST>0 and Y0_DIST>0 
			    then binIV=(Y1_DIST-Y0_DIST)*log(Y1_DIST/Y0_DIST);
			else binIV=&Gpurebiniv;
			format N_DIST_VALUE 8.;
			format VALUE $1000.;
			format MAXPERCENT 8.3;
			format N_MISSING 8.;
			format MISSING_P 8.3;
			N_DIST_VALUE=&DistXN.;
			VALUE="&valuesample.";
			MAXPERCENT=&MaxFreqP.;
			N_MISSING=&MissN.;
			MISSING_P=&MissP.;
		run;

		%if &xtype.=C %then
			%do;

				proc sort data=&outbinstat.;
					by BinY_P;
				run;

			%end;

		%let nbinstat=0;


		%getOBS(&outbinstat.);
		%let nbinstat=&_nobs_.;


		data _spbins_;
			set &outbinstat.;
			flag=999;
			retain last_Resp_Rate;
			Rate_Change=100*abs(BinY_P-last_Resp_Rate);

			if _N_>1 and Rate_change<=&mindiff. then
				flag=1;
			else if NY1=0 or NY1=N then
				flag=2;
			else if OBS_P*100<&minp. then
				flag=3;
			last=last_Resp_Rate;
			last_Resp_Rate=BinY_P;
		run;

		/*******print check*************************/
		/*		proc print data=_spbins_ ;*/
		/*			var Label bin FLAG VarType BinY_P last Rate_Change  NY1 N OBS_P;*/
		/*			format BinY_P percentn8.2;*/
		/*			format last percentn8.2;*/
		/*			format OBS_P percentn8.2;*/
		/*		run;*/
		proc sort data=_spbins_;
			by flag Rate_Change;
		run;

		data _spbins_;
			set _spbins_;

			if _N_=1 and flag<999;
		run;

		/*******print check*************************/
		/*		proc print data=_spbins_ ;*/
		/*			var Label bin FLAG VarType BinY_P last Rate_Change  NY1 N OBS_P;*/
		/*			format BinY_P percentn8.2;*/
		/*			format last percentn8.2;*/
		/*			format OBS_P percentn8.2;*/
		/*		run;*/
		%let nspbinrec=0;


		%getOBS(_spbins_);
		%let NSPBinRec=&_nobs_.;
	%end;

	%let _NOBS_=0;



	%cond_delete(intbl=_spbins_);

	proc sort data=&outbinstat.;
		by Bin;
	run;

	proc sql noprint;
		create table iv as
			select 
				count(*) as N_BIN,
				max(biniv) as MaxBinIV,
				max(biniv)/sum(biniv) as MaxBinIV_P,
				sum(biniv) as IV from &outbinstat.;
	quit;

	data &outbinstat.;
		set &outbinstat.;

		if _n_=1 then
			set IV;
	run;

	%if &xtype.=N %then
		%do;

			proc sort data=&outbinstat.;
				by bin;
			run;

			data &outbinstat.;
				set &outbinstat. end=lastobs;
				format BinName $1000.;
				format BinLower 8.3;
				format BinUpper 8.3;
				format lastBinUpper 8.3;
				format lowertxt $10.;
				format uppertxt $10.;
				retain lastBinUpper;

				if vartype="N" then
					do;
						if _N_=1 then
							BinLower=.;
						else BinLower=lastBinUpper;

						if lastobs then
							BinUpper=.;
						else BinUpper=Max;

						if binlower=. then
							lowertxt="-";
						else lowertxt=strip(BinLower);

						if binupper=. then
							uppertxt="+";
						else uppertxt=strip(Binupper);
						BinName="("||strip(lowertxt)||","||strip(uppertxt)||"]";
						lastBinUpper=Max;
					end;

				drop lastbinupper lowertxt uppertxt;
			run;

		%end;

	%if &xtype.=C %then
		%do;

			proc sql noprint;
				create table _valuestat_ as
					select bin, &varx., count(*) as N_Rec, sum(&vary.) as N1, sum(&vary.)/count(*) as Prop
						from ELOGITTMP1
							group by bin, &varx.
								order by bin, &varx.
				;
			quit;

			data _valuemap_;
				set _valuestat_;
				by bin;
				format binvalue $1000.;
				format nvalue 8.;
				retain binvalue;
				retain nvalue;

				if first.bin then
					do;
						binvalue=&varx.;
						nvalue=1;
					end;
				else
					do;
						binvalue=catx("&charsep.",binvalue,&varx.);
						nvalue=nvalue+1;
					end;

				if last.bin;
				keep bin binvalue nvalue;
			run;

			data  &outbinstat.;
				merge &outbinstat. _valuemap_;
				by bin;
				format BinName $1000.;
				BinName=tranwrd(Binvalue,"|",",");
			run;

		%end;

	%let droplist=elogittmp1 elogittmp2 iv binrawdata _valuestat_ _valuemap_;

	%cond_delete(intbl=&droplist.);


%mend;

%macro AutoBin(
			intbl=,
			DV=,
			IV=_ALL_,
			outbintbl=,
			nbin=20,
			minp=10,
			mindiff=5,
			datarole=,
			Nmissrep=-88888888,
			Cmissrep=NA,
			charsep=|,
			ExcludeMissing=N
		)
		/ store des='1018.Auto Bin'
		;

	%let dv=%upcase(&dv.);
	%let iv=&iv.;

	%if %length(&iv.)=0 %then
		%let IV=_ALL_;


	%cond_delete(intbl=&outbintbl.);

	proc contents data=&intbl.(keep=&IV.) out=_metadata_ noprint;
	run;


	data _null_;
		set _metadata_;
		call symputx('iv'||left(put(_n_,8.)),name);
		call symputx('N_var',left(put(_n_,8.)));
		where upcase(name) not in ("&dv.");
	run;


	%if %length(&N_var.)^=0 and &N_var.>0 %then
		%do m=1 %to &N_var.;
			%put "*************Auto Bin Task &m. of &N_var.*****************";
			%put "*************Variable: &&iv&m.*****************";

			%VarAutoBin(
				intbl=&intbl.,
				vary=&dv.,
				varx=&&iv&m.,
				outbinstat=binstattmp,
				nbin=&nbin.,
				minp=&minp.,
				mindiff=&mindiff.,
				Nmissrep=&Nmissrep.,
				Cmissrep=&Cmissrep.,
				charsep=&charsep.,
				ExcludeMissing=&ExcludeMissing.
				);


			%if &m=1 %then
				%do;

					data &outbintbl.;
						set binstattmp;
					run;

				%end;
			%else
				%do;

					data &outbintbl.;
						set &outbintbl. binstattmp;
					run;

				%end;

			%cond_delete(intbl=binstattmp);
		%end;


	proc sql noprint;
		create table _var_summary_ as
			select distinct variablename, IV
				from &outbintbl.
					order by iv desc,variablename;
	quit;

	data _var_summary_;
		format IV_RANK 8.;
		format DATAROLE $50.;
		set  _var_summary_;
		DATAROLE="&datarole.";
		IV_Rank=_N_;
		Keep IV_RANK variablename datarole;
	run;

	proc sort data=_var_summary_;
		by variablename;
	run;

	proc sort data=&outbintbl.;
		by variablename;
	run;

	data &outbintbl.;
		merge _var_summary_ &outbintbl.;
		by variablename;
	run;

	proc sort data=&outbintbl.;
		by iv_rank bin;
	run;

	%cond_delete(intbl=_metadata_ _var_summary_);

%mend;
%macro PlotIV(bintbl=)
		/ store des='1019.Plot IV';

	ods graphics on/noborder height=600 width=1000;

	proc sort data=&bintbl.(keep=variablename iv) nodupkey out=_ranklist_;
		by descending iv variablename;
	run;

	%let N_RANK=;

	data _null_;
		set _ranklist_;
		call symputx('IV_NO_'||left(put(_n_,8.)),variablename);
		call symputx('N_RANK',left(put(_n_,8.)));
	run;

	%do i=1 %to &N_RANK.;
		%let Xlab=;
		%let Modellab=;
		%let Xvarnm=;
		%let IV=;
		%let DistValue=;

		data _iv_tmp_;
			set &bintbl.;
			where variablename="&&IV_NO_&i.";
		run;

		proc sql noprint;
			select distinct upcase(VARTYPE) into: vartype from _iv_tmp_;
			select distinct label into: Xlab from _iv_tmp_;
			select distinct variablename into: Xvarnm from _iv_tmp_;
			select distinct RESP_LAB into: Modellab from _iv_tmp_;
			select distinct N_DIST_VALUE into: DistValue from _iv_tmp_;
			select distinct IV into: IV from _iv_tmp_;
			select distinct IV_rank into: ivRank from _iv_tmp_;
			select distinct VALUE into: TopValues from _iv_tmp_;
		quit;

		%let Xlab=&Xlab.;
		%let Modellab=&Modellab.;
		%let Xvarnm=&Xvarnm.;
		%let IV=&IV.;
		%let DistValue=&DistValue.;
		%let ivrank=&ivrank.;

		proc sgplot data=_iv_tmp_ noautolegend;
			title2 "&Xvarnm - &Xlab. (IV:&IV - Rank: &ivRank.)";
			needle x=BIN y=woe /lineattrs=(thickness=30 color=grey);
			needle x=BIN y=biniv /lineattrs=(thickness=20 color=black);
			series x=BIN y=woe /lineattrs=(thickness=2 ) markers

				%if &vartype=N %then
					%do;
						datalabel=meanX
					%end;
				;
				inset "IV Rank: &ivRank." "IV: &IV" "N Values: &DistValue" /border;
				xaxis label="&Xvarnm - &Xlab.  (IV:&IV - Rank: &ivRank.)" display=(novalues noticks);
				yaxis max=2 min=-2;
		run;

		proc sort data=_iv_tmp_ out=_var_info_ nodupkey;
			by IV_RANK VARIABLENAME LABEL VALUE N_DIST_VALUE MAXPERCENT MISSING_P;
		run;

		title2 "Variable Summary: &Xvarnm - &Xlab. (IV:&IV - Rank: &ivRank.)";

		proc print data=_var_info_ noobs label;
			var IV_RANK VARIABLENAME LABEL IV MAXBINIV_P VALUE N_DIST_VALUE MAXPERCENT MISSING_P;
		run;

		title2 "Bin Information: &Xvarnm - &Xlab. (IV:&IV - Rank: &ivRank.)";

		%if &vartype=C %then
			%do;

				proc print data=_iv_tmp_ noobs;
					var BIN BINNAME OBS_P BINY_P WOE BINIV NVALUE N NY1 Y1_DIST Y0_DIST;
				run;

			%end;

		%if &vartype=N %then
			%do;

				proc print data=_iv_tmp_ noobs;
					var BIN BINNAME OBS_P BINY_P WOE BINIV N NY1 Y1_DIST Y0_DIST MIN MEANX MAX;
				run;

			%end;

		title2;

		%cond_Delete(intbl=_iv_tmp_ _ranklist_ _var_info_);
	%end;

	ods graphics off;
%mend;

/*%runtblcode(codetbl=codedata,codevar=code);*/
/**/
/*data a;*/
/*	set sashelp.class;*/
/*	%include codefile;*/
/*run;*/
%macro AutoDummyRuleCreate(
		intbl=,
		varnm=,
		outruletable=,
		MissingDummy=N,
		MaxLevel=10,
		delimiter=,
		keepOriginal=Y,
		ORDER=0,
		DATAROLE=&RULE_DSN.
	)	/ store des='1020.Auto DV Rule Create';

	%local _N_Vars_ var_name variabletype _Value_Raw_List_ dsid _N_Var_values_ missing_dummy_name missing_dummy_label MissingMark i kkk;
	%let MaxLevel=&Maxlevel.;
	%let keepOriginal=%upcase(&keepOriginal.);

	%if "&MaxLevel"="" %then
		%let Maxlevel=1000;

	%cond_delete(intbl=&outruletable.);

	proc sql noprint;
		create table &outruletable. ( compress=CHAR  bufsize=65536 )
			(
		ORDER num format=8. label='运行顺序',
			DATAROLE char(50) format=$50. label='决策数据类型',
			VARIABLENAME char(32) format=$32. label='变量名称',
			LABEL char(1000) format=$1000. label='变量描述',
			RULE char(20000) format=$20000. label='规则',
			RULE_DESC char(1000) format=$1000. label='规则描述',
			RULE_RUN_MODE char(20) format=$20. label='规则运行模式',
			INPUTMODE char(50) format=$50. label='规则生成模式',
			TARGETVAR char(40) format=$40. label='输出变量名称',
			TARGETLABEL char(1000) format=$1000. label='输出变量描述'
			);
	quit;

	proc contents data=&intbl.(keep=&varnm.) noprint varnum 
		out=_var_list_(keep=name LABEL rename=(name=varnm ));
	run;

	%let _N_Vars_=0;
	%let fid=%sysfunc(open(work._var_list_));
	%let _N_Vars_=%sysfunc(attrn(&fid,NOBS));
	%let fid=%sysfunc(close(&fid));

	%if &_N_Vars_>0 %then
		%do;

			Data _null_;
				set _var_list_;
				call symputx('_Var_name_'||left(put(_n_,8.)),varnm);
				call symputx('_Var_label_'||left(put(_n_,8.)),Label);
			run;

			%do kkk=1 %to &_N_Vars_.;
				%let Var_name=&&_Var_name_&kkk.;
				%let Var_label=&&_Var_label_&kkk.;
				%let var_label=&var_label;

				%if %length(&var_label)>0 %then
					%let dvlabelheader=&Var_name. - &Var_label.;
				%else %let dvlabelheader=&Var_name. -;
				%let dsid=%sysfunc(open(&intbl.,i));
				%let variabletype=%sysfunc(vartype(&dsid,%sysfunc(varnum(&dsid,&var_name.))));
				%let rc=%sysfunc(close(&dsid));
				%put &variabletype.;
				%let _Value_Raw_List_=&intbl.;

				%if %length(&delimiter.)^=0 %then
					%do;
						%let _Value_Raw_List_=_zzz_;

						data _zzz_(drop=_iii_);
							set &intbl.;
							_iii_=1;

							do until(scan(&var_name.,_iii_,"&delimiter.")="");
								_vvv_=scan(&var_name.,_iii_,"&delimiter.");
								output;
								_iii_=_iii_+1;
							end;

							drop &var_name.;
							rename _vvv_=&var_name.;
						run;

					%end;

				/*create list of distinct values*/
				proc sql noprint;
					create table _Value_distinct_List_ as
						select distinct &var_name. as VARVALUE label="Variable Value"
							from &_Value_Raw_List_.

						%if &MissingDummy.=N %then
							%do;
								where  &var_name. is not null
							%end;
						;
				quit;

				%let _N_Var_values_=0;
				%let fid=%sysfunc(open(work._value_distinct_list_));
				%let _N_Var_values_=%sysfunc(attrn(&fid,NOBS));
				%let fid=%sysfunc(close(&fid));
				%put N of levels = &_N_Var_values_.;
				%put Max number of levels considered for coding dummy variables = &maxlevel.;

				%if &_N_Var_values_.>0 and &_N_Var_values_.<=&maxlevel. %then
					%do;

						data _rule_raw_;
							format ORDER 8.;
							format DATAROLE $50.;
							format VARIABLENAME $32.;
							format LABEL $1000.;
							format RULE $20000.;
							format RULE_DESC $1000.;
							format RULE_RUN_MODE $20.;
							format INPUTMODE $50.;
							format TARGETVAR $40.;
							format TARGETLABEL $1000.;
							format VALUELABEL $1000.;
							set _Value_distinct_List_;

							order=&order.;
							datarole="&datarole.";

							if missing(VARVALUE) then
								VALUELABEL="缺失值 - MISSING VALUE";
							else VALUELABEL=strip(VARVALUE);
							VARIABLENAME="&var_name.";
							LABEL="&Var_label.";
							RULE_RUN_MODE="IN";
							INPUTMODE="AUTOMATIC";
							RULE_DESC="20-为变量每个值生成虚拟变量(Create Dummy Variable for Every Value)";
							TARGETVAR=cats("DV_",substr("&var_name.",1,min(24,length("&var_name."))),_N_);
							TARGETLABEL="&dvlabelheader."||" Value: "||strip(VALUELABEL);
							RULE=catx(" ","label ",TARGETVAR,"=",quote(strip(TARGETLABEL)),";",
								"if",VARIABLENAME,"=",

							%if &variabletype.=C %then
								%do;
									quote(strip(VARVALUE)),
								%end;
							%else
								%do;
									VARVALUE,
								%end;
							
								%if &MissingDummy.=N %then
									%do;
										"and NOT", cats("MISSING(",VARIABLENAME,")"),
									%end;
								"then",TARGETVAR,"= 1; else",TARGETVAR,"= 0;"
								%if &keepOriginal.=N %then
								%do;
									,"drop",VARIABLENAME,";"
								%end;
							);
						run;

						proc append base=&outruletable. data=_rule_raw_(drop=VALUELABEL VARVALUE) force;
						quit;

					%end;
			%end;
		%end;

	%cond_delete(intbl=_var_list_ _zzz_ _Value_distinct_List_ _rule_raw_);

%mend;


%macro ExcludeRuleCreate(
			inrule=,
			outrule=,
			MaxFreqDropP=&GMaxFreqDropP., /*频数最大值所占百分比*/
			MissingDropP=&GMissingDropP., /*缺失值所占百分比*/
			MinIVDrop=&GMin_IV.,
			NumDVLvlN=&GNumDVLvlN.,
			Method=WOE)
			/ store des='1021.Exclude Rule Create';

	%let method=%upcase(&method.);

	data _exclude_rule_tmp_;
		format DECISION $100.;
		format REASON $100.;
		set &inrule.;
		format INPUTMODE $50.;
		INPUTMODE="AUTOMATIC";

		if N_dist_value=1 then
			do;
				DECISION="EXCLUDE";
				REASON="90-变量只有一个取值 (Only One Value Found)";
			end;
		else if MaxPercent>=&MaxFreqDropP. then
			do;
				DECISION="EXCLUDE";
				REASON="91-变量频数最大取值比例大于&MaxFreqDropP. (Max Percent of Single Value > &MaxFreqDropP.)";
			end;
		else if Missing_P>=&MissingDropP. then
			do;
				DECISION="EXCLUDE";
				REASON="92-缺失值百分比大于&MissingDropP.(% of Missing > &MissingDropP.)";
			end;

		%if &Method.=WOE %then
			%do;
				else if iv<&MinIVDrop. then
					do;
						DECISION="EXCLUDE";
						REASON="93-IV值小于&MinIVDrop.(IV Value Less Than &MinIVDrop.)";
					end;
			%end;

		%if &Method.=DV %then
			%do;
				if Type=2 and DECISION ='' then
					do;
						DECISION="EXCLUDE";
						REASON="94-属性变量：转换为虚拟变量入模(Character Variable: Coverted into Dummy Variables)";
					end;

				if Type=1 and DECISION ='' 
					and (not (value='0, 1' or value='1, 0'))
					and N_dist_value<=&NumDVLvlN. 
					and MaxPercent<&MaxFreqDropP.
					then
						do;
							DECISION="EXCLUDE";
							REASON="95-数值型变量：转换为虚拟变量入模(Numeric Variable: Coverted into Dummy Variables)";
						end;

			%end;

		if N_dist_value>&GMaxN_Char. and Type=2 then
			do;
				DECISION="EXCLUDE";
				REASON="96-属性变量取值过多 - >&GMaxN_Char. (Too Many Distinct Values Found - >&GMaxN_Char.)";
			end;

		if DECISION^='';
	run;

	proc sql noprint;
	create table &outrule. as
		select * from _exclude_rule_tmp_
		where substr(REASON,1,2) in ('94','95','96') or 
		upcase(variablename) not in (select upcase(variablename) from _FIXVAR_LIST_)
	;
	quit;
	%cond_delete(intbl=_exclude_rule_tmp_);

%mend;


%macro NumRecodeRuleCreate(
			DataRole=,
			InTbl=,
			VarList=_NUMERIC_,
			SearchValueList=,
			ReplaceValue=0,
			OutRule=,
			Replaceflag=Y,
			operator=EQ
			)/ store des='1022.Num Recode Rule Create';

	/*Operator=EQ,NE,GT,LT,GE,LE*/
	/*Replaceflag: if the dummy variable need to be created*/
	proc contents nodetails data=&intbl.(keep=&VarList.)
		out=_metadata_(keep=name label type FORMAT) noprint;
	run;

	%let CN_VS=;

	data _null_;
		set _metadata_;
		call symputx('CV_NM'||left(put(_n_,8.)),name);
		call symputx('CV_LB'||left(put(_n_,8.)),label);
		call symputx('CV_FM'||left(put(_n_,8.)),format);
		call symputx('CN_VS',left(put(_n_,8.)));
		where type=1;
	run;

	%cond_delete(intbl=_var_nsp_list_);

	/*create list of variables which contain special character values*/
	%if %length(&CN_VS.)^=0 and &CN_VS.>0 %then
		%do;
			%do m=1 %to &CN_VS.;
				%let curvarnm=&&CV_NM&m.;
				%let curvarlab=&&CV_LB&m.;
				%put ******NUM Recoding******&m. . &&CV_NM&m.*******&CN_VS.********;

					data _var_nsp_;
						format &curvarnm. best.;
						set &intbl.(keep=&curvarnm.);
						where &curvarnm. in (&SearchValueList.);
					run;

					proc sort data=_var_nsp_ nodupkey;
					by  &curvarnm.;
					run;

					data _var_nsp_;
						format VARIABLENAME $32.;
						format LABEL $1000.;
						set _var_nsp_;
						format repvalue best.;
						format repvalueid 8.;
						VARIABLENAME="&curvarnm.";
						LABEL="&curvarlab.";
						repvalue=&ReplaceValue.;
						repvalueid=_N_;
						rename &curvarnm.=spvalue;
					run;

					proc append base=_var_nsp_list_ data=_var_nsp_;
					quit;

					%cond_delete(intbl=_var_nsp_);
			%end;
		%end;

	data &OutRule.;
		format DATAROLE $50.;
		format VARIABLENAME $32.;
		format LABEL $1000.;
		format RULE $20000.;
		format RULE_DESC $1000.;
		format RULE_RUN_MODE $20.;
		set _var_nsp_list_;
		datarole="%upcase(&datarole.)";
		format INPUTMODE $50.;
		INPUTMODE="AUTOMATIC";

		%if &Replaceflag=Y %then
			%do;
				format TARGETVAR $40.;
				format TARGETLABEL $1000.;

				%Create_New_VAR_Nm(
					invarnm=VARIABLENAME,
					outvarnm=TARGETVAR,
					varsuffix=_NSP,
					varheaderlen=18,
					mode=DATASTEP
					);
				TARGETVAR=cats(TARGETVAR,repvalueid);
				TARGETLABEL = strip(LABEL)||" "||strip(VARIABLENAME)||': Value='||strip(spvalue);
			%end;
		rule='';
		repvalue=&ReplaceValue.;
		rule_type="Recode";
		rule_desc="10-数值型变量特殊值替换"
			%if &Replaceflag.=Y %then

			%do;
				||",并对特殊值生成虚拟变量"
			%end;

		||" (Recode Value &Operator. Special Values ("||strip(spvalue)|| ") with "||strip(repvalue)
		%if &Replaceflag.=Y %then
			%do;
				||"and Create Dummy Variable Accordingly)"
			%end;
		;
		rule_create_mode="Automatic";
		rule_auto_create_code='%CharRecodeRuleCreate';
		rule_run_mode="IN";

		label VARIABLENAME="Variable Name"
			LABEL="Variable Label"
			rule="Rule Code"
			rule_desc="Rule Description"
			rule_run_mode="Rule inside DataStep or Outside DataStep"
		;

		%if &Replaceflag=Y %then
			%do;
				rule=strip(rule)||" if "||strip(VARIABLENAME)||" &operator "||strip(spvalue)||" then "||strip(TARGETVAR)||"= 1; else "||strip(TARGETVAR)||"= 0;";
				rule=strip(rule)||"label "||strip(TARGETVAR)||'= '||quote(strip(TARGETLABEL))||';';
				output;
			%end;

		rule=strip(rule)||"if "||strip(VARIABLENAME)||" &operator "||strip(spvalue)||" then "||strip(VARIABLENAME)||" = "||strip(repvalue)||";";
		TARGETVAR=VARIABLENAME;
		output;
	run;
	%cond_delete(intbl=_METADATA_ _VAR_NSP_LIST_);

%mend;

%macro CharRecodeRuleCreate(
			DataRole=,
			InTbl=,
			VarList=_character_,
			SearchValueList=,
			ReplaceValue=,
			OutRule=,
			Replaceflag=Y,
			operator=EQ
			)	/ store des='1023.Character Recode Rule Create';
	/*Operator=EQ,NE,GT,LT,GE,LE*/
	/*Replaceflag: if the dummy variable need to be created*/
	%cond_delete(intbl=_METADATA_ _VAR_SP_LIST_);

	proc contents nodetails data=&intbl.(keep=&VarList.)
		out=_metadata_(keep=name label type FORMAT) noprint;
	run;

	%let CN_VS=;

	data _null_;
		set _metadata_;
		call symputx('CV_NM'||left(put(_n_,8.)),name);
		call symputx('CV_LB'||left(put(_n_,8.)),label);
		call symputx('CV_FM'||left(put(_n_,8.)),format);
		call symputx('CN_VS',left(put(_n_,8.)));
		where type=2;
	run;

	%cond_delete(intbl=_var_sp_list_);

	/*create list of variables which contain special character values*/
	%if %length(&CN_VS.)^=0 and &CN_VS.>0 %then
		%do;
			%do m=1 %to &CN_VS.;
				%let curvarnm=&&CV_NM&m.;
				%let curvarlab=&&CV_LB&m.;
				%let k=1;
				%put *******Char Recoding*********&m. . &&CV_NM&m.****************;

				%do %while(%scan(&SearchValueList.,&k.,%str( ))^=);

					data _var_sp_(keep=VARIABLENAME LABEL &curvarnm. repvalue repvalueid);
						format VARIABLENAME $32.;
						format LABEL $1000.;
						format &curvarnm. $20000.;
						format repvalue $1000.;
						format repvalueid 8.;
						set &intbl.(keep=&curvarnm. obs=1);
						VARIABLENAME="&curvarnm.";
						LABEL="&curvarlab.";
						repvalue="&ReplaceValue.";
						repvalueid=&k.;
						where &curvarnm. ="%scan(&SearchValueList.,&k.)";
					run;

					proc append base=_var_sp_list_ 
						data=_var_sp_(rename=(&curvarnm.=spvalue));
					quit;

					%cond_delete(intbl=_var_sp_);
					%let k=%eval(&k+1);
				%end;
			%end;
		%end;

	data &OutRule.;
		format DATAROLE $50.;
		format VARIABLENAME $32.;
		format LABEL $1000.;
		format RULE $20000.;
		format RULE_DESC $1000.;
		format RULE_RUN_MODE $20.;
		set _var_sp_list_;
		datarole="%upcase(&datarole.)";
		format INPUTMODE $50.;
		INPUTMODE="AUTOMATIC";
		rule='';

		%if &Replaceflag=Y %then
			%do;
				format TARGETVAR $40.;
				format TARGETLABEL $1000.;

				%Create_New_VAR_Nm(
					invarnm=VARIABLENAME,
					outvarnm=TARGETVAR,
					varsuffix=_CSP,
					varheaderlen=18,
					mode=DATASTEP
					);
				TARGETVAR=cats(TARGETVAR,repvalueid);
				TARGETLABEL = strip(LABEL)||" "||strip(VARIABLENAME)||': Value='||strip(spvalue);
			%end;

		repvalue="&ReplaceValue.";
		rule_type="Recode";
		rule_desc="10-字符型变量特殊值替换"
			%if &Replaceflag.=Y %then

			%do;
				||",并对特殊值生成虚拟变量"
			%end;

		||" (Recode Value &Operator. Special Values ("||strip(spvalue)|| ") with "||"'"||strip(repvalue)||"'"
		%if &Replaceflag.=Y %then
			%do;
				||"and Create Dummy Variable Accordingly)"
			%end;
		;
		rule_create_mode="Automatic";
		rule_auto_create_code='%CharRecodeRuleCreate';
		rule_run_mode="IN";

		label VARIABLENAME="Variable Name"
			LABEL="Variable Label"
			rule="Rule Code"
			rule_desc="Rule Description"
			rule_run_mode="Rule inside DataStep or Outside DataStep"
		;

		%if &Replaceflag=Y %then
			%do;
				rule=strip(rule)||" if "||strip(VARIABLENAME)||" &operator "||"'"||strip(spvalue)||"'"||" then "||strip(TARGETVAR)||"= 1; else "||strip(TARGETVAR)||"= 0;";
				rule=strip(rule)||"label "||strip(TARGETVAR)||'= '||quote(strip(TARGETLABEL))||';';
				output;
			%end;

		rule="if "||strip(VARIABLENAME)||" &operator "||"'"||strip(spvalue)||"'"||" then "||strip(VARIABLENAME)||" = "||"'"||strip(repvalue)||"'"||";";
		TARGETVAR=VARIABLENAME;
		output;
	run;
		%cond_delete(intbl=_METADATA_ _VAR_SP_LIST_);

%mend;


%macro Char2NumRuleCreate(
			DataRole=,
			InTbl=,
			VarList=_character_,
			OutRule=,
			numfmt=%str(best.),
			outcharvalue=spcharvalues,
			dropcharvar=N
			)		/ store des='1024.Convert Char into Num';

	%cond_delete(intbl=&OutRule. &outcharvalue.);

	proc contents nodetails data=&intbl.(keep=&Varlist.)
		out=_metadata_(keep=name label type FORMAT) noprint;
	run;

	%let N_VS=;

	data _null_;
		set _metadata_;
		call symputx('V_NM'||left(put(_n_,8.)),name);
		call symputx('V_LB'||left(put(_n_,8.)),label);
		call symputx('V_FM'||left(put(_n_,8.)),format);
		call symputx('N_VS',left(put(_n_,8.)));
		where type=2;
	run;

	%if %length(&N_VS.)^=0 and &N_VS.>0 %then
		%do;
			%do k=1 %to &N_VS.;
				%let curvar= &&V_NM&k.;
				%let curlabel= &&V_LB&k.;
				%put **********&k. &&V_NM&k.************;

				proc freq data=&InTbl.;
					table &curvar. /noprint missing
						out=_freq_tbl_(rename=(&curvar.=values));
				run;

				data _freq_tbl_ _char_values_;
					format values $2000.;
					format numvalues $2000.;
					set _freq_tbl_;
					ndots=countc(values,'.');
					numvalues=compress(values,'.-e+','kd');

					if numvalues=values 
						and countc(values,'.')<=1 
						and countc(values,'-')<=1 
						and countc(values,'e')<=1 
						and countc(values,'+')<=1 
						and not (countc(values,'-')=1 and substr(values,1,1)^='-') 
						then numflag=1;
					else numflag=0;
					output _freq_tbl_;

					if numflag=0 then
						output _char_values_;
					where values is not null;
				run;

				%if %getOBS2(_char_values_)=0 %then
					%do;
						%cond_delete(intbl=_char_values_);

						data _convert_rule_;
							format DATAROLE $50.;
							format VARIABLENAME $32.;
							format LABEL $1000.;
							format RULE $20000.;
							format RULE_DESC $1000.;
							format RULE_RUN_MODE $20.;
							datarole="%upcase(&datarole.)";
							format INPUTMODE $50.;
							INPUTMODE="AUTOMATIC";
							format TARGETVAR $40.;
							format TARGETLABEL $1000.;
							VARIABLENAME="&curvar.";
							LABEL="&curlabel.";
							format DECISION $100.;
							format REASON $100.;
							DECISION="EXCLUDE";
							REASON="99-转换为数值型变量(Converted into Numeric Variable)";
							%Create_New_VAR_Nm(
								invarnm=VARIABLENAME,
								outvarnm=TARGETVAR,
								varsuffix=_NUM,
								varheaderlen=18,
								mode=DATASTEP
								);
							TARGETVAR=strip(TARGETVAR);
							TARGETLABEL = strip(LABEL)||strip(VARIABLENAME)||": NUM";
							rule_type="format convert";
							rule_desc="10-字符型变量自动转换成数值型变量(Automatically Convert Character Variable into Numeric Variabless)";
							rule_create_mode="Automatic";
							rule_auto_create_code='%Char2NumRuleCreate';
							rule_run_mode="IN";
							rule="if not missing("||strip(VARIABLENAME)||") then "||strip(TARGETVAR)||"=input("||strip(VARIABLENAME)||",&numfmt.);";
							%if &dropcharvar.=Y %then 
								%do;
									rule=strip(rule)||" drop "||strip(VARIABLENAME)||";";
								%end;
							label VARIABLENAME="Variable Name"
								LABEL="Variable Label"
								rule="Rule Code"
								rule_desc="Rule Description"
								rule_run_mode="Rule inside DataStep or Outside DataStep"
							;
						run;

						proc append base=&outrule. data=_convert_rule_;
						quit;

					%end;
				%else
					%do;

						data _char_values_;
							format variablename $32.;
							format label $1000.;
							set _char_values_;
							VARIABLENAME="&curvar.";
							LABEL="&curlabel.";
						run;

						proc append base=&outcharvalue. data=_char_values_;
						quit;

						%cond_delete(intbl=_char_values_);
					%end;
			%end;
		%end;
	%cond_delete(intbl=_convert_rule_ _METADATA_ _FREQ_TBL_);

%mend;

%macro DVRuleCreate(
			inrule=,
			outrule=,
			MaxFreqDropP=&GMaxFreqDropP., /*频数最大值所占百分比*/
			NumDVLvlN=&GNumDVLvlN., /*数值变量做虚拟变量的最多取值个数*/
			NumModeDVP=&GNumModeDVP.) /*数值变量做众数虚拟变量的最少百分比*/
		/ store des='1025.DV Rule Create';

	/***该macro不使用了****/
	data &outrule.;
		format RULE_DESC $500.;
		format RULE $20000.;
		format RULE_RUN_MODE $20.;
		format dvname $32.;
		set &inrule.;
		format INPUTMODE $50.;
		INPUTMODE="AUTOMATIC";

		/*numerical variable*/
		if Type=2 then
			do;
				RULE_DESC="20-为属性变量每个值生成虚拟变量(Create Dummy Variable for Every Value)";

				/*				REASON="属性变量：转换为虚拟变量入模";*/
				RULE=cats('%Auto_Dummy_Variable(tablename=&targettbl.,variablename=',VariableName,',outputtable=&targettbl.,MaxLevel=30, keepOriginal=Y);');
				RULE_RUN_MODE="OUT";
			end;

		if Type=1 and RULE='' then
			do;
				if  (not (value='0, 1' or value='1, 0'))
					and N_dist_value<=&NumDVLvlN. and MaxPercent<&MaxFreqDropP. then
					do;
						RULE_DESC="21-为数值变量每个值生成虚拟变量(Create Dummy Variable for Every Value)";
						RULE=cats('%Auto_Dummy_Variable(tablename=&targettbl.,variablename=',VariableName,',outputtable=&targettbl.,MaxLevel=30, keepOriginal=Y);');
						RULE_RUN_MODE="OUT";
					end;

				if MaxPercent<&MaxFreqDropP. and MaxPercent>=&NumModeDVP.
					and N_dist_value>&NumDVLvlN. then
					do;
						RULE_DESC="22-为数值变量的众数生成虚拟变量(Create Dummy Variable for Mode Value)";

						if length(VariableName)>20 then
							dvname=cats(substr(VariableName,1,20),"_M_",_N_);
						else dvname=cats(VariableName,"_M_",_N_);
						RULE=cats(dvname,"=(",VariableName,"=",scan(value,1,','),');');
						RULE=catx(' ',RULE,'label',dvname,'= ',
							quote(cats(VariableName,'-','(',Label,')','是否等于众数(',scan(value,1,','),')')),";");
						RULE_RUN_MODE="IN";
					end;
			end;

		if RULE^='';
		drop dvname;
	run;

%mend;


%macro MODE_DVRuleCreate(
			inrule=,
			outrule=,
			MaxFreqDropP=&GMaxFreqDropP., /*频数最大值所占百分比*/
			NumDVLvlN=&GNumDVLvlN., /*数值变量做虚拟变量的最多取值个数*/
			NumModeDVP=&GNumModeDVP.) /*数值变量做众数虚拟变量的最少百分比*/
		/ store des='1026.Mode DV Create';


	data &outrule.;
		format RULE_DESC $500.;
		format RULE $20000.;
		format RULE_RUN_MODE $20.;
		format dvname $32.;
		format TARGETVAR $40.;
		format TARGETLABEL $1000.;

		set &inrule.;
		format INPUTMODE $50.;
		INPUTMODE="AUTOMATIC";

		if Type=1 and MaxPercent<&MaxFreqDropP. 
				and MaxPercent>=&NumModeDVP.
				and N_dist_value>&NumDVLvlN.
			then
				do;
					RULE_DESC="22-为数值变量的众数生成虚拟变量(Create Dummy Variable for Mode Value)";
					if length(VariableName)>20 then
						dvname=cats("MDV_",substr(VariableName,1,20),_N_);
					else dvname=cats("MDV_",VariableName,_N_);
					RULE=cats(dvname,"=(",VariableName,"=",scan(value,1,','),');');
					RULE=catx(' ',RULE,'label',dvname,'= ',
							quote(cats(VariableName,'-','(',Label,')','是否等于众数(',scan(value,1,','),')')),";");
					RULE_RUN_MODE="IN";
					TARGETVAR=dvname;
					TARGETLABEL=cats(VariableName,'-','(',Label,')','是否等于众数(',scan(value,1,','),')');
				end;

		if RULE^='';
		drop dvname;
	run;

%mend;

%macro MISS_DVRuleCreate(
			inrule=,
			outrule=,
			MissingDropP=&GMissingDropP.) /*缺失值最大所占百分比*/
	/ store des='1026-2.MISSING DV Create';

	data &outrule.;
		format RULE_DESC $500.;
		format RULE $20000.;
		format RULE_RUN_MODE $20.;
		format TARGETVAR $40.;
		format TARGETLABEL $1000.;
		set &inrule.;
		format INPUTMODE $50.;
		INPUTMODE="AUTOMATIC";
		format repchar $4.;
		RULE_RUN_MODE="IN";

		if Type=2 then
			repchar=cats("'","NA","'");
		else repchar='0';
		%Create_New_VAR_Nm(
			invarnm=VARIABLENAME,
			outvarnm=TARGETVAR,
			varsuffix=_MISS,
			varheaderlen=18,
			mode=DATASTEP
			);
		RULE_DESC="25-为变量的缺失值生成虚拟变量(Create Dummy Variable for Missing Value)";
		RULE=cats(TARGETVAR,"=MISSING("||strip(VariableName)||");");
		RULE=catx(' ',RULE,'label',TARGETVAR,'= ',
			quote(cats(VariableName,'-','(',Label,')','是否等于缺失值')),";");
		output;
		RULE=catx(' ',"if MISSING("||strip(VariableName)||") then",VariableName,"=",repchar,";");
		TARGETVAR=VARIABLENAME;
		output;
		where MISSING_P>0 and MISSING_P<&MissingDropP.;
	run;

%mend;

%macro TranTrimRuleCreate(
			inrule=,
			outrule=,
			MinSkewness=1, /*需做平滑变换的最小偏度绝对值*/
			min_factor=0.75, /*lower outlier multiplier*/
			max_factor=1.5, /*upper outlier multiplier*/
			min_distinct_value=10 /*做以上变换的最小distinct value个数*/
			) / store des='1027.Transformation and Trim Rule';


	data &outrule.;
		format RULE_DESC $500.;
		format RULE $20000.;
		format RULE_RUN_MODE $20.;
		format offset 8.3;
		format logname $40.;
		set &inrule.;
		format INPUTMODE $50.;
		format TARGETVAR $40.;
		format TARGETLABEL $1000.;

		INPUTMODE="AUTOMATIC";
		RULE_RUN_MODE="IN";

		if min>0 then
			offset=1;
		else if min>-1 then
			offset=1;
		else offset=abs(min)+1;

		if length(variablename)<22 then
			logname=cats("LOG_",variablename);
		else logname=cats("LOG_",substr(variablename,1,22),"_",strip(_N_));

		if DECISION='' and RULE='' and N_dist_value>&min_distinct_value. then
			do;
				if  min< &min_factor.*p1 then
					do;
						RULE_DESC="30-最小极值盖帽处理(Lower Outlier Replacement)";
						RULE="if "||strip(VariableName)||"<"||strip(p1)||" then "||strip(VariableName)||"="||strip(p1)||";";
						TARGETVAR=variablename;
						TARGETLABEL=LABEL;
						output;
						RULE_DESC="";
						RULE="";
						TARGETVAR="";
						TARGETLABEL="";
					end;

				if  max> &max_factor.*p99 then
					do;
						RULE_DESC="31-最大极值盖帽处理(Upper Outlier Replacement)";
						RULE="if "||strip(VariableName)||">"||strip(p99)||" then "||strip(VariableName)||"="||strip(p99)||";";
						TARGETVAR=variablename;
						TARGETLABEL=LABEL;
						output;
						RULE_DESC="";
						RULE="";
						TARGETVAR="";
						TARGETLABEL="";
					end;

				if  abs(skew)> &MinSkewness. then
					do;
						RULE_DESC="40-偏度过大，做平滑变换(Data Smoothing Transformation)";
						RULE=strip(logname)||"=LOG("||strip(VariableName)||"+"||strip(offset)||");";
						RULE=strip(RULE)||"label "||strip(logname)||"="||quote(strip(VariableName)||" "||strip(LABEL)||" LOG("||strip(VariableName)||"+"||strip(offset)||")")||";";
						TARGETVAR=strip(logname);
						TARGETLABEL=strip(VariableName)||"-"||strip(LABEL)||"=LOG("||strip(VariableName)||"+"||strip(offset)||")";
						output;
						RULE_DESC="";
						RULE="";
						TARGETVAR="";
						TARGETLABEL="";
					end;
			end;

		drop offset logname;
	run;

%mend;

%macro CorrRuleCreate(
			indata=,
			inmeta=,
			yvarnm=,
			outrule=,
			N_Distinct_ChiSQ=10,
			/*N_Distinct_ChiSQ考虑卡方检验最多的变量值个数*/
			N_Distinct_Corr=5,
			/*N_Distinct_Corr考虑连续性相关检验最少的变量值个数*/
			maxpvalue=0.2 
			/*maxpvalue相关性变量筛选考虑Pvalue的最大值*/
			)	/ store des='1028.Correlation Rule Create';


	data _meta_sel_;
		set &inmeta.;
		variablename=upcase(variablename);
		keep variablename label;
		where N_dist_value<=&N_Distinct_ChiSQ. or N_dist_value>=&N_Distinct_Corr.;
	run;

	proc sort data=_meta_sel_;
		by variablename;
	run;

	%let chisqlist=;

	proc sql noprint;
		select distinct VariableName into:chisqlist separated by ' '
			from &inmeta.
				where N_dist_value<=&N_Distinct_ChiSQ. 
		;
	quit;

	%let corrlist=;

	proc sql noprint;
		select distinct VariableName into:corrlist separated by ' '
			from &inmeta.
				where N_dist_value>=&N_Distinct_Corr.
		;
	quit;

	%PowerChiSQ(
		intbl=&indata.,
		outbl=chisq_summary,
		respvar=&yvarnm.,
		ivlist=&chisqlist.);
	%PowerCorr(
		intbl=&indata.,
		outbl=corr_summary,
		respvar=&yvarnm.,
		ivlist=&corrlist.,
		corrtest= Pearson Spearman Hoeffding);

	data &outrule.;
		format DECISION $100.;
		format REASON $100.;
		format Rank 8.;
		merge _meta_sel_ chisq_summary corr_summary;
		by variablename;
		array ranklist rank_chisq rank_Hoeffding rank_Pearson rank_Spearman;
		array pvaluelist pvalue_chisq pvalue_Hoeffding pvalue_Pearson pvalue_Spearman;
		format INPUTMODE $50.;
		INPUTMODE="AUTOMATIC";
		format DATAROLE $50.;
		datarole="&RULE_DSN.";

		do over ranklist;
			if ranklist=. then
				ranklist=999999999;
		end;

		do over pvaluelist;
			if pvaluelist=. then
				pvaluelist=1;
		end;

		if min(of pvalue_chisq,pvalue_Hoeffding,pvalue_Pearson,pvalue_Spearman)>&maxpvalue. then
			do;
				DECISION="EXCLUDE";
				REASON="95-未通过相关性检验 (All Tests Insignificant P Value > &maxpvalue.)";
			end;

		Rank=min(of rank_chisq,rank_Hoeffding,rank_Pearson,rank_Spearman);

		do over ranklist;
			if ranklist=999999999 then
				ranklist=.;
		end;
		keep 
			ORDER DATAROLE VARIABLENAME LABEL DECISION 
			INPUTMODE REASON RANK df_chisq corr_chisq 
			pvalue_chisq rank_chisq corr_Pearson 
			pvalue_Pearson rank_Pearson corr_Spearman 
			pvalue_Spearman rank_Spearman corr_Hoeffding 
			pvalue_Hoeffding rank_Hoeffding ;
	run;

	proc sort data=&outrule. nodup;
		by rank;
	run;

	%cond_delete(intbl=_meta_sel_);
	%cond_delete(intbl=chisq_summary);
	%cond_delete(intbl=corr_summary);
%mend;

%macro BinMapRuleCreate(
	inrule=,
	outrule=,
	mapvar=WOE,
	maptype=WOE,
	createmode=AUTOMATIC,
	Nmissrep=-999999,
	Cmissrep=MISS
	)	/ store des='1029.Bin Map Rule';

	%cond_delete(intbl=&outrule.);
	%getVarType(intbl=&inrule.,invar=&mapvar.);
	%let mapvarfmttype=%upcase(&_VTYPE_.);

	proc sort data=&inrule.(keep=variablename label vartype) nodupkey out=_ranklist_;
		by variablename;
	run;

	%let N_RANK=;

	data _null_;
		set _ranklist_;
		call symputx('MAPVAR_'||left(put(_n_,8.)),variablename);
		call symputx('N_VARS',left(put(_n_,8.)));
	run;

	%do i=1 %to &N_VARS;

		data _MAP_tmp_;
			format DECISION $100.;
			format REASON $100.;
			format RULE_DESC $1000.;
			format RULE_RUN_MODE $20.;
			format INPUTMODE $50.;
			format TARGETVAR $40.;
			format TARGETLABEL $1000.;
			set &inrule.;
			DECISION="EXCLUDE";
			REASON="99-转换为&maptype.变量(Converted into &maptype. Variables)";

			%Create_New_VAR_Nm(invarnm=variablename,outvarnm=TARGETVAR,varprefix=_&maptype._);
			Targetlabel=cats("&maptype.:",strip(Label));
			RULE_DESC="09-生成&maptype.变量(Create &maptype. Variable)";
			RULE_RUN_MODE="IN";
			INPUTMODE="&createmode.";
			where variablename="&&MAPVAR_&i.";
		run;

		proc sort data=_MAP_tmp_;
			by bin;
		run;

		data _MAP_tmp_;
			format RULE $20000.;
			set _MAP_tmp_ end=lastobs;
			format lastrule $20000.;
			retain lastrule;
			format mapvalues $20000.;

			%if &mapvarfmttype.=N %then
				%do;
					mapvalues=strip(&mapvar.);
				%end;
			%else
				%do;
					mapvalues=quote(strip(&mapvar.));
				%end;

			if _N_=1 then
				do;
					if vartype="N" then
							rule=catx(' ',rule,"if",variablename,"=. then",
								variablename,"= &Nmissrep.;");

					if vartype="C" then
							rule=catx(' ',rule,"if",variablename,"='' then",
								variablename,"=")||"'"||"&Cmissrep."||"';";

					%if &mapvarfmttype.=N %then
						%do;
							rule=catx(' ',rule,'format',TARGETVAR,'8.3;');
						%end;
					%else
						%do;
							rule=catx(' ',rule,'format',TARGETVAR,'$2000.;');
						%end;

					rule=catx(' ',rule,'label',TARGETVAR,'= ',quote(strip(TARGETLABEL)),';');

					if vartype="N" then
						rule=catx(' ',rule,'if',variablename,'<=',BinUpper,'then',TARGETVAR,'=',strip(mapvalues),';');

					if vartype="C" then
						rule=catx(' ',rule,'if ',variablename,'in ("'||strip(tranwrd(BINVALUE,'|','","'))||'")','then',TARGETVAR,'=',strip(mapvalues),';');
				end;
			else if lastobs then
				do;
					if vartype="N" then
						rule=catx(' ','else',TARGETVAR,'=',strip(mapvalues),';');

					if vartype="C" then
						rule=catx(' ','if ',variablename,'in ("'||strip(tranwrd(BINVALUE,'|','","'))||'")','then',TARGETVAR,'=',strip(mapvalues),';');
				end;
			else
				do;
					if vartype="N" then
						rule=catx(' ','else if',variablename,'<=',BinUpper,'then',TARGETVAR,'=',strip(mapvalues),';');

					if vartype="C" then
						rule=catx(' ','if ',variablename,'in ("'||strip(tranwrd(BINVALUE,'|','","'))||'")','then',TARGETVAR,'=',strip(mapvalues),';');
				end;

			rule=catx(' ',lastrule,rule);
			lastrule=rule;

			if lastobs;
			drop BIN BinName OBS_P BINY_P N NY1 MIN MAX MEANX 
				Y1_DIST Y0_DIST ELOGIT WOE BINIV BINVALUE NVALUE
				BinLower BinUpper lastrule mapvalues;
		RUN;

		proc append base=&outrule. data=_MAP_tmp_ force;
		quit;

		%cond_delete(intbl=_MAP_tmp_ _ranklist_);
	%end;
%mend;

%macro BinRuleCreate(
		inrule=,
		outrule=,
		createmode=AUTOMATIC,
		Nmissrep=-999999,
		Cmissrep=MISS
		)	/ store des='1030.Bin Rule Create';

	%BinMapRuleCreate(
		inrule=&inrule.,
		outrule=&outrule.,
		mapvar=BinName,
		maptype=Binned,
		createmode=AUTOMATIC,
		Nmissrep=&Nmissrep.,
		Cmissrep=&Cmissrep.
		);
%mend;

%macro WOERuleCreate(
		inrule=,
		outrule=,
		createmode=AUTOMATIC,
		Nmissrep=-999999,
		Cmissrep=MISS
	)		/ store des='1031.WOE Rule Create';

	%BinMapRuleCreate(
		inrule=&inrule.,
		outrule=&outrule.,
		mapvar=WOE,
		maptype=WOE,
		createmode=AUTOMATIC,
		Nmissrep=&Nmissrep.,
		Cmissrep=&Cmissrep.
	);
%mend;
%macro BinIVRuleCreate(
	inrule=,
	outrule=,
	createmode=AUTOMATIC
	)	
/ store des='1031-2.Bin IV Rule Create'
;

	%cond_delete(intbl=&outrule.);

	proc sort data=&inrule.(keep=variablename label vartype N_DIST_VALUE) nodupkey out=_ranklist_;
		by variablename;
	run;

	%let N_RANK=;

	data _null_;
		set _ranklist_;
		call symputx('MAPVAR_'||left(put(_n_,8.)),variablename);
		call symputx('N_VARS',left(put(_n_,8.)));
		where N_DIST_VALUE>1;
	run;

	%do i=1 %to &N_VARS;
		%let curbinvar=&&MAPVAR_&i.;

		%put *****currently creating BIN IV rules for &curbinvar., &i. of &N_VARS.*****;

		data _MAP_tmp_;
			format DECISION $100.;
			format REASON $100.;
			format RULE_DESC $1000.;
			format RULE_RUN_MODE $20.;
			format INPUTMODE $50.;
			format TARGETVAR $40.;
			format TARGETLABEL $2000.;

			set &inrule.;
			DECISION="EXCLUDE";
			REASON="99-转换为BIN IV变量(Converted into BINIV Variables)";

			%Create_New_VAR_Nm(invarnm=variablename,
								outvarnm=TARGETVAR,
								varsuffix=_BINIV,
								maxvarlen=24
				);
			Targetlabel=cats("Bin IV:",strip(Label),"&curbinvar.");
			RULE_DESC="09-生成BIN IV变量(Create BIN IV Variable)";
			RULE_RUN_MODE="IN";
			INPUTMODE="&createmode.";
			where variablename="&curbinvar.";
		run;

		proc sort data=_MAP_tmp_;
			by bin;
		run;

		data _MAP_tmp_;
			format VariableName $32.;
			format TARGETVAR $40.;
			format BINIV_Type $20.;
			format RULE $20000.;
			format LABEL $1000.;
			format TARGETLABEL $2000.;
			set _MAP_tmp_;
			format TARGETVAR_Base $40.;
			format TARGETLabel_Base $2000.;
			TARGETVAR_Base=catx("_",TARGETVAR,_N_);
			TARGETLabel_Base=Targetlabel;

			if _N_=1 then
				do;
					BINIV_Type="NA";
					TARGETVAR=catx("_",TARGETVAR_Base,"NA");
					TARGETLabel=catx(" ",TARGETLabel_Base,": NA");
					rule=catx(" ","if MISSING(&curbinvar.) then",TARGETVAR,"=1; else",TARGETVAR,"=0;");
					rule=catx(" ",rule,"label",TARGETVAR,"=","'"||strip(targetlabel)||"'",";");
					output;
				end;

			if vartype="N" then
			do;


					
					if not missing(BinLower) then
					do;
						BINIV_Type="GT";
						TARGETVAR=catx("_",TARGETVAR_Base,"GT");
						TARGETLabel=catx(" ",TARGETLabel_Base,": GT","("||strip(Binlower)||",+)");
						rule=catx(" ","if (Not MISSING(&curbinvar.)) and &curbinvar.>",
								  BinLower,"then",TARGETVAR,"=1; else",TARGETVAR,"=0;");
						rule=catx(" ",rule,"label",TARGETVAR,"=","'"||strip(targetlabel)||"'",";");
						output;

					end;


					if not missing(BinUpper) then
					do;
						BINIV_Type="LE";
						TARGETVAR=catx("_",TARGETVAR_Base,"LE");
						TARGETLabel=catx(" ",TARGETLabel_Base,": LE","(-,"||strip(BinUpper)||"]");
						rule=catx(" ","if (Not MISSING(&curbinvar.)) and &curbinvar.<=",
								  BinUpper,"then",TARGETVAR,"=1; else",TARGETVAR,"=0;");
						rule=catx(" ",rule,"label",TARGETVAR,"=","'"||strip(targetlabel)||"'",";");
						output;

					end;
					if (not missing(BinLower)) and (not missing(BinUpper)) then
					do;
						BINIV_Type="BW";
						TARGETVAR=catx("_",TARGETVAR_Base,"BW");
						TARGETLabel=catx(" ",TARGETLabel_Base,": BW",binname);
						rule=catx(" ","if (Not MISSING(&curbinvar.)) and &curbinvar.>",
								  Binlower,"and &curbinvar.<=",BinUpper,"then",TARGETVAR,"=1; else",TARGETVAR,"=0;");
						rule=catx(" ",rule,"label",TARGETVAR,"=","'"||strip(targetlabel)||"'",";");
						output;
					end;

			end;

			if vartype="C" then
			do;
				BINIV_Type="C";
				TARGETVAR=catx("_",TARGETVAR_Base,"C");
				TARGETLabel=catx(" ",TARGETLabel_Base,":",binname);
				rule=catx(" ","if (Not MISSING(&curbinvar.)) and &curbinvar. in ("||'"'||strip(tranwrd(BINVALUE,'|','","'))||'")','then',TARGETVAR,'= 1; else',TARGETVAR,'=0;');
				rule=catx(" ",rule,"label",TARGETVAR,"=","'"||strip(targetlabel)||"'",";");
				output;
			end;

			keep RULE DECISION REASON RULE_DESC RULE_RUN_MODE INPUTMODE BINIV_Type 
				TARGETVAR TARGETLABEL BinName BinLower BinUpper binvalue nvalue VariableName 
				Resp_Var Resp_Lab Label VarType bin N N_MISSING MISSING_P NY1 OBS_P N_DIST_VALUE;
		RUN;

		proc append base=&outrule. data=_MAP_tmp_ force;
		quit;

		%cond_delete(intbl=_MAP_tmp_ _ranklist_);
	%end;
%mend;
%macro ScoreRuleCreate(inrule=,outrule=,createmode=AUTOMATIC)
		/ store des='1032.Score Rule Create';
	%BinMapRuleCreate(inrule=&inrule.,outrule=&outrule.,mapvar=SCORE,maptype=SCORE,createmode=AUTOMATIC);
%mend;

%macro ManualRuleCreate(
			SourceVarnm=,
			TargetVarnm=,
			TargetVarLabel=,
			RuleDesc=,
			Code=,
			OutRule=
			) / store des='1033.Manual Rule Create';

	%getVarLabel(intbl=&ModelLibNm..&TrainNm.,invar=&SourceVarnm.);

	data tmpapp;
		format inputmode $50.;
		INPUTMODE="MANUAL";
		output;
	run;

	data &OutRule.;
		set &ModelLibNm..&RuleNm.(obs=0) tmpapp;

		/*		call missing(of _ALL_);*/
		DATAROLE="&RULE_DSN.";
		VARIABLENAME="&SourceVarnm.";
		LABEL="&_VLABEL_.";
		RULE="%bquote(&code.)";
		RULE_DESC="&RuleDesc.";
		RULE_RUN_MODE="IN";
		INPUTMODE="MANUAL";
		TARGETVAR="&TargetVarnm.";
		TARGETLABEL="&TargetVarLabel.";
	run;

	%cond_delete(intbl=tmpapp);
%mend;

/*%ManualRuleCreate(*/
/*	OutRule=MOB_25,*/
/*	RuleDesc=生成账龄拐点虚拟变量以构造非线性,*/
/*	SourceVarnm=MOB,*/
/*	TargetVarnm=MOB_25,*/
/*	TargetVarLabel=账龄拐点虚拟变量：25个月,*/
/*	Code=%nrstr(if MOB<=25 then MOB_25=0; else MOB_25=1;)*/
/*	);*/
%macro metadata_check_in(ctrtbl=,chkintbl=,runorder=&GStepOrder.)
		/ store des='1034.Meta Data Check in';

	%cond_delete(intbl=_chkintmp_);
	%cond_delete(intbl=_ctr_var);
	%cond_delete(intbl=_chk_var);

	proc contents data=&ctrtbl. out=_ctr_var noprint;
	run;

	proc contents data=&chkintbl. out=_chk_var noprint;
	run;

	proc sql noprint;
		select NAME into: overlapvarlist separated by ' '
			from _chk_var
				where upcase(NAME) in (select upcase(NAME) from _ctr_var)
		;
	quit;

	data _chkintmp_;
		format ORDER 8.;
		set &chkintbl.;
		order =&runorder.;
		keep &overlapvarlist. order;
	run;

	data &ctrtbl.;
		set &ctrtbl.;
		where order not in (&runorder.);
	run;

	proc append base=&ctrtbl. data=_chkintmp_ force;
	quit;

	proc sort data=&ctrtbl.;
		by order;
	run;

	%cond_delete(intbl=_chkintmp_);
	%cond_delete(intbl=_ctr_var);
	%cond_delete(intbl=_chk_var);
	%let GStepOrder=%eval(&GStepOrder.+1);
%mend;

%macro decision_check_in(ctrtbl=&ModelLibNm..&DecisionNm.,chkintbl=,runorder=&GStepOrder.)
		/ store des='1035.Decision Check in';
	%metadata_check_in(ctrtbl=&ctrtbl.,chkintbl=&chkintbl,runorder=&runorder.);
%mend;

%macro rule_check_in(ctrtbl=&ModelLibNm..&RuleNm.,chkintbl=,runorder=&GStepOrder.)
		/ store des='1036.Rule Check in';
	%metadata_check_in(ctrtbl=&ctrtbl.,chkintbl=&chkintbl,runorder=&runorder.);
	%let nfixvar=%getOBS2(_FIXVAR_LIST_);
	%if &nfixvar.>0 %then
	%do;
		proc sql noprint;
		create table _new_fix_iv_ as
			select upcase(targetvar) as variablename 
			from &chkintbl.
			where upcase(variablename) in 
			(select upcase(variablename) from _FIXVAR_LIST_)
		;
		quit;

		data  _FIXVAR_LIST_;
			set _FIXVAR_LIST_ _new_fix_iv_;
		run;
		proc sort data=_FIXVAR_LIST_ nodupkey;
			by variablename;
		run;
		quit;
	%end;
%mend;

%macro stat_check_in(ctrtbl=&ModelLibNm..&statnm.,chkintbl=,runorder=&GStepOrder.)
		/ store des='1037.Stat Check in';
	%metadata_check_in(ctrtbl=&ctrtbl.,chkintbl=&chkintbl,runorder=&runorder.);
%mend;

%macro binstat_check_in(ctrtbl=&ModelLibNm..&BinStatNm.,chkintbl=,runorder=&GStepOrder.)
		/ store des='1038.Binstat Check in';
	%metadata_check_in(ctrtbl=&ctrtbl.,chkintbl=&chkintbl,runorder=&runorder.);
%mend;

%macro corr_check_in(ctrtbl=&ModelLibNm..&CorrNm.,chkintbl=,runorder=&GStepOrder.)
		/ store des='1039.Rule Check in';
	%metadata_check_in(ctrtbl=&ctrtbl.,chkintbl=&chkintbl,runorder=&runorder.);
%mend;

%macro GetModelRules(
			ruledata=&ModelLibNm..&RuleNm.,
			modelestdata=&ModelLibNm..&EstimateNm.,
			outdata=_modelrules_)
		/ store des='1040-2.Get Model Rule'
;

	data _rule_data_ _estvarrule_(where=(rowno<0));
		format rowno 8.;
		set &ruledata.;
		variablename= strip(upcase(variablename));
		TARGETVAR= strip(upcase(TARGETVAR));
		rowno=_N_;
	run;

	data _varlist_ ;
		set &modelestdata.;
		variablename= strip(upcase(variablename));
		if variablename not in ("INTERCEPT");
		rowno=.;
		keep variablename rowno;
	run;

	%let estvarno=1;

	%do %while(&estvarno.>0);
		proc sql noprint;
			create table _varbefore_ as
			select *
				from _rule_data_
			where strip(upcase(TARGETVAR)) in 
				(select upcase(variablename) from _varlist_) 
				and rowno not in
				(select rowno from _varlist_)
			;
		quit;

/*		proc print data=_varbefore_;*/
/*		run;*/

		%getOBS(_varbefore_);

		%let estvarno=&_NOBS_.;
		%put ****obs=&estvarno.*****;

		proc append base=_estvarrule_ data=_varbefore_ force; quit;

		data _varlist_;
			set _varbefore_;
		run;
		%cond_delete(intbl=_varbefore_);

	%end;

	proc sort data=_estvarrule_ out=&outdata.;
		by rowno;
	run;

	%cond_delete(intbl=_ESTVARRULE_  _varlist_ _rule_data_);
	
%mend;

%macro ModelRuleExec(
			indata=,
			ruledata=&ModelLibNm..&RuleNm.,
			modelestdata=&ModelLibNm..&EstimateNm.,
			outdata=,
			ruleorder=
	)/ store des='1040.Model Rule Execute';

	%GetModelRules(
			ruledata=&ruledata.,
			modelestdata=&modelestdata.);

	%DataRuleExec(
				indata=&indata.,
				ruledata=_modelrules_,
				outdata=&outdata.,
				ruleorder=&ruleorder.
	);


	%cond_delete(intbl=_modelrules_);
	
%mend;


%macro DataRuleExec(
			indata=,
			ruledata=&ModelLibNm..&RuleNm.,
			outdata=,
			ruleorder=)
		/ store des='1041.Data Rule Execute';
	/*ruleorder can be a list of values (separated by space), for example: 1 4 5*/
	/*	options minoperator mindelimiter=',';*/
	%let MACRONULLVALUE=;

	%if &outdata.=&macronullvalue. %then
		%let outdata=&indata.;
	%let ruleorder=&ruleorder.;

/*	proc sort data= &ruledata.;*/
/*		by order;*/
/*	run;*/

	data _inrule_ _outrule_;
		set &ruledata.;

		if upcase(RULE_RUN_MODE)="IN" then
			output _inrule_;

		if upcase(RULE_RUN_MODE)="OUT" then
			output _outrule_;

		%if &ruleorder^=&macronullvalue. %then
			%do;
				where order in (&ruleorder.);
			%end;
	run;

	/*	options nominoperator;*/
	%runtblcode(codetbl=_inrule_,codevar=RULE,codefilenm=inds);
	%runtblcode(codetbl=_outrule_,codevar=RULE,codefilenm=outds);

	data &outdata.;
		set &indata.;

		%include inds;
	run;

	%let targettbl=&outdata.;

	%include outds;

	%cond_delete(intbl=_inrule_);
	%cond_delete(intbl=_outrule_);
%mend;

%macro GetStat(datain=,dataout=,datarole=)
		/ store des='1042.Get Statistics';

	proc contents data=&datain out=_IndataMetaTmp_ noprint;
	run;

	proc sql noprint;
		create table _keepvarlist_ as
		select name from _indatametatmp_
				where upcase(name) not in 
					(select upcase(variablename) from &ModelLibNm..&DecisionNm. 
						where upcase(decision) in ('EXCLUDE')) 
				  and upcase(name) not in 
					(select upcase(variablename) from &ModelLibNm..&StatNm.)
				  and upcase(name) not in ("%upcase(&RespYnm.)")
		;
	quit;

	data _null_;
		set _keepvarlist_;
		call symputx('xkvar_'||left(put(_n_,8.)),Name);
		call symputx('nkeepvar',left(put(_n_,8.)));
	run;

	%put **************Total Number of Variable for Analysis: &nkeepvar.***********************;

	%if &nkeepvar.>0 %then
		%do;

			data _analysis_tmp_;
				set &datain.;
				keep 
					%do v=1 %to &nkeepvar.;
						&&xkvar_&v
					%end;
			;
			run;

			%powercontents(input = _analysis_tmp_,output = &dataout.,datarole=&datarole.);
			%cond_delete(intbl=_analysis_tmp_);
		%end;

	%cond_delete(intbl=_indatametatmp_ _keepvarlist_);
%mend;


%macro PowerCorr(intbl=,outbl=,respvar=,ivlist=_ALL_,corrtest=Pearson Spearman Hoeffding)
		/ store des='1043.Correlation Analysis';

	proc contents data=&intbl.(keep=&ivlist.)
		noprint out=_corrvarinfor_;
	run;

	proc sql noprint;
		select NAME into:corrivlist separated by ' '
			from _corrvarinfor_
				where upcase(name) not in ("%upcase(&respvar.)") and type=1
		;
		select count(*) into:varnum
			from _corrvarinfor_
				where name not in ("&respvar.") and type=1
		;
	quit;

	%let varnum=&varnum.;

	proc datasets nolist;
		delete _corrvarinfor_;
	quit;

	%let jj=1;
	%let odsstring1=;
	%let odsstring2=;

	%do %while (%scan(&corrtest.,&jj)^=);
		%let testname=%scan(&corrtest.,&jj);
		%let odsstring1=&odsstring1. &testname.corr;
		%let odsstring2=&odsstring2. &testname.corr %str(=) &testname.;
		%let jj=%eval(&jj+1);
	%end;

	ods select &odsstring1.;
	ods output &odsstring2.;

	proc corr data=&intbl. &corrtest. rank;
		var &corrivlist.;
		with &respvar.;
	run;

	%let kk=1;

	%do %while (%scan(&corrtest.,&kk)^=);
		%let outdatanm=%scan(&corrtest.,&kk);

		data &outdatanm.1(keep=variablename corr_&outdatanm pvalue_&outdatanm. rank_&outdatanm.);
			length variablename $ 32;
			format corr_&outdatanm 8.3;
			format pvalue_&outdatanm.  pvalue6.4;
			set &outdatanm.;
			label 
				rank_&outdatanm. = "&outdatanm. Rank*of Variables"
				corr_&outdatanm. = "&outdatanm. Correlation"
				pvalue_&outdatanm. = "&outdatanm. p-value"
			;
			array best(&varnum.) best1--best&varnum.;
			array r(&varnum.) r1--r&varnum.;
			array p(&varnum.) p1--p&varnum.;

			do i=1 to &varnum.;
				variablename=best(i);
				corr_&outdatanm=r(i);
				pvalue_&outdatanm.=p(i);
				rank_&outdatanm.=i;
				output;
			end;
		run;

		proc sort data=&outdatanm.1;
			by variablename;
		run;

		proc datasets nolist;
			delete &outdatanm.;
			change &outdatanm.1=&outdatanm.;
		quit;

		%let kk=%eval(&kk.+1);
	%end;

	data &outbl.;
		merge &corrtest.;
		variablename=upcase(variablename);
		by variablename;
	run;

	proc datasets nolist;
		delete &corrtest.;
	quit;

	%let sorttest=%scan(&corrtest.,1);

	proc sort data=&outbl.;
		by variablename;
	run;

%mend;

%macro PowerChiSQ(intbl=,outbl=,respvar=,ivlist=_ALL_)
		/ store des='1044.ChiSQ Analysis';
	ods output chisq=_chisq_tmp;

	proc freq data=&intbl.;
		table &respvar.*(&ivlist.)/ chisq norow nocol missing nopercent;
	run;

		quit;

		ods output close;

		proc sort data=_chisq_tmp;
			by prob;
		run;

		data &outbl.;
			set _chisq_tmp;

			table=compress(scan(table,2,"*"),") ");
			rank_chisq=_N_;

			table=upcase(table);
			rename 
				table=variablename 
				value=corr_chisq
				prob=pvalue_chisq
				df=df_chisq
			;
			drop statistic;
			where  upcase(statistic)="CHI-SQUARE" or statistic="卡方";
		run;

		proc sort data=&outbl.;
			by variablename;
		run;

		%cond_delete(intbl=_chisq_tmp);
%mend;


%macro GetDecision(metadata=&ModelLibNm..&DecisionNm.,outtbl=)
		/ store des='1045.Get Decision';

	proc sort data=&metadata. out=&outtbl.;
		by variablename order;
	run;

	data &outtbl.;
		set &outtbl.;
		by variablename;
		variablename=upcase(variablename);
		decision=upcase(decision);
		datarole=upcase(datarole);
		INPUTMODE=upcase(INPUTMODE);

		if last.variablename;
	run;

%mend;
%macro LogitReg(
			traindata=&ModelLibNm..&TrainNm.,
			validdata=&ModelLibNm..&ValidNm.,
			vary=&RespYnm.,
			varxfix=,
			varxflex=,
			varsel=Y,
			outest=_esti_,
			outpara=_para_,
			interopt=N,
			step=AUTO)
		/ store des='1046.Logistics Regression';
	%let MACRONULLVALUE=;

	%let validdata=&validdata.;

	%let step=&step.;
	%let nfixvar=0;

	%if &varxfix^=&macronullvalue. %then
		%let nfixvar=%sysfunc(countw(&varxfix.));
	%let nflexvar=0;

	%getOBS(&varxfix.);
	%let nfixvar=&_NOBS_.;

	%getOBS(&varxflex.);
	%let nflexvar=&_NOBS_.;

	%put **************N of Fix IVs: &nfixvar.******************;
	%put **************N of Flexible IVs: &nflexvar.******************;
	%let varsel=%upcase(&varsel.);

	%if %length(&step.)=0 %then %let step=AUTO;

	data varxtbl;
		set &varxfix. &varxflex.;
	run;

	data _null_;
		set varxtbl;
		call symputx('xvar_'||left(put(_n_,8.)),VariableName);
		call symputx('n_vars',left(put(_n_,8.)));
	run;

	ods graphics on;

	%if &varsel=Y %then
		%do;
			/*Stepwise Model Scoring*/
			ods select ASEPlot CriterionPanel SelectionSummary FitStatistics ParameterEstimates SelectedEffects;
			ods output SelectionSummary=_SEL_STEPS_;
			proc glmselect data=&traindata.
				%if  %length(&validdata.)>0 %then
					%do;
						VALDATA=&validdata.
					%end;

				plots(stepAxis=number)=(criterionPanel ASEPlot);
				model &vary. = 
					%do v=1 %to &n_vars.;
						&&xvar_&v
					%end;
				/ selection=stepwise (

				%if &nfixvar>0 %then

					%do;
						include=&nfixvar.
					%end;

				%if %upcase(&step.)^=AUTO %then
					%do;
						steps=&step.
					%end;
				%else
					%do;
						choose=VALIDATE
					%end;

				) /* hierarchy=single */
				;
			run;

			%let varxfinal=&_GLSIND1.;
		%end;

	/*  ods select ParameterEstimates Association;*/
	ods output ParameterEstimates=&outest.;

	proc logistic data=&traindata. desc NAMELEN=200 outmodel=&outpara.  plots=roc;
		model &vary.=

		%if &varsel=Y %then %do; &varxfinal.  %end;
		%else 
		%do;
			%do v=1 %to &n_vars.;
				&&xvar_&v
			%end;
		%end;
			/outroc=troc;

		%if  %length(&validdata.)>0 %then
			%do;
				score data=&validdata. outroc=vroc out=_validscored;
			%end;
	run;

	%cond_delete(intbl=_validscored);
	%cond_delete(intbl=varxtbl);
	%cond_delete(intbl=troc);
	%cond_delete(intbl=vroc);
	ods graphics off;
%mend;

%macro Model_Fitting(
			rulein=&ModelLibNm..&DecisionNm.,
			traintbl=&ModelLibNm..&TrainNm.,
			validtbl=&ModelLibNm..&ValidNm.,
			yvar=&RespYnm.,
			paraout=&ModelLibNm..&ScoreParaNm.,
			outest=&ModelLibNm..&EstimateNm.,
			ruleout=_model_result_,
			varsel=Y,
			step=AUTO)
		/ store des='1047.Model Fitting';

	%let varsel=%upcase(&varsel.);
	%let flexIVlist=;
	%let fixIVlist=;

	%GetDecision(metadata=&rulein.,outtbl=decision_used);


	data _fixIV_;
		set decision_used;
		where decision='INCLUDE' and INPUTMODE='MANUAL';
	run;

	%if &varsel.=Y %then
		%do;

			data _VarDropBeforeModel_;
				set decision_used;
				where (upcase(datarole)^='MODEL' or INPUTMODE='MANUAL') and decision in ('EXCLUDE');
			run;

			proc contents data=&traintbl. out=trainvarlist noprint;
			run;

			data trainvarlist(rename=(name=VARIABLENAME));
				set trainvarlist;
				name=upcase(name);
			run;

			proc sql noprint;
				create table _FlexIV_ as 
					select VARIABLENAME,LABEL
						from trainvarlist
							where VARIABLENAME not in ("%upcase(&yvar.)")
								and VARIABLENAME not in
							(select variablename from _VarDropBeforeModel_)
								and variablename not in
							(select variablename from _FixIV_)
				;
			quit;

		%end;
	%else
		%do;

			data _FlexIV_;
				set decision_used;
				where upcase(decision)='INCLUDE' and INPUTMODE='AUTOMATIC';
			run;

		%end;

	data _IVlist_;
		set _FixIV_ _FlexIV_;
	run;

	%LogitReg(
		traindata=&traintbl.,
		validdata=&validtbl.,
		vary=&yvar.,
		varxfix=_FixIV_,
		varxflex=_FlexIV_,
		varsel=&varsel.,
		outest=_est_,
		outpara=&paraout.,
		step=&step.
		);

	proc sql noprint;
		create table _ModelInclude_(rename=(variable=variablename)) as
			select 
				'MODEL' as DATAROLE format=$50., 
				'INCLUDE' as DECISION format=$100.,
				'模型选择接受 (Model Accept)' as REASON format=$100.,
				'AUTOMATIC' as INPUTMODE format=$50.,
				B.LABEL,
				A.*
			from _est_ A
				left join _IVlist_  B
					on upcase(A.variable)=upcase(B.variablename)
				order by A.WaldChiSq desc
		;
		create table _ModelInclude2_ as
			select * from _ModelInclude_
				where upcase(variablename) not in 
					(select upcase(variablename) from _fixIV_)
		;
	quit;

	proc sql noprint;
		create table _ModelExclude_  as
			select 
				'MODEL' as DATAROLE format=$50., 
				'EXCLUDE' as DECISION format=$100.,
				'模型选择排除 (Model Rejection)' as REASON format=$100.,
				'AUTOMATIC' as INPUTMODE format=$50.,
				B.*
			from _IVlist_  B 
				where upcase(variablename) not in
					(select upcase(variable) from _est_)
		;
	quit;

	data &outest.;
		set _ModelInclude_;
		variablename=upcase(variablename);
		drop DATAROLE DECISION REASON INPUTMODE;
	run;

	data &ruleout.;
		set  _ModelExclude_ _ModelInclude2_;
		variablename=upcase(variablename);

		if variablename not in ("INTERCEPT");
	run;

	%let droptbllist=DECISION_USED trainvarlist _VarDropBeforeModel_  _fixIV_ _FlexIV_   _est_ _IVLIST_ _MODELINCLUDE_ _ModelInclude2_ _MODELEXCLUDE_;

	%cond_delete(intbl=&droptbllist.);
%mend;

%macro AUTO_ADJ_STEPWISE(intbl=_SEL_STEPS_,min_incre_percent=&STEP_ADJ_MIN_PER.)
		/ store des='1048.Stepwise Auto-Adjustment';

	%global Adj_GLM_LR_Step;
	%let Adj_GLM_LR_Step=0;

	proc sql noprint;
		select max(ValidationASE) into: maxSE from &intbl.;
		select ValidationASE into: selSE from &intbl. where OptValidationASE;
		select Step into: ORIGIN_STEP from &intbl. where OptValidationASE;
	quit;

	data _step_opt_;
		format incre_percent percentn8.2;
		retain keeprow 1;
		set &intbl.;
		incre_percent=(ValidationASE-&selSE.)/(&maxSE.-&selSE.);

		if keeprow and incre_percent>=&min_incre_percent. then
			keeprow=1;
		else keeprow=0;

		if keeprow;
	run;

	proc sql noprint;
		select max(Step) into:  Adj_GLM_LR_Step from _step_opt_;
	quit;

	%let ORIGIN_STEP=&ORIGIN_STEP.;
	%let Adj_GLM_LR_Step=&Adj_GLM_LR_Step.;

	%if &ORIGIN_STEP.<=15 %then %let Adj_GLM_LR_Step=&ORIGIN_STEP.;
	%if &ORIGIN_STEP.=0 %then %let Adj_GLM_LR_Step=1;

	%cond_delete(intbl=_step_opt_ );
	%put ************Before Adjustment Step = &ORIGIN_STEP.***************;
	%put *************After Adjustment Step = &Adj_GLM_LR_Step.***************;
%mend;


%macro Reg_WOE_Mode(nbin=,minp=,mindiff=)
		/ store des='1049.Logistics Reg with WOE';

	/**************************************************************************/
	/********************IV Calculation & Variable Screen**********************/
	/**************************************************************************/
	%Prep_train_data;

	%AutoBin(
		intbl=_main_model_tmp_,
		DV=&RespYnm.,
		IV=_ALL_,
		outbintbl=_Bin_,
		nbin=&nbin.,
		minp=&minp.,
		mindiff=&mindiff.,
		datarole=&RULE_DSN.,
		Nmissrep=&NumMiss_Rep.,
		Cmissrep=&CharMiss_Rep.,
		charsep=|);


	%binstat_check_in(chkintbl=_bin_);



	proc sql noprint; 
		create table VAR_IV_Stat as
			select distinct 
				datarole,
				label, 
				variablename,
			case 
				when vartype='N' then 1 
				else 2 
			end 
		as type, 
			iv, 
			N_BIN,
			N_DIST_Value,
			MAXPERCENT, 
			MISSING_P,
			Value, 
			MaxBinIV,
			MaxBinIV_P
		from _bin_
			order by iv desc,variablename;
	quit;

	%ExcludeRuleCreate(inrule=VAR_IV_Stat,outrule=_ExcludeRule_);
	%decision_check_in(chkintbl=_ExcludeRule_);

	proc sql noprint;
		create table _WOElist_ as
			select * from &ModelLibNm..&BinStatNm.
				where upcase(variablename) not in 
					(select upcase(variablename) from &ModelLibNm..&DecisionNm.
						where decision in ("EXCLUDE")
					)
		;
	quit;


	%WOERuleCreate(
		inrule=_WOElist_,
		outrule=_WOERule_,
		Nmissrep=&NumMiss_Rep.,
		Cmissrep=&CharMiss_Rep.
		);
	%decision_check_in(chkintbl=_WOERule_);
	%rule_check_in(chkintbl=_WOERule_);

	%let nfixvar=%getOBS2(_FIXVAR_LIST_);
	%if &nfixvar.>0 %then
	%do;
		proc sql noprint;
			select upcase(variablename) into: fixedvarlist separated by  ' '
			from _FIXVAR_LIST_
			where upcase(Variablename) not in 
				(select upcase(variablename) from &ModelLibNm..&DecisionNm. 
					where decision in ("EXCLUDE"))
		;
		quit;
		%let fixedvarlist=&fixedvarlist.;
		%if %length(&fixedvarlist.)>0 %then
		%do;
			%INCLUDE_VAR(vars=&fixedvarlist.,reason=人工预选入模变量);
		%end;
	%end;

	%DataRuleExec(indata=&ModelLibNm..&TrainNm.);
	%DataRuleExec(indata=&ModelLibNm..&ValidNm.);

	/**************************************************************************/
	/********************Regression Estimate*****************/
	/**************************************************************************/

	%Model_Fitting;
	
	%if "%upcase(&AUTO_STEP_ADJ.)"="Y" %then
	%do;
		%AUTO_ADJ_STEPWISE;
		%Model_Fitting(step=&Adj_GLM_LR_Step.);
	%end;

	
	%decision_check_in(chkintbl=_model_result_,runorder=90);
	%let runtmplist=_BIN_ _bin_2_ VAR_IV_Stat _ExcludeRule_ _WOElist_ _WOERule_ _model_result_;

	%cond_delete(intbl=&runtmplist. _SEL_STEPS_ _main_model_tmp_);
%mend;

%macro Reg_FBin_Mode
		/ store des='1050.Logistics Reg with Fine Bin';
	%Reg_WOE_Mode(nbin=20,minp=5,mindiff=0);
%mend;

%macro Reg_CBin_Mode
		/ store des='1051.Logistics Reg with Coarse Bin';
	%Reg_WOE_Mode(nbin=20,minp=10,mindiff=5);
%mend;

%macro Reg_DV_Mode
		/ store des='1052.Logistics Reg with Dummy Variables';

	/**************************************************************************/
	/******************Variable Screen, DV creation and Transformation*********/
	/**************************************************************************/
	%Prep_train_data;

	%let RULE_DSN=%upcase(&RULE_DSN.);
	%let RULE_DSN=&RULE_DSN._1;

	%GetStat(datain=_main_model_tmp_,dataout=_TrainMetaTmp_,datarole=&RULE_DSN.);

	%MISS_DVRuleCreate(
		inrule=_TrainMetaTmp_,
		outrule=_missrules_);

	proc sql noprint;
	select variablename into: autodvvarlist separated by ' '
	from _TrainMetaTmp_
	where type=2 or 
		(Type=1 and (not (value='0, 1' or value='1, 0'))
				and N_dist_value<=&GNumDVLvlN. 
				and MaxPercent<&GMaxFreqDropP.)
	;
	quit;

	/*statistics check in*/
	%stat_check_in(chkintbl=_TrainMetaTmp_);
	%ExcludeRuleCreate(
		inrule=_TrainMetaTmp_,
		outrule=excluderules,
		Method=DV);

		
	%AutoDummyRuleCreate(
		intbl=_main_model_tmp_,
		varnm=&autodvvarlist.,
		outruletable=_autodvrule_,
		MissingDummy=Y,
		MaxLevel=10,
		delimiter=,
		keepOriginal=Y,
		DATAROLE=&RULE_DSN.
		);

	%MODE_DVRuleCreate(
		inrule=_TrainMetaTmp_,
		outrule=_dvrules_);

	data dvrules;
		set _missrules_ _autodvrule_(drop=order) _dvrules_;
	run;

	/*Decision check in: variable exclusion (data quality and DV)*/
	%decision_check_in(chkintbl=excluderules);

	/*Rule check in: DV*/
	%rule_check_in(chkintbl=dvrules);

	proc sql noprint;
		create table _TrainMetaTmp2_ as
			select * from _TrainMetaTmp_
				where variablename not in 
					(select variablename from &ModelLibNm..&DecisionNm.
						where upcase(DECISION) in ('EXCLUDE')
					)
		;
	quit;

	%TranTrimRuleCreate(
		inrule=_TrainMetaTmp2_,
		outrule=SmoothOutlierRule,
		MinSkewness=&GMinSkewness.,
		min_factor=&Gmin_factor., 
		max_factor=&Gmax_factor., 
		min_distinct_value=&Gmin_distinct_value.
		);

	/*Rule Check In: smooth and outliers*/
	%rule_check_in(chkintbl=SmoothOutlierRule);

	%Prep_train_data;


	/**************************************************************************/
	/********************Variable Bivariate Correlation Screen*****************/
	/**************************************************************************/
	%let RULE_DSN=%substr(&RULE_DSN.,1,%length(&RULE_DSN.)-2);
	%let RULE_DSN=&RULE_DSN._2;

	%GetStat(datain= _main_model_tmp_,dataout=_TRAINStatTmp_,datarole=&RULE_DSN.);

	/*statistics check in*/
	%stat_check_in(chkintbl=_TRAINStatTmp_);

	%ExcludeRuleCreate(
		inrule=_TRAINStatTmp_,
		outrule=excluderules2,
		Method=);

	%decision_check_in(chkintbl=excluderules2);

	proc sql noprint;
		create table train_stat as
			select * from &ModelLibNm..&statnm.
				where  upcase(variablename) not in
				(select upcase(variablename) from &ModelLibNm..&DecisionNm.
					where upcase(decision) in ("EXCLUDE"));
	run;

	%CorrRuleCreate(
		indata=_main_model_tmp_,
		inmeta=train_stat,
		yvarnm=&RespYnm.,
		outrule=CorrRule,
		N_Distinct_ChiSQ=&GN_Distinct_ChiSQ.,
		N_Distinct_Corr=&GN_Distinct_Corr.,
		maxpvalue=&Gmaxpvalue.
		);

	%corr_check_in(chkintbl=CorrRule);

	data CorrRule;
		set CorrRule;
		where decision is not null;
	run;

	%decision_check_in(chkintbl=CorrRule);

	%let nfixvar=%getOBS2(_FIXVAR_LIST_);
	%if &nfixvar.>0 %then
	%do;
		proc sql noprint;
			select upcase(variablename) into: fixedvarlist separated by  ' '
			from _FIXVAR_LIST_
			where upcase(Variablename) not in 
				(select upcase(variablename) from &ModelLibNm..&DecisionNm. 
					where decision in ("EXCLUDE"))
		;
		quit;
		%let fixedvarlist=&fixedvarlist.;
		%if %length(&fixedvarlist.)>0 %then
		%do;
			%INCLUDE_VAR(vars=&fixedvarlist.,reason=人工预选入模变量);
		%end;
	%end;
	/**************************************************************************/
	/********************Regression Estimate*****************/
	/**************************************************************************/

	%DataRuleExec(indata=&ModelLibNm..&TrainNm.);
	%DataRuleExec(indata=&ModelLibNm..&ValidNm.);

	%Model_Fitting;

	%if "%upcase(&AUTO_STEP_ADJ.)"="Y" %then
	%do;
		%AUTO_ADJ_STEPWISE;
		%Model_Fitting(step=&Adj_GLM_LR_Step.);
	%end;


	%decision_check_in(chkintbl=_model_result_,runorder=90);

	%let droptmplist=_missrules_ _main_model_tmp_ _tmp_master 
						SPECIALVALUERULES excluderules excluderules2 
					_autodvrule_ _dvrules_ dvrules _MasterMetaTmp_ 
		 SmoothOutlierRule _TrainMetaTmp2_ CorrRule _TRAINMETATMP_
		_TRAINStatTmp_ train_stat _MODEL_RESULT_ _SEL_STEPS_;

	%cond_delete(intbl=&droptmplist.);

	%let RULE_DSN=%substr(&RULE_DSN.,1,%length(&RULE_DSN.)-2);

%mend;

%macro Reg_DVBIN_Mode
	/ store des='1053.Logistics Reg with DV and Bins';

	%Prep_train_data;

	%if &G_Bin_Reset.=N 
		and %sysfunc(exist(&ModelLibNm..BINSTAT_&ModelDSN._&method2.)) 
	%then
		%do;
			data _bin_;
				set &ModelLibNm..BINSTAT_&ModelDSN._&method2.;
			run;
			%binstat_check_in(chkintbl=_bin_);
		%end;
	%else
		%do;
			%let nbin=20;
			%let minp=10;
			%let mindiff=5;

			%AutoBin(
				intbl=_main_model_tmp_,
				DV=&RespYnm.,
				IV=_ALL_,
				outbintbl=_Bin_,
				nbin=&nbin.,
				minp=&minp.,
				mindiff=&mindiff.,
				datarole=&RULE_DSN.,
				Nmissrep=&NumMiss_Rep.,
				Cmissrep=&CharMiss_Rep.,
				charsep=|);

			%binstat_check_in(chkintbl=_bin_);

			%let G_Bin_Reset=N; 

		%end;


	proc sql noprint;
		create table _DVBINlist_ as
			select * from &ModelLibNm..&BinStatNm.
				where upcase(variablename) not in 
					(select upcase(variablename) from &ModelLibNm..&DecisionNm.
						where decision in ("EXCLUDE")
					)
		;
	quit;

	%BinRuleCreate(
		inrule=_DVBINlist_,
		outrule=_DVBINRule_
		);
	%rule_check_in(chkintbl=_DVBINRule_);

	%let dropafterautobin=&dropafterautobin.;

	%if %length(&dropafterautobin.)>0 %then
		%do;
			%EXCLUDE_VAR(vars=&dropafterautobin.,reason=00-此变量只能用分箱方案入模);
		%end;
	%cond_delete(intbl=_bin_ _DVBINlist_ _DVBINRule_);

	%Reg_DV_Mode;
%mend;


%macro decision_override(
			decision=EXCLUDE,
			vars=,
			reason=,
			ctrltbl=&ModelLibNm..&DecisionNm.,
			traindata=&ModelLibNm..&TrainNm.,
			steporder=999
			)
		/ store des='1054.Decision Override';

	%local N_VOR _VOR_ _TmpVar_ tranvars;
	%put ***Decision Table: &ctrltbl.***;
	%let vars=%upcase(&vars.);
	%let N_VOR=0;
	%let N_VOR=%sysfunc(Countw(&vars.));

	data tmpapp;
		ORDER=&steporder.;
		output;
	run;

	data _decitmp_;
		set &ctrltbl.(obs=0) tmpapp;

		/*		call missing(of _all_);*/
		/*		ORDER=999;*/
		DATAROLE="MODEL";
		INPUTMODE="MANUAL";
		REASON="&reason.";
		DECISION="%upcase(&decision.)";

		%do _VOR_ =1 %to &N_VOR.;
			%let _TmpVar_=%scan(&vars.,&_vor_.,%str( ));

			%getVarLabel(intbl=&traindata.,invar=&_TmpVar_.);
			variablename="&_TmpVar_.";
			label="&_VLABEL_.";
			output;
		%end;
	run;

	data tmp_ctrl;
		set &ctrltbl.;
	run;

	proc sql noprint;
		create table &ctrltbl. as
			select * from tmp_ctrl
				where not (order=&steporder. and upcase(Variablename) in 
					(select variablename from _decitmp_));
	quit;


	proc append base=&ctrltbl. data=_decitmp_;
	quit;

	%let GStepOrder=%eval(&GStepOrder.+1);

	%cond_delete(intbl=_decitmp_ tmpapp tmp_ctrl);
%mend;

%macro EXCLUDE_VAR(
		vars=,
		reason=,
		ctrltbl=&ModelLibNm..&DecisionNm.,
		traindata=&ModelLibNm..&TrainNm.,
		steporder=999
		) / store des='1055.Exclude Variables';
	%DECISION_OVERRIDE(
		decision=EXCLUDE,
		vars=&vars,
		reason=&reason,
		ctrltbl=&ctrltbl.,
		traindata=&traindata.,
		steporder=&steporder.
		);
%mend;

%macro INCLUDE_VAR(
	vars=,
	reason=,
	ctrltbl=&ModelLibNm..&DecisionNm.,
	traindata=&ModelLibNm..&TrainNm.,
	steporder=998
	)/ store des='1056.Include Variables';

	%DECISION_OVERRIDE(
		decision=INCLUDE,
		vars=&vars,
		reason=&reason,
		ctrltbl=&ctrltbl.,
		traindata=&traindata.,
		steporder=&steporder.
	);
%mend;

%macro CLEAR_DECISION_OVERRIDE(vars=ALL,ctrltbl=&ModelLibNm..&DecisionNm.)
		/ store des='1057.Clear Decision Override';

	%let vars=%upcase(&vars.);

	%if &vars=ALL %then
		%do;

			data &ctrltbl;
				set &ctrltbl;
				where order<999;
			run;

		%end;
	%else %if &vars=EXCLUDE or &vars=INCLUDE %then
		%do;

			data &ctrltbl;
				set &ctrltbl;
				where not (order=999 and upcase(decision)="&vars.");
			run;

		%end;
	%else
		%do;
			%create_tbl_from_str(mstr=&vars.,outtbl=_varclrlist_,outvarnm=variablename,outvarfmt=$40.);

			proc sql noprint;
				create table &ctrltbl. as
					select * from &ctrltbl.
						where not (order=999 and upcase(Variablename) in 
							(select variablename from _varclrlist_));
			quit;

			%cond_delete(_varclrlist_);
		%end;
%mend;

%macro CLEAR_MANUAL_RULE(targetvarlist=ALL,	ruletbl=&ModelLibNm..&RuleNm.)
		/ store des='1058.Clear Manual Rule';

	/*			traindata=&rawdatalib..&TrainNm.,*/
	/*			validdata=&rawdatalib..&ValidNm.);*/
	%let dropvarlist=;
	%let MACRONULLVALUE=;

	%if %upcase(&targetvarlist)=ALL %then
		%do;

			proc sql noprint;
				select upcase(TARGETVAR) into:dropvarlist separated by ' ' 
					from &ruletbl.
						where INPUTMODE in ("MANUAL");
			quit;

		%end;
	%else
		%do;
			%let dropvarlist=%upcase(&targetvarlist);
		%end;

	%if &dropvarlist^=&macronullvalue. %then
		%do;
			%create_tbl_from_str(mstr=&dropvarlist.);

			proc sql noprint;
				create table &ruletbl. as
					select * from &ruletbl.
						where INPUTMODE not in ("MANUAL") or 
							upcase(TARGETVAR)  not in (select col from _valuelist_);
			quit;

			%cond_delete(intbl=_valuelist_);

			/*	data &traindata;*/
			/*		set &traindata;*/
			/*		drop &dropvarlist.;*/
			/*	run;*/
			/**/
			/*	data &validdata;*/
			/*		set &validdata;*/
			/*		drop &dropvarlist.;*/
			/*	run;*/
		%end;
%mend;

%macro CLEAR_MANUAL_BIN(targetvarlist=ALL,binstattbl=&ModelLibNm..&BinStatNm.)
		/ store des='1059.Clear Manual Bin';


	data &binstattbl;
		set &binstattbl;
		where order<90;
	run;

%mend;

%macro ModelScoring(indata=,outdata=,para=&ModelLibNm..&ScoreParaNm.,prior=)
		/ store des='1060.Model Scoring';

	%let prior=&prior.;

	proc logistic inmodel=&para.  plots=roc;
		score data=&indata. out=&outdata.

		%if %length(&prior.)>0 %then
			%do;
				priorevent=&prior.
			%end;
		;
	run;
%mend;

%macro KS_ROC_Cal(indata=,para=&ModelLibNm..&ScoreParaNm.,vary=&RespYnm.,prior=)
		/ store des='1061.KS ROC Calculation';

	%ModelScoring(indata=&indata.,outdata=_scored_,para=&para.,prior=&prior.);
	ods select KS2Stats;
	ods output KS2Stats=_KS_;

	proc npar1way data=_Scored_ edf;
		class &vary.;
		var p_1;
	run;

	ods graphics on;
	ods select Association ROCCurve;
	ods output Association=_ROC_;

	proc logistic data=_scored_ desc plots=roc;
		model &vary.=p_1;
	run;
	ods graphics off;

	%global _KS_ _ROC_;
	%let _KS_=;
	%let _ROC_=;

	proc sql noprint;
		select cValue2 into: _KS_ from _KS_
			where Label2='D';
		select nValue2 into: _ROC_ from _ROC_
			where Label2='c';
	quit;

	%let _KS_=&_KS_.;
	%let _ROC_=&_ROC_.;

	proc sgplot data=_scored_;
		vbox p_1 /category= &vary.;
	run;

	data _tmp_stat_;
		format statistics $10.;
		format value 8.2;
		statistics="KS";
		value=&_KS_.*100;
		output;
		statistics="ROC";
		value=&_ROC_.;
		output;
	run;

	proc print data=_tmp_stat_ noobs;
	run;

	%cond_delete(intbl=_scored_ _KS_ _ROC_ _tmp_stat_);
%mend;


%macro KS_ROC_Cal_Basic(indata=,vary=&RespYnm.,ScoreVar=)
		/ store des='1062.KS ROC Calculation Basic';
	ods select KS2Stats;
	ods output KS2Stats=_KS_;

	proc npar1way data=&indata. edf;
		class &vary.;
		var &ScoreVar.;
	run;

	ods select Association;
	ods output Association=_ROC_;

	proc logistic data=&indata. desc plots=roc;
		model &vary.=&ScoreVar.;
	run;

	%global _KS_ _ROC_;
	%let _KS_=;
	%let _ROC_=;

	proc sql noprint;
		select cValue2 into: _KS_ from _KS_
			where Label2='D';
		select nValue2 into: _ROC_ from _ROC_
			where Label2='c';
	quit;

	%let _KS_=&_KS_.;
	%let _ROC_=&_ROC_.;

	proc sgplot data=&indata.;
		vbox &ScoreVar. /category= &vary.;
	run;

	data _tmp_stat_;
		format statistics $10.;
		format value 8.2;
		statistics="KS";
		value=&_KS_.*100;
		output;
		statistics="ROC";
		value=&_ROC_.;
		output;
	run;

	proc print data=_tmp_stat_ noobs;
	run;

	%cond_delete(intbl=_scored_ _KS_ _ROC_ _tmp_stat_);
%mend;


%macro Model_Summary(outtbl=&ModelLibNm..&ModelResultNm.)
		/ store des='1063.Model Summary';
	%if %sysfunc(exist(&ModelLibNm..&TrainNm.)) %then
		%do;
			title 'Training Data';

			%KS_ROC_Cal(indata=&ModelLibNm..&TrainNm.);

			data Fit_Train;
				KS_Train=&_KS_.;
				GINI_Train=2*&_ROC_. - 1;
				ROC_Train=&_ROC_.;
			run;

			title;
		%end;

	%if %sysfunc(exist(&ModelLibNm..&ValidNm.)) %then
		%do;
			title 'Validation Data';

			%KS_ROC_Cal(indata=&ModelLibNm..&ValidNm.);

			data Fit_Valid;
				KS_Valid=&_KS_.;
				GINI_Valid=2*&_ROC_. - 1;
				ROC_Valid=&_ROC_.;
			run;

			title;
		%end;

	%if %sysfunc(exist(&ModelLibNm..&TestNm.)) %then
		%do;
			title 'Test Data';

			%KS_ROC_Cal(indata=&ModelLibNm..&TestNm.);

			data Fit_Test;
				KS_Test=&_KS_.;
				GINI_Test=2*&_ROC_. - 1;
				ROC_Test=&_ROC_.;
			run;

			title;
		%end;

	proc sql noprint;
		select count(*) into:nvarsel  from &ModelLibNm..&EstimateNm.
			where upcase(variablename) not in ('INTERCEPT');
		select label into:varsellab separated by ',' from &ModelLibNm..&EstimateNm.
			where upcase(variablename) not in ('INTERCEPT');
		select variablename into:varselnm separated by ',' from &ModelLibNm..&EstimateNm.
			where upcase(variablename) not in ('INTERCEPT');
	quit;


	data _varsel_;
		format NVARSEL 8.;
		format VARLABEL $1000.;
		format VARNM $1000.;
		NVARSEL=&nvarsel.;
		VARLABEL="%bquote(&varsellab.)";
		VARNM="%bquote(&varselnm.)";
	run;

	%cond_delete(intbl=&outtbl.);

	data &outtbl.;
		format Model $100.;
		format NVARSEL 8.;
		format VARNM $1000.;
		format VARLABEL $1000.;

		%if %sysfunc(exist(&ModelLibNm..&TrainNm.)) %then
			%do;format KS_Train percent8.1;	%end;
		%if %sysfunc(exist(&ModelLibNm..&ValidNm.)) %then
			%do;format KS_Valid percent8.1;%end;
		%if %sysfunc(exist(&ModelLibNm..&TestNm.)) %then
			%do;format KS_Test percent8.1;%end;

		%if %sysfunc(exist(&ModelLibNm..&TrainNm.)) %then
			%do;format Gini_Train percent8.1;%end;
		%if %sysfunc(exist(&ModelLibNm..&ValidNm.)) %then
			%do;format Gini_Valid percent8.1;%end;
		%if %sysfunc(exist(&ModelLibNm..&TestNm.)) %then
			%do;format Gini_Test percent8.1; %end;

		%if %sysfunc(exist(&ModelLibNm..&TrainNm.)) %then
			%do;format ROC_Train percent8.1;%end;
		%if %sysfunc(exist(&ModelLibNm..&ValidNm.)) %then
			%do;format ROC_Valid percent8.1;%end;
		%if %sysfunc(exist(&ModelLibNm..&TestNm.)) %then
			%do;format ROC_Test percent8.1;	%end;

		merge _varsel_ 
		%if %sysfunc(exist(&ModelLibNm..&TrainNm.)) %then

			%do;
				Fit_Train
			%end;

		%if %sysfunc(exist(&ModelLibNm..&ValidNm.)) %then
			%do;
				Fit_Valid
			%end;

		%if %sysfunc(exist(&ModelLibNm..&TestNm.)) %then
			%do;
				Fit_Test
			%end;
		;
		Model="%bquote(&ModelTitleNm.)";
	run;

	%cond_delete(intbl=Fit_Train Fit_Valid Fit_Test _varsel_);
%mend;

%macro get_orginal_estimate_var(outdata=)
		/ store des='1064.get orginal estimate var';

	proc sql noprint;
		create table &outdata. as
			select distinct variablename,label, TARGETVAR
				from &ModelLibNm..&RuleNm.
					where TARGETVAR in
						(select variablename from &ModelLibNm..&EstimateNm. 
							where variablename not in ('INTERCEPT'))
		;
	quit;

%mend;

%macro PlotEstWOE
		/ store des='1065.Plot Estimate WOE';
	%get_orginal_estimate_var(outdata=est_var);

	/*proc sql noprint;*/
	/*	create table gap as*/
	/*		select **/
	/*			from Est_Vars where variablename not in */
	/*				(select variablename from &rawdatalib..&BinStatNm.)*/
	/*	;*/
	/*quit;*/
	proc sql noprint;
		create table SELBINSTAT as
			select * from &ModelLibNm..&BinStatNm.
				where variablename in 
					(select variablename from Est_Var)
						order by iv desc,variablename,bin
		;
	quit;

	proc sql noprint;
		create table EstBinStat as
			select A.* from SELBINSTAT A
				where A.order =(select max(B.order) from SELBINSTAT B
				where B.variablename=A.variablename)
		;
	quit;

	proc sql noprint;
		create table EstIVStat as
			select distinct 
				IV_rank,variablename,label,IV,N_BIN
			from EstBinStat
		;
	quit;

	%PlotIV(bintbl=EstBinStat);
	%cond_delete(intbl=SELBINSTAT  est_var);
%mend;

%macro calscore(BaseScore=400,BaseOdds=20,PDO=20,
			EstimateTbl=&ModelLibNm..&EstimateNm.,
			BinstatTbl=&ModelLibNm..&BinStatNm.,
			OutTbl=,disableplot=Y,target_type = bad)
		/ store des='1066.Calculate Scorecard';

	proc sql noprint;
		select estimate into:intercept 
			from &EstimateTbl. 
				where variablename='INTERCEPT';
		select count(distinct Variablename) into:varn 
			from &EstimateTbl. 
				where variablename^='INTERCEPT';
	quit;

	%get_orginal_estimate_var(outdata=EstVar);

	proc sql noprint;
		create table _est_ as
			select A.*,
				B.variablename as org_varnm
			from  &EstimateTbl. A
				left join EstVar B
					on A.variablename=B.targetvar
		;
	quit;

	proc sql noprint;
		create table varbin as
			select A.* 
				from &BinstatTbl. A
					where A.variablename in 
						(select variablename from Estvar)
							and A.order = 
						(select max(order) from &BinstatTbl. B
							where A.variablename=B.variablename )
								order by variablename, order,bin
		;
	quit;

	proc sql;
		create table &OutTbl. as
			select 
				a.Variablename,
				a.Label,
				a.Binname,
				a.Woe,
				a.Bin,
				a.Biny_p,
				a.Biny_p/(1-a.Biny_p) as Odds format=percentn8.2,
				b.Estimate
			from  varbin a
				right join _est_(where = (VARIABLENAME^='INTERCEPT')) b
					on upcase(a.Variablename) = upcase(b.org_Varnm);
	quit;

	%if %lowcase(&target_type.) = good %then
		%do;

			data &OutTbl.;
				set &OutTbl.;
				Basescore1 = &BaseScore-(&PDO/log(2))*log(&BaseOdds)+(&PDO/log(2))*(&intercept);
				Varscore = (&PDO/log(2)) * Estimate;
				Binscore_1 = Varscore*Woe;
			run;

		%end;
	%else
		%do;

			data &OutTbl.;
				set &OutTbl.;
				Basescore1 = &BaseScore+(&PDO/log(2))*log(&BaseOdds)-(&PDO/log(2))*(&intercept);
				Varscore = -(&PDO/log(2)) * Estimate;
				Binscore_1 = Varscore*Woe;
			run;

		%end;

	proc sort data=&OutTbl.;
		by Variablename Binscore_1;
	run;

	data &OutTbl.;
		set &OutTbl.;
		retain Minbinscore;
		by Variablename;

		if first.Variablename then
			Minbinscore = Binscore_1;

		if first.Variablename then
			Minv = Binscore_1;
		else Minv=0;
		NewBinScore = Binscore_1 + abs(Minbinscore);
	run;

	proc sql noprint;
		select sum(Minv) into: summin from &OutTbl.;
	quit;

	data &OutTbl.;
		set &OutTbl.;
		addv = (Basescore1-abs(&summin))/&varn;
		Score = round(NewBinScore+abs(addv));
		keep Variablename label bin binname Score;
	run;

	proc sort data= &OutTbl.;
		by variablename bin;
	run;

	proc sql noprint;
		select sum(max), sum(min) into:maxscore, :minscore
			from (
				select variablename,max(Score) as Max,
					min(score) as min
				from scorecard
					group by variablename
						)
		;
	quit;

	%let disableplot=&disableplot.;

	%if %upcase(&disableplot.)^=Y %then
		%do;
			title "评分卡结果 - &ModelTitleNm. (&minscore - &maxscore.)";

			proc report data=&OutTbl. spanrows  nowd;
				column
					Variablename
					label
					binname
					score 
				;
				define Variablename/order;
				define label/order;
				label Variablename="变量" label="变量标签" binname = "变量分箱" score="评分";
			run;

			title;
		%end;

	%cond_delete(intbl=varbin EstVar _est_);
%mend;

%macro get_estimate_bin_stat
		/ store des='1067.get estimate bin stat';
	%get_orginal_estimate_var(outdata=est_var);

	proc sql noprint;
		create table SELBINSTAT as
			select * from &ModelLibNm..&BinStatNm.
				where variablename in 
					(select variablename from Est_Var)
						order by iv desc,variablename,bin
		;
	quit;

	proc sql noprint;
		create table EstBinStat as
			select A.*,
				A.BINY_P/(1-A.BINY_P) as Odds
			from SELBINSTAT A
				where A.order =(select max(B.order) from SELBINSTAT B
				where B.variablename=A.variablename)
		;
	quit;

	proc sql noprint;
		create table EstIVStat as
			select distinct 
				IV_rank,variablename,label,IV,N_BIN
			from EstBinStat
		;
	quit;

	%cond_delete(intbl=SELBINSTAT est_var);
%mend;

%macro scoring_scorecard(indata=&ModelLibNm..&TrainNm.,scoredataout=_Scoredata_)
		/ store des='1068.Scoring Data';

	%get_estimate_bin_stat;
	%calscore(outtbl=_scorecard_,disableplot=Y);

	proc sql noprint;
		create table scorebinstat as
			select B.score,
				A.*
			from ESTBINSTAT A
				left join _scorecard_ B
					on A.variablename=B.variablename and
					A.Binname=B.Binname
				order by A.variablename, A.bin
		;
	quit;

	%ScoreRuleCreate(inrule=scorebinstat,outrule=scorebinrule,createmode=AUTOMATIC);
	%runtblcode(codetbl=scorebinrule,codevar=RULE,codefilenm=inds);

	data &scoredataout.;
		set &indata.;

		%include inds;
		_score=sum(of _score_:);
		label _score="Score";
	run;

	%let droplist=ESTBINSTAT ESTIVSTAT scorebinrule _scorecard_ scorebinstat;

	%cond_delete(intbl=&droplist.);

	/*%scoringdata(indata=&rawdatalib..&TrainNm.,scoredataout=_Scoredata_);*/
%mend;

%macro manual_bin_recalibration
		/ store des='1069.Manual Bin Recalibration';

	%let AutoBinCalDisable=Y;
	%let BinPlotDisable=Y;

	%Prepare_master_data;

	%TrainValidSplit;
	%CLEAR_MANUAL_RULE;
	%CLEAR_MANUAL_BIN;
	%CLEAR_DECISION_OVERRIDE;
	%ManuBinSet;
	%BinStat_check_in(
		chkintbl=MANUAL_BIN_STAT,
		runorder=80
		);
	%WOERuleCreate(
		inrule=MANUAL_BIN_STAT,
		outrule=_ManualWOERule_,
		createmode=MANUAL);
	%decision_check_in(
		chkintbl=_ManualWOERule_,
		runorder=80
		);
	%rule_check_in(
		chkintbl=_ManualWOERule_,
		runorder=80
		);
	%DataRuleExec(indata=&ModelLibNm..&TrainNm.);
	%DataRuleExec(indata=&ModelLibNm..&ValidNm.);
	%Model_Fitting(
		ruleout=_model_result_,
		varsel=Y,
		step=AUTO);
	%decision_check_in(
		chkintbl=_model_result_,
		runorder=90
		);

	%Prepare_test_data;
	%Model_Summary;

	proc print data=&ModelLibNm..&ModelResultNm.;
	run;

%mend;

%macro stepwise_recalibration(adjstep=AUTO,outtbl=&ModelLibNm..&ModelResultNm.)
		/ store des='1070.Stepwise Recalibration';

	%Model_Fitting(
		ruleout=_model_result_,
		varsel=Y,
		step=&adjstep.);
	%decision_check_in(
		chkintbl=_model_result_,
		runorder=90
		);
	%Prepare_test_data;
	%Model_Summary(outtbl=&outtbl.);

	proc print data=&ModelLibNm..&ModelResultNm.;
	run;

%mend;

%macro loopsteps(stepstart=,stepend=,outtbl=stepsummary)
		/ store des='1071.Loop Stepwise Steps';
	%cond_delete(intbl=&outtbl.);

	%do stepno=&stepstart. %to &stepend.;
		%stepwise_recalibration(
			adjstep=&stepno.,
			outtbl=_model_
		);

		data _model_;
			format stepno 8.;
			set _model_;
			stepno=&stepno.;
		run;

		proc append base=&outtbl. data=_model_;
		quit;

		proc sql noprint;
			drop table _model_;
		quit;

	%end;
%mend;

%macro Model_Refitting(selvar=Y,selstep=AUTO)
		/ store des='1072.Model Refitting';

	%Prepare_master_data;
	%TrainValidSplit;
	%DataRuleExec(indata=&ModelLibNm..&TrainNm.);
	%DataRuleExec(indata=&ModelLibNm..&ValidNm.);
	%Model_Fitting(
		ruleout=_model_result_,
		varsel=&selvar.,
		step=&selstep.);
	%decision_check_in(
		chkintbl=_model_result_,
		runorder=90
		);
	%Prepare_test_data;
	%Model_Summary;
%mend;

%macro Model_Rollback
		/ store des='1073.Model Rollback';

	%let AutoBinCalDisable=N;
	%let BinPlotDisable=N;

	%Prepare_master_data;
	%TrainValidSplit;
	%CLEAR_MANUAL_RULE;
	%CLEAR_MANUAL_BIN;
	%CLEAR_DECISION_OVERRIDE;
	%DataRuleExec(indata=&ModelLibNm..&TrainNm.);
	%DataRuleExec(indata=&ModelLibNm..&ValidNm.);
	%Model_Fitting(
		ruleout=_model_result_,
		varsel=Y,
		step=AUTO);
	%decision_check_in(
		chkintbl=_model_result_,
		runorder=90
		);
	%Prepare_test_data;
	%Model_Summary;

	proc print data=&ModelLibNm..&ModelResultNm.;
	run;

%mend;



%macro PSI_Cal(datain1 = ,datain2 = ,varlist=,varlabellist=)
		/ store des='1074.PSI Calculation';

	%let varno = %sysfunc(countw(&varlist.));

	%do i = 1 %to &varno.;
		%let var = %scan(&varlist.,&i.,%str( ));
		%let varlabel = %scan(&varlabellist.,&i.,%str( ));

		proc freq data=&datain1. noprint;
		table &var./out=&var._dev(
				rename=(count = count_dev PERCENT=percent_dev));
		run;

		proc freq data=&datain2. noprint;
		table &var./out=&var._test(
				rename=(count = count_test PERCENT=percent_test));
		run;

		proc sql;
			create table &var. as
				select a.&var. as bin label = '分箱标签',
					a.count_dev label='开发样本频数',
					a.percent_dev label = '开发样本比例' format=8.2,
					b.count_test label = '验证样本频数',
					b.percent_test label = '验证样本比例' format = 8.2,
					(percent_dev-percent_test)/100*log(percent_dev/percent_test) as 
					psi label = '群体稳定性指数' format = 8.4
				from &var._dev a left join &var._test b
					on a.&var. = b.&var.;
		quit;

		proc sql;
			create table &var._sum as
				select sum(count_dev) as count_dev,
					sum(percent_dev) as percent_dev format = 8.2,
					sum(count_test) as count_test,
					sum(percent_test) as percent_test format = 8.2,
					sum(psi) as psi format = 8.4
				from &var.;
		quit;

		data &var._new;
			set &var. &var._sum;
			label obsn = '序号' variable='特征变量';
			obsn = put(_n_,8.);

			if bin = '' then
				obsn = '总计';
			variable = "&varlabel.";
		run;

		title "PSI分布稳定性检验：&varlabel.";

		proc report data=&var._new nowd;
			column
				variable
				obsn
				bin
				count_dev
				percent_dev
				count_test
				percent_test
				psi
			;
			define variable / order style(column)=Header;
		run;

		title;

		%cond_delete(intbl=&var._new &var._sum &var. &var._test &var._dev);
	%end;
%mend;

%macro PSI_Model_Cal(
			indata1=&ModelLibNm..&MasterNm.,
			indata2=&ModelLibNm..&TestNm.,
			scorelow=340,
			scorehigh=620,
			scoreinter=20
			)/ store des='1075.PSI Model Calculation';

	%let cutlist=;

	%do cutp=&scorelow %to &scorehigh %by &scoreinter;
		%let cutlist=&cutlist. &cutp.;
	%end;

	%put ************&cutlist.**********;

	%get_estimate_bin_stat;
	%BinRuleCreate(inrule=Estbinstat,outrule=estbinnedrule);

	%do tblloopno=1 %to 2;

		data _compdata&tblloopno._;
			set &&indata&tblloopno.;
		run;


		%DataRuleExec(	indata=_compdata&tblloopno._,ruledata=estbinnedrule);
		%scoring_scorecard(indata=_compdata&tblloopno._,scoredataout=_compdata&tblloopno._);

		data _compdata&tblloopno._;
			set _compdata&tblloopno._;
			keep _Binned: _score;
		run;


		%create_bin(indata=_compdata&tblloopno._,
			outdata=_compdata&tblloopno._,
			varnm=_score,
			cutpoint=&cutlist.,
			MissValueSymbol=MISS);
	%end;

	proc contents data=_Compdata1_ out=compmeta noprint;
	run;

	data compmeta;
		set compmeta;
		label=strip(scan(label,2,':'));
		keep name label;
		where label contains 'Binned';
	run;

	%let  varnmlist=;
	%let labnmlist=;

	proc sql noprint;
		select name,label into:varnmlist separated by ' ', 
			:labnmlist separated by ' '
		from compmeta;
	quit;

	%let varnmlist= _score_bin &varnmlist.;
	%let labnmlist= 模型评分区间 &labnmlist.;
	%put ********&varnmlist.*********;
	%put *******&labnmlist.**********;

	%Psi_cal(datain1 = _compdata1_ ,
		datain2 = _compdata2_,
		varlist=&varnmlist.,
		varlabellist = &labnmlist.);
	%let droptbllist=Estbinnedrule _compdata1_ _compdata2_ compmeta;

	%cond_delete(intbl=&droptbllist.);
%mend;

%macro VIF_Cal(indata=&ModelLibNm..&TrainNm.,estitbl=&ModelLibNm..&EstimateNm.,yvar=&RespYNm.)
		/ store des='1076.VIF Calculation';


	proc sql noprint;
		select variablename into: ModelVarList separated by ' '
			from &estitbl.
				where variablename ^= 'INTERCEPT';
	quit;

	ods select ParameterEstimates;
	ods output ParameterEstimates=_Vifstat_(keep = variable label varianceinflation);

	proc reg data=&indata.;
		model &yvar. = &ModelVarList./vif;
	run;

	proc print data=_Vifstat_;
		var variable label varianceinflation;
	run;

%mend;


%macro AdjProb(NP1=, NP0=, NS1= ,NS0=, ProbVar=, Adj_ProbVar=,intbl= ,outtbl = )
		/ store des='1077.Adjusted Probability Calculation';

	data &outtbl.;
		set &intbl.;
		&Adj_ProbVar. = ((&NP1./&NS1.)*&ProbVar.)/(((&NP1./&NS1.)*&ProbVar.)+((&NP0./&NS0.)*(1-&ProbVar.)));
	run;

%mend;

%macro Delete_Common_Data
		/ store des='1078-0.Delete Common Data';
	%cond_delete(intbl=&ModelLibNm..Decision&common_suffix.);
	%cond_delete(intbl=&ModelLibNm..Rule&common_suffix.);
%mend;
%macro Remove_NA_INF
		/ store des='1078-1.Remove NA & INF';
	array allchar _character_;
	do over allchar;
		if upcase(allchar)='NA' then
			allchar='';
		else if upcase(allchar)='INF' then
			allchar="&INF_Replacer.";
	end;
%mend;

%macro Prep_train_data
		/ store des='1078.Prepare Train Data';

	%let n_vars=0;

	data _null_;
		set &ModelLibNm..&DecisionNm.;
		call symputx('exvar_'||left(put(_n_,8.)),VariableName);
		call symputx('n_vars',left(put(_n_,8.)));
		where decision in ("EXCLUDE");
	run;

	%let RULE_DSN=%upcase(&RULE_DSN.);
	%if &RULE_DSN=MASTER %then 
			%let traintmp=&ModelLibNm..&MasterNm.;
	%else %let traintmp=&ModelLibNm..&TrainNm.;

	data _main_model_tmp_;
		set &traintmp.;
	run;

	%DataRuleExec(indata=_main_model_tmp_);

	%if &n_vars.>0 %then
	%do; 
		data _main_model_tmp_;
			set _main_model_tmp_;
			drop 
				%do v=1 %to &n_vars.;
						&&exvar_&v
				%end;
			; 
		run;
	%end;
%mend;

%macro Prepare_master_data
		/ store des='1079.Create Model Master';

	%if %upcase(&OverSampFlag.)=Y %then
		%do;
			%model_oversampling(
				indata=&ModelLibNm..&MasterRawNm.,
				outdata=&ModelLibNm..&MasterNm.
				);
		%end;
	%else
		%do;
			data &ModelLibNm..&MasterNm.;
				set &ModelLibNm..&MasterRawNm.;
			run;
		%end;

	data &ModelLibNm..&MasterNm.;
		set &ModelLibNm..&MasterNm.;
		%Remove_NA_INF;
	run;


	%let PreExcludeVarList=&PreExcludeVarList.;
		data _tmp_master_;
			set &ModelLibNm..&MasterNm.;
			%if %length(&PreExcludeVarList.)>0 %then
			%do; drop &PreExcludeVarList.; %end;
		run;
	%if &G_Common_Reset.=N 
		and %sysfunc(exist(&ModelLibNm..Decision&common_suffix.)) 
		and %sysfunc(exist(&ModelLibNm..Rule&common_suffix.)) 
	%then 
		%do;
			data &ModelLibNm..&DecisionNm.;
				set &ModelLibNm..Decision&common_suffix.;
			run;

			data &ModelLibNm..&RuleNm.;
				set &ModelLibNm..Rule&common_suffix.;
			run;
			
			%let JumpStepOrder=0;
			proc sql noprint;
				select max(order) into: JumpStepOrder
				from &ModelLibNm..&RuleNm.;
			quit;	
			%let GStepOrder=%eval(&JumpStepOrder.+1);

		%end;
	%else
		%do;
			/*******字符型变量特殊值处理*******/
			%let Char_SP_List=&Char_SP_List.;

			%if %length(&Char_SP_List.)>0 %then
			%do;
				%CharRecodeRuleCreate(
					DataRole=MASTER,
					intbl=_tmp_master_,
					VarList=_character_,
					SearchValueList=&Char_SP_List.,
					OutRule=_char_recode_rule,
					ReplaceValue=&CharSP_Rep.,
					Replaceflag=&CharSP_DV.
					);

				%rule_check_in(chkintbl=_char_recode_rule);

				%DataRuleExec(
					indata=_tmp_master_,
					ruledata=_char_recode_rule,
					outdata=_tmp_master_
					);

			%end;

			/*******字符型变量自动转数值型*******/

			%let GAutoChar2Num=%upcase(&GAutoChar2Num.);

			%if &GAutoChar2Num.=Y %then
			%do;

				%Char2NumRuleCreate(
					DataRole=MASTER,
					InTbl=_tmp_master_,
					VarList=_character_,
					OutRule=_autoconvertrule_,
					numfmt=%str(best.),
					dropcharvar=N
					);

				%decision_check_in(chkintbl=_autoconvertrule_);
				%rule_check_in(chkintbl=_autoconvertrule_);

				%DataRuleExec(
					indata=_tmp_master_,
					ruledata=_autoconvertrule_,
					outdata=_tmp_master_
					);
			%end;

			/*******数值型变量特殊值处理*******/

			%let Num_SP_List=&Num_SP_List.;

			%if %length(&Num_SP_List.)>0 %then
			%do;

				%NumRecodeRuleCreate(
					DataRole=MASTER,
					intbl=_tmp_master_,
					SearchValueList=&Num_SP_List.,
					OutRule=_num_recode_rule,
					Replaceflag=&NumSP_DV.
					);

				%rule_check_in(chkintbl=_num_recode_rule);

			%end;

			data &ModelLibNm..Decision&common_suffix.;
				set &ModelLibNm..&DecisionNm.;
			run;

			data &ModelLibNm..Rule&common_suffix.;
				set &ModelLibNm..&RuleNm.;
			run;

/*			%let G_Common_Reset=N;*/

		%end;
	%cond_delete(intbl=_tmp_master_ SPCHARVALUES _num_recode_rule _autoconvertrule_ _char_recode_rule);

%mend;

%macro Prepare_test_data
		/ store des='1080.create model test data';

	%let TestRawNm=&TestRawNm.;

	%if %length(&TestRawNm.)>0 %then
		%do;
			%if %sysfunc(exist(&ModelLibNm..&TestRawNm.)) %then
				%do;

					data &ModelLibNm..&TestNm.;
						set &ModelLibNm..&TestRawNm.;
						%Remove_NA_INF;
					run;

					%ModelRuleExec(indata=&ModelLibNm..&TestNm.);
				%end;
			%else %put Warning - Test data &ModelLibNm..&TestRawNm. does not exist;
		%end;
%mend;

%macro Create_Scoring(scoredata=,outdata=)
		/ store des='1081.create model scoring data';

	%let scoredata=&scoredata.;

	%if %length(&scoredata.)>0 %then
		%do;
			%if %sysfunc(exist(&scoredata.)) %then
				%do;
					%ModelRuleExec(indata=&scoredata.,outdata=&outdata.);
					%ModelScoring(indata=&outdata.,outdata=&outdata.);
				%end;
			%else %put Warning - Scoring data &scoredata. does not exist;
		%end;

%mend;


%macro Auto_Model_Run
		/ store des='1082.Auto Model Run';

	%let GStepOrder=1;
	%let GRuleOrder=1;
	%let GDecisionOrder=1;
	%set_model_parameters;
	%put *************Current Model: &ModelTitleNm. Suffix: &modelsuffix.*******;

	%Log_Output_Start(logfilenm=&LogFileNmFul.);
	%put *************Current Model: &ModelTitleNm. Suffix: &modelsuffix.*******;
	options mprint;

	%let RULE_DSN=%upcase(&RULE_DSN.);

	%Prepare_model_metadata;

	%let PreExcludeVarlist=&PreExcludeVarlist;
	%if %length(&PreExcludeVarlist.)>0 %then
	%do;
		%EXCLUDE_VAR(
			vars=&PreExcludeVarlist.,
			reason=人工预先排除ManuallyPreExclude,
			steporder=&GStepOrder.
			);
	
	%end;

	proc sql noprint;
		create table _FIXVAR_LIST_(compress=CHAR  bufsize=12288)
			(
		ORDER num format=8. label='运行顺序',
			DATAROLE char(50) format=$50. label='决策数据类型',
			VARIABLENAME char(32) label='变量名称',
			LABEL char(1000) format=$1000. label='变量描述',
			DECISION char(100) format=$100. label='入模决策',
			INPUTMODE char(50) format=$50. label='决策生成模式',
			REASON char(100) format=$100. label='决策原因'
			);
	quit;

	%let PreIncludeVarList=%upcase(&PreIncludeVarList.);

	%if %length(&PreIncludeVarList.)>0 %then 
	%do;
		%create_tbl_from_str(
			mstr=&PreIncludeVarList.,
			outtbl=_FIXVAR_LIST_,
			outvarnm=VARIABLENAME,
			outvarfmt=$32.);
	%end;


	%Prepare_master_data;

	%TrainValidSplit;

	%Reg_&methodNm._Mode;

	%Prepare_test_data;
	
	%Model_Summary;

	proc datasets nolist lib=work;
		delete _DOCTMP:;
	quit;

	%Log_Output_End;
	options nomprint;
%mend;



%macro rule_eval(intbl=,respvar=,rulevar=,outbl=,grpvar=)
/ store des='1083.Single Rule Evaluate'
;

%let grpvar=&grpvar.;

	proc sql noprint;
		create table &outbl. as
			select 
				"&rulevar." as RuleVarNm format=$32. length=32,
				%if %length(&grpvar.)>0 %then %do; &grpvar., %end;
				count(*) as N_App,
				sum(&respvar.=1) as N_bad,
				sum(&rulevar.=1) as N_Trigger,
				sum((&respvar.=1)*(&rulevar.=1)) as N_Hit,
				sum((&respvar.=1)*(1-(&rulevar.=1))) as N_Missed
			from &intbl.(keep=&respvar. &rulevar.)
				%if %length(&grpvar.)>0 %then %do; group by &grpvar. %end;
		;
	quit;

	data &outbl.;
		set &outbl.;
		format Tot_Bad_Rate percentn8.4;
		format Trigger_Rate percentn8.4;
		format Hit_Rate percentn8.4;
		format Bad_Cover_Rate percentn8.4;
		format Nontrigger_Bad_Rate percentn8.4;
		Tot_Bad_Rate=N_bad/N_App;
		Trigger_Rate=N_Trigger/N_App;

		if N_trigger>0 then
			Hit_Rate=N_Hit/N_Trigger;
		Bad_Cover_Rate=N_Hit/N_bad;

		if (N_App-N_Trigger)>0 then
			Nontrigger_Bad_Rate=N_Missed/(N_App-N_Trigger);

		if Hit_rate>0 then
			Rule_Index1=Trigger_Rate/Hit_Rate;

		if Bad_Cover_Rate>0 then
			Rule_Index2=Trigger_Rate/Bad_Cover_Rate;

	/*	if Rule_Index1>0 then log_Rule_Index1=log(Rule_Index1);*/
	/*	if Rule_Index2>0 then log_Rule_Index2=log(Rule_Index2);*/
	/*	if Rule_Index2>0 then sqrt_Rule_Index2=sqrt(Rule_Index2);*/
	run;

%mend;

%macro Assess_All_Rules(datatbl=,rulemetatbl=,respvar=,outbl=)
/ store des='1084.All Rule Evaluate'
;
	%cond_delete(intbl=&outbl.);
	%let N_rulevar=0;

	data _null_;
		set &rulemetatbl.;
		call symputx('rulevar_'||left(put(_n_,8.)),TARGETVAR);
		call symputx('N_rulevar',left(put(_n_,8.)));
	run;

	%if &N_rulevar.>0 %then
		%do;
			%do ruleid=1 %to &N_rulevar.;
				%put *******currently evaluate rule: &&rulevar_&ruleid, &ruleid. of &N_rulevar.*******;

				%rule_eval(
					intbl=&datatbl.,
					respvar=&respvar.,
					rulevar=&&rulevar_&ruleid ,
					outbl=eval_tmp);

				%if &ruleid.=1 %then
					%do;

						data &outbl.;
							set eval_tmp;
						run;

					%end;
				%else
					%do;

						proc append base=&outbl. data=eval_tmp force;
						quit;

					%end;

				%cond_delete(intbl=eval_tmp);
			%end;
		%end;
%mend;
