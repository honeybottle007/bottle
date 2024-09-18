/*1）计算随访过程中糖尿病发病时间*/
data data.compute_test1;set data.compute;
/*1）计算随访过程中发病时间,①随访期间自报（包含随访期间服药），②随访时血糖检测*/
 /*①随访期间自报*/
    IF f1_c21=1 & f1_c21ay>0 & f1_c21am=.   then f1_c21am= 6;/*随访1发病月缺失时，使用6替代*/
  	         f1_time_DM1=mdy(f1_c21am,15,f1_c21ay);/*计算时所有日都使用15代替*/

    IF f2_c21=1 & f2_c21ay>0 & f2_c21am=.   then f2_c21am= 6;/*随访2发病月缺失时，使用6替代*/
	         f2_time_DM1=mdy(f2_c21am,15,f2_c21ay);/*计算时所有日都使用15代替*/
       
    time_DM_1=min(f1_time_DM1,f2_time_DM1);   /*有负值*/                 
    IF time_DM_1>time_b then time_DM1=time_DM_1;/*发病时间在基线前的考虑为基线发病*/
    IF .<time_DM_1<time_b then do; DM=1;end;/*自报发病但是缺发病时间的还未填补*/
    format time_b f1_time_DM1 f2_time_DM1 time_DM_1 time_DM1 mmddyy10.;
    IF (f1_c21=1 | f2_c21=1) & time_DM1>time_b then f_DM1=1;
    IF (f1_c21=1 | f2_c21=1) & time_DM_1=. then f_DM1=2;

    LABEL f1_time_DM1='自报DM发病时间(随访1)' f2_time_DM1='自报DM发病时间(随访2)' 
    time_DM_1='自报DM发病时间(含发病时间在基线前)' time_DM1='自报DM发病时间(不含发病时间在基线前)' 
    f_DM1='两次随访自报DM发病: 0=自报未发病 1=自报发病 2=自报发病缺失发病时间';
run;
     

/*对于自报发病时间缺失的，使用自报发病患者的平均发病时间填补，检测时间一开始填上缺失值*/
    proc means;ods output summary=summary(keep=time_DM1_Mean);
            var time_DM1;
         run;
     data data.compute_test1;
     if _n_=1 then set summary;
     set data.compute_test1;
        run;
 data data.compute_test1(drop=time_DM1_Mean);
    set data.compute_test1;
	if f_DM1=2 and time_DM1=. then time_DM1=time_DM1_Mean;
    LABEL time_DM1 = '糖尿病(填补缺失值的)自报发病时间';
 run;

 /*9)排除填补发病时间在基线前的对象*/
 data data.compute_test1;
    set data.compute_test1;
	if f_DM1=2 and time_DM1=<time_b then do; DM=1;f_DM1=.; time_DM=.; end;
	if f_DM1=2 and time_DM1>time_b then f_DM1=1;
	run;

   /*2.剔除基线患病的人群*/
 data data.compute_test1;
   set data.compute_test1;
   if DM = 1 then delete;/*2.排除基线DM,还应统计人数freq DM=1*/
   Run; 

      /*②随访期间服药*/
   data data.compute_test1;
     set data.compute_test1;
	 if f1_c23=1 | f1_c25=1 | f2_c23=1 | f2_c25=1 then DM_med=1;
	 run;



/*②检测(基线DM自报+检测已排) 自报是否包括服药-包括*/
 data data.compute_test2;
     set data.compute_test1;
 if f1_glu < 20 | f1_glu > 500 then f1_glu=.;/*去除奇异值*/
 IF 0 < f1_glu < 126 then f1_DM = 0; 
 IF f1_glu >= 126 then f1_DM = 1;
 if f2_glu < 20 | f2_glu > 500  then f2_glu=.;/*去除奇异值*/
 IF 0 < f2_glu < 126 then  f2_DM = 0;         
 IF f2_glu >= 126 then f2_DM = 1;
 IF f1_DM = 1 | f2_DM = 1 then f_DM2=1; 
 LABEL f1_DM='检测糖尿病(随访1)' f2_DM='检测糖尿病(随访2)'   f_DM='随访DM新发(两次随访合并0=否 1=是)'
       f_DM2='两次随访中有检测出糖尿病' ;
 run;

 data data.compute_test2;
    set data.compute_test2;
  IF f_DM1=1 | f_DM2=1 | DM_med=1 then f_DM=1; else f_DM=0;

  IF (id_1=1 | id_1=3) & (f1_follow=1 & f2_follow=1) then do; /*队列1,3随访到两次的情况*/
    if (f1_c21=. | f1_c21=9) & (f2_c21=. | f2_c21=9)/*两次随访自报缺失*/
       & (f1_glu=. & f2_glu=.)/*两次随访血糖缺*/
       & (f1_c23=. & f1_c25=. & f2_c23=. & f2_c25=.)/*两次随访用药缺*/
	then f_DM=.;DM_relatedmiss=1; end;

  IF (id_1=1 | id_1=3) & (f1_follow=1 & f2_follow<=0) then do; /*队列1,3随访到第一次的情况*/
    if (f1_c21=. | f1_c21=9) /*第一次随访自报缺失*/
       & f1_glu=. /*第一次随访血糖缺*/
       & (f1_c23=. & f1_c25=.)/*第一次随访用药缺*/
	then f_DM=.;DM_relatedmiss=1; end;

 IF (id_1=1 | id_1=3) & (f1_follow<=0 & f2_follow=1) then do; /*队列1,3随访到第二次的情况*/
    if (f2_c21=. | f2_c21=9) /*第二次随访自报缺失*/
      & f2_glu=. /*第二次随访血糖缺*/
      & (f2_c23=. & f2_c25=.)/*第二次随访用药缺*/
	then f_DM=.;DM_relatedmiss=1; end;

 IF id_1=5 & f2_follow=1 then do; /*队列5随访到的情况*/
    if (f2_c21=. | f2_c21=9) /*随访自报缺失*/
       & f2_glu=./*随访血糖缺*/
       & f2_c23=. & f2_c25=./*随访用药缺*/
	then f_DM=.;DM_relatedmiss=1; end;

  f1_time_DM2=mdy(f1_res_m,f1_res_d,f1_res_y);       
  f2_time_DM2=mdy(f2_res_m,f2_res_d,f2_res_y); 
  if f1_time_DM2=. then f1_time_DM2=time_f1;/*检测时间缺失的用随访时间替换*/
  if f2_time_DM2=. then f2_time_DM2=time_f2;
  if f1_DM = 1 then time_DM2=f1_time_DM2;
  if f1_DM ^= 1 & f2_DM = 1 then time_DM2=f2_time_DM2;		 
  IF f_DM=1 then time_DM=min(time_DM1,time_DM2);
  format time_b f1_time_DM2 f2_time_DM2 time_DM2 mmddyy10.;
 /* IF time_DM2>time_b then f_DM2=1; f_DM2 = '两次随访DM检测(0=否 1=是)'   */
 LABEL f1_time_DM2='检测时间(随访1)' f2_time_DM2='检测时间(随访2)' 
       time_DM2='检测DM发病时间(两次随访合并)'  f_DM='随访DM新发(两次随访合并0=否 1=是)'
       time_DM='DM发病时间(两次随访合并)' ;
	   run;
 

/*4.发病人年计算*/
/*9)补充非发病对象的时间*/
 data data.compute_test2;
    set data.compute_test2;
	if f_DM=0 & time_DM=. then time_DM=time_f;

/*12)计算发病人年*/
    py_DM = yrdif(time_b,time_DM,'ACT/ACT');			
    format time_b time_DM time_f mmddyy10.;
 run;
