/*1��������ù��������򲡷���ʱ��*/
data data.compute_test1;set data.compute;
/*1��������ù����з���ʱ��,������ڼ��Ա�����������ڼ��ҩ���������ʱѪ�Ǽ��*/
 /*������ڼ��Ա�*/
    IF f1_c21=1 & f1_c21ay>0 & f1_c21am=.   then f1_c21am= 6;/*���1������ȱʧʱ��ʹ��6���*/
  	         f1_time_DM1=mdy(f1_c21am,15,f1_c21ay);/*����ʱ�����ն�ʹ��15����*/

    IF f2_c21=1 & f2_c21ay>0 & f2_c21am=.   then f2_c21am= 6;/*���2������ȱʧʱ��ʹ��6���*/
	         f2_time_DM1=mdy(f2_c21am,15,f2_c21ay);/*����ʱ�����ն�ʹ��15����*/
       
    time_DM_1=min(f1_time_DM1,f2_time_DM1);   /*�и�ֵ*/                 
    IF time_DM_1>time_b then time_DM1=time_DM_1;/*����ʱ���ڻ���ǰ�Ŀ���Ϊ���߷���*/
    IF .<time_DM_1<time_b then do; DM=1;end;/*�Ա���������ȱ����ʱ��Ļ�δ�*/
    format time_b f1_time_DM1 f2_time_DM1 time_DM_1 time_DM1 mmddyy10.;
    IF (f1_c21=1 | f2_c21=1) & time_DM1>time_b then f_DM1=1;
    IF (f1_c21=1 | f2_c21=1) & time_DM_1=. then f_DM1=2;

    LABEL f1_time_DM1='�Ա�DM����ʱ��(���1)' f2_time_DM1='�Ա�DM����ʱ��(���2)' 
    time_DM_1='�Ա�DM����ʱ��(������ʱ���ڻ���ǰ)' time_DM1='�Ա�DM����ʱ��(��������ʱ���ڻ���ǰ)' 
    f_DM1='��������Ա�DM����: 0=�Ա�δ���� 1=�Ա����� 2=�Ա�����ȱʧ����ʱ��';
run;
     

/*�����Ա�����ʱ��ȱʧ�ģ�ʹ���Ա��������ߵ�ƽ������ʱ��������ʱ��һ��ʼ����ȱʧֵ*/
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
    LABEL time_DM1 = '����(�ȱʧֵ��)�Ա�����ʱ��';
 run;

 /*9)�ų������ʱ���ڻ���ǰ�Ķ���*/
 data data.compute_test1;
    set data.compute_test1;
	if f_DM1=2 and time_DM1=<time_b then do; DM=1;f_DM1=.; time_DM=.; end;
	if f_DM1=2 and time_DM1>time_b then f_DM1=1;
	run;

   /*2.�޳����߻�������Ⱥ*/
 data data.compute_test1;
   set data.compute_test1;
   if DM = 1 then delete;/*2.�ų�����DM,��Ӧͳ������freq DM=1*/
   Run; 

      /*������ڼ��ҩ*/
   data data.compute_test1;
     set data.compute_test1;
	 if f1_c23=1 | f1_c25=1 | f2_c23=1 | f2_c25=1 then DM_med=1;
	 run;



/*�ڼ��(����DM�Ա�+�������) �Ա��Ƿ������ҩ-����*/
 data data.compute_test2;
     set data.compute_test1;
 if f1_glu < 20 | f1_glu > 500 then f1_glu=.;/*ȥ������ֵ*/
 IF 0 < f1_glu < 126 then f1_DM = 0; 
 IF f1_glu >= 126 then f1_DM = 1;
 if f2_glu < 20 | f2_glu > 500  then f2_glu=.;/*ȥ������ֵ*/
 IF 0 < f2_glu < 126 then  f2_DM = 0;         
 IF f2_glu >= 126 then f2_DM = 1;
 IF f1_DM = 1 | f2_DM = 1 then f_DM2=1; 
 LABEL f1_DM='�������(���1)' f2_DM='�������(���2)'   f_DM='���DM�·�(������úϲ�0=�� 1=��)'
       f_DM2='����������м�������' ;
 run;

 data data.compute_test2;
    set data.compute_test2;
  IF f_DM1=1 | f_DM2=1 | DM_med=1 then f_DM=1; else f_DM=0;

  IF (id_1=1 | id_1=3) & (f1_follow=1 & f2_follow=1) then do; /*����1,3��õ����ε����*/
    if (f1_c21=. | f1_c21=9) & (f2_c21=. | f2_c21=9)/*��������Ա�ȱʧ*/
       & (f1_glu=. & f2_glu=.)/*�������Ѫ��ȱ*/
       & (f1_c23=. & f1_c25=. & f2_c23=. & f2_c25=.)/*���������ҩȱ*/
	then f_DM=.;DM_relatedmiss=1; end;

  IF (id_1=1 | id_1=3) & (f1_follow=1 & f2_follow<=0) then do; /*����1,3��õ���һ�ε����*/
    if (f1_c21=. | f1_c21=9) /*��һ������Ա�ȱʧ*/
       & f1_glu=. /*��һ�����Ѫ��ȱ*/
       & (f1_c23=. & f1_c25=.)/*��һ�������ҩȱ*/
	then f_DM=.;DM_relatedmiss=1; end;

 IF (id_1=1 | id_1=3) & (f1_follow<=0 & f2_follow=1) then do; /*����1,3��õ��ڶ��ε����*/
    if (f2_c21=. | f2_c21=9) /*�ڶ�������Ա�ȱʧ*/
      & f2_glu=. /*�ڶ������Ѫ��ȱ*/
      & (f2_c23=. & f2_c25=.)/*�ڶ��������ҩȱ*/
	then f_DM=.;DM_relatedmiss=1; end;

 IF id_1=5 & f2_follow=1 then do; /*����5��õ������*/
    if (f2_c21=. | f2_c21=9) /*����Ա�ȱʧ*/
       & f2_glu=./*���Ѫ��ȱ*/
       & f2_c23=. & f2_c25=./*�����ҩȱ*/
	then f_DM=.;DM_relatedmiss=1; end;

  f1_time_DM2=mdy(f1_res_m,f1_res_d,f1_res_y);       
  f2_time_DM2=mdy(f2_res_m,f2_res_d,f2_res_y); 
  if f1_time_DM2=. then f1_time_DM2=time_f1;/*���ʱ��ȱʧ�������ʱ���滻*/
  if f2_time_DM2=. then f2_time_DM2=time_f2;
  if f1_DM = 1 then time_DM2=f1_time_DM2;
  if f1_DM ^= 1 & f2_DM = 1 then time_DM2=f2_time_DM2;		 
  IF f_DM=1 then time_DM=min(time_DM1,time_DM2);
  format time_b f1_time_DM2 f2_time_DM2 time_DM2 mmddyy10.;
 /* IF time_DM2>time_b then f_DM2=1; f_DM2 = '�������DM���(0=�� 1=��)'   */
 LABEL f1_time_DM2='���ʱ��(���1)' f2_time_DM2='���ʱ��(���2)' 
       time_DM2='���DM����ʱ��(������úϲ�)'  f_DM='���DM�·�(������úϲ�0=�� 1=��)'
       time_DM='DM����ʱ��(������úϲ�)' ;
	   run;
 

/*4.�����������*/
/*9)����Ƿ��������ʱ��*/
 data data.compute_test2;
    set data.compute_test2;
	if f_DM=0 & time_DM=. then time_DM=time_f;

/*12)���㷢������*/
    py_DM = yrdif(time_b,time_DM,'ACT/ACT');			
    format time_b time_DM time_f mmddyy10.;
 run;
