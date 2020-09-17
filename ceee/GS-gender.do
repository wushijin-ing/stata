********************************************************************************
*               The Impact of Teacher gender on Student academic: CEPS survey
*Author:        吴世瑾（Wu Shijin) wushijinceee@163.com 
*Created:       July.30.2020
*Last Modified: August.03 2020 
********************************************************************************
clear
set more off   
set scrollbufsize 2048000         
capture log close
global dtadir     "/Users/wushijin/Documents/论文/教师性别/数据/rawdta"
global workingdir "/Users/wushijin/Documents/论文/教师性别/数据/working"
global savedir    "/Users/wushijin/Documents/论文/教师性别/数据/save2"
cd "$workingdir"  

use "$dtadir/w1学生_主科目教师_校长.dta",clear

***************************
drop stu_city_h stu_county_h stu_school_h stu_grade_h stu_class_h stu_stuid_h stu_notes_h stu_schoolid_h stu_type stu_q0 stu_time1 stu_time2 stu_orign stu_detail stu_ip
drop stu_x stu_ccs stu_ipid stu_cs stu_ipid1 stu_pd stu_num stu_tag1 stu_tag5 q0_score time1_score time2_score orign_score detail_score ip_score q1_score q2_score q3_score type_score qq51_score ccs_score city_score county_score school_score
drop ipid_score cs_score ipid1_score pd_score schoolid_score grade_score class_score classid_score bc_score bcs_score name_score tag1_score
drop tea_grade tea_class tea_q0 tea_time1 tea_time2 tea_orign tea_detail tea_ip banzhuren english math yuwen com els tea_q1 tea_q2 city county school tea_q5_1 tea_q5_2 tea_q6_1 tea_q6_2 tea_q6_3 tea_q6_4
drop tea_q7* tea_q8* 
drop X a2 gradeid group sch_q0 sch_time1 sch_time2 sch_orign sch_detail sch_ip sch_q1 sch_q2 sch_city sch_county sch_school
drop tea_q9_1 tea_q9_8 tea_q9_9 tea_q9_10 tea_q10_1 tea_q10_5 tea_q10_6 tea_q10_8 tea_q10_9 tea_q10_10
order class subject ,after(stu_class)
order stu_q19 ,after(stu_q18)
order stu_q47 ,after(stu_q46)
egen countyid = group(stu_county)
order countyid, after(stu_county)
sort stu_number schoolid grade_id class_id countyid

/*
gen tea_home = 0
destring stu_grade stu_class tea_q9* tea_q10*,replace
replace tea_home = 1 if stu_grade==3 & stu_class==1 & tea_q9_2 ==1
replace tea_home = 1 if stu_grade==3 & stu_class==2 & tea_q9_3 ==1
replace tea_home = 1 if stu_grade==3 & stu_class==3 & tea_q9_4 ==1
replace tea_home = 1 if stu_grade==3 & stu_class==4 & tea_q9_5 ==1
replace tea_home = 1 if stu_grade==3 & stu_class==5 & tea_q9_6 ==1
replace tea_home = 1 if stu_grade==3 & stu_class==6 & tea_q9_7 ==1

replace tea_home = 1 if stu_grade==4 & stu_class==1 & tea_q10_2 ==1
replace tea_home = 1 if stu_grade==4 & stu_class==2 & tea_q10_3 ==1
replace tea_home = 1 if stu_grade==4 & stu_class==3 & tea_q10_4 ==1
replace tea_home = 1 if stu_grade==4 & stu_class==6 & tea_q10_7 ==1
codebook tea_home
*/  
//不准确，有老师报告错了



**************************************** step1.变量处理 *******************************************
*1.因变量：认知和非认知
   g w1std = . 
   levels schids, local(schids)
   foreach i of local schids {
   *foreach i of num 1/112 {
   egen w1stdmat7`i'= std(tr_mat) if schids == `i'    & tchsubject == 1 & grade9 == 0, mean(0) std(1)
   egen w1stdmat9`i'= std(tr_mat) if schids == `i'    & tchsubject == 1 & grade9 == 1, mean(0) std(1)
   replace w1std = w1stdmat7`i'   if w1stdmat7`i'!= . & tchsubject == 1 & grade9 == 0
   replace w1std = w1stdmat9`i'   if w1stdmat9`i'!= . & tchsubject == 1 & grade9 == 1
   drop w1stdmat7`i' w1stdmat9`i'
   
   egen w1stdchn7`i'= std(tr_chn) if schids == `i'    & tchsubject == 2 & grade9 == 0, mean(0) std(1)
   egen w1stdchn9`i'= std(tr_chn) if schids == `i'    & tchsubject == 2 & grade9 == 1, mean(0) std(1)
   replace w1std = w1stdchn7`i'   if w1stdchn7`i'!= . & tchsubject == 2 & grade9 == 0
   replace w1std = w1stdchn9`i'   if w1stdchn9`i'!= . & tchsubject == 2 & grade9 == 1
   drop w1stdchn7`i' w1stdchn9`i'
   
   egen w1stdeng7`i'= std(tr_eng) if schids == `i'    & tchsubject == 3 & grade9 == 0, mean(0) std(1)
   egen w1stdeng9`i'= std(tr_eng) if schids == `i'    & tchsubject == 3 & grade9 == 1, mean(0) std(1)
   replace w1std = w1stdeng7`i'   if w1stdeng7`i'!= . & tchsubject == 3 & grade9 == 0
   replace w1std = w1stdeng9`i'   if w1stdeng9`i'!= . & tchsubject == 3 & grade9 == 1
   drop w1stdeng7`i' w1stdeng9`i'
   }
   label var w1std "基线标准化成绩" 
   codebook w1std
   

*认知分数
   *codebook cog3pl
 
*非认知能力（构造大五人格） 

*1）思维开通性（开放性）Openness：想象力、尝新、审美、思辨、感情丰富、不断检验旧观念
   *tab1 bd1701 bd1702 bd1703 bd1704, m //1=非常不符合 4=非常符合
   
   factor bd1701 bd1702 bd1703 bd1704, pcf
   estat kmo // 0.78
   estat smc
   *screeplot
   predict openness
   alpha bd1701 bd1702 bd1703 bd1704 //0.82
   *codebook openness
   egen openness_std= std(openness), mean(0) std(1)
 
 
*2）宜人性 Agreeableness：信任、利他、直率、谦虚、温顺、慈悲
   *tab1 c1706 c1707 c1708 c1709 c1710,m //1=完全不同意 4=完全同意
   
   factor c1706 c1707 c1708 c1709 c1710, pcf
   estat kmo // 0.81
   estat smc
   *screeplot
   predict agreeableness
   alpha c1706 c1707 c1708 c1709 c1710 //0.81
   *codebook agreeableness
   egen agreeableness_std= std(agreeableness), mean(0) std(1)
   
*3）情绪稳定性 Neuroticism：焦虑、沮丧、冲动、脆弱、敏感害羞、生气敌意
   *tab1 a1801 a1802 a1803 a1804 a1805,m //1=从不 5=总是
   recode a18* (1=5)(2=4)(4=2)(5=1)
   label values a18*
   label define x111 1 "总是" 2 "经常" 3 "有时" 4 "很少" 5 "从不"
   label values a18* x111
   
   factor a1801 a1802 a1803 a1804 a1805, pcf
   estat kmo // 0.86
   estat smc
   *screeplot
   predict emotional_stability
   alpha a1801 a1802 a1803 a1804 a1805 //0.86
   *codebook emotional_stability
   egen emotional_stability_std= std(emotional_stability), mean(0) std(1)
*4）尽责性Conscientiousness：自信、自律、追求成就、有条理、不依赖、深思熟虑
   *tab1 c1701 c1702,m
   recode c1701 (1=4)(2=3)(3=2)(4=1)
   recode c1702 (1=4)(2=3)(3=2)(4=1)
   label values c1701 c1702
   label define x110 1 "完全不同意" 2 "比较不同意" 3 "比较同意" 4 "完全同意"
   label values c1701 c1702 x110
   label var c1701 "我不经常迟到"
   label var c1702 "我不经常逃课"
      
   factor c1701 c1702 , pcf
   estat kmo // 0.5
   estat smc
   *screeplot
   predict conscientiousness
   alpha c1701 c1702 //0.69
   *codebook conscientiousness
   egen conscientiousness_std= std(conscientiousness) , mean(0) std(1)
   
*5）外向性Extraversion：热情、乐群、支配、兴高采烈、忙忙碌碌、寻求刺激
   *tab1 b1701 b1702,m
   
   factor b1701 b1702, pcf
   estat kmo // 0.50
   estat smc
   *screeplot
   predict extraversion
   alpha b1701 b1702 // 0.71
   *codebook extraversion
   egen extraversion_std= std(extraversion) , mean(0) std(1)
*6) 非认知能力总分
   gen noncog = openness_std + agreeableness_std + emotional_stability_std + conscientiousness_std + extraversion_std

*2.自变量：性别

*女性教师
   gen fem_tea = tchb01 
   codebook fem_tea
   recode fem_tea (1=0) (2=1)
   label values fem_tea
   label var fem_tea "教师性别 0=男 1=女"
   label define x1 0 "男" 1 "女"
   label values fem_tea x1
   
*女性学生
   gen fem_stu = stsex
   recode fem_stu (0=1) (1=0)
   label var fem_stu "学科教师性别 0=男 1=女"
   label define x3 0 "男" 1 "女"
   label values fem_stu x3
   codebook fem_stu

*3.学生层面变量

*年龄
   gen     stu_age = a02a 
   tab stu_age,m
   replace stu_age = 2013 - stu_age if fall == 1
   replace stu_age = 2014 - stu_age if fall == 0
   label var stu_age "学生年龄"
   codebook stu_age
   
*少数民族
   gen stu_ethnic = a03
   recode stu_ethnic (1=0)(2=1)(3=1)(4=1)(5=1)(6=1)(7=1)(8=1)
   label var stu_ethnic "学生民族 0=汉族 1=少数民族"
   label define c 0 "汉族" 1 "少数民族"
   label values stu_ethnic c
   codebook stu_ethnic
   
*本县户口 sth_kplace
   codebook sthkplace
   clonevar stu_hkplace = sthkplace

*农业户口 sthktype 0=非农 1=农业
   codebook sthktype
   clonevar stu_hktype = sthktype

*独生子女 stonly 0=否 1=是
   codebook stonly
   gen stu_only = stonly
   recode stu_only (2=0)
   label var stu_only "是否独生子女? 0=否 1=是"
   label define e 0 "非独生子女" 1 "独生子女"
   label values stu_only e
   codebook stu_only
   
*上幼儿园 stu_kindergarten
   codebook c01
   gen stu_kindergarten = c01
   recode stu_kindergarten (2=0)
   label var stu_kindergarten "是否上过幼儿园? 0=否 1=是"
   label define e1 0 "否" 1 "是"
   label values stu_kindergarten e1
   codebook stu_kindergarten
   
*小学复读 stu_repeat
   tab c04,m
   gen stu_repeat = c04
   recode stu_repeat (9=1)(8=1)(7=1)(6=1)(5=1)(4=1)(3=1)(2=1)
   label var stu_repeat "是否小学留过级? 0=否 1=是"
   label define e2 0 "否" 1 "是"
   label values stu_repeat e2
   codebook stu_repeat
   
*小学跳级 stu_skip
   tab c03, m
   gen stu_skip = c03
   recode stu_skip (9=1)(8=1)(7=1)(6=1)(5=1)(4=1)(3=1)(2=1)
   label var stu_skip "是否小学跳级? 0=否 1=是"
   label define e3 0 "否" 1 "是"
   label values stu_skip e3
   codebook stu_skip 

*小学排名 stu_prerank
   tab c08,m
   clonevar stu_prerank = c08
   codebook stu_prerank 
   
*清楚的表述自己的意见
   tab a1204,m 
   clonevar stu_express = a1204
   
*反应能力很迅速
   tab a1205,m 
   clonevar stu_respond = a1205
   
*很快学会新知识
   tab a1206,m    
   clonevar stu_learn = a1206
   
*对新鲜事物好奇
   tab a1207,m
   clonevar stu_curious = a1207

*4.家庭层面变量 
   
*母亲受教育程度 stmedu & momedulow 母亲受教育年限是否低？（小于等于9年）
   *codebook stmedu
   *1没上过学：0年 
   *2小学：6年  
   *3初中：9年  
   *4中专：11年 
   *5职高：13年
   *6高中：12年  
   *7大专：15年
   *8本科：16年  
   *9研究生及以上：16年
   gen momeduyear = .
   replace momeduyear = 0 if stmedu == 1
   replace momeduyear = 6 if stmedu == 2
   replace momeduyear = 9 if stmedu == 3 
   replace momeduyear = 12 if stmedu == 4 | stmedu == 5 | stmedu == 6
   replace momeduyear = 15 if stmedu == 7
   replace momeduyear = 16 if stmedu == 8 | stmedu == 9
   *tab momeduyear,m
   label var momeduyear "母亲受教育年限"

   gen momedulow = .
   replace momedulow = 1 if momeduyear <= 9
   replace momedulow = 0 if momeduyear >  9 & momeduyear <= 16
   *codebook momedulow
   label var momedulow "母亲受教育年限是否低于9年? 0=否 1=是"
   label define ee 0 "大于9年" 1 "小于等于9年"
   label values momedulow ee

*父亲受教育程度 stfedu & dadedulow 父亲受教育年限是否低？（小于等于9年）
   *codebook stfedu
   gen dadeduyear = .
   replace dadeduyear = 0 if stfedu == 1
   replace dadeduyear = 6 if stfedu == 2
   replace dadeduyear = 9 if stfedu == 3 
   replace dadeduyear = 12 if stfedu == 4 | stfedu == 5 | stfedu == 6
   replace dadeduyear = 15 if stfedu == 7
   replace dadeduyear = 16 if stfedu == 8 | stfedu == 9
   *tab dadeduyear,m
   label var dadeduyear "父亲受教育年限"

   gen dadedulow = .
   replace dadedulow = 1 if dadeduyear <= 9
   replace dadedulow = 0 if dadeduyear >  9 & dadeduyear <= 16
   *codebook dadedulow
   label var dadedulow "父亲受教育年限是否低于9年? 0=否 1=是"
   label define eee 0 "大于9年" 1 "小于等于9年"
   label values dadedulow eee

*家长受教育程度（取高值）
   *codebook stprhedu
   gen parenteduyear = .
   replace parenteduyear = 0 if stprhedu == 1
   replace parenteduyear = 6 if stprhedu == 2
   replace parenteduyear = 9 if stprhedu == 3 
   replace parenteduyear = 12 if stprhedu == 4 | stprhedu == 5 | stprhedu == 6
   replace parenteduyear = 15 if stprhedu == 7
   replace parenteduyear = 16 if stprhedu == 8 | stprhedu == 9
   *tab parenteduyear,m
   label var parenteduyear "家长受教育年限"

   gen parentlowedu = .
   replace parentlowedu = 1 if parenteduyear <= 9
   replace parentlowedu = 0 if parenteduyear >  9 & parenteduyear <= 16
   *codebook parentlowedu
   label var parentlowedu "家长受教育年限是否低于9年? 0=否 1=是"
   label define eeee 0 "大于9年" 1 "小于等于9年"
   label values parentlowedu eeee
   
*学生流动 stmigrant 0=非流动 1=流动
   codebook stmigrant
   gen stu_migrant = stmigrant
   recode stu_migrant (1=0)(2=1)(3=1)
   label var stu_migrant "学生迁移流动状态 0=非流动 1=流动"
   label define cc 0 "非流动" 1 "流动"
   label values stu_migrant cc
   codebook stu_migrant

*学生留守 stu_leftbehind 0=父母都在家 1=只有一方在家或都不在家
   codebook stlb_2c
   clonevar stu_leftbehind = stlb_2c 
   codebook stu_leftbehind

*学生寄宿 stbrd 0=不住校 1=住校
   codebook stbrd
   clonevar stu_board = stbrd
   
*学生贫困 stu_poor 是否贫穷 0=否 1=是
   codebook steco_3c
   gen stu_poor = steco_3c
   recode stu_poor (3=0)(2=0)
   label var stu_poor "是否家庭贫困? 0=否 1=是"
   label define ccc 0 "非贫困" 1 "贫困"
   label values stu_poor ccc
   codebook stu_poor

*学生父亲经常喝醉酒
   codebook stfdrunk
   gen stu_fdrunk = stfdrunk
   recode stu_fdrunk (1=0) (2=1)
   label var stu_fdrunk "学生父亲是否经常喝醉酒? 0=否 1=是"
   label define ccc1 0 "否" 1 "是"
   label values stu_fdrunk ccc1
   codebook stu_fdrunk
   
*学生父母经常吵架
   codebook stprfight
   gen stu_prfight = stprfight
   recode stu_prfight (1=0) (2=1)
   label var stu_prfight "学生父母是否经常吵架? 0=否 1=是"
   label define ccc2 0 "否" 1 "是"
   label values stu_prfight ccc2
   codebook stu_prfight
   
*学生父母关系很好
   codebook stprrel
   gen stu_prrelation = stprrel
   recode stu_prrelation (1=0) (2=1)
   label var stu_prrelation "学生父母是否关系很好? 0=否 1=是"
   label define ccc3 0 "否" 1 "是"
   label values stu_prrelation ccc3
   codebook stu_prrelation
   
   

*5.教师层面变量  

*是否为班主任
tab tchhr
   codebook tchhr
   clonevar tea_home = tchhr
   label var tea_home "是否为班主任？ 0=否 1=是"
   label define z6 0 "否" 1 "是"
   label values tea_home z6
   codebook tea_home

   
*教师年龄：tea_age
   clonevar tea_age = tchb02 
   tab tea_age,m
   replace tea_age = . if tea_age == 0 | tea_age == 7212
   replace tea_age = 1968 if tea_age == 68
   replace tea_age = 2013 - tea_age if fall == 1
   replace tea_age = 2014 - tea_age if fall == 0
   label values tea_age
   label var tea_age "教师年龄"
   codebook tea_age
   
*教师婚姻状况
   codebook tchb03
   clonevar tea_marriage = tchb03
   
   
*是否本科及以上学历
   gen tea_edu = tchb04
   codebook tea_edu
   recode   tea_edu (2=0)(4=0)(5=0)(6=1)(7=1)
   label var tea_edu "是否为大学本科？0=否 1=是"
   label define x0 0 "正规本科以下" 1 "正规本科及以上"
   label values tea_edu x0
  
*是否师范类毕业 

   clonevar tea_nomal = tchb05 
   codebook tea_nomal
   recode   tea_nomal (2=0)
   label values tea_nomal
   label var tea_nomal "是否为师范类毕业？ 0=否 1=是"
   label define m 0 "否" 1 "是"
   label values tea_nomal m
 tab tea_nomal
 
*教龄
   clonevar tea_exp = tchb07 
   codebook tea_exp
   label var tea_exp "教龄"
   
   *expyear 基线教龄分段
   clonevar tea_expyear = tea_exp
   recode tea_expyear (0/1=0) (2/5=1) (6/10=2) (11/15=3) (16/20=4) (21/25=5) (26/50=6)
   label define bbb 0 "0-1years" 1 "2-5years" 2"6-10years" 3"11-15years" 4"16-20years" 5"21-25years" 6">25years"
   label values tea_expyear bbb
   tab tea_expyear,m
 
*教师职称
   clonevar tea_rank = tchb12
   codebook tea_rank 
   recode   tea_rank (1=0)(2=1)(3=2)(4=3)(5=4)
   label var tea_rank "教师职称 0=三级 1=二级 2=一级 3=高级 4=正高级"
   label define x11 0 "三级教师" 1 "二级教师" 2 "一级教师" 3 "高级教师" 4 "正高级教师"
   label values tea_rank x11
   
   
   
*是否获得教学奖励
   clonevar tea_award = tchb1606 
   codebook tea_award
   recode tea_award (0=1) (1=0)
   label values tea_award
   label var tea_award "是否获得教学奖励？ 0=否 1=是"
   label define x9 0 "没有获得过教学奖励" 1 "获得过教学奖励"
   label values tea_award x9
   
*是否获得教师资格证
   clonevar tea_cert = tchb06 
   codebook tea_cert
   recode   tea_cert (2=0)
   label var tea_cert "是否有教师资格证？ 0=否 1=是"
   label define zz 0 "没有教师资格证" 1 "有教师资格证"
   label values tea_cert zz
   

*过去三年内，是否获得优秀教师称号
   clonevar tea_goodtea = tchb17 
   codebook tea_goodtea
   recode   tea_goodtea (2=1) (3=0) (4=0)
   label values tea_goodtea
   label var tea_goodtea "三年内是否获得优秀教师称号？ 0=否 1=是"
   label define z4 0 "否" 1 "是"
   label values tea_goodtea z4

   
*行政职务：
   clonevar tea_duty = tchb13
   codebook tea_duty
   recode tea_duty (1=0)(2=0)(3=1)(4=1)(5=1)
   label var tea_duty "是否兼任行政职务？0=否 1=是"
   label define zz5 0 "没有兼任行政职务" 1 "兼任行政职务"
   label values tea_duty zz5  

   
*是否有事业编制
   clonevar tea_insystem = tchb11
   codebook tea_insystem
   recode tea_insystem (2=0)
   label var tea_insystem "是否有事业编制？0=否 1=是"
   label define zz6 0 "没有事业编制" 1 "有事业编制"
   label values tea_insystem zz6  
 
*6.班级层面变量

*班级目前在全年级的排名(三分类)
   codebook clsrank_3c
   clonevar cls_rank = clsrank_3c
   
*班级本县(区)户口学生比例 clshkplace
   codebook clshkplace
   clonevar cls_hkplace = clshkplace
   
*班级农业户口学生比例 clshktype
   codebook clshktype
   clonevar cls_hktype = clshktype
   
*7.学校层面变量

 
*新生分班是否随机 ple1503
   tab ple1503,m
   
   
*学校初中部在本县的排名中等以上
   codebook schrank_3c 
   clonevar sch_rank = schrank_3c
   recode sch_rank (1=0)(2 3=1)
   label var sch_rank "学校初中部是否在本县的排名中等以上？ 0=否 1=是"
   label define x55 0 "否" 1 "是"
   label values sch_rank x55
   codebook sch_rank
 
*学校规模
   tab1 plb0101b plb0102b plb0103b,m   
   list schids plb0101b plb0102b plb0103b if plb0101b ==. | plb0102b == . | plb0103b == .
   gen schobs = .
   replace schobs = plb0101b + plb0102b + plb0103b if plb0101b !=. & plb0102b !=. & plb0103b!=.
   tab schobs
   codebook schobs
   
   xtile sch_size = schobs, nquantiles(3)
   tab sch_size
   label var sch_size "学校规模 1=小 2=中 3=大"
   label define x99 1 "小" 2 "中" 3 "大"
   label values sch_size x99
   
   
*学校性质(二分类公立/民办) schtype_2c
   codebook schtype_2c
   clonevar sch_type = schtype_2c


   
 *block
   gen block=.
   levels schids, local(schids)
   foreach i of local schids {
   *foreach i of num 1/112 {
   gen block`i' = `i' if schids == `i' & grade9 == 0
   replace block`i' = `i'+112 if schids == `i' & grade9 == 1
   replace block = block`i' if block`i' != .
   drop block`i' 
   }
   
*9.机制分析变量

*教师行为
  *学科教师
   tab1 c1304 c1305 c1306,m
   gen     subtea_questioning = c1304 if tchsubject == 1
   replace subtea_questioning = c1305 if tchsubject == 2
   replace subtea_questioning = c1306 if tchsubject == 3
   codebook subtea_questioning
   
   tab1 c1307 c1308 c1309,m
   gen     subtea_praises = c1307 if tchsubject == 1
   replace subtea_praises = c1308 if tchsubject == 2
   replace subtea_praises = c1309 if tchsubject == 3
   codebook subtea_praises
   
   tab bb1102,m
   clonevar love_subtea = bb1102
   
   
  *班主任
   tab1 c1704 c1705,m
   clonevar hrtea_praises = c1704
   clonevar hrtea_criticisms = c1705
   codebook hrtea_praises hrtea_criticisms
   
   tab bb1101,m
   clonevar love_hrtea = bb1101

   
   
   
   *因变量
   global var cog3pl noncog openness_std agreeableness_std emotional_stability_std conscientiousness_std extraversion_std 
    
   *学生层面
   global stucon stu_age stu_ethnic stu_hkplace stu_hktype stu_only 
   //stu_kindergarten stu_prerank stu_repeat stu_skip stu_express stu_respond stu_learn stu_curious 

   *家庭层面
   global famcon stu_leftbehind stu_board stu_poor momeduyear dadeduyear 
   //stu_fdrunk stu_prfight 

   *教师层面
   global teacon tea_home tea_age tea_marriage tea_edu tea_nomal tea_expyear tea_rank tea_award tea_cert tea_goodtea tea_duty tea_insystem

   *班级层面
   global clscon cls_rank cls_hkplace cls_hktype

   *学校层面
   global schcon sch_rank sch_size sch_type

  
***************************************** step2.描述性分析 ***************************************
   
*变量描述 
  
   sum $var if ple1503 == 1
   asdoc sum $var if ple1503 == 1, save($savedir/变量描述) fs(7) dec(2) replace
   sum fem_tea if ple1503 == 1 & tchhr == 1
   asdoc sum fem_tea if ple1503 == 1 & tchhr == 1, save($savedir/变量描述) fs(7) dec(2) append
   sum fem_tea if ple1503 == 1 & tchhr == 0
   asdoc sum fem_tea if ple1503 == 1 & tchhr == 0, save($savedir/变量描述) fs(7) dec(2) append
   sum fem_stu if ple1503 == 1 
   asdoc sum fem_stu if ple1503 == 1 , save($savedir/变量描述) fs(7) dec(2) append
   sum $stucon $famcon if ple1503 == 1
   asdoc sum $stucon $famcon if ple1503 == 1, save($savedir/变量描述) fs(7) dec(2) append
  
  
  
  
*随机性检验   
   areg fem_tea $stucon $famcon if ple1503 == 1 , absorb(block) cluster(block) r
    est store i1
    test (stu_age=0)(stu_ethnic=0)(stu_hkplace =0)(stu_hktype =0) (stu_only=0) (stu_leftbehind=0) (stu_board=0) (stu_poor=0) (momeduyear=0) (dadeduyear =0)     
   areg fem_tea $stucon $famcon if ple1503 == 1 & tchhr == 1, absorb(block) cluster(block) r
    est store i2
    test (stu_age=0)(stu_ethnic=0)(stu_hkplace =0)(stu_hktype =0) (stu_only=0) (stu_leftbehind=0) (stu_board=0) (stu_poor=0) (momeduyear=0) (dadeduyear =0)     
   areg fem_tea $stucon $famcon if ple1503 == 1 & tchhr == 0, absorb(block) cluster(block) r
    est store i3
    test (stu_age=0)(stu_ethnic=0)(stu_hkplace =0)(stu_hktype =0) (stu_only=0) (stu_leftbehind=0) (stu_board=0) (stu_poor=0) (momeduyear=0) (dadeduyear =0)     
   outreg2 [i1 i2 i3] using "$savedir/随机性检验.xls", excel dec(3) replace
  

   
  
  
***************************************** step3.回归分析 *****************************************


   gen inter = fem_tea * fem_stu
   
   
    local replace replace
	foreach var of varlist $var {
	areg `var' inter fem_tea fem_stu                                 if ple1503 == 1 , absorb(block) cluster(block) r
	outreg2 using "$savedir/性别互动1.xls", excel dec(3) `replace'
	areg `var' inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 , absorb(block) cluster(block) r
	outreg2 using "$savedir/性别互动2.xls", excel dec(3) `replace'
	local replace
    }
   
    local replace replace
	foreach var of varlist $var {
	areg `var' inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1, absorb(block) cluster(block) r
	outreg2 using "$savedir/性别互动班主任.xls", excel dec(3) `replace'
	local replace
    }
   
    local replace replace
	foreach var of varlist $var {
	areg `var' inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 0, absorb(block) cluster(block) r
	outreg2 using "$savedir/性别互动学科教师.xls", excel dec(3) `replace'
	local replace
    }
	
	
***************************************** step4.稳健性检验 *****************************************
	
   areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1 & grade9 == 1, absorb(schids) cluster(schids) r
    est store i1
   areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1 & grade9 == 0, absorb(schids) cluster(schids) r
    est store i2
   areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1 & grade9 == 1, absorb(schids) cluster(schids) r
    est store i3
   areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1 & grade9 == 0, absorb(schids) cluster(schids) r
    est store i4
   areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 0 & grade9 == 1, absorb(schids) cluster(schids) r
    est store i5
   areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 0 & grade9 == 0, absorb(schids) cluster(schids) r
    est store i6
   areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 0 & grade9 == 1, absorb(schids) cluster(schids) r
    est store i7
   areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 0 & grade9 == 0, absorb(schids) cluster(schids) r
    est store i8
	outreg2 [i1 i2 i3 i4 i5 i6 i7 i8] using "$savedir/稳健性检验.xls", excel dec(3) replace
   
   
   
    local replace replace
	foreach v of varlist cog3pl noncog {
	areg `v' inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1, absorb(clsids) cluster(clsids) r
	outreg2 using "$savedir/稳健性检验班级班主任.xls", excel dec(3) `replace'
	areg `v' inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 0, absorb(clsids) cluster(clsids) r
	outreg2 using "$savedir/稳健性检验班级学科.xls", excel dec(3) `replace'
	local replace
    }
   
   
   
   
***************************************** step5.机制分析 *****************************************
   
*机制:教师与学生之间的性别互动	
   areg hrtea_criticisms   inter fem_tea fem_stu $stucon $famcon $teacon $clscon              if ple1503 == 1 & tchhr == 1, absorb(block) cluster(block) r
    est store i1
   areg hrtea_praises      inter fem_tea fem_stu $stucon $famcon $teacon $clscon              if ple1503 == 1 & tchhr == 1, absorb(block) cluster(block) r
    est store i2
   areg subtea_questioning inter fem_tea fem_stu $stucon $famcon $teacon $clscon i.tchsubject if ple1503 == 1 & tchhr == 0, absorb(block) cluster(block) r
    est store i3
   areg subtea_praises     inter fem_tea fem_stu $stucon $famcon $teacon $clscon i.tchsubject if ple1503 == 1 & tchhr == 0, absorb(block) cluster(block) r
    est store i4
	
   areg love_hrtea         inter fem_tea fem_stu $stucon $famcon $teacon $clscon              if ple1503 == 1 & tchhr == 1, absorb(block) cluster(block) r
    est store i5
   areg love_subtea        inter fem_tea fem_stu $stucon $famcon $teacon $clscon              if ple1503 == 1 & tchhr == 0, absorb(block) cluster(block) r
    est store i6
	outreg2 [i1 i2 i3 i4 i5 i6] using "$savedir/性别互动机制1.xls", excel dec(3) replace

	codebook hrtea_criticisms hrtea_praises subtea_questioning subtea_praises love_hrtea love_subtea

	
	
	
	

//cog3pl ：subtea_questioning 
areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon i.tchsubject if ple1503 == 1 & tchhr == 0,absorb(block) cluster(block) r
sca coef_main=_b[inter]
areg subtea_questioning inter fem_tea fem_stu $stucon $famcon $teacon $clscon i.tchsubject if ple1503 == 1 & tchhr == 0,absorb(block) cluster(block) r
sca coef_interquestion=_b[inter]
areg cog3pl subtea_questioning inter fem_tea fem_stu $stucon $famcon $teacon $clscon i.tchsubject if ple1503 == 1 & tchhr == 0,absorb(block) cluster(block) r
sca coef_question=_b[subtea_questioning]
sca list coef_main coef_interquestion coef_question
gen power_question=coef_interquestion*coef_question/coef_main
codebook power_question //7.24%

//cog3pl ：subtea_praises
areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon i.tchsubject if ple1503 == 1 & tchhr == 0,absorb(block) cluster(block) r
sca coef_main=_b[inter]
areg subtea_praises inter fem_tea fem_stu $stucon $famcon $teacon $clscon i.tchsubject if ple1503 == 1 & tchhr == 0,absorb(block) cluster(block) r
sca coef_interpraises=_b[inter]
areg cog3pl subtea_praises inter fem_tea fem_stu $stucon $famcon $teacon $clscon i.tchsubject if ple1503 == 1 & tchhr == 0,absorb(block) cluster(block) r
sca coef_praises=_b[subtea_praises]
sca list coef_main coef_interpraises coef_praises
gen power_praises=coef_interpraises*coef_praises/coef_main
codebook power_praises //7.65%

//noncog ：hrtea_criticisms  
areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1,absorb(block) cluster(block) r
sca coef_main=_b[inter]
areg hrtea_criticisms inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1,absorb(block) cluster(block) r
sca coef_intercriticisms=_b[inter]
areg noncog hrtea_criticisms inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1,absorb(block) cluster(block) r
sca coef_criticisms=_b[hrtea_criticisms]
sca list coef_main coef_intercriticisms coef_criticisms
gen power_criticisms=coef_intercriticisms*coef_criticisms/coef_main
codebook power_criticisms // 14.73%

//noncog ：love_hrtea
areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1,absorb(block) cluster(block) r
sca coef_main=_b[inter]
areg love_hrtea inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1,absorb(block) cluster(block) r
sca coef_interlove=_b[inter]
areg noncog love_hrtea inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & tchhr == 1,absorb(block) cluster(block) r
sca coef_love=_b[love_hrtea]
sca list coef_main coef_interlove coef_love
gen power_love=coef_interlove*coef_love/coef_main
codebook power_love // 32.76%

	
***************************************** step6.异质性分析 *****************************************
tab1 sch_rank sch_size sch_type

	areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_rank == 1, absorb(block) cluster(block) r
	areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_rank == 0, absorb(block) cluster(block) r
	areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_rank == 1, absorb(block) cluster(block) r
	areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_rank == 0, absorb(block) cluster(block) r

	
	areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_size == 2, absorb(block) cluster(block) r
	areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_size == 1, absorb(block) cluster(block) r
	areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_size == 2, absorb(block) cluster(block) r
	areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_size == 1, absorb(block) cluster(block) r

	
	areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_type == 1, absorb(block) cluster(block) r
	areg cog3pl inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_type == 0, absorb(block) cluster(block) r
	areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_type == 1, absorb(block) cluster(block) r
	areg noncog inter fem_tea fem_stu $stucon $famcon $teacon $clscon if ple1503 == 1 & sch_type == 0, absorb(block) cluster(block) r

