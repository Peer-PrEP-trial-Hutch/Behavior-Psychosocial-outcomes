# set-up ----
rm(list=ls())
library(pacman)

dataws=getwd()


p_load(writexl, readxl, janitor, openxlsx,
       tidyverse, dplyr, scales, 
       likert, labelled,ggpubr, grid,
       compareGroups, labelled,
       
       sandwich,lmtest,
       
       lme4, lqmm,quantreg,
       
       sjstats,
       
       gee,geepack, broom,
       
       car,
       
       Amelia, mice,
       
       jtools
)

ds_pbl=readxl::read_excel(file.path(dataws,paste0("datasets/1. df_pbl_behav.xlsx")))
ds_pfu=readxl::read_excel(file.path(dataws,paste0("datasets/2. df_pfu_behav.xlsx")))
ds_cfu=readxl::read_excel(file.path(dataws,paste0("datasets/3. ds_cfu_behav.xlsx")))


# Table 1 ----
## index peers ----


labels_df_total=c(
  # Demographics
  p1_s0_arm="Study arm",
  p1_s1_q1="Age*",
  p1_s1_q4="Marital status",
  p1_s1_q6="Relationship status",
  p1_s1_q2="Years of school*",
  p1_s1_q3="Currently in school",
  p1_s1_q7="Monthly income*",
  
  # Sexual and reproducttive behaviors
  p1_s2_q17="Age of first sex",
  p1_s2_q7="No. of sexual partners*",
  new_sexpart_dico="New sexual partner (past 3 months)",
  p1_s2_q18="Ever being pregnant",
  p1_s2_q12="Emergency contraception (6 months)",
  p1_s2_q13="Emergency contraception twice (past 6 months)",
  
  # Behaviors associated witth HIV
  p1_s2_q10="Unprotected sex with partner of unknown/positive HIV status (past 6 months)",
  p1_s2_q11="STI (past 6 months)",
  p1_s2_q14="PEP use more than twice (past 6 months)",
  p1_s2_q15="Sex under influence of drugs (past 6 months)",
  p1_s2_q16="Sex exchange (past 6 months)",
  
  
  # Prior PrEP use
  p1_s5_q1_dico="Prior PrEP use"
  
  
)

vars=c(
  "p1_s0_arm",
  "p1_s1_q1",
  "p1_s1_q4",
  "p1_s1_q6",
  "p1_s1_q2",
  "p1_s1_q3",
  "p1_s1_q7",
  "p1_s2_q17",
  "p1_s2_q7",
  "new_sexpart_dico",
  "p1_s2_q18",
  "p1_s2_q12",
  "p1_s2_q13",
  "p1_s2_q10",
  "p1_s2_q11",
  "p1_s2_q14",
  "p1_s2_q15",
  "p1_s2_q16",
  "p1_s5_q1_dico"
)


vars_c=paste(vars,collapse = "+")
vars_f=as.formula(paste("p1_s0_arm~",vars_c))

df_table_total_in_f=ds_pbl[,vars]

#ds_pbl %>% dplyr::select(all_of(vars))
df_table_total_in=set_variable_labels(df_table_total_in_f, 
                                      .labels = labels_df_total)
#label(df_table_total_in)

#compareGroups(year ~ age + triglyc, data=regicor, method = c(triglyc=2), Q1=0, Q3=1)

df_table_total_out=compareGroups(vars_f, data = df_table_total_in,
                                 method=c(p1_s1_q1=2,p1_s1_q2=2,p1_s2_q7=2,p1_s1_q7=2,p1_s2_q6=2,prep_first=2,
                                          age_gap=2,p1_s2_q18a=2,p1_s2_q8=2,p1_s2_q17=2), simplify = T) %>% 
  createTable(digits = 0,
              show.all = TRUE,
              show.n=T)






## referred peers ----


labels_df_total_cfu=c(
  # Demographics
  c2_s0_arm= "Study arm",
  c2_s1_q1 = "Age*",
  c2_s1_q4 = "Marital status",
  c2_s1_q5 = "Relationship status",
  c2_s1_q2 = "Years in school*",
  c2_s1_q3 = "Currently in school",
  c2_s1_q6 = "Monthly income",
  c2_s1_q7_1 = "Enrolled in another HIV study",
  
  # Sexual and reproductive behaviors
  c2_s2_q17 = "Age of first sex*",
  c2_s2_q6 = "No. of sexual partners*",
  new_sexpart_dico = "New sexual partner (past 3 months)",
  c2_s2_q18 = "Ever being pregnant",
  c2_s2_q12 = "Prior emergency contraception use (past 6 months)",
  c2_s2_q13 = "Emergency contraception twice (past 6 months)",
  
  # Behaviors associated with HIV
  c2_s2_q10 = "Unprotected sex with partner of unknown/positive HIV status (past 6 months)",
  c2_s2_q11 = "STI (past 6 months)",
  c2_s2_q14 = "PEP use more than twice (past 6 months)",
  c2_s2_q15 = "Sex under influence of drugs (past 6 months)",
  c2_s2_q16 = "Sex exchange (past 6 months)",
  
  # Prior PrEP use
  c2_s6_q1_1_rec = "Prior PrEP use",
  c2_s6_q1_1_1 = "Currently using PrEP for HIV prevention"
)



vars=c(
  "c2_s0_arm",
  "c2_s1_q1",
  "c2_s1_q4",
  "c2_s1_q5",
  "c2_s1_q2",
  "c2_s1_q3",
  "c2_s1_q6",
  "c2_s1_q7_1",
  "c2_s2_q17",
  "c2_s2_q6",
  "new_sexpart_dico",
  "c2_s2_q18",
  "c2_s2_q12",
  "c2_s2_q13",
  "c2_s2_q10",
  "c2_s2_q11",
  "c2_s2_q14",
  "c2_s2_q15",
  "c2_s2_q16",
  "c2_s6_q1_1_rec",
  "c2_s6_q1_1_1"
       
)

vars_c=paste(vars,collapse = "+")
vars_c_f=as.formula(paste("~",vars_c))
vars_f=as.formula(paste("c2_s0_arm~",vars_c))


df_table_total_in_cfu=ds_cfu %>% dplyr::select(all_of(vars))
df_table_total_in_cfu=set_variable_labels(df_table_total_in_cfu, 
                                          .labels = labels_df_total_cfu)



df_table_total_cfu_out=compareGroups(vars_f, data = df_table_total_in_cfu,
                                     method = c(c2_s1_q1=2, c2_s2_q6=2, c2_s1_q2=2, age_gap=2,c2_s1_q6=2,
                                                c2_s2_q18a=2,c2_s2_q17=2,prep_first=2,c2_s2_q7=2),simplify=T) %>% 
  createTable(show.n = T,
              show.all = T,
              digits=0)





# Table 2 ----
## index peers ----

labels_df_total_fu=c(p2_s0_arm="Study arm",
                     p2_s2_q7="Condomless sex with partner of unknown or positive HIV status",
                     p2_s2_q9="Emergency contraception",
                     p2_s2_q11="PEP uses >2 times",
                     p2_s2_q12="Engaged in sex with drugs or alcohol",
                     p2_s2_q13="Engaged in transactional sex",
                     p2_s2_q3a_1="Condomless sex")



vars=c("p2_s0_arm",
       

       "p2_s2_q7","p2_s2_q9","p2_s2_q11","p2_s2_q12","p2_s2_q13",
       
       "p2_s2_q3a_1"

)


vars_c=paste(vars,collapse = "+")
vars_c_f=as.formula(paste("~",vars_c))
vars_f=as.formula(paste("p2_s0_arm~",vars_c))


df_table_total_in_fu=ds_pfu %>% dplyr::select(all_of(vars))
df_table_total_in_fu=set_variable_labels(df_table_total_in_fu, 
                                         .labels = labels_df_total_fu)


df_table_total_pfu_out_diss_exp=compareGroups(vars_f, data = df_table_total_in_fu,
                                              method=c(p2_s5_q3=2), simplify=TRUE) %>% createTable(show.n=T)



addmargins(table(ds_pfu$mspss_score_bin,ds_pfu$p2_s0_arm, useNA = 'always'))
round(prop.table(table(ds_pfu$mspss_score_bin,ds_pfu$p2_s0_arm, useNA = 'always'), margin=2)*100,0)


addmargins(table(ds_pfu$phq2_score_bin,ds_pfu$p2_s0_arm, useNA = 'always'))
round(prop.table(table(ds_pfu$phq2_score_bin,ds_pfu$p2_s0_arm, useNA = 'always'), margin = 2)*100,0)


## refered peers ----

addmargins(table(ds_cfu$incons_condom,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$incons_condom,ds_cfu$c2_s0_arm), margin = 2)*100,0)


addmargins(table(ds_cfu$phq2_score_bin,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$phq2_score_bin,ds_cfu$c2_s0_arm), margin = 2)*100,0)


ds_cfu$mspss_score_bin=as.factor(ifelse(ds_pfu$mspss_score<=35, "Low perceived support",
                                        ifelse(ds_pfu$mspss_score>35, "Medium/High perceived support",NA)))
ds_cfu$mspss_score_bin_num=as.numeric(ds_pfu$mspss_score_bin)-1



addmargins(table(ds_cfu$mspss_score_bin,ds_cfu$c2_s0_arm))
round(prop.table(table(ds_cfu$mspss_score_bin,ds_cfu$c2_s0_arm), margin = 2)*100,0)


# Figure 2 ----
## index peers ----

glmm_clessunk_m_glmm=glm(p2_s2_q7_num ~ p2_s0_arm_num, family = gaussian(link="identity"), data=ds_pfu)
glmm_clessunk=summary(glmm_clessunk_m_glmm)$coef
df_client_rprovid_clessunk_glmm=cbind(cbind(glmm_clessunk[2,1],
                                         t(confint(glmm_clessunk_m_glmm)[2,])),
                                   Anova(glmm_clessunk_m_glmm)[3])
df_client_rprovid_clessunk_glmm=as.data.frame(df_client_rprovid_clessunk_glmm)
rownames(df_client_rprovid_clessunk_glmm)=c("Condomless sex with partner of unknown or positive HIV status")

glmm_emcont_m_glmm=glm(p2_s2_q9_num ~ p2_s0_arm_num, family = gaussian(link="identity"), data=ds_pfu)
glmm_emcont=summary(glmm_emcont_m_glmm)$coef
df_client_rprovid_emcont_glmm=cbind(cbind(glmm_emcont[2,1],
                                          t(confint(glmm_emcont_m_glmm)[2,])),
                                    Anova(glmm_emcont_m_glmm)[3])
df_client_rprovid_emcont_glmm=as.data.frame(df_client_rprovid_emcont_glmm)
rownames(df_client_rprovid_emcont_glmm)=c("Emergency contraception")


glmm_sda_m_glmm=glm(p2_s2_q12_num ~ p2_s0_arm_num, family = gaussian(link="identity"), data=ds_pfu)
glmm_sda=summary(glmm_sda_m_glmm)$coef
df_client_rprovid_sda_glmm=cbind(cbind(glmm_sda[2,1],
                                       t(confint(glmm_sda_m_glmm)[2,])),
                                 Anova(glmm_sda_m_glmm)[3])
df_client_rprovid_sda_glmm=as.data.frame(df_client_rprovid_sda_glmm)
rownames(df_client_rprovid_sda_glmm)=c("Engaged in sex with drugs or alcohol")


glmm_sti_m_glmm=glm(p2_s2_q8_num ~ p2_s0_arm_num, family = gaussian(link="identity"), data=ds_pfu)
glmm_sti=summary(glmm_sti_m_glmm)$coef
df_client_rprovid_sti_glmm=cbind(cbind(glmm_sti[2,1],
                                       t(confint(glmm_sti_m_glmm)[2,])),
                                 Anova(glmm_sti_m_glmm)[3])
df_client_rprovid_sti_glmm=as.data.frame(df_client_rprovid_sti_glmm)
rownames(df_client_rprovid_sti_glmm)=c("STI diagnosis or treatment")


glmm_tsex_m_glmm=glm(p2_s2_q13_num ~ p2_s0_arm_num, family = gaussian(link="identity"), data=ds_pfu)
glmm_tsex=summary(glmm_tsex_m_glmm)$coef
df_client_rprovid_tsex_glmm=cbind(cbind(glmm_tsex[2,1],
                                        t(confint(glmm_tsex_m_glmm)[2,])),
                                  Anova(glmm_tsex_m_glmm)[3])
df_client_rprovid_tsex_glmm=as.data.frame(df_client_rprovid_tsex_glmm)
rownames(df_client_rprovid_tsex_glmm)=c("Engaged in transactional sex")


glmm_cless_m_glmm=glm(p2_s2_q3a_1_num ~ p2_s0_arm_num, family = gaussian(link="identity"), data=ds_pfu)
glmm_cless=summary(glmm_cless_m_glmm)$coef
df_client_rprovid_cless_glmm=cbind(cbind(glmm_cless[2,1],
                                         t(confint(glmm_cless_m_glmm)[2,])),
                                   Anova(glmm_cless_m_glmm)[3])
df_client_rprovid_cless_glmm=as.data.frame(df_client_rprovid_cless_glmm)
rownames(df_client_rprovid_cless_glmm)=c("Condomless sex")




df_client_rprovid_glmm=rbind(df_client_rprovid_clessunk_glmm,
                             df_client_rprovid_emcont_glmm,
                             df_client_rprovid_sda_glmm,
                             df_client_rprovid_tsex_glmm,
                             df_client_rprovid_cless_glmm
                             )
df_client_rprovid_glmm_out=data.frame(Var=rownames(df_client_rprovid_glmm),
                                      Estimate=round(df_client_rprovid_glmm[,1],2),
                                      ll=round(df_client_rprovid_glmm[,2],2),
                                      ul=round(df_client_rprovid_glmm[,3],2),
                                      pval=round(df_client_rprovid_glmm[,4],3),
                                      CI=paste0("(",round(df_client_rprovid_glmm[,2],2),", ",round(df_client_rprovid_glmm[,3],2),")"),
                                      "Estimate Perc"=paste0(round(df_client_rprovid_glmm[,1],2)*100,"%"),
                                      "CI Perc"=paste0("(",round(df_client_rprovid_glmm[,2],2)*100,"%, ",round(df_client_rprovid_glmm[,3],2)*100,"%)"))


## referred peers ----
ds_cfu$incons_condom=factor(ifelse(ds_cfu$c2_s2_q5==0 & ds_cfu$c2_s2_q5a!=0,1,0),levels=c(1,0),labels=c("Yes","No"))
ds_cfu$incons_condom=relevel(ds_cfu$incons_condom,ref = 'No')
ds_cfu$incons_condom_num=as.numeric(ds_cfu$incons_condom)-1


glmm_cless_c_m_glmm=glm(incons_condom_num ~ c2_s0_arm_num, family = gaussian(link="identity"), data=ds_cfu)
glmm_cless_c=summary(glmm_cless_c_m_glmm)$coef
df_client_rprovid_cless_c_glmm=cbind(cbind(glmm_cless_c[2,1],
                                         t(confint(glmm_cless_c_m_glmm)[2,])),
                                   Anova(glmm_cless_c_m_glmm)[3])
df_client_rprovid_cless_c_glmm=as.data.frame(df_client_rprovid_cless_c_glmm)
rownames(df_client_rprovid_cless_c_glmm)=c("Condomless sex")





# Figure 3 ----
## index peers ----
glmm_phq2_bin_m_glmm=glm(phq2_score_bin ~ p2_s0_arm_num, family = gaussian(link="identity"), data=ds_pfu)
glmm_phq2_bin=summary(glmm_phq2_bin_m_glmm)$coef
df_client_rprovid_phq2_glmm=cbind(cbind(glmm_phq2_bin[2,1],
                                        t(confint(glmm_phq2_bin_m_glmm)[2,])),
                                  Anova(glmm_phq2_bin_m_glmm)[3])
df_client_rprovid_phq2_glmm=as.data.frame(df_client_rprovid_phq2_glmm)
rownames(df_client_rprovid_phq2_glmm)=c("PHQ2 >= 3")



glmm_mspss_bin_m_glmm=glm(mspss_score_bin_num ~ p2_s0_arm_num, family = gaussian(link="identity"), data=ds_pfu)
glmm_mspss_bin=summary(glmm_mspss_bin_m_glmm)$coef
df_client_rprovid_mspss_glmm=cbind(cbind(glmm_mspss_bin[2,1],
                                         t(confint(glmm_mspss_bin_m_glmm)[2,])),
                                   Anova(glmm_mspss_bin_m_glmm)[3])
df_client_rprovid_mspss_glmm=as.data.frame(df_client_rprovid_mspss_glmm)
rownames(df_client_rprovid_mspss_glmm)=c("MSPSS high/medium")




## referred peers ----
glmm_phq2_bin_c_m_glmm=glm(phq2_score_bin ~ c2_s0_arm_num, family = gaussian(link="identity"), data=ds_cfu)
glmm_phq2_bin_c=summary(glmm_phq2_bin_c_m_glmm)$coef
df_client_rprovid_phq2_c_glmm=cbind(cbind(glmm_phq2_bin_c[2,1],
                                        t(confint(glmm_phq2_bin_c_m_glmm)[2,])),
                                  Anova(glmm_phq2_bin_c_m_glmm)[3])
df_client_rprovid_phq2_c_glmm=as.data.frame(df_client_rprovid_phq2_c_glmm)
rownames(df_client_rprovid_phq2_c_glmm)=c("PHQ2 >= 3")



glmm_mspss_bin_c_m_glmm=glm(mspss_score_bin_num ~ c2_s0_arm_num, family = gaussian(link="identity"), data=ds_cfu)
glmm_mspss_bin_c=summary(glmm_mspss_bin_c_m_glmm)$coef
df_client_rprovid_mspss_c_glmm=cbind(cbind(glmm_mspss_bin_c[2,1],
                                         t(confint(glmm_mspss_bin_c_m_glmm)[2,])),
                                   Anova(glmm_mspss_bin_c_m_glmm)[3])
df_client_rprovid_mspss_c_glmm=as.data.frame(df_client_rprovid_mspss_c_glmm)
rownames(df_client_rprovid_mspss_c_glmm)=c("MSPSS high/medium")




