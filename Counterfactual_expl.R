library(tidyverse)
library(reshape2)
library(lme4)       #for lmm
library(lmerTest)   #for EMMs and CIs around them
require(rstanarm)
library(psycho)
library(modelbased)
library(pwr)


######### Import data #######
init_exp_1 = read.csv("Experiment_1.csv")
init_exp_2 = read.csv("Experiment_2.csv")


######## Data cleaning #######
init_exp_1 = init_exp_1[-2,]
init_exp_1$ID = rep(NA,nrow(init_exp_1))

init_exp_2 = init_exp_2[-2,]
init_exp_2$ID = rep(NA,nrow(init_exp_2))


nam_1 = init_exp_1 %>% slice(1)
nam_1 = as.character(nam_1)

vars_ex_1 = c("Duration","Age", "Gender","Familiarity_S","Familiarity_AI",
                    "Degree_P_Cont","Degree_C_Cont","Degree_A_Cont","Sector_P_Cont","Sector_C_Cont","Sector_A_Cont",
                    "Sleep_P_Cont","Sleep_C_Cont","Sleep_A_Cont","Watch_P_Cont","Watch_C_Cont","Watch_A_Cont",
                    "Plant_P_Cont","Plant_C_Cont","Plant_A_Cont","Haircut_P_Cont","Haircut_C_Cont","Haircut_A_Cont",
                    "Clothes_P_Cont","Clothes_C_Cont","Clothes_A_Cont","Skiing_P_Cont","Skiing_C_Cont","Skiing_A_Cont",
                    "Penthouse_P_Cont","Penthouse_C_Cont","Penthouse_A_Cont",
                    "Degree_P_Pred","Degree_C_Pred","Degree_A_Pred","Sector_P_Pred","Sector_C_Pred","Sector_A_Pred",
                    "Sleep_P_Pred","Sleep_C_Pred","Sleep_A_Pred","Watch_P_Pred","Watch_C_Pred","Watch_A_Pred",
                    "Plant_P_Pred","Plant_C_Pred","Plant_A_Pred","Haircut_P_Pred","Haircut_C_Pred","Haircut_A_Pred",
                    "Clothes_P_Pred","Clothes_C_Pred","Clothes_A_Pred","Skiing_P_Pred","Skiing_C_Pred","Skiing_A_Pred",
                    "Penthouse_P_Pred","Penthouse_C_Pred","Penthouse_A_Pred",
                    "Degree_P_Expl","Degree_C_Expl","Degree_A_Expl","Sector_P_Expl","Sector_C_Expl","Sector_A_Expl",
                    "Sleep_P_Expl","Sleep_C_Expl","Sleep_A_Expl","Watch_P_Expl","Watch_C_Expl","Watch_A_Expl",
                    "Plant_P_Expl","Plant_C_Expl","Plant_A_Expl","Haircut_P_Expl","Haircut_C_Expl","Haircut_A_Expl",
                    "Clothes_P_Expl","Clothes_C_Expl","Clothes_A_Expl","Skiing_P_Expl","Skiing_C_Expl","Skiing_A_Expl",
                    "Penthouse_P_Expl","Penthouse_C_Expl","Penthouse_A_Expl",
                    
                    "Clear_Scenario","Clear_Qs",
                    "Difficulty_Qs","Study_length","Fatigue","Condition","ID")

nam_1[c(6,20,21,23:105,107:111,113,117)] = vars_ex_1

nam_2 = init_exp_2 %>% slice(1)
nam_2 = as.character(nam_2)

vars_ex_2 = c("Duration","Age", "Gender","Familiarity_S","Familiarity_AI",
                        
                         "Degree_P_Cont","Degree_C_Cont","Degree_A_Cont","Sector_P_Cont","Sector_C_Cont","Sector_A_Cont",
                         "Sleep_P_Cont","Sleep_C_Cont","Sleep_A_Cont",
                         "Watch_P_Cont","Watch_C_Cont","Watch_A_Cont","Plant_P_Cont","Plant_C_Cont","Plant_A_Cont",
                         "Haircut_P_Cont","Haircut_C_Cont","Haircut_A_Cont","Clothes_P_Cont","Clothes_C_Cont","Clothes_A_Cont",
                         "Skiing_P_Cont","Skiing_C_Cont","Skiing_A_Cont","Penthouse_P_Cont","Penthouse_C_Cont","Penthouse_A_Cont",
                         
                         "Degree_P_Pred","Degree_C_Pred","Degree_A_Pred","Sector_P_Pred","Sector_C_Pred","Sector_A_Pred",
                         "Sleep_P_Pred","Sleep_C_Pred","Sleep_A_Pred",
                         "Watch_P_Pred","Watch_C_Pred","Watch_A_Pred","Plant_P_Pred","Plant_C_Pred","Plant_A_Pred",
                         "Haircut_P_Pred","Haircut_C_Pred","Haircut_A_Pred","Clothes_P_Pred","Clothes_C_Pred","Clothes_A_Pred",
                         "Skiing_P_Pred","Skiing_C_Pred","Skiing_A_Pred","Penthouse_P_Pred","Penthouse_C_Pred","Penthouse_A_Pred",
                         
                         "Degree_P_Expl","Degree_C_Expl","Degree_A_Expl","Sector_P_Expl","Sector_C_Expl","Sector_A_Expl",
                         "Sleep_P_Expl","Sleep_C_Expl","Sleep_A_Expl",
                         "Watch_P_Expl","Watch_C_Expl","Watch_A_Expl","Plant_P_Expl","Plant_C_Expl","Plant_A_Expl",
                         "Haircut_P_Expl","Haircut_C_Expl","Haircut_A_Expl","Clothes_P_Expl","Clothes_C_Expl","Clothes_A_Expl",
                         "Skiing_P_Expl","Skiing_C_Expl","Skiing_A_Expl","Penthouse_P_Expl","Penthouse_C_Expl","Penthouse_A_Expl",
                         
                         "Degree_P_CCor","Degree_C_CCor","Degree_A_CCor","Sector_P_CCor","Sector_C_CCor","Sector_A_CCor",
                         "Sleep_P_CCor","Sleep_C_CCor","Sleep_A_CCor",
                         "Watch_P_CCor","Watch_C_CCor","Watch_A_CCor","Plant_P_CCor","Plant_C_CCor","Plant_A_CCor",
                         "Haircut_P_CCor","Haircut_C_CCor","Haircut_A_CCor","Clothes_P_CCor","Clothes_C_CCor","Clothes_A_CCor",
                         "Skiing_P_CCor","Skiing_C_CCor","Skiing_A_CCor","Penthouse_P_CCor","Penthouse_C_CCor","Penthouse_A_CCor",
                        
                         "Degree_P_PCor","Degree_C_PCor","Degree_A_PCor","Sector_P_PCor","Sector_C_PCor","Sector_A_PCor",
                         "Sleep_P_PCor","Sleep_C_PCor","Sleep_A_PCor",
                         "Watch_P_PCor","Watch_C_PCor","Watch_A_PCor","Plant_P_PCor","Plant_C_PCor","Plant_A_PCor",
                         "Haircut_P_PCor","Haircut_C_PCor","Haircut_A_PCor","Clothes_P_PCor","Clothes_C_PCor","Clothes_A_PCor",
                         "Skiing_P_PCor","Skiing_C_PCor","Skiing_A_PCor","Penthouse_P_PCor","Penthouse_C_PCor","Penthouse_A_PCor",
                         
                         "Degree_P_ECor","Degree_C_ECor","Degree_A_ECor","Sector_P_ECor","Sector_C_ECor","Sector_A_ECor",
                         "Sleep_P_ECor","Sleep_C_ECor","Sleep_A_ECor",
                         "Watch_P_ECor","Watch_C_ECor","Watch_A_ECor","Plant_P_ECor","Plant_C_ECor","Plant_A_ECor",
                         "Haircut_P_ECor","Haircut_C_ECor","Haircut_A_ECor","Clothes_P_ECor","Clothes_C_ECor","Clothes_A_ECor",
                         "Skiing_P_ECor","Skiing_C_ECor","Skiing_A_ECor","Penthouse_P_ECor","Penthouse_C_ECor","Penthouse_A_ECor",
                         
                         "Clear_Scenario","Clear_Qs",
                         "Difficulty_Qs","Study_length","Fatigue","Condition","ID")

nam_2[c(6,20,21,23:186,188:192,194,201)] = vars_ex_2


names(init_exp_1) = nam_1
names(init_exp_2) = nam_2

exp_1 = init_exp_1[-1,vars_ex_1]
exp_2 = init_exp_2[-1,vars_ex_2]

# Informative IDs for participants (so we know which condition they were in) 
pp_1 = data.frame(cond = c("FL_91","FL_92","FL_103"), pp_num = c("PC_","PP_","PE_"))
pp_2 = data.frame(cond = c("FL_23","FL_25","FL_26","FL_24","FL_27","FL_28"), pp_num = c("PC_","PP_","PE_","PCC_","PPC_","PEC_"))

ids = function(pp,df){
  for (i in 1:nrow(pp)){
    df$ID = replace(df$ID,df$Condition==pp$cond[i],paste(pp$pp_num[i], 1:sum(df$Condition==pp$cond[i]), sep = ""))
  }
  
  return(df)
}

exp_1 = ids(pp_1,exp_1)
exp_2 = ids(pp_2,exp_2)


#Relabel Condition variable
exp_1$Condition = sapply(exp_1$Condition,function(x) {
  if (x == "FL_91") {"Control"}
  else if (x == "FL_92") {"AI Prediction"}
  else {"AI Explanation"}
})

exp_1$Condition = factor(exp_1$Condition, levels = c("Control","AI Prediction", "AI Explanation"))
exp_1$ID = as.factor(exp_1$ID)


#For Experiment 2 the order is very important (don't update condition first!)
exp_2$AI = sapply(exp_2$Condition,function(x) {
  if (x == "FL_23" | x == "FL_24") {"Control"}
  else if (x == "FL_25" | x == "FL_27") {"AI Prediction"}
  else {"AI Explanation"}
})
exp_2$Correction = sapply(exp_2$Condition,function(x) {
  if (x == "FL_23" | x == "FL_25" | x == "FL_26") {"No Note"}
  else {"Note"}
})

exp_2$Condition = sapply(exp_2$Condition,function(x) {
  if (x == "FL_23") {"Control & No Note"}
  else if (x == "FL_24") {"Control & Note"}
  else if (x == "FL_25") {"AI Prediction & No Note"}
  else if (x == "FL_26") {"AI Explanation & No Note"}
  else if (x == "FL_27") {"AI Prediction & Note"}
  else {"AI Explanation & Note"}
})

exp_2$Condition = factor(exp_2$Condition, levels = c("Control & No Note","Control & Note","AI Prediction & No Note","AI Prediction & Note","AI Explanation & No Note","AI Explanation & Note"))
exp_2$AI = factor(exp_2$AI, levels = c("Control","AI Prediction", "AI Explanation"))
exp_2$Correction = factor(exp_2$Correction, levels = c("No Note", "Note"))
exp_2$ID = as.factor(exp_2$ID)



#Recode some Familiarity responses (for salary distribution and AI)
recode_fam = function(df){
  df$Familiarity_S[df$Familiarity_S == "1 - Not at all familiar"] = "1"
  df$Familiarity_S[df$Familiarity_S == "7 - Extremely familiar\n"] = "7"
  
  df$Familiarity_AI[df$Familiarity_AI == "1 - Not at all familiar"] = "1"
  df$Familiarity_AI[df$Familiarity_AI == "7 - Extremely familiar\n"] = "7"
  return(df)
}

exp_1 = recode_fam(exp_1)
exp_2 = recode_fam(exp_2)


#Get all non-test questions and set levels
c_q_s = c("Extremely Unclear","Moderately unclear","Slightly unclear","Neither clear nor unclear",
          "Slightly clear","Moderately clear","Extremely clear")
diff_q = c("Extremely difficult","Moderately difficult","Slightly difficult","Just right",
           "Slightly easy","Moderately easy","Extremely easy")
l_s = c("Too short","Short","Somewhat short","Just right","Somewhat long","Long","Too long")

non_test_q_ex1 = c("Duration","Age","Gender","Familiarity_S","Familiarity_AI","Clear_Scenario","Clear_Qs","Difficulty_Qs","Study_length","Fatigue","Condition","ID")
non_test_q_ex2 = c("Duration","Age","Gender","Familiarity_S","Familiarity_AI","Clear_Scenario","Clear_Qs","Difficulty_Qs","Study_length","Fatigue","Condition","AI","Correction","ID")

non_test_q = function(df,non_test_questions){
  full_non_test_q = df[,non_test_questions]
  
  full_non_test_q$Familiarity_S = factor(full_non_test_q$Familiarity_S, levels = c("1", "2", "3","4","5","6","7"))
  full_non_test_q$Familiarity_AI = factor(full_non_test_q$Familiarity_AI, levels = c("1", "2", "3","4","5","6","7"))
  full_non_test_q$Gender = factor(full_non_test_q$Gender, levels = c("Female", "Male", "Other"))
  full_non_test_q$Clear_Scenario = factor(full_non_test_q$Clear_Scenario, levels = c_q_s)
  full_non_test_q$Clear_Qs = factor(full_non_test_q$Clear_Qs, levels = c_q_s)
  full_non_test_q$Difficulty_Qs = factor(full_non_test_q$Difficulty_Qs, levels = diff_q)
  full_non_test_q$Study_length = factor(full_non_test_q$Study_length, levels = l_s)
  full_non_test_q$Fatigue = factor(full_non_test_q$Fatigue, levels = c("0","1", "2", "3","4","5","6","7","8","9","10"))
  
  return(full_non_test_q)
}

exp_1_non_test_q = non_test_q(exp_1,non_test_q_ex1)
exp_2_non_test_q = non_test_q(exp_2,non_test_q_ex2)


#Convert to numeric Duration and Age
exp_1_non_test_q[c("Duration","Age")] = lapply(exp_1_non_test_q[c("Duration","Age")], as.numeric) 
exp_2_non_test_q[c("Duration","Age")] = lapply(exp_2_non_test_q[c("Duration","Age")], as.numeric) 


####### Descriptive stats ########
desc_function = function(df,group='no'){
  df_desc = df[,c("Duration","Age","Gender","Condition")]
  if (group == 'no') {
      desc = df_desc %>%
                dplyr::summarise(.groups = "keep",
                          n.participants = n(),
                          age.mean = round(mean(Age),1),
                          age.sd = round(sd(Age),1),
                          n.female = sum(Gender == 'Female'),
                          n.other = sum(Gender == 'Other'),
                          duration.mean = round(mean(Duration,na.rm=TRUE)/60,1),
                          duration.sd = round(sd(Duration,na.rm=TRUE)/60,1))
  }
  else {
      desc = df_desc %>% group_by(Condition) %>%
                dplyr::summarise(.groups = "keep",
                                 n.participants = n(),
                                 age.mean = round(mean(Age),1),
                                 age.sd = round(sd(Age),1),
                                 n.female = sum(Gender == 'Female'),
                                 n.other = sum(Gender == 'Other'),
                                 duration.mean = round(mean(Duration,na.rm=TRUE)/60,1),
                                 duration.sd = round(sd(Duration,na.rm=TRUE)/60,1))
  }
  
  return(desc)
}


descriptives_ex1 = desc_function(exp_1_non_test_q,group='no')
descriptives_ex2 = desc_function(exp_2_non_test_q,group='no')


######### Plot non-test questions ########
non_test_questions_only = function(df,exp){
  if (exp == 1){
    full_non_test_q_factors = df[,c("Familiarity_S","Familiarity_AI","Clear_Scenario","Clear_Qs","Difficulty_Qs","Study_length","Fatigue","Condition")]
    full_non_test_q_factors[!(names(full_non_test_q_factors)%in%c("Condition"))] = lapply(full_non_test_q_factors[!(names(full_non_test_q_factors)%in%c("Condition"))], as.numeric)
    full_non_test_q_factors$Fatigue  = full_non_test_q_factors$Fatigue - 1
  } else {
    full_non_test_q_factors = df[,c("Familiarity_S","Familiarity_AI","Clear_Scenario","Clear_Qs","Difficulty_Qs","Study_length","Fatigue","Condition","AI","Correction")]
    full_non_test_q_factors[!(names(full_non_test_q_factors)%in%c("Condition","AI","Correction"))] = lapply(full_non_test_q_factors[!(names(full_non_test_q_factors)%in%c("Condition","AI","Correction"))], as.numeric)
    full_non_test_q_factors$Fatigue  = full_non_test_q_factors$Fatigue - 1
  }
  
  return(full_non_test_q_factors)

}

exp_1_just_non_test = non_test_questions_only(exp_1_non_test_q,exp = 1)
exp_2_just_non_test = non_test_questions_only(exp_2_non_test_q,exp = 2)


plot_non_test_questions = function(df){
  full_non_test_q_factors_melt = melt(df)
  
  dummy = data.frame(Familiarity_S = c(1,7),Familiarity_AI = c(1,7),Clear_Scenario = c(1,7), Clear_Qs = c(1,7), Difficulty_Qs = c(1,7), Study_length = c(1,7), Fatigue = c(0,10))
  dummy_melt = melt(dummy)
  names(dummy_melt) = c("variable","value")
  
  factors_p = ggplot(full_non_test_q_factors_melt, aes(x=value)) +
    geom_histogram(stat="count")+facet_wrap(vars(variable),scales="free")+
    geom_blank(data=dummy_melt)+
    scale_x_continuous(breaks=seq(0,100,1))+
    theme_bw()+
    theme(axis.text.x=element_text(angle=0, size=7, vjust=0.5),axis.text.y=element_text(size = 8),plot.title = element_text(size=20,face="bold"),axis.title.y = element_text(size = 10),
          panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
    #coord_cartesian(ylim=c(0,100))+
    labs(x = "", y = "Count")
  
  return(factors_p)
}

non_test_plot_exp_1 = plot_non_test_questions(exp_1_just_non_test)
non_test_plot_exp_1

non_test_plot_exp_2 = plot_non_test_questions(exp_2_just_non_test)
non_test_plot_exp_2

pdf(file = "Exp_1_pilot_factors_by_condition.pdf")
factors_p
dev.off()

########## Familiarity analyses ######
familiarity_analysis = function(df,exp){
  res = list()
  if (exp == 1){
    anov_fam_S_1 = aov(Familiarity_S ~ Condition, data = df) 
    res[['Familiarity Salary']] = summary(anov_fam_S_1)
    
    anov_fam_AI_1 = aov(Familiarity_AI ~ Condition, data = df)
    res[['Familiarity AI']] = summary(anov_fam_AI_1)
  } else {
    anov_fam_S_1 = aov(Familiarity_S ~ AI*Correction, data = df) 
    res[['Familiarity Salary']] = summary(anov_fam_S_1)
    
    anov_fam_AI_1 = aov(Familiarity_AI ~ AI*Correction, data = df)
    res[['Familiarity AI']] = summary(anov_fam_AI_1)
    
  }
  
  res[['Means']] = df %>%
                    summarise(Familiarity_S_mean = round(mean(Familiarity_S),1),
                              Familiarity_AI_mean = round(mean(Familiarity_AI),1))
  
  return(res)
}


anov_fam_exp_1 = familiarity_analysis(exp_1_just_non_test,exp = 1)
anov_fam_exp_2 = familiarity_analysis(exp_2_just_non_test,exp = 2)


######### Test questions ########
num_var_name_ex1 = c(setdiff(names(exp_1),c(non_test_q_ex1)),c("Condition","ID"))
num_var_name_ex2 = c(setdiff(names(exp_2),c(non_test_q_ex2)),"Condition","AI","Correction","ID")


test_q_ex1 = exp_1[,num_var_name_ex1]
test_q_l_ex1 = melt(test_q_ex1,id.vars = c("ID","Condition"))
test_q_l_ex1$value = as.numeric(test_q_l_ex1$value)
test_q_l_ex1 = test_q_l_ex1[complete.cases(test_q_l_ex1),]


test_q_ex2 = exp_2[,num_var_name_ex2]
test_q_l_ex2 = melt(test_q_ex2,id.vars = c("ID","Condition","AI","Correction"))
test_q_l_ex2$value = as.numeric(test_q_l_ex2$value)
test_q_l_ex2 = test_q_l_ex2[complete.cases(test_q_l_ex2),]


q_types_ex1 = sapply(test_q_l_ex1$variable,function(x) {
  if ((grepl("_P_Cont",x , fixed = TRUE) == TRUE) | (grepl("_P_Pred",x , fixed = TRUE) == TRUE) | (grepl("_P_Expl",x , fixed = TRUE) == TRUE)) {"Expectation"}
  else if ((grepl("_C_Cont",x , fixed = TRUE) == TRUE) | (grepl("_C_Pred",x , fixed = TRUE) == TRUE) | (grepl("_C_Expl",x , fixed = TRUE) == TRUE)) {"Confidence"}
  else {"Action"}
})

q_types_ex2 = sapply(test_q_l_ex2$variable,function(x) {
  if ((grepl("_P_Cont",x , fixed = TRUE) == TRUE) | (grepl("_P_Pred",x , fixed = TRUE) == TRUE) | (grepl("_P_Expl",x , fixed = TRUE) == TRUE) | 
      (grepl("_P_CCor",x , fixed = TRUE) == TRUE) | (grepl("_P_PCor",x , fixed = TRUE) == TRUE) | (grepl("_P_ECor",x , fixed = TRUE) == TRUE)) {"Expectation"}
  else if ((grepl("_C_Cont",x , fixed = TRUE) == TRUE) | (grepl("_C_Pred",x , fixed = TRUE) == TRUE) | (grepl("_C_Expl",x , fixed = TRUE) == TRUE) | 
           (grepl("_C_CCor",x , fixed = TRUE) == TRUE) | (grepl("_C_PCor",x , fixed = TRUE) == TRUE) | (grepl("_C_ECor",x , fixed = TRUE) == TRUE)) {"Confidence"}
  else {"Action"}
})

add_q_type_and_item = function(df,q_types){

  df$Question_type = q_types
  df$Question_type = factor(df$Question_type, levels = unique(df$Question_type))
  
  char = as.character(df$variable)
  
  new_var = sapply(char,function(x) {
    substr(x,1,nchar(x)-7)
  })
  
  df$Item = new_var
  df$Item = factor(df$Item, levels = unique(df$Item))

  return(df)
}


test_q_l_ex1 = add_q_type_and_item(test_q_l_ex1,q_types_ex1)
test_q_l_ex2 = add_q_type_and_item(test_q_l_ex2,q_types_ex2)


############# Box plots to represent all data ############
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 45, c = 100)[1:n] #l = 65 default
}

cols = gg_color_hue(3)

vals_ex2 = c("#F8766D", cols[1], "#00BA38",cols[2],"#619CFF",cols[3])
breaks_ex2 = c("Control & No Note", "Control & Note","AI Prediction & No Note","AI Prediction & Note",
               "AI Explanation & No Note","AI Explanation & Note")

vals_ex1 = c("#F8766D","#00BA38","#619CFF")
breaks_ex1 = c("Control","AI Prediction","AI Explanation")


plot_box_all = function(df,br, val,al){
  p_all_box = ggplot(df, aes(x=Question_type, y=value, fill=Condition)) +
    geom_boxplot() +
    geom_jitter(color="black", size=.7, alpha=al) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5),axis.text.y=element_text(size = 8),plot.title = element_text(size=20,face="bold"),axis.title.y = element_text(size = 10),
          panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
    labs(x = "", y = "Participants' estimates", fill = "Group", color ="Group")+
    scale_fill_manual(breaks = br, 
                      values=val)
return(p_all_box)
}

plot_all_box_ex1 = plot_box_all(test_q_l_ex1,breaks_ex1,vals_ex1,al=.3)
plot_all_box_ex1

plot_all_box_ex2 = plot_box_all(test_q_l_ex2,breaks_ex2,vals_ex2,al=.12)
plot_all_box_ex2


plot_box_by_item = function(df,br, val,al){
  p_box = ggplot(df, aes(x=Question_type, y=value, fill=Condition)) +
    geom_boxplot() +
    facet_wrap(vars(Item),scales="fixed")+
    geom_jitter(color="black", size=.5, alpha=al) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=25, size=9, vjust=0.5),axis.text.y=element_text(size = 8),plot.title = element_text(size=20,face="bold"),axis.title.y = element_text(size = 10),
          panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
    labs(x = "", y = "Participants' estimates", fill = "Group", color ="Group")+
    scale_fill_manual(breaks = br, 
                      values=val)
  return(p_box)
}


plot_item_box_ex1 = plot_box_by_item(test_q_l_ex1,breaks_ex1,vals_ex1,al=.3)
plot_item_box_ex1

plot_item_box_ex2 = plot_box_by_item(test_q_l_ex2,breaks_ex2,vals_ex2,al=.15)
plot_item_box_ex2

#save plots
pdf(file = "Exp_2_corr_box.pdf")
plot_item_box_ex1
dev.off()


######### Main analyses ##########
#Center predictors
test_q_l_ex1$Condition_c = sapply(test_q_l_ex1$Condition,function(x) {
    if (x == "Control") {-1}
    else if (x == "AI Prediction") {0}
    else {1}
  })

test_q_l_ex2$AI_c = sapply(test_q_l_ex2$AI,function(x) {
  if (x == "Control") {-1}
  else if (x == "AI Prediction") {0}
  else {1}
})

test_q_l_ex2$Correction_c = sapply(test_q_l_ex2$Correction,function(x) {
  if (x == "No Note") {-.5}
  else {.5}
})

###### Linear mixed-effects models #########
build_lmer_and_plot_hist_resid = function(df,exp){
  
  if (exp == 1){
    m1 = lmer(value ~ 1 + Condition_c + (1|ID), data = filter(df,Question_type == "Expectation"),REML=FALSE)
    m2 = lmer(value ~ 1 + Condition_c + (1|ID), data = filter(df,Question_type == "Confidence"),REML=FALSE)
    m3 = lmer(value ~ 1 + Condition_c + (1|ID), data = filter(df,Question_type == "Action"),REML=FALSE)
  } else {
    m1 = lmer(value ~ 1 + AI_c*Correction_c + (1|ID), data = filter(df,Question_type == "Expectation"),REML=FALSE)
    m2 = lmer(value ~ 1 + AI_c*Correction_c + (1|ID), data = filter(df,Question_type == "Confidence"),REML=FALSE)
    m3 = lmer(value ~ 1 + AI_c*Correction_c + (1|ID), data = filter(df,Question_type == "Action"),REML=FALSE)
    
  }
  
  dat = data.frame(Question_type = c(rep("Expectation",length(residuals(m1))),rep("Confidence",length(residuals(m2))),rep("Action",length(residuals(m3)))), 
                                     Residuals = c(residuals(m1),residuals(m2),residuals(m3)))
  dat$Question_type = factor(dat$Question_type, levels = c("Expectation","Confidence","Action"))
  
  histr = ggplot(dat, aes(x=Residuals)) + 
            geom_histogram(binwidth=10)+facet_wrap(vars(Question_type),scales = "free_x")+
            labs(title = "",x = "Residuals", y = "")+
            theme_bw()+theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5),axis.text.y=element_text(size = 12),plot.title = element_text(size=20,face="bold"),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),legend.text = element_text(size = 12),legend.position="top")
  
  
  qqresid = ggplot(data = dat,aes(sample = Residuals)) +
              geom_qq() +facet_wrap(vars(Question_type),scales = "free_x")+
              geom_qq_line(colour = "red") +
              labs(title = "",x = "Theoretical", y = "Sample")+
              theme_bw()+theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5),axis.text.y=element_text(size = 12),plot.title = element_text(size=20,face="bold"),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),legend.text = element_text(size = 12),legend.position="top")
  
  return(list(histr,qqresid))

}

exp_1_lmer = build_lmer_and_plot_hist_resid(test_q_l_ex1,exp = 1)
exp_1_lmer[[1]]
exp_1_lmer[[2]]

exp_2_lmer = build_lmer_and_plot_hist_resid(test_q_l_ex2,exp = 2)
exp_2_lmer[[1]]
exp_2_lmer[[2]]

##### Non-parametric tests: Kruskal-Wallis rank sum test ########
kruskal.test(value ~ Condition,data = filter(test_q_l_ex1,Question_type == "Expectation"))

kt_test = function(df){
  kt = list("Expectation" = kruskal.test(value ~ Condition,data = filter(df,Question_type == "Expectation")),
           "Confidence" = kruskal.test(value ~ Condition,data = filter(df,Question_type == "Confidence")),
           "Action" = kruskal.test(value ~ Condition,data = filter(df,Question_type == "Action")))
  
  return(kt)
}

kt_test_exp1 = kt_test(test_q_l_ex1)
kt_test_exp2 = kt_test(test_q_l_ex2)

#Pairwise Wilcoxon Rank Sum tests
pariwise_comp = function(df){
  cond = c("Expectation","Confidence","Action")
  parwise = list()
  
  for (i in cond) {
  e = filter(df,Question_type == i)
  pair_comp=pairwise.wilcox.test(e$value, e$Condition,
                      p.adjust.method = "fdr")
  parwise[[i]] = round(pair_comp$p.value,3)
  }

return(parwise)

}

pairs_exp1 = pariwise_comp(test_q_l_ex1)
pairs_exp2 = pariwise_comp(test_q_l_ex2)


###### Effect size of of Experiment 1: Action dependent variable ######
library(rstatix)

effect_size =  test_q_l_ex1 %>% group_by(Question_type) %>%
                kruskal_effsize(value ~ Condition)

