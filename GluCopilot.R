library(reshape2)
library(dplyr)
#######################################################
# Step 1: Clean data
# Goal format:
#  +----------+----------+-------------+-----------------+-------------------+---------------+-----------------+
#  | mealType | BGchange | grams_fiber | pcalories_carbs | pcalories_protein | pcalories_fat | pcalories_fiber |
#  +----------+----------+-------------+-----------------+-------------------+---------------+-----------------+
#######################################################

data<-read.csv("P1data.csv", sep=',', na.strings=c(""," ","NA"), strip.white=TRUE)
colnames(data)[1] <- "mealID"
data$mealType<-gsub("[0-9]$","",data$mealType)
data$mealType<-tolower(as.character(data$mealType))
data<-data[,c(-1)]

#######################################################
# Step 2: Assign scoring rules
# Scoring logic table: 
#  +-------------------+-------------------+---------+
#  | macronutrient     | condition         | score   |
#  +-------------------+-------------------+---------+
#  | pcalories_carbs   | > 0.65            |    1    |
#  | pcalories_carbs   | >=0.45 and <=0.65 |    0    |
#  | pcalories_carbs   | < 0.45            |   -1    |
#  +-------------------+-------------------+---------+
#  | grams_fiber       | >= 8              |    1    |
#  | grams_fiber       | < 8               |   -1    |
#  +-------------------+-------------------+---------+
#  | pcalories_protein | < 0.10            |    1    |
#  | pcalories_protein | >=0.10 and <=0.35 |    0    |
#  | pcalories_protein | > 0.35            |   -1    |
#  +-------------------+-------------------+---------+
#  | pcalories_fat     | < 0.20            |   -1    |
#  | pcalories_fat     | >=0.20 and <=0.35 |    0    |
#  | pcalories_fat     | > 0.35            |    1    |
#  +-------------------+-------------------+---------+
#
# Uses the above logic and appends the scoring as
# 'carb', 'fiber', 'protein', 'fat' columns
#######################################################
data<-data %>%
  mutate(Impact=ifelse(BGchange>50,"High","Low")) %>%
  mutate(carb=ifelse(pcalories_carbs>0.65,1,
                     ifelse(pcalories_carbs<0.45,-1,0))) %>%
  mutate(fiber=ifelse(grams_fiber >=8,1,-1)) %>% 
  mutate(protein=ifelse(pcalories_protein  >0.35,1,
                        ifelse(pcalories_protein <0.10,-1,0))) %>% 
  mutate(fat=ifelse(pcalories_fat >0.35,1,
                    ifelse(pcalories_fat <0.20,-1,0)))

# Setup meal_impact dataframe using the subject's data
meal_impact_df<-expand.grid(Impact=as.character(levels(factor(data$Impact))),
                            mealType=as.character(levels(factor(data$mealType))))

meal_impact_df<-data.frame(meal_impact_df[,c(1,2)])
meal_impact_df$Impact<-as.character(meal_impact_df$Impact)
meal_impact_df$mealType<-as.character(meal_impact_df$mealType)
#######################################################
# STEP 3: Make lookup table to determine if, on average,
# meals with high/low impact at each meal time are 
#   high (1) 
#   in-range(0) 
#   low(-1) 
# for each macronutrient compared to guideline
#######################################################
calculate_impact<-function(impact,meal){
  df<-data %>%
    filter(mealType==meal & Impact==impact) %>%
    summarise(carb_score=round(mean(carb)),
              fiber_score=round(mean(fiber)),
              protein_score=round(mean(protein)),
              fat_score=round(mean(fat))) %>% 
    as.data.frame()
  return(df)
}

results<-data.frame()
for(i in 1:nrow(meal_impact_df)){
  impact_score<-calculate_impact(meal_impact_df[i,]$Impact,
                                 meal_impact_df[i,]$mealType)
  # bind each mean result
  results<-rbind(results,impact_score)
}
meal_impact_df<-cbind(meal_impact_df,results)
###########################################################
# STEP 4: Calculate Deviation between high and low impact
# for each macronutrient at each mealType
###########################################################
# carb
df_carb<-meal_impact_df %>% 
  dcast(mealType ~ Impact, value.var = "carb_score") %>% 
  mutate(Diff_carb = Low - High) %>% 
  select(mealType, Diff_carb) %>% 
  mutate(Diff_carb = ifelse(is.na(Diff_carb),0,Diff_carb))
#fiber
df_fiber<-meal_impact_df %>% 
  dcast(mealType ~ Impact, value.var = "fiber_score") %>% 
  mutate(Diff_fiber = Low - High) %>% 
  select(mealType, Diff_fiber) %>% 
  mutate(Diff_fiber = ifelse(is.na(Diff_fiber),0,Diff_fiber))
#protein
df_protein<-meal_impact_df %>% 
  dcast(mealType ~ Impact, value.var = "protein_score") %>% 
  mutate(Diff_protein = Low - High) %>% 
  select(mealType, Diff_protein) %>% 
  mutate(Diff_protein = ifelse(is.na(Diff_protein),0,Diff_protein))
#fat
df_fat<-meal_impact_df %>% 
  dcast(mealType ~ Impact, value.var = "fat_score") %>% 
  mutate(Diff_fat = Low - High) %>% 
  select(mealType, Diff_fat) %>% 
  mutate(Diff_fat = ifelse(is.na(Diff_fat),0,Diff_fat))


#STEP 5: Identify problem hypothesis
#carb
carb_recs <- vector("character", 4L)
for(i in 1:nrow(df_carb)) {
  if (df_carb[i,]$Diff_carb > 0){ 
    carb_recs[i]<- sprintf("At %s eat more %s", df_carb[i,]$mealType,"carb")
  }
  else if (df_carb[i,]$Diff_carb == 0){
    carb_recs[i]<-sprintf("At %s ok on %s", df_carb[i,]$mealType,"carb")
  }
  else{
    carb_recs[i]<-sprintf("At %s eat less %s", df_carb[i,]$mealType,"carb")
  }
}

#fiber
fiber_recs <- vector("character", 4L)
for(i in 1:nrow(df_fiber)) {
  if (df_fiber[i,]$Diff_fiber > 0){ 
    fiber_recs[i]<- sprintf("At %s eat more %s", df_fiber[i,]$mealType,"fiber")
  }
  else if (df_fiber[i,]$Diff_fiber == 0){
    fiber_recs[i]<-sprintf("At %s ok on %s", df_fiber[i,]$mealType,"fiber")
  }
  else{
    fiber_recs[i]<-sprintf("At %s eat less %s", df_fiber[i,]$mealType,"fiber")
  }
}

#protein
protein_recs <- vector("character", 4L)
for(i in 1:nrow(df_protein)) {
  if (df_protein[i,]$Diff_protein > 0){ 
    protein_recs[i]<- sprintf("At %s eat more %s", df_protein[i,]$mealType,"protein")
  }
  else if (df_protein[i,]$Diff_protein == 0){
    protein_recs[i]<-sprintf("At %s ok on %s", df_protein[i,]$mealType,"protein")
  }
  else{
    protein_recs[i]<-sprintf("At %s eat less %s", df_protein[i,]$mealType,"protein")
  }
}

#fat
fat_recs <- vector("character", 4L)
for(i in 1:nrow(df_fat)) {
  if (df_fat[i,]$Diff_fat > 0){ 
    fat_recs[i]<- sprintf("At %s eat more %s", df_fat[i,]$mealType,"fat")
  }
  else if (df_fat[i,]$Diff_fat == 0){
    fat_recs[i]<-sprintf("At %s ok on %s", df_fat[i,]$mealType,"fat")
  }
  else{
    fat_recs[i]<-sprintf("At %s eat less %s", df_fat[i,]$mealType,"fat")
  }
}
carb_recs
fiber_recs
protein_recs
fat_recs

# Plot score (before rounding) by meal and by macronutrient, also plot average of high and low impact groups seperately
# Is it converging?
