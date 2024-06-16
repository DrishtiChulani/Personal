library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(caret)
library(pROC)

setwd("C:/Users/drish/downloads")
MOTOR=read.csv("car_insurclaim.csv",header = TRUE,stringsAsFactors = FALSE)
MOTOR
MOTOR <- MOTOR[,setdiff(colnames(MOTOR),"INDEX")]
MOTOR

for(column in c("INCOME","HOME_VAL","BLUEBOOK","OLDCLAIM", "CLM_AMT"))
{
  MOTOR[,column] <- str_replace_all(MOTOR[,column],pattern='\\$',replace='')
  MOTOR[,column] <- str_replace_all(MOTOR[,column],pattern=',',replace='')
  MOTOR[,column] <- as.numeric(as.vector(MOTOR[,column]))
}
MOTOR


print("Total number of records in data:")
nrow(MOTOR)



print("Number of non-NA values per variable in data:")

non_NA_per_column <- MOTOR %>%
  gather() %>%
  na.omit(value) %>%
  count(key)

non_NA_per_column <- data.frame(non_NA_per_column)

non_NA_per_column <- non_NA_per_column[order(non_NA_per_column[,2]),]

data.frame(Variable = non_NA_per_column[,1],n = non_NA_per_column[,2])








num_unique_values_per_variable <- MOTOR %>%
  gather() %>%
  group_by(key) %>%
  summarize(uniques = n_distinct(value))

num_unique_values_per_variable <- data.frame(num_unique_values_per_variable,stringsAsFactors=FALSE)

num_unique_values_per_variable <- data.frame(Variable = num_unique_values_per_variable[,1],Num.uniques = num_unique_values_per_variable[,2],stringsAsFactors=FALSE)

num_unique_values_per_variable$Variable <- as.vector(num_unique_values_per_variable$Variable)

num_unique_values_per_variable[order(num_unique_values_per_variable[,2]),]



print("Levels of EDUCATION:")
unique(MOTOR$EDUCATION)
print("Levels of HOMEKIDS:")
unique(MOTOR$HOMEKIDS[order(MOTOR$HOMEKIDS)])




colnames(MOTOR) <- plyr::mapvalues(colnames(MOTOR),
                                       from=c("CAR_USE","MSTATUS","PARENT1","RED_CAR","REVOKED","GENDER","URBANICITY","CLAIM_FLAG"),
                                       to=c("Commercial_vehicle","Married","Single_parent","Red_car","Revoked","Sex_male","Urban_not_rural", "TARGET_FLAG"))
colnames(MOTOR)

binary_variable_translations <- data.frame(Variable = c("Commercial_vehicle","Married","Single_parent","Red_car","Revoked","Sex_male","Urban_not_rural"),
                                           True.value = c("Commercial","Yes","Yes","yes","Yes","M","Highly Urban/ Urban"),
                                           stringsAsFactors=FALSE)
binary_variable_translations




for(i in 1:length(binary_variable_translations$Variable))
{
  var <- binary_variable_translations$Variable[i]
  true_value <- binary_variable_translations$True.value[i]
  MOTOR[,var] <- ifelse(MOTOR[,var] == true_value,1,0)
}




MOTOR[,c("Commercial_vehicle","Married","Single_parent","Red_car",
         "Revoked","Sex_male","Urban_not_rural","TARGET_FLAG")] %>%
  gather("variable","value") %>%
  group_by(variable) %>%
  count(value) %>%
  mutate(value = factor(value)) %>%
  mutate(percent = n*100/8161) %>%
  ggplot(.,
         aes(variable,percent)) +
  geom_bar(stat = "identity", aes(fill = value)) +
  xlab("Variable") +
  ylab("Percent of records") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



MOTOR[,c("CLM_FREQ","HOMEKIDS","KIDSDRIV")] %>%
  gather("variable","value") %>%
  group_by(variable) %>%
  count(value) %>%
  mutate(value = factor(value,levels=5:0)) %>%
  mutate(percent = n*100/8161) %>%
  
  ggplot(.,
         aes(variable,percent)) +
  geom_bar(stat = "identity", aes(fill = value)) +
  xlab("Variable") +
  ylab("Percent of records") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = rev(c("#986599","#E69F11", "#56B6E9", 
                                  "#019E73", "#F0EF00","#CC7E73")))




