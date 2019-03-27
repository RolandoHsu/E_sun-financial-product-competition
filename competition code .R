###### library #####
library(dummies)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rebus)
library(broom)
library(stringr)
library(tibble)
library(DMwR)
library(caret)

##### import data #####
TBN_CUST_BEHAVIOR <- read.csv("/Users/xubodun/Desktop/Ｒ/E_SUN/data/dataset＿ESUN/TBN_CUST_BEHAVIOR.csv")
TBN_LN_APPLY <- read.csv("/Users/xubodun/Desktop/Ｒ/E_SUN/data/dataset＿ESUN/TBN_LN_APPLY.csv")
TBN_CIF <- read.csv("/Users/xubodun/Desktop/Ｒ/E_SUN/data/dataset＿ESUN/TBN_CIF.csv",  na.strings=c("","NA"))
TBN_CC_APPLY <- read.csv("/Users/xubodun/Desktop/Ｒ/E_SUN/data/dataset＿ESUN/TBN_CC_APPLY.csv")
TBN_FX_TXN <- read.csv("/Users/xubodun/Desktop/Ｒ/E_SUN/data/dataset＿ESUN/TBN_FX_TXN.csv")
TBN_WM_TXN <- read.csv("/Users/xubodun/Desktop/Ｒ/E_SUN/data/dataset＿ESUN/TBN_WM_TXN.csv")
TBN_Y_ZERO <- read.csv("/Users/xubodun/Desktop/Ｒ/E_SUN/data/dataset＿ESUN/TBN_Y_ZERO.csv")

##### deal with web in M3 #####
behavior <- TBN_CUST_BEHAVIOR
beh_M3 <- behavior %>%
  mutate(
    Date = case_when(
      between(VISITDATE, 9447, 9447 + 30) ~ "M1",
      between(VISITDATE, 9447 + 31, 9447 + 60) ~ "M2",
      between(VISITDATE, 9447 + 61, 9447 + 90) ~ "M3",
      between(VISITDATE, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M3")

esun <-  ".tw" %R% fixed("/")
str <-  as_data_frame(str_split(beh_M3$PAGE, esun, simplify = T))[,2]

clean_raw <-  as_data_frame(str_split(str$V2, fixed("/"), simplify = T)) #%>% glimpse() 

cnt <- count(clean_raw, V1, V2, V3, V4, V5, V6, V7)
cnt2 <- count(clean_raw, V1, V2, V3)
cnt3 <- count(clean_raw, V1, V2)
cnt4 <- filter(cnt2, cnt2$n>= 10000)
cnt5 <- filter(cnt3, cnt3$n>= 10000)

web_inM3 <- as.character(transmute(cnt5, web = str_c(V1, V2, sep = "/"))$web) #%>% glimpse() 

beh_M3_1 <- transmute(clean_raw, web = str_c(V1, V2, sep = "/"))%>%
  cbind(beh_M3, .) %>% 
  filter(., web %in% web_inM3) %>% 
  .[,c(1,5)]

n_distinct(beh_M3_1$web)

beh_M3_1 <- beh_M3_1%>%
  count(CUST_NO, web)%>%
  spread(., web, n)%>%
  janitor::clean_names(case = "all_caps")%>%
  mutate_if(is.numeric, funs(replace_na(., replace = 0)))

beh_M3_2 <- as.data.frame(unique(beh_M3$CUST_NO))
colnames(beh_M3_2)[1] <- "CUST_NO"

beh_M3_2 <- full_join(beh_M3_2, beh_M3_1, by = "CUST_NO") %>% 
  mutate_if(is.numeric, funs(replace_na(., replace = 0)))

web_name_M3 <- as.data.frame(web_inM3)%>%
  mutate(web_code = paste("web_",1:7))

colnames(beh_M3_2)[2:8] <- web_name_M3$web_code

##### deal with CIF #####
CIF <- TBN_CIF[,-4]
CIF$EDU_CODE[is.na(CIF$EDU_CODE)] <- round(mean(CIF$EDU_CODE, na.rm = T))
CIF$INCOME_RANGE_CODE[is.na(CIF$INCOME_RANGE_CODE)] <-round(mean(CIF$INCOME_RANGE_CODE, na.rm = T)) 
CIF$WORK_MTHS[is.na(CIF$WORK_MTHS)] <-round(mean(CIF$WORK_MTHS, na.rm = T)) 
CIF$AGE[is.na(CIF$AGE)] <-round(mean(CIF$AGE, na.rm = T)) 
CIF$CHILDREN_CNT[is.na(CIF$CHILDREN_CNT)] <- 0

##### deal with LN #####
LN <- TBN_LN_APPLY
LN <- LN %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 30, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 60, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 90, 9447 + 120) ~ "M4"))
LN_M1M3 <- filter(LN, !(Date =="M4"))
LN_M4 <- filter(LN, Date == "M4")

#deal with USE in LN 
LN_CUST_dup <- LN_M1M3$CUST_NO[duplicated(LN_M1M3$CUST_NO)]
LN_M1M3_dup <- subset(LN_M1M3, LN_M1M3$CUST_NO %in% LN_CUST_dup)

LN_M1M3_use <- LN_M1M3[, c(1,4)] %>% 
  count(CUST_NO, LN_USE) %>% 
  spread(.,LN_USE, n )

LN_M1M3_dup <- arrange(LN_M1M3_dup, LN_M1M3_dup$CUST_NO)
LN_M1M3_use <- as.data.frame(unique(LN_M1M3_dup$CUST_NO)) %>% 
  mutate(., USE = 0)

i =1
for (g in LN_M1M3_use$`unique(LN_M1M3_dup$CUST_NO)`) {
  data = subset(LN_M1M3_dup, CUST_NO == g) %>% 
    subset(., TXN_DT == max(TXN_DT))
  LN_M1M3_use[i,2] = data[1, 4]
  i = i+1
}

#deal with the Amount in LN 

LN_M1M3_dup1 <- as.data.frame(LN_CUST_dup) %>% 
  mutate(., AMOUNT = 0)

i=1  
for (b in LN_CUST_dup) {
  data = subset(LN_M1M3, CUST_NO == b)
  data$LN_AMT = as.numeric(data$LN_AMT)
  LN_M1M3_dup1[i ,2] = sum(data$LN_AMT)
  i = i+1
}

LN_M1M3_dup1 <- arrange(LN_M1M3_dup1, LN_CUST_dup)

LN_M1M3_nodup <- filter(LN_M1M3, !(LN_M1M3$CUST_NO %in% LN_CUST_dup))[,c(1,3,4)] 

LN_M1M3_dupfinal <- cbind(LN_M1M3_dup1, LN_M1M3_use$USE) 
colnames(LN_M1M3_dupfinal)[1:3] <- colnames(LN_M1M3_nodup)
LN_M1M3_final <- rbind(LN_M1M3_nodup, LN_M1M3_dupfinal) %>% 
  arrange(., CUST_NO)


LN_M1M3_final <- LN_M1M3_final %>%
  mutate(
    Amount_A = case_when(
      between(LN_AMT, min(LN_AMT), quantile(LN_AMT, 0.25)) ~ 1,
      between(LN_AMT, quantile(LN_AMT, 0.25), quantile(LN_AMT, 0.5)) ~ 2,
      between(LN_AMT, quantile(LN_AMT, 0.5), quantile(LN_AMT, 0.75)) ~ 3,
      between(LN_AMT, quantile(LN_AMT, 0.75), max(LN_AMT)) ~ 4)) %>% 
  .[, -2]

##### buy LN in M4 #####
LN_buyinM4 <- unique(LN_M4$CUST_NO) %>%
  as.data.frame() %>% 
  mutate(LN_M4 = 1)
colnames(LN_buyinM4)[1] <- "CUST_NO"

##### buy other goods in M4 #####
CC <- TBN_CC_APPLY
FX <- TBN_FX_TXN
WM <- TBN_WM_TXN

LN_M3 <- LN %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 31, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 61, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M3")  
LN_buyinM3 <- unique(LN_M3$CUST_NO) %>% 
  as.data.frame() %>% 
  mutate(., LN_M3 = 1)
colnames(LN_buyinM3)[1] <- "CUST_NO"

CC_M3 <- CC %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 31, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 61, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M3")  
CC_buyinM3 <- unique(CC_M3$CUST_NO) %>% 
  as.data.frame() %>% 
  mutate(., CC_M3 = 1)
colnames(CC_buyinM3)[1] <- "CUST_NO"

FX_M3 <- FX %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 31, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 61, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M3")  
FX_buyinM3 <- unique(FX_M3$CUST_NO) %>% 
  as.data.frame() %>% 
  mutate(., FX_M3 = 1)
colnames(FX_buyinM3)[1] <- "CUST_NO"

WM_M3 <- WM %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 31, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 61, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M3")  
WM_buyinM3 <- unique(WM_M3$CUST_NO) %>% 
  as.data.frame() %>% 
  mutate(., WM_M3 = 1)
colnames(WM_buyinM3)[1] <- "CUST_NO"

##### merge CIF WEB LN ##### 
LN_forecast <- left_join(CIF, beh_M3_2, by = "CUST_NO") %>% 
  left_join(., LN_M1M3_final, by = "CUST_NO") %>% 
  left_join(., LN_buyinM4, by = "CUST_NO") %>% 
  left_join(., CC_buyinM3, by = "CUST_NO") %>% 
  left_join(., FX_buyinM3, by = "CUST_NO") %>%
  left_join(., WM_buyinM3, by = "CUST_NO") %>% 
  left_join(., LN_buyinM3, by = "CUST_NO")
LN_forecast$LN_USE <- as.numeric(LN_forecast$LN_USE)
LN_forecast[is.na(LN_forecast)] <- 0

#write.csv(LN_forecast, "C:/Users/Rpo/Desktop/E_SUNRACE/final data/final 2.0/LN_forecast_3.csv")

##### test and train #####
#LN_forecast <- read.csv("C:/Users/Rpo/Desktop/E_SUNRACE/final data/final 2.0/LN_forecast_3.csv")
LN_forecast <- LN_forecast[,c(-1,-16)]
LN_forecast$LN_M4 <- as.factor(LN_forecast$LN_M4)
LN_forecast$GENDER_CODE <- as.character(LN_forecast$GENDER_CODE)
LN_forecast[is.na(LN_forecast)] <- "N"
LN_forecast$GENDER_CODE <- as.factor(LN_forecast$GENDER_CODE)

# 轉換資料型態
v <- c(1,3,5,6,14,15)
for (i in v) {
  LN_forecast[,i] <- as.factor(LN_forecast[,i])
}
str(LN_forecast)

# train and test #
set.seed(37819)
LN_analysis <- LN_forecast[order(runif(187679)),] 
n=ceiling(nrow(LN_analysis)*0.3)
test_index=sample(1:nrow(LN_analysis),size=n)
test_data=LN_analysis[test_index, ]
train_data=LN_analysis[-test_index,]

##### SVM (need to use SMOTE to adjust.) #####
library(e1071)
SVMmodel <- svm(formula = LN_M4 ~ ., 
                data = train_data,
                type = 'C-classification',
                kernel = 'radial')

##### Decision tree #####
library(rpart)
library(rpart.plot)
colnames(train_data)

LN_analysis_new <- SMOTE(LN_M4 ~ ., train_data, perc.over = 200, learner = 'rpartXse')
prop.table(table(LN_analysis_new$y))# 確認數據是否達到平衡
# 利用能使決策樹具有最小誤差的CP來修剪樹
prunetree_LN_analysis_new  <- prune(LN_analysis_new, cp = LN_analysis_new$cptable[which.min(LN_analysis_new$cptable[,"xerror"]),"CP"]) 

# 畫圖 #
prp(prunetree_LN_analysis_new )
rpart.plot(prunetree_LN_analysis_new )
rpart.plot(prunetree_LN_analysis_new ,type=1,extra=4)

##### random forest #####
library(randomForest)
train_data$GENDER_CODE <- as.character(train_data$GENDER_CODE)
train_data[is.na(train_data)] <- "N" 
train_data$GENDER_CODE <- as.factor(train_data$GENDER_CODE)
test_data$GENDER_CODE <- as.character(test_data$GENDER_CODE)
test_data[is.na(test_data)] <- "N" 
test_data$GENDER_CODE <- as.factor(test_data$GENDER_CODE)
str(train_data$GENDER_CODE)
str(test_data$GENDER_CODE)

rFmodel <- randomForest(LN_M4 ~ ., data = train_data, ntree = 100)

test_data <- test_data[,-16]
##### predict ######
pred_prob=predict(prunetree_LN_analysis_new   ,newdata=test_data,type = "prob") # type ="class",matrix
pred_class=predict(prunetree_LN_analysis_new ,newdata=test_data,type = "class") # type ="class",matrix
summary(pred_class)
summary(pred_prob)

t1=table(real=test_data[,15],predit=pred_class)
t1
cat('correct Classification Rate = ',100*sum(diag(t1))/sum(t1),'%\n') #Correct/total
#同 cat((222+118)/(222+118+29+24))

###### ROC curve #####
library(ROCR)
res <- predict(prunetree_LN_analysis_new , train_data, type = "class") %>% 
  as.numeric() %>% 
  as.data.frame()
ROC_LN_M4 <- train_data$LN_M4 %>% as.numeric()
ROCRPred <- prediction(res$., ROC_LN_M4)
ROCRPerf<- performance(ROCRPred,"tpr","fpr")
#計算AUC
auc <- performance(ROCRPred, "auc")
plot(ROCRPerf,colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))

##### (M3M4) deal with predict data #####
answer <- TBN_Y_ZERO
answerCUST <- answer$CUST_NO
answer_1 <- as.data.frame(answer[,1])
colnames(answer_1) <- "CUST_NO"

# deal with web #
beh_M3M4 <- behavior %>%
  mutate(
    Date = case_when(
      between(VISITDATE, 9447, 9447 + 30) ~ "M1",
      between(VISITDATE, 9447 + 31, 9447 + 60) ~ "M2",
      between(VISITDATE, 9447 + 61, 9447 + 90) ~ "M3",
      between(VISITDATE, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M3" | Date == "M4")

esun <-  ".tw" %R% fixed("/")
str <-  as_data_frame(str_split(beh_M3M4$PAGE, esun, simplify = T))[,2]

clean_raw <-  as_data_frame(str_split(str$V2, fixed("/"), simplify = T)) #%>% glimpse() 

beh_M3M4_1 <- transmute(clean_raw, web = str_c(V1, V2, sep = "/"))%>%
  cbind(beh_M3M4, .) %>% 
  filter(., web %in% web_inM3) %>% 
  .[,c(1,5)]

n_distinct(beh_M3M4_1$web)

beh_M3M4_1 <- beh_M3M4_1%>%
  count(CUST_NO, web)%>%
  spread(., web, n)%>%
  janitor::clean_names(case = "all_caps")%>%
  mutate_if(is.numeric, funs(replace_na(., replace = 0)))

beh_answer <- left_join(answer_1, beh_M3M4_1, by = "CUST_NO") %>% 
  mutate_if(is.numeric, funs(replace_na(., replace = 0)))


web_name_M3M4 <- as.data.frame(web_inM3)%>%
  mutate(web_code = paste("web_", 1:7))

colnames(beh_answer)[2:8] <- web_name_M3M4$web_code

##### answer CIF #####
CIF_answer <- left_join(answer_1, CIF, by = "CUST_NO")
CIF_colnames <- colnames(CIF_answer) %>% .[-c(1,5)]
str(CIF_answer)
CIF_answer$AGE[is.na(CIF_answer$AGE)] <- round(mean(CIF_answer$AGE, na.rm = T))
CIF_answer$CHILDREN_CNT[is.na(CIF_answer$CHILDREN_CNT)] <- round(mean(CIF_answer$CHILDREN_CNT, na.rm = T))
CIF_answer$EDU_CODE[is.na(CIF_answer$EDU_CODE)] <- round(mean(CIF_answer$EDU_CODE, na.rm = T))
CIF_answer$INCOME_RANGE_CODE[is.na(CIF_answer$INCOME_RANGE_CODE)] <- round(mean(CIF_answer$INCOME_RANGE_CODE, na.rm = T))
CIF_answer$WORK_MTHS[is.na(CIF_answer$WORK_MTHS)] <- round(mean(CIF_answer$WORK_MTHS, na.rm = T))

##### answer LN #####
LN_all <- LN %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 30, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 60, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 90, 9447 + 120) ~ "M4"))

#deal with USE in LN 
LN_CUST_dup_all <- LN_all$CUST_NO[duplicated(LN_all$CUST_NO)]
LN_all_dup <- subset(LN_all, LN_all$CUST_NO %in% LN_CUST_dup_all)

LN_all_use <- LN_all[, c(1,4)] %>% 
  count(CUST_NO, LN_USE) %>% 
  spread(.,LN_USE, n )

LN_all_dup <- arrange(LN_all_dup, LN_all_dup$CUST_NO)

LN_all_use <- as.data.frame(unique(LN_all_dup$CUST_NO)) %>% 
  mutate(., USE = 0)

i =1
for (g in LN_all_use$`unique(LN_all_dup$CUST_NO)`) {
  data = subset(LN_all_dup, CUST_NO == g) %>% 
    subset(., TXN_DT == max(TXN_DT))
  LN_all_use[i,2] = data[1, 4]
  i = i+1
}

#deal with the Amount in LN 

LN_all_dup1 <- as.data.frame(unique(LN_all_dup$CUST_NO)) %>% 
  mutate(., AMOUNT = 0)

i=1  
for (b in LN_all_dup1$`unique(LN_all_dup$CUST_NO)`) {
  data = subset(LN_all, CUST_NO == b)
  data$LN_AMT = as.numeric(data$LN_AMT)
  LN_all_dup1[i ,2] = sum(data$LN_AMT)
  i = i+1
}

LN_all_dup1 <- arrange(LN_all_dup1, LN_all_dup1$`unique(LN_all_dup$CUST_NO)`)

LN_all_nodup <- filter(LN_all, !(LN_all$CUST_NO %in% unique(LN_CUST_dup_all)))[,c(1,3,4)] 

LN_all_dupfinal <- cbind(LN_all_dup1, LN_all_use$USE) 
colnames(LN_all_dupfinal)[1:3] <- colnames(LN_all_nodup)
LN_all_final <- rbind(LN_all_nodup, LN_all_dupfinal) %>% 
  arrange(., CUST_NO)

LN_all_final <- LN_all_final %>%
  mutate(
    Amount_A = case_when(
      between(LN_AMT, min(LN_AMT), quantile(LN_AMT, 0.25)) ~ "1",
      between(LN_AMT, quantile(LN_AMT, 0.25), quantile(LN_AMT, 0.5)) ~ "2",
      between(LN_AMT, quantile(LN_AMT, 0.5), quantile(LN_AMT, 0.75)) ~ "3",
      between(LN_AMT, quantile(LN_AMT, 0.75), max(LN_AMT)) ~ "4")) %>% 
  .[, -2]

##### answer deal with other goods #####
LN_M4 <- LN %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 31, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 61, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M4") 
LN_buyinM4 <- unique(LN_M4$CUST_NO) %>% 
  as.data.frame() %>% 
  mutate(., LN_M4 = 1)
colnames(LN_buyinM4)[1] <- "CUST_NO"

CC_M4 <- CC %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 31, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 61, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M4")  
CC_buyinM4 <- unique(CC_M4$CUST_NO) %>% 
  as.data.frame() %>% 
  mutate(., CC_M4 = 1)
colnames(CC_buyinM4)[1] <- "CUST_NO"

FX_M4 <- FX %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 31, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 61, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M4")  
FX_buyinM4 <- unique(FX_M4$CUST_NO) %>% 
  as.data.frame() %>% 
  mutate(., FX_M4 = 1)
colnames(FX_buyinM4)[1] <- "CUST_NO"

WM_M4 <- WM %>%
  mutate(
    Date = case_when(
      between(TXN_DT, 9447, 9447 + 30) ~ "M1",
      between(TXN_DT, 9447 + 31, 9447 + 60) ~ "M2",
      between(TXN_DT, 9447 + 61, 9447 + 90) ~ "M3",
      between(TXN_DT, 9447 + 91, 9447 + 120) ~ "M4")) %>% 
  filter(., Date == "M4")  
WM_buyinM4 <- unique(WM_M4$CUST_NO) %>% 
  as.data.frame() %>% 
  mutate(., WM_M4 = 1)
colnames(WM_buyinM4)[1] <- "CUST_NO"
LN_answer <- NULL
##### answer merge CIF WEB LN ##### 
LN_answer <- left_join(CIF_answer, beh_answer, by = "CUST_NO") %>% 
  left_join(., LN_all_final, by = "CUST_NO") %>% 
  left_join(., CC_buyinM4, by = "CUST_NO") %>% 
  left_join(., FX_buyinM4, by = "CUST_NO") %>%
  left_join(., WM_buyinM4, by = "CUST_NO") %>% 
  left_join(., LN_buyinM4, by = "CUST_NO")
LN_answer$LN_USE <- as.numeric(LN_answer$LN_USE)
LN_answer[is.na(LN_answer)] <- 0

#write.csv(LN_answer, "C:/Users/Rpo/Desktop/E_SUNRACE/final data/final 2.0/LN_answer_2.csv")

##### forecast #####
#LN_answer <- read.csv("C:/Users/Rpo/Desktop/E_SUNRACE/final data/final 2.0/LN_answer_2.csv")
LN_answer_test <- LN_answer[,-c(1,2)]
webname_answer <- colnames(test_data)[7:13]
LN_answer_test$GENDER_CODE <- as.character(LN_answer_test$GENDER_CODE)
LN_answer_test[is.na(LN_answer_test)] <- "N"
LN_answer_test$GENDER_CODE <- as.factor(LN_answer_test$GENDER_CODE)
colnames(LN_answer_test)[7:13] <- webname_answer
colnames(LN_answer_test)[16:19] <- c("CC_M3", "FX_M3", "WM_M3","LN_M3")

# 轉換資料型態
v <- c(1,3,5,6,14,15)
for (i in v) {
  LN_answer_test[,i] <- as.factor(LN_answer_test[,i])
}
str(LN_answer_test)

pred_prob_answer=predict(rFmodel,newdata=LN_answer_test,type = "prob") # type ="class",matrix
pred_class_answer=predict(rFmodel,newdata=LN_answer_test,type = "class") # type ="class",matrix
#pred <- predict(rFmodel, LN_answer_test, type = "response")
summary(pred_class)
summary(pred_prob)

answer_1 <- TBN_Y_ZERO[,-c(2:5)] %>% as.data.frame()
colnames(answer_1) <- "CUST_NO"
answer_1 <- mutate(answer_1, LN_IND = pred_class_answer)

write.csv(answer_1, "C:/Users/Rpo/Desktop/E_SUNRACE/final data/final 2.0/answer_5.csv")


