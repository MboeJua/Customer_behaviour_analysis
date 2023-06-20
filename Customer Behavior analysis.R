#Load/download the required libraries
MYLIBRARIES<-c("skimr","DataExplorer","dplyr","ggplot2",
"scales","car","cart","stargazer","rstatix",
"rpart","rpart.plot","tidyr","stats",
"randomForest","caret","performance","MLmetrics"
)
library(pacman)
pacman::p_load(char= MYLIBRARIES,install=TRUE,character.only=TRUE)

#Import dataset in R
main_df = read.csv("~/Downloads/Dissertation/marketing_campaign.csv")

#Explore our dataset
head(main_df,5)

#Identify data types
plot_intro(main_df,title='Dataset data types makeup')
skim(main_df)

#Analyze datasets via barchart
plot_bar(main_df,ncol=2)

# Thus we will remove those columns
main_df = subset(main_df, select = -c(Z_CostContact,Z_Revenue) )

# Remove Duplicates
#df <- main_df %>% distinct(ID, .keep_all = TRUE)
#No change to so not applied to the main_df

# Redefine Marital_Status Column
main_df = main_df %>% mutate(Marital_Status=replace(Marital_Status, Marital_Status == 'Alone' | 
                                                      Marital_Status == 'Absurd' | Marital_Status == 'YOLO' | 
                                                      Marital_Status == 'Widow' | 
                                                      Marital_Status == 'Divorced','Single')) %>%
  mutate(Marital_Status=replace(Marital_Status, Marital_Status == 'Married' | 
                                  Marital_Status == 'Together','In Relationship'))
# Redefine Education Category
main_df = main_df %>% 
  mutate(Education = replace(Education, Education == '2n Cycle','Master'))

# Redefine Income variable
main_df$Income[is.na(main_df$Income)]=median(main_df$Income,na.rm=T)

#calculate age off current year 2022
main_df$Age = 2022- main_df$Year_Birth

#calculate log of income 
main_df$LIncome = log(main_df$Income)

#define age_range variable
main_df = main_df %>% mutate(Age_Range = if_else(Age <= 16,"Children (<17)",
                            if_else(17 <= Age & Age <= 30,"Youth (17-30)",
                            if_else(30 < Age & Age <= 45,"Middle Aged (30-45)","Old (45+)"))))


#Total Spent
main_df = main_df %>%
  mutate(Total_Spent = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + 
           MntGoldProds)
#Total purchases
main_df$TotalPurchases =main_df$NumWebPurchases + main_df$NumCatalogPurchases + main_df$NumStorePurchases + 
  main_df$NumDealsPurchases

#Total accepted offers
main_df$TotalAcceptedOffers = main_df$AcceptedCmp1 + main_df$AcceptedCmp2 + main_df$AcceptedCmp3 + main_df$AcceptedCmp4 + main_df$AcceptedCmp5

#Accepted At least one campaign
main_df$AcceptedCampaign<-ifelse(main_df$TotalAcceptedOffers>0,1,0)

#calculate log of total_spent 
main_df$LTotal_Spent = log(main_df$Total_Spent)

# Calculating Loyalty based on Enrollment Year of Customers 
skim(main_df$TotalAcceptedOffers)
# Calculating the Enrollment Year of Customers 

main_df$Dt_Customer <- as.Date(main_df$Dt_Customer, "%d/%m/%Y")

main_df$Year_Enrolled <- format(main_df$Dt_Customer, "%Y")
main_df$Year_Enrolled <- as.numeric(main_df$Year_Enrolled)
main_df$Loyalty_years = 2022 - main_df$Year_Enrolled

#define loyalty_range variable
main_df = main_df %>% mutate(Loyalty_Range = if_else(Loyalty_years <= 3,"0-2",
                                                 if_else(3 <= Loyalty_years & Loyalty_years <= 5,"3-6",
                                                         if_else(7 <= Loyalty_years & Loyalty_years <= 10,"7-10",
                                                                 if_else(11 < Loyalty_years & Loyalty_years <= 15,"11-15","15plus")))))

#create subset of data with only numeric columns
num_data <- select_if(main_df, is.numeric)
boxplot(num_data)


#identify outliers
###boxplot on Income column
boxplot.stats(main_df$Income,coef = 2)$out
###Z-scores on numeric columns
z_scores <- as.data.frame(sapply(num_data, function(num_data) (abs(num_data-mean(num_data))/sd(num_data))))    
no_outliers <- z_scores[!rowSums(z_scores>4), ]
#head(no_outliers)

#Create our final dataset
master_df <- main_df[!rowSums(z_scores>4), ]
plot_intro(master_df,title='Master Dataset data types makeup')
boxplot.stats(master_df$Income,coef = 2)$out            #check ensure all possible outlier been treated




############################ Data Visualizations and Descriptive Statistics #####################################
# Percentages of Age Group
age_range = master_df %>% 
  select(Age_Range) %>%
  group_by(Age_Range) %>% 
  summarize(num=n()) %>% 
  mutate(percentage=round(num*100/sum(num),2))

ggplot(age_range, aes(x=factor(Age_Range,levels=c('Old','Middle Aged','Youth')),y=num,fill=Age_Range))+ 
  geom_bar(stat='identity') + 
  labs(x='Age Group',y='Count')+
  geom_text(aes(label=paste0(num ,' (',percentage,'%',')')),vjust=-.5)+
  scale_fill_manual(values = c("green", "red","brown" )) +
  theme_bw()


# Percentages of Loyalty group Group
loyalty_range = master_df %>% 
  select(Loyalty_years) %>%
  group_by(Loyalty_years) %>% 
  summarize(num=n()) %>% 
  mutate(percentage=round(num*100/sum(num),2))

barplot(loyalty_range$percentage,loyalty_range$Loyalty_years, 
        main = "Loyalty years of Customers (%)",
        xlab="Number of Years", names.arg = c("8", "9", "10"), 
        ylab = "Percentage",col=c("darkblue","red", "gold"),
        legend = rownames(loyalty_range$Loyalty_years), beside=TRUE)



#PDF & CDF of Total Spent Variable
ggplot(data = master_df, aes(x=Total_Spent)) + 
  geom_histogram(bins = 7,aes(y = stat(width*density)),color = "darkblue", fill = "red", 
                 alpha = 0.5) + 
  scale_x_continuous(breaks=seq(0,2800,by=400)) +
  labs(x='Total Spent',y='Probability') +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) + #from scales
  theme_bw()

#Percent of age component
mntwines <- sum(master_df$MntWines)/sum(master_df$Total_Spent)
mntfruits <- sum(master_df$MntFruits)/sum(master_df$Total_Spent)
mntmeatpdt <- sum(master_df$MntMeatProducts)/sum(master_df$Total_Spent)
mntfistpdt <- sum(master_df$MntFishProducts)/sum(master_df$Total_Spent)
mntsweetpdt <- sum(master_df$MntSweetProducts)/sum(master_df$Total_Spent)
mntgoldpdt <- sum(master_df$MntGoldProds)/sum(master_df$Total_Spent)

spent_component <- c(mntwines,mntmeatpdt,mntgoldpdt,mntfistpdt,mntsweetpdt,mntfruits)

barplot(spent_component,main = "Product Bought (%) of total spent",
        xlab="Product", names.arg = c("wines", "fruits", "meat", "fish", "sweet","gold"), 
        ylab = "Percentage",col=c("darkblue","red", "gold", "green", "purple","skyblue"),
        legend = rownames(loyalty_range$Loyalty_years), beside=TRUE, horiz = FALSE)


#PDF & CDF of Income Variable
ggplot(data = main_df, aes(x= Income)) + 
  geom_histogram(bins = 5,aes(y = stat(width*density)),color = "gold", fill = "darkgreen", alpha = 0.5) + 
  labs(x='Income',y='Probability') +
  scale_x_continuous(limits=c(0,125000),breaks=seq(0,125000,by=25000)) + # Ignoring Outlier
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) + #from scales
  theme_bw()

#marital status
ggplot(master_df, aes(x=Marital_Status,y=Total_Spent))+
  geom_boxplot(fill = 3,alpha = 0.5,color = 2,outlier.colour = 3)+
  theme_bw()

##Correlation
# Correlation Test between Income and Total Spent Variable
cor.test(master_df$Income, master_df$Total_Spent)


ggplot(master_df,aes(x=Income,y=Total_Spent))+
  geom_point(shape=21, color="black", fill="brown", size=2)+
  scale_x_continuous(limits=c(0,120000),breaks=seq(0,120000,by=20000)) + # Ignoring Outliers
  geom_smooth(method=lm, se=FALSE, color='gold',linetype='dashed')+
  theme_bw()
#se : logical value. If TRUE, confidence interval is displayed around smooth. method: linear model

# Correlation Test between log of Income and Total Spent Variable
cor.test(master_df$LIncome, master_df$Total_Spent)

ggplot(master_df,aes(x=LIncome,y=Total_Spent))+
  geom_point(shape=21, color="black", fill="brown", size=2)+
  scale_x_continuous(limits=c(0,120000),breaks=seq(0,120000,by=20000)) + # Ignoring Outliers
  geom_smooth(method=lm, se=FALSE, color='gold',linetype='dashed')+
  theme_bw()


#Correlation between Income and Ttl Purchases
cor.test(master_df$Income, master_df$TotalPurchases)


ggplot(master_df,aes(x=Income,y=TotalPurchases))+
  geom_point(shape=21, color="black", fill="brown", size=2)+
  scale_x_continuous(limits=c(0,120000),breaks=seq(0,120000,by=20000)) + # Ignoring Outliers
  geom_smooth(method=lm, se=FALSE, color='gold',linetype='dashed')+
  theme_bw()
#se : logical value. If TRUE, confidence interval is displayed around smooth. method: linear model


#correlation plot for all numeric variables
num_data <- select_if(master_df, is.numeric)
num_data <- num_data[ -c(1,17:23) ]
plot_correlation(num_data)

#Average amount spend by customers with respect to Education
ggplot(master_df, aes(x=Education,y=Total_Spent))+
  geom_boxplot(fill = 2,alpha = 0.5,color = 1,outlier.colour = 2)+
  theme_bw()

#Average amount spend by customers with respect to Products
products_df = master_df %>% select(starts_with("Mnt"))
Product_Name = data.frame(Product_Name=rep(c('Wine','Fruit','Meat','Fish','Sweet','Gold'),each=2130))
Total_Spent = data.frame(Total_Spent = unlist(products_df),row.names=NULL)
products_df = data.frame(Product_Name,Total_Spent)
head(products_df,3)


ggplot(products_df, aes(x=Product_Name,y=Total_Spent))+
  geom_boxplot(fill = 3,alpha = 0.5,color = 6,outlier.colour = 4)+
  theme_bw()


#Preferred Purchase Mode by Customers
purchase_df = master_df %>% select(starts_with("Num")) %>% select(ends_with('Purchases'))
Purchase_Name = data.frame(Purchase_Name=rep(c('Deal','Web','Catalog','Store'),each=2130))
Total_Purchases = data.frame(Total_Purchases = unlist(purchase_df),row.names=NULL)
purchase_df = data.frame(Purchase_Name,Total_Purchases)
head(purchase_df,3)

ggplot(purchase_df, aes(x=Purchase_Name,y=Total_Purchases))+
  geom_boxplot(fill = 2,alpha = 0.5,color = 1,outlier.colour = 2)+
  theme_bw()

#CDF & PDF of Customer Purchases in last 2 years
ggplot(data = master_df, aes(x= TotalPurchases)) + 
  geom_histogram(bins = 6,aes(y = stat(width*density)),color = "gold", fill = "darkgreen", 
                 alpha = 0.5) + 
  labs(x='No of Purchases',y='Probability') +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) + #from scales
  scale_x_continuous(limits=c(0,50),breaks=seq(0,50,by=5)) + # Ignoring Outliers
  theme_bw()

ggplot(main_df,aes(x=TotalPurchases))+ 
  stat_ecdf(geom='step',size=1,color="darkgreen",)+
  scale_y_continuous(breaks=seq(0,1.0,by=0.1))+
  theme_bw()

#####Performance of Campaigns
# Percentages of Accepted Offers

offers = master_df %>% 
  select(TotalAcceptedOffers) %>%
  group_by(TotalAcceptedOffers) %>% 
  summarize(num=n()) %>% 
  mutate(percentage=round(num*100/sum(num),2))

offers['TotalAcceptedOffers'] = as.factor(offers$TotalAcceptedOffers)


ggplot(offers, aes(x=factor(TotalAcceptedOffers,levels=c(0,1,2,3,4)),y=num,fill=TotalAcceptedOffers)) + 
  geom_bar(stat='identity') + 
  labs(x='Accepted No of Offers ',y='Count')+
  geom_text(aes(label=paste0(num ,' (',percentage,'%',')')),vjust=-.5)+
  scale_fill_manual(values = c('#2E5A87','#C43241','#4993C0','#146C36','#F4D166' )) +
  theme_bw()


cmp_df = master_df %>% select('AcceptedCmp1','AcceptedCmp2','AcceptedCmp3','AcceptedCmp4','AcceptedCmp5')
Campaign = data.frame(Campaign = rep(c('Campaign 1','Campaign 2','Campaign 3','Campaign 4','Campaign 5'),each=2130))
No_Of_Offers = data.frame(No_Of_Offers = unlist(cmp_df),row.names=NULL)
cmp_df = data.frame(Campaign, No_Of_Offers)
head(cmp_df,3)


cmp_df = cmp_df %>% filter(No_Of_Offers == 1) %>% 
  group_by(Campaign) %>% 
  summarize(num=n()) %>% 
  mutate(percentage=round(num*100/sum(num),2))


ggplot(cmp_df, aes(x=factor(Campaign),y=num,fill=Campaign)) + 
  geom_bar(stat='identity') + 
  labs(x='Campaign ',y='Count')+
  geom_text(aes(label=paste0(num ,' (',percentage,'%',')')),vjust=-.5)+
  scale_fill_manual(values = c('#2E5A87','#C43241','#146C36','#F4D166','#4993C0' )) +
  theme_bw()


#Total offers by Average Total Spend
master_df %>% 
  group_by(TotalAcceptedOffers) %>% 
  summarize(avg_spend = median(Income))

#Descriptive statistics
des_data <- master_df[ -c(1,6:7,9,21:27) ]
s <- skimr::skim(des_data)
print(s, include_summary = FALSE)
write.table(s, file = "sumstats.txt", sep = ",", quote = FALSE, row.names = F)

#############STATISTICAL/HYPOTHESIS TESTING
#hypothesis 1

# Testing equality of variance by levene's test
# Null Hypothesis(H0) - All groups have equal variance
# Alternate Hypothesis(H1) - Variances are not equal for altleast one pair

leveneTest(Total_Spent ~ Marital_Status, data = master_df)
# From car library

# 2 sample t test

t.test(Total_Spent ~ Marital_Status, data = master_df, var.equal=T)

#Modeling
# **** Training and testing the data on 3 different models to answer research questions***
#training testing 80-20%
training_records80<-round(nrow(master_df)*(80/100))
training80 <- master_df[1:training_records80,]
testing20 <- master_df[-(1:training_records80),]


#model 1 linear regression on 80-20%
Modellm80 <- lm(LTotal_Spent ~ Income  + Education+ Marital_Status + Age_Range + Loyalty_years, data = training80)
Modellm80a <- lm(LTotal_Spent ~ Income +Education + Age_Range+ Loyalty_years, data = training80) #additional model for appendix
Modellm80b <- lm(LTotal_Spent ~ Income  +Education +  Loyalty_years, data = training80) #additional model for appendix
summary(Modellm80)
plot(Modellm80) # diagnostics regression assumption

#checks on points
plot(Modellm80, which=1:4)

#check on influencial points
summary(influence.measures(Modellm80))


#rmse and mape
testing20$PreditedPricelm <- predict(Modellm80, testing20)
head(testing20[ , c("LTotal_Spent", "PreditedPricelm")])

actualp20lm <- testing20$LTotal_Spent
preds20lm <- testing20$PreditedPricelm

RMSE20lm <- RMSE(preds20lm, actualp20lm)
MAPE20lm <- MAPE(preds20lm, actualp20lm)
RMSE20lm
MAPE20lm

#####REGRESSION TREE
#build the initial tree
tree <- rpart(Total_Spent ~  Education + Age_Range + Marital_Status+Income+ Loyalty_years, data=training80, control=rpart.control(cp=.0001))

testing20$predictedPriceRT <- predict(tree, testing20)
head(testing20[,c("Total_Spent", "predictedPriceRT")])

preds20RT <- testing20$predictedPriceRT 
actualp20RT<- testing20$Total_Spent

RMSE20RT <- RMSE(preds20RT, actualp20RT)
MAPE20RT <- MAPE(preds20RT, actualp20RT)
RMSE20RT
MAPE20RT


#view results
printcp(tree)

#identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree <- prune(tree, cp=best)

#plot the pruned tree
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output



#model Random-forest on 80-20%
ModelRF80 <- randomForest(Total_Spent ~  Education + Age_Range + Marital_Status+Income+ Loyalty_years, data = training80, ntree = 500, mtry = 6,importance = TRUE)
summary(ModelRF80)
ModelRF80
print(round(importance(ModelRF80), 2))
plot(ModelRF80)

testing20$predictedPriceRF <- predict(ModelRF80, testing20)
head(testing20[,c("Total_Spent", "predictedPriceRF")])

preds20RF <- testing20$predictedPriceRF 
actualp20RF<- testing20$Total_Spent

RMSE20RF <- RMSE(preds20RF, actualp20RF)
MAPE20RF <- MAPE(preds20RF, actualp20RF)
RMSE20RF
MAPE20RF



######HYpothesis 2
#####REGRESSION TREE
#build the initial tree
tree2 <- rpart(TotalPurchases ~  Education + Age_Range + Marital_Status+Income+ Loyalty_years, data=training80, control=rpart.control(cp=.0001))

testing20$predictedPurchasesRT2 <- predict(tree2, testing20)
head(testing20[,c("TotalPurchases", "predictedPurchasesRT2")])

preds20RT2 <- testing20$predictedPurchasesRT2 
actualp20RT2<- testing20$TotalPurchases

RMSE20RT2 <- RMSE(preds20RT2, actualp20RT2)
MAPE20RT2 <- MAPE(preds20RT2, actualp20RT2)
RMSE20RT2
MAPE20RT2


#view results
printcp(tree2)

#identify best cp value to use
best2 <- tree2$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree2 <- prune(tree2, cp=best2)

#plot the pruned tree
prp(pruned_tree2,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output

#hyopthesis 2 linear regression on 80-20%
Model2lm80 <- lm(TotalPurchases ~ Income + Marital_Status +Education + Age_Range+ Loyalty_years, data = training80)
Model2lm80a <- lm(NumWebPurchases ~ Income + Marital_Status +Education + Age_Range+ Loyalty_years, data = training80)
Model2lm80b <- lm(NumStorePurchases ~ Income + Marital_Status +Education + Age_Range+ Loyalty_years, data = training80)
Model2lm80c <- lm(NumDealsPurchases ~ Income + Marital_Status +Education + Age_Range+ Loyalty_years, data = training80)


summary(Model2lm80)
#plot(Model2lm80)


#rmse and mape
testing20$PreditedPurchaseslm2 <- predict(Model2lm80, testing20)
head(testing20[ , c("TotalPurchases", "PreditedPurchaseslm2")])

actualp20lm2 <- testing20$TotalPurchases
preds20lm2 <- testing20$PreditedPurchaseslm2

RMSE20lm2 <- RMSE(preds20lm2, actualp20lm2)
MAPE20lm2 <- MAPE(preds20lm2, actualp20lm2)
RMSE20lm2
MAPE20lm2

#model Random-forest on 80-20%
Model2RF80 <- randomForest(TotalPurchases ~  Education + Age_Range + Marital_Status+Income+ Loyalty_years, data = training80, ntree = 500, mtry = 6,importance = TRUE)
summary(Model2RF80)
Model2RF80
print(round(importance(Model2RF80), 2))
plot(Model2RF80)

testing20$predictedPurchasesRF2 <- predict(Model2RF80, testing20)
head(testing20[,c("TotalPurchases", "predictedPurchasesRF2")])

preds20RF2 <- testing20$predictedPurchasesRF2 
actualp20RF2<- testing20$TotalPurchases

RMSE20RF2 <- RMSE(preds20RF2, actualp20RF2)
MAPE20RF2 <- MAPE(preds20RF2, actualp20RF2)
RMSE20RF2
MAPE20RF2




#Export Descriptive Statistics
stargazer(master_df, type = "text", title = "Descriptive Statistics", out = "modelsdescriptive.txt")

#Export Results Hypothesis 1
stargazer(Modellm80, type = "text", title = "Case 1: Linear Regression Results", out = "modelstest1.txt")
stargazer(Modellm80,Modellm80a,Modellm80b, type = "text", title = "Appendix:Case 1 - Linear Regression Results", out = "appendix1modelstest.txt")

#Export Results Hypothesis 2
stargazer(Model2lm80, type = "text", title = "Case 3 : Linear Regression Results", out = "hypothesis3modelstest.txt")
stargazer(Model2lm80a,Model2lm80b,Model2lm80c, type = "text", title = "Appendix:Case 3 - Linear Regression Results", out = "Appendixhypothesis3modelstest.txt")


