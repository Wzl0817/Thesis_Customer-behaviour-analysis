###############1. Load data
customer.df=read.csv("C:/Users/12866/Desktop/毕设/客户分类/marketing_campaign.csv",head=T,sep='\t',
                stringsAsFactors=TRUE)
dim(customer.df)
summary(customer.df)


###############2.Handling missing data
customer.df <- customer.df[!(is.na(customer.df$Income)),]
dim(customer.df)


###############3.Rename variables
library(dplyr)
customer.df = rename(customer.df, Wines = MntWines, Fruits = MntFruits, Meat = MntMeatProducts,
                     Fish = MntFishProducts, Sweets = MntSweetProducts, Gold = MntGoldProds )
customer.df = rename(customer.df, AcceptedCmp6 = Response )
str(customer.df)


###############4.Remove unused variables
customer.df <- customer.df[-which( colnames(customer.df)== "ID" | 
                                     colnames(customer.df)== "Z_CostContact" | 
                                     colnames(customer.df)== "Z_Revenue")] 
str(customer.df)


###############5. Create new variable
#####a. Age
customer.df$Age = 2021 - customer.df$Year_Birth
customer.df <- customer.df[-which( colnames(customer.df)=="Year_Birth" )]
#####b. Number of day customer enroll with the company (until 07/31/2021)
customer.df$Dt_CustomerCovert1 = as.Date(customer.df$Dt_Customer)
customer.df$Dt_CustomerCovert2 = as.Date("2021-07-31") - as.Date(customer.df$Dt_CustomerCovert1)
customer.df$NumberofDayEnrolled = as.numeric(customer.df$Dt_CustomerCovert2, units="days")
customer.df <- customer.df[-which( colnames(customer.df)== "Dt_Customer" | 
                                     colnames(customer.df)== "Dt_CustomerCovert1" | 
                                     colnames(customer.df)== "Dt_CustomerCovert2")]
#####c.Number of campaigns accepted offer
customer.df$NumOfferAccepted1 <- customer.df$AcceptedCmp1 + customer.df$AcceptedCmp2 + customer.df$AcceptedCmp3 +
  customer.df$AcceptedCmp4 + customer.df$AcceptedCmp5 + customer.df$AcceptedCmp6
customer.df$NumOfferAccepted2 <- customer.df$AcceptedCmp1 + customer.df$AcceptedCmp2 + customer.df$AcceptedCmp3 +
  customer.df$AcceptedCmp4 + customer.df$AcceptedCmp5
#####d. Total Spent Amount
customer.df$TotalSpentAmount <- customer.df$Wines + customer.df$Fruits + customer.df$Meat +
  customer.df$Fish + customer.df$Sweets + customer.df$Gold
#####e.Family Size
customer.df$Partner <- ifelse(customer.df$Marital_Status %in% c("Married","Together"),1,0)
customer.df$FamilySize <- customer.df$Partner + customer.df$Teenhome + customer.df$Kidhome
customer.df <- customer.df[-which( colnames(customer.df)=="Partner" )]
#####f. Whether the customer is a parent
customer.df$IsParent <- ifelse(customer.df$Kidhome > 0 | customer.df$Teenhome > 0,1,0)



############6. Factor categorical variables
customer.df$Education <- as.factor(customer.df$Education)
customer.df$Marital_Status <- as.factor(customer.df$Marital_Status)


############7. Remove outliers
boxplot( customer.df$Income , main = "Income before" )
outliers = boxplot(customer.df$Income, plot=FALSE)$out
customer.df <- customer.df[!(customer.df$Income == 666666),]
boxplot( customer.df$Income , main = "Income after" )
boxplot( customer.df$Meat , main = "Meat before" )
outliers = boxplot(customer.df$Meat, plot=FALSE)$out
customer.df <- customer.df[!(customer.df$Meat > 1500),]
boxplot( customer.df$Meat , main = "Meat after" )
boxplot( customer.df$Meat , main = "Meat before" )
outliers = boxplot(customer.df$Meat, plot=FALSE)$out
customer.df <- customer.df[!(customer.df$Meat > 1500),]
boxplot( customer.df$Meat , main = "Meat after" )
boxplot( customer.df$Gold , main = "Gold before" )
outliers = boxplot(customer.df$Gold, plot=FALSE)$out
customer.df <- customer.df[!(customer.df$Gold > 250),]
boxplot( customer.df$Gold , main = "Gold after" )
boxplot( customer.df$NumWebVisitsMonth , main = "NumWebVisitsMonth before" )
outliers = boxplot(customer.df$NumWebVisitsMonth, plot=FALSE)$out
customer.df <- customer.df[!(customer.df$NumWebVisitsMonth %in% outliers),]
boxplot( customer.df$NumWebVisitsMonth , main = "NumWebVisitsMonth after" )
boxplot( customer.df$Age , main = "Age before" )
outliers = boxplot(customer.df$Age, plot=FALSE)$out
customer.df <- customer.df[!(customer.df$Age %in% outliers),]
boxplot( customer.df$Age , main = "Age after" )



############8. Reduce categories in categorical variables
#####a. Marital Status
# see how frequently each marital status occurs
MarritalStatfreq <- data.frame(table(customer.df$Marital_Status))
MarritalStatfreq[order(MarritalStatfreq$Freq, decreasing = TRUE),]

# list the marital status that appear in at least 5% of the records
MarritalStatfreq[MarritalStatfreq$Freq / nrow(customer.df) > .05, ]

# Combines all other marital status into an "Other" level
customer.df$Marital_Status <- as.factor(ifelse(customer.df$Marital_Status %in% c("Divorced", "Married", "Single","Together"), 
                                               as.character(customer.df$Marital_Status), 
                                               "Other"))
MarritalStatfreq <- data.frame(table(customer.df$Marital_Status))
MarritalStatfreq[order(MarritalStatfreq$Freq, decreasing = TRUE),]

#####e.Eduction
# see how frequently each education level occurs
Educationfreq <- data.frame(table(customer.df$Education))
Educationfreq[order(Educationfreq$Freq, decreasing = TRUE),]

# list the marital status that appear in at least 5% of the records
Educationfreq[Educationfreq$Freq / nrow(customer.df) > .05, ]

# Combines the "Basic" and "2n Cycle" education level into an "Undergraduate" level
customer.df$Education <- as.factor(ifelse(customer.df$Education %in% c("PhD", "Graduation", "Master"), 
                                          as.character(customer.df$Education), 
                                          "Undergraduate"))
Educationfreq <- data.frame(table(customer.df$Education))
Educationfreq[order(Educationfreq$Freq, decreasing = TRUE),]



########1. Remove NumOfferAccepted1
customer.df <- customer.df[-which( colnames(customer.df)== "NumOfferAccepted1" )]
########2.Create dummy variables
library( fastDummies )
customer.df <- dummy_cols( customer.df,
                           select_columns = c("Education", "Marital_Status"),
                           remove_first_dummy = TRUE,
                           remove_selected_columns = TRUE )
summary(customer.df)

customer.df


########################Modeling
