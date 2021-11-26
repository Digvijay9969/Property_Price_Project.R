train = read.csv("Property_Price_Train.csv")
test = read.csv("Property_Price_Test.csv")
#variable.names(train)
names(test)
names(train)
View(test)

# Getting rid of the Id column
test_labels = test$Id
train_labels = train$Id

test$Id = NULL
train$Id = NULL

test$Sale_Price = NA
test$Sale_Price


# write.csv(test, "test2.csv")

all <- rbind(train, test) # append or Concatenate 

dim(all)# row and column

#install.packages("readr")
#install.packages("naniar")

library(readr)
library(naniar) # for graph
vis_miss(all)

sum(is.na(all))
colSums(is.na(all))
summary(all)

columns_with_missing_values = sapply(all, function(x) sum(is.na(x)))
columns_with_missing_values
columns_with_missing_values[columns_with_missing_values >0]
missing_col = columns_with_missing_values[columns_with_missing_values >0]

A = as.data.frame(missing_col)

write.csv(A, "missing_col.csv")
# Replacing NA Values with 'No Access' for Lane_Type variable
# Change the factor class type to character class first
head(all$Lane_Type)
class(all$Lane_Type)
#all$Lane_Type<- as.character(all$Lane_Type)
class(all$Lane_Type)
sum(is.na(all$Lane_Type))
table(all$Lane_Type)
# Filling NA with No Access
all$Lane_Type[is.na(all$Lane_Type)] <- "No Access"
sum(is.na(all$Lane_Type))

table(all$Lane_Type)

all$Lane_Type


sum(is.na(all$Basement_Height))
class(all$Basement_Height)

# Change the factor class type to character class first
#all$Basement_Height <- as.character(all$Basement_Height)
# Filling NA with No Access
table(all$Basement_Height)
all$Basement_Height[is.na(all$Basement_Height)] <- "No Basement"
sum(is.na(all$Basement_Height))




head(all$Basement_Condition)
class(all$Basement_Condition)
sum(is.na(all$Basement_Condition))
# Change the factor class type to character class first
#all$Basement_Condition <- as.character(all$Basement_Condition)
# Filling NA with No Access
all$Basement_Condition[is.na(all$Basement_Condition)] <- "No Basement"

sum(is.na(all$Basement_Condition))

head(all$Exposure_Level)
sum(is.na(all$Exposure_Level))
class(all$Exposure_Level)

#all$Exposure_Level <- as.character(all$Exposure_Level)
# Filling NA with No Access
all$Exposure_Level[is.na(all$Exposure_Level)] <- "No Exposure"
#all$Exposure_Level[is.na(all$Exposure_Level)] <- "No Basement"
sum(is.na(all$Exposure_Level))


head(all$BsmtFinType1)
class(all$BsmtFinType1)
sum(is.na(all$BsmtFinType1))

#all$BsmtFinType1 <- as.character(all$BsmtFinType1)
# Filling NA with No Access
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "No Basement"
sum(is.na(all$BsmtFinType1))


all$BsmtFinType2 <- as.character(all$BsmtFinType2)
# Filling NA with No Access
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- "No Basement"
sum(is.na(all$BsmtFinType2))



head(all$Fireplace_Quality)
sum(is.na(all$Fireplace_Quality))

all$Fireplace_Quality <- as.character(all$Fireplace_Quality)
# Filling NA with No Access
all$Fireplace_Quality[is.na(all$Fireplace_Quality)] <- "No Fireplace"
sum(is.na(all$Fireplace_Quality))


head(all$Garage)
sum(is.na(all$Garage))
all$Garage <- as.character(all$Garage)
# Filling NA with No Access
all$Garage[is.na(all$Garage)] <- "No Garage"
sum(is.na(all$Garage))

head(all$Garage_Built_Year)
class(all$Garage_Built_Year)

sum(is.na(all$Garage_Built_Year))
all$Garage_Built_Year

all$Garage_Built_Year[is.na(all$Garage_Built_Year)] <- 0
#all$Garage_Built_Year[is.na(all$Garage_Built_Year)] <- median(all$Garage_Built_Year, na.rm = TRUE)

head(all$Garage_Finish_Year)
class(all$Garage_Finish_Year)

all$Garage_Finish_Year <- as.character(all$Garage_Finish_Year)
# Filling NA with No Access
all$Garage_Finish_Year[is.na(all$Garage_Finish_Year)] <- "No Garage"
sum(is.na(all$Garage_Finish_Year))


head(all$Garage_Quality)
sum(is.na(all$Garage_Quality))
all$Garage_Quality <- as.character(all$Garage_Quality)
# Filling NA with No Access
all$Garage_Quality[is.na(all$Garage_Quality)] <- "No Garage"

sum(is.na(all$Garage_Condition))
head(all$Garage_Condition)
all$Garage_Condition <- as.character(all$Garage_Condition)
# Filling NA with No Access
all$Garage_Condition[is.na(all$Garage_Condition)] <- "No Information"

sum(is.na(all$Pool_Quality))

head(all$Pool_Quality)
all$Pool_Quality <- as.character(all$Pool_Quality)
# Filling NA with No Access
all$Pool_Quality[is.na(all$Pool_Quality)] <- "not disclosed"

sum(is.na(all$Fence_Quality))
all$Fence_Quality <- as.character(all$Fence_Quality)
# Filling NA with No Access
all$Fence_Quality[is.na(all$Fence_Quality)] <- "No Fence"

sum(is.na(all$Miscellaneous_Feature))

head(all$Miscellaneous_Feature)
all$Miscellaneous_Feature <- as.character(all$Miscellaneous_Feature)
# Filling NA with No Access
all$Miscellaneous_Feature[is.na(all$Miscellaneous_Feature)] <- "None"


class(all$Lot_Extent)

sum(is.na(all$Lot_Extent))
hist(all$Lot_Extent)

median(all$Lot_Extent)
median(all$Lot_Extent, na.rm = TRUE)

mean(all$Lot_Extent, na.rm = TRUE)

all$Lot_Extent[is.na(all$Lot_Extent)] <- median(all$Lot_Extent, na.rm = TRUE)
sum(is.na(all$Lot_Extent))
all$Lot_Extent

head(all$Brick_Veneer_Area)
head(all$Brick_Veneer_Type)
table(all$Brick_Veneer_Area,all$Brick_Veneer_Type)
addmargins(table(all$Brick_Veneer_Area,all$Brick_Veneer_Type))

cross <- table(all$Brick_Veneer_Area,all$Brick_Veneer_Type)
cross
cross_margin <- addmargins(cross)
cross_margin

sum(is.na(all$Brick_Veneer_Area))
table(all$Brick_Veneer_Area)

class(all$Brick_Veneer_Area)
all$Brick_Veneer_Area[is.na(all$Brick_Veneer_Area)] <- 0
all$Miscellaneous_Feature <- as.character(all$Miscellaneous_Feature)

sum(is.na(all$Brick_Veneer_Type))
all$Brick_Veneer_Type[is.na(all$Brick_Veneer_Type)] <- "None"

class(all$Building_Class)


table(all$Building_Class, all$Electrical_System)
addmargins(table(all$Building_Class, all$Electrical_System))

cross_elec <- table(all$Electrical_System,all$Building_Class)
cross_elec_margin <- addmargins(cross_elec)

sum(is.na(all$Electrical_System))

all$Electrical_System <- as.character(all$Electrical_System)
all$Electrical_System[is.na(all$Electrical_System)] <- "SBrkr"
sum(is.na(all$Electrical_System))

class(all$Zoning_Class)
sum(is.na(all$Zoning_Class))
all$Zoning_Class[is.na(all$Zoning_Class)] = "none"

table(all$Zoning_Class)
all$Zoning_Class

class(all$Utility_Type)
sum(is.na(all$Utility_Type))
all$Utility_Type[is.na(all$Utility_Type)] = "none"

table(all$Utility_Type)
all$Utility_Type

class(all$Exterior1st)
sum(is.na(all$Exterior1st))
all$Exterior1st[is.na(all$Exterior1st)] = "none"

table(all$Exterior1st)
all$Exterior1st

class(all$Exterior2nd)
sum(is.na(all$Exterior2nd))
all$Exterior2nd[is.na(all$Exterior2nd)] = "none"

table(all$Exterior2nd)
all$Exterior2nd


class(all$BsmtFinSF1)
sum(is.na(all$BsmtFinSF1))
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] = 0

table(all$BsmtFinSF1)
all$BsmtFinSF1


class(all$BsmtFinSF2)
sum(is.na(all$BsmtFinSF2))
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] = 0

table(all$BsmtFinSF2)
all$BsmtFinSF2

class(all$BsmtUnfSF)
sum(is.na(all$BsmtUnfSF))
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] = 0

table(all$BsmtUnfSF)
all$BsmtUnfSF

class(all$Total_Basement_Area)
sum(is.na(all$Total_Basement_Area))
all$Total_Basement_Area[is.na(all$Total_Basement_Area)] = 0

table(all$Total_Basement_Area)
all$Total_Basement_Area

class(all$Underground_Full_Bathroom)
sum(is.na(all$Underground_Full_Bathroom))
all$Underground_Full_Bathroom[is.na(all$Underground_Full_Bathroom)] = 0

table(all$Underground_Full_Bathroom)
all$Underground_Full_Bathroom

class(all$Underground_Half_Bathroom)
table(all$Underground_Half_Bathroom)
sum(is.na(all$Underground_Half_Bathroom))
all$Underground_Half_Bathroom[is.na(all$Underground_Half_Bathroom)] = 0

table(all$Underground_Half_Bathroom)
all$Underground_Half_Bathroom

class(all$Kitchen_Quality)
sum(is.na(all$Kitchen_Quality))
all$Kitchen_Quality[is.na(all$Kitchen_Quality)] = "none"

table(all$Kitchen_Quality)
all$Kitchen_Quality


class(all$Functional_Rate)
sum(is.na(all$Functional_Rate))
all$Functional_Rate[is.na(all$Functional_Rate)] = "none"

table(all$Functional_Rate)
all$Functional_Rate


class(all$Sale_Type)
sum(is.na(all$Sale_Type))
all$Sale_Type[is.na(all$Sale_Type)] = "none"

table(all$Sale_Type)
all$Sale_Type

colSums(is.na(all))


columns_with_missing_values <- sapply(all, function(x) sum(is.na(x))) 
columns_with_missing_values

columns_with_missing_values[columns_with_missing_values>0]

vis_miss(all)





sapply(all, class)

class(all$Building_Class)

numericVars <- which(sapply(all, is.numeric))
names(numericVars)
numericVarNames <- names(numericVars)
# cat('There are', length(numericVars), 'numeric variables')


#install.packages("corrplot")

library(corrplot)



all_numVar <- all[, numericVars]
str(all_numVar)
# cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")

cor_numVar = round(cor(all_numVar, use="pairwise.complete.obs"),2)

write.csv(cor_numVar, "Correlation.csv")

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

cor_numVar_sign <- round(cor_numVar,2)
cor_numVar_sign
View(cor_numVar_sign)

train1 = all[1:1459,] 
test1 = all[-c(1:1459),]
names(test1)

test1$Sale_Price <- NULL
# Full multiple linear regression model 
#model1 <- lm(Sale_Price ~ ., all)
#model1
#summary(model1)

test1

# Model Building

model1 <- lm(Sale_Price ~ ., train1)
model1
summary(model1)

#Model_experiment = lm (Sale_Price ~ Zoning_Class + Lot_Size + Road_Type, train1)
#summary(Model_experiment)



# predict the Sale_Price with all the X-values
#pdct1 = predict(model1, test)
#length(pdct1)
#pdct1 = predict(model1,data = all)
#length(pdct1)

pdct1 = predict(model1,data = train1)
length(pdct1)


#actual_Sale_Price = train$Sale_Price
actual_Sale_Price = train1$Sale_Price
length(actual_Sale_Price)

pred_Sale_Price = pdct1
difference = actual_Sale_Price - pred_Sale_Price
length(difference)

df_Sale_Price = data.frame(actual_Sale_Price, pred_Sale_Price,difference)
write.csv(df_Sale_Price,"diff.csv")

View(df_Sale_Price)
sum(df_Sale_Price)



#install.packages("broom")
library(broom)
tidy_model = tidy(model1)


#options(scipen = 999)
# This gives statistics associated with all the variables
tidy_model
tidy_model$term[tidy_model$p.value < 0.05]


sigvar<-tidy_model$term[tidy_model$p.value < 0.05]
sigvar


ABC<-c(sigvar,"Sale_Price")
ABC
XYZ <- all[,names(all) %in% ABC]
XYZ
train2 = XYZ[1:1459,]
train2

test2 = XYZ[-c(1:1459),]
test2
names(test1)

test2$Sale_Price <- NULL


sigvar_model <- lm(Sale_Price ~., data = train2)
summary(sigvar_model)


pdct_sig = predict(sigvar_model, data = train2)
pdct_sig
# table showing the difference between the actual and predicted values
actual_Sale_Price_sig = train2$Sale_Price
length(actual_Sale_Price)

pred_Sale_Price_sig = pdct_sig

difference = actual_Sale_Price_sig - pred_Sale_Price_sig

df_Sale_Price = data.frame(actual_Sale_Price_sig, pred_Sale_Price_sig,difference)

# final prediction on test2 data 

pdct_sig_test = predict(sigvar_model, data = test2)
pdct_sig_test

summary(pdct_sig_test)
