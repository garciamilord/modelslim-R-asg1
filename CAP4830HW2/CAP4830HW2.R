#=============================================================================
# PROGRAMMER: Garcia Milord
# PANTHER ID: 6168616
#
# CLASS: CAP4830
# SECTION: U01 1211
# SEMESTER: Spring 2021
# CLASSTIME: CAP4830 course meeting time :example T/TH 9:00-10:15 am

# CERTIFICATION: I understand FIU's academic policies, and I certify that this 
#                work is my own and that none of it is the work of any other person.

#=============================================================================
library("xlsx")         

getwd() 

#1) Read the excel file  "CAP4830_HW2_Data.xlsx"
# data into R and store the imported data in a variable named "modelData".
modelData  <- read.xlsx(file.choose(), 1) 

#2) Output the names of the modelData dataframe.
#Paste your R console output below:
  
names(modelData)

#3) Create a variable with name "model1" that stores the estimate of the linear model shown below

#UNRATE_PCH = b_0 +  b_1*DFII10_PCH + b_2 * CPILFESL_PCH + b_3 * XTEITT01CNM156S_PCH 
#+  b_4* DCOILWTICO_PCH + b_5 * PCOPPUSDM_PCH + b_6 * PCE_PCH 
#+ b_7 * WPU101_PCH + b_8 * GPDIC1_PCH + b_9 * RRVRUSQ156N_PCH

#Paste you model's summary below:
# Hint: use lm and summary

model1 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH 
             + DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH 
             + WPU101_PCH + GPDIC1_PCH + RRVRUSQ156N_PCH, data = modelData)



  
summary(model1)

#4) List all the estimate parameters from step 3 
# that are statistically significant for all "?? ??? 0.05" 


model1_Stats <- summary(model1)
names(model1_Stats)

model1_Stats$coefficients[[1,4]]
model1_Stats$coefficients[[2,4]]
model1_Stats$coefficients[[4,4]]
model1_Stats$coefficients[[5,4]]
model1_Stats$coefficients[[7,4]]

#5) Plot the model1's residual Density Function 
#Paste the plot below:
plot(density(model1$residuals))


#6) Check the model1's residual normality using the Sharpio test. 
# Paste your results below and explain your finding in one to two sentences. 
  # this data doesn't have a normal distribution
shapiro.test(model1$residuals)


#7) Create model2 which is a refinement of model1 by removing all regressors 
#that are statistically insignificant with a p < 0.55.
# Paste you model's summary below:
#7 the correct P- value for the problem is p > 0.55

model2 <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH 
             + DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH 
             + WPU101_PCH + GPDIC1_PCH, data = modelData)
summary(model2)

#8) What is the difference in your Adjusted R between model1 and model2.
# Since removing all statistically insignificant the regressors p >0.55 from 
# model1 was Adjusted R-squared:  0.8217 and now for model2 is 
# Adjusted R-squared:  0.8266. difference is between the two 
# Adjusted R-square is .0059
# 

#9) Calculate prediction accuracy and error rates of model2.
# Look at the R-script in module 10.
#

 model2_MSE <-mean(model2$residuals^2)
 model2_MSE
 #another way 
 model2.1_MSE <- data.frame(modelPred=predict(model2), 
                                   actual= HWData$UNRATE_PCH)
 head(model2.1_MSE)
model2.1_MSECal <-mean ((model2.1_MSE$actual-model2.1_MSE$modelPred)^2)
model2.1_MSECal
                                   
#10) Create model3 which is a refinement of model2. 
 # A requirement for model3 it must only have three regressors. 
 # How you pick the three regressor is up to you, but explain why you pick these
 # three. Paste the summary of model3 below.
 # The reason I pick these three for my summary of model3 because p-value <= 0 
 #and r squared is closest of any combination of three I had come up with least 
 #amount significant figures compare to the original dataset.

 model3 <- lm(UNRATE_PCH ~ DFII10_PCH + DCOILWTICO_PCH 
              + PCE_PCH , data = modelData)
 summary(model3)
 
 #11) Create model4 that uses a manual sampling technique with a training set of
 # 60% of the data and a testing set of  40%.
 # Paste the summary of the model below.
 set.seed(100)  
 
 # row indices for training data
 trainingRowIndex <- sample(1:nrow(modelData), 0.6*nrow(modelData)) 
 # model training data
 trainingData <- modelData[trainingRowIndex, ] 
 
 # test data
 testData  <- modelData[-trainingRowIndex, ]
 
 # build the model
 model4 <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH 
              + DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH 
              + WPU101_PCH + GPDIC1_PCH, data = trainingData)
summary(model4)
 
 #12) Use model4 to predict the values on the 40% testing set. 
 # Store the results in  the distPred variable and paste beginning of variable 
 # data below. Hint use head() for this.

distPred <- predict(model4,testData)
head(distPred)

#13) Using model4 calculate prediction accuracy and error rates then use ggplot 
# that shows actual vs Predicted values. Paste your plot below.


# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(index = seq(1: nrow(testData)), 
                                  actuals= testData$UNRATE_PCH,
                                  predicteds=distPred))

library(ggplot2)

gg <- ggplot(data = actuals_preds, aes(index))  + 
  geom_point(aes(y = actuals), color = "red") + 
  geom_point(aes(y = predicteds), color = "blue") +
  labs( title = "Actual vs Predicted Values")
gg


#14) Run a k-fold cross validation with k=10. 
# Paste the print of the model below.
library(caret)

controlled <- trainControl(method = "cv", number = 10)

k10_model <-train(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH 
             + DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH 
             + WPU101_PCH + GPDIC1_PCH, data = modelData, method ="lm", trControl = controlled)


names(k10_model)

print(k10_model)
