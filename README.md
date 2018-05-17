# RussianHousePricesPrediction
To develop a model that accurately predicts the prices in Russia

PROBLEM STATEMENT
Sberbank Russian housing market 

Sberbank, Russia’s oldest and largest bank, helps their customers by making predictions about realty prices so renters, developers, and lenders are more confident and well informed when they sign a lease or purchase a building. Sberbank provided a rich data set that included housing data (a total of 30000 observations and 292 variables)
Sberbank is the stakeholder involved and would be highly affected by the new sales made in the market. 
The bank profit indirectly depends on these right predictions and how many customers agree with these real time predictions. Prediction of prices is an ongoing process since the factors influencing the price keeps changing. 

DATASET and INPUTS
The set of predictors includes internal housing characteristics collection of features about each property’s surrounding neighborhood and some features that are constant across each sub -area.
This data set contains around twelve predictors which convey the internal housing features like Total are, floors, year built etc. Around 279 predictors convey the supporting information of surroundings, neighborhood and general attributes of the area like population, income group etc. After going through the data dictionary to understand what each predictor implies, we have eliminated some columns which don’t directly contribute to the house price. This step has eased the further steps in modeling.
The input file name is train.csv. We have partitioned the data in training and test using the same set. The data dictionary file is also available with name data_dictionary.txt.
