# Installing package
install.packages("readxl")
library(readxl)
install.packages("caret")
library(caret)
install.packages("caTools")
library(caTools)

dataset = read_excel('Russian house prices_final_new.xlsx')
# Encoding categorical variables

dataset$product_type = as.numeric((factor(dataset$product_type,
                                          levels = c('Investment', 'OwnerOccupier'),
                                          labels = c(1, 2))))


dataset$culture_objects_top_25 = as.numeric(factor(dataset$culture_objects_top_25,
                                                   levels = c('no', 'yes'),
                                                   labels = c(0, 1)))

dataset$thermal_power_plant_raion = as.numeric(factor(dataset$thermal_power_plant_raion,
                                                      levels = c('no', 'yes'),
                                                      labels = c(0, 1)))

dataset$incineration_raion = as.numeric(factor(dataset$incineration_raion,
                                               levels = c('no', 'yes'),
                                               labels = c(0, 1)))

dataset$oil_chemistry_raion = as.numeric(factor(dataset$oil_chemistry_raion,
                                                levels = c('no', 'yes'),
                                                labels = c(0, 1)))

dataset$radiation_raion = as.numeric(factor(dataset$radiation_raion,
                                            levels = c('no', 'yes'),
                                            labels = c(0, 1)))

dataset$railroad_terminal_raion = as.numeric(factor(dataset$railroad_terminal_raion,
                                                    levels = c('no', 'yes'),
                                                    labels = c(0, 1)))

dataset$big_market_raion = as.numeric(factor(dataset$big_market_raion,
                                             levels = c('no', 'yes'),
                                             labels = c(0, 1)))

dataset$nuclear_reactor_raion = as.numeric(factor(dataset$nuclear_reactor_raion,
                                                  levels = c('no', 'yes'),
                                                  labels = c(0, 1)))

dataset$detention_facility_raion = as.numeric(factor(dataset$detention_facility_raion,
                                                     levels = c('no', 'yes'),
                                                     labels = c(0, 1)))

dataset$ecology = as.numeric(factor(dataset$ecology,
                                    levels = c('excellent', 'good', 'satisfactory', 'poor', 'no data'),
                                    labels = c(1, 2, 3, 4, 5)))

dataset$sub_area = as.numeric(factor(dataset$sub_area,
                                     levels = c('N', 'S', 'E', 'W', 'C', 'O'),
                                     labels = c(1, 2, 3, 4, 5, 6)))

# Taking care of missing data
dataset$state = ifelse(is.na(dataset$state),
                       ave(dataset$state, FUN = function(x) mean(x, na.rm = TRUE)), 
                       dataset$state)


# Dividing the data into training and test set 
ind = createDataPartition(dataset$price_doc, p =4/5, list = FALSE)
training_set = dataset[ind,]
test_set = dataset[-ind,]
test_set_price = test_set$price_doc

# Defining the control parameters for cross validation using CARET package
ControlParameters = trainControl(method = "cv", number = 3)

# Decision Tree using RPART library
getModelInfo()$rpart$parameters
cost_tree <- seq(0,.01,0.001)
model_rpart = train(price_doc~.+sub_area:indust_part+indust_part:culture_objects_top_25+catering_km:mosque_count_500+school_education_centers_top_20_raion:culture_objects_top_25+product_type:green_zone_part+thermal_power_plant_raion:railroad_terminal_raion+green_zone_part:detention_facility_raion+product_type:market_count_500+indust_part:university_top_20_raion+water_km:mosque_count_500+product_type:thermal_power_plant_raion+sub_area:catering_km+catering_km:ecology+indust_part:ecology+school_education_centers_top_20_raion:big_market_raion+school_education_centers_top_20_raion:green_zone_km+product_type:radiation_raion+green_zone_part:sub_area+product_type:school_education_centers_top_20_raion+product_type:incineration_raion, data = training_set, method = "rpart", 
                    trControl = ControlParameters, tuneGrid = expand.grid(cp = cost_tree))

model_rpart$bestTune
model_rpart$results
plot(model_rpart)

y_pred_tree = predict(model_rpart, newdata = test_set[-56])
RSS_tree = sum((test_set_price - y_pred_tree)^2)
TSS_tree = sum((test_set_price - mean(test_set_price))^2)
R_square_tree = 1 - (RSS_tree/TSS_tree)
R_square_tree

# Random Forest using RF library
MTRY = c(31,32,33,34,35)
modelRF = train(price_doc~.+sub_area:indust_part+indust_part:culture_objects_top_25+catering_km:mosque_count_500+school_education_centers_top_20_raion:culture_objects_top_25+product_type:green_zone_part+thermal_power_plant_raion:railroad_terminal_raion+green_zone_part:detention_facility_raion+product_type:market_count_500+indust_part:university_top_20_raion+water_km:mosque_count_500+product_type:thermal_power_plant_raion+sub_area:catering_km+catering_km:ecology+indust_part:ecology+school_education_centers_top_20_raion:big_market_raion+school_education_centers_top_20_raion:green_zone_km+product_type:radiation_raion+green_zone_part:sub_area+product_type:school_education_centers_top_20_raion+product_type:incineration_raion, data = training_set, 
                method = "rf", ntree = 100, trControl = ControlParameters,
                tuneGrid = expand.grid(mtry = MTRY))
                
getModelInfo()$RF$parameters
modelRF$bestTune
modelRF$results
plot(modelRF)

y_pred_RF = predict(modelRF, newdata = test_set[-56])
RSS_RF = sum((test_set_price - y_pred_RF)^2)
TSS_RF = sum((test_set_price - mean(test_set_price))^2)
R_square_RF = 1 - (RSS_RF/TSS_RF)
R_square_RF

# Making the Learning curve for Random Forest

set.seed(1001)
result=data.frame("Iteration"=0,"train_MSE"=0.00,"validation_MSE"=0.00)
split <- sample.split(dataset, SplitRatio = 0.9)
train <- subset(dataset, split == TRUE)
validation <- subset(dataset, split == FALSE) 
splits=seq(0,0.9,0.1)
index=0
for(split in splits){
  index=index+1;
  set.seed(0101)
  split <- sample.split(train, SplitRatio =split)
  #get training and validation data
  train_set <- subset(train, split == TRUE) 
  modelRF = train(price_doc~.+sub_area:indust_part+indust_part:culture_objects_top_25+catering_km:mosque_count_500+school_education_centers_top_20_raion:culture_objects_top_25+product_type:green_zone_part+thermal_power_plant_raion:railroad_terminal_raion+green_zone_part:detention_facility_raion+product_type:market_count_500+indust_part:university_top_20_raion+water_km:mosque_count_500+product_type:thermal_power_plant_raion+sub_area:catering_km+catering_km:ecology+indust_part:ecology+school_education_centers_top_20_raion:big_market_raion+school_education_centers_top_20_raion:green_zone_km+product_type:radiation_raion+green_zone_part:sub_area+product_type:school_education_centers_top_20_raion+product_type:incineration_raion, data = train_set, 
                  method = "rf", ntree = 100, trControl = ControlParameters,
                  tuneGrid = expand.grid(mtry = 33))
  train_set_pred = predict(modelRF,train_set)
  validation_pred = predict(modelRF,validation)
  validation_MSE = sqrt(sum((validation_pred - validation$price_doc)^2))
  train_MSE = sqrt(sum((train_set_pred - train_set$price_doc)^2))
  result=rbind(result,data.frame("Iteration"=index,"train_MSE"=train_MSE,"validation_MSE"=validation_MSE))
}
result=result[-1,]

df1 = data.frame("x"=seq(0,0.9,0.1),"y1"=result$train_MSE,"y2"=result$validation_MSE)
g =ggplot(df1, aes(x)) +                   
  geom_line(aes(y=y1), colour="red") + 
  geom_line(aes(y=y2), colour="black") +
  ylab("RMSE") + xlab("Training Data Size ")
g

# Gradient Boosting using gbm library
 
shrink <- seq(0,.1,0.01)
modelxgb = train(price_doc~.+sub_area:indust_part+indust_part:culture_objects_top_25+catering_km:mosque_count_500+school_education_centers_top_20_raion:culture_objects_top_25+product_type:green_zone_part+thermal_power_plant_raion:railroad_terminal_raion+green_zone_part:detention_facility_raion+product_type:market_count_500+indust_part:university_top_20_raion+water_km:mosque_count_500+product_type:thermal_power_plant_raion+sub_area:catering_km+catering_km:ecology+indust_part:ecology+school_education_centers_top_20_raion:big_market_raion+school_education_centers_top_20_raion:green_zone_km+product_type:radiation_raion+green_zone_part:sub_area+product_type:school_education_centers_top_20_raion+product_type:incineration_raion, data = training_set, method = "gbm", distribution = "gaussian", 
                trControl = ControlParameters,
                tuneGrid = expand.grid(n.trees = 100, interaction.depth = 10,
                shrinkage = shrink, n.minobsinnode = 5))

modelxgb$bestTune
modelxgb$results
plot(modelxgb)

y_pred_xgb = predict(modelxgb, newdata = test_set[-56])
RSS_XGB = sum((test_set_price - y_pred_xgb)^2)
TSS_XGB = sum((test_set_price - mean(test_set_price))^2)
R_square_XGB = 1 - (RSS_XGB/TSS_XGB)
R_square_XGB

# SVM using svmRadial library

getModelInfo()$svmRadial$parameters

Cost_SVM <- seq(4,8,1)
modelSVM = train(price_doc~.+sub_area:indust_part+indust_part:culture_objects_top_25+catering_km:mosque_count_500+school_education_centers_top_20_raion:culture_objects_top_25+product_type:green_zone_part+thermal_power_plant_raion:railroad_terminal_raion+green_zone_part:detention_facility_raion+product_type:market_count_500+indust_part:university_top_20_raion+water_km:mosque_count_500+product_type:thermal_power_plant_raion+sub_area:catering_km+catering_km:ecology+indust_part:ecology+school_education_centers_top_20_raion:big_market_raion+school_education_centers_top_20_raion:green_zone_km+product_type:radiation_raion+green_zone_part:sub_area+product_type:school_education_centers_top_20_raion+product_type:incineration_raion, data = training_set, method = "svmRadial", 
                 trControl = ControlParameters,
                 tuneGrid = expand.grid(sigma = 0.01,C = Cost_SVM))

modelSVM$bestTune
modelSVM$results
plot(modelSVM)

y_pred_svm = predict(modelSVM, newdata = test_set[-56])
RSS_SVM = sum((test_set_price - y_pred_svm)^2)
TSS_SVM = sum((test_set_price - mean(test_set_price))^2)
R_square_SVM = 1 - (RSS_SVM/TSS_SVM)
R_square_SVM

# Lasso using glmnet library with all the interaction terms between categorical and numerical variables

getModelInfo()$glmnet$parameters
lambdas <- seq(0,0.1,0.01)
lasso_model_allint <- train(price_doc~ + full_sq +  life_sq + floor + max_floor + material + build_year + num_room + kitch_sq + state + product_type + 
                              area_m + green_zone_part + sub_area + indust_part + school_education_centers_top_20_raion + healthcare_centers_raion + university_top_20_raion + sport_objects_raion + 
                              additional_education_raion + culture_objects_top_25 + shopping_centers_raion + thermal_power_plant_raion + incineration_raion + oil_chemistry_raion + radiation_raion + railroad_terminal_raion + 
                              big_market_raion + nuclear_reactor_raion + detention_facility_raion + green_zone_km + industrial_km + cemetery_km + water_km + big_market_km + 
                              market_shop_km + fitness_km + ice_rink_km + hospice_morgue_km + additional_education_km + church_synagogue_km + catering_km + ecology + green_part_500 + prom_part_500 
                            + office_count_500 + office_sqm_500 + trc_count_500 + trc_sqm_500 + cafe_count_500 + big_church_count_500 + church_count_500 + mosque_count_500 + 
                              leisure_count_500 + sport_count_500 + market_count_500 + full_sq:product_type+full_sq:sub_area+full_sq:culture_objects_top_25+
                              full_sq:thermal_power_plant_raion+full_sq:incineration_raion+full_sq:oil_chemistry_raion+full_sq:radiation_raion+full_sq:railroad_terminal_raion+
                              full_sq:big_market_raion+full_sq:nuclear_reactor_raion+full_sq:detention_facility_raion+full_sq:ecology+life_sq:product_type+life_sq:sub_area+
                              life_sq:culture_objects_top_25+life_sq:thermal_power_plant_raion+life_sq:incineration_raion+life_sq:oil_chemistry_raion+life_sq:radiation_raion+
                              life_sq:railroad_terminal_raion+life_sq:big_market_raion+life_sq:nuclear_reactor_raion+life_sq:detention_facility_raion+life_sq:ecology+
                              floor:product_type+floor:sub_area+floor:culture_objects_top_25+floor:thermal_power_plant_raion+floor:incineration_raion+floor:oil_chemistry_raion+
                              floor:radiation_raion+floor:railroad_terminal_raion+floor:big_market_raion+floor:nuclear_reactor_raion+floor:detention_facility_raion+
                              floor:ecology+max_floor:product_type+max_floor:sub_area+max_floor:culture_objects_top_25+max_floor:thermal_power_plant_raion+
                              max_floor:incineration_raion+max_floor:oil_chemistry_raion+max_floor:radiation_raion+max_floor:railroad_terminal_raion+max_floor:big_market_raion+
                              max_floor:nuclear_reactor_raion+max_floor:detention_facility_raion+max_floor:ecology+material:product_type+material:sub_area+
                              material:culture_objects_top_25+material:thermal_power_plant_raion+material:incineration_raion+material:oil_chemistry_raion+
                              material:radiation_raion+material:railroad_terminal_raion+material:big_market_raion+material:nuclear_reactor_raion+
                              material:detention_facility_raion+material:ecology+build_year:product_type+build_year:sub_area+build_year:culture_objects_top_25+
                              build_year:thermal_power_plant_raion+build_year:incineration_raion+build_year:oil_chemistry_raion+build_year:radiation_raion+
                              build_year:railroad_terminal_raion+build_year:big_market_raion+build_year:nuclear_reactor_raion+build_year:detention_facility_raion+
                              build_year:ecology+num_room:product_type+num_room:sub_area+num_room:culture_objects_top_25+num_room:thermal_power_plant_raion+
                              num_room:incineration_raion+num_room:oil_chemistry_raion+num_room:radiation_raion+num_room:railroad_terminal_raion+num_room:big_market_raion+
                              num_room:nuclear_reactor_raion+num_room:detention_facility_raion+num_room:ecology+kitch_sq:product_type+kitch_sq:sub_area+
                              kitch_sq:culture_objects_top_25+kitch_sq:thermal_power_plant_raion+kitch_sq:incineration_raion+kitch_sq:oil_chemistry_raion+
                              kitch_sq:radiation_raion+kitch_sq:railroad_terminal_raion+kitch_sq:big_market_raion+kitch_sq:nuclear_reactor_raion+
                              kitch_sq:detention_facility_raion+kitch_sq:ecology+state:product_type+state:sub_area+state:culture_objects_top_25+
                              state:thermal_power_plant_raion+state:incineration_raion+state:oil_chemistry_raion+state:radiation_raion+state:railroad_terminal_raion+
                              state:big_market_raion+state:nuclear_reactor_raion+state:detention_facility_raion+state:ecology+area_m:product_type+area_m:sub_area+
                              area_m:culture_objects_top_25+area_m:thermal_power_plant_raion+area_m:incineration_raion+area_m:oil_chemistry_raion+area_m:radiation_raion+
                              area_m:railroad_terminal_raion+area_m:big_market_raion+area_m:nuclear_reactor_raion+area_m:detention_facility_raion+
                              area_m:ecology+green_zone_part:product_type+green_zone_part:sub_area+green_zone_part:culture_objects_top_25+
                              green_zone_part:thermal_power_plant_raion+green_zone_part:incineration_raion+green_zone_part:oil_chemistry_raion+
                              green_zone_part:radiation_raion+green_zone_part:railroad_terminal_raion+green_zone_part:big_market_raion+green_zone_part:nuclear_reactor_raion+
                              green_zone_part:detention_facility_raion+green_zone_part:ecology+indust_part:product_type+indust_part:sub_area+
                              indust_part:culture_objects_top_25+indust_part:thermal_power_plant_raion+indust_part:incineration_raion+indust_part:oil_chemistry_raion+
                              indust_part:radiation_raion+indust_part:railroad_terminal_raion+indust_part:big_market_raion+indust_part:nuclear_reactor_raion+
                              indust_part:detention_facility_raion+indust_part:ecology+school_education_centers_top_20_raion:product_type+
                              school_education_centers_top_20_raion:sub_area+school_education_centers_top_20_raion:culture_objects_top_25+
                              school_education_centers_top_20_raion:thermal_power_plant_raion+school_education_centers_top_20_raion:incineration_raion+
                              school_education_centers_top_20_raion:oil_chemistry_raion+school_education_centers_top_20_raion:radiation_raion+
                              school_education_centers_top_20_raion:railroad_terminal_raion+school_education_centers_top_20_raion:big_market_raion+
                              school_education_centers_top_20_raion:nuclear_reactor_raion+school_education_centers_top_20_raion:detention_facility_raion+
                              school_education_centers_top_20_raion:ecology+healthcare_centers_raion:product_type+healthcare_centers_raion:sub_area+
                              healthcare_centers_raion:culture_objects_top_25+healthcare_centers_raion:thermal_power_plant_raion+
                              healthcare_centers_raion:incineration_raion+healthcare_centers_raion:oil_chemistry_raion+healthcare_centers_raion:radiation_raion+
                              healthcare_centers_raion:railroad_terminal_raion+healthcare_centers_raion:big_market_raion+
                              healthcare_centers_raion:nuclear_reactor_raion+healthcare_centers_raion:detention_facility_raion+healthcare_centers_raion:ecology+
                              university_top_20_raion:product_type+university_top_20_raion:sub_area+university_top_20_raion:culture_objects_top_25+
                              university_top_20_raion:thermal_power_plant_raion+university_top_20_raion:incineration_raion+
                              university_top_20_raion:oil_chemistry_raion+university_top_20_raion:radiation_raion+university_top_20_raion:railroad_terminal_raion+
                              university_top_20_raion:big_market_raion+university_top_20_raion:nuclear_reactor_raion+university_top_20_raion:detention_facility_raion
                            +university_top_20_raion:ecology+sport_objects_raion:product_type+sport_objects_raion:sub_area+sport_objects_raion:culture_objects_top_25
                            +sport_objects_raion:thermal_power_plant_raion+sport_objects_raion:incineration_raion+sport_objects_raion:oil_chemistry_raion
                            +sport_objects_raion:radiation_raion+sport_objects_raion:railroad_terminal_raion+sport_objects_raion:big_market_raion
                            +sport_objects_raion:nuclear_reactor_raion+sport_objects_raion:detention_facility_raion+sport_objects_raion:ecology
                            +additional_education_raion:product_type+additional_education_raion:sub_area+additional_education_raion:culture_objects_top_25
                            +additional_education_raion:thermal_power_plant_raion+additional_education_raion:incineration_raion+
                              additional_education_raion:oil_chemistry_raion+additional_education_raion:radiation_raion+
                              additional_education_raion:railroad_terminal_raion+additional_education_raion:big_market_raion+
                              additional_education_raion:nuclear_reactor_raion+additional_education_raion:detention_facility_raion+additional_education_raion:ecology+
                              shopping_centers_raion:product_type+shopping_centers_raion:sub_area+shopping_centers_raion:culture_objects_top_25+
                              shopping_centers_raion:thermal_power_plant_raion+shopping_centers_raion:incineration_raion+shopping_centers_raion:oil_chemistry_raion+
                              shopping_centers_raion:radiation_raion+shopping_centers_raion:railroad_terminal_raion+shopping_centers_raion:big_market_raion+
                              shopping_centers_raion:nuclear_reactor_raion+shopping_centers_raion:detention_facility_raion+shopping_centers_raion:ecology+
                              green_zone_km:product_type+green_zone_km:sub_area+green_zone_km:culture_objects_top_25+green_zone_km:thermal_power_plant_raion+
                              green_zone_km:incineration_raion+green_zone_km:oil_chemistry_raion+green_zone_km:radiation_raion+green_zone_km:railroad_terminal_raion+
                              green_zone_km:big_market_raion+green_zone_km:nuclear_reactor_raion+green_zone_km:detention_facility_raion+green_zone_km:ecology+
                              industrial_km:product_type+industrial_km:sub_area+industrial_km:culture_objects_top_25+industrial_km:thermal_power_plant_raion+
                              industrial_km:incineration_raion+industrial_km:oil_chemistry_raion+industrial_km:radiation_raion+industrial_km:railroad_terminal_raion+
                              industrial_km:big_market_raion+industrial_km:nuclear_reactor_raion+industrial_km:detention_facility_raion+industrial_km:ecology+
                              cemetery_km:product_type+cemetery_km:sub_area+cemetery_km:culture_objects_top_25+cemetery_km:thermal_power_plant_raion+
                              cemetery_km:incineration_raion+cemetery_km:oil_chemistry_raion+cemetery_km:radiation_raion+cemetery_km:railroad_terminal_raion+
                              cemetery_km:big_market_raion+cemetery_km:nuclear_reactor_raion+cemetery_km:detention_facility_raion+cemetery_km:ecology+
                              water_km:product_type+water_km:sub_area+water_km:culture_objects_top_25+water_km:thermal_power_plant_raion+water_km:incineration_raion+
                              water_km:oil_chemistry_raion+water_km:radiation_raion+water_km:railroad_terminal_raion+water_km:big_market_raion+
                              water_km:nuclear_reactor_raion+water_km:detention_facility_raion+water_km:ecology+big_market_km:product_type+big_market_km:sub_area+
                              big_market_km:culture_objects_top_25+big_market_km:thermal_power_plant_raion+big_market_km:incineration_raion+
                              big_market_km:oil_chemistry_raion+big_market_km:radiation_raion+big_market_km:railroad_terminal_raion+big_market_km:big_market_raion+
                              big_market_km:nuclear_reactor_raion+big_market_km:detention_facility_raion+big_market_km:ecology+market_shop_km:product_type+
                              market_shop_km:sub_area+market_shop_km:culture_objects_top_25+market_shop_km:thermal_power_plant_raion+market_shop_km:incineration_raion+
                              market_shop_km:oil_chemistry_raion+market_shop_km:radiation_raion+market_shop_km:railroad_terminal_raion+market_shop_km:big_market_raion+
                              market_shop_km:nuclear_reactor_raion+market_shop_km:detention_facility_raion+market_shop_km:ecology+fitness_km:product_type+
                              fitness_km:sub_area+fitness_km:culture_objects_top_25+fitness_km:thermal_power_plant_raion+fitness_km:incineration_raion+
                              fitness_km:oil_chemistry_raion+fitness_km:radiation_raion+fitness_km:railroad_terminal_raion+fitness_km:big_market_raion+
                              fitness_km:nuclear_reactor_raion+fitness_km:detention_facility_raion+fitness_km:ecology+ice_rink_km:product_type+ice_rink_km:sub_area+
                              ice_rink_km:culture_objects_top_25+ice_rink_km:thermal_power_plant_raion+ice_rink_km:incineration_raion+ice_rink_km:oil_chemistry_raion+
                              ice_rink_km:radiation_raion+ice_rink_km:railroad_terminal_raion+ice_rink_km:big_market_raion+ice_rink_km:nuclear_reactor_raion+
                              ice_rink_km:detention_facility_raion+ice_rink_km:ecology+hospice_morgue_km:product_type+hospice_morgue_km:sub_area+
                              hospice_morgue_km:culture_objects_top_25+hospice_morgue_km:thermal_power_plant_raion+hospice_morgue_km:incineration_raion+
                              hospice_morgue_km:oil_chemistry_raion+hospice_morgue_km:radiation_raion+hospice_morgue_km:railroad_terminal_raion+
                              hospice_morgue_km:big_market_raion+hospice_morgue_km:nuclear_reactor_raion+hospice_morgue_km:detention_facility_raion+
                              hospice_morgue_km:ecology+additional_education_km:product_type+additional_education_km:sub_area+
                              additional_education_km:culture_objects_top_25+additional_education_km:thermal_power_plant_raion+
                              additional_education_km:incineration_raion+additional_education_km:oil_chemistry_raion+additional_education_km:radiation_raion+
                              additional_education_km:railroad_terminal_raion+additional_education_km:big_market_raion+
                              additional_education_km:nuclear_reactor_raion+additional_education_km:detention_facility_raion+additional_education_km:ecology+
                              church_synagogue_km:product_type+church_synagogue_km:sub_area+church_synagogue_km:culture_objects_top_25+
                              church_synagogue_km:thermal_power_plant_raion+church_synagogue_km:incineration_raion+
                              church_synagogue_km:oil_chemistry_raion+church_synagogue_km:radiation_raion+church_synagogue_km:railroad_terminal_raion+
                              church_synagogue_km:big_market_raion+church_synagogue_km:nuclear_reactor_raion+church_synagogue_km:detention_facility_raion+
                              church_synagogue_km:ecology+catering_km:product_type+catering_km:sub_area+catering_km:culture_objects_top_25+
                              catering_km:thermal_power_plant_raion+catering_km:incineration_raion+catering_km:oil_chemistry_raion+catering_km:radiation_raion+
                              catering_km:railroad_terminal_raion+catering_km:big_market_raion+catering_km:nuclear_reactor_raion+catering_km:detention_facility_raion+
                              catering_km:ecology+green_part_500:product_type+green_part_500:sub_area+green_part_500:culture_objects_top_25+
                              green_part_500:thermal_power_plant_raion+green_part_500:incineration_raion+green_part_500:oil_chemistry_raion+
                              green_part_500:radiation_raion+green_part_500:railroad_terminal_raion+green_part_500:big_market_raion+
                              green_part_500:nuclear_reactor_raion+green_part_500:detention_facility_raion+green_part_500:ecology+prom_part_500:product_type+
                              prom_part_500:sub_area+prom_part_500:culture_objects_top_25+prom_part_500:thermal_power_plant_raion+prom_part_500:incineration_raion+
                              prom_part_500:oil_chemistry_raion+prom_part_500:radiation_raion+prom_part_500:railroad_terminal_raion+prom_part_500:big_market_raion+
                              prom_part_500:nuclear_reactor_raion+prom_part_500:detention_facility_raion+prom_part_500:ecology+office_count_500:product_type+
                              office_count_500:sub_area+office_count_500:culture_objects_top_25+office_count_500:thermal_power_plant_raion+
                              office_count_500:incineration_raion+office_count_500:oil_chemistry_raion+office_count_500:radiation_raion+
                              office_count_500:railroad_terminal_raion+office_count_500:big_market_raion+office_count_500:nuclear_reactor_raion+
                              office_count_500:detention_facility_raion+office_count_500:ecology+office_sqm_500:product_type+office_sqm_500:sub_area+
                              office_sqm_500:culture_objects_top_25+office_sqm_500:thermal_power_plant_raion+office_sqm_500:incineration_raion+
                              office_sqm_500:oil_chemistry_raion+office_sqm_500:radiation_raion+office_sqm_500:railroad_terminal_raion+office_sqm_500:big_market_raion+office_sqm_500:nuclear_reactor_raion+office_sqm_500:detention_facility_raion+office_sqm_500:ecology+trc_count_500:product_type+trc_count_500:sub_area+trc_count_500:culture_objects_top_25+trc_count_500:thermal_power_plant_raion+trc_count_500:incineration_raion+trc_count_500:oil_chemistry_raion+trc_count_500:radiation_raion+trc_count_500:railroad_terminal_raion+trc_count_500:big_market_raion+trc_count_500:nuclear_reactor_raion+trc_count_500:detention_facility_raion+trc_count_500:ecology+trc_sqm_500:product_type+trc_sqm_500:sub_area+trc_sqm_500:culture_objects_top_25+trc_sqm_500:thermal_power_plant_raion+trc_sqm_500:incineration_raion+trc_sqm_500:oil_chemistry_raion+trc_sqm_500:radiation_raion+trc_sqm_500:railroad_terminal_raion+trc_sqm_500:big_market_raion+trc_sqm_500:nuclear_reactor_raion+trc_sqm_500:detention_facility_raion+trc_sqm_500:ecology+cafe_count_500:product_type+cafe_count_500:sub_area+cafe_count_500:culture_objects_top_25+cafe_count_500:thermal_power_plant_raion+cafe_count_500:incineration_raion+cafe_count_500:oil_chemistry_raion+cafe_count_500:radiation_raion+cafe_count_500:railroad_terminal_raion+cafe_count_500:big_market_raion+cafe_count_500:nuclear_reactor_raion+cafe_count_500:detention_facility_raion+cafe_count_500:ecology+big_church_count_500:product_type+big_church_count_500:sub_area+big_church_count_500:culture_objects_top_25+big_church_count_500:thermal_power_plant_raion+big_church_count_500:incineration_raion+big_church_count_500:oil_chemistry_raion+big_church_count_500:radiation_raion+big_church_count_500:railroad_terminal_raion+big_church_count_500:big_market_raion+big_church_count_500:nuclear_reactor_raion+big_church_count_500:detention_facility_raion+big_church_count_500:ecology+church_count_500:product_type+church_count_500:sub_area+church_count_500:culture_objects_top_25+church_count_500:thermal_power_plant_raion+church_count_500:incineration_raion+church_count_500:oil_chemistry_raion+church_count_500:radiation_raion+church_count_500:railroad_terminal_raion+church_count_500:big_market_raion+church_count_500:nuclear_reactor_raion+church_count_500:detention_facility_raion+church_count_500:ecology+mosque_count_500:product_type+mosque_count_500:sub_area+mosque_count_500:culture_objects_top_25+mosque_count_500:thermal_power_plant_raion+mosque_count_500:incineration_raion+mosque_count_500:oil_chemistry_raion+mosque_count_500:radiation_raion+mosque_count_500:railroad_terminal_raion+mosque_count_500:big_market_raion+mosque_count_500:nuclear_reactor_raion+mosque_count_500:detention_facility_raion+mosque_count_500:ecology+leisure_count_500:product_type+leisure_count_500:sub_area+leisure_count_500:culture_objects_top_25+leisure_count_500:thermal_power_plant_raion+leisure_count_500:incineration_raion+leisure_count_500:oil_chemistry_raion+leisure_count_500:radiation_raion+leisure_count_500:railroad_terminal_raion+leisure_count_500:big_market_raion+leisure_count_500:nuclear_reactor_raion+leisure_count_500:detention_facility_raion+leisure_count_500:ecology+sport_count_500:product_type+sport_count_500:sub_area+sport_count_500:culture_objects_top_25+sport_count_500:thermal_power_plant_raion+sport_count_500:incineration_raion+sport_count_500:oil_chemistry_raion+sport_count_500:radiation_raion+sport_count_500:railroad_terminal_raion+sport_count_500:big_market_raion+sport_count_500:nuclear_reactor_raion+sport_count_500:detention_facility_raion+sport_count_500:ecology+market_count_500:product_type+market_count_500:sub_area+market_count_500:culture_objects_top_25+market_count_500:thermal_power_plant_raion+market_count_500:incineration_raion+market_count_500:oil_chemistry_raion+market_count_500:radiation_raion+market_count_500:railroad_terminal_raion+market_count_500:big_market_raion+market_count_500:nuclear_reactor_raion+market_count_500:detention_facility_raion+market_count_500:ecology, data = training_set, method = "glmnet", metric = "RMSE"
                     , trControl = ControlParameters,
                     tuneGrid = expand.grid(alpha=1,
                                            lambda= lambdas))

lasso_model_allint$bestTune
lasso_model_allint$results
plot(lasso_model_allint)

y_pred_lasso = predict(lasso_model_allint, newdata = test_set[-56])
RSS_lasso = sum((test_set_price - y_pred_lasso)^2)
TSS_lasso = sum((test_set_price - mean(test_set_price))^2)
R_square_lasso = 1 - (RSS_lasso/TSS_lasso)
R_square_lasso


# Model Comparison

set.seed(111)
resamps = resamples(list(Tree = model_rpart, Random_Forest = modelRF, Boosting = modelxgb, 
                         SVM = modelSVM, Lasso = lasso_model_allint))
dotplot(resamps, metric = "Rsquared", main = "Model Comparison")
dotplot(resamps, metric = "RMSE", main = "Model Comparison")

