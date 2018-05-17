install.packages('randomForest')
require(randomForest)
require(caTools)

# Importing dataset

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
set.seed(111)
split = sample.split(dataset$price_doc, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
test_set_price = test_set$price_doc
attach(training_set)

# Creating Random Forest Model
regressor_RF = randomForest(x = training_set[-56]+sub_area:indust_part+indust_part:culture_objects_top_25+
                           catering_km:mosque_count_500+
                           school_education_centers_top_20_raion:culture_objects_top_25+
                           product_type:green_zone_part+thermal_power_plant_raion:railroad_terminal_raion
                         +green_zone_part:detention_facility_raion+product_type:market_count_500+
                           indust_part:university_top_20_raion+water_km:mosque_count_500+
                           product_type:thermal_power_plant_raion+sub_area:catering_km+
                           catering_km:ecology+indust_part:ecology+school_education_centers_top_20_raion:big_market_raion+
                           school_education_centers_top_20_raion:green_zone_km+product_type:radiation_raion+
                           green_zone_part:sub_area+product_type:school_education_centers_top_20_raion+
                           product_type:incineration_raion,
                         y = training_set$price_doc,replace = TRUE,ntree = 100, importance = TRUE)

summary(regressor)
y_pred = predict(regressor_RF, newdata  = test_set[-56])

RSS = sum((test_set_price - y_pred)^2)
TSS = sum((test_set_price - mean(test_set_price))^2)
R_square = 1 - (RSS/TSS)
R_square

plot(regressor$rsq)
importance(regressor)
varImpPlot(regressor)
