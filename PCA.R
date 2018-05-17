install.packages('caTools')
library(caTools)
install.packages('e1071')
library(e1071)

# Importing the dataset

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

# Splitting data in training and test set

set.seed(123)
split = sample.split(dataset$price_doc, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

pc = princomp(training_set[-56], cor = TRUE, score = TRUE)
summary(pc)
plot(pc)
plot(pc, type = "l")