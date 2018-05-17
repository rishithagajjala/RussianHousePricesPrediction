install.packages("readxl")
library(readxl)
install.packages("usdm")
library(usdm)

# We removed nearly 45 variables based on the business understanding only as they were very vague
# Importing the dataset

dataset = read_excel('Russian house prices_final_original_file.xlsx')
dataset = dataset[,3:256]

# Encoding categorical variables into numeric variables

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
                                    labels = c(1, 2, 3, 4, 2.5)))

# Taking care of missing data
dataset$state = ifelse(is.na(dataset$state),
                       ave(dataset$state, FUN = function(x) mean(x, na.rm = TRUE)), 
                       dataset$state)

# To calculate the Mahalanobis distance for each observation
MD = mahalanobis(dataset, colMeans(dataset), cov(dataset), tol = 1e-30)
dataset_MD = data.frame(dataset, MD)

# To calculate the variable infaltion factor for each predictor
V = vif(dataset)
dataset_VIF = data.frame(dataset, V)

# To calculate the correlation values among independent variables
dataset_cor = cor(dataset)

# Importing VIF, Mahalanobis distance and Correlation matrix in excel for further analysis
write.xlsx(dataset_MD, "C:\\Users\\adity\\Desktop\\Predictive modelling\\Group project/dataset_MD.xlsx")
write.xlsx(dataset_VIF, "C:\\Users\\adity\\Desktop\\Predictive modelling\\Group project/dataset_VIF.xlsx")
write.xlsx(dataset_cor, "C:\\Users\\adity\\Desktop\\Predictive modelling\\Group project/dataset_cor.xlsx")
