install.packages("data.table")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("scales")
install.packages("corrplot")
install.packages("DT")
install.packages("purrr")
install.packages("ggplot2")
library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(corrplot)
library(DT)
library(purrr)
library(ggplot2)

setwd("C:/Uconn/Predicitve Modelling/Project/OriginalDataset")
dataset1 = read.csv('train.csv')
dataset1 = dataset1[,3:256]
attach(dataset1)

dataset1$product_type = as.numeric((factor(dataset1$product_type,
                                           levels = c('Investment', 'OwnerOccupier'),
                                           labels = c(1, 2))))

dataset1$culture_objects_top_25 = as.numeric(factor(dataset1$culture_objects_top_25,
                                                    levels = c('no', 'yes'),
                                                    labels = c(0, 1)))

dataset1$thermal_power_plant_raion = as.numeric(factor(dataset1$thermal_power_plant_raion,
                                                       levels = c('no', 'yes'),
                                                       labels = c(0, 1)))

dataset1$incineration_raion = as.numeric(factor(dataset1$incineration_raion,
                                                levels = c('no', 'yes'),
                                                labels = c(0, 1)))

dataset1$oil_chemistry_raion = as.numeric(factor(dataset1$oil_chemistry_raion,
                                                 levels = c('no', 'yes'),
                                                 labels = c(0, 1)))

dataset1$radiation_raion = as.numeric(factor(dataset1$radiation_raion,
                                             levels = c('no', 'yes'),
                                             labels = c(0, 1)))

dataset1$railroad_terminal_raion = as.numeric(factor(dataset1$railroad_terminal_raion,
                                                     levels = c('no', 'yes'),
                                                     labels = c(0, 1)))

dataset1$big_market_raion = as.numeric(factor(dataset1$big_market_raion,
                                              levels = c('no', 'yes'),
                                              labels = c(0, 1)))

dataset1$nuclear_reactor_raion = as.numeric(factor(dataset1$nuclear_reactor_raion,
                                                   levels = c('no', 'yes'),
                                                   labels = c(0, 1)))

dataset1$detention_facility_raion = as.numeric(factor(dataset1$detention_facility_raion,
                                                      levels = c('no', 'yes'),
                                                      labels = c(0, 1)))

dataset1$ecology = as.numeric(factor(dataset1$ecology,
                                     levels = c('excellent', 'good', 'satisfactory', 'poor', 'no data'),
                                     labels = c(1, 2, 3, 4, 2.5)))


qplot(y = price_doc, data = dataset1, colour = 'blue')


#The feature full_sq is defined in the data dictionary as 'total area in square meters, including loggias, balconies and other non-residential areas' and the life_sq is defined as 'living area in square meters, excluding loggias, balconies and other non-residential areas.' So it should be the case that life_sq is always less than full_sq.

internal_chars <- c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 
                    'kitch_sq', 'state', 'price_doc')
corrplot(cor(dataset1[, c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 
                          'kitch_sq', 'state', 'price_doc')], use="complete.obs"))

# to identify the missing value percentages

miss_pct <- map_dbl(dataset1, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
miss_pct <- miss_pct[miss_pct > 0]
data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))