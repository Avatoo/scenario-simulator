library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)
library(lubridate)

source('utilities.R')

# data import
df = import_mm()

# clustering
p  = cluster_mm(df, k = 10)

# senario_0
df1 = run_senario_0(df = df, p = p)
df20 = df1 %>% filter(Asset_ID=='PROJECT-ASSET-210')

# senario_2
df21 = run_senario_2(df = df1, this_asset='PROJECT-ASSET-210',
                    standard_var = 'Depth', standard_method = 'max')

df22 = run_senario_2(df = df1, this_asset='PROJECT-ASSET-210',
                     standard_var = 'Length', standard_method = 'max')

df23 = run_senario_2(df = df1, this_asset='PROJECT-ASSET-210',
                     standard_var = 'Price', standard_method = 'min')



# df24 = run_senario_2(df = df1, this_asset='PROJECT-ASSET-237',
#                      standard_var = 'Price', standard_method = 'min')


sum(df20$Price)
sum(df21$Price)
sum(df22$Price)
sum(df23$Price)
#sum(df24$Price)



