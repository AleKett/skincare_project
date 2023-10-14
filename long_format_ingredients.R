## ingredients

#libraries
needs(stringdist, quanteda, tidyverse, text.alignment, magrittr, broom, MASS)

#load data
load('input/rossmann_products_new.RData')
#data cleaning rossmann and in long format
rossmann_df <- df
rm(df)
rossmann_df$shop <- 'rossmann'

rossmann_df %<>% filter(!is.na(ingredients)) 

rossmann_df %<>% mutate(ingredients = paste0(unlist(ingredients), collapse = ', ')) %>% 
  mutate(ingredients = ifelse(grepl(' • ', ingredients), str_split(ingredients, ' • '), str_split(ingredients, ',|;|  '))) 

rossmann_df_long <- unnest(rossmann_df, ingredients)  %>% 
  dplyr::select(shop, product_name, brand_name, price, unit, price_base, ingredients)

rossmann_df %<>% dplyr::select(shop, product_name, brand_name, price, unit, price_base, ingredients)

##########################
#data cleaning dm and in long format
load('input/dm_products.RData')
dm_df <-  dm_df_reduced
rm(dm_df_reduced)
dm_df$shop <- 'dm'

dm_df %<>% mutate(ingredients = str_replace_all(ingredients, '  | • |\\*| \\| ', ', ')) %>% 
  mutate(ingredients = str_remove_all(ingredients, 'ingredients: ')) 

dm_df %<>% 
  mutate(ingredients = str_split(ingredients, ', '))

dm_df_long <- unnest(dm_df, ingredients) %>%  
  dplyr::select(shop, product_name = name, brand_name = brand, price, unit, price_base, ingredients) %>% rowwise() %>% mutate(ingredients = list(ingredients), price = as.numeric(price))

dm_df_long %<>% mutate(ingredients = unlist(ingredients)) 

dm_df %<>% dplyr::select(shop, product_name = name, brand_name = brand, price, unit, price_base, ingredients) %>% rowwise() %>% mutate(ingredients = list(ingredients), price = as.numeric(price))

##########################
#data cleaning douglas and in long format
load('input/douglas_df_short.RData')
douglas_df$shop <- 'douglas'
douglas_df %<>% dplyr::select(shop, product_name, brand_name, price, unit, price_base, ingredients)

douglas_df$ingredients[douglas_df$ingredients == 'NULL'] <- NA
douglas_df %<>% filter(!is.na(ingredients)) 
douglas_df %<>% filter(ingredients != '')

douglas_df %<>% mutate(ingredients = str_remove_all(ingredients, '^c\\(|\\)$') %>% 
                         str_remove_all(., '\\"') %>% str_split(., ', ')) 

douglas_df_long <- unnest(douglas_df, ingredients)

##########################
# bind all data from different drugstores
df <- bind_rows(rossmann_df,
                dm_df, 
                douglas_df)

df_long <- bind_rows(rossmann_df_long, dm_df_long, douglas_df_long)


###########################
#more cleaning and find doubles and clean products with less then 30 characters
df_long %<>% filter(ingredients != '') %>% 
  mutate(ingredients = str_remove_all(ingredients, '\\(.*\\)|\\[.*\\]')) %>% 
  mutate(ingredients = str_trim(ingredients))

df_long %<>% mutate(ingredients = 
                      ifelse(ingredients %in% c('water', 'aqua', 'water \\/ aqua', 'aqua \\/ water', 
                             'water\\/aqua', 'aqua/water', 'aqua / water', 'aqua (water', 'wasser', 'aqua (water]', 
                             'eau', 'aqua/water/eau', 'water/aqua/eau', 'water\\aqua\\eau', 'water\\ aqua\\ eau'), 'aqua', ingredients))

df_long %<>% filter(nchar(ingredients) < 30) 
