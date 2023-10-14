## similarity calculation through all products ##

library(needs)
needs(stringdist, quanteda, tidyverse, text.alignment, magrittr, broom, MASS)

### function for similarity score, "smith_waterman" function
sw_def <- function(x, y) {
  tmp <- smith_waterman(x, 
                        y, 
                        type = 'words',
                        match = 5, 
                        mismatch = -1, 
                        gap = -1)
  similarity <- tmp$similarity
  def <- tmp$a$alignment$text
  if(is_empty(def)) {def <- NA}
  if(is_empty(similarity)) {similarity <- NA}
  return(paste0(def, ' tmp_value: ', similarity))
}

### load data and add column with store name, named "shop"

load('input/rossmann_products_new.RData')
rossmann_df <- df
rossmann_df$shop <- 'rossmann'
rossmann_df %<>% dplyr::select(shop, product_name, brand_name, price, unit, price_base, ingredients)

load('input/dm_products.RData')
dm_df <-  dm_df_reduced
dm_df$shop <- 'dm'
dm_df %<>% dplyr::select(shop, product_name = name, brand_name = brand, price, unit, price_base, ingredients) %>% rowwise() %>% mutate(ingredients = list(ingredients), price = as.numeric(price))

load('input/douglas_products.RData')
douglas_df$shop <- 'douglas'
douglas_df %<>% dplyr::select(shop, product_name, brand_name, price, unit, price_base, ingredients)

df <- bind_rows(rossmann_df,
                dm_df, 
                douglas_df)

# sort out face masks and gift baskets etc. of product list
df$id <- 1:nrow(df)
df %<>% mutate(ingredients = paste0(unlist(ingredients), collapse = ' '))
df %<>% filter(!is.na(ingredients)) 
df %<>% filter(ingredients != 'NA') 

df %<>% rowwise() %>% mutate(ingredients = str_remove(ingredients, '^c\\('))
df %<>% rowwise() %>% mutate(ingredients = str_remove(ingredients, '\\)$'))
df %<>% rowwise() %>% mutate(ingredients = str_remove_all(ingredients, '"'))

df %<>% filter(!grepl('Stück|Paar|Set', unit))
df %<>% filter(!grepl('Stück|Paar|Set', price_base))

#clean data
df %<>% rowwise() %>% 
  mutate(price_base_helper = ifelse(price_base == unit,
                                    str_replace_all(
                                      str_remove_all(price_base, ' l'), 
                                      ',', '.'), NA)) %>% 
  mutate(price_base_helper = 1/as.numeric(price_base_helper)) %>% 
  mutate(price_base = ifelse(is.na(price_base_helper), price_base, as.character(price * price_base_helper))) 

df %<>% mutate(ingredients = tolower(ingredients)) %>% 
  mutate(ingredients = str_replace_all(ingredients, 'water|wasser', 'aqua')) %>% 
  mutate(ingredients = str_replace_all(ingredients, '\\/\\(\\)', '')) %>% 
  mutate(ingredients = str_replace_all(ingredients, 'aquaqua|aqua aqua|aqua  aqua|aqua\\/aqua|aqua \\(aqua\\) |\\(aqua eau\\)|aqua \\/ aqua', 'aqua ')) %>% 
  mutate(ingredients = str_replace_all(ingredients, '  | • |\\*| \\| ', ' ')) %>% 
  mutate(ingredients = str_trim(ingredients))  %>% 
  mutate(ingredients = str_replace_all(ingredients, '  | • |\\*| \\; ', ' ')) %>% 
  mutate(ingredients = str_remove_all(ingredients, '\\(.*\\)')) %>% 
  mutate(ingredients = str_remove_all(ingredients, 'ingredients\\: |1 woche\\: |^bestandteile\\: |^ingredients')) %>% 
  mutate(ingredients = str_replace_all(ingredients, ' , |, |, ', ' '))

df %<>% filter(price_base != "") 

#Similarity Analyse
dfm <- dfm(tokens(df$ingredients)) %>% dfm_trim(min_termfreq = 10, min_docfreq = 5)
docvars(dfm, 'id') <- df$id

simil <- quanteda.textstats::textstat_simil(dfm, method = 'cosine', min_simil = 0.6) %>% 
  base::as.data.frame() %>% 
  mutate(id = readr::parse_number(as.character(document1))) %>% 
  mutate(id2 = readr::parse_number(as.character(document2))) 
simil %<>% 
  dplyr::select(id, id2, cosine) 

simil %<>% filter(cosine > 0.95) 

simil_top <-  simil %>% left_join(df %>% dplyr::select(id, product_name, brand_name, ingredients)) %>% 
  left_join(df %>% rename(id2 = id, product_name_2 = product_name, brand_name_2 = brand_name, ingredients_2 = ingredients) %>% 
              dplyr::select(id2, product_name_2, brand_name_2, ingredients_2)) %>% 
  filter(product_name != product_name_2) %>% filter(brand_name != brand_name_2) 

#Filter brands from famous Skincare-Influencer
simil_top %<>% mutate(brand_name = tolower(brand_name)) %>% 
  filter(brand_name %in% c('balea', 'isana', 'lavera', 'la roche-posay', 'nø cosmetics', 'eucerin', 'junglück', 'colibri skincare', "paula's choice", 'nivea', 'garnier', 'the ordinary')) %>% 
  rowwise() %>%  
  mutate(sw = sw_def(ingredients, ingredients_2)) %>% 
  mutate(sw_similarity = stringr::str_split(sw, ' tmp_value: ')[[1]][2]) %>% 
  mutate(sw_def = stringr::str_split(sw, ' tmp_value: ')[[1]][1]) %>% 
  dplyr::select(-sw) %>% 
  filter(nchar(sw_def) > 50) %>% 
  filter(nchar(ingredients) > 20) %>% 
  filter(nchar(ingredients_2) > 20)
simil_top %<>% filter(ingredients != "") 
simil_top %<>% filter(ingredients_2 != "") 

save(simil_top, file = 'input/similarity_calculations.RData')


