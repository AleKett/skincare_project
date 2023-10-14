# dm scraping

library(needs)
needs(httr, dplyr, rvest, stringr, lubridate, ggplot2, tidyr, purrr, RSelenium, magrittr, wdman, RCurl)

####################### Links für die Produkte bekommen #######################
                      
rD <- rsDriver(browser = "firefox", chromever = NULL, port = 7890L)
remDr <- rD[["client"]]

remDr$navigate('https://www.dm.de/pflege-und-parfum/gesicht')

while (T) {
  elem <- remDr$findElement(using = 'css selector', '#load-more-products-button')
  elem$highlightElement()
  elem$clickElement()
  Sys.sleep(3)}

html <- remDr$getPageSource()[[1]] %>% read_html()
link_list <- html %>% # parse HTML
  html_elements("div:nth-child(1) > div:nth-child(2) > div:nth-child(2) > a:nth-child(2)")
link_list %<>% html_attr('href')

link_list %<>% as.data.frame() %>% set_colnames('url') %>% 
  rowwise() %>% mutate(url = paste0('https://www.dm.de', url))

save(link_list, file = 'input/dm_links.RData')

################### Infos über Produkte aus den Links scrapen ##################

# ab hier ausführen, wenn schon mit gescrapeten links gestartet:

link_list_new <- link_list
load('input/dm_links.RData')
link_list_new <- setdiff(link_list_new, link_list)
link_list <- link_list_new

#nochmal Scraper starten wenn notwendig
#rD <- rsDriver(browser = "firefox", chromever = NULL, port = 48L)
#remDr <- rD[["client"]]

################################################
#Coherence check
ing_coherence <- function(ingredients) {
  if(!is.na(ingredients)) {
    if(ingredients$getElementText()[[1]][1] == '') {
      ingredients <- NA }}
  
  if(!is.na(ingredients)) {
    if(ingredients$getElementText()[[1]][1] != 'Inhaltsstoffe') {
      ingredients <- NA }}
  
  if(!is.na(ingredients)) {
    if(ingredients$getElementText()[[1]][1] == 'Warnhinweise') {
      ingredients <- NA }}
  
  if(!is.na(ingredients)) {
    if(ingredients$getElementText()[[1]][1] == 'Anschrift des Unternehmens') {
      ingredients <- NA }}
}


get_product <- function(nr) {
  
  remDr$navigate(link_list$url[nr])
  Sys.sleep(6)
  
  ingredients <- tryCatch(remDr$findElement(using = 'xpath', '/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div/div/div/div[2]/div/div[4]/button'),
                          error = function(e) {ingredients <- NA })
  
  suppressWarnings(ing_coherence(ingredients))
  
  if(is.na(ingredients)) {
    ingredients <- tryCatch(remDr$findElement(using = 'xpath', '/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div/div/div/div[2]/div/div[3]/button'),
                            error = function(e) {ingredients <- NA })}

  suppressWarnings(ing_coherence(ingredients))
  
  if(is.na(ingredients)) {
    ingredients <- tryCatch(remDr$findElement(using = 'xpath', '/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div/div/div/div[2]/div/div[2]/button'),
                            error = function(e) {ingredients <- NA })}
  
  suppressWarnings(ing_coherence(ingredients))
  
  if(is.na(ingredients)) {
    ingredients <- tryCatch(remDr$findElement(using = 'xpath', '/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div/div/div/div[2]/div/div[1]/button'),
                            error = function(e) {ingredients <- NA })}
  
  suppressWarnings(ing_coherence(ingredients))
  
  if(is.na(ingredients)) {
    ingredients <- tryCatch(remDr$findElement(using = 'xpath', '/html/body/div[2]/div/main/div[2]/div[2]/div[1]/div/div/div/div/div[2]/div/div[4]/div/div/div/div/p'),
                            error = function(e) {ingredients <- NA })}
  
  suppressWarnings(ing_coherence(ingredients))
  
  if(is.na(ingredients)) {
    ingredients <- tryCatch(remDr$findElement(using = 'xpath', '/html/body/div[1]/div/main/div[2]/div/div[2]/div[1]/div/div/div/div/div[2]/div[2]/div[5]/button'),
                            error = function(e) {ingredients <- NA })}
  
  suppressWarnings(ing_coherence(ingredients))
  
  print('coherence check done')
  
  if(!is.na(ingredients)) {
    ingredients$clickElement()
    Sys.sleep(2) }
  
  html <- remDr$getPageSource()[[1]]
  
  html %<>% read_html() 
  name <- html %>%  rvest::html_elements('h1') %>% html_text()
  brand <- html %>%  rvest::html_elements('h1') %>% html_elements('a') %>% html_text()
  price <- html %>% html_nodes(xpath = '/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div/div/div/div[1]/div[2]/div[2]/div/div[1]/div[1]/div[1]/div/span') %>% 
    html_text()
  price_base <- html %>% html_nodes(xpath = '/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div/div/div/div[1]/div[2]/div[2]/div/div[1]/div[1]/div[2]') %>% 
    html_text()
  unit <- str_remove_all(price_base, '\\(.*\\)')
  price_base <- str_extract(price_base, '\\(.*\\)')
  price_base <- str_remove_all(unit, '\\(|\\)')
  
  
  if(!is.na(ingredients)) {
    ingredients <- html %>% html_nodes(xpath = '/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div/div/div/div[2]/div/div[4]/div/div/div/div/p') %>% 
      html_text()
    if(identical(ingredients, character(0))) {
      ingredients <- html %>% html_nodes(xpath = '/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div/div/div/div[2]/div/div[5]/div/div/div/div/p') %>% 
        html_text()
    }
    
    if(identical(ingredients, character(0))) {
      ingredients <- html %>% html_nodes(xpath = '/html/body/div[1]/div/main/div[2]/div/div[2]/div[1]/div/div/div/div/div[2]/div[2]/div[3]/div/div/div/div/p') %>% 
        html_text()
    }
    
    if(identical(ingredients, character(0))) {
      ingredients <- html %>% html_nodes(xpath = '/html/body/div[1]/div/main/div[2]/div/div[2]/div[1]/div/div/div/div/div[2]/div[2]/div[2]/div/div') %>% 
        html_text()
    }
    
    if(length(ingredients) == 2) {
      ingredients <- html %>% html_nodes(xpath = '/html/body/div[1]/div/main/div[2]/div/div[2]/div[1]/div/div/div/div/div[2]/div[2]/div[6]/div/div/div/div/p') %>% 
        html_text()
    } }
  
  print('finished product ')
  print(nr)
  print(name)
  Sys.time()
  
  this_product <- c("name" = name, "brand" = brand, "price" = price, "price_base" = price_base, 
                    "unit" = unit, "ingredients" = ingredients[1]) 
  
  return(this_product)
}
  
dm_df_add <- map_df(1:nrow(link_list), get_product)
dm_2 <- dm_df_add

#hier nur die speichern, wo tatsächlich inhaltsstoffe drin sind

load('input/dm_products.RData')

dm_df <- bind_rows(dm_df, dm_df_add)

dm_df_add %<>% mutate(price = str_remove_all(price, '\n| |€')) %>% 
  mutate(price = str_replace_all(price, '\\,', '\\.')) %>% 
  mutate(price = as.double(price))
  
dm_df_reduced <- dm_df %>% mutate(ingredients = tolower(ingredients)) %>% 
  filter(grepl('aqua|glycerin|wasser|oil|acid|petrolatum|alcohol', ingredients))


save(dm_df, dm_df_reduced, file = 'input/dm_products.RData')

