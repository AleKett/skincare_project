
library(needs)
needs(httr, dplyr, rvest, stringr, lubridate, ggplot2, tidyr, purrr, RSelenium, magrittr, wdman, RCurl)

####################### get all links #######################
page_nrs <- 2:298
build_link <- function(page_nr) { 
  return(paste0('https://www.douglas.de/de/c/gesicht/12?page=', 
                page_nr))
}
pages <- lapply(page_nrs, build_link)

# add first page
pages <- append(pages, 'https://www.douglas.de/de/c/gesicht/12')


## get links from pages
get_links <- function(url) {
  product_links <- read_html(url) %>% 
    html_elements("#productlisting > div:nth-child(n) > div > div > div > div:nth-child(n) > div > a") %>%
    html_attr('href') %>% 
    as.character() %>% str_trim()
  return(product_links)
}

all_links <- lapply(pages, get_links) %>% unlist() %>% paste0('https://www.douglas.de', .) 

all_links_new <- all_links

load('input/douglas_links.RData')
all_links <- setdiff(all_links_new, all_links)

all_links %<>% as.data.frame() %>% set_colnames('url')

##to be continued
all_links %<>%  filter(url.exists(url))

save(all_links, file = 'douglas_links.RData') 


####################### product information ###################################
load('douglas_links.RData')

## function for different information
## procuct name

start <- Sys.time()

get_product_name <- function(url) {
  if(!url.exists(url)) {return(NA)}
  else {
    html <- read_html(url)
  tmp_name_part1 <- html %>%
    html_elements('div.row.product-detail-component > div > div:nth-child(1) > div:nth-child(2) > div > div:nth-child(1) > header > div > h1 > div.second-line > a')%>%
    html_text() %>% 
    as.character() %>% str_trim()
  if(length(tmp_name_part1) > 1) {tmp_name_part1 <- paste0(tmp_name_part1[1], tmp_name_part1[2])}
  tmp_name_part2 <- html %>% 
    html_elements('div.row.product-detail-component > div > div:nth-child(1) > div:nth-child(2) > div > div:nth-child(1) > header > div > h1 > div.second-line > span') %>% 
    html_text() %>% 
    str_remove_all('\n') %>% as.character() %>% str_trim()
  tmp_name <- paste0(tmp_name_part1, ' ', tmp_name_part2) %>% str_trim()
  return(tmp_name)
  }
}

all_links %<>% rowwise() %>% mutate(product_name = get_product_name(url))

end <- Sys.time()
Sys.time()
end - start

## procuct brand

get_brand <- function(url) {
  if(!url.exists(url)) {return(NA)}
  else {
  tmp_brand <- read_html(url) %>% html_elements('div.row.product-detail-component > div > div:nth-child(1) > div:nth-child(2) > div > div:nth-child(1) > header > div > h1 > div.first-line > a > span') %>%
    html_text() %>% 
    str_remove_all('\n') %>% as.character() %>% str_trim()
  return(tmp_brand[1])
  }
}

start <- Sys.time()

all_links %<>% rowwise() %>% mutate(brand_name = get_brand(url))

end <- Sys.time()
Sys.time()
end - start

## product price
get_price <- function(url) {
  if(!url.exists(url)) {return(NA)}
  else {
  tmp_price <- read_html(url) %>% 
    html_elements('div.product-price.product-price--maxWidth') %>%
    html_text() %>% 
    str_remove_all('\n| |€') %>% as.character() %>% 
    str_replace_all('\\,', '\\.') %>% str_trim()
  if (grepl('[a-zA-Z]', tmp_price)) {
    tmp_price <- readr::parse_number(str_split(tmp_price, '\\s')[[1]][1]) %>% as.numeric()
    tmp_price <- tmp_price[1]
  }
  else {
   tmp_price <- as.numeric(tmp_price[1]) 
  }
  return(tmp_price)
  }
}

start <- Sys.time() 

all_links %<>% rowwise() %>% mutate(price = tryCatch(get_price(url), 
                                                     error = function(e) {return(NA)})) 

end <- Sys.time()
Sys.time()
end - start

## capacity

get_unit <- function(url) {
  if(!url.exists(url)) {return(NA)}
  else {
  tmp_unit <- read_html(url) %>% html_elements('div.row.product-detail-component > div > div:nth-child(1) > div:nth-child(2) > div > div:nth-child(2) > div > div > div > div:nth-child(1) > div.product-detail__variant-name') %>%
    html_text() %>%  str_remove_all('\n| |€') %>%  as.character()
  if(length(tmp_unit) == 0) {return(NA)}
  else { return(tmp_unit) }
  }
}

start <- Sys.time() 

all_links %<>% rowwise() %>% mutate(unit = tryCatch(get_unit(url), 
                                          error = function(e) {return(NA)})) 

end <- Sys.time()
Sys.time()
end - start

## price per litre
get_price_base <- function(url) {
  if(!url.exists(url)) {return(NA)}
  else {
  tmp_price_base <- read_html(url) %>% html_elements('div.row.product-detail-component > div > div:nth-child(1) > div:nth-child(2) > div > div:nth-child(2) > div > div > div > div:nth-child(1) > div.product-detail__variant-row--left-content > div > span') %>%
    html_text() %>% 
    str_remove_all('\n| ') %>% as.character()
  tmp_price_base <- tmp_price_base[1]
  if(length(tmp_price_base) == 0)  { tmp_ing <- NA }
  # else if(tmp_price_base == "") { tmp_price_base <- NA }
  return(tmp_price_base)
}
}

start <- Sys.time()

all_links %<>% rowwise() %>% mutate(price_base = tryCatch(get_price_base(url), 
                                          error = function(e) {return(NA)})) 

end <- Sys.time()
Sys.time()
end - start

##Ingredients

rD <- rsDriver(browser = "firefox", chromever = NULL, port = 12345L)
remDr <- rD[["client"]]
#remDr$close()

all_links %<>% filter(!url %in% 
                        c('https://www.douglas.de/de/p/3001050033', 
                          'https://www.douglas.de/de/p/3001043628', 
                          'https://www.douglas.de/de/p/3001043637',
                          'https://www.douglas.de/de/p/3001043631',
                          'https://www.douglas.de/de/p/5002821012')) 

get_ingredients <- function(url) {
  
  tryCatch(url %>% read_html(), 
           error = function(e) {return(NA)})

  remDr$navigate(url)
  print(url)
  Sys.sleep(10)
  elem <- remDr$findElement(using = 'xpath', '//*[@id="tab:r0:1"]')
  if (elem$getElementText()[[1]] == 'INHALTSSTOFFE') {
  elem$clickElement()
  tmp_ing_html <- remDr$findElement(using = 'css selector', '.product-detail-other-info__html')
  tmp_ing <- tmp_ing_html$getElementText()[[1]][1]
  return(tmp_ing) }
  else if (elem$getElementText()[[1]] != 'INHALTSSTOFFE') {
    elem <- remDr$findElement(using = 'xpath', '//*[@id="tab:r0:2"]')
    elem$clickElement()
    tmp_ing_html <- remDr$findElement(using = 'css selector', '.product-detail-other-info__html')
    tmp_ing <- tmp_ing_html$getElementText()[[1]][1]
    return(tmp_ing) 
  }
}

start <- Sys.time()

#achtung: immer einmal pop ups wegklicken! 

all_links_head <- all_links %>% head(500) %>%  rowwise() %>% mutate(ingredients = tryCatch(list(get_ingredients(url)), 
                                                                  error = function(e) {return(NA)})) 

# hier: alle, die NA sind, nochmal mit anderen Selectors versuchen

end <- Sys.time()
Sys.time()
end - start

all_links_head_all <- all_links %>%   rowwise() %>% mutate(ingredients = tryCatch(list(get_ingredients(url)), 
                                                                                                          error = function(e) {return(NA)})) 
all_links$ingredients <- ifelse(all_links$ingredients == "NULL" | 
                                  all_links$ingredients == "", NA, 
                                all_links$ingredients)

all_links %<>% rowwise() %>% mutate(ingredients = ifelse(is.na(ingredients),"",ingredients)) %>%
  mutate(ingredients = list(tolower(ingredients %>% str_remove_all('.*(?=INGREDIENTS:)') %>% str_remove_all('.*INGREDIENTS: ') 
                                    %>% str_split('\\, |\\ - |\\ – |\\ • |\\||\\ · |(?<=\\D),'))))

douglas_df <- all_links


load('input/douglas_products.RData')
douglas_df <- bind_rows(douglas_df, new_links)
save(douglas_df, file = 'input/douglas_products.RData') 

