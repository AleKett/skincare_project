library(needs)
needs(httr, dplyr, rvest, stringr, lubridate, ggplot2, tidyr, purrr, RSelenium, magrittr)

## get links of products
page_nrs <- 1:16
build_link <- function(page_nr) { 
  return(paste0('https://www.rossmann.de/de/pflege-und-duft/gesichtspflege/c/olcat2_2?q=%3Arelevance%3A&page=', # für Baby-Kosmetik https://www.rossmann.de/de/baby-und-spielzeug/babypflege/c/olcat2_12?q=%3Arelevance%3A&page=
                page_nr,
                '&pageSize=60#'))
}

pages <- lapply(page_nrs, build_link)

# add first page
pages <- append(pages, 'https://www.rossmann.de/de/pflege-und-duft/gesichtspflege/c/olcat2_2') # für Baby-Kosmetik https://www.rossmann.de/de/baby-und-spielzeug/babypflege/c/olcat2_12

## get links from all pages
get_links <- function(url) {
  product_links <- read_html(url) %>% 
    html_elements('.rm-tile-product__wrapper--content') %>% html_elements('.rm-tile-product__wrapper--title') %>% 
    html_elements('a') %>% as.character() %>% 
    str_extract('\\".*\\\" (?=class)') %>% str_remove_all('\\"') %>% str_trim()
  return(product_links)
}

all_links <- lapply(pages, get_links) %>% unlist() %>% paste0('https://www.rossmann.de/de/pflege-und-duft/gesichtspflege/c/olcat2_2', .) # für Baby-Kosmetik https://www.rossmann.de/de/baby-und-spielzeug/babypflege/c/olcat2_12
save(all_links, file = 'rossmann_links.RData') # für Baby-Kosmetik rossmann_links_baby_kosmetik.RData


####################### product information ###################################

all_links %<>% as.data.frame() %>% set_colnames('url')

## functions for different product information 

## product name
start <- Sys.time()

get_product_name <- function(url) {
  tmp_name <- read_html(url) %>% html_elements('div.rm-product__title') %>% html_text() %>% 
    str_remove_all('\n') %>% as.character() %>% str_trim()
  return(tmp_name[1])
}
all_links %<>% rowwise() %>% mutate(product_name = get_product_name(url))
end <- Sys.time()
Sys.time()
end - start

## product brand
get_brand <- function(url) {
  tmp_brand <- read_html(url) %>% html_elements('div.rm-product__brand') %>% html_text() %>% 
    str_remove_all('\n') %>% as.character() %>% str_trim()
  return(tmp_brand[1])
}

start <- Sys.time()

all_links %<>% rowwise() %>% mutate(brand_name = get_brand(url))

end <- Sys.time()
Sys.time()
end - start

## product price
get_price <- function(link) {
  tmp_price <- link %>% read_html() %>% html_elements('div.rm-price__current') %>% html_text() %>% 
    str_remove_all('\n| |€') %>% as.double()
  return(tmp_price[1])
}

start <- Sys.time() 

all_links %<>% rowwise() %>% mutate(price = get_price(url))

end <- Sys.time()
Sys.time()
end - start

## product capacity

get_unit <- function(link) {
  tmp_unit <- link %>% read_html() %>% html_elements('div.rm-product__units') %>% html_text() %>% 
    str_remove_all('\n| ') %>% as.character()
  if(tmp_unit == "") { tmp_unit <- NA }
  return(tmp_unit)
}

start <- Sys.time() 

all_links %<>% rowwise() %>% mutate(unit = get_unit(url))

end <- Sys.time()
Sys.time()
end - start

## price per litre
get_price_base <- function(link) {
  tmp_price_base <- link %>% read_html() %>% html_elements('div.rm-price__base') %>% html_text() %>% 
    str_remove_all('\n| ') %>% as.character()
  tmp_price_base <- tmp_price_base[1]
  if(length(tmp_price_base) == 0)  { tmp_ing <- NA }
 # else if(tmp_price_base == "") { tmp_price_base <- NA }
  return(tmp_price_base)
}

start <- Sys.time()

all_links %<>% rowwise() %>% mutate(price_base = get_price_base(url))

end <- Sys.time()
Sys.time()
end - start

## ingredients

get_ingredients <- function(link) {
  tmp_ing <- link %>% read_html() %>% 
    html_elements('#product__info > div:nth-child(3)') %>% html_text() %>% 
    str_remove_all('\n|€|\t|\\.') %>%  
    as.character() 

 if(length(tmp_ing) == 0)  { tmp_ing <- NA }
  else if (!grepl('Inhaltsstoffe', tmp_ing)) { tmp_ing <- NA }
  else {
    tmp_ing %<>% str_remove('Inhaltsstoffe') %>% tolower() %>%  str_split('\\, |\\|\\•|\\·') %>% as.list()
  }
  return(tmp_ing)
}

start <- Sys.time()

all_links %<>% rowwise() %>% mutate(ingredients = list(get_ingredients(url))) 

end <- Sys.time()
Sys.time()
end - start

# save all links
df <- all_links
save(df, file = 'rossmann_products.RData') 
