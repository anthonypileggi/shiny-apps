library(rvest)
library(xml2)
library(magrittr)


# FUNCTIONS ===================================================

# get sputnik articles by date
get_sputnik_articles_by_date <- function(page, date) {
  url <- paste0("https://tr.sputniknews.com/", page, format(date, "/%Y%m%d"))
  x <- xml2::read_html(url)
  
   #href
  href <- x %>%
    rvest::html_nodes(".b-plainlist__title a") %>%
    rvest::html_attr("href") %>%
    paste0("https://tr.sputniknews.com", .)
  
  # date
  date <- x %>%
    rvest::html_nodes(".b-plainlist__date") %>%
    rvest::html_text() %>%
    lubridate::as_datetime(format = "%H:%M %d.%m.%Y", tz = "Turkey") %>%
    lubridate::with_tz(tzone = "EST")
  
  # title
  title <- x %>%
    rvest::html_nodes(".b-plainlist__title a") %>%
    rvest::html_text()
  
  # lead
  lead <- x %>%
    rvest::html_nodes(".b-plainlist__announce a") %>%
    rvest::html_text()
  
  # img 
  img <- x %>%
    rvest::html_nodes(".b-plainlist__img img") %>% 
    rvest::html_attr("src")
  
  dplyr::tibble(
    date = date,
    page = page,
    url = href,
    title = title,
    lead = lead,
    img = img
  )
}

# Get a bunch of sputnik article urls
get_sputnik_articles <- function(pages = c("haberler", "politika")) {
  purrr::map_df(
    pages,
    function(page) {
      url <- paste0("https://tr.sputniknews.com/", page)
      x <- xml2::read_html(url)
      
      # href
      href <- x %>%
        rvest::html_nodes(".b-stories__info-link") %>%
        rvest::html_attr("href") %>%
        paste0("https://tr.sputniknews.com", .)
      
      # title
      title <- x %>%
        rvest::html_nodes(".b-stories__title a") %>%
        rvest::html_text()
      
      # lead
      lead <- x %>% 
        rvest::html_nodes(".b-stories p") %>%
        rvest::html_text()
        
      # date
      date <- x %>%
        rvest::html_nodes(".b-stories__date") %>%
        rvest::html_text()
      
      # more link
      more <- x %>%
        rvest::html_nodes(".m-more") %>% 
        rvest::html_attr("data-href") %>% 
        paste0("https://tr.sputniknews.com", .)
      
      dplyr::tibble(page = page, title = title, lead = lead, date = date, url = href)
    }
  )
}

# Scrape content from a single article on sputnik
scrape_sputnik_article <- function(url) {
  
  # - read article html
  x1 <- xml2::read_html(url)
  
  # text content
  text <- x1 %>%
    rvest::html_nodes(".b-article__text") %>%
    html_text()
  
  # raw html article
  raw <- x1 %>%
    rvest::html_nodes(".b-article__text") %>%
    html_text()
  
  dplyr::tibble(raw = raw, text = text)
}


# Filter sputnik articles to only those w/ any of the search terms
filter_sputnik_articles <- function(df, terms) {
  
  search_terms <- paste(tolower(terms), collapse = "|")
  
  df %>%
    dplyr::filter(
      stringr::str_detect(title, stringr::regex(search_terms, ignore_case = T)) |
        stringr::str_detect(lead, stringr::regex(search_terms, ignore_case = T)) |
        stringr::str_detect(text, stringr::regex(search_terms, ignore_case = T))
    ) %>%
    dplyr::mutate(
      found_title = purrr::map(title, ~terms[stringr::str_detect(.x, stringr::regex(terms, ignore_case = T))]),
      found_lead = purrr::map(lead, ~terms[stringr::str_detect(.x, stringr::regex(terms, ignore_case = T))]),
      found_text = purrr::map(text, ~terms[stringr::str_detect(.x, stringr::regex(terms, ignore_case = T))]),
      found = purrr::pmap_chr(list(found_title, found_lead, found_text), function(a, b, c) paste(unique(c(a, b, c)), collapse = ",")),
      count = purrr::map_int(paste(title, lead, text), ~stringr::str_count(tolower(.x), stringr::regex(search_terms, ignore_case = T)))
    ) %>%
    dplyr::select(-found_title, -found_lead, -found_text)
}
