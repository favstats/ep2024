library(data.table)



calc_targeting <- function(only_tags, exclude = NULL) {
  
  if(sets$cntry=="TW"){
    age_limit <- 46
  } else {
    age_limit <- 48
  }
  
  # only_tags <- election_dat30 %>%
  # mutate(total_spend = total_spend_formatted) %>%
  #   filter(main_currency == the_currency)
  # only_tags <- election_dat30  %>%
  #   # left_join(all_dat) %>%
  #   # rename(internal_id = page_id) %>%
  #   filter(party != "And")  %>%
  #   filter(is.na(no_data)) %>%
  #   mutate(party = ifelse(party %in% c("GroenLinks", "PvdA"), "GroenLinks-PvdA", party)) %>%
  #   mutate(total_spend = total_spend_formatted) %>%
  #   filter(is.na(no_data)) %>%
  #   filter(page_name == "Partij voor de Dieren Gemeente Groningen")
  # filter(party == "Volt Nederland")
  
  total_sppppeen <- only_tags %>%
    distinct(internal_id, .keep_all = T)  %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
    select(internal_id, total_spend, total_num_ads) %>%
    arrange(desc(total_spend)) %>%
    summarize(total_spend = sum(total_spend),
              num_ads = sum(total_num_ads))
  
  if(!is.null(exclude)){
    if(exclude){
      only_tags <- only_tags %>% filter(is_exclusion)
    } else if(!exclude){
      only_tags <- only_tags %>% filter(!is_exclusion)      
    }
  }
  
  
  howmuchisinterest <- only_tags %>%
    filter(type == "detailed") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    arrange(desc(spend_per)) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    mutate(target = "interest")
  
  howmuchislocation <- only_tags %>%
    filter(type == "location") %>%
    group_by(internal_id, location_type) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, location_type, num_ads) %>%
    arrange(desc(spend_per)) %>%
    group_by(location_type) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    rename(target = location_type)
  
  # only_tags <- only_tags %>% filter(party == "Volt Nederland")
  howmuchisage <- only_tags %>%
    filter(type == "age") %>%
    filter(total_spend_pct != 0) %>%
    group_by(internal_id) %>%
    mutate(n_ages = n()) %>% #count(n_ages, sort = T)
    ungroup() %>%
    mutate(spending_age = sum(total_spend_pct)) 
  
  if(nrow(howmuchisage)==0){
    howmuchisage <- tibble(spend_per = 0, ads_per = 0,  target = "age")
  } else if(howmuchisage %>% slice(1) %>% pull(spending_age) >= age_limit){
    howmuchisage <- tibble(spend_per = 0, ads_per = 0,  target = "age")
  } else if (nrow(howmuchisage)<age_limit) {
    
    howmuchisage <- howmuchisage %>% mutate(spend_per = total_spend, ads_per = total_num_ads, target = "age") %>% select(spend_per, target, ads_per) %>% slice(1)
    
    
    
    ## TODO: BUT WHYYYYY?
  } else if (!all(howmuchisage$total_spend_pct==1)){
    howmuchisage <- howmuchisage %>% 
      filter(n_ages == 48) %>%
      group_by(internal_id) %>%
      filter(total_spend_pct == min(total_spend_pct)) %>%
      slice(1) %>%
      ungroup() %>%
      # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
      mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
      mutate(spend_per = total_spend * (1-total_spend_pct)) %>%
      mutate(ads_per = total_sppppeen$num_ads-num_ads) %>% 
      select(internal_id, spend_per, ads_per) %>% 
      ### TODO: this is new
      bind_rows(howmuchisage %>% 
                  filter(n_ages <= 47) %>%
                  distinct(internal_id, .keep_all = T) %>% 
                  mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
                  mutate(spend_per = total_spend) %>%
                  select(internal_id, spend_per, num_ads) %>%
                  summarize(spend_per = sum(spend_per),
                            ads_per = sum(num_ads)) %>%
                  mutate(target = "age"))  %>% 
      ### TODO: this is new
      bind_rows(howmuchisage %>% 
                  filter(n_ages > 48) %>%
                  distinct(internal_id, .keep_all = T) %>% 
                  mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
                  mutate(spend_per = 0) %>%
                  mutate(num_ads = 0) %>%
                  select(internal_id, spend_per, num_ads) %>%
                  summarize(spend_per = sum(spend_per),
                            ads_per = sum(num_ads)) %>%
                  mutate(target = "age")) %>% 
      summarize(spend_per = sum(spend_per),
                ads_per = sum(ads_per)) %>%
      mutate(target = "age") 
  } else if (all(howmuchisage$total_spend_pct==1)){
    
    howmuchisage <- howmuchisage %>% mutate(spend_per = total_spend, ads_per = total_num_ads, target = "age") %>% select(spend_per, target, ads_per) %>% slice(1)
    
  } else {
    
    howmuchisage <- tibble(spend_per = 0, ads_per = 0, target = "age")
    
  }
  
  
  
  
  
  # howmuchisgender <- only_tags %>%
  #     filter(type == "gender") %>%
  #     filter(total_spend_pct != 0) %>%
  #     filter(value != "All") %>%
  #     # group_by(internal_id) %>%
  #     # summarize()
  #     # # filter(total_spend_pct == max(total_spend_pct)) %>%
  #     # slice(1) %>%
  #     # ungroup() %>%
  #     # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
  #     mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
  #     mutate(spend_per = total_spend * total_spend_pct) %>%
  #     select(internal_id, spend_per) %>%
  #     summarize(spend_per = sum(spend_per))  %>%
  #     mutate(target = "gender")
  
  howmuchisgender <- only_tags %>%
    filter(type == "gender") %>%
    filter(value != "All") %>%
    group_by(internal_id, value) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, value, num_ads) %>%
    arrange(desc(spend_per)) %>%
    group_by(value) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    ungroup() %>% 
    mutate(target = paste0("Gender: ", value)) %>% 
    select(-value)
  
  howmuchcustom <- only_tags %>%
    filter(type == "custom_audience") %>%
    filter(total_spend_pct != 0) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    mutate(target = "custom_audience")
  
  
  howmuchlookalike <- only_tags %>%
    filter(type == "lookalike_audience") %>%
    filter(total_spend_pct != 0) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%    
    mutate(target = "lookalike_audience")
  
  howmuchlanguage <- only_tags %>%
    filter(type == "language") %>%
    filter(total_spend_pct != 0) %>%
    drop_na(value) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    mutate(target = "language")
  
  targeting_on_each <- howmuchisinterest %>%
    bind_rows(howmuchislocation) %>%
    bind_rows(howmuchisage) %>%
    bind_rows(howmuchisgender) %>%
    bind_rows(howmuchcustom) %>%
    bind_rows(howmuchlookalike) %>%
    bind_rows(howmuchlanguage) %>%
    mutate(total = total_sppppeen$total_spend) %>%
    mutate(total_ads = total_sppppeen$num_ads) %>%
    mutate(perc = spend_per/total*100) %>%
    mutate(perc_ads = ads_per/total_ads*100) %>%
    arrange(desc(perc))
  
  return(targeting_on_each)
}




relationshipstuff <- "Widowed|Recently moved|Away|[r|R]elationship|Parents|Partner|Separated|Divorced|Single|Complicated|Married|Engaged|Newlywed|Civil Union|Unspecified|Newly engaged"


add_ribbons <- function(x, adv, col) {
  x %>%
    tab_options(table.width = pct(100)) %>%
    tab_style(
      style = cell_borders(
        sides = c("left"),
        color = col,
        weight = px(18.5),
        style = "solid"
      ),
      locations = cells_body(
        columns = `Number of Advertisers`,
        rows = adv
      ))
}




get_targeting <- function(id, timeframe = "LAST_30_DAYS", lang = "en-GB") {
  
  url <- "https://www.facebook.com/api/graphql/"
  
  heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                Accept = "*/*",
                                `Accept-Language` = paste0(lang, ',', stringr::str_split(lang, "-") %>% unlist() %>% .[1],';q=0.5'),
                                `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                `X-FB-LSD`= "AVrNiQCSUnA",
                                `Alt-Used`= "www.facebook.com",
                                `Sec-Fetch-Dest`= "empty",
                                `Sec-Fetch-Mode`= "cors",
                                `Sec-Fetch-Site`= "same-origin",
                                # `Accept-Encoding` = "gzip, deflate, br",
                                `Content-Type` = "application/x-www-form-urlencoded",
                                Connection = "keep-alive"
  )
  
  
  if(timeframe == "LAST_30_DAYS"){
    
    # audienceTimeframe <- "%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22"
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8ixy7EiwvoWdwJwCwAwgU2lxS6Ehwem0nCqbwgE3awbG78b87C1xwEwgolzUO0n2US2G3i1ywa-2l0Fwwwi831wnFokwyx2cw8WfK6E5i3e4U3mxOu2S2W2K7o725U4q0HUkyE9E11EbodEGdw46wbLwiU8U6C2-&__csr=&__req=m&__hs=19237.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006139712&__s=ll61s1%3Axn89ey%3Admpplc&__hsi=7138774996758193009&__comet_req=0&lsd=AVrNiQCSYrc&jazoest=2981&__spin_r=1006139712&__spin_b=trunk&__spin_t=1662125577&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
  } else if (timeframe == "LAST_7_DAYS"){
    
    # audienceTimeframe <- "%7B%22"
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O0zE-Uqwl8cUjwdq79UbobEaUtws8nwhE2LxiawCw46wJwSyES0gq0K-1bwzwqobU&__csr=&__req=f&__hs=19245.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006179750&__s=njkc5w%3A6o847a%3A9gcoa8&__hsi=7141736891942848978&__comet_req=0&lsd=AVrbeuAiHJg&jazoest=21000&__spin_r=1006179750&__spin_b=trunk&__spin_t=1662815197&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_7_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
  } else if (timeframe == "LAST_90_DAYS"){
    
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O3u1mzXxG1kwPxe3C0D8sDwJwKwHxS1Mxu16wa-58G2q0gq2S3qazo11E2XU4K2e1FwLw8O2i&__csr=&__req=h&__hs=19301.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006553893&__s=20shv5%3A62a2bj%3A6goj90&__hsi=7162612241770415577&__comet_req=0&lsd=AVohzhTn68E&jazoest=2965&__spin_r=1006553893&__spin_b=trunk&__spin_t=1667675618&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_90_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
    url <- "https://www.facebook.com/api/graphql/"
    
    heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                  Accept = "*/*",
                                  `Accept-Language` = paste0(lang, ',', stringr::str_split(lang, "-") %>% unlist() %>% .[1],';q=0.5'),
                                  `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                  `X-FB-LSD`= "AVrNiQCSUnA",
                                  `Alt-Used`= "www.facebook.com",
                                  `Sec-Fetch-Dest`= "empty",
                                  `Sec-Fetch-Mode`= "cors",
                                  `Sec-Fetch-Site`= "same-origin",
                                  # `Accept-Encoding` = "gzip, deflate, br",
                                  `Content-Type` = "application/x-www-form-urlencoded",
                                  Connection = "keep-alive"
    )
    
  }
  
  
  
  
  
  posted = httr::POST(url, heads_up, body = da_body)
  
  contentwise <- httr::content(posted)
  
  rate_limit <- str_detect(as.character(contentwise), "Rate limit exceeded")
  if(rate_limit){
    stop(as.character(contentwise))
  }
  
  
  
  out_raw <- contentwise %>%
    rvest::html_nodes("body") %>%
    rvest::html_nodes("p") %>%
    as.character() %>% str_remove_all("</p>|<p>") %>%
    jsonlite::fromJSON()  %>%
    purrr::pluck("data") %>%
    purrr::pluck("page") %>%
    purrr::pluck("ad_library_page_targeting_insight")
  
  
  summary_dat <- out_raw %>%
    purrr::pluck("ad_library_page_targeting_summary") %>%
    dplyr::bind_rows()
  
  if(nrow(summary_dat) > 1){
    
    summary_dat <- summary_dat %>%
      dplyr::slice(which(summary_dat$detailed_spend$currency == summary_dat$main_currency)) %>%
      dplyr::select(-detailed_spend)
    
  }
  
  targeting_details_raw <- out_raw[!(names(out_raw) %in% c("ad_library_page_targeting_summary", "ad_library_page_has_siep_ads"))]
  
  # names(targeting_details_raw)
  
  res <- targeting_details_raw %>%
    purrr::discard(purrr::is_empty) %>%
    purrr::imap_dfr(~{.x %>% dplyr::mutate(type = .y %>% stringr::str_remove("ad_library_page_targeting_"))}) %>%
    dplyr::bind_cols(summary_dat) %>%
    dplyr::mutate(internal_id = id)
  
  return(res)
  
}

get_targeting <- suppressWarnings(get_targeting)

append_date_suffix <- function(dates){
  dayy <- lubridate::day(dates)
  suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                    dayy %% 10 == 1 ~ 'st',
                    dayy %% 10 == 2 ~ 'nd',
                    dayy %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(dayy, suff)
}

create_date <- function(x) {
  the_date <- format(x, "%b %d")
  the_date <- ifelse(str_detect(the_date, " 0"),
                     str_remove(the_date, "0"),
                     the_date)
  str_replace(the_date, 
              as.character(lubridate::day(x)), 
              append_date_suffix(x))
}


scale_fill_parties <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}
scale_color_parties <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}


walk_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::walk(.x, f, ...)
}

map_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map(.x, f, ...)
}

map_dfr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_dfr(.x, f, ...)
}

map_chr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_chr(.x, f, ...)
}


