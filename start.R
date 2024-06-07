
pacman::p_load(knitr, tidyverse, openxlsx, sf, rmarkdown, rvest)
# setwd("C:/Users/fabio/Dropbox/postdoc/microdashboards/wtm_iq/")
# setwd("..")
# print(getwd())

# dir("../_site", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>% keep(~str_detect(.x, "qmd")) %>% walk(render_it)


# here::i_am("elex.Rproj")
# setwd("C:/Users/fabio/Dropbox/postdoc/elex/")
# source("cntry.R")
source("utils.R")

t1 <- Sys.time()



sets <- jsonlite::fromJSON("settings.json")


eu_countries <- c("EU","AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE")

# eu_countries %>% 
#   walk(~{
#     dir.create(glue::glue("docs/{.x}"))
  # })


full_cntry_list <- read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry) %>% 
  bind_rows(tibble(iso2c = "EU",
                   country = "European Union"), .)  %>% 
  filter(iso2c %in% eu_countries) %>% 
  # sample_n(n()) %>% 
  mutate(iso2c = fct_relevel(iso2c, eu_countries)) %>% 
  arrange(iso2c)# %>% 
  # filter(iso2c %in% c("DE"))

render_it <- function(...) {
  print(...)
  thefile <- str_remove(..., "_site/") %>% str_replace("qmd", "html")
  if(any(str_detect(..., "map"))){
    if(cntryy == "EU"){
      quarto::quarto_render(..., quiet = T)
    }
  } else {
    # print(thefile)
    quarto::quarto_render(..., quiet = T)
  }

}
render_it <- possibly(render_it, otherwise = NULL, quiet = F)

# cntryy <- "NL"
for (cntryy in full_cntry_list$iso2c) {
  # print(cntryy)

  t2 <- Sys.time()

  print(paste0(cntryy,": ", t2))


  try({


    time_difference_seconds <- difftime(t2, t1, units = "hours")


    if (as.numeric(time_difference_seconds) > 4) {
      if (Sys.info()[["sysname"]] != "Windows") {
        break
      }
    }



    sets$the_country <- full_cntry_list$country[which(full_cntry_list$iso2c==cntryy)]
    sets$cntry <- cntryy

    jsonlite::write_json(sets, "settings.json",  simplifyVector = TRUE)

    # Sys.sleep(5)






    # rstudioapi::jobRunScript("fbadlibrary.R")
    # try({




      # if(nrow(election_dat30)!=0){

        # Sys.sleep(60*7)
        # all_dat <- readRDS("data/all_dat.rds")

        # write_lines(nrow(distinct(election_dat30, internal_id)), file = "n_advertisers.txt")
        # render_it <- possibly(quarto::quarto_render, otherwise = NULL, quiet = F)
        dir("_site", full.names = T) %>%
          keep(~str_detect(.x, "qmd")) %>%
          # discard( ~ str_detect(.x, "map")) %>%
          fct_relevel("_site/map.qmd") %>%
          sort() %>%
          as.character() %>%
          # .[2] %>% 
          purrr::walk(render_it, .progress = T)

        dir("docs", full.names = T) %>%
          keep(~str_detect(.x, "site|files")) %>%
          walk(~fs::dir_copy(.x, str_replace(.x, "docs/", glue::glue("docs/{sets$cntry}/")), overwrite = T))

        dir("docs", full.names = T) %>%
          keep(~str_detect(.x, "html|json|logo")) %>%
          walk(~fs::file_copy(.x, str_replace(.x, "docs/", glue::glue("docs/{sets$cntry}/")), overwrite = T))

        # knitr::knit("README.Rmd")
        #
        # rmarkdown::render("logs/overview.Rmd")
        #
        # file.copy(from = "logs/overview.html", to = glue::glue("docs/{sets$cntry}/overview.html"), overwrite = T)

        unlink("node_modules", recursive = T, force = T)
        unlink("out", recursive = T, force = T)

        dir("docs", full.names = T) %>%
          keep(~str_detect(.x, "site|files")) %>%
          walk(fs::dir_delete)

        dir("docs", full.names = T) %>%
          keep(~str_detect(.x, "html|json")) %>%
          walk(fs::file_delete)

      # } #else {

      #   rmarkdown::render("logs/index.Rmd")
      #   dir.create(glue::glue("docs/{sets$cntry}"), recursive = T)
      #   file.copy(from = "logs/index.Rmd", to = glue::glue("docs/{sets$cntry}/index.html"), overwrite = T, recursive = T)
      #
      #   unlink("node_modules", recursive = T, force = T)
      #   unlink("out", recursive = T, force = T)
      #
      # }





    # })

    # dir("docs", full.names = T) %>%
    #   keep(~str_detect(.x, "site|files")) %>%
    #   walk(fs::dir_delete)
    #
    # dir("docs", full.names = T) %>%
    #   keep(~str_detect(.x, "html|json")) %>%
    #   walk(fs::file_delete)

  })



  # file.remove("_site/_quarto.yml")

  # rm(election_dat30)

}

rmarkdown::render("index.Rmd", output_file = "docs/index.html")

fs::file_copy("docs/EU/map.html", "docs/map.html")
fs::dir_copy("docs/EU/site_libs", "docs/site_libs", overwrite = T)

# dir.create(glue::glue("docs/{sets$cntry}"), recursive = T)
# file.copy(from = "index.html", to = glue::glue("docs/index.html"), overwrite = T)
# dir(full.names = F) %>%
#   keep(~str_detect(.x, "_libs")) %>%
#   walk(~fs::dir_copy(.x, "docs/site_libs", overwrite = T))


# setwd("C:/Users/favoo/Documents/ep2024")
full_cntry_list$iso2c %>%
  # .[1] %>% 
  walk_progress( ~ {
    try({
      
    city_name <- .x
    dir("docs/EU", full.names = T) %>%
      keep( ~ str_detect(.x, "map")) %>%
      walk( ~ fs::file_copy(.x, str_replace(
        .x, "docs/EU/", glue::glue("docs/{city_name}/")
      ), overwrite = T))
    
    dir("docs/EU", full.names = T) %>%
      keep( ~ str_detect(.x, "about")) %>%
      walk( ~ fs::file_copy(.x, str_replace(
        .x, "docs/EU/", glue::glue("docs/{city_name}/")
      ), overwrite = T))
    
    dir("docs/EU", full.names = T) %>%
      keep(~str_detect(.x, "site|files"))  %>%
      walk( ~ fs::dir_copy(.x, str_replace(
        .x, "docs/EU/", glue::glue("docs/{city_name}/")
      ), overwrite = T))
    
    })
  })

if (Sys.info()[["effective_user"]] == "fabio" | Sys.info()[["effective_user"]] == "favoo") {
  # system("git pull")
  gert::git_pull()
  # system("git add -A")
  # system('git commit -m "update"')
  # system("git push")
  gert::git_add(".")
  gert::git_commit("update")
  gert::git_push()
}
