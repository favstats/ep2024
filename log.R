pacman::p_load(knitr, tidyverse, openxlsx, sf, rmarkdown, rvest)


rmarkdown::render("logs/log.Rmd")
# dir.create(glue::glue("docs/{sets$cntry}"), recursive = T)
file.copy(from = "logs/log.html", to = glue::glue("docs/log.html"), overwrite = T, recursive = T)

unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)
