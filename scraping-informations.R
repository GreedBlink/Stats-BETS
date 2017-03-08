library(rvest)
library(stringr)
library(lubridate)
library(tidyverse)

cran_link <- function(...) {
  file.path("https://cran.rstudio.com/src/contrib", ...)
}

pkgs_raw <- read_html(cran_link()) %>% 
  html_nodes("table") %>% 
  .[[1]] %>%
  html_table()

pkgs_raw <- pkgs_raw[,-1]

pkgs <- pkgs_raw %>%
  filter(Size != "-",
         str_detect(Name, "tar.gz$")) %>%
  mutate(Date = dmy_hm(`Last modified`),
         Name = str_extract(Name, "^[^_]+(?=_)")) %>%
  select(-Size, -Description) %>%
  as_tibble()

pkgs


#------------------------------------------------------------------


archives_raw <- read_html(cran_link("Archive")) %>% 
  html_nodes("table") %>% 
  .[[1]] %>%
  html_table()

archives_raw <- archives_raw[,-1]

archives_processed <- archives_raw %>%
  filter(str_detect(Name, "/$")) %>%
  mutate(Date = dmy_hm(`Last modified`),
         Name = str_sub(Name, end = -2)) %>%
  select(-Size, -Description) %>%
  as_tibble()

#--------------------------------------------

read_page <- function(name) {
  message(name)
  read_html(cran_link("Archive", name)) %>% 
    html_nodes("td") %>% 
    html_text()
}

archives_scraped <- archives_processed %>%
  mutate(page = map(Name, read_page))


archives <- archives_scraped %>%
  mutate(Date = dmy_hm(map_chr(page, ~ .[8])),
         ArchivedVersions = map_dbl(page, ~ length(.) / 5 - 1)) %>%
  select(-page)




all_pkgs <- bind_rows(archives %>% 
                        anti_join(pkgs, by = "Name") %>% 
                        mutate(Archived = TRUE),
                      pkgs %>% 
                        anti_join(archives, by = "Name") %>% 
                        mutate(ArchivedVersions = 0,
                               Archived = FALSE),
                      archives %>%
                        semi_join(pkgs, by = "Name") %>%
                        mutate(Archived = FALSE)) %>%
  mutate(Versions = ifelse(Archived, ArchivedVersions, ArchivedVersions + 1)) %>%
  arrange(Name)

all_pkgs



all_pkgs %>%
  filter(!Archived) %>%
  group_by(Date = floor_date(Date, unit = "month")) %>%
  summarise(NewPackages = n()) %>%
  ungroup %>%
  mutate(TotalPackages = cumsum(NewPackages)) %>%
  ggplot(aes(Date, TotalPackages)) +
  geom_line(size = 1.5, alpha = 0.8, color = "midnightblue") +
  labs(x = NULL, y = "Number of available packages",
       title = "How many packages are available on CRAN?",
       subtitle = "Only packages that are still available")



all_pkgs %>%
  ggplot(aes(Archived)) +
  geom_histogram(stat = "count", alpha = 0.8, fill = "midnightblue") +
  scale_x_discrete(labels=c("Still available", "Archived, no longer available")) +
  labs(y = "Number of packages", x = NULL,
       title = "How many packages are no longer available on CRAN?",
       subtitle = "About 10% of total packages are no longer available")


all_pkgs %>%
  ggplot(aes(Versions)) +
  geom_histogram(binwidth = 10, alpha = 0.8, fill = "midnightblue") +
  labs(y = "Number of packages", x = "Number of versions on CRAN",
       title = "How many versions do CRAN packages have?",
       subtitle = "About 25% of packages are on their first version")