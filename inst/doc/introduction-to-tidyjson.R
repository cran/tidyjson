## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(tidyjson)

## ---- message = FALSE---------------------------------------------------------
library(dplyr)

# Define a simple people JSON collection
people <- c('{"age": 32, "name": {"first": "Bob",   "last": "Smith"}}',
            '{"age": 54, "name": {"first": "Susan", "last": "Doe"}}',
            '{"age": 18, "name": {"first": "Ann",   "last": "Jones"}}')

# Tidy the JSON data
people %>% spread_all

## -----------------------------------------------------------------------------
worldbank %>% str

## -----------------------------------------------------------------------------
worldbank %>% spread_all

## -----------------------------------------------------------------------------
worldbank %>% spread_all %>% select(regionname, totalamt)

## ---- echo = FALSE, message = FALSE-------------------------------------------
options(tibble.print_min = 10L, tibble.print_max = 10L)

## -----------------------------------------------------------------------------
worldbank %>% gather_object %>% json_types %>% count(name, type)

## ---- echo = FALSE, message = FALSE-------------------------------------------
options(tibble.print_min = 4L, tibble.print_max = 4L)

## -----------------------------------------------------------------------------
worldbank %>% enter_object(majorsector_percent)

## -----------------------------------------------------------------------------
worldbank %>% enter_object(majorsector_percent) %>% gather_array

## -----------------------------------------------------------------------------
worldbank %>% 
  enter_object(majorsector_percent) %>% gather_array %>% spread_all

## -----------------------------------------------------------------------------
worldbank %>%
  spread_all %>% select(region = regionname, funding = totalamt) %>%
  enter_object(majorsector_percent) %>% gather_array %>% 
  spread_all %>% rename(sector = Name, percent = Percent) %>%
  group_by(region, sector) %>%
  summarize(funding = sum(funding * percent))

## -----------------------------------------------------------------------------
worldbank %>% spread_all %>% select(regionname, totalamt)

## -----------------------------------------------------------------------------
worldbank %>% gather_object %>% json_types %>% count(name, type)

## -----------------------------------------------------------------------------
worldbank %>% enter_object(majorsector_percent) %>% gather_array

## -----------------------------------------------------------------------------
companies[1] %>% gather_object %>% 
  filter(is_json_array(.)) %>% gather_array

## -----------------------------------------------------------------------------
companies[1] %>% gather_object %>% 
  filter(is_json_object(.)) %>% gather_object

## -----------------------------------------------------------------------------
json <- '{"2015": 5, "2016": 10}'
json %>% gather_object("year") %>% append_values_number("count")

## -----------------------------------------------------------------------------
worldbank %>% as.tbl_json

## ---- error = TRUE------------------------------------------------------------
bad_json <- '{"key": "value"'
bad_json %>% as.tbl_json

## -----------------------------------------------------------------------------
issues %>% as.tbl_json

## -----------------------------------------------------------------------------
issues %>% as.tbl_json %>% gather_array

## -----------------------------------------------------------------------------
library(purrr)
list('1', '2') %>% flatten_chr %>% as.tbl_json

## -----------------------------------------------------------------------------
df <- tibble(id = 1:2, json = list('[1, 2]', '[3, 4]'))
df %>% as.tbl_json(json.column = "json")

