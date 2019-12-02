## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 10L)

## ----load, echo=TRUE, results='hide', message=FALSE----------------------
library(dplyr)
library(tidyr)
library(jsonlite)
library(tidyjson)
library(ggplot2)
library(lubridate)

## ----gitapi, echo=TRUE---------------------------------------------------
baseurl <- 'https://api.github.com/repos/tidyverse/dplyr/issues'
dplyr_issues <- as.tbl_json(baseurl)

dplyr_issues %>% json_schema %>% prettify

## ----gitapi_highlevel, echo=TRUE-----------------------------------------

highlevel <- dplyr_issues %>% gather_array('index') %>% 
  spread_values(id=jnumber('id')
                , assignee=jstring('assignee','login')
                , comments=jnumber('comments')
                , title=jstring('title')
                , state=jstring('state')
                , number=jnumber('number')
                )

print(highlevel)


## ----gitapi_summarize, echo=TRUE-----------------------------------------

highlevel %>% group_by(assignee) %>% summarize(nissues=n())

highlevel %>% group_by(comments) %>% summarize(nissues=n(), issues=paste(number,collapse=',')) %>% 
  ungroup() %>% arrange(desc(comments))

highlevel %>% group_by(state) %>% summarize(nissues=n())


## ----gitapi_many, echo=TRUE----------------------------------------------
manyissues <- lapply(c(1:7), function(x){as.tbl_json(paste0(baseurl,'?state=all&per_page=50&page=',x))})

## Collapse into one tbl_json
manyissues <- tidyjson::bind_rows(manyissues)

## Summarize status & users that create issues
manyissues %>% gather_array('issue') %>% spread_values(
  login=jstring('user','login')
  , comments=jnumber('comments')
  , issuenum = jnumber('number')
  , state = jstring('state')
) %>% group_by(login, state) %>% summarize(issuecount=n()) %>% ungroup() %>%
  spread(state, issuecount, fill=0) %>%
  mutate(total=closed+open) %>%
  arrange(desc(total), desc(open)) %>% head(10)

## ----citibike_init, echo=TRUE--------------------------------------------
citibike <- as.tbl_json("http://citibikenyc.com/stations/json")

## We see what we have is an object
citibike %>% json_types()

## So let's explore that object
citibike %>% gather_object()

## ----citibike_prep, echo=TRUE--------------------------------------------
citibike_list <- citibike %>% 
  spread_values(execution=jstring(executionTime)) %>%
  enter_object('stationBeanList') %>% gather_array('arrayid')

citibike_list %>%
  filter(arrayid==1) %>%
  json_schema() %>% prettify()


## ----citibike_available, echo=TRUE---------------------------------------
citibike_available <- citibike_list %>% 
  spread_values(id=jnumber(id)
                , location=jstring(location)
                , lastCommunication=jstring(lastCommunicationTime)
                , availableBikes=jnumber(availableBikes)
                , availableDocks=jnumber(availableDocks)
                , totalDocks=jnumber(totalDocks)) %>%
  mutate(openDockPct=availableDocks / totalDocks
         , bikeDockPct=availableBikes / totalDocks
         , timeSinceUpdateMinutes=as.integer(as_datetime(execution)-as_datetime(lastCommunication))/60
         , timeSinceUpdateBin=cut(timeSinceUpdateMinutes
                                  ,c(0,1,15,60,6*60,24*60,Inf)
                                  , labels=c('0-1 Min','1-15 Min'
                                             , '15 Min - 1 Hr'
                                             , '1-6 Hr'
                                             , '6-24 Hr'
                                             , '24+ Hr'))
         )

## Expect generally linear behavior
ggplot(citibike_available, aes(openDockPct, bikeDockPct)) + geom_point()

## ----citibike_count, echo=TRUE-------------------------------------------
ggplot(citibike_available, aes(availableBikes, availableDocks, col=timeSinceUpdateBin)) +
  geom_point()


## ----citibike_map_prep, ECHO=TRUE----------------------------------------
citibike_map <- citibike_available %>%
  spread_values(lat=jnumber(latitude)
                , long=jnumber(longitude))

citibike_map %>% group_by(is.na(lat),is.na(long)) %>% summarize(n())

## ----citibike_error_test, ECHO=TRUE--------------------------------------
citibike_list_0 <- '{}' %>% 
  spread_values(execution=jstring(executionTime)) %>%
  enter_object('stationBeanList') %>% gather_array('arrayid')

citibike_available_0 <- citibike_list_0 %>% 
  spread_values(id=jnumber(id)
                , location=jstring(location)
                , lastCommunication=jstring(lastCommunicationTime)
                , availableBikes=jnumber(availableBikes)
                , availableDocks=jnumber(availableDocks)
                , totalDocks=jnumber(totalDocks)) %>%
  mutate(openDockPct=availableDocks / totalDocks
         , bikeDockPct=availableBikes / totalDocks
         , timeSinceUpdateMinutes=as.integer(as_datetime(execution)-as_datetime(lastCommunication))/60
         , timeSinceUpdateBin=cut(timeSinceUpdateMinutes
                                  ,c(0,1,15,60,6*60,24*60,Inf)
                                  , labels=c('0-1 Min','1-15 Min'
                                             , '15 Min - 1 Hr'
                                             , '1-6 Hr'
                                             , '6-24 Hr'
                                             , '24+ Hr'))
  )

ggplot(citibike_available_0, aes(availableBikes, availableDocks, col=timeSinceUpdateBin)) +
  geom_point()

