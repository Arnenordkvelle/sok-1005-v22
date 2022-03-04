library(rvest)
library(tidyverse)
library(rlist)
library(purrr)

# Lager liste med de tre nødvendige linkene

url_list <- list("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list",
                 "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&week=1-20&View=list",
                 "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list")

# Lager funksjon som kan scrape disse tre nettsidene med koden vi fikk fra "scrape_timeplan.R"

scrape <- function(url) {
  page <- read_html(url)
  
  table <- html_nodes(page, 'table') 
  table <- html_table(table, fill=TRUE) 
  
  dframe <- list.stack(table)
  
  colnames(dframe) <- dframe[1,]
  
  dframe <- dframe %>% filter(!Dato=="Dato")
  
  dframe <- dframe %>% separate(Dato, 
                                into = c("Dag", "Dato"), 
                                sep = "(?<=[A-Za-z])(?=[0-9])")
  
  dframe$Dato <- as.Date(dframe$Dato, format="%d.%m.%Y")
  
  dframe$Uke <- strftime(dframe$Dato, format = "%V")
  
  dframe <- dframe %>% select(Dag,Dato,Uke,Tid,Rom)
  
  return(dframe)
  
}

# Definerer "timeplan" der mapping av "url_list" blir brukt, med funksjonen "scrape", så binder jeg sammen dataframesa. Til slutt arrangerer jeg timene etter dato og tidspunkt.

timeplan <- map(url_list, scrape)

timeplan <- bind_rows(timeplan)

timeplan <- timeplan %>% 
  arrange(Dato, Tid)
