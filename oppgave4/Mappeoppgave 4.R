library(rvest)
library(tidyverse)
library(rlist)
library(purrr)

url1 <-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list"
url2 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&week=1-20&View=list"
url3 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list"

# Lager liste med de tre nødvendige linkene

url_list <- list(url1, url2, url3)

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

# Definerer "timeplan" med mapping av "url_list", med funksjonen "scrape", så binder jeg sammen dataframesa

timeplan <- map(url_list, scrape)
timeplan <- bind_rows(timeplan)
