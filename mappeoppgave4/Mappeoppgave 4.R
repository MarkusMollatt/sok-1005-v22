# Laster ned pakker

library(rvest)
library(tidyverse)
library(rlist)
library(purrr)
library(zoo)


# Laster ned timeplan og lagrer de under navnet til de forskjellige fagene vi har.

sok_1005 <-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list"
sok_1006 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&week=1-20&View=list"
sok_1016 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list"



# Lager en liste med alle fagene i en.


listeFag <- list(sok_1005, sok_1006, sok_1016)


# Lager en funksjon som scraper alle nettsidene med koden vi fikk fra scrape_timeplan.R.
# I tillegg benytter jeg meg av pakken, zoo for å få en dato på timene som fikk verdien
# NA på dag og dato. Det så ut til å bli slik på grunn av timen(e) som var tidligere den dagen,
# så jeg la inn dag og dato fra de på raden over.


scrape <- function(url) {
  page <- read_html(url)
  
  table <- html_nodes(page, 'table') 
  table <- html_table(table, fill=TRUE) 
  
  dframe <- list.stack(table)
  
  colnames(dframe) <- dframe[1,]
  
  dframe <- dframe %>% filter(!Dato=="Dato")
  
  dframe$Dato[dframe$Dato == ""] <- NA
  dframe$Dato <- na.locf(dframe$Dato)
  
  dframe <- dframe %>% separate(Dato, 
                                into = c("Dag", "Dato"), 
                                sep = "(?<=[A-Za-z])(?=[0-9])")
  
  dframe$Dato <- as.Date(dframe$Dato, format="%d.%m.%Y")
  
  dframe$Uke <- strftime(dframe$Dato, format = "%V")
  
  dframe <- dframe %>% 
    arrange(Dato)
  
  dframe <- dframe %>% select(Dag,Dato,Uke,Tid,Rom, Emnekode)
  
  return(dframe)
  
}


# Legger alle timene i en tabell og sorterer dette etter dato.

timeplan <- map(listeFag, scrape)

timeplan <- bind_rows(timeplan)

timeplan <- timeplan %>% 
  arrange(Dato, Tid)
timeplan
































