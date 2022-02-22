library(tidyverse)
library(rvest)


# Oppgave 1

# Henter og leser data
biler <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

# Gjør den om til en tabell
biler <- biler %>% html_table()

# Velger den første tabellen 
biler <- biler[[1]]

# Bytter navn på to kolonner jeg skal bruke senere
biler <- biler %>% 
  rename(wltp = X2,
         stopp = X3)

# Fjerner hva som er bak f.eks kw/h, slik at vi bare får ut tall
biler$wltp <- substr(biler$wltp, 0, 3) %>% 
  as.numeric(biler$wltp)
biler$stopp <- substr(biler$stopp, 0, 3) %>% 
  as.numeric(biler$stopp) 



# Bruker tabellen til å lage et plot over hvor lang rekkevidde det faktisk er på bilen i forhold til hva de markedsfører at bilen skal ha.

ggplot(biler, aes(x = wltp, y = stopp)) +
  geom_point() + 
  geom_abline(size = 1, color = "red") +
  ylim(200, 650)+ xlim(200, 650) +
  labs(title = "Rekkeviddetest for elbiler")

# I plottet viser den røde linja hvor langt bilene skulle ha kjørt.
# De svarte prikkene viser hvor langt de forskjellige bilene kjørte.


# Oppgave 2


lm(stopp ~ wltp, data = biler) 


# Her får vi opp to koeffisienter. Jeg tolker intercept til å være forskjellen mellom tester
# for eksemper vær og temperatur. Wltp koeffisienten tolker jeg til å være hvor mye over eller under
# rekkevidden bilselskapene selv sier at bilene skal ha, hvor 1 er likt i praksis og teori.
# Den er på 0.8671, og det ser da ut som bilene faktisk ikke har like lang rekkevidde som de reklammerer for.





# Plottet i oppgave 1, vist etter å ha lagt til regresjonslinja.

ggplot(biler, aes(x = wltp, y = stopp)) +
  geom_point() + 
  geom_abline(size = 1, color = "red") +
  ylim(200, 650)+ xlim(200, 650) +
  labs(title = "Rekkeviddetest for elbiler") +
  geom_smooth(method = lm)


# Regresjonslinja viser et gjennomsnitt over hvor bra elbilene gjør det i kulda i motsetning til wltp testen.
# Forskjellen mellom den røde og den blå linja viser hvor mye været påvirker elbilen din.


