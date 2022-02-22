library(tidyverse)
library(rvest)


# Oppgave 1


biler <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")


biler <- biler %>% html_table()


biler <- biler[[1]]


biler <- biler %>% 
  rename(wltp = X2,
         stopp = X3)


biler$wltp <- substr(biler$wltp, 0, 3) %>% 
  as.numeric(biler$wltp)
biler$stopp <- substr(biler$stopp, 0, 3) %>% 
  as.numeric(biler$stopp) 





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


