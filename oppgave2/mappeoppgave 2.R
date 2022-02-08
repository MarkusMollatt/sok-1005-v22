library(tidyverse)
library(rjson)
library(jsonlite)

covidDeath <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

covidDeath %>%
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k)) +
  geom_point(col="blue") +
  scale_x_continuous(labels = scales::percent) +
  labs(title="Latest Global Temps",
       x ="Share of total population fully vaccinated",
       y = "Monthly deaths per 100,000") +
  theme_bw()
