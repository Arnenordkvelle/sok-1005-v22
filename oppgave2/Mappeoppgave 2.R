library(tidyverse)
library(jsonlite)
library(scales)
library(ggrepel)

# Laster ned data med jsonlite
UScovid <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

# Bruker R sin funksjon for forkortelser av de 50 statene
UScovid$name <- state.abb[match(UScovid$name, state.name)]

# I dataframen er DC også med, og som vi vet er DC ikke en stat. I dette tilfellet gjør vi
# alle "NA" verdier om til "DC" for å få objektet med på plottet.
UScovid[is.na(UScovid)] <- "DC"

# Plot 1
# Bruker scale_x_continuous for å få riktig labels på x-aksen, samt breaks og limits for å få det så likt som mulig.
UScovid %>%
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, label = name)) +
  geom_point(size = 4, shape = 20, col="palegreen4", fill = "palegreen4", stroke = 1, alpha = 0.5) + geom_text_repel(box.padding = unit(0.1, "lines")) +
  scale_x_continuous(labels = percent, limits=c(0.45, 0.80), breaks=seq(0.45, 0.80, by = 0.05)) +
  scale_y_continuous(limits= c(0, 20), breaks = seq(0, 20, by = 5)) +
  labs(title="Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x ="Share of total population fully vaccinated",
       y = "Monthly deaths per 100,000") +
  theme_bw()


lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = UScovid)

# Plot 2
UScovid %>%
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, label = name)) +
  geom_point(size = 3, shape = 21, col="palegreen4", fill = "palegreen4", stroke = 1, alpha = 0.5) + geom_text_repel(box.padding = unit(0.5, "lines")) +
  geom_smooth(method = lm) +
  scale_x_continuous(labels = percent, limits=c(0.45, 0.80), breaks=seq(0.45, 0.80, by = 0.05)) +
  scale_y_continuous(limits= c(0, 20), breaks = seq(0, 20, by = 5)) +
  labs(title="Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x ="Share of total population fully vaccinated",
       y = "Monthly deaths per 100,000") +
  theme_bw()

# Den blå lineære linja på grafen tror jeg skal representere gjennomsnittet og den svarte området viser koeffisienten.
# Linja viser en nedgang av dødsfall desto mer prosent av befolkningen er vaksinert. 
# Det er en tydelig korrelasjon mellom antall prosent vaksinerte og dødsfall.