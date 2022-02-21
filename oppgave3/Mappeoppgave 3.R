# Nødvendige pakker

library(tidyverse)
library(proto)

data <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

# Laster inn dataen, og trenger deataen til å bli gjort om til tabell

data <- data %>% html_table()

# Selekterer første tabell som er observert

data <- data[[1]]

# Renamer nødvendige kolonner til ønsker navn

data <- data %>% 
  rename(WLTP = X2,
         STOPP = X3)

# Selekterer andre og tredje kolonne

data = select(data, -c(1, 4))

# Fjerner første rad som bare inneholder tekst sånn at jeg får gjort om til numerisk

data = data[-1,]

# Fjerner rader med data som inneholder characteren "X"

data <- data[!grepl("x",data$STOPP),]

# Erstatter "KM" med "", altså ingenting.

data$STOPP <- gsub("km","",as.character(data$STOPP))

# Gjør om til numerisk fra character

data$STOPP <- as.numeric(as.character(data$STOPP))

# Selekterer de 3 første characterene i hver obersvasjon i kolonna
# https://stackoverflow.com/questions/21675379/r-only-keep-the-3-x-first-characters-in-a-all-rows-in-a-column/21675473

data$WLTP <- substr(data$WLTP, 0, 3)

# Gjør om til numerisk fra character

data$WLTP <- as.numeric(as.character(data$WLTP))

# Plot med geom_point, geom_abline, brakes og limits. Kunne også brukt ylim og xlim.

data %>%
  ggplot(aes(x=WLTP, y=STOPP)) +
  geom_point(size = 1.5, col="black") +
  geom_abline(size = 1.55, col = "red") +
  scale_x_continuous(limits= c(200, 650), breaks = seq(200, 650, by = 100)) +
  scale_y_continuous(limits= c(200, 600), breaks = seq(200, 600, by = 100)) +
  labs(x ="WLTP",
       y = "STOPP") +
  theme_bw()

# Regresjonslinje

lm(STOPP ~ WLTP, data =data)

# Plot med regresjonslinje og konfidensintervall

data %>%
  ggplot(aes(x=WLTP, y=STOPP)) +
  geom_point(size = 1.5, col="black") +
  geom_abline(size = 1.5, col = "red") +
  geom_smooth(method = lm) +
  scale_x_continuous(limits= c(200, 645), breaks = seq(200, 645, by = 100)) +
  scale_y_continuous(limits= c(200, 600), breaks = seq(200, 600, by = 100)) +
  labs(title = "STOPP versus WLTP Test",
       x ="WLTP", 
       y = "STOPP") +
  theme_bw()

# Den blå regresjonslinja viser at bilene er i gjennomsnitt ganske nærme på å ha rekkeviden de reklamerer å ha.
# WLTP testen er en test som blir gjort innendørs i et rom med 23 grader, i motsettning til STOPP gjort utendørs i kulda. 
# Den røde linja (WLTP-testen) viser stigningstall på 1:1, mens den blå linja (STOPP-testen) viser at stigningstallet er 1:0,867. 
# Dette avviket mellom den blå og røde linja sier noe om hvordan kulde kan påvirke el-biler samt avvik fra hva bilprodusentens oppgitte kjørelengde.

