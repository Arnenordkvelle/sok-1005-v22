<<<<<<< HEAD
=======
# Nødvendige pakker

>>>>>>> 1ea309dd288822ac77e53a96a1526dd5c58c1413
library(tidyverse)
library(rvest)
library(proto)

<<<<<<< HEAD
data <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

data <- data %>% html_table()

data <- data[[1]]

data <- data %>% 
  rename(Modell = X1,
         WLTP = X2,
         STOPP = X3,
         Avvik = X4)

data = select(data, -c(1, 4))

data =data[-1,]

data <-data[!grepl("x",data$STOPP),]

data$STOPP<-gsub("km","",as.character(data$STOPP))

data$STOPP <- as.numeric(as.character(data$STOPP))

data$WLTP <- sub("^(\\d{3}).*$", "\\1",data$WLTP)

# kilde: https://stackoverflow.com/questions/21675379/r-only-keep-the-3-x-first-characters-in-a-all-rows-in-a-column/21675473

data$WLTP <- as.numeric(as.character(data$WLTP))

data %>%
  ggplot(aes(x=WLTP, y=STOPP)) +
  geom_point(size = 2.5, col="black") +
  geom_abline(size = 1.75, col = "red") +
  scale_x_continuous(limits= c(200, 600), breaks = seq(200, 600, by = 100)) +
  scale_y_continuous(limits= c(200, 600), breaks = seq(200, 600, by = 100)) +
  labs(title="Plot",
       x ="WLTP",
       y = "STOPP") +
  theme_bw()

lm(STOPP ~ WLTP, data =data)

data %>%
  ggplot(aes(x=WLTP, y=STOPP)) +
  geom_point(size = 2.5, col="black") +
  geom_abline(size = 1.75, col = "red") +
  geom_smooth(method = lm) +
  scale_x_continuous(limits= c(200, 600), breaks = seq(200, 600, by = 100)) +
  scale_y_continuous(limits= c(200, 600), breaks = seq(200, 600, by = 100)) +
  labs(title="Plot",
       x ="WLTP",
       y = "STOPP") +
  theme_bw()

=======
# Laster inn linken

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
# Kan ikke ta æren for å skrive dette, fant koden etter søking på stackoverflow.com
# kilde: https://stackoverflow.com/questions/21675379/r-only-keep-the-3-x-first-characters-in-a-all-rows-in-a-column/21675473

data$WLTP <- sub("^(\\d{3}).*$", "\\1",data$WLTP)

# Gjør om til numerisk fra character

data$WLTP <- as.numeric(as.character(data$WLTP))

# Plot med geom_point, geom_abline, brakes og limits

data %>%
  ggplot(aes(x=WLTP, y=STOPP)) +
  geom_point(size = 1.5, col="black") +
  geom_abline(size = 1.55, col = "red") +
  scale_x_continuous(limits= c(200, 600), breaks = seq(200, 600, by = 100)) +
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
  scale_x_continuous(limits= c(200, 600), breaks = seq(200, 600, by = 100)) +
  scale_y_continuous(limits= c(200, 600), breaks = seq(200, 600, by = 100)) +
  labs(x ="WLTP",
       y = "STOPP") +
  theme_bw()

# Den blå regresjonslinja viser at bilene er i gjennomsnitt ganske nærme på å ha rekkeviden de reklamerer å ha.
# 


>>>>>>> 1ea309dd288822ac77e53a96a1526dd5c58c1413


