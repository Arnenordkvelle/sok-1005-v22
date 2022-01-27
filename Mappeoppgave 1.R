library(tidyverse)
library(data.table)
library(zoo)
library(gdata)



# Oppgave 1

data <- fread ("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

data <- data [-c(1)]
data <- head(data, -1)

# Fjerner første og siste rad i dataframen, fordi første rad er siste måned i 1978 og siste er tekst.

clean_data <- data %>% 
  select(Year, Globe, Mo)

# Selekterer Year, Globe og Mo

clean_data$Globe <- as.numeric(as.character(clean_data$Globe))

clean_data$moyear <- paste(clean_data$Year, "-", clean_data$Mo)

clean_data$moyear <- as.yearmon(paste(clean_data$Year, clean_data$Mo), "%Y %m")

# Lager moyear som er Mo og Year blandet, og gjør formatet om til år og måned

clean_data %>% 
  ggplot(aes(x = moyear, y = Globe)) +
  geom_point(shape = 21, col = "dodgerblue4", fill = "white", stroke = 0.75) +
  geom_line(col = "dodgerblue4", group = 1) +
  geom_line(col = "red", size = 1.25, aes(y=rollmean(clean_data$Globe, 13, na.pad = TRUE))) +
  scale_x_yearmon(labels = clean_data$Year, breaks = clean_data$Year, expand = c(0, 0), limits = c(1979, NA)) +
  scale_y_continuous(labels = scales::comma_format(accuracy=0.1), limits=c(-0.7, 0.9), breaks=seq(-0.7, 0.9, by = 0.1)) +
  labs(y = "T departure from '91 - '20 Avg. (deg. C)", 
       x = "Latest Global Avarage Trpopspheric Tempratures") +
  theme(axis.text.x = element_text(angle = 90))

# Oppgave 2

clean_data1 <- fread ("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
clean_data2 <- fread ("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
clean_data3 <- fread ("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
clean_data4 <- fread ("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

clean_data1 <- rename.vars(clean_data1, from = "NoPol", to = "NoPol1")
clean_data1$NoPol1 <- as.numeric(as.character(clean_data1$NoPol1))
clean_data1 <- clean_data1 [-c(1)]
clean_data1 <- head(clean_data1, -1)
clean_data1 <- clean_data1 %>% 
  select(Year, NoPol1, Mo)
clean_data1$moyear <- paste(clean_data1$Year, "-", clean_data1$Mo)
clean_data1$moyear <- as.yearmon(paste(clean_data1$Year, clean_data1$Mo), "%Y %m")

clean_data2 <- rename.vars(clean_data2, from = "NoPol", to = "NoPol2")
clean_data2$NoPol2 <- as.numeric(as.character(clean_data2$NoPol2))
clean_data2 <- clean_data2 [-c(1)]
clean_data2 <- head(clean_data2, -1)
clean_data2 <- clean_data2 %>% 
  select(NoPol2)

clean_data3 <- rename.vars(clean_data3, from = "NoPol", to = "NoPol3")
clean_data3$NoPol3 <- as.numeric(as.character(clean_data3$NoPol3))
clean_data3 <- clean_data3 [-c(1)]
clean_data3 <- head(clean_data3, -1)
clean_data3 <- clean_data3 %>% 
  select(NoPol3)

clean_data4 <- rename.vars(clean_data4, from = "NoPol", to = "NoPol4")
clean_data4$NoPol4 <- as.numeric(as.character(clean_data4$NoPol4))
clean_data4 <- clean_data4 [-c(1)]
clean_data4 <- clean_data4 %>% 
  select(NoPol4)

# Sorterer dataen

ultimate_data <- cbind(clean_data1, clean_data2, clean_data3, clean_data4)

# I "NoPol4" så er det ikke like mange observasjoner som i de andre "NoPol" kolonnene. Dataen blir resirklulert som gjør at
# at gjennomsnittet fra 2020 og utover ikke blir riktig. Jeg fant ikke ut noen enkel måte å fikse dette på.

ultimate_data %>% 
  ggplot(aes(x = moyear)) +
  geom_point(aes(y = NoPol1), colour = "red") +
  geom_point(aes(y = NoPol2), colour = "blue") +
  geom_point(aes(y = NoPol3), colour = "purple") +
  geom_point(aes(y = NoPol4), colour = "green") +
  geom_line(colour = "black", size = 1.25, aes(y=rollmean(ultimate_data$NoPol1+NoPol2+NoPol3+NoPol4, 13, na.pad = TRUE))) +
  scale_x_yearmon(labels = ultimate_data$Year, breaks = ultimate_data$Year, expand = c(0, 0), limits = c(1979, NA)) +
  scale_y_continuous(labels = scales::comma_format(accuracy=1), limits=c(-10, 10), breaks=seq(-10, 10, by = 1)) +
  labs(y = "Temperaturer 60° - 90° Nord",
       x = "") +
  theme(axis.text.x = element_text(angle = 90))

# Skulle legge til "stat = "identity" eller liknende for å vise hva hver farge viser, men fikk ikke det til.
