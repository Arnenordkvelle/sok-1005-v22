library(tidyverse)
library(rvest)
library(proto)

data <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

data <- data %>% html_table()

data <- data[[1]]

data <- data %>% 
  rename(Modell = X1,
         WLTP = X2,
         STOPP = X3,
         Avvik = X4)

data = select(data, -c(1, 4))

data = data[-1,]

data <- data[!grepl("x",data$STOPP),]

data$STOPP <- gsub("km","",as.character(data$STOPP))

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