library(tidyverse)
library(jsonlite)
library(scales)
library(ggrepel)

data <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

data$name <- state.abb[match(data$name, state.name)]

data[is.na(data)] <- "DC"

data %>%
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, label = name)) +
  geom_point(size = 4, shape = 21, col="aquamarine3", fill = "aquamarine4", stroke = 0.1, alpha = 1) + geom_text_repel() +
  scale_x_continuous(labels = scales::percent, limits=c(0.45, 0.80), breaks=seq(0.45, 0.80, by = 0.05)) +
  labs(title="20 avg. monthly deaths per 100,000",
       x ="Share of total population fully vaccinated",
       y = "Monthly deaths per 100,000") +
  theme_bw()


lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = data)

data %>%
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, label = name)) +
  geom_point(size = 4, shape = 21, col="aquamarine1", fill = "aquamarine4", stroke = 0.3, alpha = 0.7) + geom_text_repel() +
  geom_smooth(method = lm) +
  scale_x_continuous(labels = scales::percent, limits=c(0.45, 0.80), breaks=seq(0.45, 0.80, by = 0.05)) +
  labs(title="20 avg. monthly deaths per 100,000",
       x ="Share of total population fully vaccinated",
       y = "Monthly deaths per 100,000") +
  theme_bw()