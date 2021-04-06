# Required libraries
library(gganimate)
library(ggplot2)
library(dplyr)
library(scales)
library(png)
library(ggimage)
library(ggpubr)

# data prep
forest_data <- rio::import("2021/2021-04-06/data/forest-area-km.csv")
forest_data_BR <- forest_data %>%
  filter(Entity == "Brazil")

names(forest_data_BR) <- c("entity", "Code", "year", "forest_area")

forest_data_BR$Image <- rep(c("2021/2021-04-06/img/fire_0.png"),31)
forest_data_BR$Image2 <- rep(c("2021/2021-04-06/img/fire_1.png"),31)
forest_data_BR[forest_data_BR$year %% 2 == 1, ]$Image <- NA
forest_data_BR[forest_data_BR$year %% 2 == 0, ]$Image2 <- NA

my_image <- readPNG("2021/2021-04-06/img/brazil_transp.png", native = TRUE)

# plot
brazil_plot <- forest_data_BR %>%
  ggplot(aes(x = entity, y = forest_area)) +
  geom_bar(stat='identity', colour = "#079A49", fill = "#079A49", width = 3) +
  geom_image(aes(image=Image)) +
  geom_image(aes(image=Image2)) +
  theme_minimal() +
  theme(plot.title = element_text(size=20), text = element_text(size=15),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        line = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  coord_flip() +
  background_image(my_image) +
  #scale_y_continuous(expand = c(0,0)) +
  labs(title = "Change in Brazil's total forest area over time",
       subtitle =  'Year: {closest_state}', caption="Source: Our World in Data \nCreated by Hugo Fitipaldi",
       y = "Forest area (hectares)") +
  transition_states(
    year,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')

# Save gif
animate(brazil_plot, nframes = 750, fps = 50, end_pause = 50,
        width = 800, height = 600, renderer = gifski_renderer("brazil.gif"))

