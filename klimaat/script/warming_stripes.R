# Warming stripes oftewel klimaat streepjescode voor Twente
# Gebaseerd op het werk van prof Ed Hawkins van de University of Reading
# in 2018.


library(tidyverse)
library(lubridate)

# Bron weergegevens
# url <- "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_290.zip"

weer <-
  read_delim("data/ruw/etmgeg_290.txt", skip = 51, delim = ",") |>
  janitor::clean_names() |>
  select(yyyymmdd, tg) |>
  filter(
    yyyymmdd <= 20241231
  ) |>
  mutate(
    tg = as.numeric(tg) / 10
  )

summary(weer)

weer |>
  filter(is.na(tg))

weer <- weer |>
  mutate(
    jaar = as.integer( year(ymd(yyyymmdd)) )
  ) |>
  group_by(jaar) |>
  summarise(
    t_gem = mean(tg, na.rm = TRUE)
  )

# De gemiddelde temperatuur in de referentieperiode 1971 - 2000.
ref_gem <- weer |>
  filter(
    jaar >= 1971, jaar <= 2000
  ) |>
  summarise(
    t_gem = mean(t_gem)
  ) |>
  pull()


weer <- weer |>
  mutate(
    afwijking = t_gem - ref_gem
  )


weer |>
  ggplot(aes(x = jaar, y = 1, fill = afwijking)) +
  geom_col(width = 1, show.legend = FALSE) +
  scale_fill_fermenter(n.breaks = 11, palette = "RdBu") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.06, vjust = -7),
    plot.subtitle = element_text(face = "italic", size = 10, hjust = 0.05, vjust = -9),
    plot.caption = element_text(size = 8, vjust = 14, hjust = 0.07)
  ) +
  labs(
    title = "Twente - Klimaat streepjescode",
    subtitle = "1951 - 2024",
    caption = "data: KNMI daggegevens station Vliegveld Twenthe (290), visualisatie en analyse: Erwin Bramer"
  )
