#https://www.r-bloggers.com/2019/10/building-a-shiny-app-for-cycling-in-ottawa/
#install.packages(c("tidyverse", "leaflet", "leafpop"))
#install.packages("tidyverse")
library(tidyverse)
#install.packages("leaflet")
library(leaflet)
#install.packages("leafpop")
library(leafpop)
bikes <- read_csv("https://raw.githubusercontent.com/whipson/Ottawa_Bicycles/master/bikes_app.csv", col_types = c("?nnnnnnnnnnnnnn"))
bikes

bikes_total <- bikes %>%
  pivot_longer(names_to = "counter", values_to = "count", -date) %>%
  group_by(date) %>%
  mutate(daily_total = sum(count, na.rm = TRUE))
bikes_total

bikes_total %>%
  ggplot(aes(x = date, y = daily_total)) +
  geom_line(size = .5, alpha = .80, color = "#36648B") +
  scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL,
       y = "Count",
       title = "Total Bicycle Crossings in Ottawa",
       subtitle = "Jan 2010 - Sep 2019") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(size = 16))

bikes %>%
  pivot_longer(names_to = "counter", values_to = "count", -date) %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line(size = .5, alpha = .80, color = "#36648B") +
  labs(x = NULL,
       y = "Count",
       title = "Bicycle Crossings in Ottawa by Location",
       subtitle = "Jan 2010 - Sep 2019") +
  facet_wrap(~counter) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_blank())
## Warning: Removed 2191 rows containing missing values (geom_path).

bikes %>%
  pivot_longer(names_to = "counter", values_to = "count", -date) %>%
  group_by(date) %>%
  mutate(daily_average = mean(count, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = daily_average)) +
  geom_line(size = .5, alpha = .80, color = "#36648B") +
  scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL,
       y = "Count",
       title = "Average Bicycle Crossings in Ottawa",
       subtitle = "Jan 2010 - Sep 2019") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(size = 16))

coords <- data.frame(counter = names(bikes[,2:15]),
                     name = c("Alexandra Bridge", "Eastern Canal Pathway", "Ottawa River Pathway", "Western Canal Pathway",
                              "Laurier at Bay", "Laurier at Lyon", "Laurier at Metcalfe", "Somerset Bridge", "OTrain at Young",
                              "OTrain at Gladstone", "OTrain at Bayview", "Portage Bridge", "Adawe Crossing A", "Adawe Crossing B"),
                     lat = c(45.430366, 45.420924, 45.411959, 45.406280,
                             45.415893, 45.417036, 45.419790, 45.420512,
                             45.402859, 45.404599, 45.408636, 45.421980, 
                             45.426282, 45.426575),
                     long = c(-75.704761, -75.685060, -75.723424, -75.681814,
                              -75.705328, -75.702613, -75.697623, -75.684625,
                              -75.712760, -75.714812, -75.723644, -75.713324,
                              -75.670234, -75.669765),
                     desc = c("Ottawa approach to the NCC Alexandra Bridge Bikeway. This counter was not operational for most of 2010
                              due to bridge construction. This is one of the more consistent counters, until the internal battery
                              failed in August 2019.",
                              "NCC Eastern Canal Pathway approximately 100m north of the Corktown Bridge.",
                              "NCC Ottawa River Pathway approximately 100m east of the Prince of Wales Bridge. Canada Day in 2011
                              boasts the highest single day count of any counter.",
                              "NCC Western Canal Pathway approximately 200m north of “The Ritz”. Out of operation for much of 2018.
                              MEC Bikefest on May 17, 2015 accounts for the large spike that day.",
                              "Laurier Segregated Bike lane just west of Bay. Minimal data available due to inactivity after 2014.",
                              "Laurier Segregated Bike lane just east of Lyon. No longer in operation since 2016.",
                              "Laurier Segregated Bike lane just west of Metcalfe. Construction in late 2012 accounts for unusual dip
                              in counts.",
                              "Somerset bridge over O-Train west-bound direction only. Inexplicably large spike in 2012 followed by a
                              typical seasonal pattern. Inactive since late 2018.",
                              "O-Train Pathway just north of Young Street. Minimal data available due to inactivity after 2016. See
                              O-Train at Gladstone counter for a better estimate.",
                              "O-Train Pathway just north of Gladstone Avenue. In operation since mid-2013. Shows unusual spike in
                              November of 2017.",
                              "O-Train Pathway just north of Bayview Station. In operation since mid-2013. Trending upward.",
                              "Portage Bridge connecting Gatineau to Ottawa. Installed in late 2013, this counter registered
                              relatively high traffic but seems to have experienced outages during Winter months. Inactive since early
                              2016.",
                              "Adàwe Crossing Bridge bike lane. This counter is one of a pair on this pedestrian bridge. Installed in
                              2016, it seems to have experienced an outage during the Winter of its inaugural year.",
                              "The second of two counters on the Adàwe Crossing Bridge. This counter may pick up more pedestrian than
                              bike traffic, as suggested by the trend over time."))

leaflet(data = coords) %>%
  addTiles() %>%
  addMarkers(~long, ~lat)

leaflet(data = coords) %>%
  addTiles() %>%
  addMarkers(~long, ~lat, label = ~name) %>%
  setMaxBounds(-75.65, 45.38, -75.75, 45.46) %>%
  addProviderTiles(providers$CartoDB.Positron)
