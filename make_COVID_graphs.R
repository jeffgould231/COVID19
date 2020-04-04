
library(tidyverse)
library(gganimate)
library(ggrepel)
library(RColorBrewer)

setwd("~/Documents/COVID19/")

JHU_cases_URL <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
JHU_deaths_URL <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
NYT_county_URL <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
NYT_state_URL <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
# 
# destfile <-  "/tr/proj10/m1jtg03/COVID19/jhu_corona_cases.csv"
# download.file(JHU_cases_URL, destfile, method="wget")
# 
# destfile <-  "/tr/proj10/m1jtg03/COVID19/jhu_corona_deaths.csv"
# download.file(JHU_deaths_URL, destfile, method="wget")
# 
# 
# destfile <-  "/tr/proj10/m1jtg03/COVID19/NYT_corona_states.csv"
# download.file(NYT_state_URL, destfile, method="wget")
# 
# 
# destfile <-  "/tr/proj10/m1jtg03/COVID19/NYT_corona_county.csv"
# download.file(NYT_county_URL, destfile, method="wget")

theme_set(theme_bw())
JHU_cases <- read_csv(JHU_cases_URL)
JHU_deaths <- read_csv(JHU_deaths_URL)
NYT_county <- read_csv(NYT_county_URL)
NYT_states <- read_csv(NYT_state_URL)
clean_jhu <- function(data, cases_val = 100) {
  
  last_col <- tail(colnames(data),1)
  
  jhu_clean <- data %>%
    select(-Lat, -Long) %>%
    pivot_longer(cols = `1/22/20`:!!last_col, names_to = "date") %>%
    group_by(`Country/Region`, date) %>%
    summarise(cases = sum(value, na.rm=T)) %>%
    ungroup() %>%
    mutate(date = lubridate::mdy(date)) %>%
    arrange(`Country/Region`, date) %>%
    mutate(cases_100 = ifelse(cases >= cases_val,1,0)) %>%
    group_by(`Country/Region`) %>%
    mutate(days_since_100 = cumsum(cases_100) - 1) %>%
    ungroup()
  return(jhu_clean)
}
COVID19_cases <- clean_jhu(JHU_cases)
COVID19_deaths <- clean_jhu(JHU_deaths, cases_val = 10)


### Make Current map of cases across US
if (require("maps")) {
  states <- map_data("state")
  
  NYT_last_state_obs <- NYT_states %>%
    group_by(state) %>%
    top_n(1, date) %>%
    mutate(region = tolower(state))
  plot_date <- max(NYT_last_state_obs$date)
  
  choro <- merge(states, NYT_last_state_obs, sort = FALSE, by = "region")
  choro <- choro[order(choro$order), ]
  ggplot(choro, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = cases), color = "black") +
    scale_fill_distiller(palette = "OrRd", 
                         trans = "log10", 
                         direction = 1, 
                         na.value = "white") +
    #scale_fill_gradient(palette= "Reds", trans = "log10") +
    theme(legend.position = "right") + 
    labs(title = "Confirmed COVID-19 Cases Across the United States",
         x = "",
         y = "",
         subtitle = format(plot_date, "%m/%d/%Y"),
         fill = "Cases",
         caption = "Data from NYT: https://github.com/nytimes/covid-19-data") + 
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) #+
   # coord_map("albers",  at0 = 45.5, lat1 = 29.5)
  ggsave("COVID 19 Cases US.jpg", width = 8.5, height = 4.75, units = "in")

}



### Make Current map of cases across NYS
if (require("maps")) {
  counties <- map_data("county") %>%
    filter(region == "new york")
  
  NYT_last_county_obs <- NYT_county %>%
    filter(state == "New York") %>%
    group_by(county) %>%
    top_n(1, date) %>%
    mutate(subregion = tolower(county)) %>%
    ungroup()
  
  total_cases <- sum(NYT_last_county_obs$cases)
  NYC_cases <- NYT_last_county_obs[NYT_last_county_obs$subregion == "new york city",]$cases
  
  NYC <- NYT_last_county_obs %>%
    filter(county == "New York City")
  NYC <- bind_rows(NYC, NYC, NYC, NYC, NYC)
  NYC$subregion = c("bronx","queens", "new york", "kings", "richmond")
  
  plot_date <- max(NYT_last_county_obs$date)
  
  choro <- left_join(counties, bind_rows(NYT_last_county_obs, NYC)) %>%
    mutate(date = replace_na(date, plot_date)) 
  choro <- choro[order(choro$order), ]
  
  ggplot(choro, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = cases), color = "black", show.legend = F)  +
    scale_fill_distiller(palette = "OrRd", 
                         trans = "log10", 
                         direction = 1, 
                         na.value = "white") +
    # scale_fill_gradient(low = "white", high = "red", 
    #                     trans = "log10",
    #                     na.value = "white") +
    labs(title = "Confirmed COVID-19 Cases Across New York State",
         x = "",
         y = "",
         subtitle = format(plot_date, "%m/%d/%Y"),
         fill = "Cases",
         caption = "Data from NYT: https://github.com/nytimes/covid-19-data")  +
    annotate("text", x = -78.5, y = 44.5, 
    label = paste0("New York State has ", total_cases, "
    confirmed cases of COVID-19"), 
    size = 5) +
    annotate("text", x = -73, y = 41.35, label = paste0("NYC has ", NYC_cases, "
    confirmed cases."), 
             size = 4) +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) 

  ggsave("COVID 19 Cases NYS.jpg", width = 8.5, height = 5.25, units = "in")
  
}


Cases_plot <- COVID19_cases %>%
  filter(`Country/Region` %in% c("US", "Spain", "Italy", "Iran", "Germany",
                                 "China", "Japan", "Korea, South", "United Kingdom"), days_since_100 >= 0) 

Deaths_plot <-COVID19_deaths %>%
  filter(`Country/Region` %in% c("US", "Spain", "Italy", "Iran", "Germany",
                                 "China", "Japan", "Korea, South", "United Kingdom"), days_since_100 >= 0)


ggplot(data = Cases_plot, aes(x = days_since_100, y = cases, color = `Country/Region`)) +
  geom_point(size = 2, show.legend = F) +
  geom_line(size = .75, show.legend = F) +
  scale_y_log10(labels = scales::comma, limits = c(100, 1000000)) +
  geom_label_repel(data = Cases_plot %>% group_by(`Country/Region`) %>% top_n(1, days_since_100),
                   aes(label = `Country/Region`), color = "black", nudge_x = 1.5) +
  geom_line(aes(y = 100 * 2^(days_since_100 / 3)), linetype = 2, color = "grey") +
  geom_line(aes(y = 100 * 2^(days_since_100 / 2)), linetype = 2, color = "grey") +
  geom_line(aes(y = 100 * 2^(days_since_100 / 5)), linetype = 2, color = "grey") +
  theme_bw() +
  labs(x = "Days Since 100th Confirmed Case", y = "Total Confirmed Cases (log-scaled)", 
       title = "Confirmed COVID-19 Cases by Days Since 100th Confirmed Case",
       caption = paste0("Data From JHU CSSE COVID-19 Project. 
                        Updated as of: ", format(max(Deaths_plot$date), "%m/%d/%Y"))) +
  annotate("text", label = "Every 3 Days", x = 39, y = 900000) +
  annotate("text", label = "Cases Double Every 2 Days", x = 24, y = 900000) +
  annotate("text", label = "Every 5 Days", x = 60, y = 600000) +
  scale_x_continuous(breaks = seq(0,80,10)) +
  scale_color_brewer(palette = "Paired")

ggsave("days_since_100_cases.jpg", width = 16.1, height = 10, units = "in", dpi = "retina")


ggplot(data = Deaths_plot, aes(x = days_since_100, y = cases, color = `Country/Region`)) +
  geom_point(size = 2, show.legend = F) +
  geom_line(size = 0.75, show.legend = F) +
  scale_y_log10(labels = scales::comma, limits = c(10, 50000)) +
  geom_label_repel(data = Deaths_plot %>% group_by(`Country/Region`) %>% top_n(1, days_since_100),
                   aes(label = `Country/Region`), color = "black") +
  geom_line(aes(y = 10 * 2^(days_since_100 / 3)), linetype = 2, color = "grey") +
  geom_line(aes(y = 10 * 2^(days_since_100 / 2)), linetype = 2, color = "grey") +
  geom_line(aes(y = 10 * 2^(days_since_100 / 5)), linetype = 2, color = "grey") +
  theme_bw() +
  labs(x = "Days Since 10th Confirmed Death From COVID-19", y = "Total Deaths (log-scaled)", 
       title = "Confirmed COVID-19 Deaths by Days Since 10th Attributed Death",
       caption = paste0("Data From JHU CSSE COVID-19 Project. 
                        Updated as of: ", format(max(Deaths_plot$date), "%m/%d/%Y"))) +
  annotate("text", label = "Every 3 Days", x = 38, y = 50000) +
  annotate("text", label = "Deaths Double Every 2 Days", x = 22, y = 50000) +
  annotate("text", label = "Every 5 Days", x = 60, y = 50000) +
  scale_x_continuous(breaks = seq(0,80,10))

ggsave("days_since_10_deaths.jpg", width = 16.1, height = 10, units = "in", dpi = "retina")





NY <- NYT_states %>%
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(cases_100 = ifelse(cases >= 100, 1, 0),
         deaths_10 = ifelse(deaths >= 10, 1, 0)) %>%
  mutate(cases_100 = cumsum(cases_100) - 1,
         deaths_10 = cumsum(deaths_10) - 1) %>%
  filter(state %in% c("New York", "California", "Washington", "New Jersey", "Connecticut", "Florida", "Texas"))


ggplot(data = NY, aes(x = cases_100, y = cases, color = state)) +
  geom_point(size = 2, show.legend = F) +
  geom_line(size = .75, show.legend = F) +
  scale_y_log10(labels = scales::comma, limits = c(100, 200000)) +
  geom_label_repel(data = NY %>% group_by(state) %>% top_n(1, cases_100),
                   aes(label = state), color = "black", nudge_x = 1.5) +
  geom_line(aes(y = 100 * 2^(cases_100 / 3)), linetype = 2, color = "grey") +
  geom_line(aes(y = 100 * 2^(cases_100 / 2)), linetype = 2, color = "grey") +
  geom_line(aes(y = 100 * 2^(cases_100 / 5)), linetype = 2, color = "grey") +
  theme_bw() +
  labs(x = "Days Since 100th Confirmed Case", y = "Total Confirmed Cases (log-scaled)", 
       title = "Confirmed COVID-19 Cases by Days Since 100th Confirmed Case",
       caption = paste0("Data from NYT: https://github.com/nytimes/covid-19-data 
                        Updated as of: ", format(max(Deaths_plot$date), "%m/%d/%Y"))) +
  annotate("text", label = "Every 3 Days", x = 24, y = 25000) +
  annotate("text", label = "Cases Double Every 2 Days", x = 22, y = 175000) +
  annotate("text", label = "Every 5 Days", x = 24, y = 3000) +
  scale_x_continuous(breaks = seq(0,30,5)) +
  scale_color_brewer(palette = "Paired")


ggsave("states_days_since_100.jpg", width = 16.1, height = 10, units = "in", dpi = "retina")


ggplot(data = NY %>% filter(deaths >=10), aes(x = deaths_10, y = deaths, color = state)) +
  geom_point(size = 2, show.legend = F) +
  geom_line(size = .75, show.legend = F) +
  scale_y_log10(labels = scales::comma, limits = c(10, 20000)) +
  scale_x_continuous(breaks = seq(0,35,5)) +
  geom_label_repel(data = NY %>% group_by(state) %>% top_n(1, deaths_10),
                   aes(label = state), color = "black", nudge_x = 1.5) +
  geom_line(aes(y = 10 * 2^(deaths_10 / 3)), linetype = 2, color = "grey") +
  geom_line(aes(y = 10 * 2^(deaths_10 / 2)), linetype = 2, color = "grey") +
  geom_line(aes(y = 10 * 2^(deaths_10 / 5)), linetype = 2, color = "grey") +
  theme_bw() +
  labs(x = "Days Since 10th Confirmed Death", y = "Total Confirmed Deaths (log-scaled)", 
       title = "Confirmed COVID-19 Deaths by Days Since 10th Attributed Death",
       caption = paste0("Data from NYT: https://github.com/nytimes/covid-19-data 
                        Updated as of: ", format(max(Deaths_plot$date), "%m/%d/%Y"))) +
  annotate("text", label = "Every 3 Days", x = 27, y = 7000) +
  annotate("text", label = "Deaths Double Every 2 Days", x = 21, y = 15000) +
  annotate("text", label = "Every 5 Days", x = 27, y = 650) +
  scale_color_brewer(palette = "Paired")


ggsave("states_days_since_10deaths.jpg", width = 16.1, height = 10, units = "in", dpi = "retina")

NY_growth <- NY %>%
  filter(state == "New York", cases > 0) %>%
  mutate(cases_growth = (cases - lag(cases)) / (lag(cases) - lag(cases , 2)),
          deaths_growth = (deaths - lag(deaths)) / (lag(deaths) - lag(deaths , 2)))


ggplot(data = NY_growth %>% filter(cases >= 100), aes(x = cases_100, y = cases_growth)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 1, color = "red") +
  labs(x = "Days Since 100th Confirmed Case", y = "Daily Case Growth Factor", 
       title = "COVID-19 Confirmed Case Growth Rate in New York State",
       caption = paste0("Data from NYT: https://github.com/nytimes/covid-19-data 
                        Updated as of: ", format(max(Deaths_plot$date), "%m/%d/%Y")))
ggsave("NYS_case_growth_rate.jpg", width = 16.1, height = 10, units = "in", dpi = "retina")


ggplot(data = NY_growth %>% filter(deaths >= 10), aes(x = deaths_10, y = deaths_growth)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 1, color = "red") +
  labs(x = "Days Since 10th Confirmed Death", y = "Daily Morbidity Growth Factor", 
       title = "COVID-19 Morbidity Daily Growth Rate in New York State",
       caption = paste0("Data from NYT: https://github.com/nytimes/covid-19-data 
                        Updated as of: ", format(max(Deaths_plot$date), "%m/%d/%Y"))) +
  scale_y_log10()
ggsave("NYS_death_growth_rate.jpg", width = 16.1, height = 10, units = "in", dpi = "retina")




### Make gif of cases over time across the US
if (require("maps")) {
  states <- map_data("state")
  
  NYT_last_state_obs <- NYT_states %>%
    # group_by(state) %>%
    # top_n(1, date) %>%
    mutate(region = tolower(state))
  
  choro <-
    merge(states, NYT_last_state_obs, sort = FALSE, by = "region")
  choro <- choro[order(choro$order),]
  anim <-  ggplot(choro, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = cases),
                 color = "black",
                 show.legend = F) +
    scale_fill_gradient(
      low = "white",
      high = "red",
      trans = "log10",
      na.value = "white"
    ) +
    gganimate::transition_reveal(date) +
    theme(legend.position = "right") +
    labs(
      title = "Confirmed COVID-19 Cases Across the United States",
      x = "",
      y = "",
      subtitle = "{frame_along}"
    ) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL)# +
  # coord_map("albers",  at0 = 45.5, lat1 = 29.5)
  gganimate::anim_save("cases_across_US2.gif",
                       anim,
                       end_pause = 50)
}















