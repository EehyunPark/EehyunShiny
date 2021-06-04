library(tidyverse)
library(readxl)

load("Data/help_dataset.RData")


gdp_data_1 <- gdp_data %>%
  select(GeoAreaName, TimePeriod, Value)
gdp_data_2 <- gdp_data_1 %>%
  group_by(GeoAreaName) %>%
  dplyr::summarise(
    mean_rate = mean(Value)
  )

gdp_data_3 <- gdp_data_2 %>%
  mutate(rank = rank(-mean_rate, na.last = TRUE, ties.method = "last")) %>%
  arrange(rank) %>%
  mutate(country_group = case_when(mean_rate < 3 ~ "Less-Tourism",
                                   mean_rate >= 3 & mean_rate < 5 ~ "Medium-Tourism",
                                   mean_rate >= 5 ~ "More-Tourism"))




continent_data <- continent_data %>%
  select(GeoAreaName, continent, sub_region)


mixed_country_data <- gdp_data_3 %>%
  left_join(continent_data, by = "GeoAreaName")

mixed_country_data_gdp <- gdp_data_3 %>%
  left_join(continent_data, by = "GeoAreaName") %>%
  filter(sub_region == "Eastern Asia" | sub_region == "South-Eastern Asia" | continent == "Europe" | continent == "Oceania" | sub_region == "Northern America")



flight_data_2 <- flight_data %>%
  separate(Flighttime, c("hour","minute"), -2)

flight_data_2$hour <- as.integer(flight_data_2$hour)
flight_data_2$minute <- as.integer(flight_data_2$minute)

flight_data_3 <- flight_data_2 %>%
  mutate(flight_time = 60*hour + minute) %>%
  mutate(rank = min_rank(flight_time)) %>%
  arrange(flight_time)

flight_data_final <- flight_data_3 %>%
  select(-City, -hour, -minute) %>%
  group_by(Country) %>%
  summarise(flight_time = mean(flight_time)) %>%
  mutate(rank = min_rank(flight_time)) %>%
  arrange(flight_time)

flight_data_final <- rename(flight_data_final, GeoAreaName = Country)



continent_data <- continent_data %>%
  select(GeoAreaName, continent, sub_region)

mixed_country_data_flight <- flight_data_final %>%
  left_join(continent_data, by = "GeoAreaName") %>%
  filter(sub_region == "Eastern Asia" | sub_region == "South-Eastern Asia" | continent == "Europe" | continent == "Oceania" | sub_region == "Northern America")

library(tidyverse)
library(lubridate)


corona_data_Global_2 <- corona_data_Global %>%
  pivot_longer(cols = 5:457,
               names_to = "date", values_to = "cases") %>%
  separate(date, sep="[\\/\\-]",
           into = c("month","day","year"),
           remove = FALSE, fill = "right") %>%
  select(-`Province/State`, -Lat, -Long)

corona_data_Global_2$month <- as.numeric(corona_data_Global_2$month)
corona_data_Global_2$day <- as.numeric(corona_data_Global_2$day)
corona_data_Global_2$year <- as.numeric(corona_data_Global_2$year)

corona_data_Global_3 <- corona_data_Global_2 %>%
  mutate(month = case_when(month > 2000 ~ month - 2000,
                           month < 2000 ~ month)) %>%
  select(-date)

corona_data_Global_4 <- corona_data_Global_3 %>%
  group_by(Country, year, month, day) %>%
  summarise(cases = sum(cases), .groups = "drop")

corona_data_US_2 <- corona_data_US %>%
  pivot_longer(cols = 12:464,
               names_to = "date", values_to = "cases") %>%
  separate(date, sep="[\\/\\-]",
           into = c("month","day","year"),
           remove = FALSE, fill = "right") %>%
  select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
  filter(Province_State == "Guam" | Province_State == "Hawaii" | Province_State == "Northern Mariana Islands")

corona_data_US_2$month <- as.numeric(corona_data_US_2$month)
corona_data_US_2$day <- as.numeric(corona_data_US_2$day)
corona_data_US_2$year <- as.numeric(corona_data_US_2$year)

corona_data_US_3 <- corona_data_US_2 %>%
  mutate(month = case_when(month > 2000 ~ month - 2000,
                           month < 2000 ~ month)) %>%
  select(-date)


corona_data_US_3_hawaii <- corona_data_US_3 %>%
  filter(Province_State == "Hawaii") %>%
  group_by(Province_State, year, month, day) %>%
  summarise(cases = sum(cases), .groups = "drop")


corona_data_US_3_Guam <- corona_data_US_3 %>%
  filter(Province_State == "Guam") %>%
  group_by(Province_State, year, month, day) %>%
  summarise(cases = sum(cases), .groups = "drop")


corona_data_US_3_Saipan <- corona_data_US_3 %>%
  filter(Province_State == "Northern Mariana Islands") %>%
  group_by(Province_State, year, month, day) %>%
  summarise(cases = sum(cases), .groups = "drop")


corona_data_US_4 <- rbind(corona_data_US_3_Guam, corona_data_US_3_hawaii,
                          corona_data_US_3_Saipan)

colnames(corona_data_US_4) <- c("Country", "year", "month", "day", "cases")


corona_data_mid <- rbind(corona_data_Global_4, corona_data_US_4)

corona_data_mid_US <- corona_data_mid %>%
  filter(Country == "United States")


corona_data_mid_US_2 <- corona_data_mid_US %>%
  mutate(cases = corona_data_mid_US$cases - corona_data_US_3_hawaii$cases) %>%
  mutate(cases = corona_data_mid_US$cases - corona_data_US_3_Guam$cases) %>%
  mutate(cases = corona_data_mid_US$cases - corona_data_US_3_Saipan$cases)

corona_data_mid_US_2

corona_data_final <- corona_data_mid %>%
  filter(Country != "United States") %>%
  rbind(corona_data_mid_US_2)


pop_data <- pop_data %>%
  select(Country, `Population (2020)`)


corona_data_propotion <- corona_data_final %>%
  group_by(Country) %>%
  summarise(mean = mean(cases))

mixed_corona_data <- corona_data_propotion %>% 
  left_join(pop_data, by = "Country") %>%
  drop_na('Population (2020)')

mixed_corona_data_2 <- mixed_corona_data %>%
  mutate(propotion = mean / `Population (2020)` *100)

mixed_country_data_corona <- rename(mixed_corona_data_2, GeoAreaName = Country)



continent_data <- continent_data %>%
  select(GeoAreaName, continent, sub_region)

mixed_country_data_corona <- mixed_country_data_corona %>%
  left_join(continent_data, by = "GeoAreaName") %>%
  filter(sub_region == "Eastern Asia" | sub_region == "South-Eastern Asia" | continent == "Europe" | continent == "Oceania" | sub_region == "Northern America") %>%
  mutate(rank = min_rank(propotion))

mixed_country_data_corona_1 <- mixed_country_data_corona %>%
  select(GeoAreaName,propotion)

library(caret)
cluster_1 <- mixed_country_data_gdp %>%
  left_join(mixed_country_data_corona_1, by = "GeoAreaName") 

cluster_2 <- cluster_1 %>%
  left_join(flight_data_final, by = "GeoAreaName") %>%
  select(-rank.y)

cluster_2 <- cluster_2[complete.cases(cluster_2), ] 

cluster_2.subset <- cluster_2 %>% 
  select(GeoAreaName, mean_rate, propotion, flight_time) %>%
  filter(GeoAreaName != "Macao") %>%
  mutate(mean_rate = (mean_rate - min(mean_rate)) / (max(mean_rate) - min(mean_rate))) %>%
  mutate(propotion = (propotion - min(propotion)) / (max(propotion) - min(propotion))) %>%
  mutate(statistics = propotion / mean_rate)

cluster_2.subset <- cluster_2.subset[!is.infinite(rowSums(cluster_2.subset[,5])),]

cluster_2.subset_feature <- cluster_2.subset[,4:5]

results <- kmeans(cluster_2.subset_feature, 4)

table(cluster_2.subset$GeoAreaName, results$cluster)
plot(cluster_2.subset[c("statistics", "flight_time")], col = results$cluster)

cluster_2.subset$cluster <- as.character(results$cluster)

cluster_2.subset_2 <- cluster_2.subset %>%
  filter(statistics < 4)

cluster_2.subset_2_feature <- cluster_2.subset_2[,4:5]

results <- kmeans(cluster_2.subset_2_feature, 4)

table(cluster_2.subset_2$GeoAreaName, results$cluster)
plot(cluster_2.subset_2[c("statistics", "flight_time")], col = results$cluster)


cluster_2.subset_fixed <- as.data.frame(cluster_2.subset)
cluster_2.subset_fixed <- cluster_2.subset_fixed[,-6]

save.image("C:/Users/dlgus/Desktop/Shiny/Data/help_dataset.RData")
