## Call the library
library(dplyr)
library(lubridate)
library(kableExtra)
library(ggplot2)

## Read the data in 2022
df_2022 <- read.table(gzfile("C:/Users/Nhu Ngo/Documents/STA 404/Project/StormEvents_details-ftp_v1.0_d2022_c20230317.csv.gz"), header = TRUE, sep = ",")

## Choose the data types 
df_22 <- df_2022 %>%
  select(EPISODE_ID, EVENT_ID, EVENT_TYPE, STATE, BEGIN_DATE_TIME, END_DATE_TIME,CZ_TIMEZONE,
         INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY,
         DAMAGE_CROPS, FLOOD_CAUSE, EPISODE_NARRATIVE, EVENT_NARRATIVE, BEGIN_LAT, BEGIN_LON,
         END_LAT, END_LON)

## Clean the data
df_22 <- df_22 %>%
  mutate(Begin_Date_Time_Timezone = as.POSIXct(dmy_hms(BEGIN_DATE_TIME), tz = CZ_TIMEZONE),
         Begin_Date_Time = lubridate::with_tz(Begin_Date_Time_Timezone, "America/New_York"), 
         End_Date_Time_Timezone = as.POSIXct(dmy_hms(END_DATE_TIME), tz = CZ_TIMEZONE),
         End_Date_Time = lubridate::with_tz(End_Date_Time_Timezone, "America/New_York"),
         Duration = hour(seconds_to_period(as.difftime(ymd_hms(END_DATE_TIME) - ymd_hms(BEGIN_DATE_TIME)))),
         Damage_Crops = gsub("K", "", DAMAGE_CROPS)) %>%
  select(EPISODE_ID, EVENT_ID, EVENT_TYPE, STATE, Begin_Date_Time, End_Date_Time, Duration,
         INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY,
         DAMAGE_CROPS, FLOOD_CAUSE, EPISODE_NARRATIVE, EVENT_NARRATIVE, BEGIN_LAT, BEGIN_LON,
         END_LAT, END_LON)

## Change the cost of crops damage and property into number
df_22.1 <- df_22 %>%
  mutate(Damage_Crops = as.integer(gsub("K", "", DAMAGE_CROPS)) * 1000,
         Damage_Property = as.integer(gsub("K", "", DAMAGE_PROPERTY)) * 1000)

## Filter the data that does not injure anyone
df_22_no <- df_22.1 %>%
  group_by(EVENT_TYPE) %>%
  summarize(Indirect = sum(INJURIES_INDIRECT) + sum(DEATHS_INDIRECT),
            Direct = sum(INJURIES_DIRECT) + sum(DEATHS_DIRECT),
            Crops = sum(Damage_Crops, na.rm = TRUE),
            Property = sum (Damage_Property, na.rm = TRUE),
            Cost = Crops + Property) %>%
  filter(Indirect == 0 & Direct == 0) %>%
  select(EVENT_TYPE, Indirect, Direct, Crops, Property, Cost) %>%
  arrange(desc(Cost))

## Create a table
kbl(df_22_no) %>%
  kable_paper("striped", full_width = F) %>%
  row_spec(1:5, bold = T, color = "white", background = "#D7261E")

## Take the first five rows
df_22_no_top5 <- df_22_no %>%
  slice(0:5)

## Crate a ggplot to compare the damage cost
ggplot(df_22_no_top5, aes(x = Property, y = Crops, color = EVENT_TYPE, size = Cost)) +
  geom_point() +
  scale_color_manual(values=c("blue", "red", "green", "purple", "orange")) +
  labs(title = "Relationship between Property Damage Cost and the Total Cost",
       caption = "Source: NOAA Storm Event Data") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  annotate(geom = "label", x = 1500000, y = 10000, 
           label = "Size conrresponds\nto the total cost",
           color = "gray60", size = 2)

## Filter the data that injure people
df_22_yes <- df_22.1 %>%
  group_by(EVENT_TYPE) %>%
  summarize(Indirect = sum(INJURIES_INDIRECT) + sum(DEATHS_INDIRECT),
            Direct = sum(INJURIES_DIRECT) + sum(DEATHS_DIRECT),
            Crops = sum(Damage_Crops, na.rm = TRUE),
            Property = sum (Damage_Property, na.rm = TRUE),
            Cost = Crops + Property) %>%
  mutate(Damage = ifelse(Crops > Property, "Crops", 
                         ifelse(Crops < Property, "Property", "No Cost"))) %>%
  filter(Indirect != 0 | Direct != 0) %>%
  select(EVENT_TYPE, Indirect, Direct, Crops, Property, Cost, Damage) %>%
  arrange(desc(Cost))

## Create a ggplot to compare the cost damage
ggplot(df_22_yes, aes(x = Property, y = Crops, size = Cost, color = Damage)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green3")) +
  labs(title = "Relationship between Property Damage Cost and the Total Cost",
       caption = "Source: NOAA Storm Event Data") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_size_continuous(name = "Total Cost:",
                        breaks = c(0, 30000000, 60000000, 90000000),
                        labels = scales::comma_format()) +
  annotate(geom = "text", x = 100000000, y = 7000000, 
           label = "Thunderstorm\nWind", color = "green4",
           fontface = "bold", size = 3.0) +
  annotate(geom = "text", x = 10000000, y = 17000000, 
           label = "Drought", color = "red",
           fontface = "bold", size = 3.0) +
  theme_minimal() +
  annotate(geom = "label", x = 95000000, y = 16000000, 
           label = "Size conrresponds\nto the total cost",
           color = "gray60", size = 2) 

## Read the data in 2021
df_2021 <- read.table(gzfile("C:/Users/Nhu Ngo/Documents/STA 404/Project/StormEvents_details-ftp_v1.0_d2021_c20230317.csv.gz"), header = TRUE, sep = ",")

## Choose only the data I use
df_21 <- df_2021 %>%
  select(EPISODE_ID, EVENT_ID, EVENT_TYPE, STATE, BEGIN_DATE_TIME, END_DATE_TIME,CZ_TIMEZONE,
         INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY,
         DAMAGE_CROPS, FLOOD_CAUSE, EPISODE_NARRATIVE, EVENT_NARRATIVE, BEGIN_LAT, BEGIN_LON,
         END_LAT, END_LON)

## Clean the data
df_21 <- df_21 %>%
  mutate(Begin_Date_Time_Timezone = as.POSIXct(dmy_hms(BEGIN_DATE_TIME), tz = CZ_TIMEZONE),
         Begin_Date_Time = lubridate::with_tz(Begin_Date_Time_Timezone, "America/New_York"), 
         End_Date_Time_Timezone = as.POSIXct(dmy_hms(END_DATE_TIME), tz = CZ_TIMEZONE),
         End_Date_Time = lubridate::with_tz(End_Date_Time_Timezone, "America/New_York"),
         Duration = hour(seconds_to_period(as.difftime(ymd_hms(END_DATE_TIME) - ymd_hms(BEGIN_DATE_TIME))))) %>%
  select(EPISODE_ID, EVENT_ID, EVENT_TYPE, STATE, Begin_Date_Time, End_Date_Time, Duration,
         INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY,
         DAMAGE_CROPS, FLOOD_CAUSE, EPISODE_NARRATIVE, EVENT_NARRATIVE, BEGIN_LAT, BEGIN_LON,
         END_LAT, END_LON)