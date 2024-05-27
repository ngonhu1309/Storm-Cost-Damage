# Storm-Cost-Damage

## Data Source

The data used in this project is sourced from NOAA Storm Event Data.
- Description: https://www.ncdc.noaa.gov/stormevents/Links to an external site.
- Storm events by year: https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/Links to an external site
- NOTE: Data since the 1950s on specific storm events (think the Tornado outbreak in Dayton 2022); thunderstorms, tornado, floods.

## Requirements

To run this project, you will need the following software and packages:

- R (version 4.0.0 or higher)
- RStudio
- The following R packages:
  - `lubridate`
  - `kableExtra`
  - `ggplot2`
  - `dplyr`
  - `maps`
  - `sf`
  - `datasets`
  - `ggthemes`
  - `ggmosaic`
  - `tidyr`

## Installation

1. Clone the repository to your local machine:

    ```bash
    git clone https://github.com/ngonhu1309/Storm-Cost-Damage.git
    ```

2. Open the project in RStudio.

3. Install the required packages if you haven't already:

    ```R
    install.packages(c("lubridate", "kableExtra", "sf", "ggplot2", "dplyr", "maps", "datasets", "ggthemes", "ggmosaic", "tidyr"))
    ```
## Usage

1. Load the libraries:

    ```R
    library(dplyr)
    library(lubridate)
    library(kableExtra)
    library(ggplot2)
    library(maps)
    library(sf)
    library(datasets)
    library(ggthemes)
    library(ggmosaic)
    library(tidyr)
    ```

2. Load the data into R:

    ```R
    # Change the data location if needed
    df_2022 <- read.table(gzfile("C:/Users/Nhu Ngo/Documents/STA 404/Project/StormEvents_details-ftp_v1.0_d2022_c20230317.csv.gz"), header = TRUE, sep = ",") 
    ```
    
3. Select the necessary data:

    ```R
    df_22 <- df_2022 %>% select(EPISODE_ID, EVENT_ID, EVENT_TYPE, STATE, STATE_FIPS, BEGIN_DATE_TIME, END_DATE_TIME,CZ_TIMEZONE,
                                INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY,
                                DAMAGE_CROPS, FLOOD_CAUSE, EPISODE_NARRATIVE, EVENT_NARRATIVE, BEGIN_LAT, BEGIN_LON,
                                END_LAT, END_LON)
    ```

4. Manipulate the data:

    ```R
    ## Change the time zone into the EST time zone
    df_22 <- df_22 %>% mutate(Begin_Date_Time_Timezone = as.POSIXct(dmy_hms(BEGIN_DATE_TIME), tz = CZ_TIMEZONE),
                              Begin_Date_Time = lubridate::with_tz(Begin_Date_Time_Timezone, "America/New_York"), 
                              End_Date_Time_Timezone = as.POSIXct(dmy_hms(END_DATE_TIME), tz = CZ_TIMEZONE),
                              End_Date_Time = lubridate::with_tz(End_Date_Time_Timezone, "America/New_York"),
                              Duration = hour(seconds_to_period(as.difftime(ymd_hms(END_DATE_TIME) - ymd_hms(BEGIN_DATE_TIME)))),
                              Damage_Crops = gsub("K", "", DAMAGE_CROPS)) %>%
                      select(EPISODE_ID, EVENT_ID, EVENT_TYPE, STATE, STATE_FIPS, Begin_Date_Time, End_Date_Time, Duration,
                             INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY,
                             DAMAGE_CROPS, FLOOD_CAUSE, EPISODE_NARRATIVE, EVENT_NARRATIVE, BEGIN_LAT, BEGIN_LON,
                             END_LAT, END_LON)

    ## Change the data in the damage cost into number
    df_22.final <- df_22 %>% mutate(Damage_Crops = as.integer(gsub("K", "", DAMAGE_CROPS)) * 1000,
                                    Damage_Property = as.integer(gsub("K", "", DAMAGE_PROPERTY)) * 1000)
    ```

5. Select only events that did not impact the people's lives:

    ```R
    df_22_no <- df_22.final %>% group_by(EVENT_TYPE) %>%
                                summarize(Indirect = sum(INJURIES_INDIRECT) + sum(DEATHS_INDIRECT),
                                          Direct = sum(INJURIES_DIRECT) + sum(DEATHS_DIRECT),
                                          Crops = sum(Damage_Crops, na.rm = TRUE),
                                          Property = sum (Damage_Property, na.rm = TRUE),
                                          Cost = Crops + Property) %>%
                                filter(Indirect == 0 & Direct == 0) %>%
                                select(EVENT_TYPE, Indirect, Direct, Crops, Property, Cost) %>%
                                arrange(desc(Cost))

    kbl(df_22_no) %>% kable_paper("striped", full_width = F) %>%
                      row_spec(1:5, bold = T, color = "white", background = "#D7261E")
    ```
<p align="center">
  <img src="https://github.com/ngonhu1309/Storm-Cost-Damage/assets/135569353/f83fa4e3-2980-4edf-a0c3-609583fac221" alt="image1">
</p>

6. Create a scatter plot to compare the top 5 events:

    ```R
    ## Change the time zone into the EST time zone
    df_22 <- df_22 %>% mutate(Begin_Date_Time_Timezone = as.POSIXct(dmy_hms(BEGIN_DATE_TIME), tz = CZ_TIMEZONE),
                              Begin_Date_Time = lubridate::with_tz(Begin_Date_Time_Timezone, "America/New_York"), 
                              End_Date_Time_Timezone = as.POSIXct(dmy_hms(END_DATE_TIME), tz = CZ_TIMEZONE),
                              End_Date_Time = lubridate::with_tz(End_Date_Time_Timezone, "America/New_York"),
                              Duration = hour(seconds_to_period(as.difftime(ymd_hms(END_DATE_TIME) - ymd_hms(BEGIN_DATE_TIME)))),
                              Damage_Crops = gsub("K", "", DAMAGE_CROPS)) %>%
                        select(EPISODE_ID, EVENT_ID, EVENT_TYPE, STATE, STATE_FIPS, Begin_Date_Time, End_Date_Time, Duration,
                               INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY,
                               DAMAGE_CROPS, FLOOD_CAUSE, EPISODE_NARRATIVE, EVENT_NARRATIVE, BEGIN_LAT, BEGIN_LON,
                               END_LAT, END_LON)

    ## Change the data in the damage cost into number
    df_22.final <- df_22 %>% mutate(Damage_Crops = as.integer(gsub("K", "", DAMAGE_CROPS)) * 1000,
                                    Damage_Property = as.integer(gsub("K", "", DAMAGE_PROPERTY)) * 1000)
    ```
    
<p align="center">
  <img src="https://github.com/ngonhu1309/Storm-Cost-Damage/assets/135569353/8e39618c-77ab-47e1-9db0-ecb456cf2a07" alt="image2">
</p>


7. Select the events that have impacts on the humans'lives:

    ```R
    df_22_yes <- df_22.final %>% group_by(EVENT_TYPE) %>%
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
    ```

8. Create a scatter plot to compare these events:

    ```R
    ggplot(df_22_yes, aes(x = Property, y = Crops, size = Cost, color = Damage)) + geom_point() +
    scale_color_manual(values = c("red", "blue", "green3")) +
    labs(title = "Relationship between Property Damage Cost and the Crops Damage Cost",
         subtitle = "These events have impacts people's lives.",
         caption = "Source: NOAA Storm Event Data") +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_size_continuous(name = "Total Cost:",
                          breaks = c(0, 30000000, 60000000, 90000000),
                          labels=scales::dollar_format()) +
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
    ```
<p align="center">
  <img src="https://github.com/ngonhu1309/Storm-Cost-Damage/assets/135569353/b5f4bad0-1e41-492a-81ed-66984e492d16" alt="image3">
</p>

  9. Create a map to compare the total damage cost in USA:

    ```R
    df_22_state <- df_22.final %>% group_by(STATE, STATE_FIPS) %>%
                                   summarize(sum = sum(Damage_Crops, na.rm = TRUE) + sum(Damage_Property, na.rm = TRUE))
    
    us_map <- map_data("state")
    
    my_table <- us_map %>% mutate(Name = toupper(region))
    
    df_state <- inner_join(my_table, df_22_state, by = c("Name" = "STATE"))
    
    ggplot(df_state) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = sum)) +
    geom_polygon(data = dplyr::filter(df_state, sum == 1000), mapping=aes(x = long, y = lat, group = group), fill = "green", color= "black", linewidth = 1) +
    geom_polygon(data=dplyr::filter(df_state, sum == 30956000), mapping=aes(x = long, y = lat, group = group), fill = "red", color= "black", linewidth = 1) +
    labs(title = "The Damage Cost in 56 States in USA in 2022",
         subtitle = "The lowest damage cost is Delaware, and the highest damage cost is Alabama",
         caption = "Source: NOAA Storm Event Data") +
    scale_fill_continuous(name = "Damage Cost", labels=scales::dollar_format()) +
    theme(legend.position = "right") +
    theme_map()
    ```
<p align="center">
  <img src="https://github.com/ngonhu1309/Storm-Cost-Damage/assets/135569353/e2c487a2-5950-4ce9-98d6-7909655b4d86" alt="image4">
</p>

10. Select the data to calculate the percentage of damage cost in every month:

    ```R
    df_22.Month <- df_22.final %>% mutate(Date = as.character.Date(Begin_Date_Time),
                                          Month = month(Date),
                                          Year = year(Date)) %>%
                                   filter(Year != 2021)

    df <- df_22.Month %>% group_by(Month) %>%
                          summarise(ValueC = sum(Damage_Crops, na.rm = TRUE),
                                    ValueP = sum(Damage_Property, na.rm = TRUE),
                                    Total = ValueC + ValueP)

    df_crop <- df %>% group_by(Month) %>%
                      summarize(PercentC = ValueC/Total)

    df_property <- df %>% group_by(Month) %>%
                          summarize(PercentP = ValueP/Total)

    df.mosaic <- full_join(df_crop, df_property, by = c("Month"))

    df.mosaic <- pivot_longer(df.mosaic, cols = starts_with("Percent"), names_to = "Percent")

    df.mosaic$Type <- with(df.mosaic, ifelse(Percent == "PercentC", "Crop", "Property"))
    ```

11. Create a mosaic plot to compare the months in 2022:

    ```R
    ggplot(df.mosaic) + geom_mosaic(aes(x = product(Month), fill = Type, weight = value)) +
                        labs(x = "Month", y = "Damage Cost",
                             title = "The Proportion of Damge Cost by the Type of Damage For Every Month\nin 2022",
                             subtitle = "From April to October, the damage cost in crops is outstanding, so there must be a\nspecial event that influnce these costs.",
                             caption = "Source: NOAA Storm Event Data") + theme_minimal()
    ```
<p align="center">
  <img src="https://github.com/ngonhu1309/Storm-Cost-Damage/assets/135569353/5ccf7b7c-c977-44b8-aaef-3cc6e6d64068" alt="image5">
</p>

12. Filter the data to look closer from April to October:

    ```R
    df_state_month <- df_22.Month %>% filter(Month == 4 | Month == 5 | Month == 6 | Month == 7 | Month == 8 | Month == 9 | Month == 10) %>%
                                      select(STATE, Month, Damage_Crops, EVENT_TYPE)

    df_state_month_drought <- df_state_month %>% filter(EVENT_TYPE == "Drought") %>% filter(Damage_Crops != 0)

    df_state_month <- df_state_month_drought %>% group_by(STATE, Month) %>% summarise(Crops = sum(Damage_Crops, na.rm = TRUE))
    ```
    
13. Create a scatter plot to compare the damage cost in crops:

    ```R
    ggplot(df_state_month, aes(x = Month, y = Crops, color = STATE)) +
    geom_point(size = 3) +
    scale_color_manual(name = "State", values=c("red", "blue", "green", "purple")) +
    labs(x = "Crops Damage Cost",
         title = "Relationship between Damage Cost during the Drought Time",
         subtitle = "Drought Time is from Apirl to October",
         caption = "Source: NOAA Storm Event Data") +
    scale_y_continuous(labels=scales::dollar_format()) +
    scale_x_continuous(breaks = 4:10, labels = 4:10) +
    theme_minimal()
    ```
<p align="center">
  <img src="https://github.com/ngonhu1309/Storm-Cost-Damage/assets/135569353/b0477e66-8b98-4cc0-ac12-baef57fc44eb" alt="image6">
</p>
    
14. Calculate the 95% confidence interval:

    ```R
    df_CI <- df_state_month_drought %>% group_by(Month) %>%
                                        summarize(N = n(),
                                                  Average_Crops = mean(Damage_Crops),
                                                  sd = sd(Damage_Crops),
                                                  Lo.int = Average_Crops - qt((1-0.05/2/6), N-1)*sd/sqrt(N),
                                                  Hi.int = Average_Crops + qt((1-0.05/2/6), N-1)*sd/sqrt(N))
    ```

  15. Create a plot for 95% confidence interval:

    ```R
    ggplot(df_CI, aes(x = Month)) + 
    geom_errorbar(aes(ymin = Lo.int, ymax = Hi.int), width=0.25) + 
    geom_point(aes(y = Average_Crops), size=2.5) +
    labs(y = "Crops Damage Cost",
         title = "The Damage Cost in Crops in Six Months during the Drought Event",
         subtitle = "This is a Bonferoni adjusted 95% confidence inteverals reporting the crops damage cost\nduring the drought months",
         caption = "Source: NOAA Storm Event Data") +
    scale_y_continuous(labels=scales::dollar_format()) +
    scale_x_continuous(breaks = 4:10, labels = 4:10) +
    theme_minimal()
    ```
<p align="center">
  <img src="https://github.com/ngonhu1309/Storm-Cost-Damage/assets/135569353/48a0cacc-e52a-4739-8b68-af5f020f85b4" alt="image6">
</p>

10. Run the script to generate the visualization.


## Contributing

Contributions are welcome! Please fork the repository and create a pull request with your changes. Ensure that your code adheres to the existing style and include appropriate tests.

## License

This project is licensed under the MIT License.

## Credits

Author: Nhu Ngo
