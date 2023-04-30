

# Load the required libraries
install.packages("tidyverse")
library(tidyverse)
install.packages("sf")
library(sf)
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("lwgeom")
library(lwgeom)
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
install.packages("tidyr")
library(tidyr)


# Import the unicef_indicator_2.csv file
data <- read_csv("unicef_indicator_2.csv")

#Clean and transform the data
clean_data <- data %>%
  drop_na() %>%
  filter(!is.na(obs_value) & obs_value >= 0)

# Filter for the top 50 countries with the highest obs_value
top_50 <- clean_data %>%
  group_by(country) %>%
  summarise(total_obs_value = sum(obs_value)) %>%
  top_n(50, total_obs_value)

install.packages("dplyr")
library(dplyr)

install.packages("sf")
library(sf)

install.packages("devtools")
library(devtools)

# Load World Map dataset and make the geometry valid
world_map <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>%
  st_make_valid()

# Create a lookup table to map country names to ISO A2 codes and join with top_20
top_50 <- top_50 %>%
  left_join(world_map %>% select(iso_a2, name_long) %>% distinct(), by = c("country" = "name_long"))

install.packages("dplyr")
library(dplyr)

# Merge world_map and top_50 data frames
merged_data <- left_join(world_map, top_50, by = "iso_a2")

install.packages("ggplot2")
library(ggplot2)

# Plot 50 countries with the highest obs_value
world_map_chart <- ggplot() +
  geom_sf(data = merged_data, aes(fill = total_obs_value), color = "white") +
  scale_fill_continuous(low = "green", high = "lightyellow", na.value = "lightblue", name = "Obs Value") +
  labs(title = "Top 50 Countries with the Highest number of skilled birth attendents)") +
  theme_minimal()
world_map_chart

unicef_metadata <- read.csv("unicef_metadata.csv")
data_selected <- unicef_metadata %>%
  select(country, Population..total, Life.expectancy.at.birth..total..years. )


bottom_50 <- merged_data %>%
  arrange(total_obs_value) %>%
  head(50)

install.packages("sf")
library(sf)

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

# Bar chart: World bank regions with the lowest number of skilled birth attendants
bar_chart <- ggplot(bottom_50, aes(x = reorder(region_wb, total_obs_value), y = total_obs_value)) +
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  labs(title = "World Bank's regional classification of skilled birth attendent",
       x = "Region",
       y = "Skilled birth attendent (total obs value)") +
  theme_minimal()
bar_chart

install.packages("dplyr")
library(dplyr)

# Create a factor variable for time period
clean_data$time_period_cat <- factor(clean_data$time_period, levels = c("2018","2019"))
# Create the time series chart with facets for time period
ggplot(clean_data, aes(x = time_period_cat, y = obs_value, group = country)) +
  geom_line() +
  facet_wrap(~ indicator, ncol = 2) +
  ggtitle("Observed Value for Country by Time Period") +
  xlab("Time Period") +
  ylab("% observation value") +
  theme_light()


# Load the clean data and add a row number column
clean_data$id <- 1:nrow(clean_data)

# Join the two datasets on the row number column
data_combined <- left_join(data_selected, clean_data, by = "country",relationship = "many-to-many")

# Create the scatter plot with linear regression line
ggplot(data_combined, aes(x = Life.expectancy.at.birth..total..years., y = Population..total, color = obs_value )) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "orange") +
  ggtitle("Population vs Life Expectancy by Observed Value") +
  xlab("Life Expectancy at birth") +
  ylab("Total Population") +
  theme_minimal()


