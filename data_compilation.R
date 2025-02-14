# RoRI Proposal - code to generate figures
# Load necessary libraries
library(tidyverse)
library(sf)

# All fires in Canada
# Download and unzip Canadian fire data
dl_dir <- file.path("Data/Fire_polys/Canada")
dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)
can_fire_shp <- list.files(dl_dir, pattern = ".shp$", full.names = TRUE)

if(!length(can_fire_shp)) {
  # URL for download
  url <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip"
  download.file(url, file.path(dl_dir, "NFDB_poly.zip"))
  unzip(file.path(dl_dir, "NFDB_poly.zip"), exdir = dl_dir)
  unlink(file.path(dl_dir, "NFDB_poly.zip"))
  can_fire_shp <- list.files(dl_dir, pattern = ".shp$", full.names = TRUE)
}

# Canadian dataset
# Transform the CRS to a common North American projection system (NAD83
# Albers Equal Area Conic, aka ESRI:102008), then filter the data to remove
# human caused fires, then rename the fire cause acronyms to be more explicit,
# and rename the columns to match the US dataset (later).
can_fires <- st_read(can_fire_shp, quiet = TRUE) %>% 
  st_transform("ESRI:102008") %>% 
  filter(CAUSE %in% c("U", "L", "Re", "n/a")) %>% 
  mutate(
    CAUSE = case_when(
      CAUSE == "U" ~ "Undetermined",
      CAUSE == "L" ~ "Lightning", 
      CAUSE == "Re" ~ "Reburn",
      .default = NA
    ),
    country = "Canada") %>% 
  select(country = country, fire_id = FIRE_ID, fire_year = YEAR, 
         size_ha = CALC_HA, fire_cause = CAUSE) %>% 
  rename(geometry = attributes(.)$sf_column)

# All fires in the US
dl_dir <- file.path("Data/Fire_polys/US")
dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)
us_fire_shp <- list.files(dl_dir, pattern = "Fire_Feature_Data.gdb", full.names = TRUE)

# NOTE: Users will need to download the US data manually. Go to this link and 
# select the "File_Feature_Data_Pro2_8_Geodatabase.zip" to begin the download 
# process: https://www.sciencebase.gov/catalog/item/61aa537dd34eb622f699df81
# Once it has been downloaded, simply place the .zip file into the file path
# given in dl_dir (i.e.: Data/Fire_polys/US/File_Feature_Data_Pro2_8_Geodatabase.zip)
# Note: GeoJSON is available, but it's very large and R doesn't seem to want
# to open it.

if(!length(us_fire_shp)) {
  # Unzip the geodatabase and remove the zip file
  z_file <- list.files(dl_dir, pattern = "Geodatabase.zip$", full.names = TRUE)
  unzip(z_file, exdir = dl_dir)
  unlink(z_file)
}

# US dataset
# Any fire with "Natural" listed in the cause class gets changed to "Lightning"
# Numbers are removed from the "cause_class" column; anything left that was 
# suspected to be human caused was removed from this dataset. Rename the columns
# of interest to match the Canadian dataset names.
us_fires <- st_read(us_fire_shp, quiet = TRUE,
                    layer = "USGS_Wildland_Fire_Combined_Dataset") %>% 
  mutate(Listed_Fire_Cause_Class = case_when(
    grepl("Natural", Listed_Fire_Cause_Class) ~ "Lightning",
    .default = Listed_Fire_Cause_Class),
    country = "US") %>% 
  mutate(Listed_Fire_Cause_Class = gsub("\\s*\\([^\\)]+\\)","", Listed_Fire_Cause_Class)) %>% 
  filter(!grepl("Human", Listed_Fire_Cause_Class)) %>% 
  select(country = country, fire_id = USGS_Assigned_ID, fire_year = Fire_Year, 
         size_ha = GIS_Hectares, fire_cause = Listed_Fire_Cause_Class) %>% 
  rename(geometry = attributes(.)$sf_column)

# Combine Canada and US, apply a date filter, and classify the fire sizes.
na_fires <- rbind(can_fires, us_fires) %>% 
  filter(between(fire_year, 1980, 2020)) %>% 
  mutate(
    decade = case_when(
      between(fire_year, 1980, 1989) ~ "1980-1989",
      between(fire_year, 1990, 1999) ~ "1990-1999",
      between(fire_year, 2000, 2009) ~ "2000-2009",
      between(fire_year, 2010, 2019) ~ "2010-2019",
      between(fire_year, 2020, 2029) ~ "2020-2029",
      .default = NA
    ),
    size_class = case_when(
      size_ha >= 200 ~ "large",
      size_ha >= 1 & size_ha < 200 ~ "medium",
      size_ha >= 0 & size_ha < 1  ~ "small",
      .default = NA
    ),
    .before = geometry) %>% 
  st_set_agr("constant")

# Generate a plot of North American fire frequency by latitude and by year
fire_by_latitude_data <- st_point_on_surface(na_fires) %>% 
  st_transform(4326) %>% 
  cbind(st_coordinates(.)) %>% 
  st_drop_geometry() %>%
  mutate(y_floor = floor(Y), x_floor = floor(X)) %>% 
  filter(fire_cause == "Lightning")

pretty_name <- c(large = "Large fires\n (above 200 ha)",
                 medium = "Medium fires\n (1 - 200 ha)",
                 small = "Small fires\n (below 1 ha)")

fire_by_latitude_plot <-  ggplot(fire_by_latitude_data, aes(x = Y)) +
  geom_histogram(binwidth = 1) +
  facet_grid(
    size_class ~ decade,
    labeller = labeller(size_class = pretty_name)) +
  coord_flip() +
  scale_x_continuous(labels = function(x) paste0(x, "\u00b0N")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 410))+
  theme_bw(base_size = 20) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.ticks =  element_line(color = "black"),
    strip.text = element_text(face = "bold"),
    strip.text.y = element_text(angle = 0),
    strip.background.y = element_blank(),
    panel.spacing = unit(1.2, "lines")) +
  labs(x = "Latitude", y = "Frequency of lightning caused fires") 
fire_by_latitude_plot

# Save the file
dir.create("Figures", showWarnings = FALSE)
ggsave(fire_by_latitude_plot, file = "Figures/fire_by_latitude.png", 
       height = 9, width = 16, units = "in")

# Create a plot of the trend of wildfire
fire_trend_data <- st_drop_geometry(na_fires) %>% 
  group_by(fire_year, fire_cause, size_class) %>% 
  tally() %>%
  filter(fire_cause == "Lightning")

fire_trend_plot <- ggplot(fire_trend_data, aes(y = n, x = fire_year)) +
  geom_point(pch = 21, fill = "steelblue", size = 4, alpha = 0.5)+
  geom_smooth(
    linewidth = 1.5, method = "lm", color = "black", linetype = "dashed") +
  scale_colour_viridis_d(begin = 0.2, end = 0.8) +
  facet_grid(
    ~size_class,
    labeller = labeller(size_class = pretty_name)) +
  theme_bw(base_size = 20) +
  labs(y = "Frequency of \nlightning caused fires", x = "") +
  theme(
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(2.5, "lines"))
fire_trend_plot

ggsave(fire_trend_plot, file = "Figures/fire_trend_plot.png", 
       height = 4.5, width = 12, units = "in")

# Create a plot of cumulative burned area per year
tot_burned_area_data <- st_drop_geometry(na_fires) %>% 
  group_by(fire_year, size_class) %>% 
  summarise(total_burned_area = sum(size_ha), .groups = "drop")

tot_burned_area_plot <- ggplot(
  tot_burned_area_data, aes(x = fire_year, y = total_burned_area)) +
  geom_point(pch = 21, fill = "steelblue", size = 3, alpha = 0.5) +
  geom_smooth(
    linewidth = 1.5, method = "lm", color = "black", linetype = "dashed") +
  scale_colour_viridis_d(begin = 0.2, end = 0.8) +
  facet_wrap(
    ~size_class,
    labeller = labeller(size_class = pretty_name),
    scales = "free") +
  theme_bw(base_size = 20) +
  labs(y = "Total burned area (ha)", x = "") +
  theme(
    axis.text = element_text(color = "black"),
    axis.ticks =  element_line(color = "black"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::scientific)
tot_burned_area_plot

ggsave(tot_burned_area_plot, file = "Figures/tot_burned_area.png", 
       height = 5, width = 13, units = "in")
