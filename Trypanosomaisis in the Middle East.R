############################################################
# 0. PACKAGES
############################################################
library(dplyr)
library(tibble)
library(stringr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(patchwork)

############################################################
# 1. HELPER: "7/1005" -> numeric proportion
############################################################
calc_pct <- function(x) {
  if (is.na(x)) return(NA_real_)
  eval(parse(text = x))
}

############################################################
# 2. BASE DATASETS (old Pakistan / Sudan / Middle East)
############################################################

# --- Pakistan old ---
pakistan_old <- tribble(
  ~Country, ~Region, ~Host, ~Year, ~SampleSize, ~Microscopy_raw, ~PCR_raw, ~Serology_raw, ~Notes,
  
  "Pakistan","Cholistan Desert (Punjab)","Camel","2015",1005,
  "7/1005","320/1005","479/1005","FGT, CATT, ELISA, TL",
  
  "Pakistan","Southern Punjab","Camel","2024",48,
  "22/48","6/48",NA,"Muzaffargarh/Lodhran/Bahawalpur",
  
  "Pakistan","Balochistan","Camel","2023",393,
  NA,"111/393",NA,"PCR only",
  
  "Pakistan","Attock District (Punjab)","Horse","2019",100,
  "11/100",NA,NA,NA,
  
  "Pakistan","Attock District (Punjab)","Donkey","2019",100,
  "9/100",NA,NA,NA,
  
  "Pakistan","Southern Punjab","Cattle","2024",48,
  "9/48","3/48",NA,NA,
  
  "Pakistan","Southern Punjab","Buffalo","2024",48,
  "4/48","0/48",NA,NA,
  
  "Pakistan","Southern Punjab","Goat","2024",48,
  "9/48","4/48",NA,NA,
  
  "Pakistan","Southern Punjab","Sheep","2024",48,
  "9/48","1/48",NA,NA
)

# --- Sudan old ---
sudan_old <- tribble(
  ~Country, ~Region, ~Host, ~Year, ~SampleSize, ~Microscopy_raw, ~PCR_raw, ~Serology_raw, ~Notes,
  
  "Sudan","Western Sudan","Camel","2015",189,
  "13/189","70/189",NA,"Mixed infections (T. vivax 25%)"
)

# --- Middle East old (partial) ---
middle_east_old <- tribble(
  ~Country, ~Region, ~Host, ~Year, ~SampleSize, ~Microscopy_raw, ~PCR_raw, ~Serology_raw, ~Notes,
  
  "Saudi Arabia","Al-Jouf","Camel","2016",195,
  NA,"49/195","4/118","ELISA 3.4%",
  
  "Saudi Arabia","Taif (Makkah)","Camel","2022",102,
  "2/102","16/102",NA,"10/102 type A",
  
  "Saudi Arabia","Dammam (Eastern Region)","Camel","2024",350,
  "24/350","27/350","115/350","CATT 32.8%",
  
  "Saudi Arabia","Riyadh","Dog","2018",117,
  "5/117","5/117",NA,"Genus PCR",
  
  "Palestine","West Bank","Camel","2016",87,
  NA,"26/87",NA,"Microscopy 2.7%",
  
  "Iran","Kerman","Camel","2011",95,
  "2/95","2/95",NA,"Older study",
  
  "Iran","Khorasan","Camel","2019",152,
  NA,"10/152",NA,"PCR 6.5%"
)

############################################################
# 3. EXTENDED MIDDLE EAST / MENA TABLE
############################################################

middle_east_extended <- tribble(
  ~Country, ~Region, ~Host, ~Year, ~SampleSize, ~Microscopy_raw, ~PCR_raw, ~Serology_raw, ~Notes,
  
  # Egypt
  "Egypt","Cairo, Giza","Camel","2021–2025",181,"15/181","43/181",NA,"Amer 2024",
  "Egypt","Qalyubia/Kafr El-Sheikh/Marsa Matrouh","Camel","2022",370,
  "64/370","84/370","70/370","Mahmoud 2022",
  
  # Iran
  "Iran","Sistan–Baluchestan","Camel","2015–2020",370,"44/370","116/370",NA,"Mirshekar 2019",
  "Iran","Yazd/Khuzestan/Sistan-Baluch/Hormozgan","Camel","2021–2025",167,
  "10/167","14/167",NA,"Bahrami 2021",
  
  # Iraq
  "Iraq","Wasit Province","Camel","2015–2020",96,"4/96","26/96",NA,"Asal 2020",
  
  # Oman
  "Oman","Al Batinah","Camel","2023",425,NA,NA,"83/425","Al-Harrasi 2023",
  "Oman","Al Buraimi/Ad Dakhiliyah/Sharqiyah","Camel","2017",388,
  "2/388","78/388","95/388","Al-Khalidi 2022",
  
  # Palestine (extended)
  "Palestine","Multiple districts","Camel","2015–2017",61,"7/61","26/61",NA,"Ereqat 2020",
  "Palestine","Multiple districts","Horse","2015–2017",46,"1/46","8/46",NA,"Ereqat 2020",
  "Palestine","Multiple districts","Donkey","2015–2017",28,NA,"3/28",NA,"Ereqat 2020",
  "Palestine","Multiple districts","Mule","2015–2017",2,NA,"1/2",NA,"Ereqat 2020",
  "Palestine","Multiple districts","Sheep","2015–2017",42,NA,"2/42",NA,"Ereqat 2020",
  "Palestine","Multiple districts","Goat","2015–2017",42,NA,"6/42",NA,"Ereqat 2020",
  
  # Pakistan (extra camel series)
  "Pakistan","Punjab (10 districts)","Camel","2025",400,"33/400","59/400",NA,"Hafeez 2025",
  
  # Saudi Arabia (extra camel series)
  "Saudi Arabia","Riyadh","Camel","2021–2025",200,"0/200","79/200",NA,"Metwally 2021",
  "Saudi Arabia","Al-Qassim","Camel","2021–2025",200,"0/200","92/200",NA,"Metwally 2021",
  "Saudi Arabia","Taif","Camel","2021–2022",102,"2/102","16/102",NA,"Al-Malki 2022",
  
  # Sudan (Blue Nile/Kordofan)
  "Sudan","Blue Nile & West Kordofan","Cattle","2019",70,"3/70","5/70","32/70","Mossaad 2020",
  "Sudan","Blue Nile & West Kordofan","Sheep","2019",62,"1/62","14/62","28/62","Mossaad 2020",
  "Sudan","Blue Nile & West Kordofan","Goat","2019",116,"5/116","12/116","16/116","Mossaad 2020",
  "Sudan","Great Butana","Camel","2014–2015",828,"25/828",NA,NA,"Bala 2018",
  
  # UAE
  "UAE","Abu Dhabi","Camel","2021–2025",77,"14/77","46/77","17/77","Habeeba 2022"
)

############################################################
# 4. Harmonise Year to character & combine all
############################################################
pakistan_old$Year <- as.character(pakistan_old$Year)
sudan_old$Year <- as.character(sudan_old$Year)
middle_east_old$Year <- as.character(middle_east_old$Year)
middle_east_extended$Year <- as.character(middle_east_extended$Year)

df_all_raw <- bind_rows(
  pakistan_old,
  sudan_old,
  middle_east_old,
  middle_east_extended
) %>%
  distinct()

############################################################
# 5. Convert raw fractions to numeric prevalence
############################################################
df_all <- df_all_raw %>%
  mutate(
    Microscopy = sapply(Microscopy_raw, calc_pct),
    PCR        = sapply(PCR_raw,        calc_pct),
    Serology   = sapply(Serology_raw,   calc_pct)
  ) %>%
  select(Country, Region, Host, Year, SampleSize,
         Microscopy, PCR, Serology, Notes)

############################################################
# 6. COUNTRY-LEVEL AGGREGATION
############################################################
country_prev <- df_all %>%
  group_by(Country) %>%
  summarise(
    Microscopy = mean(Microscopy, na.rm = TRUE),
    PCR        = mean(PCR,        na.rm = TRUE),
    Serology   = mean(Serology,   na.rm = TRUE),
    .groups = "drop"
  )

############################################################
# 7. WORLD MAP + PALESTINE FIX
############################################################
world <- ne_countries(scale = "medium", returnclass = "sf")

# Find how Palestine appears in Natural Earth
pal_name <- unique(world$name[grep("Palest|West Bank|Gaza", world$name,
                                   ignore.case = TRUE)])

# Harmonise country names, including UAE
country_prev <- country_prev %>%
  mutate(
    name = countrycode(
      Country,
      origin = "country.name",
      destination = "country.name",
      custom_match = c("UAE" = "United Arab Emirates")
    ),
    name = ifelse(Country == "Palestine", pal_name[1], name)
  )

map_country <- world %>%
  left_join(country_prev, by = "name")

############################################################
# 8. PREVALENCE CATEGORIES & COLORS (country-level)
############################################################
breaks <- c(0, 0.10, 0.20, 0.40, 0.60, 1)
labels <- c("<10%", "10–20%", "20–40%", "40–60%", ">60%")

map_country <- map_country %>%
  mutate(
    Mic_cat      = cut(Microscopy, breaks, labels, include.lowest = TRUE),
    PCR_cat      = cut(PCR,        breaks, labels, include.lowest = TRUE),
    Sero_cat     = cut(Serology,   breaks, labels, include.lowest = TRUE),
    Gap_PCR_Mic  = PCR - Microscopy,
    Gap_Sero_Mic = Serology - Microscopy   # NEW: serology–microscopy gap
  )

pal_colors <- c(
  "<10%"   = "#4CAF50",
  "10–20%" = "#FFEB3B",
  "20–40%" = "#FF9800",
  "40–60%" = "#E53935",
  ">60%"   = "#8B0000"
)

############################################################
# 9. COUNTRY-LEVEL MAP FUNCTION
############################################################
make_country_map <- function(var, title_text) {
  ggplot(map_country) +
    geom_sf(aes(fill = .data[[var]]), color = "grey40", size = 0.25) +
    scale_fill_manual(values = pal_colors, na.value = "lightgrey", name = "Prevalence") +
    coord_sf(xlim = c(20, 80), ylim = c(-5, 40), expand = FALSE) +
    theme_minimal(base_size = 14) +
    labs(
      title = title_text,
      caption = "Grey = no data"
    ) +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

# Individual maps
p_country_pcr   <- make_country_map("PCR_cat",  "PCR prevalence (country level)")
p_country_micro <- make_country_map("Mic_cat",  "Microscopy prevalence (country level)")
p_country_sero  <- make_country_map("Sero_cat", "Serology prevalence (country level)")

# 3-panel country-level
p_country_panel <- p_country_micro + p_country_pcr + p_country_sero +
  plot_layout(ncol = 3, guides = "collect")

# GAP map (PCR - microscopy, continuous)
p_country_gap <- ggplot(map_country) +
  geom_sf(aes(fill = Gap_PCR_Mic), color = "grey40", size = 0.25) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, name = "PCR - Microscopy"
  ) +
  coord_sf(xlim = c(20, 80), ylim = c(-5, 40), expand = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Diagnostic gap: PCR minus microscopy (country level)",
    caption = "Positive values = more infections detected by PCR"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

# GAP map (Serology - microscopy, continuous)
p_country_gap_sero <- ggplot(map_country) +
  geom_sf(aes(fill = Gap_Sero_Mic), color = "grey40", size = 0.25) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, name = "Serology - Microscopy"
  ) +
  coord_sf(xlim = c(20, 80), ylim = c(-5, 40), expand = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Diagnostic gap: Serology minus microscopy (country level)",
    caption = "Positive values = more animals detected by serology than by microscopy"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

# show it
p_country_gap_sero

############################################################
# 10. SUBNATIONAL ADMIN-1 POLYGONS
############################################################
states_all <- ne_states(returnclass = "sf") %>%
  filter(admin %in% c(
    "Pakistan", "Sudan", "Saudi Arabia",
    "Egypt", "Iran", "Oman", "United Arab Emirates"
  )) %>%
  mutate(
    Country = dplyr::case_when(
      admin == "United Arab Emirates" ~ "UAE",
      TRUE ~ admin
    )
  )

# Region → admin-name mapping (make sure names match unique(states_all$name))
region_admin_map <- tribble(
  ~Country,       ~Region,                                  ~admin_name,
  
  # Pakistan
  "Pakistan",     "Cholistan Desert (Punjab)",             "Punjab",
  "Pakistan",     "Southern Punjab",                       "Punjab",
  "Pakistan",     "Punjab (10 districts)",                 "Punjab",
  "Pakistan",     "Attock District (Punjab)",              "Punjab",
  "Pakistan",     "Balochistan",                           "Balochistan",
  
  # Sudan
  "Sudan",        "Western Sudan",                         "North Darfur",  # approximation
  "Sudan",        "Blue Nile & West Kordofan",             "Blue Nile",
  "Sudan",        "Blue Nile & West Kordofan",             "West Kordofan",
  "Sudan",        "Great Butana",                          "Al Jazirah",
  "Sudan",        "Great Butana",                          "Gedaref",
  "Sudan",        "Great Butana",                          "Kassala",
  
  # Saudi Arabia
  "Saudi Arabia", "Al-Jouf",                               "Al Jawf",
  "Saudi Arabia", "Taif (Makkah)",                         "Makkah",
  "Saudi Arabia", "Taif",                                  "Makkah",
  "Saudi Arabia", "Dammam (Eastern Region)",               "Ash Sharqiyah",
  "Saudi Arabia", "Riyadh",                                "Ar Riyad",
  "Saudi Arabia", "Al-Qassim",                             "Al Qasim",
  
  # Egypt
  "Egypt",        "Cairo, Giza",                           "Cairo",
  "Egypt",        "Cairo, Giza",                           "Giza",
  "Egypt",        "Qalyubia/Kafr El-Sheikh/Marsa Matrouh", "Al Qalyubiyah",
  "Egypt",        "Qalyubia/Kafr El-Sheikh/Marsa Matrouh", "Kafr ash Shaykh",
  "Egypt",        "Qalyubia/Kafr El-Sheikh/Marsa Matrouh", "Matruh",
  
  # Iran
  "Iran",         "Sistan–Baluchestan",                    "Sistan and Baluchestan",
  "Iran",         "Yazd/Khuzestan/Sistan-Baluch/Hormozgan","Yazd",
  "Iran",         "Yazd/Khuzestan/Sistan-Baluch/Hormozgan","Khuzestan",
  "Iran",         "Yazd/Khuzestan/Sistan-Baluch/Hormozgan","Sistan and Baluchestan",
  "Iran",         "Yazd/Khuzestan/Sistan-Baluch/Hormozgan","Hormozgan",
  
  # Oman
  "Oman",         "Al Batinah",                            "Al Batinah North",
  "Oman",         "Al Batinah",                            "Al Batinah South",
  "Oman",         "Al Buraimi/Ad Dakhiliyah/Sharqiyah",    "Al Buraimi",
  "Oman",         "Al Buraimi/Ad Dakhiliyah/Sharqiyah",    "Ad Dakhiliyah",
  "Oman",         "Al Buraimi/Ad Dakhiliyah/Sharqiyah",    "Ash Sharqiyah North",
  "Oman",         "Al Buraimi/Ad Dakhiliyah/Sharqiyah",    "Ash Sharqiyah South",
  "Oman",         "Al Buraimi/Ad Dakhiliyah/Sharqiyah",    "Ad Dhahirah",
  
  # UAE
  "UAE",          "Abu Dhabi",                             "Abu Dhabi"
)

############################################################
# 11. REGION-LEVEL PREVALENCE + JOIN TO ADMIN-1
############################################################
region_prev <- df_all %>%
  group_by(Country, Region) %>%
  summarise(
    Microscopy = mean(Microscopy, na.rm = TRUE),
    PCR        = mean(PCR,        na.rm = TRUE),
    Serology   = mean(Serology,   na.rm = TRUE),
    .groups    = "drop"
  )

region_prev_map <- region_prev %>%
  left_join(region_admin_map, by = c("Country", "Region"))

states_joined <- states_all %>%
  left_join(region_prev_map, by = c("Country", "name" = "admin_name")) %>%
  mutate(
    Mic_cat  = cut(Microscopy, breaks, labels, include.lowest = TRUE),
    PCR_cat  = cut(PCR,        breaks, labels, include.lowest = TRUE),
    Sero_cat = cut(Serology,   breaks, labels, include.lowest = TRUE)
  )

############################################################
# 12. SUBNATIONAL MAP FUNCTION
############################################################
############################################################
# FIXED: Subnational map with countries in background
############################################################

draw_state_map <- function(fillvar, title_text) {
  
  # 1. Full world map background (light grey)
  p <- ggplot() +
    geom_sf(data = world,
            fill = "grey92", color = "grey70", size = 0.15)
  
  # 2. Draw countries in your dataset (Egypt, Sudan, Pakistan, etc.)
  p <- p +
    geom_sf(
      data = world %>% filter(admin %in% c(
        "Pakistan", "Sudan", "Saudi Arabia",
        "Egypt", "Iran", "Oman", "United Arab Emirates"
      )),
      fill = "grey85", color = "grey50", size = 0.25
    )
  
  # 3. Draw admin-1 states (white base)
  p <- p +
    geom_sf(
      data = states_all,
      fill = "white", color = "grey60", size = 0.20
    )
  
  # 4. Draw admin-1 states WITH DATA colored by prevalence
  p <- p +
    geom_sf(
      data = states_joined,
      aes(fill = .data[[fillvar]]),
      color = "grey30", size = 0.25
    )
  
  # 5. Styling
  p +
    scale_fill_manual(
      values = pal_colors,
      na.value = "lightgrey",
      name = "Prevalence"
    ) +
    coord_sf(xlim = c(20, 80), ylim = c(-5, 40), expand = FALSE) +
    theme_minimal(base_size = 14) +
    labs(
      title = title_text,
      subtitle = "Admin-1 subnational prevalence | Grey = no data",
      caption = "Countries with no subnational data are shown in grey."
    ) +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

p_state_pcr   <- draw_state_map("PCR_cat",  "PCR prevalence (subnational)")
p_state_micro <- draw_state_map("Mic_cat",  "Microscopy prevalence (subnational)")
p_state_sero  <- draw_state_map("Sero_cat", "Serology prevalence (subnational)")

p_state_panel <- p_state_micro + p_state_pcr + p_state_sero +
  plot_layout(ncol = 3, guides = "collect")

############################################################
# 13. HOST-SPECIFIC COUNTRY-LEVEL MAPS (composite)
############################################################
host_prev_country <- df_all %>%
  group_by(Country, Host) %>%
  summarise(
    Combined = mean(c(Microscopy, PCR, Serology), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    name = countrycode(
      Country,
      origin = "country.name",
      destination = "country.name",
      custom_match = c("UAE" = "United Arab Emirates")
    ),
    name = ifelse(Country == "Palestine", pal_name[1], name)
  )

host_map <- world %>%
  left_join(host_prev_country, by = "name") %>%
  mutate(
    Host_cat = cut(
      Combined,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE
    )
  )

make_host_map <- function(host_species) {
  ggplot(host_map %>% filter(Host == host_species)) +
    geom_sf(aes(fill = Host_cat), color = "grey40", size = 0.25) +
    scale_fill_manual(values = pal_colors, na.value = "lightgrey", name = "Prevalence") +
    coord_sf(xlim = c(20, 80), ylim = c(-5, 40), expand = FALSE) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Surra prevalence in", host_species, "(country level)"),
      caption = "Composite of microscopy, PCR and serology"
    ) +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

# Example: camel & cattle maps
p_host_camel  <- make_host_map("Camel")
p_host_cattle <- make_host_map("Cattle")
p_host_goat   <- make_host_map("Goat")
p_host_sheep  <- make_host_map("Sheep")
p_host_horse  <- make_host_map("Horse")
p_host_donkey <- make_host_map("Donkey")

############################################################
# 14. DISPLAY KEY FIGURES
############################################################

# Country level
p_country_panel      # Microscopy | PCR | Serology (country)
p_country_gap        # PCR - microscopy

# Subnational admin-1
p_state_panel        # Microscopy | PCR | Serology (admin-1)

# Host-specific (example)
p_host_camel
p_host_cattle
p_host_goat
p_host_sheep
p_host_horse
p_host_donkey
