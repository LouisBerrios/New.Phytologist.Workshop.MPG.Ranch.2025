### Fig. 1, New Phytologist In-situ Viewpoint Paper ###

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

MF <- read.csv("Final.NP.MFF.10.22.26_LB+YL.csv")
MF2 <- subset(MF, Inclusion !="No") ## this accounts for the 'Yes?' (MAYBE)

# Topic names, in the same order as your schematic
category_labels <- c(
  "Resource.Transfer"         = "Resource Transfer\nBetween Symbionts",
  "Life.History"              = "Fungal Life History\nStrategies",
  "Hyposphere.Interactions"   = "Hyphosphere\nInteractions",
  "Soil.Carbon.Dynamics"      = "Soil Carbon\nDynamics",
  "Non.Resource.Interactions" = "Non-Resource\nInteractions"
)

category_levels <- unname(category_labels)

location_levels <- c(
  "In-situ", "Ex-situ", "In-situ + Ex-situ", "In-silico"
)

function_levels <- c("Functional potential", "Realized function")

# Standardize punctuation, spacing, and capitalization
clean_label <- function(x) {
  x <- gsub("[._-]", " ", as.character(x))
  tolower(trimws(gsub("\\s+", " ", x)))
}

study_data <- MF2 %>%
  mutate(
    Year = suppressWarnings(as.integer(as.character(Publication.Year))),
    location_key = clean_label(Location),
    function_key = clean_label(Function.Type),
    
    Location = case_when(
      location_key == "in situ" ~ "In-situ",
      location_key == "ex situ" ~ "Ex-situ",
      location_key %in% c(
        "both", "in situ + ex situ", "in situ+ex situ",
        "in situ and ex situ", "in situ ex situ"
      ) ~ "In-situ + Ex-situ",
      location_key == "in silico" ~ "In-silico",
      TRUE ~ NA_character_
    ),
    
    Function = case_when(
      function_key == "functional potential" ~ "Functional potential",
      function_key == "realized function" ~ "Realized function",
      TRUE ~ NA_character_
    )
  )

# Check for values that were not recognized
study_data %>%
  filter(is.na(Location) | is.na(Function)) %>%
  distinct(location_key, function_key)

# One row per study × topic
topic_data <- study_data %>%
  pivot_longer(
    cols = all_of(names(category_labels)),
    names_to = "Category",
    values_to = "Present"
  ) %>%
  filter(as.character(Present) == "1") %>%
  mutate(
    Category = factor(
      Category,
      levels = names(category_labels),
      labels = category_levels
    )
  )
### 2nd Step
function_counts <- topic_data %>%
  filter(!is.na(Function)) %>%
  count(Category, Function, name = "Studies") %>%
  complete(
    Category,
    Function = function_levels,
    fill = list(Studies = 0)
  ) %>%
  mutate(Function = factor(Function, levels = function_levels))

# Table of values to insert into your schematic
count_table <- function_counts %>%
  pivot_wider(
    names_from = Function,
    values_from = Studies
  )

count_table

### 3rd Step
year_range <- seq(
  min(study_data$Year, na.rm = TRUE),
  max(study_data$Year, na.rm = TRUE)
)

annual_counts <- topic_data %>%
  filter(!is.na(Year), !is.na(Location)) %>%
  count(Category, Year, Location, name = "Studies") %>%
  complete(
    Category,
    Year = year_range,
    Location = location_levels,
    fill = list(Studies = 0)
  ) %>%
  mutate(Location = factor(Location, levels = location_levels))

### 4th Step

p_counts <- ggplot(
  function_counts,
  aes(x = Function, y = 1, label = paste0("n = ", Studies))
) +
  geom_text(size = 4.5, family = "serif") +
  facet_grid(Category ~ ., switch = "y") +
  scale_x_discrete(
    position = "top",
    labels = c(
      "Functional potential" = "Functional\nPotential",
      "Realized function" = "Realized\nFunction"
    )
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold", color = "black"),
    strip.placement = "outside",
    strip.text.y.left = element_text(
      angle = 0, hjust = 0, face = "bold"
    ),
    panel.spacing.y = grid::unit(0.8, "lines")
  )

# Divide the observed year range into three approximately equal bins
year_min <- min(study_data$Year, na.rm = TRUE)
year_max <- max(study_data$Year, na.rm = TRUE)

bin_edges <- floor(seq(year_min, year_max + 1, length.out = 4))

bin_labels <- paste0(
  bin_edges[1:3], "–", bin_edges[2:4] - 1
)

binned_counts <- topic_data %>%
  filter(!is.na(Year), !is.na(Location)) %>%
  mutate(
    YearBin = cut(
      Year,
      breaks = bin_edges,
      labels = bin_labels,
      right = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  count(Category, YearBin, Location, name = "Studies") %>%
  complete(
    Category,
    YearBin = factor(bin_labels, levels = bin_labels),
    Location = location_levels,
    fill = list(Studies = 0)
  ) %>%
  mutate(Location = factor(Location, levels = location_levels))

# Distinct colors, reinforced by line types and shapes
location_colors <- c(
  "In-situ"           = "darkgreen",  
  "Ex-situ"           = "gray",  
  "In-situ + Ex-situ" = "#C49A45",  
  "In-silico"         = "#8A6F91"
)

p_time <- ggplot(
  binned_counts,
  aes(
    x = YearBin,
    y = Studies,
    group = Location,
    color = Location,
    linetype = Location,
    shape = Location
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.7) +
  facet_grid(Category ~ .) +
  scale_color_manual(values = location_colors, drop = FALSE) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotdash", "dotted"),
    drop = FALSE
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 18),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 3),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Publication Year",
    y = "Number of Studies",
    color = NULL,
    linetype = NULL,
    shape = NULL
  ) +
  guides(
    color = guide_legend(ncol = 2, byrow = TRUE),
    linetype = guide_legend(ncol = 2, byrow = TRUE),
    shape = guide_legend(ncol = 2, byrow = TRUE)
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom",
    panel.spacing.y = grid::unit(0.8, "lines")
  )

# Rebuild the combined figure
combined <- (p_counts | p_time) +
  plot_layout(widths = c(1.4, 2)) +
  plot_annotation(
    title = "Mapping Mycorrhizal Function Research"
  )

combined

ggsave(
  "Fig1_combined.png",
  plot = combined,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)
