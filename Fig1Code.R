### Figure 1 | New Phytologist Montana Workshop ###
# By Louis Berrios | 11.02.2026
### Figure 1 | New Phytologist Montana Workshop ###
# By Louis Berrios | 11.02.2026

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

# --------------------------------------------------
# 1. Read data and define labels
# --------------------------------------------------

MF <- read.csv(
  "NP.Data.TEMP.Broad.Manipulation.Type.Revised+LB+YL.csv"
)

MF2 <- MF %>%
  filter(Inclusion != "No")

category_labels <- c(
  "Resource.Transfer" =
    "Resource Transfer\nBetween Symbionts",
  "Life.History" =
    "Fungal Life History\nStrategies",
  "Hyphosphere.Interactions" =
    "Hyphosphere\nInteractions",
  "Soil.Carbon.Dynamics" =
    "Soil Carbon\nDynamics",
  "Non.Resource.Interactions" =
    "Non-Resource\nInteractions"
)

category_levels <- unname(category_labels)

fungal_levels <- c(
  "AM Fungi",
  "EcM Fungi"
)

function_levels <- c(
  "Realized Function",
  "Functional Potential"
)

location_levels <- c(
  "In-situ",
  "Ex-situ"
)

location_colors <- c(
  "In-situ" = "#000000",
  "Ex-situ" = "#BDBDBD"
)

location_shapes <- c(
  "In-situ" = 15,
  "Ex-situ" = 16
)

bin_labels <- c(
  "1987–1999",
  "2000–2012",
  "2013–2026"
)

manipulation_levels <- c(
  "Plant manipulations",
  "Fungal manipulations/isolations",
  "Environmental manipulations",
  "Surveys"
)

clean_label <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[._-]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# --------------------------------------------------
# 2. Standardize study data
# --------------------------------------------------

study_data <- MF2

if (
  "Hyposphere.Interactions" %in% names(study_data) &&
  !"Hyphosphere.Interactions" %in% names(study_data)
) {
  study_data <- study_data %>%
    dplyr::rename(
      Hyphosphere.Interactions = Hyposphere.Interactions
    )
}

study_data <- study_data %>%
  mutate(
    Study.ID = row_number(),
    
    Article.Title = str_squish(
      as.character(Article.Title)
    ),
    
    across(
      all_of(names(category_labels)),
      ~ as.integer(trimws(as.character(.x)))
    ),
    
    Year = as.integer(
      as.character(Publication.Year)
    ),
    
    Year.Bin = cut(
      Year,
      breaks = c(1987, 2000, 2013, 2027),
      labels = bin_labels,
      right = FALSE
    ),
    
    Function.Key = clean_label(Function.Type),
    
    Function = case_when(
      Function.Key == "realized function" ~
        "Realized Function",
      
      Function.Key == "functional potential" ~
        "Functional Potential",
      
      TRUE ~ NA_character_
    ),
    
    Location.Raw = as.character(Location),
    Location.Key = clean_label(Location.Raw),
    
    # Exclude Both and In-silico from this figure.
    Location = case_when(
      Location.Key == "in situ" ~ "In-situ",
      Location.Key == "ex situ" ~ "Ex-situ",
      TRUE ~ NA_character_
    ),
    
    Myco.Key = tolower(
      coalesce(as.character(Mycorrhizal.Type), "")
    ),
    
    `AM Fungi` = grepl(
      "(^|[^a-z])am([^a-z]|$)",
      Myco.Key
    ),
    
    `EcM Fungi` = grepl(
      "(^|[^a-z])ecm([^a-z]|$)",
      Myco.Key
    )
  )

# --------------------------------------------------
# 3. Prepare study × category × fungal-group data
# --------------------------------------------------

line_data <- study_data %>%
  filter(
    !is.na(Year.Bin),
    Function %in% function_levels,
    Location %in% location_levels
  ) %>%
  pivot_longer(
    cols = all_of(names(category_labels)),
    names_to = "Category",
    values_to = "Present"
  ) %>%
  filter(Present == 1L) %>%
  pivot_longer(
    cols = all_of(fungal_levels),
    names_to = "Fungal.Group",
    values_to = "Included"
  ) %>%
  filter(Included) %>%
  mutate(
    Category = factor(
      Category,
      levels = names(category_labels),
      labels = category_levels
    ),
    
    Fungal.Group = factor(
      Fungal.Group,
      levels = fungal_levels
    ),
    
    Function = factor(
      Function,
      levels = function_levels
    ),
    
    Location = factor(
      Location,
      levels = location_levels
    )
  )

# --------------------------------------------------
# 4. Count studies for trend panels
# --------------------------------------------------

# Retains the original row-based Study.ID counting.
line_counts <- line_data %>%
  distinct(
    Study.ID,
    Category,
    Fungal.Group,
    Function,
    Location,
    Year.Bin
  ) %>%
  dplyr::count(
    Category,
    Fungal.Group,
    Function,
    Location,
    Year.Bin,
    name = "Studies"
  ) %>%
  complete(
    Category,
    Fungal.Group,
    Function,
    Location,
    Year.Bin,
    fill = list(Studies = 0L)
  ) %>%
  mutate(
    Line.ID = interaction(
      Category,
      Fungal.Group,
      Function,
      Location,
      drop = TRUE
    )
  ) %>%
  arrange(
    Category,
    Fungal.Group,
    Location,
    Function,
    Year.Bin
  )

# --------------------------------------------------
# 5. Trend plot
# --------------------------------------------------

p_fungal_trends <- ggplot(
  line_counts,
  aes(
    x = Year.Bin,
    y = Studies,
    color = Location,
    linetype = Function,
    group = Line.ID
  )
) +
  geom_line(
    linewidth = 0.8,
    show.legend = c(
      colour = TRUE,
      linetype = TRUE
    )
  ) +
  geom_point(
    aes(shape = Location),
    size = 2,
    show.legend = c(
      colour = TRUE,
      shape = TRUE,
      linetype = FALSE
    )
  ) +
  facet_grid(
    Category ~ Fungal.Group,
    switch = "y",
    drop = FALSE
  ) +
  scale_color_manual(
    name = "Location",
    values = location_colors,
    breaks = location_levels,
    na.translate = FALSE,
    drop = FALSE
  ) +
  scale_shape_manual(
    name = "Location",
    values = location_shapes,
    breaks = location_levels,
    na.translate = FALSE,
    drop = FALSE
  ) +
  scale_linetype_manual(
    name = "Function Type",
    values = c(
      "Realized Function" = "solid",
      "Functional Potential" = "dashed"
    ),
    breaks = function_levels,
    na.translate = FALSE,
    drop = FALSE
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0, NA),
    
    breaks = function(limits) {
      ticks <- pretty(limits, n = 3)
      ticks[ticks >= 0 & ticks == floor(ticks)]
    },
    
    minor_breaks = NULL,
    expand = expansion(mult = c(0.03, 0.10))
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  guides(
    linetype = guide_legend(
      order = 1,
      nrow = 1,
      override.aes = list(
        colour = "black",
        shape = NA
      )
    ),
    
    color = guide_legend(
      order = 2,
      nrow = 1,
      override.aes = list(
        linetype = "solid",
        size = 2.5
      )
    ),
    
    shape = guide_legend(
      order = 2,
      nrow = 1
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    
    strip.background = element_rect(
      fill = "gray95",
      color = "gray20"
    ),
    
    strip.text.x = element_text(
      face = "bold"
    ),
    
    strip.text.y.left = element_text(
      angle = 0,
      face = "bold",
      hjust = 1
    ),
    
    strip.placement.x = "inside",
    strip.placement.y = "outside",
    
    panel.spacing = grid::unit(
      0.7,
      "lines"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    
    legend.position = "bottom",
    legend.box = "vertical",
    
    legend.key.width = grid::unit(
      1.5,
      "cm"
    )
  )

# --------------------------------------------------
# 6. Prepare manipulation data by location
# --------------------------------------------------

# Pool AM and EcM while retaining study location.
# Duplicate fungal-group and function rows are removed.
manip_source <- line_data %>%
  transmute(
    Article.Title,
    Category,
    Location = as.character(Location),
    
    Broad.Manipulation.Type =
      as.character(Broad.Manipulation.Type)
  ) %>%
  filter(
    !is.na(Article.Title),
    Article.Title != "",
    Location %in% location_levels
  ) %>%
  distinct()

# Denominator: all unique articles within each category,
# pooled across the plotted locations and fungal groups.
manip_denominators <- manip_source %>%
  distinct(
    Article.Title,
    Category
  ) %>%
  dplyr::count(
    Category,
    name = "Total.Studies"
  )

# Separate studies assigned to multiple manipulation types.
manip_studies <- manip_source %>%
  separate_longer_delim(
    cols = Broad.Manipulation.Type,
    delim = ";"
  ) %>%
  mutate(
    Broad.Manipulation.Type =
      str_squish(Broad.Manipulation.Type)
  ) %>%
  distinct(
    Article.Title,
    Category,
    Location,
    Broad.Manipulation.Type
  )

# Check manipulation labels.
invalid_labels <- manip_studies %>%
  filter(
    is.na(Broad.Manipulation.Type) |
      !Broad.Manipulation.Type %in% manipulation_levels
  )

if (nrow(invalid_labels) > 0L) {
  print(invalid_labels)
  
  stop(
    "Some manipulation labels are missing or unrecognized. ",
    "Inspect invalid_labels before continuing."
  )
}

# Check for articles represented in both location segments.
location_overlap <- manip_studies %>%
  dplyr::count(
    Article.Title,
    Category,
    Broad.Manipulation.Type,
    name = "Number.of.locations"
  ) %>%
  filter(Number.of.locations > 1L)

if (nrow(location_overlap) > 0L) {
  warning(
    "Some articles contribute to both location segments. ",
    "Stacked totals include their contributions in each location. ",
    "Inspect location_overlap."
  )
}

# --------------------------------------------------
# 7. Calculate segment proportions and bar totals
# --------------------------------------------------

manip_counts <- manip_studies %>%
  dplyr::count(
    Category,
    Broad.Manipulation.Type,
    Location,
    name = "Studies"
  ) %>%
  complete(
    Category,
    Broad.Manipulation.Type = manipulation_levels,
    Location = location_levels,
    fill = list(Studies = 0L)
  ) %>%
  left_join(
    manip_denominators,
    by = "Category"
  ) %>%
  mutate(
    Total.Studies = coalesce(
      Total.Studies,
      0L
    ),
    
    Proportion =
      Studies / na_if(Total.Studies, 0L),
    
    Category = factor(
      Category,
      levels = category_levels
    ),
    
    Broad.Manipulation.Type = factor(
      Broad.Manipulation.Type,
      levels = rev(manipulation_levels)
    ),
    
    Location = factor(
      Location,
      levels = location_levels
    ),
    
    Summary = factor(
      "Manipulation Types",
      levels = "Manipulation Types"
    )
  )

# One total label per stacked bar.
manip_totals <- manip_counts %>%
  group_by(
    Category,
    Broad.Manipulation.Type,
    Summary
  ) %>%
  summarise(
    Total.Proportion = if (all(is.na(Proportion))) {
      NA_real_
    } else {
      sum(Proportion, na.rm = TRUE)
    },
    
    .groups = "drop"
  )

# --------------------------------------------------
# 8. Stacked manipulation plot
# --------------------------------------------------

p_manipulation_category <- ggplot(
  manip_counts,
  aes(
    x = Proportion,
    y = Broad.Manipulation.Type,
    fill = Location,
    group = Location
  )
) +
  geom_col(
    position = position_stack(
      reverse = TRUE
    ),
    orientation = "y",
    width = 0.65,
    na.rm = TRUE
  ) +
  geom_text(
    data = manip_totals,
    
    aes(
      x = Total.Proportion,
      y = Broad.Manipulation.Type,
      
      label = scales::percent(
        Total.Proportion,
        accuracy = 1
      )
    ),
    
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 2.7,
    color = "black",
    na.rm = TRUE
  ) +
  facet_grid(
    Category ~ Summary,
    switch = "y",
    drop = FALSE
  ) +
  scale_fill_manual(
    values = location_colors,
    breaks = location_levels,
    drop = FALSE,
    guide = "none"
  ) +
  scale_y_discrete(
    labels = c(
      "Plant manipulations" = "Plant",
      "Fungal manipulations/isolations" = "Fungal",
      "Environmental manipulations" = "Environment",
      "Surveys" = "Survey"
    ),
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = NULL,
    expand = expansion(
      mult = c(0, 0.25)
    )
  ) +
  expand_limits(
    x = c(0, 1)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    strip.background = element_rect(
      fill = "gray95",
      color = "gray20"
    ),
    
    strip.text.x = element_text(
      face = "bold"
    ),
    
    strip.text.y = element_blank(),
    strip.background.y = element_blank(),
    strip.placement.x = "inside",
    
    panel.spacing = grid::unit(
      0.7,
      "lines"
    ),
    
    axis.text.y = element_text(
      size = 8
    ),
    
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    
    legend.position = "none"
  )

top_strip_theme <- theme(
  strip.background.x = element_rect(
    fill = "black",
    color = "black"
  ),
  strip.text.x = element_text(
    color = "white",
    face = "bold"
  )
)

p_fungal_trends <- p_fungal_trends + top_strip_theme

p_manipulation_category <- p_manipulation_category +
  top_strip_theme

# --------------------------------------------------
# 9. Combine plots
# --------------------------------------------------

p_combined <- (
  p_fungal_trends | p_manipulation_category
) +
  plot_layout(
    widths = c(2, 0.7),
    guides = "collect"
  ) +
  plot_annotation(
    caption = paste(
      "Only AM and EcM studies classified as In-situ or Ex-situ are shown.",
      "\nManipulation segments show percentages of all unique studies",
      "within each research category, pooled across AM and EcM.",
      "\nBlack indicates In-situ; gray indicates Ex-situ.",
      "Labels show summed segment percentages.",
      "\nStudies may contribute to multiple manipulation types",
      "and, where applicable, both location segments."
    ),
    
    theme = theme(
      plot.caption = element_text(
        hjust = 0,
        size = 8
      )
    )
  )

p_combined <- p_combined &
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

p_combined

# Underlying summaries
manip_counts
manip_totals

# --------------------------------------------------
# 10. Save
# --------------------------------------------------

ggsave(
  "Fig1_AM_EcM_Stacked_Manipulations.png",
  plot = p_combined,
  width = 7.7,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)

# ---------------------------------------------------
# Summary Table
# ---------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# ---------------------------------------------------
# 1. Read included studies
# ---------------------------------------------------

sub_tab <- read_csv(
  "NP.Data.TEMP.Broad.Manipulation.Type.Revised+LB+YL.csv",
  show_col_types = FALSE
) %>%
  filter(Inclusion != "No")

# Accommodate either hyphosphere spelling.
interaction_col <- intersect(
  c(
    "Hyphosphere.Interactions",
    "Hyposphere.Interactions"
  ),
  names(sub_tab)
)

stopifnot(length(interaction_col) == 1L)

categories <- c(
  "Resource.Transfer",
  "Life.History",
  interaction_col,
  "Soil.Carbon.Dynamics",
  "Non.Resource.Interactions"
)

# Standardize punctuation, capitalization, and spacing.
key <- function(x) {
  str_remove_all(
    str_to_lower(as.character(x)),
    "[^a-z0-9]"
  )
}

# ---------------------------------------------------
# 2. Standardize data
# ---------------------------------------------------

dat <- sub_tab %>%
  mutate(
    study = str_squish(as.character(Article.Title)),
    
    myc_key = str_to_lower(
      str_squish(as.character(Mycorrhizal.Type))
    ),
    
    location_key = key(Location),
    function_key = key(Function.Type),
    
    # Ensure category columns have compatible types.
    across(
      all_of(categories),
      ~ as.integer(str_trim(as.character(.x)))
    )
  ) %>%
  filter(
    !is.na(study),
    study != ""
  )

# Overall unique studies, counting duplicated SPLIT entries once.
total_studies <- dat %>%
  summarise(
    Unique.Studies = n_distinct(study)
  )

# ---------------------------------------------------
# 3. Assign mycorrhizal groups
# ---------------------------------------------------

# Match AM and EcM as separate abbreviations.
# Studies containing both abbreviations enter both groups.
dat <- dat %>%
  mutate(
    EcM = coalesce(
      str_detect(
        myc_key,
        "(^|[^a-z])ecm([^a-z]|$)"
      ),
      FALSE
    ),
    
    AM = coalesce(
      str_detect(
        myc_key,
        "(^|[^a-z])am([^a-z]|$)"
      ),
      FALSE
    ),
    
    Unknown = is.na(myc_key) | myc_key == "",
    
    Others = !Unknown & !EcM & !AM
  )

myc_rows <- bind_rows(
  dat %>%
    mutate(
      Mycorrhizal.Group = "All studies"
    ),
  
  dat %>%
    pivot_longer(
      cols = c(EcM, AM, Others, Unknown),
      names_to = "Mycorrhizal.Group",
      values_to = "present"
    ) %>%
    filter(present) %>%
    dplyr::select(-present)
)

# ---------------------------------------------------
# 4. Expand research categories
# ---------------------------------------------------

category_rows <- bind_rows(
  myc_rows %>%
    mutate(
      Category = "All categories"
    ),
  
  myc_rows %>%
    pivot_longer(
      cols = all_of(categories),
      names_to = "Category",
      values_to = "present"
    ) %>%
    filter(present == 1L) %>%
    dplyr::select(-present)
)

# ---------------------------------------------------
# 5. Expand function groups
# ---------------------------------------------------

# Preserve each function assignment on its original row.
function_base <- category_rows %>%
  mutate(
    Function.Group = case_when(
      function_key == "realizedfunction" ~
        "Realized.Function",
      
      function_key == "functionalpotential" ~
        "Functional.Potential",
      
      TRUE ~ "Other/unknown"
    )
  )

function_rows <- bind_rows(
  function_base,
  
  function_base %>%
    mutate(
      Function.Group = "All function types"
    )
)

# ---------------------------------------------------
# 6. Expand location groups
# ---------------------------------------------------

# In.situ and Ex.situ are inclusive:
# a record coded Both enters both location columns.
#
# Both requires BOTH flags, rather than either flag.

location_rows <- function_rows %>%
  mutate(
    in_situ = coalesce(
      str_detect(
        location_key,
        fixed("insitu")
      ) |
        location_key == "both",
      FALSE
    ),
    
    ex_situ = coalesce(
      str_detect(
        location_key,
        fixed("exsitu")
      ) |
        location_key == "both",
      FALSE
    ),
    
    in_silico = coalesce(
      str_detect(
        location_key,
        fixed("insilico")
      ),
      FALSE
    ),
    
    `All locations` = TRUE,
    
    In.situ = in_situ,
    Ex.situ = ex_situ,
    
    # Corrected: intersection, not union.
    Both = in_situ & ex_situ,
    
    In.silico = in_silico
  ) %>%
  pivot_longer(
    cols = c(
      `All locations`,
      In.situ,
      Ex.situ,
      Both,
      In.silico
    ),
    names_to = "Study.Location",
    values_to = "include_location"
  ) %>%
  filter(include_location)

# ---------------------------------------------------
# 7. Count unique studies
# ---------------------------------------------------

summary_table <- location_rows %>%
  group_by(
    Mycorrhizal.Group,
    Category,
    Function.Group,
    Study.Location
  ) %>%
  summarise(
    Studies = n_distinct(study),
    .groups = "drop"
  ) %>%
  complete(
    Mycorrhizal.Group,
    Category,
    Function.Group,
    
    Study.Location = c(
      "All locations",
      "In.situ",
      "Ex.situ",
      "Both",
      "In.silico"
    ),
    
    fill = list(
      Studies = 0L
    )
  ) %>%
  pivot_wider(
    names_from = Study.Location,
    values_from = Studies,
    values_fill = 0L
  ) %>%
  dplyr::select(
    Mycorrhizal.Group,
    Category,
    Function.Group,
    `All locations`,
    In.situ,
    Ex.situ,
    Both,
    In.silico
  ) %>%
  arrange(
    Mycorrhizal.Group,
    Category,
    Function.Group
  )

# ---------------------------------------------------
# 8. Verify location counts
# ---------------------------------------------------

# Both cannot exceed either inclusive location count.
stopifnot(
  all(summary_table$Both <= summary_table$In.situ),
  all(summary_table$Both <= summary_table$Ex.situ)
)

# ---------------------------------------------------
# 9. Display results
# ---------------------------------------------------

print(total_studies)

print(
  summary_table,
  n = Inf
)

# Overall counts across all categories and function types.
overall_location_counts <- summary_table %>%
  filter(
    Mycorrhizal.Group == "All studies",
    Category == "All categories",
    Function.Group == "All function types"
  )

print(overall_location_counts)

# ---------------------------------------------------
# 10. Export without sequential row numbers
# ---------------------------------------------------

write_csv(
  summary_table,
  "Summary.Table.NP.Data.11.02.2026.Corrected.csv"
)


# ---------------------------------------------------
# Summary Table
# ---------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# ---------------------------------------------------
# 1. Read included studies
# ---------------------------------------------------

sub_tab <- read_csv(
  "NP.Data.TEMP.Broad.Manipulation.Type.Revised+LB+YL.csv",
  show_col_types = FALSE
) %>%
  filter(Inclusion != "No")

# Accommodate either hyphosphere spelling.
interaction_col <- intersect(
  c(
    "Hyphosphere.Interactions",
    "Hyposphere.Interactions"
  ),
  names(sub_tab)
)

stopifnot(length(interaction_col) == 1L)

categories <- c(
  "Resource.Transfer",
  "Life.History",
  interaction_col,
  "Soil.Carbon.Dynamics",
  "Non.Resource.Interactions"
)

# Standardize punctuation, capitalization, and spacing.
key <- function(x) {
  str_remove_all(
    str_to_lower(as.character(x)),
    "[^a-z0-9]"
  )
}

# ---------------------------------------------------
# 2. Standardize data
# ---------------------------------------------------

dat <- sub_tab %>%
  mutate(
    study = str_squish(as.character(Article.Title)),
    
    myc_key = str_to_lower(
      str_squish(as.character(Mycorrhizal.Type))
    ),
    
    location_key = key(Location),
    function_key = key(Function.Type),
    
    # Ensure category columns have compatible types.
    across(
      all_of(categories),
      ~ as.integer(str_trim(as.character(.x)))
    )
  ) %>%
  filter(
    !is.na(study),
    study != ""
  )

# Overall unique studies, counting duplicated SPLIT entries once.
total_studies <- dat %>%
  summarise(
    Unique.Studies = n_distinct(study)
  )

# ---------------------------------------------------
# 3. Assign mycorrhizal groups
# ---------------------------------------------------

# Match AM and EcM as separate abbreviations.
# Studies containing both abbreviations enter both groups.
dat <- dat %>%
  mutate(
    EcM = coalesce(
      str_detect(
        myc_key,
        "(^|[^a-z])ecm([^a-z]|$)"
      ),
      FALSE
    ),
    
    AM = coalesce(
      str_detect(
        myc_key,
        "(^|[^a-z])am([^a-z]|$)"
      ),
      FALSE
    ),
    
    Unknown = is.na(myc_key) | myc_key == "",
    
    Others = !Unknown & !EcM & !AM
  )

myc_rows <- bind_rows(
  dat %>%
    mutate(
      Mycorrhizal.Group = "All studies"
    ),
  
  dat %>%
    pivot_longer(
      cols = c(EcM, AM, Others, Unknown),
      names_to = "Mycorrhizal.Group",
      values_to = "present"
    ) %>%
    filter(present) %>%
    dplyr::select(-present)
)

# ---------------------------------------------------
# 4. Expand research categories
# ---------------------------------------------------

category_rows <- bind_rows(
  myc_rows %>%
    mutate(
      Category = "All categories"
    ),
  
  myc_rows %>%
    pivot_longer(
      cols = all_of(categories),
      names_to = "Category",
      values_to = "present"
    ) %>%
    filter(present == 1L) %>%
    dplyr::select(-present)
)

# ---------------------------------------------------
# 5. Expand function groups
# ---------------------------------------------------

# Preserve each function assignment on its original row.
function_base <- category_rows %>%
  mutate(
    Function.Group = case_when(
      function_key == "realizedfunction" ~
        "Realized.Function",
      
      function_key == "functionalpotential" ~
        "Functional.Potential",
      
      TRUE ~ "Other/unknown"
    )
  )

function_rows <- bind_rows(
  function_base,
  
  function_base %>%
    mutate(
      Function.Group = "All function types"
    )
)

# ---------------------------------------------------
# 6. Expand location groups
# ---------------------------------------------------

# In.situ and Ex.situ are inclusive:
# a record coded Both enters both location columns.
#
# Both requires BOTH flags, rather than either flag.

location_rows <- function_rows %>%
  mutate(
    in_situ = coalesce(
      str_detect(
        location_key,
        fixed("insitu")
      ) |
        location_key == "both",
      FALSE
    ),
    
    ex_situ = coalesce(
      str_detect(
        location_key,
        fixed("exsitu")
      ) |
        location_key == "both",
      FALSE
    ),
    
    in_silico = coalesce(
      str_detect(
        location_key,
        fixed("insilico")
      ),
      FALSE
    ),
    
    `All locations` = TRUE,
    
    In.situ = in_situ,
    Ex.situ = ex_situ,
    
    # Corrected: intersection, not union.
    Both = in_situ & ex_situ,
    
    In.silico = in_silico
  ) %>%
  pivot_longer(
    cols = c(
      `All locations`,
      In.situ,
      Ex.situ,
      Both,
      In.silico
    ),
    names_to = "Study.Location",
    values_to = "include_location"
  ) %>%
  filter(include_location)

# ---------------------------------------------------
# 7. Count unique studies
# ---------------------------------------------------

summary_table <- location_rows %>%
  group_by(
    Mycorrhizal.Group,
    Category,
    Function.Group,
    Study.Location
  ) %>%
  summarise(
    Studies = n_distinct(study),
    .groups = "drop"
  ) %>%
  complete(
    Mycorrhizal.Group,
    Category,
    Function.Group,
    
    Study.Location = c(
      "All locations",
      "In.situ",
      "Ex.situ",
      "Both",
      "In.silico"
    ),
    
    fill = list(
      Studies = 0L
    )
  ) %>%
  pivot_wider(
    names_from = Study.Location,
    values_from = Studies,
    values_fill = 0L
  ) %>%
  dplyr::select(
    Mycorrhizal.Group,
    Category,
    Function.Group,
    `All locations`,
    In.situ,
    Ex.situ,
    Both,
    In.silico
  ) %>%
  arrange(
    Mycorrhizal.Group,
    Category,
    Function.Group
  )

# ---------------------------------------------------
# 8. Verify location counts
# ---------------------------------------------------

# Both cannot exceed either inclusive location count.
stopifnot(
  all(summary_table$Both <= summary_table$In.situ),
  all(summary_table$Both <= summary_table$Ex.situ)
)

# ---------------------------------------------------
# 9. Display results
# ---------------------------------------------------

print(total_studies)

print(
  summary_table,
  n = Inf
)

# Overall counts across all categories and function types.
overall_location_counts <- summary_table %>%
  filter(
    Mycorrhizal.Group == "All studies",
    Category == "All categories",
    Function.Group == "All function types"
  )

print(overall_location_counts)

# ---------------------------------------------------
# 10. Export without sequential row numbers
# ---------------------------------------------------

write_csv(
  summary_table,
  "Summary.Table.NP.Data.11.02.2026.Corrected.csv"
)


