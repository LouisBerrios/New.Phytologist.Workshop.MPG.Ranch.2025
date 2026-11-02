### Adding Broad Mnaipulation Type ###

# --------------------------------------------------------------
# Adding Broad Manipulation Type Grouping
# --------------------------------------------------------------

library(dplyr)
library(stringr)

MF <- read.csv("NP.Data.TEMP.Broad.Manipulation.Type.Revised+LB+YL.csv")
### Change all values in NRI are integer 
MF2 <- subset(MF, Inclusion !="No") ## this accounts for the 'Yes?' (MAYBE)
MF2 <- MF2 %>%
  mutate(Non.Resource.Interactions = as.integer(Non.Resource.Interactions))

# Standardize punctuation so, for example, "Plant.Gene.Manipulation"
# becomes "plant gene manipulation".
normalize_text <- function(x) {
  x %>%
    coalesce("") %>%
    str_to_lower() %>%
    str_replace_all("₂", "2") %>%
    str_replace_all("[[:punct:]]+", " ") %>%
    str_squish()
}

has <- function(x, patterns) {
  str_detect(x, str_c(patterns, collapse = "|"))
}

MF3 <- MF2 %>%
  mutate(
    manipulation_text = normalize_text(Manipulation.Type),
    notes_text = normalize_text(Notes),
    
    # Search both fields without creating matches across field boundaries.
    combined_text = str_c(manipulation_text, notes_text, sep = " ; "),
    
    Plant.Manipulation = has(
      manipulation_text,
      c(
        "plant gene manipulation",
        "plant host manipulation",
        "host removal",
        "plant addition",
        "plant richness",
        "girdl",
        "\\bco2\\b"
      )
    ) |
      has(
        combined_text,
        c(
          "\\bshading\\b",
          "light availability modified",
          "girdl",
          "\\bweeding\\b",
          "manipulated host diversity",
          "plant gene knockout",
          "rnai (of|on) plant genes",
          "crispr.{0,30}(plant|marchantia)",
          "transgenic plants",
          "gmo medicago",
          "plant mutant",
          "mutant (rice|plants)",
          "mycorrhizal mutants?",
          "wild ?type and mutant",
          "overexpressed gene.{0,40}strigolactone",
          "(elevated|different|ambient and elevated) co2",
          "co2 (enrichment|concentrations|manipulation)"
        )
      ),
    
    Fungal.Manipulation = has(
      combined_text,
      c(
        "inoculat",
        "fungicid",
        "benomyl",
        "mesh ?bags?",
        "mesh cores?",
        "in ?growth",
        "rotated ?cores?",
        "trench",
        "hyphal exclusion",
        "cmn sever",
        "ecm gene manipulation",
        "rnai of laccaria",
        "vector based expression.{0,50}lacarria"
      )
    ) |
      has(manipulation_text, "ecm richness") |
      has(
        notes_text,
        "manipulated amf species richness"
      ),
    
    Environmental.Manipulation = has(
      manipulation_text,
      c(
        "nutrient",
        "fertili",
        "drought",
        "\\bwater\\b",
        "\\btemperature\\b",
        "\\btemp\\b",
        "warming",
        "salinity",
        "metal stress",
        "organic matter addition",
        "\\bn addition\\b",
        "\\bnitrogen\\b",
        "\\bphosphorus\\b",
        "\\bsulfur\\b",
        "\\bo3\\b",
        "\\bco2 n\\b",
        "^n isotopes$",
        "^inoculation p$"
      )
    ) |
      has(
        notes_text,
        c(
          "fertilization experiment",
          "added base cations",
          "n addition",
          "nitrogen addition",
          "phosphorus fertilization",
          "cation additions",
          "altering the proportions",
          "with and without fe coatings",
          "varying n and p concentration"
        )
      ),
    
    Survey.Candidate = has(
      manipulation_text,
      c("\\bsurvey\\b", "\\bgradients?\\b", "community profiling")
    ) |
      has(
        notes_text,
        c(
          "\\bsurvey\\b",
          "along.{0,40}gradient",
          "across.{0,40}gradient",
          "sampling plots",
          "community comparison between"
        )
      ),
    
    # This identifies isotope mentions, not proof of experimental tracing.
    Isotope.Mentioned = has(
      combined_text,
      c(
        "isotop",
        "natural abundance",
        "pulse labell?ing",
        "\\b(13c|14c|15n|33p|32p|2h|n15)\\b"
      )
    ),
    
    Broad.Manipulation.Type = str_c(
      if_else(
        Plant.Manipulation,
        "Plant manipulations; ", ""
      ),
      if_else(
        Fungal.Manipulation,
        "Fungal manipulations/isolations; ", ""
      ),
      if_else(
        Environmental.Manipulation,
        "Environmental manipulations; ", ""
      )
    ) %>%
      str_remove("; $"),
    
    Broad.Manipulation.Type = case_when(
      Broad.Manipulation.Type != "" ~ Broad.Manipulation.Type,
      
      # Survey candidates mentioning isotopes require interpretation.
      Survey.Candidate & !Isotope.Mentioned ~ "Surveys",
      
      TRUE ~ "Needs review"
    )
  ) %>%
  select(-manipulation_text, -notes_text, -combined_text)

### Manual Checking of Codes in Excel -- > Reformat --> 
# Rerun Fig1.Code.10.26.26.R Script using the revised data frame