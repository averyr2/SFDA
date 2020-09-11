2020-09-10

``` r
# Libraries
library(tidyverse)
library(rvest)
library(lubridate)
library(readr)
library(readxl)
# Parameters
webpage <- "https://www.dir.ca.gov/fraud_prevention/suspension-list.htm"
css_selector <- "#tbl-1"
provider_xl <- "~/Downloads/CaPhysiciansData2015.xlsx"
#===============================================================================

# Code
```

``` r
medical_fraud <-
  webpage %>% 
  read_html() %>% 
  html_node(css = css_selector) %>% 
  html_table() %>% 
  as_tibble()

medical_fraud <-
  medical_fraud %>% 
  arrange(Name) %>% 
  mutate(
    issued = as.Date(`Notice issued`, format = "%m/%d/%Y"), 
    suspension = as.Date(`Suspension`, format = "%m/%d/%Y")
  ) %>% 
  select(-c(`Notice issued`, Suspension, Appealed))
```

``` r
medical_fraud %>% 
  count(round_date(issued, unit = "quarter"), name = "total_docs") %>% 
  ggplot(aes(`round_date(issued, unit = "quarter")`, total_docs)) +
  geom_line()
```

![](RFAgrant_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
providers <- read_excel(provider_xl, skip = 2)

providers
```

    ## # A tibble: 81 x 67
    ##    `County Name` `Aerospace Medi… `Allergy and Im… Anesthesiology Cardiology
    ##    <chr>                    <dbl>            <dbl>          <dbl>      <dbl>
    ##  1 California                  29              428           3727       2003
    ##  2 Alameda                      0               21            153         78
    ##  3 Amador                       0                0              2          2
    ##  4 Butte                        0                0             21         11
    ##  5 Calaveras                    0                0              2          1
    ##  6 Colusa                       0                0              1          0
    ##  7 Contra Costa                 0               16            124         49
    ##  8 Del Norte                    0                0              0          0
    ##  9 El Dorado                    0                0             12          5
    ## 10 Fresno                       0               12             60         44
    ## # … with 71 more rows, and 62 more variables: `Colon and Rectal Surgery` <dbl>,
    ## #   `Complementary and Alternative Medicine` <dbl>, `Cosmetic Surgery` <dbl>,
    ## #   `Critical Care` <dbl>, Dermatology <dbl>, `Emergency Medicine` <dbl>,
    ## #   Endocrinology <dbl>, Epilepsy <dbl>, `Facial, Plastic and Reconstructive
    ## #   Surgery` <dbl>, `Family Medicine` <dbl>, Gastroenterology <dbl>, `General
    ## #   Practice` <dbl>, `General Surgery` <dbl>, `Geriatric Medicine` <dbl>,
    ## #   Hematology <dbl>, `Hospice and Palliative Medicine` <dbl>, `Infectious
    ## #   Disease` <dbl>, `Internal Medicine` <dbl>, `Medical Genetics` <dbl>,
    ## #   `Neonatal-Perinatal Medicine` <dbl>, Nephrology <dbl>, `Neurological
    ## #   Surgery` <dbl>, Neurology <dbl>, `Nuclear Medicine` <dbl>, `Obstetrics and
    ## #   Gynecology` <dbl>, `Occupational Medicine` <dbl>, Oncology <dbl>,
    ## #   Ophthalmology <dbl>, `Orthopedic Surgery` <dbl>, Otolaryngology <dbl>,
    ## #   `Pain Medicine` <dbl>, Pathology <dbl>, Pediatrics <dbl>, `Pediatric
    ## #   Surgery` <dbl>, `Physical Medicine and Rehabilitation` <dbl>, `Plastic
    ## #   Surgery` <dbl>, Psychiatry <dbl>, `Psychosomatic Medicine` <dbl>, `Public
    ## #   Health and General Preventive Medicine` <dbl>, Pulmonology <dbl>,
    ## #   `Radiation Oncology` <dbl>, Radiology <dbl>, Rheumatology <dbl>, `Sleep
    ## #   Medicine` <dbl>, `Spine Surgery` <dbl>, `Sports Medicine` <dbl>, `Surgery
    ## #   of the Hand` <dbl>, `Surgical Critical Care` <dbl>, `Surgical
    ## #   Oncology` <dbl>, `Thoracic Surgery` <dbl>, Urology <dbl>, `Vascular
    ## #   Surgery` <dbl>, `Other Specialty` <dbl>, `Responded "None"` <dbl>, `Did Not
    ## #   Answer Question` <dbl>, `All Specialties` <dbl>, `All Speciaties excluding
    ## #   None and Did Not Answer` <dbl>, PCPs <dbl>, Specialists <dbl>,
    ## #   Population <dbl>, `PCPs per 100,000 population` <dbl>, `Specialists per
    ## #   100,000 population` <dbl>

``` r
providers_simple <- 
  providers %>% 
  select(
    county = `County Name`, 
    total = `All Specialties`, 
    population = Population, 
    per_hundredk = `Specialists per 100,000 population`
  ) %>% 
  mutate(
    countycolor =
      ifelse(county == "San Francisco", "sf", 
             ifelse(county == "Los Angeles", "la", 
                    ifelse(county == "San Diego", "sd", "no")))) 

providers_simple %>% 
  filter(per_hundredk > 0) %>% 
  filter(population > 100000) %>% 
  ggplot(aes(reorder(county, desc(per_hundredk)), per_hundredk, fill = countycolor)) +
  geom_col() +
  scale_fill_manual(
    values = 
      c("sf" = "tomato", "la" = "deepskyblue1", "sd" = "springgreen3", "no" = "gray70"), 
    guide = FALSE 
  ) +
  geom_text(
    aes( x= "Placer", y= 210, label= "San Francisco"),
    color="black", 
    size=3.5, 
  ) +
  geom_text(
    aes( x= "Sonoma", y= 125, label= "Los Angeles"),
    color="black", 
    size=3.5, 
  ) +
  geom_text(
    aes( x= "Santa Barbara", y= 147, label= "San Diego"),
    color="black", 
    size=3.5, 
  ) +
  scale_y_continuous(limits = c(0,250), expand = c(0, 0)) +
  geom_segment(x = "Contra Costa", xend = "San Diego", y = 140, yend = 117, size = .2) +
  geom_segment(x = "Santa Barbara", xend = "Los Angeles", y = 120, yend = 112, size = .2) +
  theme_minimal() +
  theme(
    axis.text.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major = element_line(colour = "grey88")
  ) +
  labs(
    x = "County", 
    y = "Physicians of All Specialties per 100,000 residents", 
    title = "Physicians of all Specialties per 100,000 Residents in California Counties, 2015"
  )
```

![](RFAgrant_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
