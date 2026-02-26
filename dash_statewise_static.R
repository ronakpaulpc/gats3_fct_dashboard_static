# Here we do Code trials for generating input code for the Dashboard of
# Himachal Pradesh.



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C00 - Installing and loading --------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Working directory check
getwd()
# "C:/Users/Ronak/Documents/ALL Jobs/iips_gats3/gats3_rworks"

# ** Installing packages --------------------------------------------------
# First we install the required packages
# install.packages(
#     c(
#         "tidyverse",        # universe of pkgs essential for data manipulation
#         "arrow",
#         "babynames",
#         "curl",
#         "duckdb",
#         "devtools",
#         "gapminder",
#         "ggrepel",
#         "ggridges",
#         "ggthemes",         # graph themes for ggplot2
#         "googlesheets4",
#         "hexbin",
#         "janitor",          # cmds for data cleaning
#         "Lahman",
#         "leaflet",
#         "maps",
#         "nycflights13",     # flights datasets
#         "openxlsx",
#         "palmerpenguins",   # penguins datasets
#         "repurrrsive",
#         "styler",           # to modify existing R code to a specific style
#         "tidymodels",
#         "writexl"
#     )
# )

# # Extra packages required for this script
# install.packages("DT")          # R Interface to the DataTables lib
# install.packages("gtExtras")    # Helper Functions for the gt package
# install.packages("reactable")   # Postgre SQL server


# ** Loading packages -----------------------------------------------------
# Loading the required packages
easypackages::libraries(
    # Data i/o
    "here",                 # relative file path
    "rio",                  # file import-export
    
    # Data manipulation
    "janitor",              # data cleaning fns
    "haven",                # stata, sas, spss data io
    "labelled",             # var labelling
    "readxl",               # input excel sheets
    "writexl",              # output excel sheets
    "skimr",                # quick data summary
    "broom",                # view model results
    
    # Data analysis
    # "DHS.rates",            # demographic rates for dhs-like surveys
    # "GeneralOaxaca",        # BO decomposition for non-linear
    "survey",               # apply survey weights
    
    # Analysis output
    "gt",                   # Publication-ready HTML Tables
    "gtExtras",             # Helper functions for gt pkg
    "gtsummary",            # output summary tables
    "flextable",            # creating tables from objects
    "officer",              # editing in office docs
    "DT",
    
    # R graph related packages
    "ggstats",
    "RColorBrewer",
    # "scales",               # to change formats and units
    "patchwork",
    
    # Misc packages
    "tidyverse",            # Data manipulation iron man
    "tictoc"                # Code timing
)
# p_loaded()                        # checking the loaded packages
# unloadNamespace("writexl")        # Unloading a pkg


# ** Setting options ------------------------------------------------------
# Turn off scientific notations
options(scipen = 999)

# Set gtsummary theme for tables
theme_gtsummary_printer(print_engine = "flextable")
# theme_gtsummary_compact()

# Set flextable defaults.
set_flextable_defaults(
    font.size = 12, 
    font.family = "Times New Roman",
    align = "left",
    theme = theme_booktabs
)


# ** Data import ----------------------------------------------------------
# We import the Flatfile for Himachal Pradesh
data_ex_00 <- read_csv(here("data_tablet_it", "gats3_flatfile_hp_260219.csv"))

# Check the data dictionary
data_ex_00 |> 
    generate_dictionary(details = "basic") |> 
    view(title = "dict_ex_00")



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C01 - Data Prep ---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we prepare the dataset by mutating custom variables needed for 
# preparing the Dashboard.
data_ex_01 <- data_ex_00 |> 
    # Constant
    mutate(cons = 1) |> 
    
    # Overall Progress Check - Based on Status Code
    # Prepare the binary variable for Progress Calculation
    # Here we set missing as "0" for progress calculation
    mutate(progress_hh_01 = if_else(
        HHCurrentEvent == 200 & IQCurrentEvent == 400,
        1, 0, missing = 0
    )) |> 
        
    # Overall Progress Check - Based on Consent
    mutate(consent_part = if_else(CONSENT6 == 1, 1, 0, missing = 0)) |> 
    mutate(consent_part_fct = factor(
        consent_part, 
        levels = c(0, 1),
        labels = c("Remaining", "Completed")
    )) |> 
    
    # Age Distribution [5-year age groups]
    mutate(age5c = case_when(
        RSAGE < 15 ~ "14 & Below",
        RSAGE >=15 & RSAGE <=19 ~ "15-19",
        RSAGE >=20 & RSAGE <=24 ~ "20-24",
        RSAGE >=25 & RSAGE <=29 ~ "25-29",
        RSAGE >=30 & RSAGE <=34 ~ "30-34",
        RSAGE >=35 & RSAGE <=39 ~ "35-39",
        RSAGE >=40 & RSAGE <=44 ~ "40-44",
        RSAGE >=45 & RSAGE <=49 ~ "45-49",
        RSAGE >=50 & RSAGE <=54 ~ "50-54",
        RSAGE >=55 & RSAGE <=59 ~ "55-59",
        RSAGE >=60 & RSAGE <=64 ~ "60-64",
        RSAGE >=65 & RSAGE <=69 ~ "65-69",
        RSAGE >=70 & RSAGE <=74 ~ "70-74",
        RSAGE >=75 & RSAGE <=79 ~ "75-79",
        RSAGE >= 80 ~ "80+"
    ),
    age5c = factor(
        age5c,
        levels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                   "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                   "75-79", "80+")
    )) |> 
    # Gender Distribution Check
    mutate(gender = factor(
        A01,
        levels = c(1, 2, 3),
        labels = c("Male", "Female", "Other")
    )) |> 
    mutate(gender_fm = if_else(A01 == 1, 1, 0, missing = NA)) |> 
    mutate(gender_fm = factor(
        gender, 
        levels = c(0, 1), 
        labels = c("Female", "Male")
    )) |> 
    # Currently Smoke Tobacco Prevalence Check
    mutate(curr_smoke = factor(
        B01,
        levels = c(1, 2, 3, -7, -9),
        labels = c("Daily", "Less than Daily", "Not at all", 
                   "Don't know", "Refused")
    )) |>
    # Currently Smokeless Tobacco Prevalence Check
    mutate(curr_smokeless = factor(
        C01,
        levels = c(1, 2, 3, -7, -9),
        labels = c("Daily", "Less than Daily", "Not at all", 
                   "Don't know", "Refused")
    )) |>
    # Heard/Seen E-cigarette Check
    mutate(seen_ecig = factor(
        EC1,
        levels = c(1, 2, -9),
        labels = c("Yes", "No", "Refused")
    )) |> 
    # Currently Use E-cigarette Check
    mutate(curr_ecig = factor(
        EC2,
        levels = c(1, 2, 3, -7, -9),
        labels = c("Daily", "Less than Daily", "Not at all", 
                   "Don't know", "Refused")
    )) |> 
    
    # Consume Paan masala without tobacco
    mutate(paan_wotob = factor(
        CC1,
        levels = c(1, 2, -9),
        labels = c("Yes", "No", "Refused")
    )) |> 
    # Consume Areca nut without tobacco
    mutate(areca_nut = factor(
        CC5,
        levels = c(1, 2, -9),
        labels = c("Yes", "No", "Refused")
    ))



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C02 - DB Overall Progress Page Contents ---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we develop and test the Dashboard Cards and them implement it in the
# Dashboard. For Dashboard we run and check which version Displays the best 
# for each variable, like whether a variable should be displayed as a table,
# a horizontal bar graph, vertical bar graph, etc. So we keep the code for
# all possibilities here.
# This section contains the Cards for the "Overall Progress" Page 

# ** Progress Valuebox ----------------------------------------------------
# PSU Progress should actually be calculated from the Interview Status Codes 
# of Household (survey0) and Individual (survey1) Questionnaires.
# For a completed Interview both HHCurrentEvent and IQCurrentEvent Codes 
# should be 200 and 400 respectively

# Transform the Data Overall for Interview Progress calculation Across
# ALL DISTRICTS
data_progress_dist <- data_ex_01 |> 
    # Group by Districts
    group_by(DATA04) |> 
    # Calculate the Total and Completed Cases across districts
    summarize(
        tot_hh = sum(cons),
        n_hh = sum(progress_hh_01)
    ) |> 
    # Calculate Number of Districts Completed
    mutate(progress_dist_01 = if_else(
        tot_hh == n_hh, 
        1, 0, missing = 0
    )) |> 
    # Calculate overall stats of Districts
    summarize(
        tot_dist = n(),
        n_dist = sum(progress_dist_01)
    ) |> 
    # Calculate Percentage of Completed Districts
    mutate(pct_dist = (n_dist / tot_dist)) |>
    # Prepare the Progress Display Stat in N (%)
    mutate(stat_dist = str_c(
        n_dist, 
        " (", 
        scales::label_percent()(pct_dist),
        ")"
    ))
# Check Progress
data_progress_dist
data_progress_dist$stat_dist

# Transform the Data Overall for Interview Progress calculation Across
# ALL PSUS
data_progress_psu <- data_ex_01 |> 
    # Group by PSUs
    group_by(DATA02) |> 
    # Calculate the Total and Completed Cases across districts
    summarize(
        tot_hh = sum(cons),
        n_hh = sum(progress_hh_01)
    ) |> 
    # Calculate Number of PSUs Completed
    mutate(progress_psu_01 = if_else(
        tot_hh == n_hh, 
        1, 0, missing = 0
    )) |> 
    # Calculate overall stats of PSUs
    summarize(
        tot_psu = n(),
        n_psu = sum(progress_psu_01)
    ) |> 
    # Calculate Percentage of Completed PSUs
    mutate(pct_psu = (n_psu / tot_psu)) |>
    # Prepare the Progress Display Stat in N (%)
    mutate(stat_psu = str_c(
        n_psu, 
        " (", 
        scales::label_percent()(pct_psu),
        ")"
    ))
# Check Progress
data_progress_psu
data_progress_psu$stat_psu

# Transform the Data Overall for Interview Progress calculation Across
# ALL HOUSEHOLDS
data_progress_hh <- data_ex_01 |> 
    # Calculate Total and Completed Cases
    summarize(
        tot_hh = sum(cons),
        n_hh = sum(progress_hh_01)
    ) |> 
    # Calculate Percentage of Completed Cases
    mutate(pct_hh = (n_hh / tot_hh)) |> 
    # This converts the stat to string, which we don't want
    # mutate(cases_pct = format(cases_pct))
    # Prepare the Progress Display Stat in N (%)
    mutate(stat_hh = str_c(
        n_hh, 
        " (", 
        scales::label_percent()(pct_hh),
        ")"
    ))
# Check Progress
data_ex_01 |> tabyl(progress_hh_01)
data_progress_hh
data_progress_hh$stat_hh

# Estimate the Consent refusal rate
data_consent <- data_ex_01 |> 
    tabyl(CONSENT6) |> 
    filter(CONSENT6 == 2) |> 
    mutate(
        n_consent = str_c(
            n, 
            " (", 
            scales::label_percent(accuracy = 0.1)(valid_percent),
            ")"
        ))
# Check the Consent Refusal Rate
data_consent$n_consent


# ** Overall Progress Check -----------------------------------------------
# Overall Progress comes from CONSENT6 var
# Consent distribution table - raw data
data_ex_00 |> tabyl(CONSENT6)

# Consent distribution table - Basic
dt_consent_part00 <- data_ex_01 |> 
    tabyl(consent_part_fct) |>
    adorn_totals()
dt_consent_part00
# Make changes to the consent distribution table For graph
dt_consent_part01 <- dt_consent_part00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
        rename(
        "status_code" = "consent_part_fct",
        "pct" = "percent"
    ) |> 
    mutate(status = "status", .before = 1) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
dt_consent_part01

# Overall Progress Graph - Basic
fig_op00 <- dt_consent_part01 |> 
    ggplot(aes(x = pct, y = status, fill = status_code)) +
    geom_col(
        # colour = "grey10",
        position = position_fill()
    ) +
    geom_text(
        aes(label = str_c(n, " (", format(pct, digits = 1, nsmall = 1), "%)")),
        position = position_fill(),
        hjust = 2
    ) +
    labs(
        x = NULL,
        y = "Cases (%)",
        fill = NULL
    ) +
    theme_minimal()
fig_op00
# Overall Progress Graph - Polished
fig_op01 <- fig_op00 +
    theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom"
    ) +
    scale_fill_brewer(
        guide = guide_legend(reverse = T),
        palette = "YlOrRd"
    )
fig_op01


# ** Age distribution -----------------------------------------------------
# Age distribution - raw data
data_ex_00 |> tabyl(A03)
data_ex_00 |> tabyl(RSAGE)


# **** DISTN TABLE ====
# Age distribution - Basic
# !!! Missing values will not be displayed!!!
dt_age5c00 <- data_ex_01 |> 
    tabyl(age5c, show_na = F) |> 
    adorn_totals()
dt_age5c00 


# Age distribution table - For graph
dt_age5c01 <- dt_age5c00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
dt_age5c01


# **** VER BAR GRAPH ====
# Age distribution Graph - Basic
fig_age5c00 <- dt_age5c01 |> 
    ggplot(aes(x = age5c, y = pct)) +
    # Adding the Bars
    geom_col(
        # colour = "grey10",
        fill = "steelblue", width = 0.7
    ) +
    # Adding the graph labels
    geom_text(
        aes(
            label = str_c(n, " (", format(pct, digits = 1, nsmall = 1), "%)"),
        ),
        hjust = 0,
        vjust = 0,
        size = 3.5,
        fontface = "bold",
        angle = 30
    ) +
    labs(
        x = NULL,
        y = NULL
    ) +
    # Expand Y limits slightly so labels don't hit the top of the box
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme_light()
fig_age5c00

# Age distribution Graph - Polished
fig_age5c01 <- fig_age5c00 +
    theme(
        # panel.grid = element_blank(),
        axis.text.x = element_text(
            face = "bold",
            size = rel(1.2)
        ),
        axis.text.y = element_blank()
    )
fig_age5c01


# ** Gender distribution --------------------------------------------------
# Gender distribution - raw data
data_ex_00 |> tabyl(A01)


# **** DISTN TABLE ====
# Gender distribution - Basic
# !!! Missing values will not be displayed!!!
dt_gender00 <- data_ex_01 |> 
    tabyl(gender, show_na = F) |> 
    adorn_totals()
dt_gender00 

# Gender distribution table - For graph
dt_gender01 <- dt_gender00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
dt_gender01


# **** VER BAR GRAPH ====
# Gender distribution Graph - Basic
fig_gender00 <- dt_gender01 |> 
    ggplot(aes(x = gender, y = pct)) +
    geom_col(
        # colour = "grey10",
        fill = "pink2"
    ) +
    geom_text(
        aes(
            y = 15, 
            label = str_c(n, " (", format(pct, digits = 1, nsmall = 1), "%)"),
        )
    ) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme_minimal()
fig_gender00

# Gender distribution Graph - Polished
fig_gender01 <- fig_gender00 +
    theme(
        # panel.grid = element_blank(),
        axis.text.x = element_text(
            face = "bold",
            size = rel(1.2)
        ),
        axis.text.y = element_blank()
    )
fig_gender01


# **** HOR BAR GRAPH ====
# Gender distribution Graph - Basic
fig_gender00 <- dt_gender01 |> 
    ggplot(aes(x = pct, y = gender |> fct_rev())) +
    geom_col(
        # colour = "grey10",
        fill = "pink2"
    ) +
    geom_text(
        aes(
            x = 15, 
            label = str_c(n, " (", format(pct, digits = 1, nsmall = 1), "%)"),
        )
    ) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme_light()
fig_gender00

# Gender distribution Graph - Polished
fig_gender01 <- fig_gender00 +
    theme(
        # panel.grid = element_blank(),
        axis.text.y = element_text(
            face = "bold",
            size = rel(1.2)
        ),
        axis.text.x = element_blank()
    )
fig_gender01


# ** Currently smoke tobacco ----------------------------------------------
# Currently smoke tobacco - raw data
data_ex_00 |> tabyl(B01)

# Currently smoke tobacco table - Basic
# !!! Missing values will not be displayed!!!
dt_curr_smoke00 <- data_ex_01 |> 
    tabyl(curr_smoke, show_na = F) |> 
    adorn_totals()
dt_curr_smoke00 

# Currently smoke tobacco table - For graph
dt_curr_smoke01 <- dt_curr_smoke00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
dt_curr_smoke01

# Currently smoke tobacco Graph - Basic
fig_curr_smoke00 <- dt_curr_smoke01 |> 
    ggplot(aes(x = pct, y = curr_smoke |> fct_rev())) +
    geom_col(
        # colour = "grey10",
        fill = "orange"
    ) +
    geom_text(
        aes(x = 15, 
            label = str_c(n, " (", format(pct, digits = 1, nsmall = 1), "%)"))
    ) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme_minimal()
fig_curr_smoke00

# Currently smoke tobacco Graph - Polished
fig_curr_smoke01 <- fig_curr_smoke00 +
    xlim(0, 100) +
    theme(
        # panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
            face = "bold",
            size = rel(1.2)
        )
    )
fig_curr_smoke01


# ** Currently smokeless tobacco ------------------------------------------
# Currently smokeless tobacco - raw data
data_ex_00 |> tabyl(C01)

# Currently smokeless tobacco table - Basic
# !!! Missing values will not be displayed!!!
dt_curr_smokeless00 <- data_ex_01 |> 
    tabyl(curr_smokeless, show_na = F) |> 
    adorn_totals()
dt_curr_smokeless00 

# Currently smokeless tobacco table - For graph
dt_curr_smokeless01 <- dt_curr_smokeless00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
dt_curr_smokeless01

# Currently smokeless tobacco Graph - Basic
fig_curr_smokeless00 <- dt_curr_smokeless01 |> 
    ggplot(aes(x = pct, y = curr_smokeless |> fct_rev())) +
    geom_col(
        # colour = "grey10",
        fill = "orange"
    ) +
    geom_text(
        aes(x = 15, 
            label = str_c(n, " (", format(pct, digits = 1, nsmall = 1), "%)"))
    ) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme_minimal()
fig_curr_smokeless00

# Currently smokeless tobacco Graph - Polished
fig_curr_smokeless01 <- fig_curr_smokeless00 +
    xlim(0, 100) +
    theme(
        # panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
            face = "bold",
            size = rel(1.2)
        )
    )
fig_curr_smokeless01


# ** Heard/Seen E-cigarette -----------------------------------------------
# Seen e-cigarette - raw data
data_ex_00 |> tabyl(EC1)

# Seen e-cigarette table - Basic
# !!! Missing values will not be displayed!!!
dt_seen_ecig00 <- data_ex_01 |> 
    tabyl(seen_ecig, show_na = F) |> 
    adorn_totals()
dt_seen_ecig00 

# Seen e-cigarette table - For graph
dt_seen_ecig01 <- dt_seen_ecig00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
dt_seen_ecig01

# Seen e-cigarette Graph - Basic
fig_seen_ecig00 <- dt_seen_ecig01 |> 
    ggplot(aes(x = pct, y = seen_ecig |> fct_rev())) +
    geom_col(
        # colour = "grey10",
        fill = "orange3"
    ) +
    geom_text(
        aes(x = 15, 
            label = str_c(n, " (", format(pct, digits = 1, nsmall = 1), "%)"))
    ) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme_minimal()
fig_seen_ecig00

# Seen e-cigarette Graph - Polished
fig_seen_ecig01 <- fig_seen_ecig00 +
    xlim(0, 100) +
    theme(
        # panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
            face = "bold",
            size = rel(1.2)
        )
    )
fig_seen_ecig01


# ** Currently use E-cigarette --------------------------------------------
# Currently use e-cigarette - raw data
data_ex_00 |> tabyl(EC2)

# Currently use e-cigarette table - Basic
# !!! Missing values will not be displayed!!!
dt_curr_ecig00 <- data_ex_01 |> 
    tabyl(curr_ecig, show_na = F) |> 
    adorn_totals()
dt_curr_ecig00 

# Currently use e-cigarette table - For graph
dt_curr_ecig01 <- dt_curr_ecig00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
dt_curr_ecig01

# Currently use e-cigarette Graph - Basic
fig_curr_ecig00 <- dt_curr_ecig01 |> 
    ggplot(aes(x = pct, y = curr_ecig |> fct_rev())) +
    geom_col(
        # colour = "grey10",
        fill = "orange3"
    ) +
    geom_text(
        aes(x = 15, 
            label = str_c(n, " (", format(pct, digits = 1, nsmall = 1), "%)"))
    ) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme_minimal()
fig_curr_ecig00

# Currently use e-cigarette Graph - Polished
fig_curr_ecig01 <- fig_curr_ecig00 +
    xlim(0, 100) +
    theme(
        # panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
            face = "bold",
            size = rel(1.2)
        )
    )
fig_curr_ecig01


# ** Consume Paan Masala without tobacco ----------------------------------
#### NOTE ####
# NOTE: Make a combined graph of consumption of other tobacco products

# Paan Masala without tobacco - raw data
data_ex_00 |> tabyl(CC1)

# Consume Paan Masala without tobacco - Basic
# !!! Missing values will not be displayed!!!
dt_paan_wotob00 <- data_ex_01 |> 
    tabyl(paan_wotob, show_na = F) |> 
    adorn_totals()
dt_paan_wotob00 

# Seen e-cigarette table - For graph
dt_paan_wotob01 <- dt_paan_wotob00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
dt_paan_wotob01

# Seen e-cigarette Graph - Basic
fig_paan_wotob00 <- dt_paan_wotob01 |> 
    ggplot(aes(x = pct, y = paan_wotob |> fct_rev())) +
    geom_col(
        # colour = "grey10",
        fill = "orange3"
    ) +
    geom_text(
        aes(x = 15, 
            label = str_c(n, " (", format(pct, digits = 1, nsmall = 1), "%)"))
    ) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme_minimal()
fig_paan_wotob00

# Seen e-cigarette Graph - Polished
fig_paan_wotob01 <- fig_paan_wotob00 +
    xlim(0, 100) +
    theme(
        # panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
            face = "bold",
            size = rel(1.2)
        )
    )
fig_paan_wotob01


data_ex_00 |> tabyl(HHCurrentEvent)
data_ex_00 |> tabyl(IQCurrentEvent)
data_ex_00 |> tabyl(CC5)

# NOTE: This variable does not exist in the data
# Smoking prevalence - raw data
data_ex_00 |> tabyl(WP5)


