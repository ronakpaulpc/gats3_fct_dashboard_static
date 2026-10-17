# Here we do Code trials for generating input code for the Fieldwork 
# Monitoring Dashboard. We test it on training Cases of Himachal Pradesh or 
# the actual India Survey Flatfile as per requirement.



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C00 - Installing and loading --------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Working directory check
getwd()
# "C:/Users/Ronak/Documents/ALL Jobs/iips_gats3/gats3_fct_dashboard_static"

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
    "reactable",
    
    # R graph related packages
    "ggstats",
    "RColorBrewer",
    # "scales",               # to change formats and units
    "patchwork",
    "leaflet",
    
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
# data_ex_00 <- read_csv(here("data_tablet_it", "gats3_flatfile_hp_260219.csv"))

# We import the Flatfile or Actual Data for Matching
# VERSION D260315
# data_ex_00 <- read_csv(here("data_tablet_it", "GATS_FlatFile_260315.csv"))
# VERSION D260326
# data_ex_00 <- read_csv(here("data_tablet_it", "GATS_FlatFile_260325.csv"))
# VERSION D260405
# data_ex_00 <- read_csv(here("data_tablet_it", "GATS_FlatFile_260405.csv"))
# VERSION D260414
# data_ex_00 <- read_csv(here("data_tablet_it", "GATS_FlatFile_260414.csv"))
# VERSION D260521
# data_ex_00 <- read_csv(here("data_tablet_it", "GATS_FlatFile_260521.csv"))
# VERSION D260531
# data_ex_00 <- read_csv(here("data_tablet_it", "GATS_FlatFile_260531.csv"))

# # VERSION D260614
# data_ex_00 <- read_csv(here("data_tablet_it", "GATS_FlatFile_260614.csv"))

# VERSION D261006
data_ex_00 <- read_csv(here("data_tablet_it", "GATS_FlatFile_261006.csv"))

# Check the data dictionary
data_ex_00 |> 
    generate_dictionary(details = "basic") |> 
    view(title = "dict_ex_00")
# Check the data
data_ex_00 |> slice_sample(n = 30) |> view("chk")



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C01 - Data Prep ---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's go!


# ** Data prep ------------------------------------------------------------
# Here we prepare the dataset by mutating custom variables needed for 
# preparing the Dashboard.
data_ex_01 <- data_ex_00 |> 
    
    # Constant
    mutate(cons = 1) |> 
    
    # Factor Version of Response Codes
    mutate(
        # HH Survey Response Code
        hq_resp_code = factor(
            HHCurrentEvent,
            levels = c(102, 103, 104, 105, 106, 108, 109, 887,
                       200, 201, 202, 203, 204, 205, 206, 208, 209)
        ),
        # IND Survey Response Code
        iq_resp_code = factor(
            IQCurrentEvent,
            levels = c(302, 303, 304, 307, 308, 309, 887, 
                       400, 402, 403, 404, 407, 408, 409)
        )    
    ) |> 
    
    # Response Rate Across Total, Household and Individual Questionnaires and
    # Person-level Refusal Rate computed based on
    # GATS QUALITY ASSURANCE MANUAL Page 44-45
    # Keep only the relevant Codes for HH and IND Surveys
    mutate(
        hq_curr_event_6c = if_else(
            !(HHCurrentEvent %in% c(200, 202, 203, 209, 204, 208)),
            NA, HHCurrentEvent, missing = NA
        ),
        iq_curr_event_6c = if_else(
            !(IQCurrentEvent %in% c(400, 402, 409, 404, 407, 408)),
            NA, IQCurrentEvent, missing = NA
        )
    ) |> 
    # Prepare binary variables for HH, IND Surveys and Refusal Rate
    # Will use this to sum by Team, FIID, PSU and get the response rates
    mutate(
        # Progress - Household Questionnaire using Actual Event Code
        rrate_hq_1_01 = if_else(
            HHCurrentEvent == 200,
            1, 0, missing = NA
        ),
        # Progress - Household Questionnaire using RR-relevant Event Code
        rrate_hq_2_01 = if_else(
            hq_curr_event_6c == 200,
            1, 0, missing = NA
        ),
        # Progress - Individual Questionnaire using Actual Event Code
        rrate_iq_1_01 = if_else(
            IQCurrentEvent == 400,
            1, 0, missing = NA
        ),
        # Progress - Individual Questionnaire using Actual Event Code
        rrate_iq_2_01 = if_else(
            IQCurrentEvent == 400,
            1, 0, missing = NA
        )
    ) |>  
    
    # Age Distribution [5-year age groups]
    mutate(age_5yc = case_when(
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
    age_5yc = factor(
        age_5yc,
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
    
    # Generate Unique Team ID for FIID-wise completion rate 
    # and response rate calculation
    mutate(
        team_id = str_sub(FIID, 3, 4) |> as.numeric(),
        team_id = str_c(DATA01, " ", "Team", " ", team_id)
    )


# ** Variable checks ------------------------------------------------------
# Please Check the variables after data preparation
# Percentage - Interview Event Actual Codes
data_ex_01 |> 
    tabyl(HHCurrentEvent) |> 
    adorn_pct_formatting() |> 
    adorn_totals()
data_ex_01 |> 
    tabyl(IQCurrentEvent) |> 
    adorn_pct_formatting() |> 
    adorn_totals()
# Percentage - Interview Event All Codes
data_ex_01 |> 
    tabyl(hh_resp_code) |> 
    adorn_pct_formatting() |> 
    adorn_totals()
data_ex_01 |> 
    tabyl(ind_resp_code) |> 
    adorn_pct_formatting() |> 
    adorn_totals()

# Crosstab - 6 Category Final Codes with All Final Codes
data_ex_01 |> tabyl(HHCurrentEvent, hq_curr_event_6c)
data_ex_01 |> tabyl(IQCurrentEvent, iq_curr_event_6c)
data_ex_01 |> tabyl(hq_curr_event_6c, rrate_hq_1_01)
data_ex_01 |> tabyl(iq_curr_event_6c, rrate_iq_1_01)

# Frequency - Binary Completion Code
data_ex_01 |> tabyl(rrate_hq_1_01, show_na = F) |> adorn_totals()
data_ex_01 |> tabyl(rrate_iq_1_01, show_na = F) |> adorn_totals()
data_ex_01 |> tabyl(rrate_hq_2_01, show_na = F) |> adorn_totals()
data_ex_01 |> tabyl(rrate_iq_2_01, show_na = F) |> adorn_totals()

# Check Team Number variable
data_ex_01 |> tabyl(team_id)


# ** Variable labels ------------------------------------------------------
# Add variable labels to selected variables
data_ex_01 <- data_ex_01 |> 
    set_variable_labels(
        # ID Variables
        DATA02 = "PSU Code",
        # FIID = "Field Investigator ID",
        team_id = "Team ID",
        
        # Custom Variables
        hh_resp_code = "HHQ Response Codes",
        ind_resp_code = "INDQ Response Codes",
        final_code_hhq = "Household Survey (Final Code)",
        final_code_indq = "Individual Survey (Final Code)"
    )
# Check the data
data_ex_01 |> view()


