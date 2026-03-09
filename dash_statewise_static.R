# Here we do Code trials for generating input code for the Dashboard of
# Himachal Pradesh.



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
data_ex_00 <- read_csv(here("data_tablet_it", "gats3_flatfile_hp_260219.csv"))

# Check the data dictionary
data_ex_00 |> 
    generate_dictionary(details = "basic") |> 
    view(title = "dict_ex_00")



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
    
    # Overall Progress Check - Based on Status Code
    # Prepare the binary variables for Progress Calculation
    # NOTE: We set missing as "0" across all variables
    mutate(
        progress_all_01 = if_else(
            HHCurrentEvent >= 200 | IQCurrentEvent >= 400,
            1, 0, missing = 0
        ),
        progress_hh_01 = if_else(
            HHCurrentEvent >= 200, 1, 0, missing = 0
        ),
        progress_ind_01 = if_else(
            IQCurrentEvent >= 400, 1, 0, missing = 0
        ),
        progress_e01_01 = if_else(
            !is.na(E01), 1, 0, missing = 0
        )
    ) |>
    
    # Response Rate Across Total, Household and Individual Questionnaires and
    # Person-level Refusal Rate computed based on 
    # GATS QUALITY ASSURANCE MANUAL Page 44-45
    # Keep only the relevant Codes for HH and IND Surveys
    mutate(
        hh_curr_event_6c = if_else(
            !(HHCurrentEvent %in% c(200, 202, 203, 209, 204, 208)),
            NA, HHCurrentEvent, missing = NA
        ),
        ind_curr_event_6c = if_else(
            !(IQCurrentEvent %in% c(400, 402, 409, 404, 407, 408)),
            NA, IQCurrentEvent, missing = NA
        )
    ) |> 
    # Prepare binary variables for HH, IND Surveys and Refusal Rate
    # Will use this to sum by PSU and get the response rates
    mutate(
        # Progress - Household Questionnaire
        rrate_hhq_01 = if_else(
            HHCurrentEvent == 200,
            1, 0, missing = NA
        ),
        # Progress - Individual Questionnaire
        rrate_indq_01 = if_else(
            IQCurrentEvent == 400,
            1, 0, missing = NA
        ),
        # Person-level Refusal Rate
        ref_rate_01 = if_else(
            IQCurrentEvent == 404,
            1, 0, missing = NA
        )
    ) |>
    
    # Final Survey Codes - Recode Pending Codes into Single Category
    # Recategorize HH Questionnaire - Codes <200 is "Pending"
    mutate(final_code_hhq = if_else(
        HHCurrentEvent < 200, 
        "Pending", as.character(HHCurrentEvent), missing = NA
    )) |> 
    # Recategorize IND Questionnaire - Codes < 400 is "Pending"
    mutate(final_code_indq = if_else(
        IQCurrentEvent < 400,
        "Pending", as.character(IQCurrentEvent), missing = NA
    )) |> 
    # Convert to Factor for showing all values in crosstab
    mutate(
        final_code_hhq = factor(
            final_code_hhq, 
            levels = c("200", "201", "202", "203", "204", "205", "206",
                       "208", "209", "999", "Pending")
        ),
        final_code_indq = factor(
            final_code_indq,
            levels = c("400", "402", "403", "404", "407", "408", "409",
                       "Pending")
        )
    ) |> 
    
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


# ** Variable checks ------------------------------------------------------
# Please Check the variables after data preparation
# Frequency - Overall progress
data_ex_01 |> tabyl(progress_all_01)
data_ex_01 |> tabyl(progress_hh_01)
data_ex_01 |> tabyl(progress_ind_01)
data_ex_01 |> tabyl(progress_e01_01)

# Crosstab - 6 Category Final Codes with All Final Codes
data_ex_01 |> tabyl(HHCurrentEvent, hh_curr_event_6c)
data_ex_01 |> tabyl(IQCurrentEvent, ind_curr_event_6c)
data_ex_01 |> tabyl(hh_curr_event_6c, rrate_hhq_01)
data_ex_01 |> tabyl(ind_curr_event_6c, rrate_indq_01)
data_ex_01 |> tabyl(ind_curr_event_6c, ref_rate_01)
# Frequency - Binary Completion Code
data_ex_01 |> tabyl(rrate_hhq_01, show_na = F)
data_ex_01 |> tabyl(rrate_indq_01, show_na = F)
data_ex_01 |> tabyl(ref_rate_01, show_na = F)

# Check - Questionnaire Final Code Recode
# All pending codes should be categorized as "Pending" and all NA should 
# be NA
data_ex_01 |> tabyl(HHCurrentEvent, final_code_hhq)
data_ex_01 |> tabyl(IQCurrentEvent, final_code_indq)


# ** Variable labels ------------------------------------------------------
# Add variable labels to selected variables
data_ex_01 <- data_ex_01 |> 
    set_variable_labels(
        # ID Variables
        # FIID = "Field Investigator ID",
        
        # Custom Variables
        final_code_hhq = "Household Survey (Final Code)",
        final_code_indq = "Individual Survey (Final Code)"
    )



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C02 - 1st Page Overall Progress -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we develop and test the Dashboard Cards and them implement it in the
# Dashboard. For Dashboard we run and check which version Displays the best 
# for each variable, like whether a variable should be displayed as a table,
# a horizontal bar graph, vertical bar graph, etc. So we keep the code for
# all possibilities here.
# This section contains the Cards for Page 1 - Overall Progress.


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
        tot_hh_n = sum(cons),
        comp_hh_n = sum(progress_all_01)
    ) |> 
    # Calculate Number of Districts Completed
    mutate(progress_dist_01 = if_else(
        comp_hh_n >= 30, 
        1, 0, missing = 0
    )) |> 
    # Calculate overall stats of Districts
    summarize(
        tot_dist_n = n(),
        comp_dist_n = sum(progress_dist_01)
    ) |> 
    # Calculate Percentage of Completed Districts
    mutate(comp_dist_pct = (comp_dist_n / tot_dist_n)) |>
    # Prepare the Progress Display Statistics in N (%)
    mutate(stat_dist = str_c(
        comp_dist_n, 
        " (", 
        scales::label_percent()(comp_dist_pct),
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
    # Calculate the Total and Completed Cases across PSUs
    summarize(
        tot_hh_n = sum(cons),
        comp_hh_n = sum(progress_all_01)
    ) |> 
    # Calculate Number of PSUs Completed
    mutate(progress_psu_01 = if_else(
        comp_hh_n >= 30, 
        1, 0, missing = 0
    )) |> 
    # Calculate overall stats of PSUs
    summarize(
        tot_psu_n = n(),
        comp_psu_n = sum(progress_psu_01)
    ) |> 
    # Calculate Percentage of Completed PSUs
    mutate(comp_psu_pct = (comp_psu_n / tot_psu_n)) |>
    # Prepare the Progress Display Statistics in N (%)
    mutate(stat_psu = str_c(
        comp_psu_n, 
        " (", 
        scales::label_percent()(comp_psu_pct),
        ")"
    ))
# Check Progress
data_progress_psu
data_progress_psu$stat_psu

# Transform the Data Overall for Interview Progress calculation Across
# ALL HOUSEHOLDS
data_progress_hh <- data_ex_01 |> 
    # Calculate Total and Completed HH Interviews
    summarize(
        tot_hh_n = sum(cons),
        comp_hh_n = sum(progress_hh_01)
    ) |> 
    # Calculate Percentage of Completed HH Interviews
    mutate(pct_hh = (comp_hh_n / tot_hh_n)) |> 
    # This converts the stat to string, which we don't want
    # mutate(cases_pct = format(cases_pct))
    # Prepare the Progress Display Statistics in N (%)
    mutate(stat_hh = str_c(
        comp_hh_n, 
        " (", 
        scales::label_percent()(pct_hh),
        ")"
    ))
# Check Progress
data_ex_01 |> tabyl(progress_hh_01)
data_progress_hh
data_progress_hh$stat_hh

# Transform the Data Overall for Interview Progress calculation Across
# ALL INDIVIDUALS
# NOTE: CALCULATED BASED ON E01 IS NOT MISSING
data_progress_ind <- data_ex_01 |> 
    # Calculate Total and Completed IND Interviews
    summarize(
        tot_ind_n = sum(cons),
        comp_ind_n = sum(progress_e01_01)
    ) |> 
    # Calculate Percentage of Completed IND Interviews
    mutate(pct_ind = (comp_ind_n / tot_ind_n)) |> 
    # This converts the stat to string, which we don't want
    # mutate(cases_pct = format(cases_pct))
    # Prepare the Progress Display Statistics in N (%)
    mutate(stat_ind = str_c(
        comp_ind_n, 
        " (", 
        scales::label_percent()(pct_ind),
        ")"
    ))
# Check Progress
data_ex_01 |> tabyl(progress_e01_01)
data_progress_ind
data_progress_ind$stat_ind

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
df_consent_part00 <- data_ex_01 |> 
    tabyl(consent_part_fct) |>
    adorn_totals()
df_consent_part00
# Make changes to the consent distribution table For graph
df_consent_part01 <- df_consent_part00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
        rename(
        "status_code" = "consent_part_fct",
        "pct" = "percent"
    ) |> 
    mutate(status = "status", .before = 1) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
df_consent_part01

# Overall Progress Graph - Basic
fig_op00 <- df_consent_part01 |> 
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


# ** District Completion Rate Table ---------------------------------------
# Transform the Data Overall for Interview Progress calculation Across
# ALL DISTRICTS
df_tbl_progress_dist <- data_ex_01 |> 
    # Group by Districts
    group_by(DATA04) |> 
    # Calculate the Total and Completed Cases across Districts
    summarize(
        tot_hh_n = sum(cons),
        comp_hh_n = sum(progress_all_01)
    ) |> 
    # Calculate Prop and % of HHs Completed across Districts
    mutate(
        prop_hh = comp_hh_n / tot_hh_n,
        pct_hh = scales::label_percent()(prop_hh)
    )

# Check table
df_tbl_progress_dist
# View as DT table
datatable(
    df_tbl_progress_dist |> select(-prop_hh),
    colnames = c("Districts", "Total HHs", "Completed HHs", "Completion Rate")
)
# View as gt table
df_tbl_progress_dist |> select(-prop_hh) |> 
    gt() |> 
    cols_label(
        DATA04 = "District Name",
        tot_hh_n = "Total HHs",
        comp_hh_n = "Completed HHs",
        pct_hh = "Completion Rate"
    ) |> 
    gt_theme_espn() |> 
    opt_interactive(
        use_pagination = FALSE
    )


# ** PSU Completion Rate Table --------------------------------------------
# Transform the Data Overall for Interview Progress calculation Across
# ALL PSUS
df_tbl_progress_psu <- data_ex_01 |> 
    # Group by Districts
    group_by(DATA02) |> 
    # Calculate the Total and Completed Cases across Districts
    summarize(
        tot_hh_n = sum(cons),
        comp_hh_n = sum(progress_all_01)
    ) |> 
    # Calculate Prop and % of HHs Completed across Districts
    mutate(
        prop_hh = comp_hh_n / tot_hh_n,
        pct_hh = scales::label_percent()(prop_hh)
    )

# Check table
df_tbl_progress_psu
# View as DT table
datatable(
    df_tbl_progress_psu |> select(-prop_hh),
    colnames = c("PSUs", "Total HHs", "Completed HHs", "Completion Rate")
)
# View as gt table
df_tbl_progress_psu |> select(-prop_hh) |> 
    gt() |> 
    cols_label(
        "DATA02" = "PSU Code",
        "tot_hh_n" = "Total HHs", 
        "comp_hh_n" = "Completed HHs", 
        "pct_hh" = "Completion Rate"
    ) |> 
    gt_theme_espn() |> 
    opt_interactive(
        use_pagination = FALSE
    )


# ** Map of PSU Location --------------------------------------------------
# Assuming your Himachal data is loaded as a dataframe called 'hp_data'
# Replace 'lat', 'lon', and 'HH_ID' with your actual column names from the CDC app
# Prepare the GPS Data
df_ex_gps <- data_ex_01 |>
    # Keep only the required variables
    select(FIID, CASEID, DATA02, DATA04, DATA11, INTRO_PASSIVE_GPS) |> 
    # Filter out the missing values
    filter(!is.na(INTRO_PASSIVE_GPS)) |> 
    # Extract Coordinate values from the GPS Variable
    separate_wider_delim(
        INTRO_PASSIVE_GPS, 
        delim = " ",
        names = c("latitude", "longitude", NA, NA)
    ) |> 
    # Convert the variable type to numeric
    mutate(
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude)
    )

# Draw the Map
leaflet(data = df_ex_gps) |> 
    # Add the default OpenStreetMap basemap
    addTiles() |>
    # Adds a clean, professional basemap (less cluttered than 
    # default OpenStreetMap)
    # addProviderTiles(providers$CartoDB.Positron) |>
    # Map with Full J&K shown
    # addProviderTiles(providers$Esri.NatGeoWorldMap) |> 
    
    # Adds the GPS points with clustering
    addMarkers(
        lat = ~latitude, 
        lng = ~longitude,
        # This is the magic line for large datasets
        clusterOptions = markerClusterOptions(), 
        # Adds a clickable popup showing the consent status for that household
        popup = ~str_c(
            "<b>District: </b>", DATA04, "<br>",
            "<b>PSU Code: </b>", DATA02, "<br>",
            "<b>FIID: </b>", FIID, "<br>",
            "<b>HH No: </b>", DATA11
        )
    )



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C02 - 2nd Page Key Indicators -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we develop and test the Dashboard Cards and them implement it in the
# Dashboard. For Dashboard we run and check which version Displays the best 
# for each variable, like whether a variable should be displayed as a table,
# a horizontal bar graph, vertical bar graph, etc. So we keep the code for
# all possibilities here.
# This section contains the Cards for Page 2 - Key Indicators.

# ** Prevalence Valuebox --------------------------------------------------
# PSU Progress should actually be calculated from the Interview Status Codes 
# of Household (survey0) and Individual (survey1) Questionnaires.
# For a completed Interview both HHCurrentEvent and IQCurrentEvent Codes 
# should be 200 and 400 respectively


# ** Age distribution -----------------------------------------------------
# Age distribution - raw data
data_ex_00 |> tabyl(A03)
data_ex_00 |> tabyl(RSAGE)


# **** DISTN TABLE ====
# Age distribution - Basic
# !!! Missing values will not be displayed!!!
df_age5c00 <- data_ex_01 |> 
    tabyl(age5c, show_na = F) |> 
    adorn_totals()
df_age5c00 


# Age distribution table - For graph
df_age5c01 <- df_age5c00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
df_age5c01


# **** VER BAR GRAPH ====
# Age distribution Graph - Basic
fig_age5c00 <- df_age5c01 |> 
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
df_gender00 <- data_ex_01 |> 
    tabyl(gender, show_na = F) |> 
    adorn_totals()
df_gender00 

# Gender distribution table - For graph
df_gender01 <- df_gender00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
df_gender01


# **** VER BAR GRAPH ====
# Gender distribution Graph - Basic
fig_gender00 <- df_gender01 |> 
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
fig_gender00 <- df_gender01 |> 
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
df_curr_smoke00 <- data_ex_01 |> 
    tabyl(curr_smoke, show_na = F) |> 
    adorn_totals()
df_curr_smoke00 

# Currently smoke tobacco table - For graph
df_curr_smoke01 <- df_curr_smoke00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
df_curr_smoke01

# Currently smoke tobacco Graph - Basic
fig_curr_smoke00 <- df_curr_smoke01 |> 
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
df_curr_smokeless00 <- data_ex_01 |> 
    tabyl(curr_smokeless, show_na = F) |> 
    adorn_totals()
df_curr_smokeless00 

# Currently smokeless tobacco table - For graph
df_curr_smokeless01 <- df_curr_smokeless00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
df_curr_smokeless01

# Currently smokeless tobacco Graph - Basic
fig_curr_smokeless00 <- df_curr_smokeless01 |> 
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
df_seen_ecig00 <- data_ex_01 |> 
    tabyl(seen_ecig, show_na = F) |> 
    adorn_totals()
df_seen_ecig00 

# Seen e-cigarette table - For graph
df_seen_ecig01 <- df_seen_ecig00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
df_seen_ecig01

# Seen e-cigarette Graph - Basic
fig_seen_ecig00 <- df_seen_ecig01 |> 
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
df_curr_ecig00 <- data_ex_01 |> 
    tabyl(curr_ecig, show_na = F) |> 
    adorn_totals()
df_curr_ecig00 

# Currently use e-cigarette table - For graph
df_curr_ecig01 <- df_curr_ecig00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
df_curr_ecig01

# Currently use e-cigarette Graph - Basic
fig_curr_ecig00 <- df_curr_ecig01 |> 
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
df_paan_wotob00 <- data_ex_01 |> 
    tabyl(paan_wotob, show_na = F) |> 
    adorn_totals()
df_paan_wotob00 

# Seen e-cigarette table - For graph
df_paan_wotob01 <- df_paan_wotob00 |> 
    # Convert percent scale to 0-100
    mutate(percent = percent * 100) |> 
    rename(
        "pct" = "percent"
    ) |> 
    # Remove the Row of Totals
    slice(1:(n() - 1))
df_paan_wotob01

# Seen e-cigarette Graph - Basic
fig_paan_wotob00 <- df_paan_wotob01 |> 
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



#_====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C03 - 3rd Page FIID Statistics ------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we develop and test the Dashboard Cards and them implement it in the
# Dashboard. For Dashboard we run and check which version Displays the best 
# for each variable, like whether a variable should be displayed as a table,
# a horizontal bar graph, vertical bar graph, etc. So we keep the code for
# all possibilities here.
# This section contains the Cards for Page 3 - FIID Statistics.

# ** Response Rate Valuebox -----------------------------------------------
# PSU Response Rate should actually be calculated from the Interview Status 
# Codes of Household (survey0) and Individual (survey1) Questionnaires.
# For a completed Interview both HHCurrentEvent and IQCurrentEvent Codes 
# should be 200 and 400 respectively
df_vb_fiid_rrate <- data_ex_01 |> 
    # Calculate Count of Completed HQ and IQ
    summarize(
        tot_hhq_n = sum(!is.na(rrate_hhq_01)),
        tot_indq_n = sum(!is.na(rrate_indq_01)),
        comp_hhq_hh_n = sum(rrate_hhq_01, na.rm = T),
        comp_indq_hh_n = sum(rrate_indq_01, na.rm = T),
        ref_rate_hh_n = sum(ref_rate_01, na.rm = T)
    ) |> 
    # Calculate Response Rate of HQ, IQ and All in proportion
    # Calculate Refusal Rate in proportion
    mutate(
        comp_hhq_hh_pct = comp_hhq_hh_n / tot_hhq_n,
        comp_indq_hh_pct = comp_indq_hh_n / tot_indq_n,
        comp_allq_hh_pct = comp_hhq_hh_pct * comp_indq_hh_pct,
        ref_rate_hh_pct = ref_rate_hh_n / tot_indq_n
    ) |> 
    # Calculate the statistics for displaying in Valuebox
    mutate(
        stat_hrr = scales::label_percent(accuracy = 0.1)(comp_hhq_hh_pct),
        stat_irr = scales::label_percent(accuracy = 0.1)(comp_indq_hh_pct),
        stat_trr = scales::label_percent(accuracy = 0.1)(comp_allq_hh_pct),
        stat_ref_rate = scales::label_percent(accuracy = 0.1)(ref_rate_hh_pct),
    )
# Check the stats
df_vb_fiid_rrate$stat_hrr
df_vb_fiid_rrate$stat_irr
df_vb_fiid_rrate$stat_trr
df_vb_fiid_rrate$stat_ref_rate


# ** Response Rate Table --------------------------------------------------
# Transform the Data for calculating the Response Rate for Overall, 
# Household and Individual Questionnaire.
df_fiid_rrate <- data_ex_01 |> 
    # Group by Field Investigator ID
    group_by(FIID) |> 
    # Calculate Count of HHs
    summarize(
        tot_hhq_n = sum(!is.na(rrate_hhq_01)),
        tot_indq_n = sum(!is.na(rrate_indq_01)),
        comp_hhq_hh_n = sum(rrate_hhq_01, na.rm = T),
        comp_indq_hh_n = sum(rrate_indq_01, na.rm = T),
        ref_rate_hh_n = sum(ref_rate_01, na.rm = T)
    ) |> 
    # Calculate Response Rate of HHs in %
    mutate(
        comp_hhq_hh_pct = comp_hhq_hh_n / tot_hhq_n,
        comp_indq_hh_pct = comp_indq_hh_n / tot_indq_n,
        comp_allq_hh_pct = comp_hhq_hh_pct * comp_indq_hh_pct,
        ref_rate_hh_pct = ref_rate_hh_n / tot_indq_n
    ) |> 
    # Categorical status var of Response Rates
    # Categorical var with scores as category
    mutate(rrate_status = case_when(
        comp_hhq_hh_pct >= 0.92 & comp_indq_hh_pct >= 0.98 & 
            comp_allq_hh_pct >= 0.92 ~ 3,
        comp_hhq_hh_pct < 0.92 & comp_indq_hh_pct < 0.98 &  
            comp_allq_hh_pct < 0.92 ~ 1,
        .default = 2
    )) |> 
    # Convert to factor
    mutate(rrate_status = factor(
        rrate_status,
        levels = c(3, 2, 1),
        labels = c("Good", "Poor", "Very Poor")
    ))

# Check table
df_fiid_rrate |> select(-ends_with("_n"))
# View as gt table
df_fiid_rrate |> 
    select(FIID, ref_rate_hh_pct, comp_hhq_hh_pct, comp_indq_hh_pct, 
           comp_allq_hh_pct, rrate_status) |> 
    # Convert data to gt object
    gt() |> 
    # Add a formal title
    # tab_header(title = md("**Field Investigator Response Rates**")) |> 
    # Add Column labels
    cols_label(
        FIID = "FIID",
        ref_rate_hh_pct = html("Person-level <br>Refusal Rate"),
        comp_hhq_hh_pct = "HRR",
        comp_indq_hh_pct = "IRR",
        comp_allq_hh_pct = "TRR",
        rrate_status = html("Interviewer <br>Performance")
    ) |> 
    # Add footnotes for the response rates
    tab_footnote(
        footnote = "Total Response Rate, Target >= 92%",
        locations = cells_column_labels(comp_allq_hh_pct)
    ) |> 
    tab_footnote(
        footnote = "Household Response Rate, Target >= 92%",
        locations = cells_column_labels(comp_hhq_hh_pct)
    ) |> 
    tab_footnote(
        footnote = "Individual Response Rate, Target >= 98%",
        locations = cells_column_labels(comp_indq_hh_pct)
    ) |> 
    # Add % Symbol
    fmt_percent(
        columns = ends_with("_pct"),
        decimals = 1
    ) |> 
    # Colour Code the Response Rate Status
    data_color(
        columns = rrate_status,
        method = "factor",
        palette = c("Good" = "green3", 
                    "Poor" = "orange", 
                    "Very Poor" = "red")
    ) |> 
    # Add Theme to the gt table 
    gt_theme_espn() |> 
    # Make the table interactive    
    opt_interactive(use_pagination = FALSE)


# ** HH Survey Code by FIID -----------------------------------------------
# Crosstab using gtsummary
data_ex_01 |> 
    tbl_cross(
        FIID, final_code_hhq,
        missing = "no"
    ) |> 
    bold_labels()
# Crosstab using tabyl and gt
data_ex_01 |> 
    tabyl(FIID, HHCurrentEvent, show_na = F) |> 
    adorn_totals(where = c("row", "col")) |> 
    gt()


# ** IND Survey Code by FIID ----------------------------------------------
# Crosstab using gtsummary
data_ex_01 |> 
    tbl_cross(
        FIID, final_code_indq,
        missing = "no"
    ) |> 
    bold_labels()
# Crosstab using tabyl and gt
data_ex_01 |> 
    tabyl(FIID, IQCurrentEvent, show_na = F) |> 
    adorn_totals(where = c("row", "col")) |> 
    gt()





