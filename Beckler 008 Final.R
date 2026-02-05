# --- Load Necessary Libraries ---
# (Combined from v4.8 and Classification Snippet, including shinyjs)
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinyjs) # For show/hide and delay (Task 1: Intro/Classification Flow)
library(scales)
library(shinyWidgets)
library(RColorBrewer)
library(rlang)
library(stringr)
library(purrr)
library(glue)
library(zoo)

# --- Constants ---
# (Some constants from v4.8, new ones for OGIP, and for Intro Pop-up)
ACRES_TO_SQFT <- 43560.0 # For OGIP calculation
ACRES_TO_M2 <- 4046.86     # For potential other calculations
FT_TO_M <- 0.3048         # For potential other calculations
# CO2 density at Standard Conditions (STP: 0°C, 1 atm) approx. 1.84 kg/m^3
# Convert kg/m^3 to tonnes/scf: (kg/m^3) * (m^3/35.3147 scf) * (1 tonne / 1000 kg)
CO2_DENSITY_STP_TONNES_SCF <- 1.84 / 35.3147 / 1000 # ~0.0000521 tonnes/scf (Verify this density value for specific conditions if possible)
N_YEARS_FOR_LM_FIT <- 5 # For forecasting
SCENARIO_RATE_FACTOR <- 0.70 # For fill time scenario (70% of average rate)
KT_CONVERSION <- 1000     # Factor for Mt to kt or tonnes to kt

# --- Constant for Intro Pop-up Duration --- (Task 1: Intro/Classification Flow)
INTRO_POPUP_DURATION_MS <- 10000 # 10 seconds in milliseconds

# --- Reservoir Classification Parameters ---
# (Descriptions are already in English)
classification_params <- list(
  list(id = "co2_density", name = "CO2 Density",
       pos_text = "High", pos_val = "Positive",
       neg_text = "Low", neg_val = "Cautionary"),
  list(id = "porosity", name = "Porosity",
       pos_text = "> 20%", pos_val = "Positive",
       neg_text = "< 10%", neg_val = "Cautionary"),
  list(id = "permeability", name = "Permeability",
       pos_text = "> 100 mD", pos_val = "Positive",
       neg_text = "< 10 mD", neg_val = "Cautionary"),
  list(id = "res_temp", name = "Reservoir Temperature",
       pos_text = "< 120°C", pos_val = "Positive",
       neg_text = "> 150°C", neg_val = "Cautionary"),
  list(id = "res_depth", name = "Reservoir Depth",
       pos_text = "> 800 m", pos_val = "Positive", # Min depth for supercritical CO2
       neg_text = "< 800 m", neg_val = "Cautionary"),
  list(id = "seal_thick", name = "Seal Thickness",
       pos_text = "> 20 m", pos_val = "Positive",
       neg_text = "< 10 m", neg_val = "Cautionary"),
  list(id = "res_pressure", name = "Reservoir Pressure",
       pos_text = "Sufficient (>7.4 MPa / Hydrostatic)", pos_val = "Positive",
       neg_text = "Low (Sub-hydrostatic)", neg_val = "Cautionary"),
  list(id = "seal_litho", name = "Seal Lithology",
       pos_text = "Good Seal (Shale, Salt, etc.)", pos_val = "Positive",
       neg_text = "Poor Seal (Fractured/Faulted)", neg_val = "Cautionary")
)

# --- Helper Functions ---
# (No change to these from v4.9 logic, already in English)
safe_reactive_value <- function(expr, default = NULL) {
  tryCatch({
    val <- expr()
    if (is.null(val) || length(val) == 0 || all(is.na(val))) { return(default) }
    # Fix: Prevent NA conversion if default is numeric and val is single NA
    if (is.numeric(default) && length(val) == 1 && is.na(val)) { return(default) }
    return(val)
  }, error = function(e) {
    warning("Error in safe_reactive_value: ", e$message) # English warning
    return(default)
  })
}

format_number <- function(num, digits = 0) {
  if (is.null(num) || length(num) == 0 || !is.numeric(num)) return("N/A") # N/A is fine
  if (all(is.na(num))) return ("N/A") # N/A is fine
  formatted_nums <- sapply(num, function(n) {
    if (is.na(n) || !is.finite(n)) {
      "N/A" # N/A is fine
    } else {
      tryCatch({
        scales::comma(round(n, digits), accuracy = 10^(-digits))
      }, error = function(e) "N/A") # N/A is fine
    }
  })
  # Return single value if input is single, else the vector
  if (length(num) == 1) return(formatted_nums) else return(formatted_nums)
}

# --- NEW OGIP Calculation Function ---
# Based on provided Python code, translated to R
# Calculates OGIP in MM SCF (Million Standard Cubic Feet)
calculate_ogip_mmscf <- function(params, acres_to_sqft = ACRES_TO_SQFT) {
  required_params <- c("ogip_porosity_percent", "ogip_connate_water_percent", "ogip_area_acres", "ogip_thickness_ft", "ogip_bgi_cuft_scf")
  # Check if all required params are present and numeric
  if (!all(required_params %in% names(params)) ||
      any(sapply(params[required_params], function(p) is.null(p) || !is.numeric(p) || is.na(p)))) {
    warning("OGIP Calculation Error: Missing or invalid input parameter.") # English warning
    return(list(ogip_mmscf = NA_real_, calculation_details = "Missing/invalid input")) # English detail
  }
  
  # Get and validate values
  porosity_percent <- params$ogip_porosity_percent
  connate_water_percent <- params$ogip_connate_water_percent
  area_acres <- params$ogip_area_acres
  thickness_ft <- params$ogip_thickness_ft
  bgi_cuft_scf <- params$ogip_bgi_cuft_scf
  
  # Basic range validation
  if (porosity_percent < 0 || porosity_percent > 100) {
    warning("OGIP Calculation Error: Porosity must be between 0 and 100.") # English warning
    return(list(ogip_mmscf = NA_real_, calculation_details = "Invalid Porosity %")) # English detail
  }
  if (connate_water_percent < 0 || connate_water_percent > 100) {
    warning("OGIP Calculation Error: Connate water saturation must be between 0 and 100.") # English warning
    return(list(ogip_mmscf = NA_real_, calculation_details = "Invalid Connate Water %")) # English detail
  }
  if (area_acres <= 0) {
    warning("OGIP Calculation Error: Area must be positive.") # English warning
    return(list(ogip_mmscf = NA_real_, calculation_details = "Non-positive Area")) # English detail
  }
  if (thickness_ft <= 0) {
    warning("OGIP Calculation Error: Thickness must be positive.") # English warning
    return(list(ogip_mmscf = NA_real_, calculation_details = "Non-positive Thickness")) # English detail
  }
  if (bgi_cuft_scf <= 0) {
    warning("OGIP Calculation Error: Bgi must be positive.") # English warning
    return(list(ogip_mmscf = NA_real_, calculation_details = "Non-positive Bgi")) # English detail
  }
  
  # Convert percentages to fractions
  porosity <- porosity_percent / 100.0
  connate_water <- connate_water_percent / 100.0
  
  # --- Calculations ---
  # 1. Calculate Pore Volume (cu ft)
  pore_volume_cuft <- acres_to_sqft * area_acres * thickness_ft * porosity
  if (!is.finite(pore_volume_cuft)) pore_volume_cuft <- NA_real_
  
  # 2. Calculate Initial Gas Saturation (Sgi)
  initial_gas_saturation <- 1.0 - connate_water
  if (!is.finite(initial_gas_saturation)) initial_gas_saturation <- NA_real_
  
  # 3. Calculate Original Gas In Place (OGIP) in Standard Cubic Feet (SCF)
  ogip_scf <- NA_real_ # Initialize as NA
  if (!is.na(pore_volume_cuft) && !is.na(initial_gas_saturation) && bgi_cuft_scf > 0) {
    ogip_scf <- (pore_volume_cuft * initial_gas_saturation) / bgi_cuft_scf
  } else {
    warning("OGIP Calculation Error: Could not calculate OGIP (SCF) due to invalid intermediate values or Bgi=0.") # English warning
  }
  if (!is.finite(ogip_scf)) ogip_scf <- NA_real_
  
  # 4. Convert OGIP to Million Standard Cubic Feet (MM SCF)
  ogip_mmscf <- ogip_scf / 1e6
  if (!is.finite(ogip_mmscf)) ogip_mmscf <- NA_real_
  
  # Prepare calculation details string (already in English)
  details <- glue::glue(
    "OGIP Calculation Details:\n",
    "--------------------------\n",
    "Inputs:\n",
    "  Porosity:                {format_number(porosity_percent, 1)} %\n",
    "  Connate Water:           {format_number(connate_water_percent, 1)} %\n",
    "  Area:                    {format_number(area_acres, 1)} acres\n",
    "  Thickness:               {format_number(thickness_ft, 1)} ft\n",
    "  Bgi:                     {format_number(bgi_cuft_scf, 6)} cu ft/SCF\n",
    "--------------------------\n",
    "Intermediate Values:\n",
    "  Pore Volume (Vp):      {format_number(pore_volume_cuft, 0)} cu ft\n",
    "  Initial Gas Sat (Sgi):   {format_number(initial_gas_saturation, 3)}\n",
    "  OGIP (SCF):              {format_number(ogip_scf, 0)} SCF\n",
    "--------------------------\n",
    "Result:\n",
    "  OGIP (MM SCF):           {format_number(ogip_mmscf, 2)} MM SCF"
  )
  
  return(list(ogip_mmscf = ogip_mmscf, calculation_details = details))
}

# --- NEW Function to convert OGIP (MM SCF) to kt CO2e ---
convert_ogip_mmscf_to_kt_co2e <- function(ogip_mmscf, co2_density_tonnes_scf = CO2_DENSITY_STP_TONNES_SCF, kt_divisor = KT_CONVERSION) {
  if (is.null(ogip_mmscf) || is.na(ogip_mmscf) || !is.numeric(ogip_mmscf) || ogip_mmscf < 0) {
    warning("OGIP to kt Conversion Error: Invalid OGIP (MM SCF) input.") # English warning
    return(list(capacity_kt = NA_real_, capacity_tonnes = NA_real_, conversion_details = "Invalid OGIP input")) # English detail
  }
  if (is.null(co2_density_tonnes_scf) || is.na(co2_density_tonnes_scf) || co2_density_tonnes_scf <= 0) {
    warning("OGIP to kt Conversion Error: Invalid CO2 density.") # English warning
    return(list(capacity_kt = NA_real_, capacity_tonnes = NA_real_, conversion_details = "Invalid CO2 density")) # English detail
  }
  if (is.null(kt_divisor) || is.na(kt_divisor) || kt_divisor <= 0) {
    warning("OGIP to kt Conversion Error: Invalid kt divisor.") # English warning
    return(list(capacity_kt = NA_real_, capacity_tonnes = NA_real_, conversion_details = "Invalid kt divisor")) # English detail
  }
  
  # Convert MM SCF to SCF
  ogip_scf <- ogip_mmscf * 1e6
  
  # Calculate capacity in tonnes
  capacity_tonnes <- ogip_scf * co2_density_tonnes_scf
  if (!is.finite(capacity_tonnes)) capacity_tonnes <- NA_real_
  
  # Convert tonnes to kilotonnes (kt)
  capacity_kt <- capacity_tonnes / kt_divisor
  if (!is.finite(capacity_kt)) capacity_kt <- NA_real_
  
  # Details string (already in English)
  details <- glue::glue(
    "OGIP to Capacity (kt CO2e) Conversion:\n",
    "---------------------------------------\n",
    "Input OGIP: {format_number(ogip_mmscf, 2)} MM SCF\n",
    "            = {format_number(ogip_scf, 0)} SCF\n",
    "CO2 Density: {format_number(co2_density_tonnes_scf, 6)} tonnes/SCF (at STP)\n",
    "Calculation:\n",
    "  Capacity (tonnes) = OGIP (SCF) * Density (tonnes/SCF)\n",
    "                    = {format_number(ogip_scf, 0)} * {format_number(co2_density_tonnes_scf, 6)}\n",
    "                    = {format_number(capacity_tonnes, 0)} tonnes CO2e\n",
    "  Capacity (kt) = Capacity (tonnes) / {format_number(kt_divisor, 0)}\n",
    "                = {format_number(capacity_tonnes, 0)} / {format_number(kt_divisor, 0)}\n",
    "---------------------------------------\n",
    "Resulting CO2 Storage Capacity: {format_number(capacity_kt, 0)} kt CO2e"
  )
  
  return(list(capacity_kt = capacity_kt, capacity_tonnes = capacity_tonnes, conversion_details = details))
}

# --- REMOVED: Old Capacity Calculation Function ---
# No longer used as OGIP is now the basis
# calculate_storage_capacity_bgi_kt <- function(...) { ... }

# --- Process Emission Data Function ---
# Handles '_CO2e' (assumed Mt) and '_kt' (assumed kt) columns.
# Converts all to kt. Calculates annual/cumulative per plant and total.
process_emission_data_v5_0 <- function(df, plant_choices, mt_to_kt_factor = KT_CONVERSION) {
  req(df, plant_choices) # Require data frame and plant choices
  
  # --- Input Validation ---
  if (!"Year" %in% names(df)) stop("'Year' column is missing in the input data.") # English error
  if (!is.numeric(df$Year)) {
    df$Year <- suppressWarnings(as.numeric(as.character(df$Year)))
    if(any(is.na(df$Year))) stop("'Year' column contains non-numeric values that could not be converted.") # English error
  }
  
  actual_headers <- names(df)
  # Pattern for columns assumed to be Mt (_CO2e)
  expected_mt_pattern <- glue::glue("^(?:{paste(plant_choices, collapse='|')})_Emissions_CO2e$")
  # Pattern for columns assumed to be kt (_kt)
  expected_kt_pattern <- glue::glue("^(?:{paste(plant_choices, collapse='|')})_Emissions_kt$")
  
  found_plant_cols_mt <- grep(expected_mt_pattern, actual_headers, ignore.case = FALSE, value = TRUE)
  found_plant_cols_kt <- grep(expected_kt_pattern, actual_headers, ignore.case = FALSE, value = TRUE)
  
  # Check if any valid columns were found (stop message already in English)
  if (length(found_plant_cols_mt) == 0 && length(found_plant_cols_kt) == 0) {
    stop(glue::glue("No valid annual plant emission columns found. ",
                    "Expected format: 'PlantName_Emissions_CO2e' (assumed Mt) or 'PlantName_Emissions_kt' (assumed kt). ",
                    "Detected plant names: {paste(plant_choices, collapse=', ')}")) # English error
  }
  
  df_processed <- df
  final_kt_cols <- c() # Store the final column names that are in kt units
  
  # --- Process _CO2e (Mt) columns ---
  if (length(found_plant_cols_mt) > 0) {
    for (col in found_plant_cols_mt) {
      # Ensure numeric
      if (!is.numeric(df_processed[[col]])) {
        df_processed[[col]] <- suppressWarnings(as.numeric(as.character(df_processed[[col]])))
      }
      # Replace NA with 0, ensure >= 0, then convert Mt to kt
      df_processed[[col]] <- replace_na(df_processed[[col]], 0)
      df_processed[[col]][df_processed[[col]] < 0] <- 0
      df_processed[[col]] <- df_processed[[col]] * mt_to_kt_factor # Multiply Mt by 1000 -> kt
    }
    # Rename columns from _CO2e to _kt
    new_names_mt_to_kt <- gsub("_Emissions_CO2e$", "_Emissions_kt", found_plant_cols_mt)
    names(df_processed)[match(found_plant_cols_mt, names(df_processed))] <- new_names_mt_to_kt
    final_kt_cols <- c(final_kt_cols, new_names_mt_to_kt)
    print(glue("Processed {length(found_plant_cols_mt)} columns assumed to be Mt (multiplied by {mt_to_kt_factor} to get kt): {paste(found_plant_cols_mt, collapse=', ')}")) # Debug message (English)
  }
  
  # --- Process existing _kt columns ---
  if (length(found_plant_cols_kt) > 0) {
    for (col in found_plant_cols_kt) {
      # Ensure numeric
      if (!is.numeric(df_processed[[col]])) {
        df_processed[[col]] <- suppressWarnings(as.numeric(as.character(df_processed[[col]])))
      }
      # Replace NA with 0, ensure >= 0
      df_processed[[col]] <- replace_na(df_processed[[col]], 0)
      df_processed[[col]][df_processed[[col]] < 0] <- 0
    }
    final_kt_cols <- c(final_kt_cols, found_plant_cols_kt)
    print(glue("Processed {length(found_plant_cols_kt)} columns assumed to be kt: {paste(found_plant_cols_kt, collapse=', ')}")) # Debug message (English)
  }
  
  # Ensure final columns are unique (in case a plant had both _CO2e and _kt columns)
  final_kt_cols <- unique(final_kt_cols)
  if(length(final_kt_cols) == 0) stop("Logic error: No final kt columns identified after processing.") # Stop message English
  
  # Function to get plant identifier from column name
  map_col_to_plant <- function(col_name) { sub("_(Emissions_kt|Emissions_CO2e)$", "", col_name) }
  
  # --- Calculate Annual Sums per Plant ---
  # Use final_kt_cols which are guaranteed to be in kt units
  annual_emissions_per_plant <- df_processed %>%
    select(Year, all_of(final_kt_cols)) %>%
    pivot_longer(cols = all_of(final_kt_cols), names_to = "Source_Column", values_to = "Annual_Emissions_kt") %>%
    mutate(
      Plant_Identifier = map_chr(Source_Column, map_col_to_plant),
      # Double check finite and non-negative (redundant check)
      Annual_Emissions_kt = ifelse(!is.finite(Annual_Emissions_kt) | Annual_Emissions_kt < 0, 0, Annual_Emissions_kt)
    ) %>%
    group_by(Year, Plant_Identifier) %>%
    # Sum in case there were multiple columns for one plant (shouldn't happen with renaming logic)
    summarise(Total_Annual_Emissions_kt = sum(Annual_Emissions_kt, na.rm = TRUE), .groups = 'drop') %>%
    arrange(Year, Plant_Identifier)
  
  if (nrow(annual_emissions_per_plant) == 0) stop("No valid annual emission data found after processing.") # Stop message English
  
  # --- Calculate Cumulative per Plant ---
  cumulative_per_plant <- annual_emissions_per_plant %>%
    arrange(Plant_Identifier, Year) %>%
    group_by(Plant_Identifier) %>%
    mutate(Cumulative_Emissions_kt = cumsum(Total_Annual_Emissions_kt)) %>%
    ungroup()
  
  # --- Calculate Total Annual Emissions Across All Plants ---
  total_annual_emissions <- annual_emissions_per_plant %>%
    group_by(Year) %>%
    summarize(Total_Annual_Emissions_kt = sum(Total_Annual_Emissions_kt, na.rm = TRUE), .groups = 'drop') %>%
    arrange(Year)
  
  # --- Calculate Total Cumulative Emissions Across All Plants ---
  calculated_total_cumulative <- total_annual_emissions %>%
    mutate(Cumulative_Emissions_kt = cumsum(Total_Annual_Emissions_kt)) %>%
    select(Year, Cumulative_Emissions_kt)
  
  # --- Calculate Overall Average Annual Rate --- (From positive annual totals only)
  total_annual_emissions_for_avg <- total_annual_emissions$Total_Annual_Emissions_kt[total_annual_emissions$Total_Annual_Emissions_kt > 0]
  overall_avg_annual_rate <- if (length(total_annual_emissions_for_avg) > 0) {
    mean(total_annual_emissions_for_avg, na.rm = TRUE)
  } else { 0 }
  overall_avg_annual_rate <- ifelse(is.na(overall_avg_annual_rate) || !is.finite(overall_avg_annual_rate) || overall_avg_annual_rate < 0, 0, overall_avg_annual_rate)
  
  # Return the list of processed data frames
  return(list(
    annual_emissions_per_plant = annual_emissions_per_plant,
    cumulative_per_plant = cumulative_per_plant,
    total_annual_emissions = total_annual_emissions,
    calculated_total_cumulative = calculated_total_cumulative,
    overall_avg_annual_rate = overall_avg_annual_rate
  ))
}

# --- Fill Details Function (kt units - uses new capacity source) ---
calculate_fill_details_v5_0 <- function(capacity_kt, calculated_total_cumulative_kt, injection_rate_scenario_kt) {
  fill_year_injection <- NA_real_
  years_to_fill_injection <- NA_real_
  
  # Basic input validation (warnings already in English)
  if (is.null(capacity_kt) || is.na(capacity_kt) || !is.numeric(capacity_kt) || capacity_kt <= 0) {
    warning("Invalid capacity provided for fill calculation (expected OGIP-based kt).") # English warning
    return(list(fill_year_injection = NA, years_to_fill_injection = NA))
  }
  if (is.null(calculated_total_cumulative_kt) || !is.data.frame(calculated_total_cumulative_kt) || nrow(calculated_total_cumulative_kt) == 0 || !"Year" %in% names(calculated_total_cumulative_kt) || !"Cumulative_Emissions_kt" %in% names(calculated_total_cumulative_kt)) {
    warning("Invalid cumulative data structure for fill calculation.") # English warning
    return(list(fill_year_injection = NA, years_to_fill_injection = NA))
  }
  if (is.null(injection_rate_scenario_kt) || is.na(injection_rate_scenario_kt) || !is.numeric(injection_rate_scenario_kt)) {
    injection_rate_scenario_kt <- NA # Treat as NA if non-numeric
    warning("Invalid injection rate scenario provided for fill calculation.") # English warning
  }
  
  # Get the last known cumulative state
  last_calculated_cumulative_row <- calculated_total_cumulative_kt %>%
    filter(is.finite(Year) & is.finite(Cumulative_Emissions_kt)) %>%
    arrange(Year) %>%
    tail(1)
  
  if (nrow(last_calculated_cumulative_row) == 0) {
    warning("Could not determine the last valid cumulative state.") # English warning
    return(list(fill_year_injection = NA, years_to_fill_injection = NA))
  }
  
  last_cumulative_kt_val <- last_calculated_cumulative_row$Cumulative_Emissions_kt
  last_year_data <- last_calculated_cumulative_row$Year
  
  # Check if already full based on historical data
  if (last_cumulative_kt_val >= capacity_kt) {
    # Find the first year it became full
    full_year_data_calculated <- calculated_total_cumulative_kt %>%
      filter(Cumulative_Emissions_kt >= capacity_kt, is.finite(Year)) %>%
      arrange(Year) %>%
      slice_head(n = 1)
    fill_year_injection <- if (nrow(full_year_data_calculated) > 0) full_year_data_calculated$Year else last_year_data
    years_to_fill_injection <- 0 # Already full means 0 years remaining
  }
  # Check if it can be filled based on the scenario rate
  else if (!is.na(injection_rate_scenario_kt) && injection_rate_scenario_kt > 0) {
    remaining_capacity_inj_kt <- capacity_kt - last_cumulative_kt_val
    years_to_fill_inj_calc <- remaining_capacity_inj_kt / injection_rate_scenario_kt
    if (is.finite(years_to_fill_inj_calc) && !is.na(last_year_data)) {
      fill_year_injection <- ceiling(last_year_data + years_to_fill_inj_calc) # Use ceiling for the year
      years_to_fill_injection <- years_to_fill_inj_calc
    } else {
      warning("Years to fill calculation resulted in non-finite value.") # English warning
      # Keep NAs from initialization
    }
  }
  # Handle cases where rate is <= 0 and not already full (cannot fill)
  else if (!is.na(injection_rate_scenario_kt) && injection_rate_scenario_kt <= 0) {
    # Cannot fill if rate is non-positive and not already full. Keep NAs.
    warning("Scenario rate is <= 0, cannot fill remaining capacity.") # English warning
  }
  # Fallback case (should ideally not be reached if inputs are valid)
  else {
    warning("Fill calculation did not result in a valid state.") # English warning
  }
  
  return(list(fill_year_injection = fill_year_injection, years_to_fill_injection = years_to_fill_injection))
}

# --- Default Data ---
# Contains '_CO2e' columns, assumed to be Million Tonnes (Mt)
default_annual_data_mt <- data.frame(
  Year = 1994:2023,
  Ilijan_Emissions_CO2e = c(0.0005, 0.0008, 0.0008, 0.0010, 0.0014, 0.0025, 0.0250, 0.0582, 1.7429, 1.6827, 2.3384, 2.3639, 2.7359, 2.7036, 2.7678, 2.6785, 2.4166, 3.5347, 2.4792, 2.7847, 2.7688, 3.0773, 3.0591, 2.4100, 2.1651, 2.0638, 2.1661, 2.2128, 1.8018, 2.2128),
  SantaRita_Emissions_CO2e = c(0.0034, 0.0035, 0.0033, 0.0044, 0.0065, 0.0067, 0.0067, 0.0657, 1.6341, 1.5254, 2.2578, 2.2570, 2.5374, 2.3680, 2.4242, 2.3474, 2.2376, 2.6264, 2.1131, 2.4738, 2.8673, 2.5928, 2.4130, 1.9838, 1.8018, 1.8036, 1.8018, 1.8036, 1.8018, 1.8018),
  SanLorenzo_Emissions_CO2e = c(0.0017, 0.0007, 0.0017, 0.0018, 0.0022, 0.0032, NA, 0.0652, 0.7698, 0.7596, 1.1349, 1.1460, 1.2510, 1.1550, 1.2330, 1.1400, 1.1836, 1.8941, 1.0750, 1.2176, 1.3584, 1.5586, 1.6846, 1.0662, 0.9957, 0.9962, 0.9862, 1.0362, NA, 1.0362)
)

# Plant Choices (Must match column name prefixes before "_Emissions...")
PLANT_CHOICES <- c("Ilijan", "SantaRita", "SanLorenzo")

# Pre-process default data using the new function
# This will convert Mt to kt
default_processed_list <- tryCatch({
  process_emission_data_v5_0(default_annual_data_mt, PLANT_CHOICES, mt_to_kt_factor = KT_CONVERSION)
}, error = function(e) {
  warning("Failed to process default data: ", e$message) # English warning
  NULL # Return NULL on error
})

# Prepare the final default data structure (if processing was successful)
if (!is.null(default_processed_list)) {
  # Get the annual kt data back into wide format
  default_annual_kt_processed <- default_processed_list$annual_emissions_per_plant %>%
    pivot_wider(names_from = Plant_Identifier, values_from = Total_Annual_Emissions_kt, names_glue = "{Plant_Identifier}_Emissions_kt") %>%
    mutate(across(ends_with("_Emissions_kt"), ~replace_na(., 0))) # Ensure NAs from pivot are 0
  
  # Include the calculated total cumulative
  default_data_kt_processed <- default_annual_kt_processed %>%
    left_join(default_processed_list$calculated_total_cumulative, by = "Year") %>%
    rename(Cumulative_CO2_Emission_kt = Cumulative_Emissions_kt) # Consistent name
  
} else {
  # Fallback: Create an empty structure if processing failed
  warning("Using empty default data due to processing error.") # English warning
  default_data_kt_processed <- data.frame(Year = integer(0))
  # Add expected columns, even if empty
  for (plant in PLANT_CHOICES) {
    default_data_kt_processed[[paste0(plant, "_Emissions_kt")]] <- numeric(0)
  }
  default_data_kt_processed[["Cumulative_CO2_Emission_kt"]] <- numeric(0)
}

# --- End of First Half ---
# --- Plant Display Names --- (No change, already setup for English mapping)
# --- Plant Display Names --- (No change, already setup for English mapping)
get_display_names <- function(internal_names) { setNames(internal_names, gsub("([a-z])([A-Z])", "\\1 \\2", internal_names)) }
PLANT_CHOICES_DISPLAY <- get_display_names(PLANT_CHOICES)


# --- UI Definition (UPDATED Task 1, 2, 8, 10, 11) ---
# NOTE: The UI definition was largely provided in the first half response.
# It is included here structurally but assumed to be correct based on the previous output.
# Key changes applied in the first half were:
#   - All user-facing text set to English.
#   - OGIP numeric inputs start with value = NA_real_.
#   - Forecast years numeric input starts with value = NA_real_.
ui <- dashboardPage(
  dashboardHeader( title = "CO2 Storage Analysis Tool v5.2 (Forecast Btn)", titleWidth = 450, # Version Bump (English Title)
                   tags$li(class = "dropdown",
                           tags$head(
                             tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&family=Roboto+Mono&display=swap"),
                             tags$style(HTML("
                               /* --- Base Styling --- */
                               /* ... (Keep all CSS from v5.1 - Assumed English comments) ... */
                               body { font-family: 'Roboto', sans-serif; }
                               .main-header .logo { font-weight: 700; font-size: 18px; }
                               .box-title { font-weight: 700; color: #495057; }
                               .value-box .icon { transition: all .3s linear; position: absolute; top: -10px; right: 10px; z-index: 0; font-size: 70px; color: rgba(0,0,0,0.15); }
                               .value-box:hover .icon { font-size: 80px; }
                               .plot-container { margin-top: 15px; background-color: #f8f9fa; border-radius: 5px; padding: 10px; border: 1px solid #dee2e6; min-height: 450px; } /* Light background for plots */
                               .shiny-output-error { color: #dc3545; }
                               .shiny-output-error:before { content: 'Error: '; font-weight: bold; }
                               .plant-total-label-container { padding: 10px; border-top: 1px solid #dee2e6; margin-top: 15px; }
                               .plant-total-label { margin-bottom: 5px; font-size: 0.95em; }
                               .plant-total-label strong { color: #003366; } /* Deep blue for plant names */
                               .dome-plot-controls { display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px; }
                               #domeFillPlotContainer, #fillPieChartContainer { border: 1px solid #dee2e6; border-radius: 5px; padding: 5px; margin-bottom: 10px; background-color: #ffffff;}
                               .irs-bar, .irs-bar-edge, .irs-single, .irs-grid-pol { background: #0056b3 !important; border-color: #0056b3 !important; } /* Slider color */

                               /* --- CSS for Classification Modal --- */
                               /* ... (Keep all CSS from v5.1 - Assumed English comments) ... */
                               .classification-result { padding: 15px; border-radius: 5px; margin-top: 15px; border-left: 5px solid; }
                               .classification-suitable { border-left-color: #28a745; background-color: #d4edda; color: #155724; }
                               .classification-not-suitable { border-left-color: #dc3545; background-color: #f8d7da; color: #721c24; }
                               .classification-ambiguous { border-left-color: #ffc107; background-color: #fff3cd; color: #856404; }
                               .classification-result h5 { margin-top: 0; font-weight: bold; }
                               .justification-list { list-style: none; padding-left: 0; font-size: 0.9em; }
                               .justification-list li { margin-bottom: 3px; }

                               /* --- CSS for Intro Modal --- */
                               /* ... (Keep all CSS from v5.1 - Assumed English comments) ... */
                               .intro-modal-content { text-align: center; padding: 20px; }
                               .intro-modal-content h1 { font-size: 2.5em; color: #003366; margin-bottom: 15px; }
                               .intro-modal-content .logo-container { margin-bottom: 20px; }
                               .intro-modal-content svg { width: 100px; height: 100px; }
                               .intro-modal-content .creators-list { font-size: 0.95em; color: #495057; margin-top: 15px; line-height: 1.6; }
                               .intro-modal-content .creators-list strong { color: #003366; }

                               /* Ensure new OGIP inputs look good */
                               .numeric-input-label { font-weight: bold; margin-bottom: 2px; }
                               .input-group .form-control { height: 34px; }
                               .form-group { margin-bottom: 10px; }
                               .help-block { font-size: 0.85em; color: #6c757d; margin-top: 2px; }

                               /* Style for forecast button row (Task 2: Layout) */
                               .forecast-controls-row { display: flex; align-items: flex-end; justify-content: space-between; }
                               .forecast-controls-row .form-group { flex-grow: 1; margin-right: 10px; margin-bottom: 0 !important; } /* Let input grow, remove bottom margin */
                               .forecast-controls-row .btn { white-space: nowrap; } /* Prevent button text wrapping */

                             ")) # End tags$style
                           ), # End tags$head
                           tags$button(id = "upload_help", class = "btn btn-default btn-sm", style = "margin-top: 10px; margin-right: 15px; background-color: transparent; border-color: #adb5bd; color: #495057;", icon("question-circle"), "Help") # English Button Text
                   ) # End tags$li
  ), # End dashboardHeader
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Input & Summary", tabName = "input", icon = icon("upload")), # English Label
      menuItem("Capacity & Forecasts", tabName = "forecasts", icon = icon("chart-line")), # English Label
      menuItem("Plant Emission Details", tabName = "plant_details", icon = icon("industry")), # English Label
      menuItem("Reservoir Simulation", tabName = "reservoir_viz", icon = icon("cube")) # English Label
    )
  ), # End dashboardSidebar
  dashboardBody(
    useShinyjs(), # Important for shinyjs functions to work
    
    # Task 1: Hide main content initially using div and shinyjs in server
    div(id = "main_content_area", style = "display: none;", # Initially hidden
        tabItems(
          # --- Tab 1: Input & Summary ---
          tabItem(tabName = "input",
                  fluidRow(
                    column(width = 4,
                           box(id = "input-panel", title = "1. Upload & Parameters", width = NULL, solidHeader = TRUE, status = "primary", # English Title
                               # File Input (Label is English)
                               fileInput("excelFile", "Upload CO2 Data File (.xlsx, .xls)", accept = c(".xlsx", ".xls")), # English Label
                               div(id = "file_upload_message", style="color: #6c757d; font-style: italic; margin-bottom: 15px;",
                                   # Initial message (updated by server logic - English)
                                   p(icon("info-circle"), " Upload available after suitability check.") # English Text
                               ),
                               tags$hr(),
                               
                               # --- Inputs for OGIP Calculation --- (Labels/Help text English)
                               h4("Capacity Calculation Inputs (OGIP Method)"), # English Title
                               helpText("Enter parameters to calculate Original Gas In Place (OGIP), which will be used as the reservoir's CO2 storage capacity."), # English Text
                               
                               # Task 2: Inputs start empty (value = NA_real_)
                               numericInput("ogip_porosity_percent", label = div("Porosity (%)", class = "numeric-input-label"), value = NA_real_, min = 0, max = 100, step = 1), # English Label
                               numericInput("ogip_connate_water_percent", label = div("Connate water saturation (%)", class = "numeric-input-label"), value = NA_real_, min = 0, max = 100, step = 1), # English Label
                               numericInput("ogip_area_acres", label = div("Area (acres)", class = "numeric-input-label"), value = NA_real_, min = 1, step = 10), # English Label
                               numericInput("ogip_thickness_ft", label = div("Net productive thickness (ft)", class = "numeric-input-label"), value = NA_real_, min = 1, step = 10), # English Label
                               numericInput("ogip_bgi_cuft_scf", label = div("Initial gas FVF (Bgi) (cu ft/SCF)", class = "numeric-input-label"), value = NA_real_, min = 1e-6, step = 1e-4, width="100%"), # English Label
                               helpText("Unit: Reservoir Cubic Feet / Standard Cubic Foot.", class="help-block"), # English Text
                               
                               tags$hr(),
                               # Analyze button (English)
                               actionButton("analyze", "Analyze Data", icon=icon("cogs"), class = "btn-success btn-lg", width="100%"), # English Label
                               # Analysis completion message (hidden initially - updated by server logic - English)
                               hidden(div(id = "analysis_message", style="color: #28a745; font-weight: bold; margin-top: 15px;", p(icon("check-circle"), " Analysis Complete.") )) # English Text
                           ) # End box input-panel
                    ), # End column 4
                    column(width = 8,
                           hidden( # Results panel (hidden initially)
                             box(id = "results-panel", title = "2. Analysis Summary", width = NULL, solidHeader = TRUE, status = "primary", # English Title
                                 fluidRow(
                                   # Capacity Box (shows OGIP-based capacity - subtitle set in server - English)
                                   valueBoxOutput("ogipCapacityBox", width = 6),
                                   # Fill Time Box (uses OGIP-based capacity - subtitle set in server - English)
                                   valueBoxOutput("fillTimeBox", width = 6)
                                 ),
                                 h4("Total Annual Emissions (Calculated)"), # English Title
                                 helpText("Sum of annual emissions from all detected plant columns (converted to kt CO2e)."), # English Text
                                 div(class="plot-container", plotOutput("annualEmissionsPlot", height = 450)) # Plot titles/labels from server (English)
                             ) # End box results-panel
                           ) # End hidden
                    ) # End column 8
                  ) # End fluidRow
          ), # End tabItem input
          
          # --- Tab 2: Capacity & Forecasts (UPDATED Task 2, 8, 11) ---
          tabItem(tabName = "forecasts",
                  fluidRow(
                    column(width = 12,
                           # OGIP Calculation Details Box (Title/Help text English)
                           box(title = "Mathematical Model (OGIP & Capacity Calculation)", width = NULL, solidHeader = TRUE, status = "info", collapsible = TRUE, collapsed = TRUE, # English Title
                               helpText("Calculation of Original Gas In Place (OGIP) and its conversion to CO2 Storage Capacity (kt CO2e)."), # English Text
                               verbatimTextOutput("ogipCapacityModelOutput") # Output ID for OGIP details (Content from server - English)
                           ) # End box
                    ), # End column 12
                    column(width = 12,
                           # Main Forecast Plot Box (Title/Help text English)
                           box(title = "Plant Cumulative Emissions & Forecast vs. Total Capacity", width = NULL, solidHeader = TRUE, status = "success", collapsible = TRUE, # English Title
                               helpText(glue::glue("Calculated cumulative per plant (kt CO2e) with forecasts vs. total capacity (kt CO2e from OGIP). Click 'Calculate Forecast' to update plots.")), # English Text
                               fluidRow(
                                 # Task 2: Added CSS class for layout
                                 div(class="forecast-controls-row",
                                     # Task 3: Forecast years starts empty (value = NA_real_)
                                     numericInput("forecast_years_model_f", "Forecast Years:", value = NA_real_, min = 1, max = 100, step = 1), # English label
                                     actionButton("calculate_forecast_btn", "Calculate Forecast", icon=icon("calculator"), class="btn-info") # Task 2 Button (English)
                                 )
                               ),
                               fluidRow(
                                 column(width=12, h5("Fill Status (Scenario Rate vs OGIP Capacity):", style="margin-top:15px; margin-bottom: 5px;"), uiOutput("forecastSummaryText")) # English title, Content from server (English)
                               ),
                               hr(),
                               # Task 2: This plot will update when calculate_forecast_btn is clicked (Titles/labels from server - English)
                               div(class = "plot-container", plotOutput("plantCumulativeForecastVsCapacityPlot", height = 450)),
                               textOutput("plantCumulativeForecastVsCapacityAnalysis") # Content from server (English)
                           ) # End box
                    ), # End column 12
                    column(width = 12,
                           # Rate vs Fill Plot Box (Title/Help text English)
                           box(title = "Annual Injection Rate vs. Fill Level (Forecasted)", width = NULL, solidHeader = TRUE, status = "warning", collapsible = TRUE, collapsed = FALSE, # English Title
                               # Task 2: Updated help text (English)
                               helpText(glue::glue("Calculated total annual emissions rate (kt CO2e/yr) vs. Fill Level (fraction of OGIP Capacity). Forecast uses {N_YEARS_FOR_LM_FIT}yr trend and is updated by 'Calculate Forecast' button.")), # English Text
                               # Task 2: This plot will also update when calculate_forecast_btn is clicked (Titles/labels from server - English)
                               div(class = "plot-container", plotOutput("rateVsFillPlot", height = 450)),
                               textOutput("rateVsFillAnalysis") # Content from server (English)
                           ) # End box
                    ) # End column 12
                  ) # End fluidRow
          ), # End tabItem forecasts
          
          # --- Tab 3: Plant Emission Details ---
          tabItem(tabName = "plant_details",
                  fluidRow(
                    box(title = "Annual Emission Trends per Plant", width = 6, solidHeader = TRUE, status = "primary", collapsible = TRUE, # English title
                        helpText("Annual CO2e emission trend per plant (in kt CO2e, from processed data)."), # English Text
                        checkboxGroupButtons(inputId = "annual_plant_select", label = "Select Plants:", choices = PLANT_CHOICES_DISPLAY, selected = PLANT_CHOICES, # English label
                                             justified = TRUE, status = "primary", size = "sm",
                                             checkIcon = list(yes = icon("check-square"), no = icon("square"))
                        ),
                        hr(style="margin-top: 10px; margin-bottom: 10px;"),
                        div(class = "plot-container", plotOutput("annualPerPlantPlot", height = 400)), # Titles/labels from server (English)
                        div(class="plant-total-label-container", h5("Total Cumulative Emissions (Calculated, kt CO2e):"), uiOutput("plantTotalLabels") ) # English title, Content from server (English)
                    ),
                    box(title = "Plant Cumulative vs. Calculated Annual Rate", width = 6, solidHeader = TRUE, status = "warning", collapsible = TRUE, # English title
                        helpText("Compares CALCULATED cumulative per plant (kt CO2e, solid, left) with the annual rate CALCULATED from the cumulative difference (kt CO2e/yr, dashed, right)."), # English Text
                        div(class = "plot-container", plotOutput("plantCumulativeVsCalcAnnualRatePlot", height = 450)), # Titles/labels from server (English)
                        textOutput("plantCumulativeVsCalcAnnualRateNote") # Content from server (English)
                    )
                  ) # End fluidRow
          ), # End tabItem plant_details
          
          # --- Tab 4: Reservoir Simulation ---
          tabItem(tabName = "reservoir_viz",
                  fluidRow(
                    column(width = 7, # Made larger for pie chart
                           box(title = "Reservoir Fill Visualization & Proportion", width = NULL, solidHeader = TRUE, status = "danger", # English title
                               div(class="dome-plot-controls",
                                   uiOutput("reservoirYearSelectionUI"), # UI for year selection (Label set in server - English)
                                   actionButton("visualize_dome", "Visualize Fill Level", icon=icon("eye"), class="btn-primary") # English button
                               ),
                               # Updated help text (English)
                               helpText(glue::glue("Fill based on CUMULATIVE emissions up to the selected year, or TOTAL CUMULATIVE emissions if 'All Years' is selected (vs OGIP Capacity).")), # English Text
                               hr(),
                               # Dome Plot (updates needed in server logic - Titles/labels from server - English)
                               div(id="domeFillPlotContainer", plotOutput("domeFillPlot", height = 400) ), # Slightly reduced height
                               hr(),
                               # NEW Pie Chart Output
                               h5("Annual Contribution Proportion (kt CO2e):"), # English title
                               helpText("Shows the proportion of total annual emissions for relevant year(s) contributing to the fill level shown above."), # Adjusted text (English)
                               div(id="fillPieChartContainer", plotlyOutput("fillPieChart", height=350)) # New output ID (Titles/labels from server - English)
                           ) # End box
                    ), # End column 7
                    column(width = 5,
                           box(title = "Conceptual 3D Reservoir Structure", width = NULL, solidHeader = TRUE, status = "info", collapsible = TRUE, collapsed = TRUE, # English title
                               helpText("Conceptual 3D view correlates with fill % visualized (vs OGIP Capacity)."), # English Text
                               plotlyOutput("reservoir3DModel", height = 600) # Fill here should also use OGIP capacity (Titles/labels from server - English)
                           ) # End box
                    ) # End column 5
                  ) # End fluidRow
          ) # End tabItem reservoir_viz
        ) # End tabItems
    ) # End div main_content_area
  ) # End dashboardBody
) # End dashboardPage


# --- Server Logic (UPDATED Task 1, 2, 5, 7, 8, 9, 10, 11, 12 - Both Halves) ---
server <- function(input, output, session) {
  
  # --- Reactive Values Store ---
  rv <- reactiveValues(
    # App State
    app_state = "initializing", # "initializing", "intro", "classification", "running", "error"
    
    # File upload related
    uploaded_data = NULL,          # Raw data from upload
    processed_emission_data = NULL, # List from process_emission_data_v5_0 (either from default or upload)
    data_source = "pending",       # "pending", "embedded", "uploaded", "error"
    data_ready = FALSE,            # Flag if valid data source is available
    
    # Analysis results
    analysis_error = NULL,
    latest_analysis_results = NULL, # Store results from 'Analyze Data' button (params, capacity, emissions, fill)
    
    # OGIP calculation results
    ogip_calculation_result = NULL, # List containing ogip_mmscf and details string
    ogip_capacity_result = NULL,    # List containing capacity_kt, capacity_tonnes, conversion_details
    
    # Classification reactive values
    classification_result_ui = NULL,
    classification_justification_pos = NULL,
    classification_justification_neg = NULL,
    classification_status = "Pending", # "Pending", "SUITABLE", "NOT SUITABLE", "AMBIGUOUS"
    classification_complete = FALSE,
    
    # Task 2: Forecast Plot Data storage
    forecast_plot_data = NULL       # Store results from 'Calculate Forecast' button
  )
  
  # --- Task 1: Intro Pop-up Logic ---
  
  # Function to Create Intro Modal UI (Content is English)
  introModalUI <- function() {
    modalDialog(
      title = NULL, # No default title bar
      easyClose = FALSE, # Cannot close by clicking outside
      footer = NULL, # No buttons
      size = "l", # Large size
      div(class = "intro-modal-content",
          # Simple SVG Logo (Text is English)
          div(class="logo-container",
              HTML('<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
                   <circle cx="50" cy="50" r="45" fill="#e0f2f7" stroke="#003366" stroke-width="3"/>
                   <text x="50" y="55" font-family="Roboto, sans-serif" font-size="14" font-weight="bold" fill="#003366" text-anchor="middle">BECKLER</text>
                   <text x="50" y="70" font-family="Roboto, sans-serif" font-size="8" fill="#0056b3" text-anchor="middle">CO₂ Storage</text>
                 </svg>')
          ),
          h1("BECKLER CO₂ Storage Analysis Tool"), # English Title
          div(class = "creators-list",
              tags$strong("Developed by:"), br(), # English Label
              "Benchz Nicole C. Sobrepeña", br(),
              "Oliven Diandly L. Cosep", br(),
              "Kimm Z. Alvarez", br(),
              "Lloydroldan A. Mejorada"
          )
      ) # End intro-modal-content div
    ) # End modalDialog
  }
  
  # Show Intro Modal on Startup, then proceed to Classification
  observeEvent(TRUE, {
    req(rv$app_state == "initializing") # Ensure this runs only once
    
    # Show Intro Modal
    showModal(introModalUI())
    rv$app_state <- "intro" # Update state
    
    # Schedule the next step after delay
    shinyjs::delay(INTRO_POPUP_DURATION_MS, {
      removeModal() # Remove the Intro Modal
      rv$app_state <- "classification" # Update state
      # After intro, show the Classification Modal
      if (!rv$classification_complete) {
        showModal(classificationModalUI()) # classificationModalUI defined below
      }
    })
    
  }, once = TRUE, ignoreInit = FALSE) # Use `once = TRUE` for single execution
  
  # --- Function to Create Classification Modal UI --- (Content/Titles English)
  classificationModalUI <- function() {
    modalDialog(
      title = HTML("<h3 style='color: #003366;'><i class='fa fa-check-circle' style='color: #28a745;'></i>&nbsp; Reservoir Suitability Classification</h3>"), # English Title
      size = "l",
      easyClose = FALSE, # Prevent closing by clicking outside
      footer = tagList(
        actionButton("classify_button", "Classify Reservoir", icon = icon("cogs"), class = "btn-primary"), # English Button Text
        uiOutput("modal_footer_output") # Placeholder for conditional buttons
      ),
      fluidPage(
        p("Select the indicator that best describes the potential reservoir based on available data. These are based on general screening guidelines."), # English Text
        hr(),
        fluidRow(
          lapply(classification_params, function(param) {
            column(width = 6,
                   selectInput(inputId = paste0("classify_", param$id),
                               label = tags$strong(param$name), # English Label (from param list)
                               choices = setNames(c(param$pos_val, param$neg_val),
                                                  c(param$pos_text, param$neg_text)), # English Choices (from param list)
                               selected = param$pos_val) # Default to Positive
            ) # End column
          }) # End lapply
        ), # End fluidRow for inputs
        hr(),
        uiOutput("classificationResultOutput") # Content generated below (English)
      ) # End fluidPage
    ) # End modalDialog
  }
  
  # --- Logic for Classification Button Click ---
  observeEvent(input$classify_button, {
    # (Logic remains the same as provided, ensures selections are gathered,
    # result determined, and UI updated)
    # 1. Gather selections
    selections <- list()
    selection_display_texts <- list()
    all_inputs_valid <- TRUE
    for (param in classification_params) {
      input_id <- paste0("classify_", param$id)
      selected_value <- input[[input_id]]
      if(is.null(selected_value) || !selected_value %in% c("Positive", "Cautionary")) {
        showNotification(paste("Error: Invalid or missing input value for", param$name), type = "error", duration = 7) # English error
        all_inputs_valid <- FALSE
        break
      }
      selections[[param$id]] <- selected_value
      display_text <- if (selected_value == "Positive") param$pos_text else param$neg_text
      selection_display_texts[[param$name]] <- display_text
    }
    
    if(!all_inputs_valid) {
      rv$classification_result_ui <- NULL; rv$classification_justification_pos <- NULL; rv$classification_justification_neg <- NULL
      rv$classification_status <- "Pending"; rv$classification_complete <- FALSE
      shinyjs::enable("classify_button") # Re-enable on validation error
      return()
    }
    
    # 2. Count Positive vs Cautionary
    all_values <- unlist(selections)
    positive_count <- sum(all_values == "Positive", na.rm = TRUE)
    cautionary_count <- sum(all_values == "Cautionary", na.rm = TRUE)
    
    # 3. Determine Result, Status, and Justification Items (Result messages are English)
    result_ui <- NULL
    result_status <- "AMBIGUOUS"
    positive_justification_items <- c()
    cautionary_justification_items <- c()
    for (param_name in names(selection_display_texts)) {
      param_index <- which(sapply(classification_params, `[[`, "name") == param_name)
      if(length(param_index) > 0){
        param_id_lookup <- classification_params[[param_index[1]]]$id
        param_value <- selections[[param_id_lookup]]
        param_display_text <- selection_display_texts[[param_name]]
        if (!is.null(param_value) && !is.na(param_value)) {
          if (param_value == "Positive") {
            positive_justification_items <- c(positive_justification_items, paste0(param_name, ": ", param_display_text))
          } else if (param_value == "Cautionary") {
            cautionary_justification_items <- c(cautionary_justification_items, paste0(param_name, ": ", param_display_text))
          }
        }
      }
    }
    
    if (positive_count > cautionary_count) {
      result_status <- "SUITABLE"
      result_class <- "classification-suitable"
      result_ui <- tags$div(class = paste("classification-result", result_class),
                            tags$h5(icon("thumbs-up"), "Result: POTENTIALLY SUITABLE for CO2 Storage"), # English Result
                            p("The reservoir shows more positive indicators than cautionary ones, suggesting potential suitability based on the selected screening criteria.")) # English Text
    } else if (cautionary_count > positive_count) {
      result_status <- "NOT SUITABLE"
      result_class <- "classification-not-suitable"
      result_ui <- tags$div(class = paste("classification-result", result_class),
                            tags$h5(icon("thumbs-down"), "Result: LIKELY NOT SUITABLE for CO2 Storage"), # English Result
                            p("The reservoir shows more cautionary indicators than positive ones, suggesting potential challenges or unsuitability based on the selected screening criteria. Further investigation is highly recommended.")) # English Text
    } else { # Equal counts
      result_status <- "AMBIGUOUS"
      result_class <- "classification-ambiguous"
      result_ui <- tags$div(class = paste("classification-result", result_class),
                            tags$h5(icon("question-circle"), "Result: AMBIGUOUS / INDETERMINATE"), # English Result
                            p("The number of positive and cautionary indicators selected is equal. Suitability is unclear based on these criteria alone. More detailed analysis is required.")) # English Text
    }
    
    rv$classification_result_ui <- result_ui
    rv$classification_justification_pos <- positive_justification_items
    rv$classification_justification_neg <- cautionary_justification_items
    rv$classification_status <- result_status
    rv$classification_complete <- TRUE
    # shinyjs::disable("classify_button") # TASK 2: REMOVED this line to allow re-classification
    
  })
  
  # --- Render Conditional Modal Footer --- (Button text English)
  output$modal_footer_output <- renderUI({
    req(rv$classification_complete)
    if (rv$classification_status == "SUITABLE") {
      actionButton("proceed_to_analysis", "Proceed to Analysis Tool", icon = icon("arrow-right"), class = "btn-success") # English Button Text
    } else {
      actionButton("close_app_button", "Close Application", icon = icon("times"), class = "btn-danger") # English Button Text
    }
  })
  
  # --- Logic for Proceed Button Click ---
  observeEvent(input$proceed_to_analysis, {
    req(rv$classification_status == "SUITABLE") # Double-check status
    removeModal() # Close the classification modal dialog
    
    # *** Task 1: Show the main content after classification ***
    shinyjs::show("main_content_area")
    rv$app_state <- "running" # Update state
    
    # Update UI - switch to input tab, enable controls (Messages English)
    updateTabItems(session, "sidebarmenu", selected = "input")
    shinyjs::enable("excelFile")
    shinyjs::enable("analyze")
    shinyjs::html("file_upload_message", "<p style='color:#6c757d; font-style:italic;'><i class='fa fa-info-circle'></i> Using default data. Upload new file or click Analyze.</p>") # Updated message (English)
    
    # Process default data upon proceeding if not already loaded/processed (Notifications English)
    if (is.null(rv$processed_emission_data)) {
      showNotification("Loading default data...", type = "message", duration = 3, id="load_default_msg") # English Notification
      processed_default <- tryCatch({
        process_emission_data_v5_0(default_annual_data_mt, PLANT_CHOICES, mt_to_kt_factor = KT_CONVERSION)
      }, error = function(e) {
        warning("Error processing default data on proceed: ", e$message) # English Warning
        rv$data_source <- "error"
        rv$data_ready <- FALSE
        showNotification(paste("Error loading default data:", e$message), type="error", duration=10) # English Notification
        NULL
      })
      
      if (!is.null(processed_default)) {
        rv$processed_emission_data <- processed_default
        rv$data_source <- "embedded"
        rv$data_ready <- TRUE
        showNotification("Default data loaded and processed (kt). Ready to Analyze.", type="message", duration=5) # English Notification
      }
      removeNotification("load_default_msg")
    }
  })
  
  # --- Logic for Close Application Button Click ---
  observeEvent(input$close_app_button, {
    req(rv$classification_complete, rv$classification_status %in% c("NOT SUITABLE", "AMBIGUOUS"))
    stopApp(paste("Application closed: Reservoir classified as", rv$classification_status)) # Stop message English
  })
  
  # --- Render Classification Result and Justification UI --- (Titles English)
  output$classificationResultOutput <- renderUI({
    req(rv$classification_complete, rv$classification_result_ui)
    positive_list_ui <- NULL
    if (length(rv$classification_justification_pos) > 0) {
      positive_list_ui <- tagList(
        tags$h6("Positive Indicators Selected:", style="color:#155724; font-weight:bold;"), # English Title
        tags$ul(class="justification-list", lapply(rv$classification_justification_pos, tags$li))
      )
    }
    cautionary_list_ui <- NULL
    if (length(rv$classification_justification_neg) > 0) {
      cautionary_list_ui <- tagList(
        tags$h6("Cautionary Indicators Selected:", style="color:#721c24; font-weight:bold;"), # English Title
        tags$ul(class="justification-list", lapply(rv$classification_justification_neg, tags$li))
      )
    }
    tagList(
      rv$classification_result_ui,
      tags$br(),
      fluidRow(
        column(6, positive_list_ui),
        column(6, cautionary_list_ui)
      )
    )
  })
  
  # --- ============================================= ---
  # --- Start of Main Analysis Server Logic           ---
  # --- (Gated by classification, uses new functions) ---
  # --- ============================================= ---
  
  # --- File Upload Logic --- (Notifications/Messages English)
  observeEvent(input$excelFile, {
    # *** GATE: Requires classification to be 'Suitable' ***
    req(rv$classification_complete, message = "Classification must be 'Suitable' before uploading.") # Req message English
    req(rv$classification_status == "SUITABLE", message = "Classification must be 'Suitable' before uploading.") # Req message English (redundant but safe)
    req(input$excelFile)
    file_path <- input$excelFile$datapath
    showNotification("Reading and processing uploaded file...", type = "message", duration = NULL, id="read_upload_msg") # English Notification
    
    # Reset analysis and forecast results if new file is uploaded
    rv$latest_analysis_results <- NULL
    rv$forecast_plot_data <- NULL # Task 2: Reset forecast data
    hide("analysis_message"); hide("results-panel")
    
    # Try reading and processing the file
    processed_upload <- tryCatch({
      df_upload <- read_excel(file_path)
      # Use the NEW processing function
      process_emission_data_v5_0(df_upload, PLANT_CHOICES, mt_to_kt_factor = KT_CONVERSION)
    }, error = function(e) {
      rv$uploaded_data <- NULL # Clear raw data on error
      rv$processed_emission_data <- NULL # Also clear processed data
      rv$data_source <- "error"
      rv$data_ready <- FALSE
      rv$analysis_error <- paste("File Error:", e$message) # English error
      shinyjs::html("file_upload_message", glue("<p style='color:#dc3545;'><i class='fa fa-times-circle'></i> File error: {e$message}. Using default data if available.</p>")) # English message
      showNotification(rv$analysis_error, type = "error", duration = 10) # English Notification
      NULL # Return NULL to indicate error
    })
    
    removeNotification("read_upload_msg") # Remove "Reading..." message
    
    # If upload processing was successful (Messages English)
    if (!is.null(processed_upload)) {
      rv$uploaded_data <- read_excel(file_path) # Store raw uploaded data (optional)
      rv$processed_emission_data <- processed_upload # Store the PROCESSED data
      rv$data_source <- "uploaded"
      rv$data_ready <- TRUE
      rv$analysis_error <- NULL # No error
      shinyjs::html("file_upload_message", "<p style='color:#28a745;'><i class='fa fa-check-circle'></i> Using uploaded file (processed to kt).</p>") # English message
      showNotification("File uploaded and processed (kt). Ready to Analyze.", type = "message", duration = 7) # English Notification
    } else {
      # If upload processing errored, try falling back to default if available (Messages English)
      if(rv$data_source == "error" && !is.null(default_processed_list)) {
        showNotification("Falling back to default data due to upload error.", type = "warning", duration = 5) # English Notification
        rv$processed_emission_data <- default_processed_list
        rv$data_source <- "embedded"
        rv$data_ready <- TRUE
        rv$analysis_error <- NULL # Clear error since fallback is available
        shinyjs::html("file_upload_message", "<p style='color:#ffc107;'><i class='fa fa-exclamation-triangle'></i> Upload failed. Using default data (processed to kt).</p>") # English message
      } else if (rv$data_source == "error") {
        # If error and NO valid default data (Messages English)
        showNotification("Upload failed and no valid default data available.", type = "error", duration = 10) # English Notification
        shinyjs::html("file_upload_message", "<p style='color:#dc3545;'><i class='fa fa-times-circle'></i> Upload failed. Default data also unavailable.</p>") # English message
      }
    }
    
  })
  
  
  # --- Core Analysis Trigger ('Analyze Data' Button) --- (Messages English)
  observeEvent(input$analyze, {
    # *** GATE 1: Requires Classification to be complete and Suitable ***
    req(rv$classification_complete, message = "Please complete Reservoir Classification first.") # English Req Message
    req(rv$classification_status == "SUITABLE", message = "Analysis requires 'Potentially Suitable' classification.") # English Req Message
    # *** GATE 2: App State must be 'running' ***
    req(rv$app_state == "running", message = "Application is not in running state.") # English Req Message
    
    # *** GATE 3: Requires a valid data source ***
    req(rv$data_ready, cancelOutput = TRUE, message="No valid data (default or uploaded) is ready.") # English Req Message
    req(!is.null(rv$processed_emission_data), cancelOutput = TRUE, message="Processed emission data is missing.") # English Req Message
    
    # *** Task 2 GATE: Check if all OGIP inputs are valid ***
    ogip_inputs_valid <- all(
      !is.null(input$ogip_porosity_percent) && is.finite(input$ogip_porosity_percent) && input$ogip_porosity_percent >= 0 && input$ogip_porosity_percent <= 100,
      !is.null(input$ogip_connate_water_percent) && is.finite(input$ogip_connate_water_percent) && input$ogip_connate_water_percent >= 0 && input$ogip_connate_water_percent <= 100,
      !is.null(input$ogip_area_acres) && is.finite(input$ogip_area_acres) && input$ogip_area_acres > 0,
      !is.null(input$ogip_thickness_ft) && is.finite(input$ogip_thickness_ft) && input$ogip_thickness_ft > 0,
      !is.null(input$ogip_bgi_cuft_scf) && is.finite(input$ogip_bgi_cuft_scf) && input$ogip_bgi_cuft_scf > 0
    )
    
    if (!ogip_inputs_valid) {
      showNotification("Please ensure all 'Capacity Calculation Inputs' are filled with valid numbers.", type = "warning", duration = 7) # English Notification
      return() # Stop the analysis
    }
    
    # --- Start of Analysis ---
    progress <- shiny::Progress$new(session, min=0, max=6) # 6 steps now
    progress$set(message = "Starting Analysis...", value = 0) # English Progress Text
    on.exit(progress$close()) # Ensure progress bar closes
    
    rv$analysis_error <- NULL # Reset error
    rv$forecast_plot_data <- NULL # Task 2: Reset forecast data on new analysis
    hide("analysis_message"); hide("results-panel") # Hide results panel initially
    
    # --- Step 1: Get OGIP Parameters ---
    progress$set(message = "Reading OGIP Parameters...", value = 1) # English Progress Text
    ogip_params <- list(
      ogip_porosity_percent = input$ogip_porosity_percent, # Directly use validated input
      ogip_connate_water_percent = input$ogip_connate_water_percent, # Directly use validated input
      ogip_area_acres = input$ogip_area_acres, # Directly use validated input
      ogip_thickness_ft = input$ogip_thickness_ft, # Directly use validated input
      ogip_bgi_cuft_scf = input$ogip_bgi_cuft_scf # Directly use validated input
    )
    
    # --- Step 2: Calculate OGIP (MM SCF) ---
    progress$set(message = "Calculating OGIP...", value = 2) # English Progress Text
    ogip_calc <- calculate_ogip_mmscf(ogip_params)
    rv$ogip_calculation_result <- ogip_calc # Store OGIP calculation result
    
    if (is.na(ogip_calc$ogip_mmscf)) {
      rv$analysis_error <- paste("OGIP Calculation Error:", ogip_calc$calculation_details %||% "Check inputs.") # English error
      showNotification(rv$analysis_error, type = "error", duration=10); rv$latest_analysis_results <- NULL; return() # English Notification
    }
    
    # --- Step 3: Convert OGIP (MM SCF) to Capacity (kt CO2e) ---
    progress$set(message = "Converting OGIP to CO2 Capacity (kt)...", value = 3) # English Progress Text
    ogip_capacity <- convert_ogip_mmscf_to_kt_co2e(ogip_calc$ogip_mmscf)
    rv$ogip_capacity_result <- ogip_capacity # Store conversion result
    
    if (is.na(ogip_capacity$capacity_kt)) {
      rv$analysis_error <- paste("Capacity Conversion Error:", ogip_capacity$conversion_details %||% "Check OGIP/density.") # English error
      showNotification(rv$analysis_error, type = "error", duration=10); rv$latest_analysis_results <- NULL; return() # English Notification
    }
    # This is the capacity value used throughout the app now
    current_capacity_kt <- ogip_capacity$capacity_kt
    
    # --- Step 4: Get Processed Emission Data ---
    progress$set(message = "Preparing Emission Data...", value = 4) # English Progress Text
    emission_info <- rv$processed_emission_data # This is the list from process_emission_data_v5_0
    req(emission_info, names(emission_info)) # Check if valid list
    
    # --- Step 5: Calculate Fill Details using OGIP Capacity ---
    progress$set(message = "Calculating Fill Details...", value = 5) # English Progress Text
    # Calculate the scenario rate
    injection_rate_scenario_kt <- emission_info$overall_avg_annual_rate * SCENARIO_RATE_FACTOR
    
    fill_details <- calculate_fill_details_v5_0(
      capacity_kt = current_capacity_kt, # Use OGIP-based capacity
      calculated_total_cumulative_kt = emission_info$calculated_total_cumulative,
      injection_rate_scenario_kt = injection_rate_scenario_kt
    )
    
    # --- Step 6: Store All Results ---
    progress$set(message = "Storing Results...", value = 6) # English Progress Text
    rv$latest_analysis_results <- list(
      ogip_params = ogip_params, # Store OGIP inputs
      ogip_calculation_result = ogip_calc, # Store OGIP calc details
      ogip_capacity_result = ogip_capacity, # Store capacity conversion details
      capacity_info = list( # Create structure similar to previous version for compatibility
        capacity_kt = current_capacity_kt, # The important value is this one
        capacity_tonnes = ogip_capacity$capacity_tonnes # Tonnes equivalent
      ),
      emission_info = emission_info, # Full list of processed emissions
      injection_rate_scenario_kt = injection_rate_scenario_kt,
      fill_details = fill_details,
      timestamp = Sys.time(),
      data_source = rv$data_source # Which data was used
    )
    
    # --- Analysis Complete ---
    # Show results panel and success message (English)
    show("results-panel"); show("analysis_message")
    showNotification("Analysis complete! You can now calculate forecasts.", type = "message", duration = 5) # Updated message (English)
    
  }) # End observeEvent analyze
  
  
  # --- Reactive Accessor for Analysis Results ('Analyze Data' button output) ---
  # Ensures results are available before use in outputs (Messages English)
  analysis_results <- reactive({
    req(rv$classification_complete, rv$classification_status == "SUITABLE") # Must be suitable
    req(rv$app_state == "running") # App must be running
    req(rv$latest_analysis_results, message = "Click 'Analyze Data' first.") # English Req Message
    req(!is.null(rv$latest_analysis_results$capacity_info), message = "Analysis results incomplete. Re-analyze.") # English Req Message
    return(rv$latest_analysis_results)
  })
  
  # --- Help Modal --- (Content English)
  observeEvent(input$upload_help, {
    showModal(modalDialog(
      title = HTML("<h3 style='color: #003366;'>Tool Instructions</h3>"), # English Title
      HTML(glue(
        "<div style='color: #212529; line-height: 1.6;'>",
        "<p><strong>Step 1: Introduction & Suitability Check</strong></p>",
        "<ul><li>The application starts with an introductory pop-up.</li>",
        "<li>After the introduction, complete the Reservoir Classification pop-up to classify the reservoir's suitability based on screening criteria.</li>",
        "<li>If classified as 'SUITABLE', you can proceed to the analysis tool. Otherwise, the application may close.</li></ul>",
        
        "<p><strong>Step 2: Data Upload (Optional)</strong></p>",
        "<ul>",
        "<li>After proceeding, you can upload an Excel file (.xlsx/.xls) on the 'Input & Summary' tab.</li>",
        "<li>The file must contain a '<b>Year</b>' column (numeric).</li>",
        "<li>It must contain annual emission columns named like '<b>PlantName_Emissions_CO2e</b>' (assumed to be in <strong>Million Tonnes - Mt</strong>) or '<b>PlantName_Emissions_kt</b>' (assumed to be in Kilotonnes). Use the exact plant names: {paste(PLANT_CHOICES, collapse=', ')}.</li>",
        "<li>Values in '..._CO2e' columns (Mt) will be automatically <strong>multiplied by {KT_CONVERSION}</strong> to convert them to kilotonnes (kt).</li>",
        "<li>Values in '..._kt' columns will be used directly (assumed to be in kt).</li>",
        "<li>The app calculates total annual and cumulative emissions (in kt) from these columns.</li>",
        "<li>If no file is uploaded, default data for {paste(PLANT_CHOICES, collapse=', ')} will be used (processed to kt).</li>",
        "</ul>",
        
        "<p><strong>Step 3: Capacity & Analysis</strong></p>",
        "<ul><li>Set the <strong>Capacity Calculation Inputs (OGIP Method)</strong> on the 'Input & Summary' tab (Porosity %, Area acres, etc.). <strong>All these fields must be filled with valid numbers before analysis.</strong></li>", # Emphasize Task 2 requirement
        "<li>The calculated OGIP will determine the CO2 storage capacity used in all analyses.</li>",
        "<li>Click '<b>Analyze Data</b>'. This processes inputs and historical data, enabling forecasts. Results will appear across the tabs.</li></ul>",
        
        # Task 2: Added explanation for forecast button (English)
        "<p><strong>Step 4: Forecasting</strong></p>",
        "<ul><li>Go to the '<b>Capacity & Forecasts</b>' tab.</li>",
        "<li>Enter the desired number of '<b>Forecast Years</b>'.</li>",
        "<li>Click the '<b>Calculate Forecast</b>' button. This will generate and display the forecast plots based on the selected number of years.</li></ul>",
        
        "</div>"
      )),
      easyClose = TRUE, footer = modalButton("Close", icon = icon("times"), class="btn-secondary"), size = "l" # English Button Text
    ))
  })
  
  # --- Theme & Palettes --- (No change)
  theme_light_rich <- reactive({ theme_minimal(base_size = 12) + theme( text = element_text(family = "sans"), plot.background = element_rect(fill = '#FFFFFF', color = NA), panel.background = element_rect(fill = '#FFFFFF', color = NA), panel.grid.major = element_line(color = '#e9ecef', linewidth = 0.4), panel.grid.minor = element_blank(), plot.title = element_text(size = 15, face = 'bold', color = '#003366', hjust = 0), plot.subtitle = element_text(size=11, color="#6C757D", hjust=0, margin=margin(b=10)), axis.title = element_text(size = 11, color = '#003366', face="bold"), axis.text = element_text(size = 10, color = '#495057'), legend.background = element_rect(fill = 'transparent'), legend.key = element_rect(fill = 'transparent'), legend.title = element_text(size = 10, color = '#003366', face="bold"), legend.text = element_text(size = 9, color = '#495057'), plot.caption = element_text(color = "#6C757D", size = 8, face = "italic", hjust=1, margin=margin(t=10)), strip.background = element_rect(fill = '#e9ecef'), strip.text = element_text(color = '#212529', face="bold") ) })
  plant_palette <- reactive({ num_plants <- length(PLANT_CHOICES); if (num_plants <= 8) { setNames(RColorBrewer::brewer.pal(max(3, num_plants), "Dark2")[1:num_plants], PLANT_CHOICES) } else { setNames(colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_plants), PLANT_CHOICES) } })
  plant_palette_display <- reactive({ setNames(plant_palette(), names(PLANT_CHOICES_DISPLAY)) })
  
  
  # --- ============================================= ---
  # --- Start of Output Renderings (Both Halves)    ---
  # --- ============================================= ---
  
  # --- Input & Summary Tab Outputs ---
  
  # ValueBox for OGIP-based Capacity (Subtitle English)
  output$ogipCapacityBox <- renderValueBox({
    results <- analysis_results() # Requires analysis result
    req(results, results$capacity_info)
    capacity_kt_val <- results$capacity_info$capacity_kt # Get OGIP-based capacity
    value_str <- if (!is.null(capacity_kt_val) && is.finite(capacity_kt_val)) paste(format_number(capacity_kt_val, 0), " kt CO2e") else "N/A"
    valueBox(
      value = value_str,
      subtitle = "Est. Capacity (from OGIP)", # Changed subtitle (English)
      icon = icon("warehouse"),
      color = "blue"
    )
  })
  
  # ValueBox for Fill Time (uses OGIP capacity implicitly via analysis_results) (Subtitle/Value text English)
  output$fillTimeBox <- renderValueBox({
    results <- analysis_results()
    req(results, results$fill_details, results$emission_info, results$capacity_info)
    
    fill_year <- results$fill_details$fill_year_injection
    last_cumul_calc_kt <- 0 # Default if no data
    if (!is.null(results$emission_info$calculated_total_cumulative) && nrow(results$emission_info$calculated_total_cumulative) > 0) {
      last_row <- tail(results$emission_info$calculated_total_cumulative, 1)
      if (nrow(last_row) > 0 && "Cumulative_Emissions_kt" %in% names(last_row)) {
        last_cumul_calc_kt <- last_row$Cumulative_Emissions_kt
      }
    }
    capacity_kt_val <- results$capacity_info$capacity_kt # This is the OGIP-based capacity
    inj_rate_scenario <- results$injection_rate_scenario_kt
    
    status_color_name <- "yellow"; status_icon <- icon("question-circle"); fill_year_display <- "N/A" # English Default
    
    if (!is.null(fill_year) && !is.na(fill_year)) {
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      if (fill_year <= current_year) {
        status_color_name <- "red"; status_icon <- icon("calendar-check"); fill_year_display <- as.character(round(fill_year))
        # Check if ACTUALLY full
        if(!is.na(last_cumul_calc_kt) && !is.na(capacity_kt_val) && last_cumul_calc_kt >= capacity_kt_val) fill_year_display <- "Already Full" # English Text
      } else {
        status_color_name <- "green"; status_icon <- icon("calendar-alt"); fill_year_display <- as.character(round(fill_year))
      }
    } else { # fill_year is NA
      if (!is.null(last_cumul_calc_kt) && !is.na(last_cumul_calc_kt) && !is.na(capacity_kt_val) && last_cumul_calc_kt >= capacity_kt_val) {
        status_color_name <- "red"; status_icon <- icon("calendar-check"); fill_year_display <- "Already Full" # English Text
      } else if (!is.null(inj_rate_scenario) && !is.na(inj_rate_scenario) && inj_rate_scenario <= 0) {
        status_color_name <- "yellow"; status_icon <- icon("minus-circle"); fill_year_display <- "Rate <= 0" # English Text
      } else {
        status_color_name <- "yellow"; status_icon <- icon("question-circle"); fill_year_display <- "N/A" # English Text
      }
    }
    valueBox(value = fill_year_display, subtitle = "Est. Fill Year (Scenario Rate)", icon = status_icon, color = status_color_name) # English subtitle
  })
  
  # Annual Emissions Plot (Shows historical total) (Titles/Labels English)
  output$annualEmissionsPlot <- renderPlot({
    results <- analysis_results()
    req(results, results$emission_info$total_annual_emissions)
    plot_data <- results$emission_info$total_annual_emissions
    req(nrow(plot_data) > 0)
    
    ggplot(plot_data, aes(x = Year, y = Total_Annual_Emissions_kt)) +
      geom_line(color = "#0056b3", linewidth = 1.1) +
      geom_point(color = "#0056b3", size = 1.5, alpha = 0.8) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Total Annual CO2e Emissions (Calculated)", # English Title
        subtitle = "Sum of annual emissions from all detected plant columns (in kt CO2e)", # English Subtitle
        x = "Year", # English Label
        y = "Total Annual Emissions (kt CO2e)" # English Label
      ) +
      theme_light_rich() +
      scale_x_continuous(breaks = scales::pretty_breaks(n=10))
    
  }, bg = "transparent", res=96)
  
  
  # --- Capacity & Forecasts Tab Outputs ---
  
  # Output for OGIP Calculation Details (Content comes from functions, already English)
  output$ogipCapacityModelOutput <- renderText({
    results <- analysis_results()
    req(results, results$ogip_calculation_result, results$ogip_capacity_result)
    
    # Combine details from OGIP calc and Capacity conversion
    paste(
      results$ogip_calculation_result$calculation_details,
      "\n\n", # Add space
      results$ogip_capacity_result$conversion_details,
      sep=""
    )
  })
  
  
  # --- Task 2: Forecast Button Logic (Implementation) ---
  observeEvent(input$calculate_forecast_btn, {
    # Requires analysis to be completed first
    results <- analysis_results()
    req(results, message = "Please run 'Analyze Data' first before calculating forecasts.") # English message
    
    forecast_horizon_input <- input$forecast_years_model_f # Get forecast years input (might be NA)
    
    # Validate forecast horizon input (Message English)
    if(is.null(forecast_horizon_input) || is.na(forecast_horizon_input) || !is.numeric(forecast_horizon_input) || forecast_horizon_input < 0 || !is.finite(forecast_horizon_input)) {
      showNotification("Invalid number of forecast years entered. Please enter a non-negative number.", type="error", duration=7) # English Notification
      return()
    }
    forecast_horizon <- floor(forecast_horizon_input) # Ensure integer
    
    # --- Start Forecast Calculation --- (Progress messages English)
    progress_fc <- shiny::Progress$new(session, min=0, max=6)
    progress_fc$set(message = "Calculating Forecast...", value = 0) # English Progress Text
    on.exit(progress_fc$close()) # Ensure progress bar closes
    
    rv$forecast_plot_data <- NULL # Reset previous forecast data
    
    tryCatch({
      # --- Get required data from analysis results ---
      progress_fc$set(message = "Preparing historical data...", value = 1) # English Progress Text
      hist_cumulative_per_plant <- results$emission_info$cumulative_per_plant
      hist_total_annual <- results$emission_info$total_annual_emissions
      hist_total_cumulative <- results$emission_info$calculated_total_cumulative
      capacity_kt_val <- results$capacity_info$capacity_kt # OGIP Capacity
      
      # Ensure sufficient historical data for fitting, and valid capacity
      req(nrow(hist_cumulative_per_plant) > 0,
          nrow(hist_total_annual) >= 2, # Min 2 points for LM fit
          !is.na(capacity_kt_val), capacity_kt_val > 0)
      
      last_hist_year <- max(hist_cumulative_per_plant$Year, na.rm = TRUE)
      if (!is.finite(last_hist_year)) { stop("Cannot determine last historical year.") } # English error
      
      # Handle 0 forecast years - prepare historical data in the correct format
      if (forecast_horizon == 0) {
        progress_fc$set(message = "Preparing historical data only...", value = 2) # English Progress Text
        combined_cumulative_per_plant <- hist_cumulative_per_plant %>%
          mutate(Type = "Historical", Line_Type = factor(Type, levels = c("Historical"))) %>%
          left_join(tibble(Plant_Identifier = unname(PLANT_CHOICES_DISPLAY), Plant_Display = names(PLANT_CHOICES_DISPLAY)), by = "Plant_Identifier") %>%
          filter(!is.na(Plant_Display) & is.finite(Cumulative_Emissions_kt))
        
        combined_fill <- hist_total_cumulative %>%
          mutate(Fill_Level = pmin(1, pmax(0, Cumulative_Emissions_kt / capacity_kt_val))) %>%
          filter(is.finite(Fill_Level))
        
        rate_vs_fill_data <- hist_total_annual %>%
          left_join(combined_fill %>% select(Year, Fill_Level), by = "Year") %>%
          filter(!is.na(Total_Annual_Emissions_kt), !is.na(Fill_Level)) %>%
          mutate(Type = "Historical", Line_Type = factor(Type, levels = c("Historical")))
        
        rv$forecast_plot_data <- list(
          cumulative_per_plant = combined_cumulative_per_plant,
          rate_vs_fill = rate_vs_fill_data,
          forecast_horizon = forecast_horizon # Store horizon used
        )
        showNotification("Forecast calculation complete (0 years). Plots updated.", type = "message", duration = 5) # English notification
        return() # Exit observer for 0 years
      } # End 0 year forecast
      
      
      # --- Define future years ---
      future_years <- (last_hist_year + 1):(last_hist_year + forecast_horizon)
      
      # --- Internal forecast function --- (Warnings English)
      create_forecast_internal <- function(data_chunk, value_col_name, n_years_fit, future_years, ensure_non_decreasing=FALSE, ensure_non_negative=TRUE) {
        value_col_sym <- sym(value_col_name)
        hist_data <- data_chunk %>% filter(Year <= last_hist_year & is.finite(!!value_col_sym))
        n_years_fit_actual <- n_years_fit
        
        if(nrow(hist_data) < n_years_fit) {
          warning(glue("Not enough historical data ({nrow(hist_data)} < {n_years_fit}) for {value_col_name} forecast. Trying with available data.")) # English Warning
          if(nrow(hist_data) >= 2) {
            n_years_fit_actual <- nrow(hist_data)
          } else {
            last_value_row <- hist_data %>% filter(Year == max(Year))
            last_value <- if(nrow(last_value_row) > 0) last_value_row[[value_col_name]][1] else 0
            if(!is.finite(last_value)) last_value = 0
            warning("Cannot forecast with < 2 points, returning flat projection.") # English Warning
            return(data.frame(Year = future_years, Forecast_Value = rep(last_value, length(future_years))))
          }
        }
        
        last_n_years <- hist_data %>% arrange(desc(Year)) %>% head(n_years_fit_actual)
        last_value_row <- hist_data %>% filter(Year == max(Year))
        last_value <- if(nrow(last_value_row) > 0) last_value_row[[value_col_name]][1] else 0
        if(!is.finite(last_value)) last_value = 0
        
        projected_values <- rep(last_value, length(future_years)) # Default: flat
        
        model <- tryCatch(
          lm(as.formula(paste0("`", value_col_name, "` ~ Year")), data = last_n_years),
          error = function(e) {
            warning(glue("LM model failed for {value_col_name}: {e$message}")) # English Warning
            NULL
          }
        )
        
        if (!is.null(model) && !any(is.na(coef(model)))) {
          new_data <- data.frame(Year = future_years)
          predicted <- tryCatch(predict(model, newdata = new_data), error=function(e) {warning("Prediction failed."); rep(last_value, length(future_years))}) # English Warning
          predicted <- ifelse(is.finite(predicted), predicted, last_value)
          
          if (ensure_non_decreasing) {
            cumulative_max_path <- cummax(c(last_value, predicted))
            projected_values <- tail(cumulative_max_path, -1)
            if(length(projected_values)>0 && projected_values[1] < last_value){
              projected_values[1] <- last_value
              projected_values <- cummax(projected_values)
            }
          } else {
            projected_values <- predicted
          }
          if(ensure_non_negative) {
            projected_values <- pmax(0, projected_values)
          }
          projected_values[!is.finite(projected_values)] <- last_value
        }
        
        if(length(projected_values) != length(future_years)) {
          warning("Forecasted value length mismatch.") # English Warning
          return(NULL)
        }
        return(data.frame(Year = future_years, Forecast_Value = projected_values))
      } # End internal forecast function
      
      
      # --- Forecast Cumulative Per Plant ---
      progress_fc$set(message = "Forecasting cumulative per plant...", value = 2) # English Progress Text
      fc_per_plant_list <- hist_cumulative_per_plant %>%
        group_by(Plant_Identifier) %>%
        group_split() %>%
        map(~ {
          fc <- create_forecast_internal(.x, "Cumulative_Emissions_kt", N_YEARS_FOR_LM_FIT, future_years, TRUE, TRUE) # Non-decreasing, Non-negative
          if (!is.null(fc)) {
            fc %>% mutate(Plant_Identifier = first(.x$Plant_Identifier))
          } else { NULL }
        })
      fc_per_plant <- bind_rows(fc_per_plant_list) %>%
        filter(!is.null(Forecast_Value) & is.finite(Forecast_Value)) %>%
        rename(Cumulative_Emissions_kt = Forecast_Value) %>%
        mutate(Type = "Forecast")
      
      # --- Forecast Total Annual ---
      progress_fc$set(message = "Forecasting total annual rate...", value = 3) # English Progress Text
      fc_total_annual <- create_forecast_internal(hist_total_annual, "Total_Annual_Emissions_kt", N_YEARS_FOR_LM_FIT, future_years, FALSE, TRUE) # Can decrease, Non-negative
      fc_total_annual_df <- if (!is.null(fc_total_annual)) {
        fc_total_annual %>%
          rename(Total_Annual_Emissions_kt = Forecast_Value) %>%
          mutate(Type = "Forecast") %>%
          filter(is.finite(Total_Annual_Emissions_kt))
      } else {
        tibble(Year = future_years, Total_Annual_Emissions_kt = NA_real_, Type="Forecast")
      }
      
      # --- Combine Historical and Forecasted Data ---
      progress_fc$set(message = "Combining datasets...", value = 4) # English Progress Text
      combined_cumulative_per_plant <- bind_rows(
        hist_cumulative_per_plant %>% mutate(Type = "Historical"),
        fc_per_plant
      ) %>%
        left_join(tibble(Plant_Identifier = unname(PLANT_CHOICES_DISPLAY), Plant_Display = names(PLANT_CHOICES_DISPLAY)), by = "Plant_Identifier") %>%
        mutate(Line_Type = factor(Type, levels = c("Historical", "Forecast"))) %>%
        filter(!is.na(Plant_Display) & is.finite(Cumulative_Emissions_kt)) # Filter invalid rows
      
      combined_annual <- bind_rows(
        hist_total_annual %>% mutate(Type = "Historical"),
        fc_total_annual_df
      ) %>% filter(is.finite(Total_Annual_Emissions_kt))
      
      # --- Forecast Total Cumulative (Directly from total historical) ---
      progress_fc$set(message = "Forecasting total cumulative...", value = 5) # English Progress Text
      fc_total_cumulative <- create_forecast_internal(hist_total_cumulative, "Cumulative_Emissions_kt", N_YEARS_FOR_LM_FIT, future_years, TRUE, TRUE)
      fc_total_cumulative_df <- if (!is.null(fc_total_cumulative)) {
        fc_total_cumulative %>%
          rename(Cumulative_Emissions_kt = Forecast_Value) %>%
          mutate(Type = "Forecast") %>%
          filter(is.finite(Cumulative_Emissions_kt))
      } else {
        tibble(Year = future_years, Cumulative_Emissions_kt = NA_real_, Type="Forecast")
      }
      
      combined_total_cumulative <- bind_rows(
        hist_total_cumulative %>% mutate(Type = "Historical"),
        fc_total_cumulative_df
      ) %>% filter(is.finite(Cumulative_Emissions_kt))
      
      # --- Calculate Fill Level using OGIP Capacity ---
      combined_fill <- combined_total_cumulative %>%
        mutate(Fill_Level = pmin(1, pmax(0, Cumulative_Emissions_kt / capacity_kt_val))) %>% # Uses OGIP capacity
        filter(is.finite(Fill_Level))
      
      # --- Prepare Data for Rate vs Fill Plot ---
      all_years_range <- range(c(combined_annual$Year, combined_fill$Year), na.rm = TRUE)
      req(all(is.finite(all_years_range)), diff(all_years_range) >= 0)
      all_years_df <- data.frame(Year = seq(all_years_range[1], all_years_range[2]))
      
      rate_vs_fill_data <- all_years_df %>%
        left_join(combined_annual %>% select(Year, Total_Annual_Emissions_kt, Type), by = "Year") %>%
        left_join(combined_fill %>% select(Year, Fill_Level), by = "Year") %>%
        mutate(Type = case_when( # Ensure Type covers full range
          Year <= last_hist_year ~ "Historical",
          Year > last_hist_year ~ "Forecast",
          TRUE ~ NA_character_ )) %>%
        filter(!is.na(Total_Annual_Emissions_kt), !is.na(Fill_Level), !is.na(Type)) %>%
        mutate(Line_Type = factor(Type, levels = c("Historical", "Forecast")))
      
      # --- Store results ---
      progress_fc$set(message = "Finalizing...", value = 6) # English Progress Text
      rv$forecast_plot_data <- list(
        cumulative_per_plant = combined_cumulative_per_plant,
        rate_vs_fill = rate_vs_fill_data,
        forecast_horizon = forecast_horizon # Store horizon used
      )
      showNotification(glue("Forecast calculation complete ({forecast_horizon} years). Plots updated."), type = "message", duration = 5) # English notification
      
    }, error = function(e) {
      # Error handling for forecast calculation (Message English)
      rv$forecast_plot_data <- NULL # Clear data on error
      showNotification(paste("Forecast Calculation Error:", e$message), type="error", duration=10) # English Notification
      warning("Forecast Calculation Error: ", e$message) # English Warning
    }) # End tryCatch
    
  }) # End observeEvent calculate_forecast_btn
  
  
  # --- Render Forecast Plots (Task 2 - Using rv$forecast_plot_data) ---
  
  output$plantCumulativeForecastVsCapacityPlot <- renderPlot({
    # --- Task 2: Depends on rv$forecast_plot_data & analysis_results ---
    a_results <- analysis_results()
    fc_plot_data_list <- rv$forecast_plot_data
    req(a_results) # Need analysis results for capacity line
    
    # Show placeholder if forecast not calculated yet (Message English)
    if (is.null(fc_plot_data_list)) {
      msg <- "Click 'Calculate Forecast' to generate plot." # English Placeholder Text
      capacity_kt_val_pl <- if(!is.null(a_results$capacity_info$capacity_kt)) a_results$capacity_info$capacity_kt else NA
      gg_placeholder <- ggplot() + theme_void() +
        geom_text(aes(x=0.5, y=0.6), label=msg, color="grey50", size=5) +
        labs(title = "Plant Cumulative Emissions & Forecast vs. Capacity") # English title
      if (!is.na(capacity_kt_val_pl)) {
        gg_placeholder <- gg_placeholder + geom_hline(yintercept=capacity_kt_val_pl, linetype="dotted", color="#A52A2A", linewidth=1) +
          annotate("text", x=0.1, y=capacity_kt_val_pl, label=" OGIP Capacity", hjust=0, vjust=-0.5, color="#A52A2A", size=3.5, family="sans", fontface="italic") # English label
      }
      return(gg_placeholder)
    }
    
    # --- Proceed with plotting if data exists ---
    plot_data <- fc_plot_data_list$cumulative_per_plant
    capacity_kt_val <- a_results$capacity_info$capacity_kt # OGIP Capacity
    forecast_horizon <- fc_plot_data_list$forecast_horizon
    req(nrow(plot_data) > 0, !is.na(capacity_kt_val)) # Need plot data and capacity
    
    # --- Dynamic Y-axis Limit ---
    max_hist_cumul_plant_kt <- plot_data %>%
      filter(Type=="Historical", is.finite(Cumulative_Emissions_kt)) %>%
      pull(Cumulative_Emissions_kt) %>% max(0, na.rm=TRUE)
    
    max_forecast_val <- plot_data %>%
      filter(is.finite(Cumulative_Emissions_kt)) %>%
      pull(Cumulative_Emissions_kt) %>% max(0, na.rm=TRUE)
    
    max_y_consider <- max(max_forecast_val, capacity_kt_val, na.rm = TRUE) * 1.1
    y_max_limit_upscaled <- pmin(max_y_consider, capacity_kt_val * 1.5) # Cap zoom relative to capacity
    y_max_limit_upscaled <- max(y_max_limit_upscaled, max_hist_cumul_plant_kt * 1.2, 10, na.rm=TRUE) # Ensure minimum height
    
    # --- Plotting --- (Titles/Labels English)
    ggplot(plot_data, aes(x = Year, y = Cumulative_Emissions_kt, color = Plant_Display, linetype = Line_Type, group = interaction(Plant_Identifier, Type))) +
      geom_line(linewidth = 1.1) +
      geom_hline(yintercept = capacity_kt_val, linetype = "dotted", color = "#A52A2A", linewidth = 1) +
      annotate("text", x = min(plot_data$Year, na.rm=TRUE), y = capacity_kt_val, label = " OGIP Capacity", hjust = 0, vjust = -0.5, color = "#A52A2A", size = 3.5, family = "sans", fontface = "italic") + # English Label
      scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed"), name = "Period") + # English Legend Title
      scale_color_manual(values = plant_palette_display(), name = "Plant") + # English Legend Title
      coord_cartesian(ylim = c(0, y_max_limit_upscaled), expand = FALSE, clip = "on") +
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=6)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
      labs(
        title = glue("Plant Cumulative Emissions & Forecast ({forecast_horizon} Yrs) vs. Capacity"), # English Title
        subtitle = glue("Calculated cumulative per plant (kt CO2e). Capacity based on OGIP. Forecast uses {N_YEARS_FOR_LM_FIT}yr trend."), # English Subtitle
        x = "Year", # English Label
        y = "Cumulative Stored CO2e per Plant (kt CO2e)" # English Label
      ) +
      theme_light_rich() +
      theme(legend.position = "bottom")
    
  }, bg="transparent", res=96)
  
  # Text explanation for the cumulative forecast plot (English)
  output$plantCumulativeForecastVsCapacityAnalysis <- renderText({
    req(analysis_results()) # Require analysis to be run
    if (is.null(rv$forecast_plot_data)) {
      "Click 'Calculate Forecast' to generate forecast data and plot." # English Text
    } else {
      "Plot shows calculated historical and forecasted cumulative emissions (kt) per plant versus the estimated total storage capacity (kt from OGIP)." # English Text
    }
  })
  
  output$rateVsFillPlot <- renderPlot({
    # --- Task 2: Depends on rv$forecast_plot_data ---
    a_results <- analysis_results() # Needed for context/titles maybe, but plot data is from forecast
    fc_plot_data_list <- rv$forecast_plot_data
    req(a_results)
    
    # Show placeholder if forecast not calculated yet (Message English)
    if (is.null(fc_plot_data_list)) {
      return(ggplot() + theme_void() +
               geom_text(aes(x=0.5, y=0.5), label="Click 'Calculate Forecast' to generate plot.", color="grey50", size=5) + # English Placeholder Text
               labs(title = "Annual Injection Rate vs. Fill Level (Forecasted)") ) # English Title
    }
    
    # --- Proceed with plotting if data exists ---
    plot_data <- fc_plot_data_list$rate_vs_fill
    forecast_horizon <- fc_plot_data_list$forecast_horizon
    req(nrow(plot_data) > 0)
    
    # --- Secondary Axis Scaling ---
    max_rate_kt <- max(plot_data$Total_Annual_Emissions_kt[is.finite(plot_data$Total_Annual_Emissions_kt)], 0, na.rm = TRUE)
    max_fill <- 1.0
    scaling_factor_ratefill <- if (max_rate_kt > 1e-9) max_fill / max_rate_kt else 1
    if (!is.finite(scaling_factor_ratefill) || scaling_factor_ratefill <= 0) scaling_factor_ratefill <- 1
    
    # --- Plotting --- (Titles/Labels English)
    ggplot(plot_data, aes(x = Year)) +
      geom_line(aes(y = Fill_Level, linetype = Line_Type, color = "Fill Level"), linewidth = 1.1) +
      geom_line(aes(y = Total_Annual_Emissions_kt * scaling_factor_ratefill, linetype = Line_Type, color = "Annual Rate"), linewidth = 1.0) +
      scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed"), name = "Period") + # English Legend Title
      scale_color_manual(name = "Metric", values = c("Fill Level" = "#0056b3", "Annual Rate" = "#ff7f0e")) + # English Legend Title
      scale_y_continuous(
        name = "Reservoir Fill Level (Fraction of OGIP Capacity)", # English Axis Label
        labels = scales::percent_format(accuracy = 1),
        limits=c(0, max(1.05, max(plot_data$Fill_Level, na.rm=TRUE)*1.05)),
        sec.axis = sec_axis( ~ . / scaling_factor_ratefill,
                             name = "Total Annual Emissions Rate (kt CO2e / Yr)", # English Axis Label
                             labels = scales::comma)
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
      labs(
        title = glue("Annual Emissions Rate vs. Fill Level ({forecast_horizon} Yr Forecast)"), # English Title
        subtitle = glue("Rate = Calculated total annual (kt/yr). Fill = Calculated total cumulative / OGIP Capacity."), # English Subtitle
        x = "Year" # English Label
      ) +
      theme_light_rich() +
      theme(legend.position = "bottom")
    
  }, bg="transparent", res=96)
  
  # Text explanation for the rate vs fill plot (English)
  output$rateVsFillAnalysis <- renderText({
    req(analysis_results()) # Require analysis to be run
    if (is.null(rv$forecast_plot_data)) {
      "Click 'Calculate Forecast' to generate forecast data and plot." # English Text
    } else {
      "Compares the calculated total annual emissions rate (kt/yr) with the calculated reservoir fill level (fraction of OGIP capacity), including linear forecasts." # English Text
    }
  })
  
  # Summary text based on analysis results (English)
  output$forecastSummaryText <- renderUI({
    results <- analysis_results()
    req(results)
    
    capacity_kt_val <- results$capacity_info$capacity_kt
    cumulative_data_calculated <- results$emission_info$calculated_total_cumulative
    injection_rate_scenario_kt_val <- results$injection_rate_scenario_kt
    fill_details <- results$fill_details
    
    # Validate required inputs (Messages English)
    if (is.null(capacity_kt_val) || is.na(capacity_kt_val) || !is.numeric(capacity_kt_val) || capacity_kt_val <= 0) { return(HTML("<p style='color:red;'>Invalid OGIP capacity.</p>")) } # English Error Text
    if (is.null(cumulative_data_calculated) || nrow(cumulative_data_calculated) == 0) { return(HTML("<p style='color:red;'>Insufficient historical data.</p>")) } # English Error Text
    
    # Get last historical state
    last_cumulative_row <- cumulative_data_calculated %>% filter(is.finite(Year) & is.finite(Cumulative_Emissions_kt)) %>% arrange(desc(Year)) %>% head(1)
    last_cumulative_kt_val <- if(nrow(last_cumulative_row)>0) last_cumulative_row$Cumulative_Emissions_kt else 0
    last_hist_year <- if(nrow(last_cumulative_row)>0) last_cumulative_row$Year else NA
    if(is.na(last_hist_year)) return(HTML("<p style='color:red;'>Cannot determine last historical year.</p>")) # English Error Text
    
    current_fill_percent <- if(capacity_kt_val > 0) (last_cumulative_kt_val / capacity_kt_val) * 100 else 0
    remaining_capacity_kt_val <- max(0, capacity_kt_val - last_cumulative_kt_val)
    
    # --- Format fill time details --- (Text English)
    estimated_fill_year_str <- "N/A"; years_to_fill_str <- "N/A" # English Defaults
    
    if (!is.na(fill_details$fill_year_injection)) {
      estimated_fill_year_str <- as.character(round(fill_details$fill_year_injection))
      if (!is.na(fill_details$years_to_fill_injection)) {
        if (fill_details$years_to_fill_injection <= 0) {
          if(current_fill_percent >= 100){
            years_to_fill_str <- "Already Full" # English Text
            first_full_year_row <- cumulative_data_calculated %>% filter(Cumulative_Emissions_kt >= capacity_kt_val) %>% arrange(Year) %>% head(1)
            estimated_fill_year_str <- if(nrow(first_full_year_row)>0) as.character(round(first_full_year_row$Year)) else "Already Full" # English Text
          } else {
            years_to_fill_str <- "Indeterminate (Check Rate)" # English Text
            estimated_fill_year_str <- "Indeterminate" # English Text
          }
        } else {
          years_to_fill_str <- format_number(fill_details$years_to_fill_injection, 1)
        }
      }
    } else {
      if (current_fill_percent >= 100) {
        years_to_fill_str <- "Already Full" # English Text
        first_full_year_row <- cumulative_data_calculated %>% filter(Cumulative_Emissions_kt >= capacity_kt_val) %>% arrange(Year) %>% head(1)
        estimated_fill_year_str <- if(nrow(first_full_year_row)>0) as.character(round(first_full_year_row$Year)) else "Already Full" # English Text
      } else if (!is.na(injection_rate_scenario_kt_val) && injection_rate_scenario_kt_val <= 0) {
        years_to_fill_str <- "Rate Scenario <= 0" # English Text
        estimated_fill_year_str = "Never (Rate <= 0)" # English Text
      } else {
        years_to_fill_str <- "Indeterminate" # English Text
        estimated_fill_year_str = "Indeterminate" # English Text
      }
    }
    
    # --- Determine Status Color and Text Explanations (Based on Scenario Rate) --- (Text English)
    status_color_css <- if (current_fill_percent >= 100) "#dc3545" else if (current_fill_percent >= 80) "#ffc107" else "#28a745"
    explanation_current <- if(current_fill_percent >= 100) {
      "Est. <strong>full</strong> (based on calculated total vs OGIP Capacity)." # English Text
    } else {
      glue("Est. <strong>{round(current_fill_percent, 1)}%</strong> full. Leaves <strong>{format_number(remaining_capacity_kt_val, 0)}</strong> kt capacity.") # English Text
    }
    scenario_rate_percent_display <- round(SCENARIO_RATE_FACTOR*100, 0)
    scenario_rate_value_display <- if(!is.na(injection_rate_scenario_kt_val)) format_number(injection_rate_scenario_kt_val, 1) else "N/A" # English Default
    
    # --- Final HTML Output (Focus on Scenario Rate Fill) --- (Text English)
    HTML(glue(
      "<div style='line-height: 1.6; font-size: 0.9em;'>",
      "<strong>Current Status ({last_hist_year}):</strong> <span style='color:{status_color_css};'>{explanation_current}</span><br>", # English Label
      "<strong>Fill Time ({scenario_rate_percent_display}% Scenario Rate = {scenario_rate_value_display} kt/yr):</strong>", # English Label
      " <span style='margin-left: 5px;'>Years Left:</span> <strong style='color: #003366;'>{years_to_fill_str}</strong> |", # English Label
      " <span style='margin-left: 5px;'>Est. Fill Year:</span> <strong style='color: #003366;'>{estimated_fill_year_str}</strong>", # English Label
      # Removed forecast status based on scenario rate for less confusion with button forecast
      "</div>" ))
  })
  
  
  # --- ============================================= ---
  # --- Implementation of Plant Details Tab Outputs   ---
  # --- (Using analysis_results)                      ---
  # --- ============================================= ---
  
  # Annual emissions per selected plant (Titles/Labels English)
  output$annualPerPlantPlot <- renderPlot({
    results <- analysis_results()
    selected_plants_internal <- input$annual_plant_select
    req(results, results$emission_info$annual_emissions_per_plant,
        !is.null(selected_plants_internal), length(selected_plants_internal) > 0)
    
    plot_data <- results$emission_info$annual_emissions_per_plant %>%
      filter(Plant_Identifier %in% selected_plants_internal)
    req(nrow(plot_data) > 0)
    
    plot_data_display <- plot_data %>%
      left_join(tibble(Plant_Identifier = unname(PLANT_CHOICES_DISPLAY), Plant_Display = names(PLANT_CHOICES_DISPLAY)), by = "Plant_Identifier") %>%
      filter(!is.na(Plant_Display))
    
    ggplot(plot_data_display, aes(x = Year, y = Total_Annual_Emissions_kt, color = Plant_Display, group = Plant_Identifier)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5, alpha = 0.7) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = plant_palette_display(), name = "Plant") + # English Legend Title
      labs(
        title = "Annual Emissions per Selected Plant", # English Title
        subtitle = "Calculated from processed data (in kt CO2e)", # English Subtitle
        x = "Year", # English Label
        y = "Annual Emissions (kt CO2e)" # English Label
      ) +
      theme_light_rich() +
      scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
      theme(legend.position="bottom")
    
  }, bg = "transparent", res=96)
  
  # Render total cumulative labels per plant (English)
  output$plantTotalLabels <- renderUI({
    results <- analysis_results()
    selected_plants_internal <- input$annual_plant_select
    req(results, results$emission_info$cumulative_per_plant,
        !is.null(selected_plants_internal), length(selected_plants_internal) > 0)
    
    cumulative_data <- results$emission_info$cumulative_per_plant
    req(nrow(cumulative_data) > 0)
    
    latest_year_in_data <- max(cumulative_data$Year, na.rm = TRUE)
    req(is.finite(latest_year_in_data))
    
    latest_cumulative_selected <- cumulative_data %>%
      filter(Year == latest_year_in_data, Plant_Identifier %in% selected_plants_internal) %>%
      right_join(tibble(Plant_Identifier = selected_plants_internal), by = "Plant_Identifier") %>%
      mutate(Cumulative_Emissions_kt = ifelse(is.na(Cumulative_Emissions_kt), 0, Cumulative_Emissions_kt)) %>%
      left_join(tibble(Plant_Identifier = unname(PLANT_CHOICES_DISPLAY), Plant_Display = names(PLANT_CHOICES_DISPLAY)), by = "Plant_Identifier") %>%
      select(Plant_Display, Cumulative_Emissions_kt) %>%
      filter(!is.na(Plant_Display))
    
    tagList(
      lapply(1:nrow(latest_cumulative_selected), function(i) {
        plant_name <- latest_cumulative_selected$Plant_Display[i]
        total_val_kt <- latest_cumulative_selected$Cumulative_Emissions_kt[i]
        tags$p(class = "plant-total-label",
               tags$strong(paste0(plant_name, ":")),
               paste0(" ", format_number(total_val_kt, 0), " kt CO2e")) # English Unit Label
      })
    )
  })
  
  # Plot cumulative vs calculated annual rate (Titles/Labels English)
  output$plantCumulativeVsCalcAnnualRatePlot <- renderPlot({
    results <- analysis_results()
    req(results, results$emission_info$cumulative_per_plant)
    cumulative_per_plant <- results$emission_info$cumulative_per_plant
    req(nrow(cumulative_per_plant) > 0)
    
    plot_data_calc_rate <- cumulative_per_plant %>%
      arrange(Plant_Identifier, Year) %>%
      group_by(Plant_Identifier) %>%
      mutate(
        Calculated_Annual_Rate_kt = Cumulative_Emissions_kt - lag(Cumulative_Emissions_kt, default = 0)
      ) %>%
      ungroup() %>%
      left_join(tibble(Plant_Identifier = unname(PLANT_CHOICES_DISPLAY), Plant_Display = names(PLANT_CHOICES_DISPLAY)), by = "Plant_Identifier") %>%
      filter(!is.na(Plant_Display))
    
    plot_data_filtered <- plot_data_calc_rate %>%
      filter(
        is.finite(Cumulative_Emissions_kt),
        is.finite(Calculated_Annual_Rate_kt),
        Calculated_Annual_Rate_kt >= 0, # Often makes sense to filter out negative rates for viz
        Cumulative_Emissions_kt >= 0
      )
    req(nrow(plot_data_filtered) > 0)
    
    max_y1_kt <- max(plot_data_filtered$Cumulative_Emissions_kt, 0, na.rm = TRUE)
    max_y2_kt <- max(plot_data_filtered$Calculated_Annual_Rate_kt, 0, na.rm = TRUE)
    scaling_factor <- if (max_y2_kt > 1e-9) { max_y1_kt / max_y2_kt } else { 1 }
    if (!is.finite(scaling_factor) || scaling_factor <= 0) scaling_factor <- 1
    
    plot_data_final <- plot_data_filtered %>%
      mutate( Scaled_Annual_Rate = Calculated_Annual_Rate_kt * scaling_factor ) %>%
      filter(is.finite(Scaled_Annual_Rate))
    req(nrow(plot_data_final) > 0)
    
    ggplot(plot_data_final, aes(x = Year, group = Plant_Identifier, color = Plant_Display)) +
      geom_line(aes(y = Cumulative_Emissions_kt), linetype = "solid", linewidth = 1.1) +
      geom_line(aes(y = Scaled_Annual_Rate), linetype = "dashed", linewidth = 1.0) +
      scale_y_continuous( name = "Calculated Cumulative Emissions (kt CO2e)", labels = scales::comma, # English Axis Label
                          sec.axis = sec_axis( ~ . / scaling_factor, name = "Calculated Annual Rate (kt CO2e / Yr)", labels = scales::comma) # English Axis Label
      ) +
      scale_color_manual(values = plant_palette_display(), name = "Plant") + # English Legend Title
      labs( title = "Plant Cumulative vs. Calculated Annual Rate", # English Title
            subtitle = "Cumulative (solid, left axis) vs. Annual rate derived from cumulative difference (dashed, right axis)", # English Subtitle
            x = "Year", # English Label
            caption = "Note: Dashed line represents the year-on-year change in the solid line." ) + # English Caption
      theme_light_rich() + theme(legend.position = "bottom") +
      scale_x_continuous(breaks = scales::pretty_breaks(n=10))
    
  }, bg = "transparent", res=96)
  
  # Text note for cumulative vs rate plot (English)
  output$plantCumulativeVsCalcAnnualRateNote <- renderText({
    "Solid line: Cumulative emissions (kt) calculated by summing processed annual plant data. Dashed line: Annual rate (kt/yr) calculated from the year-on-year difference in the cumulative value." # English Text
  })
  
  
  # --- ============================================= ---
  # --- Implementation of Reservoir Simulation Tab    ---
  # --- (Using analysis_results & dome_plot_data)     ---
  # --- ============================================= ---
  
  # --- Reservoir Year Selection UI --- (Label English)
  output$reservoirYearSelectionUI <- renderUI({
    req(rv$classification_complete, rv$classification_status == "SUITABLE", rv$app_state == "running")
    processed_data <- rv$processed_emission_data
    req(processed_data, processed_data$total_annual_emissions) # Need data
    
    years <- tryCatch(sort(unique(processed_data$total_annual_emissions$Year)), error=function(e) NULL)
    years <- years[!is.na(years) & is.finite(years)]
    req(!is.null(years) && length(years) > 0)
    
    char_years <- as.character(years)
    choices <- c("All Years", char_years) # "All Years" for total cumulative (English)
    
    current_selection <- isolate(input$anim_year_select)
    latest_year_char <- as.character(max(years, na.rm = TRUE))
    default_selection <- latest_year_char
    selected_val <- default_selection
    if (!is.null(current_selection) && (current_selection %in% choices)) {
      selected_val <- current_selection
    }
    
    selectInput("anim_year_select", "Select Period:", # English Label
                choices = choices,
                selected = selected_val,
                width="200px")
  })
  
  # --- dome_plot_data reactive (Triggered by visualize_dome button) --- (Warnings/Labels English)
  dome_plot_data <- eventReactive(input$visualize_dome, {
    results <- analysis_results()
    req(results)
    selected_period <- input$anim_year_select # Get selection from dropdown
    req(results$capacity_info, results$emission_info, selected_period)
    
    capacity_kt_val <- results$capacity_info$capacity_kt # OGIP Capacity
    total_cumulative_data <- results$emission_info$calculated_total_cumulative
    
    req(!is.na(capacity_kt_val), capacity_kt_val > 0,
        !is.null(total_cumulative_data), nrow(total_cumulative_data) > 0)
    
    target_emissions_kt_val <- 0.0 # Initialize target emissions (always cumulative)
    selected_year_num <- NA
    period_label <- ""
    years_for_pie <- integer(0) # Years to include in the pie chart
    
    if (selected_period == "All Years") {
      # --- CUMULATIVE logic for "All Years" ---
      last_cumulative_row <- total_cumulative_data %>%
        filter(is.finite(Year), is.finite(Cumulative_Emissions_kt)) %>%
        arrange(Year) %>%
        tail(1)
      
      if (nrow(last_cumulative_row) > 0) {
        target_emissions_kt_val <- last_cumulative_row$Cumulative_Emissions_kt
        selected_year_num <- last_cumulative_row$Year # Store the latest year
        period_label <- glue("Total Cumulative up to {selected_year_num}") # English label
        # Include all years in the pie chart where annual data exists
        if(!is.null(results$emission_info$total_annual_emissions)) {
          years_for_pie <- results$emission_info$total_annual_emissions %>%
            filter(is.finite(Year), Total_Annual_Emissions_kt > 0) %>%
            pull(Year)
        } else {
          years_for_pie <- total_cumulative_data$Year[is.finite(total_cumulative_data$Year)] # Fallback
        }
      } else {
        warning("Could not retrieve latest cumulative data for 'All Years'.") # English warning
        period_label <- "All Years (Error: No data)" # English label
        target_emissions_kt_val <- NA # Indicate error
      }
    } else {
      # --- CUMULATIVE logic for a specific year ---
      selected_year_num <- suppressWarnings(as.numeric(selected_period))
      if (!is.na(selected_year_num) && is.finite(selected_year_num)) {
        # Get the cumulative value for the specific year
        cumulative_data_for_year <- total_cumulative_data %>%
          filter(Year == selected_year_num, is.finite(Cumulative_Emissions_kt))
        
        if (nrow(cumulative_data_for_year) > 0) {
          target_emissions_kt_val <- cumulative_data_for_year$Cumulative_Emissions_kt[1]
          period_label <- glue("Cumulative up to {selected_year_num}") # English label
          # Include years from start up to selected year in pie chart
          start_year <- min(total_cumulative_data$Year, na.rm=TRUE)
          if(is.finite(start_year)) {
            if(!is.null(results$emission_info$total_annual_emissions)) {
              years_for_pie <- results$emission_info$total_annual_emissions %>%
                filter(Year >= start_year, Year <= selected_year_num, is.finite(Year), Total_Annual_Emissions_kt > 0) %>%
                pull(Year)
            } else {
              years_for_pie <- seq(from=start_year, to=selected_year_num) # Fallback
            }
          } else {
            years_for_pie <- selected_year_num # Fallback if start year cannot be found
          }
          
        } else {
          # Handle case if selected year is not in cumulative data (Labels English)
          first_data_year <- min(total_cumulative_data$Year, na.rm = TRUE)
          if (is.finite(first_data_year) && selected_year_num < first_data_year) {
            target_emissions_kt_val <- 0 # Assume 0 cumulative before data starts
            period_label <- glue("Cumulative up to {selected_year_num} (Before Data Start)") # English Label
            years_for_pie <- integer(0) # No data for pie chart
          } else {
            period_label <- glue("Cumulative up to {selected_year_num} (Data Missing)") # English Label
            target_emissions_kt_val <- NA # Indicate data missing for year
          }
        }
      } else {
        warning(paste("Invalid year selected:", selected_period)) # English warning
        period_label <- "Invalid Year Selection" # English label
        target_emissions_kt_val <- NA # Indicate invalid selection
      }
    }
    
    # Calculate fill level
    fill_level_fraction <- NA # Default to NA
    if (!is.na(target_emissions_kt_val) && !is.na(capacity_kt_val) && capacity_kt_val > 0) {
      fill_level_fraction <- target_emissions_kt_val / capacity_kt_val
    } else {
      warning(glue("Could not calculate fill fraction. Target Emissions: {target_emissions_kt_val}, Capacity: {capacity_kt_val}")) # English warning
    }
    # Clamp between 0 and 1, handling NA
    fill_level_fraction <- pmin(1.0, pmax(0.0, fill_level_fraction), na.rm = TRUE)
    # Ensure fill_level_fraction is not NA if calculation was possible
    if (is.na(fill_level_fraction) && !is.na(target_emissions_kt_val) && !is.na(capacity_kt_val)) {
      fill_level_fraction <- 0.0
    }
    
    
    # Data for Pie Chart - Get annual totals for the relevant years
    annual_data_for_pie <- NULL
    if(length(years_for_pie) > 0 && !is.null(results$emission_info$total_annual_emissions)) {
      annual_data_for_pie <- results$emission_info$total_annual_emissions %>%
        filter(Year %in% years_for_pie, Total_Annual_Emissions_kt > 0) %>%
        select(Year, Total_Annual_Emissions_kt)
    }
    
    return(list(
      capacity_kt = capacity_kt_val,
      target_emissions_kt = target_emissions_kt_val, # Cumulative value
      fill_level_fraction = fill_level_fraction,
      period_label = period_label,
      selected_period = selected_period,
      annual_data_for_pie = annual_data_for_pie # Data for the pie chart
    ))
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # ignoreInit=TRUE
  
  
  # --- Dome Plot Output --- (Titles/Labels English)
  output$domeFillPlot <- renderPlot({
    plot_data_list <- dome_plot_data() # Get data (always cumulative)
    req(plot_data_list)
    
    if (is.null(plot_data_list) || is.na(plot_data_list$fill_level_fraction) || is.na(plot_data_list$target_emissions_kt)) {
      error_message <- "Select period and click 'Visualize Fill Level'." # English message
      if(!is.null(plot_data_list)) { error_message <- paste("Cannot visualize:", plot_data_list$period_label) } # English message
      ggplot() + theme_void() +
        geom_text(aes(x=0.5, y=0.5), label=error_message, color="grey60", size=5, family="sans") +
        theme(plot.background = element_rect(fill="#f8f9fa", color=NA))
    } else {
      capacity_kt_val <- plot_data_list$capacity_kt
      target_emissions_kt_val <- plot_data_list$target_emissions_kt
      fill_level_fraction <- plot_data_list$fill_level_fraction
      period_label <- plot_data_list$period_label
      selected_period <- plot_data_list$selected_period
      
      # --- Geometry calculations ---
      dome_base_y <- 0.1; dome_max_y <- 0.8; dome_height <- dome_max_y - dome_base_y
      dome_center_x <- 0.5; dome_width_factor <- 0.4
      x_curve <- seq(dome_center_x - dome_width_factor, dome_center_x + dome_width_factor, length.out = 100)
      y_curve <- dome_base_y + dome_height * (1 + cos(pi + (x_curve - dome_center_x) * pi / dome_width_factor)) / 2
      y_curve[x_curve < dome_center_x - dome_width_factor | x_curve > dome_center_x + dome_width_factor] <- dome_base_y
      dome_poly <- data.frame(
        x = c(dome_center_x - dome_width_factor, x_curve, dome_center_x + dome_width_factor),
        y = c(dome_base_y, y_curve, dome_base_y)
      )
      
      fill_height <- dome_base_y + fill_level_fraction * dome_height
      fill_poly_final <- data.frame(x=numeric(), y=numeric()) # Init empty
      
      if (fill_level_fraction > 1e-6 && fill_level_fraction < 1) {
        target_cos_arg <- (fill_height - dome_base_y) * 2 / dome_height - 1
        target_cos_arg <- max(-1, min(1, target_cos_arg))
        # Check for valid acos input
        if(is.finite(target_cos_arg) && target_cos_arg >= -1 && target_cos_arg <= 1){
          angle <- acos(target_cos_arg)
          x_offset <- (angle / pi) * dome_width_factor
          x_fill_left <- dome_center_x - x_offset
          x_fill_right <- dome_center_x + x_offset
          
          fill_curve_indices <- which(dome_poly$x >= x_fill_left & dome_poly$x <= x_fill_right & dome_poly$y <= fill_height & dome_poly$y >= dome_base_y)
          
          if(length(fill_curve_indices) > 1){
            fill_curve_indices_sorted <- fill_curve_indices[order(dome_poly$x[fill_curve_indices])]
            fill_poly_final <- data.frame(
              x = c(dome_poly$x[fill_curve_indices_sorted], x_fill_right, x_fill_left),
              y = c(dome_poly$y[fill_curve_indices_sorted], fill_height, fill_height)
            )
          } else {
            fill_poly_final <- data.frame(x=c(x_fill_left, x_fill_right, x_fill_right, x_fill_left), y=c(dome_base_y, dome_base_y, fill_height, fill_height))
          }
        } else {
          warning("Invalid argument for acos in dome plot fill calculation.") # English Warning
          # Leave fill_poly_final empty if calculation fails
        }
      } else if (fill_level_fraction >= 1) {
        fill_poly_final <- dome_poly
      }
      
      # --- Colors and Labels --- (Labels English)
      dome_color <- "#e9ecef"; fill_color <- "#6baed6"; bg_color <- "#FFFFFF"
      title_color <- "#003366"; text_color <- "#212529"; label_bg_color <- "#FFFFFFE0"
      
      amount_type_label <- if (selected_period == "All Years") "Total Cumulative" else "Cumulative" # English Labels
      fill_label <- glue("{amount_type_label}:\n{format_number(target_emissions_kt_val, 0)} kt\n({round(fill_level_fraction*100)}% Full)") # English Label Text
      
      # --- Plotting --- (Titles/Labels English)
      gg <- ggplot() +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = bg_color, color = NA),
          panel.background = element_rect(fill = bg_color, color = NA),
          plot.title = element_text(hjust = 0.5, size=16, color=title_color, face="bold", margin=margin(b=5)),
          plot.subtitle = element_text(hjust = 0.5, size=11, color="#6c757d", margin=margin(b=15))
        ) +
        geom_polygon(data = dome_poly, aes(x = x, y = y), fill = dome_color, color = "#adb5bd", linewidth = 0.5) +
        { if(nrow(fill_poly_final) > 0)
          geom_polygon(data = fill_poly_final, aes(x = x, y = y), fill = fill_color, alpha = 0.7)
        } +
        geom_path(data = dome_poly, aes(x = x, y = y), color = "#adb5bd", linewidth = 1) +
        annotate("text", x = dome_center_x, y = dome_max_y + 0.08, hjust = 0.5, vjust=0, size = 4, family="sans", color=title_color, fontface="bold",
                 label = glue("OGIP Capacity:\n{format_number(capacity_kt_val, 0)} kt")) + # English Label Text
        { if(fill_level_fraction > 0.02)
          annotate("label", x = dome_center_x, y = dome_base_y + (dome_height * max(0.1, fill_level_fraction * 0.5)),
                   label = fill_label,
                   hjust = 0.5, size=4, family="sans", color=title_color, fontface="bold", fill=label_bg_color,
                   label.padding = unit(0.2, "lines"), label.r = unit(0.1, "lines"))
        } +
        labs(
          title = "Reservoir Storage Visualization", # English Title
          subtitle = glue("Showing Fill Level for: {period_label}") # English Subtitle
        )
      return(gg)
    }
  }, bg = "transparent", res=96)
  
  
  # --- Pie Chart Output --- (Titles/Labels English)
  output$fillPieChart <- renderPlotly({
    plot_data_list <- dome_plot_data() # Get data from the dome plot reactive
    req(plot_data_list) # Need data
    
    pie_data <- plot_data_list$annual_data_for_pie # DataFrame of Year and Total_Annual_Emissions_kt
    
    if (is.null(pie_data) || nrow(pie_data) == 0) {
      return(
        plot_ly() %>%
          layout(title = list(text="Annual Contribution Proportion", font=list(size=12)), # English title
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, visible=FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, visible=FALSE),
                 annotations = list(x=0.5, y=0.5, text="No annual data to display for this selection.", showarrow=FALSE, xref="paper", yref="paper") # English text
          )
      )
    }
    
    pie_data <- pie_data %>% arrange(Year) # Arrange by year
    max_slices_direct_label = 15
    show_legend_pie = TRUE
    
    fig <- plot_ly(pie_data, labels = ~as.character(Year), values = ~Total_Annual_Emissions_kt, type = 'pie',
                   textinfo = if(nrow(pie_data) <= max_slices_direct_label) 'percent' else 'none',
                   insidetextorientation = 'radial',
                   hoverinfo = 'text',
                   text = ~paste(Year, '<br>', format_number(Total_Annual_Emissions_kt, 1), ' kt CO2e'), # Hover text English
                   marker = list(colors = scales::viridis_pal()(nrow(pie_data)),
                                 line = list(color = '#FFFFFF', width = 1)),
                   sort = FALSE,
                   rotation = 90
    )
    
    fig <- fig %>% layout(title = list(text = glue("Annual Contribution ({plot_data_list$period_label})"), font=list(size=12, color="#003366")), # Title English
                          showlegend = show_legend_pie,
                          legend = list(orientation = "v",
                                        bgcolor = 'rgba(255,255,255,0.7)',
                                        font = list(size = 9)),
                          margin = list(l = 10, r = 10, b = 20, t = 50),
                          paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(fig)
  })
  
  
  # --- 3D Model Plot Output --- (Titles/Labels English)
  output$reservoir3DModel <- renderPlotly({
    results <- analysis_results()
    res_data_for_3d <- dome_plot_data() # Use data from dome plot for fill level
    req(results, results$ogip_params, results$capacity_info, res_data_for_3d)
    req(!is.null(res_data_for_3d$fill_level_fraction), is.finite(res_data_for_3d$fill_level_fraction))
    
    # --- Get relevant values ---
    thickness_ft <- results$ogip_params$ogip_thickness_ft
    area_acres <- results$ogip_params$ogip_area_acres
    fill_level_fraction <- res_data_for_3d$fill_level_fraction
    period_label_for_3d <- res_data_for_3d$period_label
    selected_period_for_3d <- res_data_for_3d$selected_period
    
    # --- Geometry Calculations ---
    area_sq_km <- area_acres * ACRES_TO_M2 / 1e6
    approx_radius_km <- tryCatch(sqrt(area_sq_km / pi), error = function(e) 1.0)
    approx_radius_km <- max(0.1, approx_radius_km)
    thickness_m <- thickness_ft * FT_TO_M
    reservoir_thickness_viz <- max(50, thickness_m * 1.5) # Visual thickness exaggeration
    
    grid_res <- 30
    x_range <- y_range <- seq(-1.5*approx_radius_km, 1.5*approx_radius_km, length.out = grid_res)
    depth_center <- 2500 # Assumed depth center (m)
    
    base_surface_func <- function(x, y) {
      depth_center - 50 * exp(-(x^2 + y^2)/(2*(approx_radius_km*0.8)^2))
    }
    
    num_layers <- 5;
    layer_thickness_factor <- max(100, reservoir_thickness_viz * 1.2)
    layer_palette <- RColorBrewer::brewer.pal(max(3,num_layers), "Greys")
    reservoir_layer_index <- 3; reservoir_color <- "#A0522D" # Brown for reservoir
    fill_color_3d <- "#6baed6" # Light blue for fill
    
    max_dome_height <- layer_thickness_factor * 0.3 # Visual bulging effect
    current_dome_height <- max_dome_height * fill_level_fraction
    dome_radius_factor <- 1.2
    
    fig <- plot_ly()
    z_grid_list <- list() # Store Z grids
    
    # --- Create geological layers ---
    for (i in 1:num_layers) {
      z_grid_base <- tryCatch(outer(x_range, y_range, base_surface_func) + ((num_layers - i) * layer_thickness_factor), error = function(e){ NULL })
      if (is.null(z_grid_base)) next
      
      mesh_opacity <- 0.8
      mesh_color <- layer_palette[min(i, length(layer_palette))]
      current_z_grid <- z_grid_base
      
      # Apply visual dome effect based on fill
      if (i >= reservoir_layer_index && current_dome_height > 0) {
        dome_effect_matrix <- outer(x_range, y_range, function(xi, yi) {
          current_dome_height * exp(-(xi^2 + yi^2)/(2*(approx_radius_km/dome_radius_factor)^2))
        })
        current_z_grid <- z_grid_base - dome_effect_matrix # Subtract effect from depth (shallower)
      }
      
      if (i == reservoir_layer_index) {
        mesh_opacity <- 0.9
        mesh_color <- reservoir_color
      }
      
      z_grid_list[[i]] <- current_z_grid
      
      fig <- fig %>% add_trace( x = ~x_range, y = ~y_range, z = ~current_z_grid, type = 'mesh3d',
                                opacity = mesh_opacity, color = I(mesh_color),
                                showscale = FALSE, name = paste("Layer", i, if(i==reservoir_layer_index) "(Res.)" else "") ) # Name English
    }
    
    # --- Add fill level indicator within the reservoir layer ---
    if (fill_level_fraction > 0.01 && reservoir_layer_index <= num_layers && !is.null(z_grid_list[[reservoir_layer_index]])) {
      reservoir_z_top <- z_grid_list[[reservoir_layer_index]] # Top surface Z of reservoir layer
      
      # Calculate Z level for the bottom of the conceptual fill volume
      # visual_fill_thickness = min(reservoir_thickness_viz, fill_level_fraction * reservoir_thickness_viz / max(fill_level_fraction, 0.01))
      # z_fill_bottom <- reservoir_z_top + visual_fill_thickness # Z increases with depth
      
      # Define the area for the fill
      fill_radius_km <- approx_radius_km * 0.8
      
      # Create a surface representing the *top* of the filled portion
      fill_surface_z <- reservoir_z_top
      outside_fill_mask <- outer(x_range, y_range, function(x,y) x^2 + y^2) > fill_radius_km^2
      fill_surface_z[outside_fill_mask] <- NA
      
      # Plot the fill surface slightly *above* the actual top surface (less depth) for visibility
      fig <- fig %>% add_trace(
        x = ~x_range, y = ~y_range, z = ~(fill_surface_z - 1), # Offset slightly shallower
        type = 'surface',
        opacity = 0.7,
        colorscale = list(c(0, 1), c(fill_color_3d, fill_color_3d)), # Single color
        showscale = FALSE,
        name = "CO2 Plume (Conceptual)", # Name English
        hoverinfo = "skip"
      )
    }
    
    # --- Layout and Final Touches --- (Titles/Labels English)
    fill_type_label_3d <- if (selected_period_for_3d == "All Years") "Cumulative" else "Cumulative" # English Label
    plot_title_3d <- glue("Conceptual 3D ({fill_type_label_3d} Fill: ~{round(fill_level_fraction*100)}% for {period_label_for_3d})") # English Title
    
    fig <- fig %>% layout(
      title = list(text=plot_title_3d,
                   font=list(family="sans", size=14, color="#003366"),
                   y=0.97, x=0.5, xanchor='center'),
      scene = list(
        xaxis = list(title = 'X (km)', zeroline=F, showgrid=T, gridcolor='rgba(220,220,220,0.5)', backgroundcolor="#ffffff"), # English Label
        yaxis = list(title = 'Y (km)', zeroline=F, showgrid=T, gridcolor='rgba(220,220,220,0.5)', backgroundcolor="#ffffff"), # English Label
        zaxis = list(title = 'Depth (m)', autorange = "reversed", zeroline=F, showgrid=T, gridcolor='rgba(220,220,220,0.5)', backgroundcolor="#ffffff"), # English Label
        bgcolor = '#ffffff',
        aspectratio = list(x=1, y=1, z=0.3),
        camera = list(eye = list(x = 1.7, y = 1.7, z = 0.7))
      ),
      paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
      legend = list(font = list(color="#6c757d"), bgcolor="rgba(255,255,255,0.7)", bordercolor="#dee2e6", borderwidth=1)
    )
    
    if (!inherits(fig, "plotly")) {
      return(plot_ly() %>% layout(title = "Error generating 3D plot")) # Error message English
    }
    return(fig)
    
  }) # End reservoir3DModel
  
  
} # End Server Logic


# --- Run the App ---
shinyApp(ui = ui, server = server)
