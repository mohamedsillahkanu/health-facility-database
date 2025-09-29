# Seasonality Analysis for SMC Implementation

## Overview

This analysis examines malaria seasonality using time-series data, structured into overlapping blocks that capture both short-term (e.g., 4-month) and annual (12-month) intervals.

Seasonal peaks are defined by a transmission intensity threshold: a peak occurs when rainfall or malaria cases within any consecutive four-month period reach or exceed a specified proportion of the total for the corresponding 12-month period. Geographic units, such as districts, are classified as having seasonal transmission if these peaks are consistently observed.

This approach identifies areas where time-limited interventions, such as Seasonal Malaria Chemoprevention (SMC), are most effective and helps guide optimal timing aligned with natural transmission cycles.

**Objectives:**
- Determine which districts demonstrate seasonal transmission patterns suitable for SMC implementation  
- Map the timing of rainfall and malaria transmission peaks to optimize intervention scheduling for maximum impact

![Profiling seasonality for SMC targeting](https://github.com/mohamedsillahkanu/snt-package/raw/7aca50c7688bbc679b33f5543150078c978c7edf/seasonality_eligibility_v2.png)

## Data Preparation and Setup

CHIRPS satellite-derived rainfall data will be used to assess malaria seasonality, with Sierra Leone as an illustrative example. For guidance on downloading rainfall datasets, refer to [CHIRPS data extraction guide](https://ahadi-analytics.github.io/snt-code-library/english/library/data/climate/extract_raster_climate.html).

### Step 1: Load Required Packages

```{r setup}
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  readxl, dplyr, openxlsx, lubridate, ggplot2, readr, stringr, 
  here, tidyr, gridExtra, knitr, writexl
)
```

### Step 2: Configure Analysis Parameters

```{r config}
# ===== USER CUSTOMIZATION SECTION =====
# Modify these parameters according to your data and analysis needs

# File paths
file_path <- here::here("english/data_r/modeled", "chirps_data_2015_2023_lastest.xls")

# Column names in your dataset
year_column <- "Year"                    # Name of the year column
month_column <- "Month"                  # Name of the month column  
value_column <- "mean_rain"              # Name of the value column to analyze
admin_columns <- c("FIRST_DNAM", "FIRST_CHIE")  # Administrative unit columns to group by

# Analysis parameters
analysis_start_year <- 2015             # Year to start analysis from
analysis_start_month <- 1               # Month to start analysis from (1-12)
seasonality_threshold <- 60             # Threshold percentage for seasonality (0-100)

# Output file names
detailed_output <- "detailed_seasonality_results_09_22_2025.xlsx"
yearly_output <- "yearly_analysis_summary.xlsx"
location_output <- "location_seasonality_summary.xlsx"
```

## Analysis Phase 1: Time Block Generation and Processing

### Step 3: Load and Validate Data

```{r load_data}
# Load your data
data <- readxl::read_excel(file_path)

# Display data structure
cat("Data dimensions:", dim(data), "\n")
cat("Column names:", names(data), "\n")
cat("Date range:", min(data[[year_column]], na.rm = TRUE), 
    "to", max(data[[year_column]], na.rm = TRUE), "\n")
```

### Step 4: Define Helper Functions

#### Step 4.1: Date Range Validation Function

```{r date_functions}
# Function to check if a given year/month falls within a specified date range
is_date_in_range <- function(year, month, start_date, end_date) {
  if (start_date$year == end_date$year) {
    return(year == start_date$year & month >= start_date$month & month <= end_date$month)
  } else {
    return((year == start_date$year & month >= start_date$month) |
           (year == end_date$year & month <= end_date$month) |
           (year > start_date$year & year < end_date$year))
  }
}
```

#### Step 4.2: Time Block Generation Function

##### Step 4.2.1: Initialize Block Storage

```{r time_blocks_init}
generate_time_blocks <- function(start_year, start_month, num_blocks) {
  blocks <- list()
  current_year <- start_year
  current_month <- start_month
```

##### Step 4.2.2: Start Loop Through Blocks

```{r time_blocks_loop}
  for (i in 1:num_blocks) {
```

##### Step 4.2.3: Calculate 4-Month Period Start

```{r time_blocks_4m_start}
    start_4m <- list(year = current_year, month = current_month)
```

##### Step 4.2.4: Calculate 4-Month Period End

```{r time_blocks_4m_end}
    end_4m_year <- current_year
    end_4m_month <- current_month + 3
    
    if (end_4m_month > 12) {
      end_4m_year <- end_4m_year + 1
      end_4m_month <- end_4m_month - 12
    }
    
    end_4m <- list(year = end_4m_year, month = end_4m_month)
```

##### Step 4.2.5: Calculate 12-Month Period Start

```{r time_blocks_12m_start}
    start_12m <- start_4m
```

##### Step 4.2.6: Calculate 12-Month Period End

```{r time_blocks_12m_end}
    end_12m_year <- current_year
    end_12m_month <- current_month + 11
    
    if (end_12m_month > 12) {
      end_12m_year <- end_12m_year + 1
      end_12m_month <- end_12m_month - 12
    }
    
    end_12m <- list(year = end_12m_year, month = end_12m_month)
```

##### Step 4.2.7: Create Date Label

```{r time_blocks_label}
    month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    date_range <- base::paste0(month_names[start_4m$month], " ", start_4m$year, "-",
                              month_names[end_4m$month], " ", end_4m$year)
```

##### Step 4.2.8: Store Block Information

```{r time_blocks_store}
    blocks[[i]] <- list(
      block_number = i,
      start_4m = start_4m,
      end_4m = end_4m,
      start_12m = start_12m,
      end_12m = end_12m,
      date_range = date_range
    )
```

##### Step 4.2.9: Move to Next Month

```{r time_blocks_next}
    current_month <- current_month + 1
    if (current_month > 12) {
      current_month <- 1
      current_year <- current_year + 1
    }
  }
  
  return(blocks)
}
```

### Step 5: Generate Detailed Seasonality Results

#### Step 5.1: Main Analysis Function

##### Step 5.1.1: Check Required Columns

```{r detailed_analysis_check}
generate_detailed_results <- function(data, year_col, month_col, value_col, 
                                     admin_unit_cols,
                                     start_year = NULL, start_month = 1, 
                                     threshold_percent = 60) {
  
  required_cols <- c(year_col, month_col, value_col, admin_unit_cols)
  missing_cols <- required_cols[!required_cols %in% base::colnames(data)]
  if (base::length(missing_cols) > 0) {
    base::stop(base::paste("Missing columns:", base::paste(missing_cols, collapse = ", ")))
  }
```

##### Step 5.1.2: Set Start Year

```{r detailed_analysis_start_year}
  if (base::is.null(start_year)) {
    start_year <- base::min(data[[year_col]], na.rm = TRUE)
  }
```

##### Step 5.1.3: Filter and Clean Data

```{r detailed_analysis_filter}
  filtered_data <- data %>%
    dplyr::filter(!base::is.na(!!dplyr::sym(year_col)) & 
                  !base::is.na(!!dplyr::sym(month_col)) & 
                  !!dplyr::sym(year_col) >= start_year) %>%
    dplyr::mutate(!!dplyr::sym(month_col) := base::as.numeric(!!dplyr::sym(month_col)))
```

##### Step 5.1.4: Create Combined Grouping Variable

```{r detailed_analysis_grouping}
  if (base::length(admin_unit_cols) == 1) {
    filtered_data$admin_group <- filtered_data[[admin_unit_cols[1]]]
  } else {
    admin_values <- filtered_data[admin_unit_cols]
    filtered_data$admin_group <- base::apply(admin_values, 1, function(x) {
      base::paste(x, collapse = " | ")
    })
  }
```

##### Step 5.1.5: Check Data Span

```{r detailed_analysis_span}
  available_years <- base::sort(base::unique(filtered_data[[year_col]]))
  data_span_years <- base::length(available_years)
  
  if (data_span_years < 6) {
    base::stop(base::paste("Insufficient data: Analysis requires at least 6 years of data, but only", 
               data_span_years, "years found."))
  }
```

##### Step 5.1.6: Calculate Number of Blocks

```{r detailed_analysis_blocks}
  num_complete_years <- data_span_years - 1
  total_num_blocks <- num_complete_years * 12
```

##### Step 5.1.7: Generate Blocks

```{r detailed_analysis_generate}
  blocks <- generate_time_blocks(start_year, start_month, total_num_blocks)
```

##### Step 5.1.8: Split Data by Location

```{r detailed_analysis_split}
  admin_unit_groups <- base::split(filtered_data, filtered_data$admin_group)
```

##### Step 5.1.9: Initialize Results

```{r detailed_analysis_init}
  detailed_results <- base::data.frame()
```

##### Step 5.1.10: Start Location Loop

```{r detailed_analysis_location_loop}
  for (admin_unit in base::names(admin_unit_groups)) {
    unit_data <- admin_unit_groups[[admin_unit]]
```

##### Step 5.1.11: Start Block Loop

```{r detailed_analysis_block_loop}
    for (i in base::seq_along(blocks)) {
      block <- blocks[[i]]
```

##### Step 5.1.12: Get 4-Month Data

```{r detailed_analysis_4m_data}
      data_4m <- unit_data[is_date_in_range(unit_data[[year_col]], 
                                           unit_data[[month_col]],
                                           block$start_4m, block$end_4m), ]
```

##### Step 5.1.13: Calculate 4-Month Total

```{r detailed_analysis_4m_total}
      total_4m <- base::sum(data_4m[[value_col]], na.rm = TRUE)
```

##### Step 5.1.14: Get 12-Month Data

```{r detailed_analysis_12m_data}
      data_12m <- unit_data[is_date_in_range(unit_data[[year_col]], 
                                            unit_data[[month_col]],
                                            block$start_12m, block$end_12m), ]
```

##### Step 5.1.15: Calculate 12-Month Total

```{r detailed_analysis_12m_total}
      total_12m <- base::sum(data_12m[[value_col]], na.rm = TRUE)
```

##### Step 5.1.16: Calculate Seasonality Percentage

```{r detailed_analysis_percent}
      percent_seasonality <- base::ifelse(total_12m > 0, (total_4m / total_12m) * 100, 0)
```

##### Step 5.1.17: Check Threshold

```{r detailed_analysis_threshold}
      is_seasonal <- base::as.numeric(percent_seasonality >= threshold_percent)
```

##### Step 5.1.18: Create Result Row

```{r detailed_analysis_row}
      result_row <- base::data.frame(
        Block = i,
        DateRange = block$date_range,
        Total_4M = total_4m,
        Total_12M = total_12m,
        Percent_Seasonality = base::round(percent_seasonality, 2),
        Seasonal = is_seasonal,
        stringsAsFactors = FALSE
      )
```

##### Step 5.1.19: Add Admin Columns (Multiple Levels)

```{r detailed_analysis_admin_multi}
      if (base::length(admin_unit_cols) > 1) {
        admin_parts <- base::strsplit(admin_unit, " \\| ")[[1]]
        for (j in base::seq_along(admin_unit_cols)) {
          if (j <= base::length(admin_parts)) {
            result_row[[admin_unit_cols[j]]] <- admin_parts[j]
          } else {
            result_row[[admin_unit_cols[j]]] <- NA
          }
        }
```

##### Step 5.1.20: Add Admin Column (Single Level)

```{r detailed_analysis_admin_single}
      } else {
        result_row[[admin_unit_cols[1]]] <- admin_unit
      }
```

##### Step 5.1.21: Append Result

```{r detailed_analysis_append}
      detailed_results <- base::rbind(detailed_results, result_row)
    }
  }
  
  return(detailed_results)
}
```

#### Step 5.2: Execute Detailed Analysis

```{r run_detailed_analysis}
# Run the detailed analysis
detailed_results <- generate_detailed_results(
  data = data,
  year_col = year_column,
  month_col = month_column, 
  value_col = value_column,
  admin_unit_cols = admin_columns,
  start_year = analysis_start_year,
  start_month = analysis_start_month,
  threshold_percent = seasonality_threshold
)

# Save results
writexl::write_xlsx(detailed_results, detailed_output)

# Display preview
knitr::kable(head(detailed_results), caption = "Preview of Detailed Block Results")
```

## Analysis Phase 2: Yearly Aggregation

### Step 6: Seasonality Eligibility by Years

#### Step 6.1: Yearly Summary Function

##### Step 6.1.1: Extract Start Year from Date Range

```{r yearly_summary_extract}
generate_yearly_summary <- function(detailed_results, admin_cols = admin_columns) {
  
  detailed_results$StartYear <- base::sapply(detailed_results$DateRange, function(x) {
    parts <- base::strsplit(x, "-")[[1]]
    first_part <- base::trimws(parts[1])
    base::as.numeric(base::substr(first_part, base::nchar(first_part) - 3, base::nchar(first_part)))
  })
```

##### Step 6.1.2: Group by Admin and Year

```{r yearly_summary_group}
  yearly_summary <- detailed_results %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(admin_cols)), StartYear) %>%
```

##### Step 6.1.3: Calculate Yearly Metrics

```{r yearly_summary_metrics}
    dplyr::summarise(
      Year = dplyr::first(StartYear),
      SeasonalCount = base::sum(Seasonal, na.rm = TRUE),
      total_blocks_in_year = 12,
      at_least_one_seasonal_block = base::as.numeric(SeasonalCount > 0),
      .groups = 'drop'
    ) %>%
```

##### Step 6.1.4: Create Year Period Label

```{r yearly_summary_label}
    dplyr::mutate(
      year_period = base::paste0("(Jan ", Year, "-Apr ", Year, ", Dec ", Year, "-Mar ", Year + 1, ")")
    ) %>%
```

##### Step 6.1.5: Select and Arrange Columns

```{r yearly_summary_arrange}
    dplyr::select(Year, dplyr::all_of(admin_cols), year_period, 
                  total_blocks_in_year, at_least_one_seasonal_block) %>%
    dplyr::arrange(Year, dplyr::across(dplyr::all_of(admin_cols)))
  
  return(yearly_summary)
}
```

#### Step 6.2: Generate Yearly Results

```{r generate_yearly_summary}
# Generate yearly summary from detailed results
yearly_summary <- generate_yearly_summary(detailed_results, admin_columns)

# Save results
writexl::write_xlsx(yearly_summary, yearly_output)

# Display results
knitr::kable(tail(yearly_summary, 10), caption = "Yearly Seasonality Summary (Last 10 rows)")
```

### Step 7: Location-Level Seasonality Classification

#### Step 7.1: Location Summary Function

##### Step 7.1.1: Group by Location

```{r location_summary_group}
generate_location_summary <- function(yearly_summary, admin_cols = admin_columns) {
  
  location_summary <- yearly_summary %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(admin_cols))) %>%
```

##### Step 7.1.2: Count Seasonal Years

```{r location_summary_count}
    dplyr::summarise(
      SeasonalYears = base::sum(at_least_one_seasonal_block, na.rm = TRUE),
      TotalYears = dplyr::n(),
      .groups = 'drop'
    ) %>%
```

##### Step 7.1.3: Classify Seasonality

```{r location_summary_classify}
    dplyr::mutate(
      Seasonality = base::ifelse(SeasonalYears == TotalYears, "Seasonal", "Not Seasonal")
    ) %>%
```

##### Step 7.1.4: Arrange Results

```{r location_summary_arrange}
    dplyr::arrange(dplyr::across(dplyr::all_of(admin_cols)))
  
  return(location_summary)
}
```

#### Step 7.2: Execute Location Analysis

```{r execute_location_analysis}
# Generate location summary from yearly summary
location_summary <- generate_location_summary(yearly_summary, admin_columns)

# Save results
writexl::write_xlsx(location_summary, location_output)

# Display results
knitr::kable(head(location_summary, 10), caption = "Location Seasonality Classification")
```

## Analysis Phase 3: Peak Period Analysis

### Step 8: Rainfall Peak Analysis Setup

#### Step 8.1: Configure Peak Analysis Parameters

```{r peak_analysis_config}
# Peak analysis configuration
INPUT_FILE <- here::here("english/data_r/modeled", "chirps_data_2015_2023_lastest.xls")
PEAK_ANALYSIS_OUTPUT <- "rainfall_max_percentages_by_location.xlsx"

# Analysis time range
PEAK_ANALYSIS_START_YEAR <- 2018
PEAK_ANALYSIS_END_YEAR <- 2023

# Column mappings
LOCATION_COL <- c("FIRST_DNAM", "FIRST_CHIE")
RAINFALL_COL <- "mean_rain"
YEAR_COL <- "Year"
MONTH_COL <- "Month"
```

#### Step 8.2: Load and Process Peak Analysis Data

##### Step 8.2.1: Load Data

```{r load_peak_data}
df <- readxl::read_excel(INPUT_FILE)
```

##### Step 8.2.2: Filter to Analysis Period

```{r filter_peak_data}
df <- df |> 
  dplyr::filter(!!sym(YEAR_COL) >= PEAK_ANALYSIS_START_YEAR & 
                !!sym(YEAR_COL) <= PEAK_ANALYSIS_END_YEAR)
```

##### Step 8.2.3: Pivot Data Wide

```{r pivot_peak_data}
df_pivot <- df |>
  dplyr::select(dplyr::all_of(LOCATION_COL), !!sym(MONTH_COL), !!sym(YEAR_COL), !!sym(RAINFALL_COL)) |>
  tidyr::pivot_wider(
    names_from = !!sym(YEAR_COL),
    values_from = !!sym(RAINFALL_COL),
    names_sort = TRUE
  )
```

##### Step 8.2.4: Calculate Mean Rainfall

```{r mean_peak_data}
year_columns <- as.character(PEAK_ANALYSIS_START_YEAR:PEAK_ANALYSIS_END_YEAR)
df_pivot <- df_pivot |>
  dplyr::rowwise() |>
  dplyr::mutate(Mean = mean(dplyr::c_across(dplyr::all_of(year_columns)), na.rm = TRUE)) |>
  dplyr::ungroup()
```

### Step 9: Rolling Window Analysis

#### Step 9.1: Window Creation Helper Function

##### Step 9.1.1: Filter by Months

```{r window_helper_filter}
create_window <- function(months_list, start_month, window_size, pivot_data = df_pivot) {
  pivot_data |>
    dplyr::filter(!!sym(MONTH_COL) %in% months_list) |>
```

##### Step 9.1.2: Group by Location

```{r window_helper_group}
    dplyr::group_by(dplyr::across(dplyr::all_of(LOCATION_COL))) |>
```

##### Step 9.1.3: Sum Rainfall

```{r window_helper_sum}
    dplyr::summarise(rainfall_mean = sum(Mean, na.rm = TRUE), .groups = "drop") |>
```

##### Step 9.1.4: Add Window Info

```{r window_helper_info}
    dplyr::mutate(
      start_month = start_month,
      window_size = window_size
    )
}
```

#### Step 9.2: Generate 3-Month Windows

```{r windows_3_month}
w3_3 <- create_window(c(3, 4, 5), 3, 3)
w4_3 <- create_window(c(4, 5, 6), 4, 3)
w5_3 <- create_window(c(5, 6, 7), 5, 3)
w6_3 <- create_window(c(6, 7, 8), 6, 3)
w7_3 <- create_window(c(7, 8, 9), 7, 3)
w8_3 <- create_window(c(8, 9, 10), 8, 3)
w9_3 <- create_window(c(9, 10, 11), 9, 3)
w10_3 <- create_window(c(10, 11, 12), 10, 3)
w11_3 <- create_window(c(11, 12, 1), 11, 3)
w12_3 <- create_window(c(12, 1, 2), 12, 3)
```

#### Step 9.3: Generate 4-Month Windows

```{r windows_4_month}
w3_4 <- create_window(c(3, 4, 5, 6), 3, 4)
w4_4 <- create_window(c(4, 5, 6, 7), 4, 4)
w5_4 <- create_window(c(5, 6, 7, 8), 5, 4)
w6_4 <- create_window(c(6, 7, 8, 9), 6, 4)
w7_4 <- create_window(c(7, 8, 9, 10), 7, 4)
w8_4 <- create_window(c(8, 9, 10, 11), 8, 4)
w9_4 <- create_window(c(9, 10, 11, 12), 9, 4)
w10_4 <- create_window(c(10, 11, 12, 1), 10, 4)
w11_4 <- create_window(c(11, 12, 1, 2), 11, 4)
w12_4 <- create_window(c(12, 1, 2, 3), 12, 4)
```

#### Step 9.4: Generate 5-Month Windows

```{r windows_5_month}
w3_5 <- create_window(c(3, 4, 5, 6, 7), 3, 5)
w4_5 <- create_window(c(4, 5, 6, 7, 8), 4, 5)
w5_5 <- create_window(c(5, 6, 7, 8, 9), 5, 5)
w6_5 <- create_window(c(6, 7, 8, 9, 10), 6, 5)
w7_5 <- create_window(c(7, 8, 9, 10, 11), 7, 5)
w8_5 <- create_window(c(8, 9, 10, 11, 12), 8, 5)
w9_5 <- create_window(c(9, 10, 11, 12, 1), 9, 5)
w10_5 <- create_window(c(10, 11, 12, 1, 2), 10, 5)
w11_5 <- create_window(c(11, 12, 1, 2, 3), 11, 5)
w12_5 <- create_window(c(12, 1, 2, 3, 4), 12, 5)
```

### Step 10: Window Analysis Consolidation

#### Step 10.1: Combine All Windows

```{r combine_windows}
all_windows <- dplyr::bind_rows(
  w3_3, w3_4, w3_5,
  w4_3, w4_4, w4_5,
  w5_3, w5_4, w5_5,
  w6_3, w6_4, w6_5,
  w7_3, w7_4, w7_5,
  w8_3, w8_4, w8_5,
  w9_3, w9_4, w9_5,
  w10_3, w10_4, w10_5,
  w11_3, w11_4, w11_5,
  w12_3, w12_4, w12_5
)
```

#### Step 10.2: Reshape and Calculate Percentages

##### Step 10.2.1: Pivot Window Data Wider

```{r reshape_pivot}
final_result <- all_windows |>
  tidyr::pivot_wider(
    names_from = window_size,
    values_from = rainfall_mean,
    names_glue = "window_{window_size}_month_mean"
  )
```

##### Step 10.2.2: Calculate Annual Total per Location

```{r reshape_annual}
annual_total <- df_pivot |>
  dplyr::group_by(dplyr::across(dplyr::all_of(LOCATION_COL))) |>
  dplyr::summarise(annual_total = sum(Mean, na.rm = TRUE), .groups = "drop")
```

##### Step 10.2.3: Merge Annual Totals

```{r reshape_merge}
final_result <- final_result |>
  dplyr::left_join(annual_total, by = LOCATION_COL) |>
```

##### Step 10.2.4: Calculate Window Percentages

```{r reshape_percentages}
  dplyr::mutate(
    window_3_month_pct = ifelse(annual_total == 0, 0, 
                                (window_3_month_mean / annual_total) * 100),
    window_4_month_pct = ifelse(annual_total == 0, 0, 
                                (window_4_month_mean / annual_total) * 100),
    window_5_month_pct = ifelse(annual_total == 0, 0, 
                                (window_5_month_mean / annual_total) * 100)
  )
```

### Step 11: Peak Identification and SMC Window Selection

#### Step 11.1: Find Maximum Percentages

##### Step 11.1.1: Find Max 3-Month Window

```{r find_max_3}
max_3_month_details <- final_result |>
  dplyr::group_by(dplyr::across(dplyr::all_of(LOCATION_COL))) |>
  dplyr::slice_max(window_3_month_pct, n = 1, with_ties = FALSE) |>
  dplyr::select(dplyr::all_of(LOCATION_COL), 
                max_3_month_pct = window_3_month_pct, 
                max_3_month_start = start_month) |>
  dplyr::ungroup()
```

##### Step 11.1.2: Find Max 4-Month Window

```{r find_max_4}
max_4_month_details <- final_result |>
  dplyr::group_by(dplyr::across(dplyr::all_of(LOCATION_COL))) |>
  dplyr::slice_max(window_4_month_pct, n = 1, with_ties = FALSE) |>
  dplyr::select(dplyr::all_of(LOCATION_COL), 
                max_4_month_pct = window_4_month_pct, 
                max_4_month_start = start_month) |>
  dplyr::ungroup()
```

##### Step 11.1.3: Find Max 5-Month Window

```{r find_max_5}
max_5_month_details <- final_result |>
  dplyr::group_by(dplyr::across(dplyr::all_of(LOCATION_COL))) |>
  dplyr::slice_max(window_5_month_pct, n = 1, with_ties = FALSE) |>
  dplyr::select(dplyr::all_of(LOCATION_COL), 
                max_5_month_pct = window_5_month_pct, 
                max_5_month_start = start_month) |>
  dplyr::ungroup()
```

#### Step 11.2: Create Consolidated Peak Summary

##### Step 11.2.1: Merge All Window Maxima

```{r peak_summary}
max_summary <- max_3_month_details |>
  dplyr::left_join(max_4_month_details, by = LOCATION_COL) |>
  dplyr::left_join(max_5_month_details, by = LOCATION_COL)
```

#### Step 11.3: SMC Window Selection Logic

##### Step 11.3.1: Define Month Names

```{r smc_month_names}
month_names <- c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December")
```

##### Step 11.3.2: Create Month Name Function

```{r smc_get_name}
get_month_name <- function(month_num) {
  month_names[month_num]
}
```

##### Step 11.3.3: Create End Month Function

```{r smc_get_end}
get_end_month <- function(start_month, window_size) {
  end_month_num <- ((start_month - 1 + window_size - 1) %% 12) + 1
  return(end_month_num)
}
```

##### Step 11.3.4: Process Each Row

```{r smc_process_row}
max_summary <- max_summary |>
  dplyr::rowwise() |>
```

##### Step 11.3.5: Calculate Difference

```{r smc_calc_diff}
  dplyr::mutate(
    difference_5mo_vs_4mo_percent = max_5_month_pct - max_4_month_pct,
```

##### Step 11.3.6: Select Window Size

```{r smc_select_window}
    SMC_window_size_selected = ifelse(difference_5mo_vs_4mo_percent > 10, 5, 4),
```

##### Step 11.3.7: Get Best Start Month

```{r smc_best_start}
    best_start_month = case_when(
      SMC_window_size_selected == 4 ~ max_4_month_start,
      SMC_window_size_selected == 5 ~ max_5_month_start,
      TRUE ~ NA_real_
    ),
```

##### Step 11.3.8: Calculate End Month

```{r smc_calc_end}
    end_month = get_end_month(best_start_month, SMC_window_size_selected),
```

##### Step 11.3.9: Create SMC Cycle Label

```{r smc_create_label}
    SMC_cycles = paste0(get_month_name(best_start_month), " - ", get_month_name(end_month))
  ) |>
```

##### Step 11.3.10: Clean Up

```{r smc_cleanup}
  dplyr::ungroup() |>
  dplyr::select(-end_month)
```

##### Step 11.3.11: Save Peak Analysis Results

```{r smc_save_peak}
openxlsx::write.xlsx(max_summary, PEAK_ANALYSIS_OUTPUT, rowNames = FALSE)

knitr::kable(tail(max_summary, 10), caption = "Peak Analysis Results (Last 10 locations)")
```

## Analysis Phase 4: Final SMC Eligibility Determination

### Step 12: Integrate Incidence Data and Determine Final Eligibility

#### Step 12.1: Configure Final Analysis Parameters

```{r final_analysis_config}
# Final analysis configuration
incidence_file <- here::here("english/data_r/modeled", "incidence.xlsx")
FINAL_SMC_OUTPUT <- "final_SMC_eligibility.xlsx"

# SMC Eligibility criteria
SEASONALITY_REQUIRED <- "Seasonal"
INCIDENCE_THRESHOLD <- 250

# Column names
SEASONALITY_COL <- "Seasonality"
INCIDENCE_COL <- "incidence"
```

#### Step 12.2: Load Incidence Data and Merge All Results

##### Step 12.2.1: Load Incidence File

```{r load_incidence}
incidence_data <- readxl::read_excel(incidence_file)
```

##### Step 12.2.2: Merge Seasonality Data

```{r merge_seasonality}
final_smc_data <- location_summary |>
  dplyr::left_join(max_summary, by = LOCATION_COL) |>
```

##### Step 12.2.3: Merge Incidence Data

```{r merge_incidence}
  dplyr::left_join(incidence_data, by = LOCATION_COL)
```

#### Step 12.3: Apply Final SMC Eligibility Criteria

##### Step 12.3.1: Check Both Criteria

```{r final_eligibility}
final_output <- final_smc_data |>
  dplyr::mutate(
    SMC_cycles = dplyr::case_when(
      .data[[SEASONALITY_COL]] == SEASONALITY_REQUIRED & 
      .data[[INCIDENCE_COL]] > INCIDENCE_THRESHOLD ~ SMC_cycles,
      TRUE ~ "Not Eligible"
    )
  ) |>
```

##### Step 12.3.2: Sort Results

```{r final_sort}
  dplyr::arrange(dplyr::across(dplyr::all_of(LOCATION_COL)))
```

##### Step 12.3.3: Save Final Output

```{r final_save}
openxlsx::write.xlsx(final_output, FINAL_SMC_OUTPUT, rowNames = FALSE)

knitr::kable(head(final_output, 10), caption = "Final SMC Eligibility Results")
```

##### Step 12.3.4: Create Summary by Cycle

```{r final_summary}
smc_summary <- final_output |>
  dplyr::count(SMC_cycles, name = "Number_of_Locations") |>
  dplyr::arrange(desc(Number_of_Locations))

knitr::kable(smc_summary, caption = "SMC Eligibility Summary by Cycle")
```

## Summary

This analysis provides a comprehensive framework for determining SMC eligibility based on:

1. **Seasonality Assessment**: Using time-block analysis to identify consistent seasonal patterns
2. **Peak Period Identification**: Determining optimal intervention timing through rolling window analysis
3. **Incidence Integration**: Incorporating malaria burden data to refine targeting
4. **Final Eligibility**: Combining all criteria to determine SMC-eligible areas and optimal implementation periods

The structured approach ensures reproducible results and clear decision-making criteria for malaria prevention programs.
