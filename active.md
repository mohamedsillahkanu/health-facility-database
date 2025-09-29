
## Step 1: Define file path and load data

```r
# Step 1: Define file path using here package for reproducible paths
data_filepath <- here::here("english/data_r/routine_cases/clean_malaria_routine_data_final.rds")

# Step 2: Load the preprocessed DHIS2 malaria surveillance data
df <- base::readRDS(data_filepath)
```

**To adapt the code**:

* **Line 2:** Update the folder path in `here::here()` to match your directory structure.
* **Line 5:** Replace `clean_malaria_routine_data_final.rds` with your actual data file name.

---

## Step 2: Specify reporting columns

```r
report_cols <- c("allout", "susp", "test", "conf", "maltreat")
```

**To adapt the code**:

* **Line 1:** Replace the variable names with the columns representing reporting data in your dataset.

---

## Step 3: Create reporting flags and boundaries

```r
df <- df |>
  # Create a flag if any reporting occurred in a month
  dplyr::mutate(reported = ifelse(rowSums(dplyr::select(., dplyr::all_of(report_cols)), na.rm = TRUE) > 0, 1, 0)) |>
  
  # Group by facility
  dplyr::group_by(hf_uid) |>
  
  # Calculate total reports, first month, and last month of reporting
  dplyr::mutate(
    total_reports = sum(reported, na.rm = TRUE),
    first_month_reported = ifelse(any(reported == 1), min(date[reported == 1], na.rm = TRUE), NA),
    last_month_reported  = ifelse(any(reported == 1), max(date[reported == 1], na.rm = TRUE), NA)
  ) |>
  dplyr::ungroup()
```

**To adapt the code**:

* **Line 7:** Ensure `hf_uid` matches your health facility unique identifier column.
* **Line 11:** Make sure `date` matches your date column format.

---

## Step 4: Create reporting status using three methods

### Step 4.1: Method 1 – Permanent activation

```r
df <- df |>
  dplyr::mutate(
    reporting_status_method1 = dplyr::case_when(
      is.na(first_month_reported) ~ "Never reported",
      date >= first_month_reported & reported == 1 ~ "Expected and reported",
      date >= first_month_reported & reported == 0 ~ "Expected but didn't report",
      TRUE ~ "Not expected to report"
    )
  )
```

**To adapt the code**:

* **Line 4:** Ensure `first_month_reported` exists from Step 3 before running this block.
* Use this method if facilities are **always expected to report** after their first reporting month.

---

### Step 4.2: Method 2 – Defined active period

```r
df <- df |>
  dplyr::mutate(
    reporting_status_method2 = dplyr::case_when(
      is.na(first_month_reported) ~ "Never reported",
      date >= first_month_reported & date <= last_month_reported & reported == 1 ~ "Expected and reported",
      date >= first_month_reported & date <= last_month_reported & reported == 0 ~ "Expected but didn't report",
      TRUE ~ "Not expected to report"
    )
  )
```

**To adapt the code**:

* **Line 4:** Ensure both `first_month_reported` and `last_month_reported` columns exist from Step 3.
* Use this method when a facility is **only expected to report between its first and last reporting dates**.

---

### Step 4.3: Method 3 – Dynamic windows

```r
df <- df |>
  dplyr::arrange(hf_uid, date) |>
  dplyr::group_by(hf_uid) |>
  dplyr::mutate(
    zero_run = {
      r <- base::rle(reported == 0)
      run_flag <- r$values & r$lengths >= 6
      rep(run_flag, r$lengths)
    },
    reporting_status_method3 = dplyr::case_when(
      is.na(first_month_reported) ~ "Never reported",
      !zero_run & reported == 1 ~ "Expected and reported",
      !zero_run & reported == 0 ~ "Expected but didn't report",
      TRUE ~ "Not expected to report"
    )
  ) |>
  dplyr::ungroup()
```

**To adapt the code**:

* **Line 4:** Make sure `hf_uid` and `date` are correctly specified.
* **Line 7:** Adjust `6` to change the number of consecutive months with no reports to classify as inactive.
* Use this method if **long reporting gaps indicate inactivity**.

---

## Step 5: Define colors for plots

```r
status_colors <- c(
  "Never reported" = "#D55E00",
  "Expected and reported" = "#0072B2",
  "Expected but didn't report" = "#E69F00",
  "Not expected to report" = "#999999"
)

active_colors <- c(
  "Active" = "#0072B2",
  "Inactive" = "#D55E00"
)
```

**To adapt the code**:

* Update hex color codes to match your organization’s branding or style guidelines.

---

## Step 6: Define method meta information

```r
status_methods <- c("method1", "method2", "method3")
status_titles <- c(
  "Method 1: Permanent Activation",
  "Method 2: Defined Active Period",
  "Method 3: Dynamic Windows"
)
```

**To adapt the code**:

* **Line 1:** Add or remove methods as needed.
* **Line 2–4:** Update titles to match new method descriptions.

---

## Step 7: Loop through methods to create and save plots

```r
for (i in seq_along(status_methods)) {
  
  # --- Detailed status plot
  fill_column <- paste0("reporting_status_", status_methods[i])
  
  p_status <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = reorder(hf_uid, total_reports), fill = !!rlang::sym(fill_column))) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(values = status_colors, name = "Status", na.value = "white") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 6, angle = 90, hjust = 1)
    ) +
    ggplot2::labs(x = "Date", y = "Health Facilities", title = status_titles[i])
  
  ggplot2::ggsave(
    filename = paste0("method_", i, "_status_plot.png"),
    plot = p_status,
    width = 15, height = 15, units = "in", dpi = 300
  )
  
  base::print(p_status)
  
  # --- Create active vs inactive column
  active_col <- paste0("active_status_", status_methods[i])
  df <- df |>
    dplyr::mutate(
      !!active_col := dplyr::case_when(
        !!rlang::sym(fill_column) %in% c("Expected and reported", "Expected but didn't report") ~ "Active",
        TRUE ~ "Inactive"
      )
    )
  
  # --- Active vs inactive plot
  p_active <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = reorder(hf_uid, total_reports), fill = !!rlang::sym(active_col))) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(values = active_colors, name = "Facility Status", na.value = "white") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 6, angle = 90, hjust = 1)
    ) +
    ggplot2::labs(x = "Date", y = "Health Facilities", title = paste(status_titles[i], "- Active vs Inactive"))
  
  ggplot2::ggsave(
    filename = paste0("method_", i, "_active_vs_inactive_plot.png"),
    plot = p_active,
    width = 15, height = 15, units = "in", dpi = 300
  )
  
  base::print(p_active)
}
```

**To adapt the code**:

* **Line 2:** Ensure `status_methods` matches the reporting methods created in Step 4.
* **Line 5:** Update `hf_uid` and `date` to match your dataset columns.
* **Line 19:** Adjust `width` and `height` in `ggplot2::ggsave()` to resize plots.
* Generated plots will be saved as PNG files in the working directory.

---

## Step 8: Save the final dataset

```r
# Save as Excel
openxlsx::write.xlsx(df, "final_reporting_status.xlsx", overwrite = TRUE)

# Save as CSV
utils::write.csv(df, "final_reporting_status.csv", row.names = FALSE)
```

**To adapt the code**:

* **Line 2:** Change `"final_reporting_status.xlsx"` to your preferred Excel filename.
* **Line 5:** Update `"final_reporting_status.csv"` to your preferred CSV filename.
* Ensure you have write permissions in the output folder.


By including **“To adapt the code”** after each block, this Markdown file gives clear, contextual guidance on what to modify and why.
