# Health-facility reporting status (three methods) — step-by-step guide (R)

## Overview

This walkthrough builds three monthly reporting–status methods for each health facility (`hf_uid`) and generates two heatmaps per method:

* a detailed **four-class status** view, and
* a simplified **Active vs Inactive** view.

**Methods**

* **Method 1 — Permanent activation:** once a facility reports at least once, all subsequent months are “expected” (reported or not)
* **Method 2 — Defined active period:** only the months between the first and last report are “expected”
* **Method 3 — Dynamic windows:** long runs of non-reporting (≥ 6 consecutive zero-report months) are treated as **not expected to report**

---

## Step 1: Install and load packages

```r
# Install (run once as needed)
install.packages(c("readxl", "dplyr", "ggplot2", "rlang"))

# Load
library(readxl)
library(dplyr)
library(ggplot2)
library(rlang)
```

---

## Step 2: Read data

```r
# Replace with your actual file path
df <- read_excel("/content/clean_malaria_routine_data_final.xlsx")

# Ensure date is Date class (adjust the column name if yours differs)
if (!inherits(df$date, "Date")) {
  df <- df |> mutate(date = as.Date(date))
}
```

---

## Step 3: Select reporting columns

```r
# Columns whose (row-wise) sum > 0 indicates any reporting in that month
report_cols <- c("allout", "susp", "test", "conf", "maltreat")
```

---

## Step 4: Create reporting flags and facility boundaries

```r
df <- df |>
  # 4.1: Flag if any reporting occurred in that month
  mutate(reported = ifelse(rowSums(dplyr::select(., dplyr::all_of(report_cols)), na.rm = TRUE) > 0, 1, 0)) |>
  # 4.2: Group by facility
  group_by(hf_uid) |>
  # 4.3: Calculate first and last reporting month and total reporting counts
  mutate(
    total_reports         = sum(reported, na.rm = TRUE),
    first_month_reported  = ifelse(any(reported == 1), min(date[reported == 1], na.rm = TRUE), as.Date(NA)),
    last_month_reported   = ifelse(any(reported == 1), max(date[reported == 1], na.rm = TRUE), as.Date(NA))
  ) |>
  ungroup()
```

---

## Step 5: Create three reporting-status methods

```r
df <- df |>
  # METHOD 1 — Permanent activation
  mutate(
    reporting_status_method1 = dplyr::case_when(
      is.na(first_month_reported) ~ "Never reported",
      date >= first_month_reported & reported == 1 ~ "Expected and reported",
      date >= first_month_reported & reported == 0 ~ "Expected but didn't report",
      TRUE ~ "Not expected to report"
    )
  ) |>
  # METHOD 2 — Defined active period (between first and last report)
  mutate(
    reporting_status_method2 = dplyr::case_when(
      is.na(first_month_reported) ~ "Never reported",
      date >= first_month_reported & date <= last_month_reported & reported == 1 ~ "Expected and reported",
      date >= first_month_reported & date <= last_month_reported & reported == 0 ~ "Expected but didn't report",
      TRUE ~ "Not expected to report"
    )
  ) |>
  # METHOD 3 — Dynamic windows (≥ 6 consecutive zero months => not expected)
  arrange(hf_uid, date) |>
  group_by(hf_uid) |>
  mutate(
    zero_run = {
      r <- rle(reported == 0)
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
  ungroup()
```

---

## Step 6: Plot and export outputs for all methods

```r
# Method meta
status_methods <- c("method1", "method2", "method3")
status_titles <- c("Method 1: Permanent Activation",
                   "Method 2: Defined Active Period",
                   "Method 3: Dynamic Windows")

for (i in seq_along(status_methods)) {

  # --- Colors (declared inside the plotting step to keep styles close to usage)
  status_colors <- c(
    "Never reported" = "#D55E00",
    "Expected and reported" = "#0072B2",
    "Expected but didn't report" = "#E69F00",
    "Not expected to report" = "#999999"
  )
  active_colors <- c(
    "Active" = "#0072B2",   # Blue
    "Inactive" = "#D55E00"  # Orange/Red
  )

  # ======================
  # 1) Detailed Status Plot
  # ======================
  fill_column <- paste0("reporting_status_", status_methods[i])

  p_status <- ggplot(
      df,
      aes(x = date, y = reorder(hf_uid, total_reports), fill = .data[[fill_column]])
    ) +
    geom_raster() +
    scale_fill_manual(values = status_colors, name = "Status", na.value = "white") +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    labs(
      x = "Date",
      y = "Health facilities",
      title = status_titles[i]
    ) +
    theme_minimal() +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x  = element_text(size = 6, angle = 90, hjust = 1)
    )

  ggsave(
    filename = paste0("method_", i, "_status_plot.png"),
    plot = p_status,
    width = 15, height = 15, units = "in", dpi = 300
  )
  print(p_status)
  message(sprintf("Saved: method_%s_status_plot.png", i))

  # ===========================
  # 2) Active vs Inactive Plot
  # ===========================
  active_col <- paste0("active_status_", status_methods[i])

  df <- df |>
    mutate(
      "{active_col}" := dplyr::case_when(
        .data[[fill_column]] %in% c("Expected and reported", "Expected but didn't report") ~ "Active",
        TRUE ~ "Inactive"
      )
    )

  p_active <- ggplot(
      df,
      aes(x = date, y = reorder(hf_uid, total_reports), fill = .data[[active_col]])
    ) +
    geom_raster() +
    scale_fill_manual(values = active_colors, name = "Facility status", na.value = "white") +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    labs(
      x = "Date",
      y = "Health facilities",
      title = paste(status_titles[i], "- Active vs Inactive")
    ) +
    theme_minimal() +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x  = element_text(size = 6, angle = 90, hjust = 1)
    )

  ggsave(
    filename = paste0("method_", i, "_active_vs_inactive_plot.png"),
    plot = p_active,
    width = 15, height = 15, units = "in", dpi = 300
  )
  print(p_active)
  message(sprintf("Saved: method_%s_active_vs_inactive_plot.png", i))
}
```

---

## Step 7: Optional — Export the augmented data

```r
# CSV
write.csv(df, "reporting_status_augmented.csv", row.names = FALSE)

# Or Excel (if you prefer)
# install.packages("writexl")
# writexl::write_xlsx(df, "reporting_status_augmented.xlsx")

message("Exported augmented data successfully")
```

---

## How to adapt the code

* Step 2: change the file path in `read_excel()`
* Step 2: change `date` to your date column name if different
* Step 3: change `report_cols` to match your dataset’s reporting variables
* Step 4: change `hf_uid` to your unique facility identifier column
* Step 5: change the zero-run length in Method 3 by editing `>= 6` if your definition differs
* Step 6: change titles in `status_titles` if you want different plot headings
* Step 6: change color hex codes inside the plotting step if you need a different palette
* Step 7: change the export file name or switch to Excel with `writexl::write_xlsx()`

---

## Quick checks

* Confirm `date` is a proper Date; if not, convert before Step 4
* Ensure every column in `report_cols` exists and is numeric (0/NA/positive counts)
* Verify plots are created in your current working directory (`getwd()` to see where)

You can paste this entire guide into a `.md` (or Quarto `.qmd`) file and run the R chunks as is.
