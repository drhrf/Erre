# Erre: Interactive Data Analysis and Visualization App

**Erre** is a Shiny-based web application built in R for interactive data analysis, visualization, and statistical testing. It provides a graphical interface for uploading datasets, exploring descriptive statistics, performing inferential tests, and generating publication-ready plots — all without writing a single line of R code.

---

## Features

Erre supports an end-to-end exploratory data analysis (EDA) workflow, including:

### Data Import
- Upload `.xlsx`, `.xls`, `.csv`, or `.tsv` files through the web interface.
- Automatic detection of file type and seamless data import.
- Preview of the uploaded dataset.

### Data Exploration
- Interactive display of the data frame head.
- Basic descriptive statistics (mean, median, SD, variance, etc.).
- Outlier detection and visualization.

### Visualization Tools
- **Histogram:** Density plots, annotation, and custom breaks.
- **Boxplot:** Distribution visualization with violin overlay and jittered points.
- **Barplot:** Summaries by group (mean, median, SD, variance) with error bars.
- **Regression Plot:** Linear (`lm`), generalized linear (`glm`), or LOESS models with customizable annotations and confidence bands.

### Statistical Analysis
- Normality testing (Shapiro–Wilk).
- ANOVA with post-hoc Tukey test.
- Welch t-test.
- Kruskal–Wallis and Dunn’s non-parametric tests.
- Wilcoxon rank-sum test.
- Comprehensive p-value reporting.
- Exportable tables for all results.

### Export Options
- Downloadable plots in TIFF format with adjustable resolution, width, and height.
- Exportable results tables in `.xlsx` format for further analysis or reporting.

---

## Requirements

### R Version
- R >= 4.0.0 (tested with R 4.3+)

### Dependencies
The app requires the following R packages:

```r
shiny
tidyverse
gplots
bslib
plotly
reshape2
stats
psych
kableExtra
rstatix
DescTools
readxl
openxlsx
```
If they are not installed, run:

```r
install.packages(c(
  "shiny", "tidyverse", "gplots", "bslib", "plotly", "reshape2", 
  "psych", "kableExtra", "rstatix", "DescTools", "readxl", "openxlsx"
))
```

### Installation & Running Locally

Clone the repository and run the app directly from R:

```r
# From GitHub
shiny::runGitHub("Erre", "drhrf")

# Or locally
shiny::runApp("path/to/Erre")
```

This will launch the app in your default browser.

## File Upload Guide

The app accepts the following file types:
	•	.xlsx / .xls – Excel workbooks (use the Sheet number input to select a specific sheet)
	•	.csv – Comma-separated values
	•	.tsv – Tab-separated values

Important: Your dataset must contain only numeric columns for statistical analyses and plotting. Character or factor variables will cause warnings.

⸻

## User Guide

1. Upload Data

Go to the “File upload” tab and upload your dataset. Select the correct sheet number if using Excel.

2. Explore Data

Navigate to the “Dataframe” tab to inspect the first few rows.

3. Visualize

Use the Histogram, Boxplot, Barplot, and Regression plot tabs to visualize data. Customize plot aesthetics in the “General” tab.

4. Statistics

Visit the “Statistics” tab to view descriptive statistics, normality tests, ANOVA, Tukey post-hoc, t-tests, non-parametric tests, and more.

5. Download Results

Each results table and plot can be downloaded for publication or further processing.

⸻

## Configuration Options

All visualizations support extensive customization:
	•	Title, axis labels, colors, and annotations.
	•	Plot size (width/height) and resolution for export.
	•	Choice of statistical functions for barplots.
	•	Confidence levels for statistical tests.
	•	Control over regression type and standard error visualization.

⸻

## Tips & Best Practices
	•	Always clean and format your dataset before uploading. Missing or non-numeric values may interfere with statistical tests.
	•	Use meaningful column names — they are used automatically in plots and outputs.
	•	For .xlsx files with multiple sheets, ensure the correct sheet number is selected before loading.
	•	When exporting plots, adjust resolution and size in the “General” panel to ensure publication-quality output.

⸻

## Example Dataset

If no file is uploaded, the app loads the built-in iris dataset automatically so you can explore the features before uploading your own data.

⸻

## Troubleshooting

Problem: .xlsx files are not selectable in the upload dialog.
Solution: Make sure your fileInput() accept parameter includes ".xlsx" and the MIME type:

```r
accept = c(".xlsx", ".xls", ".csv", ".tsv", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
```

Problem: Warning about non-numeric data.
Solution: Remove or convert non-numeric columns before uploading, as most statistical tests require numeric input.

Problem: Plots or statistics do not render.
Solution: Ensure that required inputs (e.g., column selections, group indices) are specified correctly in the sidebar.

⸻

📜 License

This project is distributed under the MIT License. See LICENSE for details.

⸻

👨‍🔬 Citation

If you use Erre for your research or teaching, please cite it as:

Hercules Freitas, Erre: A Shiny Application for Interactive Data Analysis and Visualization, 2025. [Github Repository](https://github.com/drhrf/Erre)