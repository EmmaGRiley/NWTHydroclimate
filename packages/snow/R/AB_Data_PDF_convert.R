#Convert Alberta WISKI snow course PDF reports to Excel
#Use this when Alberta sends the monthly snow course report as a PDF instead of Excel (usually for March/May maps).
#See README_snow_scripts.md, section 6b. Based on snow_local/R/AB_Data_PDF_convert.R
#Output: one Excel file in snow/data/AB_Data/, with a sheet per report (Mountains, Plains)

# ---- Settings: change these each time ----
report_year  <- 2026
report_month <- "may" #used in the output file name
mountains_pdf_name <- "2026_may_mountains_AB.pdf" #PDF in snow/data/AB_Data/. NA if you only have a plains report
plains_pdf_name    <- NA                          #e.g. "2026_may_plains_AB.pdf". NA if you only have a mountains report
# -------------------------------------------

# Load required libraries (one-time install: install.packages(c("pdftools", "writexl")))
library(pdftools)
library(writexl)

user <- paste0("C:/Users/", tolower(Sys.getenv("USERNAME")))
ab_path <- paste0(user, "/Documents/R_Scripts/Packages/snow/data/AB_Data/")

# Check inputs
pdf_names <- c(Mountains = mountains_pdf_name, Plains = plains_pdf_name)
pdf_names <- pdf_names[!is.na(pdf_names)]
if (length(pdf_names) == 0) {
  stop("Set mountains_pdf_name and/or plains_pdf_name at the top of the script.", call. = FALSE)
}
for (f in pdf_names) {
  if (!file.exists(paste0(ab_path, f))) {
    stop("Can't find ", ab_path, f, "\nPDFs in that folder: ",
         paste(list.files(ab_path, pattern = "\\.pdf$", ignore.case = TRUE), collapse = ", "), call. = FALSE)
  }
}

# Function to clean and structure extracted text
parse_snow_data <- function(text_data) {
  # Split text into lines
  lines <- unlist(strsplit(text_data, "\n"))

  # Find the starting index where data begins (first occurrence of "Athabasca River")
  start_index <- which(grepl("Athabasca River", lines))[1]

  # If no match is found, return an empty dataframe
  if (is.na(start_index)) {
    cat("No data table found in this document.\n")
    return(data.frame())
  }

  # Keep only relevant lines from the start index onward
  lines <- lines[start_index:length(lines)]

  # Remove header and footer lines
  lines <- lines[!grepl("WISKI - Snow Course|Page \\d+ of \\d+|Report Parameters", lines)]

  # Trim whitespace
  lines <- trimws(lines)

  # Long station names run into the station ID in the PDF text (e.g. "Mount Odlum - EPA (SW deriv05BL812").
  # Put a gap before the ID so it splits into its own column
  lines <- gsub("(\\S)(\\d{2}[A-Z]{2}\\d{3})(\\s{2,})", "\\1  \\2\\3", lines)

  # Define column names
  columns <- c(
    "Basin/SubBasin", "Station Name", "Station ID", "Snow Parameter",
    "Survey/Reading Date", "Survey/Reading Value", "Survey/Reading Last Year",
    "Historical Minimum", "Historical Minimum Year", "Historical Maximum",
    "Historical Maximum Year", "Historical Average", "Years of Data for Month", "% of Avg"
  )

  # Extract only lines containing actual data (start with a known basin name)
  data_lines <- lines[grepl("River|Creek|Lake|Pass|Snow", lines)]

  # Split by multiple spaces to extract fields. Keep lines with enough fields to match expected columns
  fields <- strsplit(data_lines, "\\s{2,}")
  complete <- lengths(fields) >= length(columns)
  skipped <- data_lines[!complete & !grepl("Survey /|Reading", data_lines)] #column header lines are expected to be skipped
  if (length(skipped) > 0) {
    cat(length(skipped), "line(s) skipped because they didn't split into", length(columns), "columns. Check these against the PDF:\n")
    cat(skipped, sep = "\n")
  }

  df <- as.data.frame(do.call(rbind, lapply(fields[complete], function(x) x[1:length(columns)])), stringsAsFactors = FALSE)
  colnames(df) <- columns

  # Convert columns to appropriate types
  df[df == "---"] <- NA # Replace missing values with NA
  df$`Survey/Reading Date` <- as.Date(df$`Survey/Reading Date`, format = "%Y-%m-%d")
  df[6:14] <- lapply(df[6:14], as.numeric)

  return(df)
}

# Extract and parse each PDF
snow_data <- lapply(pdf_names, function(f) {
  cat("\n", f, ":\n", sep = "")
  df <- parse_snow_data(pdf_text(paste0(ab_path, f)))
  cat(nrow(df), "rows extracted\n")
  df
})

# Save to Excel
output_file <- paste0(ab_path, report_month, "_snow_data_", report_year, ".xlsx")
write_xlsx(snow_data, path = output_file)

# Print completion message
cat("\nData extracted and saved to", output_file, "\n")
