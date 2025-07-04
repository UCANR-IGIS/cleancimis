% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc_stndrze_csv.R
\name{cc_stndrze_csv}
\alias{cc_stndrze_csv}
\title{Standardize hourly CSVs and flag suspect values}
\usage{
cc_stndrze_csv(
  year,
  data_dir = Sys.getenv("CC_DATADIR"),
  proj_subdir = "my_project_area",
  overwrite = FALSE
)
}
\arguments{
\item{year}{Year(s), integer}

\item{data_dir}{The data directory}

\item{proj_subdir}{Project subdirectory}

\item{overwrite}{Overwrite existing files, logical}
}
\description{
Standardize CSVs and flag suspect values
}
\details{
This will import the stacked CSV files (created with cc_stack_hly_yr and cc_stack_hly_mth),
standardize the column names and data types, convert any values that CIMIS flagged with a quality code
to NAs, and save as RDS files. These "gappy" data will then be the input into the next function.
}
