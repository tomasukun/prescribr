#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import readr
#' @import purrr

build_prescriber_year <- R6::R6Class(
  'build_prescriber_year',
  
  public = list(
    
    year = NULL,
    source_file_dir = '~/Dropbox/physician_payments/raw_source_data/',
    drug_folder = 'PartD_Prescriber_PUF_NPI_DRUG',
    phys_folder = 'PartD_Prescriber_PUF_NPI',
    partd_drug_source = dplyr::data_frame(),
    partd_phys_source = dplyr::data_frame(),
    
    exclusion_criteria = list(
      md = 'MD|md|DO|do',
      claim_count = 20
    ),
    
    
    initialize = function(yr = '2014') {
      self$year <- yr
    },
    
    read_source_tables = function() {
      self$partd_drug_source = readr::read_delim(
        paste0(self$source_file_dir, self$drug_folder, '/', self$year,
               '/PartD_Prescriber_PUF_NPI_Drug.txt'),
        delim = '\t')
      
      self$partd_phys_source = readr::read_delim(
        paste0(self$source_file_dir, self$phys_folder, '/', self$year, 
               '/PartD_Prescriber_PUF_NPI.txt'),
        delim = '\t')
    },
    
    process_tables = function() {
      return()
    },
    
    save_processed_tables = function() {
      return()
    }
      
  )
)