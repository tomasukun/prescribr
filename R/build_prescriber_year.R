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
    phys_comp_folder = 'Physician_Compare',
    partd_drug_source = dplyr::data_frame(),
    partd_phys_source = dplyr::data_frame(),
    phys_compare_source = dplyr::data_frame(),
    
    exclusion_criteria = list(
      doc = 'M.?D.?|D.?O.?',
      claim_count = 100,
      states = c(state.abb, 'DC'),
      grad_year_limits = c(1910, 2013)
    ),
    study_drugs = list(     
      statins = stringr::str_c('LIVALO', 'CRESTOR', 'LIPITOR', 'ZOCOR',
                  'LESCOL', 'LESCOL XL', 'ALTOPREV', 'MEVACOR',
                  'PRAVACHOL', 'ATORVASTATIN CALCIUM', 'FLUVASTATIN SODIUM',
                  'LOVASTATIN', 'PRAVASTATIN SODIUM', 'SIMVASTATIN', sep = '|')
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
      
      self$phys_compare_source = readr::read_csv(
        paste0(self$source_file_dir, self$phys_comp_folder, '/', self$year, 
               '/Physician_Compare_National_Downloadable_File.csv'), 
        col_names = FALSE
      )
    },
    
    process_tables = function() {
      self$partd_phys_source <- self$partd_phys_source %>% 
        filter(TOTAL_CLAIM_COUNT < self$exclusion_criteria$claim_count,
               str_detect(NPPES_CREDENTIALS, self$exclusion_criteria$doc),
               BRAND_CLAIM_COUNT > 0,
               !is.na(BRAND_CLAIM_COUNT),
               NPPES_PROVIDER_STATE %in% self$exclusion_criteria$states) %>% 
        group_by(NPPES_PROVIDER_LAST_ORG_NAME, NPPES_PROVIDER_FIRST_NAME,
                 NPPES_PROVIDER_CITY, NPPES_PROVIDER_STATE) %>% 
        mutate(dup_count = n()) %>% 
        filter(dup_count == 0) %>% 
        ungroup()
      
      self$partd_drug_source <- self$partd_drug_source %>% 
        filter(str_detect(DRUG_NAME, study_drugs$statins))
        
        
    },
    
    save_processed_tables = function() {
      return()
    },
    
    build_source = function() {
      self$read_source_tables()
      self$process_tables()
      self$save_processed_tables()
    }
      
  )
)