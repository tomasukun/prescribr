#' Library Class to hold exlusion criteria, study population numbers, and other descriptive data
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import readr
#' @import purrr

study_pop_library <- R6::R6Class(
  'study_pop_library',
  
  public = list(
    
    source_file_dir = '~/Dropbox/physician_payments/raw_source_data/',
    processed_file_dir = '~/Dropbox/physician_payments/processed_source_data/',
    
    study_pop = list(partd_docs = NULL,
                     openpay_docs = NULL,
                     partd_us_docs = NULL,
                     valid_brand_docs = NULL,
                     phys_compare_docs = NULL,
                     unq_match_crit_partd_docs = NULL,
                     med_grad_exclude_docs = NULL,
                     unq_match_crit_op_docs = NULL,
                     final_study_docs = NULL),
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
    )
    
  )
)