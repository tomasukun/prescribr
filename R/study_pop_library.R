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
    shared_docs_dir = '~/Dropbox/OP2 Documents/',
    
    doc_specialty_categories = suppressMessages(
      readr::read_csv('spec/partd_doc_specialty_tabulation_2014.csv')),
    
    study_pop = list(
      study_2013 = list(
        partd_docs = NULL,
        openpay_docs = NULL,
        partd_specialties_keep = NULL,
        partd_us_docs = NULL,
        valid_brand_docs = NULL,
        phys_comp_docs = NULL,
        unq_match_crit_partd_docs = NULL,
        med_grad_exclude_docs = NULL,
        unq_match_crit_op_docs = NULL,
        final_study_docs = NULL
        ),
      study_2014 = list(
        partd_docs = NULL,
        openpay_docs = NULL,
        partd_specialties_keep = NULL,
        partd_us_docs = NULL,
        valid_brand_docs = NULL,
        phys_comp_docs = NULL,
        unq_match_crit_partd_docs = NULL,
        med_grad_exclude_docs = NULL,
        unq_match_crit_op_docs = NULL,
        final_study_docs = NULL
      )
    ),
    
    study_group_pop = list(
      statins_2013 = list(
        claim_count_100plus = NULL,
        meal_payment = NULL,
        tagged_payment = NULL
      ),
      statins_2014 = list(
        claim_count_100plus = NULL,
        meal_payment = NULL,
        tagged_payment = NULL
      )
    ),
    
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
                               'LOVASTATIN', 'PRAVASTATIN SODIUM', 'SIMVASTATIN', sep = '|'),
      anti_mrsa = stringr::str_c('CLEOCIN HCL', 'CLINDAMYCIN HCL', 'CLEOCIN PALMITATE',
                                 'CLINDAMYCIN PALMITATE HCL', 'CLINDAMYCIN PEDIATRIC', 'BACTRIM',
                                 'SULFAMETHOXAZOLE-TRIMETHOPRIM', 'BACTRIM DS', 'SULFATRIM',
                                 'ZYVOX', 'SIVEXTRO', 'VIBRAMYCIN', 'DOXY 100', 'DORYX', 'DOXYCYCLINE HYCLATE',
                                 'MORGIDOX', 'ORACEA', 'DOXYCYCLINE MONOHYDRATE', 'MINOCIN', 'SOLODYN', 
                                 'MINOCYCLINE HCL', sep = '|'),
      opthalmic_corticosteroid = stringr::str_c('MAXIDEX', 'DEXAMETHASONE SODIUM PHOSPHATE', 'DUREZOL', 
                                                'FLUOROMETHOLONE', 'FML FORTE', 'FML', 'FML S.O.P', 
                                                'FLAREX', 'LOTEMAX', 'PRED FORTE', 'PRED MILD', 
                                                'PREDNISOLONE ACETATE', 'OMNIPRED', 'PREDNISOLONE SODIUM PHOSPHATE',
                                                'VEXOL'),
      osteoporosis = stringr::str_c('ALENDRONATE SODIUM', 'BINOSTO', 'FOSAMAX', 'FOSAMAX PLUS D',
                                    'IBANDRONATE SODIUM', 'BONIVA', 'ATELVIA', 'ACTONEL', 
                                    'RISEDRONATE SODIUM', 'ZOLEDRONIC ACID', 'ZOMETA', 'RECLAST',
                                    'PROLIA', 'FORTEO', 'RALOXIFENE HCL', 'EVISTA', sep = '|')
    ),
    
    open_payments_target = c(
      'crestor' = 'CRESTOR',
      'zyvox' = 'ZYVOX',
      'durezol' = 'DUREZOL',
      'prolia' = 'PROLIA'
    ),
    
    target_drug_manufacturer = c(
      'crestor' = 'AstraZeneca Pharmaceuticals LP',
      'zyvox' = 'Pfizer',
      'durezol' = 'Alcon',
      'prolia' = 'Amgen Inc.|Medtronic Sofamor Danek USA, Inc.'
    )
  )
)