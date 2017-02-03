#' Builder Class for Open Payments data by year
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import readr
#' @import purrr
#' @importFrom Kmisc kLoad

build_openpay_year <- R6::R6Class(
  'build_openpay_year',
  inherit = study_pop_library,
  public = list(
    
    year = NULL,
    openpay_folder = 'Open_Payments',
    openpay_source = dplyr::data_frame(),
    openpay_docs = dplyr::data_frame(),
    
    initialize = function(yr = '2014') {
      self$year <- yr
    },
    
    read_source_tables = function() {
      self$openpay_source <- readr::read_csv(
        paste0(self$source_file_dir, self$openpay_folder, '/',
               self$year, '/OP_DTL_GNRL_PGYR', self$year, '_P06302016.csv')
        ) %>% 
        filter(Dispute_Status_for_Publication == 'No',
               Third_Party_Payment_Recipient_Indicator == 'No Third Party Payment') %>% 
        select(doc_id = Physician_Profile_ID,
               doc_last_name = Physician_Last_Name,
               doc_first_name = Physician_First_Name,
               doc_mi = Physician_Middle_Name,
               doc_city = Recipient_City,
               doc_state = Recipient_State,
               doc_zip = Recipient_Zip_Code,
               drug_manufacturer = Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name,
               payment_dollars = Total_Amount_of_Payment_USDollars,
               payment_date = Date_of_Payment,
               payment_number = Number_of_Payments_Included_in_Total_Amount,
               payment_type = Nature_of_Payment_or_Transfer_of_Value,
               payment_type_context = Contextual_Information,
               payment_drug_1 = Name_of_Associated_Covered_Drug_or_Biological1,
               payment_drug_2 = Name_of_Associated_Covered_Drug_or_Biological2,
               payment_drug_3 = Name_of_Associated_Covered_Drug_or_Biological3,
               payment_drug_4 = Name_of_Associated_Covered_Drug_or_Biological4,
               payment_drug_5 = Name_of_Associated_Covered_Drug_or_Biological5,
               payment_device_1 = Name_of_Associated_Covered_Device_or_Medical_Supply1,
               payment_device_2 = Name_of_Associated_Covered_Device_or_Medical_Supply2,
               payment_device_3 = Name_of_Associated_Covered_Device_or_Medical_Supply3,
               payment_device_4 = Name_of_Associated_Covered_Device_or_Medical_Supply4,
               payment_device_5 = Name_of_Associated_Covered_Device_or_Medical_Supply5) %>% 
        mutate(doc_mi = str_sub(doc_mi, 1, 1),
               doc_zip = str_sub(doc_zip, 1, 5),
               payment_date = lubridate::mdy(payment_date))
    },
    
    process_source_tables = function() {
      self$openpay_docs <- self$openpay_source %>%
        group_by(doc_id) %>% 
        mutate(total_payment_dollars = sum(payment_dollars, na.rm = TRUE)) %>% 
        select(doc_id, doc_last_name, doc_first_name, doc_city, doc_state, doc_zip,
               total_payment_dollars) %>% 
        distinct(doc_id, .keep_all = TRUE)
      study_pop <- Kmisc::kLoad('data/study_pop.rData')
      study_pop[[paste0('study_', self$year)]]$openpay_docs <- nrow(self$openpay_docs)
    },
    
    save_processed_tables = function() {
      save(study_pop, file = 'data/study_pop.rData')
      openpay <- self$openpay_source
      openpay_docs <- self$openpay_docs
      save(openpay, file = paste0(self$processed_file_dir, self$year,'/open_payments.rData'))
      save(openpay_docs, file = paste0(self$processed_file_dir, self$year,'/open_payments_docs.rData'))
    },
    
    build_tables = function() {
      self$read_source_tables()
      self$process_source_tables()
      self$save_processed_tables()
    }
    
  )
)
    
