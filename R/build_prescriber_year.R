#' Builder Class for Part D medicare prescribing data by year
#' @import dplyr
#' @import stringr

build_prescriber_year <- R6::R6Class(
  'build_prescriber_year',
  inherit = study_pop_library,
  public = list(

    year = NULL,
    drug_folder = 'PartD_Prescriber_PUF_NPI_DRUG',
    phys_folder = 'PartD_Prescriber_PUF_NPI',
    phys_comp_folder = 'Physician_Compare',
    partd_drug_source = dplyr::data_frame(),
    partd_phys_source = dplyr::data_frame(),
    partd_combined = dplyr::data_frame(),
    phys_compare_source = dplyr::data_frame(),
    doc_specialty = NULL,

    initialize = function(yr = '2014') {
      self$year <- yr
      self$doc_specialty <- self$doc_specialty_categories()
    },

    read_source_tables = function() {
      message('Reading Part D Drug File')
      private$read_partd_drug_file()

      message('Reading Part D Physician File')
      private$read_partd_physician_file()

      message('Reading Physician Compare File')
      private$read_physician_compare_file()
    },

    filter_tables = function() {
      message('Filtering Physicians by inclusion criteria')

      exclude_specialites <- self$doc_specialty %>%
        filter(specialty_category == 'EXCLUDE') %>%
        .$doc_specialty

      # filtering MDs or DOs
      self$partd_phys_source <- self$partd_phys_source %>%
        filter(str_detect(doc_cred, self$exclusion_criteria$doc)) %>%
        distinct(NPI, .keep_all = TRUE)
      self$study_pop[[paste0('study_', self$year)]]$partd_docs <- nrow(self$partd_phys_source)
      # filtering specialties to exclude
      self$partd_phys_source <- self$partd_phys_source %>%
        filter(!(doc_specialty %in% exclude_specialites))
      self$study_pop[[paste0('study_', self$year)]]$partd_specialties_keep <- nrow(self$partd_phys_source)
      # filtering docs in US
      self$partd_phys_source <- self$partd_phys_source %>%
        filter(doc_state %in% self$exclusion_criteria$states)
      self$study_pop[[paste0('study_', self$year)]]$partd_us_docs <- nrow(self$partd_phys_source)
      # filtering docs with valid brand count
      self$partd_phys_source <- self$partd_phys_source %>%
        inner_join(self$phys_compare_source, by = 'NPI')
      self$study_pop[[paste0('study_', self$year)]]$phys_comp_docs <- nrow(self$partd_phys_source)
      # filtering docs with identical matching criteria
      self$partd_phys_source <- self$partd_phys_source %>%
        group_by(doc_last_name, doc_first_name,
                 doc_city, doc_state) %>%
        mutate(dup_count = n()) %>%
        filter(dup_count == 1) %>%
        ungroup()
      self$study_pop[[paste0('study_', self$year)]]$unq_match_crit_partd_docs <- nrow(self$partd_phys_source)
    },

    merge_partd_phys_drugs = function() {
      message('Merging Physician file with Part D Drug file')

      self$partd_combined <- self$partd_phys_source %>%
        left_join(self$partd_drug_source, by = 'NPI')
    },

    save_processed_tables = function() {
      message('Saving processed prescriber file')

      study_pop <- self$study_pop
      partd_combined <- self$partd_combined
      partd_docs <- self$partd_phys_source
      save(study_pop, file = 'data/study_pop.rData')
      save(partd_docs, file = paste0(self$processed_file_dir, self$year,'/partd_docs.rData'))
      save(partd_combined, file = paste0(self$processed_file_dir, self$year,'/partd_combined.rData'))
    },

    build_tables = function() {
      self$read_source_tables()
      self$filter_tables()
      self$merge_partd_phys_drugs()
      self$save_processed_tables()
    }
  ),
  private = list(
    read_partd_drug_file = function() {
      self$partd_drug_source = readr::read_delim(
        paste0(self$source_file_dir, self$drug_folder, '/', self$year,
               '/PartD_Prescriber_PUF_NPI_Drug.txt'), delim = '\t')

      names(self$partd_drug_source) <- toupper(names(self$partd_drug_source))

      self$partd_drug_source = self$partd_drug_source %>%
        select(
          NPI,
          doc_drug_brand_name = DRUG_NAME,
          doc_drug_generic_name = GENERIC_NAME,
          doc_drug_bene_count = BENE_COUNT,
          doc_drug_total_claims = TOTAL_CLAIM_COUNT,
          doc_drug_total_day_supply = TOTAL_DAY_SUPPLY,
          doc_drug_total_drug_cost = TOTAL_DRUG_COST,
          doc_drug_bene_count_65 = BENE_COUNT_GE65,
          doc_drug_total_claims_65 = TOTAL_CLAIM_COUNT_GE65,
          doc_drug_total_day_supply_65 = TOTAL_DAY_SUPPLY_GE65,
          doc_drug_total_drug_cost_65 = TOTAL_DRUG_COST_GE65
        )
    },

    read_partd_physician_file = function() {
      self$partd_phys_source = readr::read_delim(
        paste0(self$source_file_dir, self$phys_folder, '/', self$year,
               '/PartD_Prescriber_PUF_NPI.txt'), delim = '\t')

      names(self$partd_phys_source) <- toupper(names(self$partd_phys_source))

      self$partd_phys_source = self$partd_phys_source %>%
        select(
          doc_zip = starts_with('NPPES_PROVIDER_ZIP5'),
          NPI,
          doc_specialty = starts_with('SPECIALTY_DESC'),
          doc_city = NPPES_PROVIDER_CITY,
          doc_last_name = NPPES_PROVIDER_LAST_ORG_NAME,
          doc_first_name = NPPES_PROVIDER_FIRST_NAME,
          doc_mi = NPPES_PROVIDER_MI,
          doc_cred = NPPES_CREDENTIALS,
          doc_gender = NPPES_PROVIDER_GENDER,
          doc_state = NPPES_PROVIDER_STATE,
          doc_bene_count = BENE_COUNT,
          doc_total_claims = TOTAL_CLAIM_COUNT,
          doc_total_drug_cost = TOTAL_DRUG_COST,
          doc_total_day_supply = TOTAL_DAY_SUPPLY,
          doc_bene_count_65 = BENE_COUNT_GE65,
          doc_total_claims_65 = TOTAL_CLAIM_COUNT_GE65,
          doc_total_drug_cost_65 = TOTAL_DRUG_COST_GE65,
          doc_total_day_supply_65 = TOTAL_DAY_SUPPLY_GE65,
          doc_brand_claims = BRAND_CLAIM_COUNT,
          doc_brand_cost = BRAND_DRUG_COST,
          doc_generic_claims = GENERIC_CLAIM_COUNT,
          doc_generic_cost = GENERIC_DRUG_COST,
          doc_other_count = OTHER_CLAIM_COUNT,
          doc_other_cost = OTHER_DRUG_COST,
          doc_mapd_claims = MAPD_CLAIM_COUNT,
          doc_mapd_cost = MAPD_DRUG_COST,
          doc_lis_claims = LIS_CLAIM_COUNT,
          doc_lis_cost = LIS_DRUG_COST
        )
    },

    read_physician_compare_file = function() {
      self$phys_compare_source = readr::read_csv(
        paste0(self$source_file_dir, self$phys_comp_folder, '/2014',
               '/Physician_Compare_National_Downloadable_File.csv'), col_names = TRUE) %>%
        distinct(NPI, .keep_all = TRUE) %>%
        select(NPI,
               doc_grad_year = `Graduation year`,
               doc_group_size = `Number of Group Practice members`)
    }
  )
)