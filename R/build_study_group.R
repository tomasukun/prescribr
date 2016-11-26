#' Builder Class for Drug Class Study Groups
#' @import dplyr
#' @importFrom plyr mapvalues
#' @import stringr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import lubridate
#' @importFrom Kmisc kLoad

build_study_group <- R6::R6Class(
  'build_study_group',
  inherit = study_pop_library,
  
  public = list(
    year = NULL,
    drug_class = c('statins', 'beta_blockers'),
    
    initialize = function(yr = '2014', drug_class = '') {
      self$year <- yr
      self$drug_class <- drug_class
    },
    
    read_processed_tables = function() {
      
    }
      
      
  )
)

