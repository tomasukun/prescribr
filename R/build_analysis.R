#' Analysis class
#' @import dplyr
#' @importFrom plyr mapvalues
#' @import stringr
#' @import tidyr

build_analysis <- R6::R6Class(
  'build_analysis',
  inherit = build_changer_data,
  public = list(
    drug_class = '',
    type = '',
    analysis_data = data_frame(),
    
    initialize = function(data, class, type = NULL) {
      self$analysis_data <- data
      self$type <- type
      self$drug_class <- class
    },
    
    build_analysis = function() {
      
    }
  )
)