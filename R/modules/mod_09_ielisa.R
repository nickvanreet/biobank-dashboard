# R/modules/mod_09_ielisa.R
# iELISA module wrapper
# Uses the iELISA coordinator with iELISA-specific configuration

#' iELISA UI
#' @param id Module namespace ID
#' @export
mod_ielisa_ui <- function(id) {
  mod_ielisa_coordinator_ui(
    id = id,
    label = "iELISA"
  )
}

#' iELISA Server
#' @param id Module namespace ID
#' @param biobank_df Reactive returning biobank data frame (optional)
#' @param filters Reactive returning global filters (optional)
#' @export
mod_ielisa_server <- function(id, biobank_df = reactive(NULL), filters = reactive(NULL)) {
  mod_ielisa_coordinator_server(
    id = id,
    biobank_df = biobank_df,
    filters = filters
  )
}
