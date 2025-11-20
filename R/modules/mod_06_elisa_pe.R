# R/modules/mod_06_elisa_pe.R
# ELISA-PE module wrapper
# Uses the generic ELISA coordinator with PE-specific configuration

#' ELISA-PE UI
#' @param id Module namespace ID
#' @export
mod_elisa_pe_ui <- function(id) {
  mod_elisa_coordinator_ui(
    id = id,
    label = "ELISA-PE",
    elisa_type = "ELISA_pe"
  )
}

#' ELISA-PE Server
#' @param id Module namespace ID
#' @param biobank_df Reactive returning biobank data frame
#' @param filters Reactive returning global filters (optional)
#' @export
mod_elisa_pe_server <- function(id, biobank_df = reactive(NULL), filters = reactive(NULL)) {
  mod_elisa_coordinator_server(
    id = id,
    elisa_type = "ELISA_pe",
    biobank_df = biobank_df,
    filters = filters
  )
}
