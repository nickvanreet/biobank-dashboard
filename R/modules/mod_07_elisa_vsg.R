# R/modules/mod_07_elisa_vsg.R
# ELISA-VSG module wrapper
# Uses the generic ELISA coordinator with VSG-specific configuration

#' ELISA-VSG UI
#' @param id Module namespace ID
#' @export
mod_elisa_vsg_ui <- function(id) {
  mod_elisa_coordinator_ui(
    id = id,
    label = "ELISA-VSG",
    elisa_type = "ELISA_vsg"
  )
}

#' ELISA-VSG Server
#' @param id Module namespace ID
#' @param biobank_df Reactive returning biobank data frame
#' @param filters Reactive returning global filters (optional)
#' @export
mod_elisa_vsg_server <- function(id, biobank_df = reactive(NULL), filters = reactive(NULL)) {
  mod_elisa_coordinator_server(
    id = id,
    elisa_type = "ELISA_vsg",
    biobank_df = biobank_df,
    filters = filters
  )
}
