# ELISA-VSG module wrappers

mod_elisa_vsg_ui <- function(id) {
  mod_elisa_generic_ui(id, "ELISA-VSG")
}

mod_elisa_vsg_server <- function(id, elisa_data) {
  mod_elisa_generic_server(id, "ELISA_vsg", elisa_data)
}
