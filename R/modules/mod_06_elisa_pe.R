# ELISA-PE module wrappers

mod_elisa_pe_ui <- function(id) {
  mod_elisa_generic_ui(id, "ELISA-PE")
}

mod_elisa_pe_server <- function(id, elisa_data) {
  mod_elisa_generic_server(id, "ELISA_pe", elisa_data)
}
