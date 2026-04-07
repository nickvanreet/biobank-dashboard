# ==============================================================================
# STARTUP / HOME SCREEN MODULE
# ==============================================================================
# Full-page landing panel with clickable biobank cards.
# Clicking a card triggers data loading via the data_manager and navigates
# to the Sample Overview tab.
# ==============================================================================

# Per-site accent colours (used for top card border + icon fallback colour)
.site_colours <- c(
  lsd  = "#3498DB",   # blue   — Mbuji-Mayi
  ipp  = "#27AE60",   # green  — Bangui
  inrb = "#E67E22",   # orange — Kinshasa
  itm  = "#2C3E50"    # dark   — Antwerp
)

mod_startup_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Home",
    icon  = icon("home"),
    value = "startup_home",

    # ── Hover / active CSS ──────────────────────────────────────────────────
    tags$style(HTML("
      .site-card {
        cursor: pointer;
        transition: transform 0.22s ease, box-shadow 0.22s ease;
      }
      .site-card:hover {
        transform: translateY(-7px);
        box-shadow: 0 14px 32px rgba(0,0,0,0.14) !important;
      }
      .startup-header img { max-height: 72px; object-fit: contain; }
    ")),

    div(
      class = "container py-5",

      # ── Header ─────────────────────────────────────────────────────────────
      div(
        class = "text-center mb-5 startup-header",
        tags$img(
          src   = "logos/itm.png",
          id    = "startup-itm-logo",
          class = "mb-3 d-block mx-auto",
          style = "max-height:72px; object-fit:contain;",
          onerror = "document.getElementById('startup-itm-logo').style.display='none'"
        ),
        h1(config$app$title, class = "display-5 fw-bold"),
        p(
          class = "lead text-muted mb-0",
          icon("map-marker-alt"), " ", config$app$institution
        ),
        tags$small(
          class = "text-muted",
          paste0("v", config$app$version)
        ),
        tags$hr(class = "w-25 mx-auto my-4")
      ),

      # ── Site cards ─────────────────────────────────────────────────────────
      h4(
        "Select a Biobank to get started",
        class = "text-center text-muted fw-normal mb-4"
      ),

      div(
        class = "row g-4 justify-content-center",

        if (!is.null(config$sites)) {
          lapply(names(config$sites), function(site_id) {
            site  <- config$sites[[site_id]]
            colour <- .site_colours[site_id]
            if (is.na(colour)) colour <- "#6c757d"

            div(
              class = "col-12 col-sm-6 col-lg-3",
              div(
                class = "card h-100 border-0 shadow-sm site-card",

                # ── Card body ───────────────────────────────────────────────
                div(
                  class = "card-body text-center p-4",
                  style = paste0("border-top: 5px solid ", colour, ";"),

                  # Logo (PNG) with icon fallback
                  div(
                    class = "mb-3",
                    style = "height: 72px; display:flex; align-items:center; justify-content:center;",
                    tags$img(
                      id    = paste0("site-logo-", site_id),
                      src   = paste0("logos/", site_id, ".png"),
                      style = "max-height:68px; max-width:140px; object-fit:contain;",
                      onerror = paste0(
                        "document.getElementById('site-logo-", site_id, "').style.display='none';",
                        "document.getElementById('site-icon-", site_id, "').style.display='inline-block';"
                      )
                    ),
                    tags$span(
                      id    = paste0("site-icon-", site_id),
                      style = paste0(
                        "display:none; font-size:2.8rem; color:", colour, ";"
                      ),
                      icon("hospital")
                    )
                  ),

                  h4(site$short_name, class = "fw-bold mb-1"),
                  p(site$name, class = "text-muted small mb-2 lh-sm"),
                  div(
                    class = "d-flex justify-content-center align-items-center gap-2 text-muted",
                    tags$small(icon("map-marker-alt"), " ", site$location),
                    tags$small("|"),
                    tags$small(icon("building"), " ", site$institution)
                  )
                ),

                # ── Card footer ─────────────────────────────────────────────
                div(
                  class = "card-footer bg-white border-0 text-center pb-4 pt-0",
                  actionButton(
                    ns(paste0("load_", site_id)),
                    label = tagList(icon("database"), paste0(" Load ", site$short_name)),
                    class = "btn-primary px-4"
                  ),
                  div(class = "mt-2", uiOutput(ns(paste0("badge_", site_id))))
                )
              )
            )
          })
        }
      ),

      # ── Footer ─────────────────────────────────────────────────────────────
      div(
        class = "text-center mt-5 pt-3 text-muted border-top",
        tags$small(
          icon("code-branch"), " ",
          config$app$title, " v", config$app$version,
          " — ", config$app$institution
        )
      )
    )
  )
}


mod_startup_server <- function(id, current_site) {
  moduleServer(id, function(input, output, session) {

    selected_site <- reactiveVal(NULL)

    if (!is.null(config$sites)) {
      lapply(names(config$sites), function(site_id) {

        # "Active" badge when this site is the loaded one
        output[[paste0("badge_", site_id)]] <- renderUI({
          cs <- if (is.reactive(current_site)) current_site() else current_site
          if (isTRUE(identical(cs, site_id))) {
            tags$span(class = "badge bg-success", icon("check"), " Active")
          } else {
            tags$span()  # empty placeholder keeps layout stable
          }
        })

        # Load button observer
        observeEvent(input[[paste0("load_", site_id)]], {
          selected_site(site_id)
        })
      })
    }

    return(selected_site)
  })
}
