# R/core/rbac.R
# Role-Based Access Control (RBAC) for Biobank Dashboard
# Provides authentication hooks and authorization framework
# ==============================================================================

suppressPackageStartupMessages({
  library(digest)
})

# ==============================================================================
# RBAC CONFIGURATION
# ==============================================================================

# Access levels (hierarchical)
.access_levels <- list(
  viewer = 1,
  analyst = 2,
  editor = 3,
  admin = 4
)

# Module permissions by access level
.module_permissions <- list(
  viewer = c(
    "data_quality",
    "overview_assays",
    "overview_demographics",
    "geographic",
    "transport",
    "sample_journey"
  ),
  analyst = c(
    "data_quality",
    "overview_assays",
    "overview_demographics",
    "geographic",
    "transport",
    "extractions",
    "mic",
    "elisa_pe",
    "elisa_vsg",
    "ielisa",
    "concordance",
    "drs_rnasep",
    "sample_journey",
    "sample_processing",
    "predictive_analytics"
  ),
  editor = c(
    # All analyst permissions plus:
    "data_import",
    "cache_management",
    "export_all"
  ),
  admin = c(
    # All permissions
    "user_management",
    "config_management",
    "audit_logs"
  )
)

# Data permissions by access level
.data_permissions <- list(
  viewer = c("view_aggregates", "export_summary"),
  analyst = c("view_aggregates", "view_details", "export_summary", "export_details"),
  editor = c("view_aggregates", "view_details", "export_summary", "export_details", "import_data"),
  admin = c("view_aggregates", "view_details", "export_summary", "export_details", "import_data", "delete_data", "manage_cache")
)

# RBAC state environment
.rbac_env <- new.env(parent = emptyenv())
.rbac_env$enabled <- FALSE
.rbac_env$current_user <- NULL
.rbac_env$auth_provider <- NULL
.rbac_env$session_timeout <- 3600  # 1 hour default

# ==============================================================================
# CONFIGURATION FUNCTIONS
# ==============================================================================

#' Enable RBAC for the application
#'
#' @param auth_provider Authentication provider ("local", "ldap", "oauth", "shinymanager")
#' @param session_timeout Session timeout in seconds
#' @param config Additional configuration options
#' @export
enable_rbac <- function(
  auth_provider = "local",
  session_timeout = 3600,
  config = list()
) {
  .rbac_env$enabled <- TRUE
  .rbac_env$auth_provider <- auth_provider
  .rbac_env$session_timeout <- session_timeout
  .rbac_env$config <- config

  message(sprintf("RBAC enabled with %s authentication", auth_provider))
  invisible(TRUE)
}

#' Disable RBAC (allows anonymous access)
#'
#' @export
disable_rbac <- function() {
  .rbac_env$enabled <- FALSE
  .rbac_env$current_user <- NULL
  message("RBAC disabled - anonymous access allowed")
  invisible(TRUE)
}

#' Check if RBAC is enabled
#'
#' @return Boolean
#' @export
is_rbac_enabled <- function() {
  .rbac_env$enabled
}

# ==============================================================================
# USER MANAGEMENT
# ==============================================================================

#' User structure
#'
#' @param user_id User identifier
#' @param username Display name
#' @param role User role (viewer, analyst, editor, admin)
#' @param email User email (optional)
#' @param groups Additional group memberships
#' @return User object (list)
#' @export
create_user <- function(user_id, username, role = "viewer", email = NULL, groups = NULL) {
  if (!role %in% names(.access_levels)) {
    stop(sprintf("Invalid role: %s. Must be one of: %s",
                 role, paste(names(.access_levels), collapse = ", ")))
  }

  list(
    user_id = user_id,
    username = username,
    role = role,
    access_level = .access_levels[[role]],
    email = email,
    groups = groups,
    created_at = Sys.time(),
    last_login = NULL
  )
}

#' Set the current user for the session
#'
#' @param user User object from create_user()
#' @export
set_current_user <- function(user) {
  if (!is.list(user) || is.null(user$user_id)) {
    stop("Invalid user object")
  }

  user$last_login <- Sys.time()
  .rbac_env$current_user <- user

  # Log the login
  if (exists("log_user_action")) {
    log_user_action("login", user_id = user$user_id)
  }

  invisible(TRUE)
}

#' Get the current user
#'
#' @return User object or NULL if not authenticated
#' @export
get_current_user <- function() {
  .rbac_env$current_user
}

#' Clear the current user (logout)
#'
#' @export
clear_current_user <- function() {
  user <- .rbac_env$current_user

  if (!is.null(user) && exists("log_user_action")) {
    log_user_action("logout", user_id = user$user_id)
  }

  .rbac_env$current_user <- NULL
  invisible(TRUE)
}

# ==============================================================================
# AUTHORIZATION FUNCTIONS
# ==============================================================================

#' Check if current user has required access level
#'
#' @param required_level Minimum required level (viewer, analyst, editor, admin)
#' @return Boolean
#' @export
has_access_level <- function(required_level) {
  # If RBAC disabled, allow all
  if (!.rbac_env$enabled) {
    return(TRUE)
  }

  user <- get_current_user()

  # No user = no access
  if (is.null(user)) {
    return(FALSE)
  }

  # Check level
  required <- .access_levels[[required_level]]
  current <- user$access_level

  current >= required
}

#' Check if current user can access a module
#'
#' @param module_id Module identifier
#' @return Boolean
#' @export
can_access_module <- function(module_id) {
  # If RBAC disabled, allow all
  if (!.rbac_env$enabled) {
    return(TRUE)
  }

  user <- get_current_user()

  if (is.null(user)) {
    return(FALSE)
  }

  # Get permissions for user's role
  role <- user$role

  # Admin can access everything
  if (role == "admin") {
    return(TRUE)
  }

  # Check module permissions
  allowed_modules <- .module_permissions[[role]]

  # Also include permissions from lower levels
  for (level in names(.access_levels)) {
    if (.access_levels[[level]] <= user$access_level) {
      allowed_modules <- unique(c(allowed_modules, .module_permissions[[level]]))
    }
  }

  module_id %in% allowed_modules
}

#' Check if current user has a specific data permission
#'
#' @param permission Permission name
#' @return Boolean
#' @export
has_data_permission <- function(permission) {
  # If RBAC disabled, allow all
  if (!.rbac_env$enabled) {
    return(TRUE)
  }

  user <- get_current_user()

  if (is.null(user)) {
    return(FALSE)
  }

  role <- user$role

  # Admin has all permissions
  if (role == "admin") {
    return(TRUE)
  }

  # Check data permissions for role
  allowed <- .data_permissions[[role]]

  permission %in% allowed
}

#' Require specific access level, stopping if not met
#'
#' @param required_level Required access level
#' @param message Custom error message
#' @export
require_access <- function(required_level, message = NULL) {
  if (!has_access_level(required_level)) {
    msg <- message %||% sprintf(
      "Access denied: requires %s level or higher",
      required_level
    )

    # Log access denial
    user <- get_current_user()
    if (exists("log_warn")) {
      log_warn(
        sprintf("Access denied: %s required", required_level),
        module = "rbac",
        user_id = user$user_id,
        required_level = required_level
      )
    }

    stop(msg, call. = FALSE)
  }

  invisible(TRUE)
}

# ==============================================================================
# SHINY INTEGRATION
# ==============================================================================

#' Create UI wrapper that conditionally shows based on access
#'
#' @param ui UI element to conditionally display
#' @param required_level Minimum required access level
#' @param fallback UI to show if access denied (default: NULL)
#' @return Conditional UI
#' @export
rbac_ui <- function(ui, required_level = "viewer", fallback = NULL) {
  if (has_access_level(required_level)) {
    ui
  } else {
    fallback
  }
}

#' Wrap server logic with access check
#'
#' @param expr Server expression
#' @param required_level Required access level
#' @param module Module name (for logging)
#' @export
rbac_server <- function(expr, required_level = "viewer", module = NULL) {
  if (!has_access_level(required_level)) {
    if (exists("log_warn")) {
      user <- get_current_user()
      log_warn(
        sprintf("Blocked server action requiring %s access", required_level),
        module = module %||% "rbac",
        user_id = user$user_id
      )
    }
    return(NULL)
  }

  expr
}

#' Create authentication gate for Shiny app
#'
#' This function can be used to wrap the main app UI/server
#' to require authentication before access.
#'
#' @param app_ui Main app UI
#' @param app_server Main app server
#' @param login_page Custom login page UI (optional)
#' @return List with wrapped ui and server functions
#' @export
create_auth_gate <- function(app_ui, app_server, login_page = NULL) {
  # Default login page
  if (is.null(login_page)) {
    login_page <- shiny::fluidPage(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .login-container {
            max-width: 400px;
            margin: 100px auto;
            padding: 20px;
            border: 1px solid #ddd;
            border-radius: 8px;
            background: white;
          }
          .login-title {
            text-align: center;
            margin-bottom: 20px;
          }
        "))
      ),
      shiny::div(
        class = "login-container",
        shiny::h2("Biobank Dashboard", class = "login-title"),
        shiny::textInput("username", "Username"),
        shiny::passwordInput("password", "Password"),
        shiny::actionButton("login_btn", "Login", class = "btn-primary btn-block"),
        shiny::br(),
        shiny::textOutput("login_error")
      )
    )
  }

  # Wrapped UI
  wrapped_ui <- function(request) {
    if (!is_rbac_enabled()) {
      return(app_ui)
    }

    user <- get_current_user()

    if (is.null(user)) {
      login_page
    } else {
      app_ui
    }
  }

  # Wrapped server
  wrapped_server <- function(input, output, session) {
    if (!is_rbac_enabled()) {
      app_server(input, output, session)
      return()
    }

    # Check authentication state
    user <- shiny::reactiveVal(get_current_user())

    # Login handler
    shiny::observeEvent(input$login_btn, {
      # This is a placeholder - actual authentication would go here
      # In production, use shinymanager, shinyauthr, or custom auth

      # Demo: accept any non-empty credentials
      if (nchar(input$username) > 0 && nchar(input$password) > 0) {
        new_user <- create_user(
          user_id = input$username,
          username = input$username,
          role = "analyst"  # Default role
        )
        set_current_user(new_user)
        user(new_user)
        shiny::updateTextInput(session, "password", value = "")
        session$reload()
      } else {
        output$login_error <- shiny::renderText("Invalid credentials")
      }
    })

    # If authenticated, run main server
    shiny::observe({
      if (!is.null(user())) {
        app_server(input, output, session)
      }
    })

    # Logout handler (add to main app)
    shiny::observeEvent(input$logout_btn, {
      clear_current_user()
      session$reload()
    })
  }

  list(ui = wrapped_ui, server = wrapped_server)
}

# ==============================================================================
# AUDIT LOGGING
# ==============================================================================

#' Log an access event for audit trail
#'
#' @param action Action performed
#' @param resource Resource accessed
#' @param result Success or failure
#' @param details Additional details
#' @export
log_access_event <- function(action, resource, result = "success", details = NULL) {
  user <- get_current_user()
  user_id <- if (!is.null(user)) user$user_id else "anonymous"

  # If app_logger is available, use it
  if (exists("log_info")) {
    log_info(
      sprintf("Access: %s on %s - %s", action, resource, result),
      module = "audit",
      user_id = user_id,
      action = action,
      resource = resource,
      result = result,
      details = details
    )
  }

  # Also write to audit-specific file if configured
  if (!is.null(.rbac_env$config$audit_file)) {
    entry <- list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      user_id = user_id,
      action = action,
      resource = resource,
      result = result,
      details = details
    )

    cat(
      jsonlite::toJSON(entry, auto_unbox = TRUE),
      "\n",
      file = .rbac_env$config$audit_file,
      append = TRUE
    )
  }

  invisible(TRUE)
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get list of modules accessible to current user
#'
#' @return Character vector of module IDs
#' @export
get_accessible_modules <- function() {
  if (!.rbac_env$enabled) {
    # Return all modules if RBAC disabled
    return(unique(unlist(.module_permissions)))
  }

  user <- get_current_user()

  if (is.null(user)) {
    return(character(0))
  }

  # Collect all modules user can access
  modules <- character(0)
  for (level in names(.access_levels)) {
    if (.access_levels[[level]] <= user$access_level) {
      modules <- unique(c(modules, .module_permissions[[level]]))
    }
  }

  modules
}

#' Get current user's role name
#'
#' @return Role name or "anonymous"
#' @export
get_current_role <- function() {
  user <- get_current_user()
  if (is.null(user)) "anonymous" else user$role
}

#' Check if current session is still valid
#'
#' @return Boolean
#' @export
is_session_valid <- function() {
  user <- get_current_user()

  if (is.null(user)) {
    return(FALSE)
  }

  # Check session timeout
  if (!is.null(user$last_login)) {
    elapsed <- as.numeric(difftime(Sys.time(), user$last_login, units = "secs"))
    if (elapsed > .rbac_env$session_timeout) {
      clear_current_user()
      return(FALSE)
    }
  }

  TRUE
}

# ==============================================================================
# NULL-COALESCING OPERATOR
# ==============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
