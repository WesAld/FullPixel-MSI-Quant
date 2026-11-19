# Centralized presentation routing for the MALDIPixelQuant V4 shell.

fpq_v4_routes <- function() c(
  "home", "project", "msi_samples", "mzmine", "dataset_configuration",
  "validation", "workflow", "runs", "results_internal", "artifacts",
  "logs", "provenance", "scientific_validation_details", "about"
)

fpq_v4_standard_routes <- function() c(
  "home", "project", "msi_samples", "mzmine", "dataset_configuration",
  "validation", "workflow", "runs", "about"
)

fpq_v4_advanced_routes <- function() c(
  "artifacts", "logs", "provenance", "scientific_validation_details"
)

fpq_v4_route_child <- function(route) switch(route,
  project_create = list(route = "project", tab = "Create"),
  project_open = list(route = "project", tab = "Open"),
  project_overview = list(route = "project", tab = "Overview"),
  experimental_design = list(route = "dataset_configuration",
                             tab = "Experimental Design"),
  files_columns = list(route = "dataset_configuration",
                       tab = "Files and Columns"),
  list(route = route, tab = NULL)
)

fpq_v4_navigate <- function(session, route, child = NULL) {
  target <- fpq_v4_route_child(child %||% route)
  if (!target$route %in% fpq_v4_routes())
    fpq_stop("ui.v4_route", paste("Unknown V4 route:", target$route),
             "configuration")
  shiny::updateSelectInput(session, "v4_route", selected = target$route)
  session$sendCustomMessage("fpq-v4-route", list(route = target$route))
  if (identical(target$route, "project") && !is.null(target$tab))
    shiny::updateTabsetPanel(session, "project-project_tabs",
                             selected = target$tab)
  if (identical(target$route, "dataset_configuration") && !is.null(target$tab))
    shiny::updateTabsetPanel(session, "dataset_configuration_tabs",
                             selected = target$tab)
  invisible(target$route)
}

fpq_v4_nav_item <- function(ns, route, label, icon) {
  htmltools::tags$div(
    class = paste("fpq-v4-nav-item", if (route == "home") "active" else ""),
    `data-route` = route,
    shiny::actionLink(ns(paste0("nav_", route)),
      label = htmltools::tagList(shiny::icon(icon),
                                 htmltools::tags$span(label)))
  )
}

fpq_v4_bind_navigation <- function(input, session, state) {
  standard <- fpq_v4_standard_routes()
  advanced <- fpq_v4_advanced_routes()
  for (route in c(standard, advanced)) {
    local({
      target <- route
      shiny::observeEvent(input[[paste0("nav_", target)]], {
        if (target %in% advanced && !isTRUE(state$advanced_mode())) return()
        fpq_v4_navigate(session, target)
      })
    })
  }
}
