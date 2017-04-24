### simulation info module [PARENT] --------------------------------------------
simInfoUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(width = 12,
           tabPanel("Module Diagram", simModuleDiagramUI(ns("moduleDiagram"))),
           tabPanel("Object Diagram", simObjectDiagramUI(ns("objectDiagram"))),
           tabPanel("Event Diagram", simEventDiagramUI(ns("eventDiagram")))
    )
  )
}

simInfo <- function(input, output, session, sim) {
  callModule(simModuleDiagram, "moduleDiagram", sim)
  callModule(simObjectDiagram, "objectDiagram", sim)
  callModule(simEventDiagram, "eventDiagram", sim)
}

## module diagram --------------------------------------------------------------
simModuleDiagramUI <- function(id) {
  ns <- NS(id)

  ui_output <- tagList()

  ui_output$diagramTitle <- h3("Dependency graph (simplified)")

  ui_output$diagramDescription <- p(paste(
    "A network diagram illustrating the simplified module",
    "dependencies of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."))

  ui_output$diagram <- imageOutput(ns("modDiag"), height = 750)

  return(ui_output)
}

simModuleDiagram <- function(input, output, session, sim) {
  output$modDiag <- renderPlot({
    moduleDiagram(sim, vertex.size = 30)
  })
}

## object diagram --------------------------------------------------------------
simObjectDiagramUI <- function(id) {
  ns <- NS(id)

  ui_output <- tagList()

  ui_output$title <- h3("Summary of the data objects shared among modules.")

  ui_output$description <- p(paste(
    "A sequence diagram illustrating the data object dependencies",
    "of a simulation.",
    "Arrows between modules indicate at least one data object",
    "passed from one module to the other."
  ))

  ui_output$diagram <- DiagrammeR::DiagrammeROutput(ns("objectDiagram"), height = 1500)

  return(ui_output)
}

simObjectDiagram <- function(input, output, session, sim) {
  output$objectDiagram <- DiagrammeR::renderDiagrammeR({
    objectDiagram(sim)
  })
}


## event diagram ---------------------------------------------------------------
simEventDiagramUI <- function(id) {
  ns <- NS(id)

  out <- tagList()

  out$title <-  h3("Summary of the simulation event sequence.")

  out$description <- p(paste(
    "Simulation time is presented on the x-axis.",
    "Each module appears in a color-coded row,",
    "within which each event for that module is displayed",
    "corresponding to the sequence of events for that module.",
    "Note that only the start time of the event is meaningful is",
    "this figure:",
    "the width of the bar associated with a particular module's",
    "event DOES NOT correspond to an event's 'duration'."
  ))

  out$diagram <- DiagrammeR::DiagrammeROutput(ns("eventDiagram"), height = 1500)

  return(out)
}

simEventDiagram <- function(input, output, session, sim) {
  output$eventDiagram <- DiagrammeR::renderDiagrammeR({
    eventDiagram(sim)
  })
}

### detailed module info -------------------------------------------------------
moduleInfoUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("allModuleInfo"))
}

moduleInfo <- function(input, output, session, sim) {
  output$allModuleInfo <- renderUI({
    fluidRow(
      tagList(lapply(modules(sim), function(module) {
        m <- slot(depends(sim), "dependencies")[[module]]
        rmdFile <- file.path(modulePath(sim), module, paste0(module, ".Rmd"))
        box(title = module, width = 12, status = "success", collapsible = TRUE,
            div(
              p(paste("Description:", slot(m, "description"))),
              p(paste("Keywords:", paste(slot(m, "keywords"), collapse = ", "))),
              p(paste("Authors:", paste(slot(m, "authors"), collapse = "; "))),
              p(paste("Version:", slot(m, "version"))),
              p("Documentation:", actionLink(paste0(module, "_Rmd"), "Rmd"))
              ## TO DO: add more metadata as required
            ),
            bsModal(module, basename(rmdFile),
                    trigger = paste0(module, "_Rmd"), size = "large",
                    includeMarkdown(rmdFile))
        )
      }))
    )
  })
}
