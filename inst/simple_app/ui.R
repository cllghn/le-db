shinyUI(
  fluidPage(
    theme = shinytheme("yeti"),
    titlePanel("Network Explorer"),
    sidebarLayout(
      sidebarPanel(
        textInput(
          inputId = "pid",
          label   = "Enter PID(s) of interest (if multiple, separate with commas)",
          value   = ""
        ),
        tags$br(),
        fluidRow(
          align = "center",
          actionButton(
            inputId = "submit",
            label   = "Submit")
          ),
        tags$br(),
        fluidRow(
          uiOutput("download_ui")
        ),
        tags$hr(),
        fluidRow(
          tags$p(tags$b("About:"),
                 "This application was created by Christopher Callaghan as part of a larger discussion on conducting social network analysis with portable databases. All code can be found on GitHub,",
                 tags$a("by clicking here.", href="https://github.com/cjcallag/le-db"))
        )
        ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Network",
                             visNetworkOutput("visplot")),
                    tabPanel("Edges",
                             DT::dataTableOutput("edges")),
                    tabPanel("Nodes",
                             DT::dataTableOutput("nodes"))
                    )
        )
      )
    )
  )
