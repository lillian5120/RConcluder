library(shiny)
library(shinythemes)
fluidPage(
  theme = shinytheme("cerulean"),
  # import script file
  tags$script(src = "script.js"),
  
  #titlePanel("RConcluder : A simple tool for complex decision making."),
  #actionButton(inputId="clicks",label = "Click me"),
  navbarPage(
             title="RConcluder",
             id = "navBar",
             collapsible = TRUE,
             inverse = TRUE,
             position = "fixed-top",
             header = tags$style(
               ".navbar-right {
                       float: right !important;
                       }",
               "body {padding-top: 75px;}"),

             tabPanel(icon("home"),
                      fluidRow(
                        tags$img(src="Rocket.png",width="100%",height=" 300px")
                      ),
            
                      # General info.
                      tabPanel(
                        "Overview",
                        tags$h1("Scope"),
                        tags$p(HTML("This collection of visualizations addresses the question, \"What is it like to live in the Lego world?\"  In other words, if you're Wyldstyle, what kinds of people do you meet?  How are they feeling?  What plants and animals do you find around you?")),
                        tags$p(HTML("Think of each theme as an island on the Lego planet.  Each visualization can be faceted by theme, so you can compare fashion, flora and fauna, etc. across themes.")),
                        tags$h1("Approach"),
                        tags$p(HTML("Parts are labeled and categorized using three main sources of information:")),
                        tags$ul(
                          tags$li(HTML("The part category (e.g., \"Minifig Heads\" or \"Plants and Animals\") specified in the database")),
                          tags$li(HTML("The hexadecimal part color specified in the database")),
                          tags$li(HTML("Keywords in the part name"))
                        ),
                        tags$p(HTML("The keywords that map part names to categories involve more-or-less hand-curated lists and some <i>very</i> basic text processing (mostly regular expressions).  The process is <b>not 100% accurate</b>; there are plenty of false positives and false negatives.  But it's good enough for a first pass.")),
                        tags$h1("GitHub"),
                        tags$p(HTML("Source code is available at <a href=\"https://github.com/\">https://github.com/</a>."))
                      )
                      
                      
                      
                      
                      
                      
             ),
             tabPanel("New",
                        HTML(
                          '<input type="button" value="Add Row" onclick="addRow(\'dataTable\')" /> 
                          <input type="button" value="Delete Row" onclick="deleteRow(\'dataTable\')" />
                          <table id="dataTable" width="350px" border="1">
                          <tr>
                          <td><INPUT type="checkbox" name="chk"/></td>
                          <td> 1 </td>
                          <td> <INPUT type="text" /> </td>
                          <td> <INPUT type="text" /> </td>
                          <td> <INPUT type="button" value="add to tree"/> </td>
                          </tr>
                          </table>'
                        ),
                      uiOutput("add_child_ui"),
                      grVizOutput("xx") 
                      ,uiOutput("show_matrix") 
                      
                      ),
             tabPanel("Open"),
             tabPanel("save"),
             tabPanel("add"),
             tabPanel("delete"),
             navbarMenu(title="More",
                        tabPanel("About","This is a simple tool for complex decision making."),
                        tabPanel("Version","Current Version is V0.1.")
                        )
  )
)