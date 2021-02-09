library(shiny)
library(shinythemes)
library(DiagrammeR)
fluidPage(
  theme = shinytheme("united"),
  #theme = "common.css",
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
               "body {padding-top: 55px;}"),

             tabPanel(icon("home"),
                      fluidRow(
                        tags$img(src="Rocket.jpg",width="100%",height="300px")
                      ),
            
                      # General info.
                      tabPanel(
                        "Overview",
                        tags$h1("Scope"),
                        tags$p(HTML("Multi-criteria decision making which is a sub-discipline of operations research explicitly evaluates multiple conflicting criteria in decision making. The consistency-driven pairwise comparisons method is a valuable tool for solving multi-criteria decision making problems. It is a powerful inference tool that could be used for knowledge acquisition for the knowledge management system. In fact, pairwise comparisons are basics for practically the entire science and have been used for projects of national importance.")),
                        tags$h1("Approach"),
                        tags$p(HTML("This project had implemented an application basing the consistency-driven pairwise comparisons method. This application, RConcluder, is not only a standalone system but also could be used as a supplement of any expert or knowledge management system.")),
                        tags$h1("Keywords"),
                        tags$ul(
                          tags$li(HTML("Multi-criteria Decision Making(MCDM)")),
                          tags$li(HTML("Consistency-driven Pairwise comparisons Method")),
                          tags$li(HTML("Inconsistency analysis"))
                        ),
                        tags$h1("How to use it?"),
                        tags$p(HTML("Check tutorial video at <a href=\"https://youtube.com/\">https://youtube.com/</a>."))
                        ,
                        tags$h1("GitHub"),
                        tags$p(HTML("Source code is available at <a href=\"https://github.com/lillian5120/RConcluder\">https://github.com/lillian5120/RConcluder</a>."))
                      )
             ),
           #  tabPanel("New",
           #             HTML(
           #               '<input type="button" value="Add Row" onclick="addRow(\'dataTable\')" /> 
           #               <input type="button" value="Delete Row" onclick="deleteRow(\'dataTable\')" />
           #               <table id="dataTable" width="350px" border="1">
           #               <tr>
           #               <td><INPUT type="checkbox" name="chk"/></td>
           #               <td> 1 </td>
           #               <td> <INPUT type="text" /> </td>
           #               <td> <INPUT type="text" /> </td>
           #               <td> <INPUT type="button" value="add to tree"/> </td>
           #               </tr>
           #               </table>'
           #             )
           #           ),
             tabPanel("Model Analysis",
                        sidebarPanel(
                          tags$h4("Import your model"),
                          tags$hr(),
                          uiOutput("file1"),
                          tags$h5("Do not know how to begin? Start from example data"),
                          downloadLink("downloadData", "Example Model File Download"),
                          # Horizontal line ----
                          tags$hr(),
                          actionButton("reset_tree","Reset Tree", icon("refresh"),style="margin-right:40px;"),
                                       #style="color: #1E7BDB; background-color: #337ab7; border-color: #2e6da4"),
                          tags$hr(),
                          actionButton("reset_tree_weight","Reset Tree Weight", icon("refresh"),style="margin-right:40px;"),
                          , width = 2
                                       #style="color: #1E7BDB; background-color: #337ab7; border-color: #2e6da4")
                          
                        ),
                        mainPanel(
                          wellPanel(
                          #style = "background: white",
                          uiOutput("add_child_ui"),
                          #titlePanel(h1("Tree model",style={'background-color:grey;'})),
                          titlePanel(h4("Tree model")),
                          wellPanel(
                            style = "background: white",
                            grVizOutput("xx") 
                          )
                          ,uiOutput("show_matrix") 
                          )
                        )
                      ),
             navbarMenu(title="More",
                        tabPanel("About",
                                 tags$h1("Background Info"),
                                 tags$p(HTML(" Multi-criteria decision making (MCDM) is a handy method to apply to many complex decisions. Especially for solving problems that are characterized as a choice among alternatives. It has all the characteristics of a helpful decision support tool which are helping us focus on what is important, logical and consistent, and easy to use.")),
                                 tags$p(HTML("  Pairwise comparisons(PC) is widely recognized as a useful and effective method for supporting the MCDM process based on subjective judgments. When the relative importance of these items can not be rated directly, the benefit of using it for building a global ranking from binary comparisons become apparent. The consistency-driven pairwise comparisons(CDPC) method is a primary technique for MCDM. It provides an elegant means by comparing, ranking, quantizing and offering methods for the identification and resolution of inconsistencies. It is based on an inconsistency index, Koczkodaj inconsistency indicator, which is proposed and named after prof. W.W. Koczkodaj (in 1993) and its use as a validation technique.")),
                                 tags$p(HTML("  Hierarchical pairwise comparisons belong to the MCDM. A hierarchical structure reduces complexity (in terms of a number of pairwise comparisons) from $O(n^2)$ to close to $n*log(n)$, which is a major step forward. 
Users should be prompted to enter entity names and asked to group them in not more than 8.")),
                                 
                                  ),
                        tabPanel("Version","Current Version is V0.1.")
                        )
  )
)