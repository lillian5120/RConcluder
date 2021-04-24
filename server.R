#Shiny is an R package that makes it easy to build interactive web apps straight from R.
library(shiny)
#Data.tree is an R package that create tree structures from hierarchical data, and traverse the tree in various orders.
library(data.tree)
#DiagrammeR is an R package that allows you to create flowcharts, diagrams, and grhs with Markdown-like text
library(DiagrammeR)
# shinyMatrix provides you with an editable matrix input field for shiny apps.
library(shinyMatrix)
#The kableExtra package is designed to extend the basic functionality of tables
library(kableExtra)
#The goal of 'readr' is to provide a fast and friendly way to read rectangular data (like 'csv', 'tsv', and 'fwf'). 
library(readr)
#ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics.
library(ggplot2)

function(input, output,session){
  #Create an object for storing reactive values
  #Usage:https://shiny.rstudio.com/reference/shiny/0.11/reactiveValues.html
  vv <- reactiveValues(org=NULL,names=NULL,weight=NULL)
  
  ########## the World Health Organization’s Quality of Life Index ExampleData downloading ##########
  #Please check the paper with title "Assessing the Properties of the World Health Organization’s Quality of Life Index" for further explanation
  #WHOQOL stands for World Health Organization Quality of Life
  #SR stands for social relationships
  #PH stands for physical health
  #ENV stands for environment 
  #PSYCH stands for psychological health
  #p_rel stands for personal relationship
  exampleData <- data.frame(
    #Parent nodes list
    from = c(
      "WHOQOL", "WHOQOL", "WHOQOL", "WHOQOL", "WHOQOL", "SR", "SR", "SR"
    ),
    #Child nodes list
    to = c(
      "PH", "SR", "ENV1", "ENV2", "PSYCH", "p_rel", "social","sex"
    ),
    # Weight of each node.
    weight = c(
      31.57, 18.8, 19.87, 19.87, 9.93, 10.96, 4.35, 3.45
    )
  )

  #Function: downloadHandler
  #Description: Save example data to CSV file and download it
  #Arguments: filename - A string of the filename
  #           content - A function that takes a single argument file that is a file path (string) of a nonexistent temp file, and writes the content to that file path. (Reactive values and functions may be used from this function.)
  #Usage: https://shiny.rstudio.com/reference/shiny/0.14/downloadHandler.html
  output$downloadData <- downloadHandler(
    filename = function() {
      # 'ExampleData-' append with current system date and suffix with CSV extension.
      paste("ExampleData-", Sys.Date(), ".csv", sep="")
    },
    #Writing CSV file
    content = function(file) {
      write_csv(exampleData, file)
    }
  )
  
  
  #Function: observe
  #Description: Create a reactive observer, 
  #             Here, for creating the  hierarchical tree with the imported data or initial data
  #usage:https://shiny.rstudio.com/reference/shiny/0.14/observe.html
  observe({
    #Render the import file UI
    output$importFile <- renderUI({
      fileInput('importFile', label = NULL)
    })
    
    #Get the input file name
    inFile <- input$importFile
    
    #If it does not start importing module data, then building the hierarchical tree with the root node first.
    if (is.null(inFile)){
      #set initial weight for root 100
      vv$org <- Node$new("Root",weight=100) 
      # get node names of hierarchical tree
      vv$names=vv$org$Get('name') 
      #initial hierarchical tree
      output$xx=renderGrViz({
        # set the style of tree graph
        #graphs are laid out from top to bottom ("TB")#
        SetGraphStyle(vv$org, rankdir = "TB")
        #arrow style in VEE -> shape, with grey color and width of 2 #
        SetEdgeStyle(vv$org, arrowhead = "vee", color = "grey", penwidth = 2)
        #setting the node style, filled and rounded with color pink and helvetica font #
        SetNodeStyle(vv$org, style = "filled,rounded", shape = "polygon", fillcolor = "pink", fontname = "helvetica", tooltip = getNodeTip, label = getNodeLabel)
        #grViz() function renders graphs with the standard dot layout. The dot layout flows the directed graph in the direction of rank (i.e., downstream nodes of the same rank are #aligned).
        grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
      })
      
    }else{
      #build the hierarchical tree with the imported csv file
      
      #read csv module data
      useRdf <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
      
      #convert data frame to Node
      useRtree <- FromDataFrameNetwork(useRdf)
      
      #initial the weight of root node to 100
      rootNode <- FindNode(useRtree,name=useRtree$root$name)
      rootNode$weight = 100
      
      #generate hierarchical tree
      output$xx=renderGrViz({
        # set the style of tree graph
        #graphs are laid out from top to bottom ("TB")#
        SetGraphStyle(useRtree, rankdir = "TB")
        #arrow style in VEE -> shape, with grey color and width of 2 #
        SetEdgeStyle(useRtree, arrowhead = "vee", color = "grey", penwidth = 2)
        #setting the node style, filled and rounded with color pink and helvetica font #
        SetNodeStyle(useRtree, style = "filled,rounded", shape = "polygon", fillcolor = "pink", fontname = "helvetica", tooltip = getNodeTip, label = getNodeLabel)
        #grViz() function renders graphs with the standard dot layout. The dot layout flows the directed graph in the direction of rank (i.e., downstream nodes of the same rank are #aligned).
        grViz(generate_dot(ToDiagrammeRGraph(useRtree)),engine = "dot")
      })
      
      vv$org <- useRtree 
      vv$names <- vv$org$Get('name') # get names of main tree
      
      # Clear "Pairwise Comparisons Matrix" section result
      output$show_matrix <- NULL
    }
  })   
  

  #Function: renderUI
  #Description: Renders reactive HTML using the Shiny UI library.Here for initialing moduel edit UI 
  #usage:https://shiny.rstudio.com/reference/shiny/1.6.0/renderUI.html
  output$edit_module_ui <- renderUI({
    fluidRow(
      style = "position:relative",
      column(3,
             #Add Node Section on UI
             style = "position:relative",
             wellPanel(
               tags$h4(tags$b("Add Node")),
               tags$hr(),
               #Drop Down for Parent_node name
               selectInput("Parent_name","Parent_name",vv$names),
               #NEW_NAME for parent node
               div(id='my_textinput', textInput ('new_node_name', label = "New_name*", value = "",placeholder = "--Enter name--")),
               #Button to add the node
               actionButton("add_child","Add"),
               tags$hr(),
               textOutput("add_error_msg"),
               #ERROR Message to display
               tags$head(tags$style("#add_error_msg{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"))
             )),
      
      column(
        3,
        style = "position:relative",
        wellPanel(
          #Change the node name section in UI
          tags$h4(tags$b("Change Node Name")),
          tags$hr(),
          #Drop Down for change_node name
          selectInput("Name_to_change","Name_to_change",vv$names),
          #NEW_NAME for the changing node
          div(id='my_textinput', textInput ('new_name', label = "New_name*", value = "",placeholder = "--Enter name--")),
          tags$head(tags$style(type="text/css", "#my_textinput {color: red}")),
          #Button to change the node name
          actionButton("change_name","Change"),
          tags$hr(),
          textOutput("update_error_msg"),
          # ERROR Message to display
          tags$head(tags$style("#update_error_msg{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"))
        )),
      
      column(3,
             style = "position:relative",
             wellPanel(
               #Delete the node section in UI
               tags$h4(tags$b("Delete Node")),
               tags$hr(),
               #Drop down to select the node to delete
               selectInput("Name_to_delete","Name_to_delete",vv$names),
               helpText(strong("Attention: The children of this node will be removed too!")),
               tags$br(),
               #Button to confirm the delete action
               actionButton("remove_node","Remove"),
               tags$hr()
             )),
      
      column(3,
             style = "position:relative",
             wellPanel(
               #Section in UI to perform Analysis
               tags$h4(tags$b("Analysis")),
               tags$hr(),
               #Default Tolerant level for inconsistency set to 0.33 and can increase 0.1 each time and can be increase maximum upto 0.1
               numericInput("eps","Inconsistency Tolerance Level",0.33, min = 0, max = 1, step = 0.1),
               #Dropdown which node to be selected for analysis.
               selectInput("Parent_node_name","Parent_node_name",vv$names),
               #Button to confirm the action for analysis.
               actionButton("analysis","Analysis"),
               tags$hr(),
               textOutput("error_msg"),
               #ERROR Message to display
               tags$head(tags$style("#error_msg{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"))
             ))
    )
  })
  
  
  #Function: updateTreeWeights
  #Description: update the node weight for the hierarchical tree
  #Arguments: Tree - the hierarchical tree
  #Return: Tree with updated weight
  updateTreeWeights <- function(Tree){
    height <- Tree$height
    #Level 1 one is assigned to nodeName
    nodeName <- Tree$Get('name',filterFun= function(x) x$level == 1)
    #Level 1 nodeName is now assigned as rootNode
    rootNode <- FindNode(Tree,name=nodeName)
    #rootNode weight is set to 100
    rootNode$weight <- 100
    
    for(i in 1:height){
      nodeName <- Tree$Get('name',filterFun= function(x) x$level == i)
      for(j in nodeName){
        #finding the node children
        nodeObj  <- FindNode(Tree,name = j)
        childrenNum <- length(nodeObj$children)
        #Weight is distributed based on total children on a given node
        for(k in nodeObj$children){
          #Consider nodeObj$weight is 100 and it has total 3 children then k$weight will be 33.33
          k$weight = round(nodeObj$weight/childrenNum,2)
        }
      }
    }
  }
  
  #Function: getNodeLabel
  #Description: Get label for the node in the hierarchical tree
  #Arguments: node - the node in the hierarchical tree 
  #Return: label information
  getNodeLabel <- function(node){
    #Node is passed in the function and it's name and weight is assigned to label and returned from function.
    label = paste0(node$name, "\n",node$weight)
    return(label)
  }
  
  #Function: getNodeTip
  #Description: Get tip for the node in the hierarchical tree
  #Arguments: node - the node in the hierarchical tree 
  #Return: tip information
  getNodeTip<- function(node){
    #Node is passed in the function and it's returning the node weight.
    tip = node$weight
    return(tip)
  }
  
  #Function: observeEvent
  #Usage:https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  #Description: Oberserve event for reset hierarchical tree button
  observeEvent(input$reset_tree,{
    vv$org <- Node$new("Root",weight=100) #set initial weight for Root node with 100
    vv$names = vv$org$Get('name') # get node names of hierarchical tree
    
    #initial hierarchical tree
    output$xx=renderGrViz({
      #grViz() function renders graphs with the standard dot layout. The dot layout flows the directed graph in the direction of rank (i.e., downstream nodes of the same rank are #aligned).
      grViz(generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
    
    # Clear "Pairwise Comparisons Matrix" section result
    output$show_matrix <- NULL
  })
  
  #Function: observeEvent
  #Usage:https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  #Description: Observe event for resetting tree weight button 
  observeEvent(input$reset_tree_weight,{
    updateTreeWeights(vv$org)
    
    #refresh hierarchical tree
    output$xx=renderGrViz({
      #grViz() function renders graphs with the standard dot layout. The dot layout flows the directed graph in the direction of rank (i.e., downstream nodes of the same rank are #aligned).
      grViz(generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
    
    # Clear "Pairwise Comparisons Matrix" section result
    output$show_matrix <- NULL
    
  })
  
  #Function: observeEvent
  #Usage:https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  #Description: observeEvent for adding child node 
  observeEvent(input$add_child,{
    # valid check for node name
    if (input$new_node_name ==""){
      #Outputting the error if the nodename is empty
      output$add_error_msg <- renderText('New node name is empty!')
      return(FALSE)
    }else if(!is.null(FindNode(vv$org,name=input$new_node_name))){
      #Outputting the error if the nodename already exist
      output$add_error_msg <- renderText('The node name is already exist!')
      return(FALSE)
    }else{
      output$add_error_msg <- renderText("")
    }
    #Adding the child Node
    FindNode(node=vv$org,name = input$Parent_name)$AddChildNode(Node$new(input$new_node_name, weight=input$new_node_weight)) # add child
    
    vv$names=vv$org$Get('name')# get names of new tree
    #Weight are updated
    updateTreeWeights(vv$org)
    
    #re-generate hierarchical tree/ renders graphs
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
  })
  
  
  #Function: observeEvent
  #Usage:https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  #Description: observeEvent for changing hierarchical tree node name 
  observeEvent(input$change_name,{
    #valid check for name
    if (input$new_name ==""){
      #Outputting the error if the nodename is empty
      output$update_error_msg <- renderText('New node name is empty!')
      return(FALSE)
    }else if(!is.null(FindNode(vv$org,name=input$new_name))){
      #Outputting the error if the nodename already exist
      output$update_error_msg <- renderText('The node name is already exist!')
      return(FALSE)
    }else{
      output$update_error_msg <- renderText("")
    }
    #Below line will Find and change the name of an existing node if the above condition will not throw an error
    aa=FindNode(node=vv$org,name = input$Name_to_change) 
    aa$name=input$new_name # Changed name will be assigned
    
    vv$names=vv$org$Get('name')# get names of new tree
    
    #re-generate hierarchical tree  / renders graphs
    output$xx=renderGrViz({
      grViz(generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
  })
  
  #Function: observeEvent
  #Usage:https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  #Description: observeEvent for removing hierarchical tree node
  observeEvent(input$remove_node,{
    #Prune tree / Based on the selected node input, it will get deleted
    Prune(vv$org, function(x) x$name != input$Name_to_delete)
    vv$names=vv$org$Get('name') # get names of new tree
    #Tree weights are reupdated
    updateTreeWeights(vv$org)
    
    #re-generate hierarchical tree  / renders graphs for display
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
    
    # Clear "Pairwise Comparisons Matrix" section result
    output$show_matrix <- NULL
  })
  
  #Function: observeEvent
  #Usage:https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  #Description: observeEvent for analysis event 
  observeEvent(input$analysis,{
    
    eps <- input$eps
    #Selecting the node which we want to analyze
    aa = FindNode(node=vv$org,name = input$Parent_node_name) 
    
    #get the number of children of current node which we want to analyze
    n =length(aa$children)
    
    #If the selected node for analysis has less than 3 children then below code will execute and will throw the error.
    if(n < 3){
      errorMsg = "The selected node has to have more than 3 child nodes"
      output$error_msg <- renderText(errorMsg)
      # Clear "Pairwise Comparisons Matrix" section result
      output$show_matrix <- NULL
      return(FALSE)
    }else{
      #Else no error message
      errorMsg = ""
      output$error_msg <- renderText(errorMsg)
    }
    #Going to next level of the selected children node
    childrenLevel <- aa$level+1
    #Getting the node list of the selected children
    nodeList <- aa$Get('weight',filterFun= function(x) x$level == childrenLevel)
    
    #generate reciprecial matrix
    genMatrix <- generatePCMatrix(nodeList)
    #show Pairwise Comparisons Matrix section
    output$show_matrix=renderUI({
      wellPanel(
      #Title of the matrix after clicking the Analyse
      titlePanel(h4(strong("Pairwise Comparisons Matrix"))),
      fluidRow(
        #Property of the grid
        style = "position:relative",
        column(
          12,
          style = "position:relative",
          matrixInput(
            inputId = "matrix",
            value = genMatrix,
            class = "numeric",
            cols = list(
              names = TRUE,
              editableNames = TRUE
            ),
            rows = list(
              names = TRUE,
              editableNames = TRUE
            )
          ),
          helpText(strong("Guidance: Click the ratio you want to change in the upper triangle part of Pairwise Comparison Marix, and click the 'Update Value' button!")),
          actionButton("updateMatrix","Update Value",style="margin-top: 1.5%; margin-left: 80%; height: 40px;", icon = icon("refresh")),
          tags$hr()
        )
      ),
      #Title of the dashboard which displays the consistency matrix
      titlePanel(h4(strong("Dashboard"))),
      wellPanel(
      fluidRow(
        style = "position:relative",
        column(
          6,
          style = "position:relative",
            tableOutput("highlight_table"),
            h4(textOutput("nTriads")),
            tags$hr(),
            #Reduce Inconsistency button after Clicking the Analysis
            actionButton("reduceInconsistency"," Reduce Inconsistency",style="margin-top: 1.5%; margin-left: 5%; height: 40px;", icon = icon("balance-scale")),
            #Download Tree Model button after Clicking the Analysis  
            downloadButton("saveModule", "Download Tree Model",style= "margin-top: 1.5%; margin-left: 17%; height: 40px;")
        ),
        column(
          6,
          style = "position:relative",
            plotOutput("barChartPlot")
          )
        )
      )
      )
    })
    
    #find all the triads and calculate the Kii value for them then sort by Kii in descending order
    #store the return dataframe in df
    df <- findAllTriads(genMatrix)

    #Get the max Kii value which stored at the seventh value of the first row in df
    max_kii <- df[1,7]

    #Generate the notification at the bottom of dashboard section
    output$nTriads <- renderText({
      
      #Get the default inconsistency tolerance value
      eps <- input$eps
      
      if(max_kii > eps){
        paste("The max kii value is:", max_kii)
      }else{
        paste("Congras! you get a consistent PC Matrix with kii value:",max_kii)
      }
    })
    
    #Render the highlight table for the most inconsistent triad in the matrix of dashboard section
    output$highlight_table <- function() {
      # highlight condition
      if(max_kii > eps){
        #get the indexes of the triad with max Kii value
        i <- df[1,1]
        j <- df[1,2]
        k <- df[1,3]
        
        r <- nrow(genMatrix)
        #Matrix element will be highlighted in RED if there is inconsistency exist.
        genMatrix[(j-1)*r+i] <- cell_spec(genMatrix[i,j], color = "white", bold = T,background = "red")
        genMatrix[(k-1)*r+j] <- cell_spec(genMatrix[j,k], color = "white", bold = T,background = "red")
        genMatrix[(k-1)*r+i] <- cell_spec(genMatrix[i,k], color = "white", bold = T,background = "red")
      }
      # Render the matrix with highlight UI
      genMatrix %>%
        kbl(escape = F, align = "c") %>%
        kable_styling("striped", full_width = F) 
    }
    
    # get the number of triads  
    nTriads <- nrow(df)
    
    # get the triads number which Kii value is larger than the threshold
    nInconsisTriads <- nrow(subset(df,X7 > eps))

    # Render the bar chart in Dashboard section
    output$barChartPlot <- renderPlot({
      #Construct the data for bar chart
      df <- data.frame(
        group = c("Inconsistent Triads", "Consistent Triads"),
        value = c( nInconsisTriads, nTriads-nInconsisTriads)
      )
      
      #Plot the numbers of inconsistent and consistent triads based on group and value
      ggplot(df, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")+
        # Set the value in y axis from 0 to the number of triads , step is 1
        scale_y_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)
        ) +
        theme_minimal()
    })

  })

  #Function: generatePCMatrix
  #Description: generate the PC Matrix for the selected nodes
  #Arguments: nodeList - the children nodes list with name and weight of the selected nodes in the hierarchical tree 
  #           nodeList example : PH     SR    ENV1  ENV2  PSYCH 
  #                              31.57 18.80 19.87 19.87  9.93 
  #Return: the PC Matrix of the children nodes 
  generatePCMatrix<-function(nodeList){
    #initial the dimension value for PC matrix with 0
    dim <- 0
    #initial a weight list
    weight_elements <- c()
    
    #get the name of nodes
    index_names <- names(nodeList)
    
    #generate reciprecial matrix
    for(i in index_names){
      dim <- dim + 1
      #get the weight of nodes
      weight_elements[dim] <- nodeList[i]
    }
    #initial a elements list for pc matrix
    matrix_elements <- c()
    index <- 1
    # generate the element list of pc matrix by using the values in weight_elements
    for(i in weight_elements){
      for(j in weight_elements){
        matrix_elements[index] <- round(j/i,2)
        index <- index +1
      }
    }
    
    #Generate the recipical pc matrix
    pc_matrix <- matrix(data = matrix_elements, nrow = dim, ncol = dim)
    
    # Generate the rownames and colnames for pc matrix
    rownames(pc_matrix) <- index_names
    colnames(pc_matrix) <-index_names
    
    #return a recipical pc matrix
    return(pc_matrix)
  }
  
  #Function: observeEvent
  #Usage:https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  #Description: observeEvent for updating matrix button
  observeEvent(input$updateMatrix,{
    new_matrix <- input$matrix
    eps <- input$eps
    
    #Update reciprocal pairwise comparison matrix
    dim <- nrow(new_matrix)
    #loop the updated matrix and generate a recipical matrix for it
    for(r in 1:dim){
      for( c in 1:dim){
        if(c > r){
          #set the value on the lower triangle to the reciprocal of the corresponding value in the upper triangle
          new_matrix[c,r] <- round(1/new_matrix[r,c],2)
        }
        #set the value on the diagonal to 1
        if(c == r){
          new_matrix[c,r] <- 1
        }
      }
    }
    
    #Update the pc matrix at "Pairwise Comparisons Matrix" section with new_matrix
    updateMatrixInput(session,"matrix",new_matrix)
    
    #find all the triads and calculate the Kii value for them then sort by Kii in descending order
    #store the return dataframe in df
    df <- findAllTriads(new_matrix)
    #Get the maximun Kii value which stored at the seventh column of the first row
    max_kii <- df[1,7]
    
    #Generate the notification msg at the bottom of "Dashboard" section
    output$nTriads <- renderText({
      if(max_kii > eps){
        paste("The maximum kii value is:", max_kii)
      }else{
        paste("Congras! you get a consistent PC Matrix with kii value:", df[1,7])
      }
    })
    
    #Render the highlight table for the most inconsistent triad in the matrix of dashboard section
    output$highlight_table <- function(){
      # highlight condition
      if(max_kii > eps){
        #get the indexes of the triad which Kii value is larger than inconsistent threshold
        i <- df[1,1]
        j <- df[1,2]
        k <- df[1,3]
        
        #Matrix element will be highlighted in RED if there is inconsistency exist.
        r <- nrow(new_matrix)
        new_matrix[(j-1)*r+i] <- cell_spec(new_matrix[i,j], color = "white", bold = T,background = "red")
        new_matrix[(k-1)*r+j] <- cell_spec(new_matrix[j,k], color = "white", bold = T,background = "red")
        new_matrix[(k-1)*r+i] <- cell_spec(new_matrix[i,k], color = "white", bold = T,background = "red")
      }
      # Render the matrix with highlight UI
      new_matrix %>%
        kbl(escape = F, align = "c") %>%
        kable_styling("striped", full_width = F) 
    }
    
    # get the triads number 
    nTriads <- nrow(df)
    
    # get the triads number which kii larger than threshold
    nInconsisTriads <- nrow(subset(df,X7 > eps))

    # Generate the bar chart in the "Dashboard" section
    output$barChartPlot <- renderPlot({
      #Construct the data for bar chart
      df <- data.frame(
        group = c("Inconsistent Triads", "Consistent Triads"),
        value = c( nInconsisTriads, nTriads-nInconsisTriads)
      )
      #Plot the numbers of inconsistent and consistent triads based on group and value
      ggplot(df, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")+
        # Set the value in y axis from 0 to the number of triads , step is 1
        scale_y_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)
        ) +
        theme_minimal()
    })
    
  })
  
  #Function: observeEvent
  #Usage:https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  #Description: observeEvent for the "reduce inconsistency" button 
  observeEvent(input$reduceInconsistency,{
    #Get the pc matrix at "Pairwise Comparisons Matrix" section
    matrix <- input$matrix
    #Find all the triads and calculate the Kii value for them then sort by Kii in descending order
    #Store the return dataframe in df
    df <- findAllTriads(matrix)
    #Get the inconsistency tolerance threshold
    eps <- input$eps
    
    #Get the triads which kii value is larger than the threshold
    triads <- subset(df,X7 > eps)
    #Get the number of triads which kii value is larger than the threshold
    N <- nrow(triads)
    
    while(N !=0){
      #get the indexes of the triad
      i <- df[1,1]
      j <- df[1,2]
      k <- df[1,3]
      
      X <- df[1,4] #a_ij
      Y <- df[1,5] #a_ik
      Z <- df[1,6] #a_jk
      
      #improve the value
      new_x <- X^(2/3)*Z^(-1/3)*Y^(1/3)
      new_y <- X^(1/3)*Z^(1/3)*Y^(2/3)
      new_z <- X^(-1/3)*Z^(2/3)*Y^(1/3)
      
      
      #update the elements in the upper triangle of matrix using new_x,y,z
      matrix[i,j] <- round(new_x,2)
      matrix[i,k] <- round(new_y,2)
      matrix[j,k] <- round(new_z,2)
      #update the elements in the down triangle of matrix using 1/new_x,1/y,1/z
      matrix[j,i] <- round(1/new_x,2)
      matrix[k,i] <- round(1/new_y,2)
      matrix[k,j] <- round(1/new_z,2)
      
      
      #find all the triads and calculate the Kii value for them then sort by Kii in descending order
      #store the return dataframe in df
      df <- findAllTriads(matrix)
      #get the number of kii value is larger than eps
      subFrame <- subset(df,X7 > eps)
      N <- nrow(subFrame)
      paste("new N value is: ",N)
    }
    
    #Update the pc matrix at "Pairwise Comparisons Matrix" section with matrix
    updateMatrixInput(session,"matrix",matrix)
    #Generate the notification msg at the bottom of "Dashboard" section
    
    output$nTriads <- renderText({
      paste("Congras! you get a consistent PC Matrix with kii value:", df[1,7])
    })
    
    #Render the highlight table for the most inconsistent triad in the matrix of dashboard section
    output$highlight_table <- function() {
      #Render the matrix with highlight UI
      matrix %>%
        # https://www.rdocumentation.org/packages/kableExtra/versions/1.3.4/topics/kbl
        kbl(escape = F, align = "c") %>%
        # https://www.rdocumentation.org/packages/kableExtra/versions/1.3.4/topics/kable_styling
        kable_styling("striped", full_width = F) 
    }
    
    
    #Update the tree
    
    #Get the row name of pc matrix
    colIDs <- rownames(matrix)
    
    #get node by first element name
    node <- FindNode(node=vv$org,name = colIDs[1])
    
    #Get the weight of parent node
    pNodeWeight <- node$parent$weight
    
    #initial element ratio vector
    eRatio <- c()
    
    #initial element ratio vector index
    r <- 1

    #initial element sum number
    sum <- 0
    
    print(matrix)
    print(matrix[1,])
    
    
    #Get the elements in the first row of PC matrix
    for(j in matrix[1,]){
      eRatio[r] <- 1/j
      r <- r+1
      sum <- sum + 1/j
    }
    
    print(pNodeWeight)
    print(sum)
    print(eRatio)
    newWeightArrary <- round((pNodeWeight/sum)*eRatio,2)
    
    #Calculate the weight for each node
    m=1
    for(i in colIDs){
      #Find the node with name i
      aa = FindNode(node=vv$org,name = i)
      #Get the former weight
      oldWeight = aa$weight
     
      #Set new weight to node
      aa$weight = newWeightArrary[m]
      
      #Get the number of children node for node aa
      childrenNum <- length(aa$children)
      #If node aa has children
      if(childrenNum != 0){
        #assign weight by children's ratio
        for(k in aa$children ){
          k$weight = round(aa$weight/oldWeight*k$weight,2)
        }
      }
      m=m+1
    }
    
    #re-generate tree
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })

    #Update Pie chartpart
    #Get the triads number
    nTriads <- nrow(df) 
    #Get the triads number which kii larger than threshold
    nInconsisTriads <- nrow(subset(df,X7 > eps))
    #Render the bar chart in Dashboard section
    output$barChartPlot <- renderPlot({
      #Construct the data for bar chart
      df <- data.frame(
        group = c("Inconsistent Triads", "Consistent Triads"),
        value = c( nInconsisTriads, nTriads-nInconsisTriads)
      )
      
      #Plot the numbers of inconsistent and consistent triads based on group and value
      ggplot(df, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")+
        # Set the value in y axis from 0 to the number of triads , step is 1
        scale_y_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)
        ) +
        theme_minimal()
    })
    
  })
  
  #Function: findAllTriads
  #Description: Find all triads in a PC matrix
  #Arguments: matrix - A pairwise comparisons matrix
  #Return: A Dataframe with the index, value, Kii value for all the triads in a pc matrix
  #        and sort in a descending order of Kii value. 
  #        This resut dataframe has seven columns:
  #        i - the first index of current triad
  #        j - the second index of current triad
  #        k - the third index of current triad
  #        a_ij - the first value of current triad
  #        a_jk - the second value of current triad
  #        a_ik - the tird value of current triad
  #        Kii - the Kii value of current triad
  findAllTriads <- function(matrix){
    #get the number of rows in matrixs
    row <- nrow(matrix)
    #Initiates a temporary list
    templist <- list()
    #loop the pc matrix to find all the triads in the upper triangle
    #condition : i<j<k
    for(i in 1:(row-1)){
      for(j in (i+1):row){
        #X is the value a_ij in the triad
        X<- matrix[i,j]   
        if(j < row){
          for (k in (j+1):row){
            #Z is the value a_jk in the triad
            Z <- matrix[j,k] # 
            #Y is the value a_ik in the triad
            Y <- matrix[i,k]  
            
            #Calculate Kii value for each triad
            kii <- round(1 - min(Y/(X*Z),(X*Z)/Y),4)
            #add one row to the temporary list
            templist <- c(templist,list(c(i,j,k,X,Y,Z,kii)))
          }
        }
      }
    }
    
    #Generate out put data frame
    tempmatr <- data.frame(do.call(rbind,templist))
    #sort data frame by last column(the 7th column is Kii vallue) in a descending order
    orderFrame <- tempmatr[order(-tempmatr$X7),]
  }
  
  
  #Function: downloadHandler
  #Description: Save model to CSV file and downloads
  #Arguments: filename - A string of the filename
  #           content - A function that takes a single argument file that is a file path (string) of a nonexistent temp file, and writes the content to that file path. (Reactive values and functions may be used from this function.)
  #Usage: https://shiny.rstudio.com/reference/shiny/0.14/downloadHandler.html 
  output$saveModule <- downloadHandler(
    filename = function() {
      #Naming the file with current date and suffix as .csv
      paste(Sys.Date(), '.csv',sep = '')
    },
    content = function(con) {
      #Writing the data in CSV format
      data3 <- ToDataFrameNetwork(vv$org, "weight")
      write.csv(data3, file = file.path(con),row.names=FALSE)
    }
  ) 
}
