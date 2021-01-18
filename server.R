library(shiny)
library(data.tree)
library(DiagrammeR)
library(shinyMatrix)
library(dplyr)
library(kableExtra)
library(readr)
function(input, output,session){
  
  #observeEvent() triggers code to run on the server
  observeEvent(input$clicks,{print(as.numeric(input$clicks))})
  
  # use observe() for a more implicit syntax
  observe({print(input$clicks)})
  
  
  #Delay reactions with eventReactive()
  #A reactive expression that only responds to specific values input$go
  data <- eventReactive(input$go,{rnorm(input$num)})
  
  
  #manage state with reactive values()
  #creates a list of reactive values to manipulate programmatically
  rv<- reactiveValues(data=rnorm(100))
  
  #Create reative value to app
  vv=reactiveValues(org=NULL,names=NULL,weight=NULL)
  
  #create main tree
  observe({
    vv$org <- Node$new("Root",weight=100) #set initial weight for root 100
    vv$names=vv$org$Get('name') # get names of main tree
  })
  
  #initial Tree
  output$xx=renderGrViz({
    # set the style of graph
    SetGraphStyle(vv$org, rankdir = "TB")
    SetEdgeStyle(vv$org, arrowhead = "vee", color = "grey", penwidth = 2)
    SetNodeStyle(vv$org, style = "filled,rounded", shape = "polygon", fillcolor = "pink", fontname = "helvetica", tooltip = getNodeTip, label = getNodeLabel, href="javascript.function(){alert()}", attr_string="123")
    grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
  })
  
  
  output$add_child_ui=renderUI({
    fluidRow(
      style = "position:relative",
      
      column(
        3,
        style = "position:relative",
        wellPanel(
          selectInput("Name_to_change","Name_to_change",vv$names),
          textInput("new_name","New_name",""),
          numericInput("new_weight","New_weight","", min = 1, max = 100),
          actionButton("Change_name","Change")
        )),
      
      column(3,
             style = "position:relative",
             wellPanel(
               selectInput("Parent_name","Parent_name",vv$names),
               textInput("new_node_name","New_node_name",""),
               numericInput("new_node_weight","New_node_weight","", min = 1, max = 100),
               actionButton("add_child","Add")
             )),
      
      column(3,
             style = "position:relative",
             wellPanel(
               selectInput("Name_to_delete","Name_to_delete",vv$names),
               actionButton("remove_node","Remove"),
               helpText("Attention: The children of this node will be removed too!")
             )),
      
      column(3,
             style = "position:relative",
             wellPanel(
               helpText("This is a help"),
               selectInput("Parent_node_name","Parent_node_name",vv$names),
               actionButton("analysis","Analysis"),
               textOutput("error_msg"),
               tags$head(tags$style("#error_msg{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"
               )
               )
             ))
    )
  })
  
  getNodeLabel <- function(node){
    label = paste0(node$name, "\n",node$weight)
    return(label)
  }
  
  getNodeTip<- function(node){
    tip = node$weight
    return(tip)
  }
  
  
  #change_name
  observeEvent(input$Change_name,{
    
    aa=FindNode(node=vv$org,name = input$Name_to_change) 
    aa$name=input$new_name # Change name
    aa$weight = input$new_weight # Change weight
    
    vv$names=vv$org$Get('name')# get names of new tree
    
    #re-generate chart
    output$xx=renderGrViz({
      grViz(generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
  })
  
  
  #add_child
  observeEvent(input$add_child,{
    FindNode(node=vv$org,name = input$Parent_name)$AddChildNode(Node$new(input$new_node_name, weight=input$new_node_weight)) # add child
    vv$names=vv$org$Get('name')# get names of new tree
    #re-generate 
    output$xx=renderGrViz({
      
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
  })
  
  
  #remove_node
  observeEvent(input$remove_node,{
    Prune(vv$org, function(x) x$name != input$Name_to_delete)
    vv$names=vv$org$Get('name')# get names of new tree
    
    #re-generate 
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
  })
  
  #set an inconsistency threshold
  eps <- 1/3
  
  #Analysis
  observeEvent(input$analysis,{
    aa = FindNode(node=vv$org,name = input$Parent_node_name) 
    
    #get the number of children of current node
    n =length(aa$children)
    
    if(n < 3){
      errorMsg = "The selected node has to have more than 3 child nodes"
      output$error_msg <- renderText(errorMsg)
      output$show_matrix <- NULL
      return(FALSE)
    }else{
      errorMsg = ""
      output$error_msg <- renderText(errorMsg)
    }
    
    
    childrenLevel <- aa$level+1
    nodeList <- aa$Get('weight',filterFun= function(x) x$level == childrenLevel)
    
    
    #generate reciprecial matrix
    
    genMatrix <- generateMatrix(nodeList)
    
    output$show_matrix=renderUI({
      fluidRow(
        style = "position:relative",
        
        column(
          6,
          style = "position:relative",
          wellPanel(
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
            actionButton("update","Update Value"),
            actionButton("reduce","Reduce Inconsistency"),
          )),
        column(
          6,
          style = "position:relative",
          wellPanel(
            tableOutput("mtcars_kable"),
            #helpText("This is a help"),
            h4(textOutput("nTraids")),
            tableOutput("results")
          ))
      )
    })
    
    #result_summary <- updateInconsistency(genMatrix)
    #df <- result_summary[2]
    
    df <- findAllTrials(genMatrix)
    print(df)
    max_kii <- df[1,7]
    
    output$nTraids <- renderText({
      #nTraids  <- nrow(df)
      #paste("The number of traids is:", nTraids)
      if(max_kii > eps){
        paste("The max kii value is:", max_kii)
      }else{
        paste("Congras! you get a consistent PC Matrix with kii value:",max_kii)
      }
    })
    
    output$mtcars_kable <- function() {
      # highlight sign
      if(max_kii > eps){
        i <- df[1,1]
        j <- df[1,2]
        k <- df[1,3]
        r <- nrow(new_matrix)
        new_matrix[(j-1)*r+i] <- cell_spec(new_matrix[i,j], color = "white", bold = T,background = "red")
        new_matrix[(k-1)*r+j] <- cell_spec(new_matrix[j,k], color = "white", bold = T,background = "red")
        new_matrix[(k-1)*r+i] <- cell_spec(new_matrix[i,k], color = "white", bold = T,background = "red")
      }
      
      kbl(genMatrix, escape = F, align = "c") %>%
        kable_classic("striped", full_width = F)
    }
    
  })
  
  
  #generate PC Matrix for the nodes
  generateMatrix<-function(nodeList){
    
    dim <- 0
    weight_elements <- c()
    index_names <-names(nodeList)
    
    #generate reciprecial matrix
    for(i in index_names){
      dim <- dim + 1
      weight_elements[dim] <-  nodeList[i]
    }
    
    matrix_elements <- c()
    
    index <- 1
    
    for(i in weight_elements){
      for(j in weight_elements){
        matrix_elements[index] <- round(i/j,2)
        index <- index +1
      }
    }
    
    pc_matrix <- matrix(data = matrix_elements, nrow = dim, ncol = dim)
    rownames(pc_matrix) <- index_names
    colnames(pc_matrix) <-index_names
    
    return(pc_matrix)
  }
  
  
  
  #Update matrix
  observeEvent(input$update,{
    new_matrix <- input$matrix
    
    #Update reciprocal pairwise comparison matrix
    dim <- nrow(new_matrix)
    for(r in 1:dim){
      for( c in 1:dim){
        if(c > r){
          new_matrix[c,r] <- round(1/new_matrix[r,c],2)
        }
        if(c == r){
          new_matrix[c,r] <- 1
        }
      }
    }
    
    updateMatrixInput(session,"matrix",new_matrix)
    
    df <- findAllTrials(new_matrix)
    
    
    
    max_kii <- df[1,7]
    output$nTraids <- renderText({
      if(max_kii > eps){
        paste("The max kii value is:", max_kii)
      }else{
        paste("Congras! you get a consistent PC Matrix with kii value:", df[1,7])
      }
    })
    
    
    output$mtcars_kable <- function() {
      # highlight sign
      if(max_kii > eps){
        i <- df[1,1]
        j <- df[1,2]
        k <- df[1,3]
        r <- nrow(new_matrix)
        new_matrix[(j-1)*r+i] <- cell_spec(new_matrix[i,j], color = "white", bold = T,background = "red")
        new_matrix[(k-1)*r+j] <- cell_spec(new_matrix[j,k], color = "white", bold = T,background = "red")
        new_matrix[(k-1)*r+i] <- cell_spec(new_matrix[i,k], color = "white", bold = T,background = "red")
      }
      
      kbl(new_matrix, escape = F, align = "c") %>%
        kable_classic("hover", full_width = F)
    }
    
  })
  
  #Reduce inconsistency
  observeEvent(input$reduce,{
    matrix <- input$matrix
    print("matrix is :")
    print(matrix)
    
    df <- findAllTrials(matrix)
    
    # get the triads which kii larger than threshold
    traids <- subset(df,X7 > eps)
    
    N <- nrow(traids)
    print("the number of inconsistet triads is:",N)
    print(traids)
    #repeat_time <- 1
    #timestart <-Sys.time()
    
    while(N !=0){
      
      
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
      
      
      #update matrix using new_x,y,z
      matrix[i,j] <- round(new_x,2)
      matrix[i,k] <- round(new_y,2)
      matrix[j,k] <- round(new_z,2)
      
      matrix[j,i] <- round(1/new_x,2)
      matrix[k,i] <- round(1/new_y,2)
      matrix[k,j] <- round(1/new_z,2)
      
      
      #recalculate the kii of all traids and sort by kii desc
      df <- findAllTrials(matrix)
      #get the number of kii value is larger than eps
      subFrame <- subset(df,X7 > eps)
      N <- nrow(subFrame)
      print("new_N")
      print(N)
      #paste("new N value is: ",N)
      #repeat_time <- repeat_time+1
      #paste("new_repeat_time is:",repeat_time)
    }
    
    #timeend<-Sys.time()
    #paste("runningtime is:",timeend-timestart)
    
    
    updateMatrixInput(session,"matrix",matrix)
    
    output$nTraids <- renderText({
      paste("Congras! you get a consistent PC Matrix with kii value:", df[1,7])
    })
    
    output$mtcars_kable <- function() {
      kbl(matrix, escape = F, align = "c") %>%
        kable_classic("striped", full_width = F)
    }
    
  })
  
  
  findAllTrials <- function(matrix){
    #get the number of rows in matrixs
    row <- nrow(matrix)
    templist <- list()
    for(i in 1:(row-1)){
      for(j in (i+1):row){
        X<- matrix[i,j]    # a_ij
        if(j < row){
          for (k in (j+1):row){
            Z <- matrix[j,k] # a_jk
            Y <- matrix[i,k] # a_ik
            kii <- round(1 - min(Y/(X*Z),(X*Z)/Y),4)
            
            templist <- c(templist,list(c(i,j,k,X,Y,Z,kii)))
          }
        }
      }
    }
    tempmatr <- data.frame(do.call(rbind,templist))
    orderFrame <- tempmatr[order(-tempmatr$X7),]
    print("this is all traids")
    print(orderFrame)
  }
  
}
