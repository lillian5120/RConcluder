library(shiny)
library(data.tree)
library(DiagrammeR)
library(shinyMatrix)
library(dplyr)
library(kableExtra)
library(readr)
library(ggplot2)

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
  
  ########## exampleData downloading begin ##########
  exampleData <- data.frame(
    from = c(
      "WHOQOL", "WHOQOL", "WHOQOL", "WHOQOL", "WHOQOL", "SR", "SR", "SR"
    ),
    to = c(
      "PH", "SR", "ENV1", "ENV2", "PSYCH", "p_rel", "social","sex"
    ),
    weight = c(
      31.57, 18.8, 19.87, 19.87, 9.93, 10.96, 4.35, 3.45
    )
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("exampleData-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(exampleData, file)
    }
  )
  
  ########## initial main tree ##########
  observe({
    
    output$file1 <- renderUI({
      input$reset ## Create a dependency with the reset button
      fileInput('file1', label = NULL)
      
    })
    
    inFile <- input$file1
    if (is.null(inFile)){
      vv$org <- Node$new("Root",weight=100) #set initial weight for root 100
      vv$names=vv$org$Get('name') # get names of main tree
      #initial Tree
      output$xx=renderGrViz({
        # set the style of graph
        SetGraphStyle(vv$org, rankdir = "TB")
        SetEdgeStyle(vv$org, arrowhead = "vee", color = "grey", penwidth = 2)
        SetNodeStyle(vv$org, style = "filled,rounded", shape = "polygon", fillcolor = "pink", fontname = "helvetica", tooltip = getNodeTip, label = getNodeLabel)
        grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
      })
      
    }else{
      
      #fileName <- system.file(inFile$datapath,package="data.tree")
      useRdf <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
      
      #convert data frame to Node
      useRtree <- FromDataFrameNetwork(useRdf)
      
      #initial the weight of root node to 100
      rootNode <- FindNode(useRtree,name=useRtree$root$name)
      rootNode$weight = 100
      
      
      #generate Tree
      output$xx=renderGrViz({
        # set the style of graph
        SetGraphStyle(useRtree, rankdir = "TB")
        SetEdgeStyle(useRtree, arrowhead = "vee", color = "grey", penwidth = 2)
        SetNodeStyle(useRtree, style = "filled,rounded", shape = "polygon", fillcolor = "pink", fontname = "helvetica", tooltip = getNodeTip, label = getNodeLabel)
        grViz(generate_dot(ToDiagrammeRGraph(useRtree)),engine = "dot")
      })
      
      vv$org <- useRtree 
      vv$names <- vv$org$Get('name') # get names of main tree
      
      # Clear analysis result, need re-analysis
      output$show_matrix <- NULL
    }
  })   
  
  ########## initial input UI ##########
  output$add_child_ui=renderUI({
    fluidRow(
      style = "position:relative",
      column(3,
             style = "position:relative",
             wellPanel(
               tags$h4("Add Node"),
               tags$hr(),
               selectInput("Parent_name","Parent_name",vv$names),
               div(id='my_textinput', textInput ('new_node_name', label = "New_name*", value = "",placeholder = "--Enter name--")),
               #tags$head(tags$style(type="text/css", "#my_textinput {color: red}")),
               #numericInput("new_node_weight","New_weight","", min = 1, max = 100),
               actionButton("add_child","Add"),
               tags$hr(),
               textOutput("add_error_msg"),
               tags$head(tags$style("#add_error_msg{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"))
             )),
      
      column(
        3,
        style = "position:relative",
        wellPanel(
          tags$h4("Change Node Name"),
          tags$hr(),
          selectInput("Name_to_change","Name_to_change",vv$names),
          div(id='my_textinput', textInput ('new_name', label = "New_name*", value = "",placeholder = "--Enter name--")),
          tags$head(tags$style(type="text/css", "#my_textinput {color: red}")),
          #numericInput("new_weight","New_weight","", min = 1, max = 100),
          actionButton("Change_name","Change"),
          tags$hr(),
          textOutput("update_error_msg"),
          tags$head(tags$style("#update_error_msg{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"))
        )),
      
      column(3,
             style = "position:relative",
             wellPanel(
               tags$h4("Delete Node"),
               tags$hr(),
               selectInput("Name_to_delete","Name_to_delete",vv$names),
               actionButton("remove_node","Remove"),
               tags$hr(),
               helpText("Attention: The children of this node will be removed too!")
             )),
      
      column(3,
             style = "position:relative",
             wellPanel(
               tags$h4("Analysis"),
               tags$hr(),
               numericInput("eps","Inconsistency Tolerance Level",0.33, min = 0, max = 1, step = 0.1),
               selectInput("Parent_node_name","Parent_node_name",vv$names),
               actionButton("analysis","Analysis"),
               tags$hr(),
               textOutput("error_msg"),
               tags$head(tags$style("#error_msg{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }"))
             ))
    )
  })
  
  
  ########## update Tree weight function ##########
  updateTreeWeights <- function(Tree){
    height <- Tree$height
    #print("height")
    #print(height)
    nodeName <- Tree$Get('name',filterFun= function(x) x$level == 1)
    rootNode <- FindNode(Tree,name=nodeName)
    rootNode$weight <- 100
    
    for(i in 1:height){
      nodeName <- Tree$Get('name',filterFun= function(x) x$level == i)
      for(j in nodeName){
        nodeObj  <- FindNode(Tree,name = j)
        childrenNum <- length(nodeObj$children)
        #print("children num:")
        #print(childrenNum)
        for(k in nodeObj$children){
          #print(k$name)
          k$weight = round(nodeObj$weight/childrenNum,2)
        }
      }
    }
  }
  
  ##########  Get node label function ##########
  getNodeLabel <- function(node){
    label = paste0(node$name, "\n",node$weight)
    return(label)
  }
  ##########  Get node tip function ##########
  getNodeTip<- function(node){
    tip = node$weight
    return(tip)
  }
  
  ##########  Oberserve event for reset tree button ##########
  observeEvent(input$reset_tree,{
    vv$org <- Node$new("Root",weight=100) #set initial weight for root 100
    #vv$org <- Node$new("Root") 
    vv$names = vv$org$Get('name') # get names of main tree
    
    #initial Tree
    output$xx=renderGrViz({
      grViz(generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
    
    # need re-analysis
    output$show_matrix <- NULL
  })
  
  ##########  Oberserve event for reset tree weight button ##########
  observeEvent(input$reset_tree_weight,{
    updateTreeWeights(vv$org)
    
    #refresh Tree
    output$xx=renderGrViz({
      grViz(generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
    
    # need re-analysis
    output$show_matrix <- NULL
    
  })
  
  ########## add child node ##########
  observeEvent(input$add_child,{
    # valid check for node name
    if (input$new_node_name ==""){
      output$add_error_msg <- renderText('New node name is empty!')
      return(FALSE)
    }else if(!is.null(FindNode(vv$org,name=input$new_node_name))){
      output$add_error_msg <- renderText('The node name is already exist!')
      return(FALSE)
    }else{
      output$add_error_msg <- renderText("")
    }
    
    FindNode(node=vv$org,name = input$Parent_name)$AddChildNode(Node$new(input$new_node_name, weight=input$new_node_weight)) # add child
    
    vv$names=vv$org$Get('name')# get names of new tree
    
    updateTreeWeights(vv$org)
    
    #re-generate 
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
  })
  
  
  ########## change_name ##########
  observeEvent(input$Change_name,{
    #valid check for name
    if (input$new_name ==""){
      output$update_error_msg <- renderText('New node name is empty!')
      return(FALSE)
    }else if(!is.null(FindNode(vv$org,name=input$new_name))){
      output$update_error_msg <- renderText('The node name is already exist!')
      return(FALSE)
    }else{
      output$update_error_msg <- renderText("")
    }
    
    aa=FindNode(node=vv$org,name = input$Name_to_change) 
    aa$name=input$new_name # Change name
    
    vv$names=vv$org$Get('name')# get names of new tree
    
    #re-generate chart
    output$xx=renderGrViz({
      grViz(generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
  })
  

  
  ########## remove_node ##########
  observeEvent(input$remove_node,{
    #Prune tree
    Prune(vv$org, function(x) x$name != input$Name_to_delete)
    vv$names=vv$org$Get('name')# get names of new tree
    
    updateTreeWeights(vv$org)
    
    #re-generate 
    output$xx=renderGrViz({
      grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
    })
    
    # need re-analysis
    output$show_matrix <- NULL
  })
  
  
  ##########  Oberserve event for Analysis button ##########
  observeEvent(input$analysis,{
    
    eps <- input$eps
    
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
      
      wellPanel(
      titlePanel(h4("Pairwise Comparisons Matrix")),
      fluidRow(
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
          actionButton("update","Update PC Matrix Value",style="margin-left:100px;margin-top:30px;", icon = icon("refresh")),
          actionButton("reduce"," Reduce Inconsistency",style="margin-left:500px;margin-top:30px;", icon = icon("balance-scale")),
          tags$hr()
        )
      ),
      titlePanel(h4("Dashboard")),
      wellPanel(
      fluidRow(
        style = "position:relative",
        column(
          6,
          style = "position:relative",
            tableOutput("highlight_table"),
            h4(textOutput("nTraids")),
            tags$hr(),
            downloadButton("save", "Download Tree Model",style="margin-bottom:90px;margin-left:100px;margin-right:auto")
        ),
        column(
          6,
          style = "position:relative",
            #h2("Pie Chart"),
            plotOutput("pieChartPlot")
            #,plotOutput("pieChartPlot2")
          )
        )
      )
      )
    })
    
    
    df <- findAllTrials(genMatrix)
    print(df)
    max_kii <- df[1,7]
    
    output$nTraids <- renderText({
      eps <- input$eps
      if(max_kii > eps){
        paste("The max kii value is:", max_kii)
      }else{
        paste("Congras! you get a consistent PC Matrix with kii value:",max_kii)
      }
    })
    
    output$highlight_table <- function() {
      # highlight sign
      if(max_kii > eps){
        i <- df[1,1]
        j <- df[1,2]
        k <- df[1,3]
        r <- nrow(genMatrix)
        genMatrix[(j-1)*r+i] <- cell_spec(genMatrix[i,j], color = "white", bold = T,background = "red")
        genMatrix[(k-1)*r+j] <- cell_spec(genMatrix[j,k], color = "white", bold = T,background = "red")
        genMatrix[(k-1)*r+i] <- cell_spec(genMatrix[i,k], color = "white", bold = T,background = "red")
      }
      
      genMatrix %>%
        kbl(escape = F, align = "c") %>%
        kable_styling("striped", full_width = F) 
    }
    
    #Generate Pie chart part
    
    # get the triads number 
    nTriads <- nrow(df)
    
    # get the triads number which kii larger than threshold
    nInconsisTraids <- nrow(subset(df,X7 > eps))
    print(nTriads)
    print(nInconsisTraids)
    output$pieChartPlot <- renderPlot({
      df <- data.frame(
        group = c("Inconsistent Triads", "Consistent Triads"),
        value = c( nInconsisTraids, nTriads-nInconsisTraids)
      )
      ggplot(df, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")+
        scale_y_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
          expand = expand_scale(mult = c(0, 0.05))
        ) +
        theme_minimal()
    })

  })


  ##########  generate PC Matrix for the nodes ##########
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
  
  ##########  Oberserve event for Update matrix button ##########
  observeEvent(input$update,{
    new_matrix <- input$matrix
    eps <- input$eps
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
        paste("The maximum kii value is:", max_kii)
      }else{
        paste("Congras! you get a consistent PC Matrix with kii value:", df[1,7])
      }
    })
    
    output$highlight_table <- function(){
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
      
      new_matrix %>%
        kbl(escape = F, align = "c") %>%
        kable_styling("striped", full_width = F) 
    }
    
    #Update Pie chart part
    
    # get the triads number 
    nTriads <- nrow(df)
    
    # get the triads number which kii larger than threshold
    nInconsisTraids <- nrow(subset(df,X7 > eps))
    print(nTriads)
    print(nInconsisTraids)
    output$pieChartPlot <- renderPlot({
      df <- data.frame(
        group = c("Inconsistent Triads", "Consistent Triads"),
        value = c( nInconsisTraids, nTriads-nInconsisTraids)
      )
      ggplot(df, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")+
        scale_y_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
          expand = expand_scale(mult = c(0, 0.05))
        ) +
        theme_minimal()
    })
  
    
  })
  
  ##########  Oberserve event for Reduce inconsistency button ##########
  observeEvent(input$reduce,{
    matrix <- input$matrix
    print("matrix is :")
    print(matrix)
    
    df <- findAllTrials(matrix)
    eps <- input$eps
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
      paste("new N value is: ",N)
      #repeat_time <- repeat_time+1
      #paste("new_repeat_time is:",repeat_time)
    }
    
    #timeend<-Sys.time()
    #paste("runningtime is:",timeend-timestart)
    
    updateMatrixInput(session,"matrix",matrix)
    
    output$nTraids <- renderText({
      paste("Congras! you get a consistent PC Matrix with kii value:", df[1,7])
    })
    
    output$highlight_table <- function() {
      matrix %>%
        kbl(escape = F, align = "c") %>%
        kable_styling("striped", full_width = F) 
    }
    
    
    #update the tree
    colIDs = rownames(matrix)
    #get node by first element name
    node = FindNode(node=vv$org,name = colIDs[1])
    #pNodeName =  node$parent$name
    pNodeWeight = node$parent$weight
    
    #initial element ratio array
    eRatio <- c()
    
    # element ratio array index
    r <- 1

    #initial element sum number
    sum <- 0

    #Get the elements in the first row of PC matrix
    for(j in matrix[1,]){
      eRatio[r] <- 1/j
      r <- r+1
      sum <- sum + 1/j
    }
    
    newWeightArrary <- round((pNodeWeight/sum)*eRatio,2)
    
    output$pieChartPlot2 <- renderPlot({
      df <- data.frame(
        group = colIDs,
        value = newWeightArrary
      )
      
      ggplot(df, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")+
        coord_polar("y", start=0) +
        theme_minimal()
    })
    
    #Calculate the weight for each node
    m=1
    for(i in colIDs){
      aa = FindNode(node=vv$org,name = i)
      oldWeight = aa$weight
      aa$weight = newWeightArrary[m]
      
      #if current node has children
      childrenNum <- length(aa$children)
      
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

    #update Pie chartpart
    #get the triads number 
    nTriads <- nrow(df)
    
    #get the triads number which kii larger than threshold
    nInconsisTraids <- nrow(subset(df,X7 > eps))
    output$pieChartPlot <- renderPlot({
      df <- data.frame(
        group = c("Inconsistent Triads", "Consistent Triads"),
        value = c( nInconsisTraids, nTriads-nInconsisTraids)
      )
      ggplot(df, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")+
        scale_y_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
          expand = expand_scale(mult = c(0, 0.05))
        ) +
        theme_minimal()
    })
    
  })
  
  ########## Find all trials in a PC matrix function ##########
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
  }
  
  
  ########## Save model to CSV file ##########
  output$save <- downloadHandler(
    filename = function() {
      paste(getwd(), Sys.Date(), '.csv',sep = '')
    },
    content = function(con) {
      data3 <- ToDataFrameNetwork(vv$org, "weight")
      write.csv(data3, file = file.path(con),row.names=FALSE)
    }
  ) 
  
}
