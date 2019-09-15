require(shinyWidgets)



# zoib::zoib(model, data, )



# scorecard::woebin_plot(woe[['OD_kan_DO_2017_04_month']], show_iv = FALSE)

u <- shiny::shinyUI(
  shiny::fluidPage(
    shiny::textInput(inputId = 'path', value = paste0(getwd(), '/models_archive'), label = 'path')
    ,uiOutput('file_picker')
    ,actionButton(input='load_comments', 'load comments')
    ,actionButton(inputId = 'load_models', label = 'load models')
    ,materialSwitch(inputId = 'load_with_data', value = FALSE, label = 'load with data' )
    ,flowLayout(
      uiOutput('models_picker_binary')
      ,uiOutput('models_picker_categorical')
      ,uiOutput('models_picker_numeric')
    )d
    ,actionButton(inputId = 'compare_models', label = 'compare models')
    # ,pickerInput(
    #   inputId = "statistics", 
    #   label = "select model", 
    #   choices = c('confusion_TR', 'confusion_PR'),
    #   options = list(
    #     `selected-text-format` = "count > 5",
    #     `count-selected-text` = "{0} villes sélectionnées",
    #     `actions-box` = TRUE,
    #     `deselect-all-text` = "Unselect",
    #     `select-all-text` = "Select all"
    #   ), 
    #   multiple = TRUE
    # )
    
    ,mainPanel(
      tabsetPanel(
        tabPanel("comments", verbatimTextOutput('comments'))
        # ,tabPanel('models_all', )
        ,tabPanel(  "classification_binary"
                    , tableOutput('classif_binary_table')
                    , tableOutput('classif_binary_table_2')
                    , verbatimTextOutput('classif_binary_print') )
        ,tabPanel("classification", tableOutput('classif_categorical_table'), verbatimTextOutput('classif_categorical_print'))
        ,tabPanel("regression", tableOutput('classif_numeric_table'), verbatimTextOutput('classif_numeric_print'))
      )
      
    )
  )
)



s <- function(input, output, session){
  
  # list of files you can load -----
  output$file_picker <- renderUI({
    
    
    #files list
    
    files <- list.files(input$path)
    files <- files[!str_detect(files, '^comments') ]
    
    tagList(  pickerInput(
      inputId = "picker", 
      label = "select model", 
      choices = files,
      options = list(
        `selected-text-format` = "count > 5",
        `count-selected-text` = "{0} villes sélectionnées",
        `actions-box` = TRUE,
        `deselect-all-text` = "Unselect",
        `select-all-text` = "Select all"
      ), 
      multiple = TRUE
    ))
    
  })
  
  
  # loading files with comments -----
  
  observeEvent(eventExpr = input$load_comments, {
    
    output$comments <- renderPrint({
      
      path <- isolate(input$path)
      
      # czy istnieje plik z komentarzami
      if_comments_exists <- file.exists(paste0(path, '/comments.RDS'))
      
      if(if_comments_exists){
        comments <- readRDS(paste0(path, '/comments.RDS'))
      }else{
        comments <- tibble::tibble(model = character(0) , comment = character(0))
      }
      comments
    })
    
  })
  
  
  
  # loading selected files to memory -----
  
  observeEvent(eventExpr = input$load_models, {
    
    models_list <<- list()
    
    selected_models <<- isolate(input$picker)
    
    for(i in 1:length(selected_models)){
      models_list[[selected_models[i]]] <<- readRDS(file = paste0(isolate(input$path), '/', selected_models[i]))
      if(!isolate(input$load_with_data)){
        models_list[[selected_models[i]]]$task_TR <<- NULL
        models_list[[selected_models[i]]]$task_PR <<- NULL
        models_list[[selected_models[i]]]$train   <<- NULL
      }  
      
    }
    
    print(names(models_list))
    print(object_size(models_list))
    
  })
  
  
  
  # pickers of models to compare -----
  
  observeEvent(input$load_models,
               output$models_picker_binary <- renderUI({
                 
                 files <- selected_models
                 files <- files[str_detect(files, '^classif')]
                 
                 tagList(  pickerInput(
                   inputId = "model_picker_binary_to_compare", 
                   label = "select binary models to compare", 
                   choices = files,
                   options = list(
                     `selected-text-format` = "count > 5",
                     `count-selected-text` = "{0} villes sélectionnées",
                     `actions-box` = TRUE,
                     `deselect-all-text` = "Unselect",
                     `select-all-text` = "Select all"
                   ), 
                   multiple = TRUE
                 ))
               })
  )
  
  observeEvent(input$load_models,
               output$models_picker_categorical <- renderUI({
                 
                 files <- selected_models
                 files <- files[str_detect(files, '^classif')]
                 
                 tagList(  pickerInput(
                   inputId = "model_picker_categorical_to_compare", 
                   label = "select categorical models to compare", 
                   choices = files,
                   options = list(
                     `selected-text-format` = "count > 5",
                     `count-selected-text` = "{0} villes sélectionnées",
                     `actions-box` = TRUE,
                     `deselect-all-text` = "Unselect",
                     `select-all-text` = "Select all"
                   ), 
                   multiple = TRUE
                 ))
               })
  )
  
  observeEvent(input$load_models,
               output$models_picker_numeric <- renderUI({
                 
                 files <- selected_models
                 files <- files[str_detect(files, '^regr')]
                 
                 tagList(  pickerInput(
                   inputId = "model_picker_numeric_to_compare", 
                   label = "select numeric models to compare", 
                   choices = files,
                   options = list(
                     `selected-text-format` = "count > 5",
                     `count-selected-text` = "{0} villes sélectionnées",
                     `actions-box` = TRUE,
                     `deselect-all-text` = "Unselect",
                     `select-all-text` = "Select all"
                   ), 
                   multiple = TRUE
                 ))
               })
  )
  
  
  
  
  
  # model variables tables ----
  
  observeEvent(input$compare_models,{
    
    
    output$classif_binary_table <- renderTable({
      
      models <- isolate(input$model_picker_binary_to_compare)
      
      targets_list  <- list()
      features_list <- list()
      sizes_list    <- list()
      
      for(i in 1:length(models_list)){
        
        if(names(models_list)[i] %in%  models){
          
          targets_list[[models[i]]]  <- models_list[[i]]$statistics_TR_basic$target
          features_list[[models[i]]] <- models_list[[i]]$statistics_TR_basic$features
          sizes_list[[models[i]]]    <- models_list[[i]]$statistics_TR_basic$set_size
          
        }
      }
      
      # targets_list <- list(c1=c('a', 'b'), c2=c('b','d'), c3=c('w','z'))
      
      features <- unlist(features_list) %>% unique
      
      tb <- tibble(zm=features)
      
      for(i in 1:length(targets_list)){
        zm_i = paste0('zm_',i)
        tb %<>% left_join(tibble(zm = features_list[[i]], !!zm_i := zm), by = 'zm')
      }
      
      tb %<>% mutate_at(vars(starts_with('zm_')), .funs = function(x) case_when(is.na(x) ~ 0L, TRUE ~ 1L) ) %>% as.data.frame
      
      tb
    })
    
    output$classif_categorical_table <- renderTable({
      
      models <- isolate(input$model_picker_categorical_to_compare)
      
      targets_list  <- list()
      features_list <- list()
      sizes_list    <- list()
      
      for(i in 1:length(models_list)){
        
        if(names(models_list)[i] %in%  models){
          
          targets_list[[models[i]]]  <- models_list[[i]]$statistics_TR_basic$target
          features_list[[models[i]]] <- models_list[[i]]$statistics_TR_basic$features
          sizes_list[[models[i]]]    <- models_list[[i]]$statistics_TR_basic$set_size
          
        }
      }
      
      # targets_list <- list(c1=c('a', 'b'), c2=c('b','d'), c3=c('w','z'))
      
      features <- unlist(features_list) %>% unique
      
      tb <- tibble(zm=features)
      
      for(i in 1:length(targets_list)){
        zm_i = paste0('zm_',i)
        tb %<>% left_join(tibble(zm = features_list[[i]], !!zm_i := zm), by = 'zm')
      }
      
      tb %<>% mutate_at(vars(starts_with('zm_')), .funs = function(x) case_when(is.na(x) ~ 0L, TRUE ~ 1L) ) %>% as.data.frame
      
      tb
    })
    
    output$classif_numeric_table <- renderTable({
      
      models <- isolate(input$model_picker_numeric_to_compare)
      
      targets_list  <- list()
      features_list <- list()
      sizes_list    <- list()
      
      for(i in 1:length(models_list)){
        
        if(names(models_list)[i] %in%  models){
          
          targets_list[[models[i]]]  <- models_list[[i]]$statistics_TR_basic$target
          features_list[[models[i]]] <- models_list[[i]]$statistics_TR_basic$features
          sizes_list[[models[i]]]    <- models_list[[i]]$statistics_TR_basic$set_size
          
        }
      }
      
      # targets_list <- list(c1=c('a', 'b'), c2=c('b','d'), c3=c('w','z'))
      
      features <- unlist(features_list) %>% unique
      
      tb <- tibble(zm=features)
      
      for(i in 1:length(targets_list)){
        zm_i = paste0('zm_',i)
        tb %<>% left_join(tibble(zm = features_list[[i]], !!zm_i := zm), by = 'zm')
      }
      
      tb %<>% mutate_at(vars(starts_with('zm_')), .funs = function(x) case_when(is.na(x) ~ 0L, TRUE ~ 1L) ) %>% as.data.frame
      
      tb
    })
    
    
    
    
    
    
    # binary statistics ----
    
    
    output$classif_binary_table_2 <- renderTable({
      
      # statistics <- isolate(input$statistics) # not implemented
      
      models <- isolate(input$model_picker_binary_to_compare)
      
      for(i in 1:length(models_list)){
        
        if(names(models_list)[i] %in%  models){
          
          #binary stat
          if(i == 1){
            binary_stat_TR <- models_list[[i]]$statistics_PR_basic$ROC_measures_TR[,1:2]
            binary_stat_PR <- models_list[[i]]$statistics_PR_basic$ROC_measures_PR[,1:2]
          }else{
            binary_stat_TR <- dplyr::bind_cols(binary_stat_TR, models_list[[i]]$statistics_PR_basic$ROC_measures_TR[,2])
            binary_stat_PR <- dplyr::bind_cols(binary_stat_PR, models_list[[i]]$statistics_PR_basic$ROC_measures_PR[,2])
          }
          
          
        }
        
      }
      
      dplyr::bind_rows(TR = binary_stat_TR, PR = binary_stat_PR, .id = 'stage') %>% filter(measure %in% c('ppv', 'npv', 'acc')) %>% as.data.frame
      
    })
    
    
    
    
    
    output$classif_binary_print <- renderPrint({
      
      statistics <- isolate(input$statistics)
      
      models <- isolate(input$model_picker_binary_to_compare)
      
      
      threshold_PR <- tibble()
      confusion_TR <- list()
      confusion_PR <- list()
      auc_TR <- tibble(auc=numeric(0))
      auc_PR <- tibble(auc=numeric(0))
      
      
      
      for(i in 1:length(models_list)){
        
        if(names(models_list)[i] %in%  models){
          
          
          threshold_PR <- dplyr::bind_rows(  threshold_PR
                                             , tibble(c(names(models_list)[i],models_list[[i]]$prediction_PR$threshold)))
          
          
          # threshold
          # threshold_PR[[models[i]]] <- models_list[[i]]$prediction_PR$threshold
          # colnames_ <- colnames(threshold_PR)
          # colnames(threshold_PR) <- NULL
          # threshold_PR <- rbind(threshold_PR, models_list[[i]]$prediction_PR$threshold)
          # colnames(threshold_PR) <- c(colnames_, names(models_list)[i] )
          # 
          # names(models_list)[i]
          
          
          # confusion matrix
          confusion_TR[[models[i]]] <- models_list[[i]]$F_conf_matrix_function(mlr_conf_matrix = 'TR')
          confusion_PR[[models[i]]] <- models_list[[i]]$F_conf_matrix_function(mlr_conf_matrix = 'PR')
          
          #auc
          auc_TR <- dplyr::bind_rows(auc_TR, tibble(auc = models_list[[i]]$statistics_TR_extra[['auc']][[1]] ))
          auc_PR <- dplyr::bind_rows(auc_PR, tibble(auc = models_list[[i]]$statistics_PR_extra[['auc']][[1]] ))
          
          
          
        }
        
      }
      
      cat('\n\n\n<<< AUC test>>>')
      print(threshold_PR)
      
      cat('\n<<< CONFUSION MATRIX train >>>')
      print(confusion_TR)
      
      cat('\n\n\n<<< CONFUSION MATRIX test >>>')
      print(confusion_PR)
      
      cat('\n\n\n<<< AUC train>>>')
      print(auc_TR)
      
      cat('\n\n\n<<< AUC test>>>')
      print(auc_PR)
      
      # cat('\n\n\n<<< BINARY STATS train>>>')
      # print(binary_stat_TR)
      
      # cat('\n\n\n<<< BINARY STATS test>>>')
      # print(binary_stat_PR)
      
      
      
    })
    
    
    
    
    
    # classification statistics ----
    
    output$classif_categorical_print <- renderPrint({
      
      # statistics <- isolate(input$statistics)
      
      models <- isolate(input$model_picker_categorical_to_compare)
      
      
      confusion_TR <- list()
      confusion_PR <- list()
      
      
      for(i in 1:length(models_list)){
        
        if(names(models_list)[i] %in%  models){
          
          # confusion matrix
          confusion_TR[[models[i]]] <- models_list[[i]]$F_conf_matrix_function(mlr_conf_matrix = 'TR')
          confusion_PR[[models[i]]] <- models_list[[i]]$F_conf_matrix_function(mlr_conf_matrix = 'PR')
          
          
        }
        
      }
      
      
      cat('\n<<< CONFUSION MATRIX train >>>')
      print(confusion_TR)
      
      cat('\n\n\n<<< CONFUSION MATRIX test >>>')
      print(confusion_PR)
      
      
    })
    
    
    
    
    
    
    # numeric statistics
    
    output$classif_numeric_print <- renderPrint({
      
      statistics <- isolate(input$statistics)
      
      models <- isolate(input$model_picker_categorical_to_compare)
      
      
      confusion_TR <- list()
      confusion_PR <- list()
      
      
      for(i in 1:length(models_list)){
        
        if(names(models_list)[i] %in%  models){
          
          # ??? 
          
          
          
        }
        
      }
      
      
      cat('\n<<< ??? >>>')
      
      
      cat('\n\n\n<<< ??? >>>')
      
      
      
    })
    
    
    
  })
  
  
  
}


shiny::shinyApp(ui = u, server = s)
