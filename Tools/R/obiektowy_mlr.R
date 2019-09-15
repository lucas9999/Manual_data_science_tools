# funckjce wrapujace tez obiektowo i hierarchia analogiczna do hierarchii obiektow z modelami 
# w modelach zapisuje podstawowe wyniki


# classif -----

# mlr::undersample(rf_cat_1_$task)
# 
# 
# 
# rf_cat_1_ <- classif.mlr_$new()
# 
# rf_cat_1_$F_task(data =  data_train %>% select(rec_12m_przed_2017, splaty_sum_3m_2017, recovery_exposure_weight_binary_01_20), target = 'recovery_exposure_weight_binary_01_20' )
# 
# rf_cat_1_$F_learner()
# 
# 
# rf_cat_1_$F_build_holdout(threshold = c(`0`=0.1, `1`=0.5, `2`=0.4), keep.pred = TRUE, keep.models = TRUE, predict = 'both', keep.confussion_matrix = TRUE)
# 
# rf_cat_1_$F_summary_1(build_type = 'build_holdout', learning_curve = TRUE)
# 
# rf_cat_1_$F_effect_ALE(build_type = 'build_holdout', feature = 'splaty_sum_3m_2017', plot = TRUE)
# 
# 
# rf_cat_1_$F_interaction(build_type = 'build_holdout')
# 
# rf_cat_1_$F_surrogate_global(build_type = 'build_holdout')
# 
# rf_cat_1_$F_surrogate_local(build_type = 'build_holdout', obs_nr = 10)
# 
# rf_cat_1_$F_shapley(build_type = 'build_holdout', obs_nr = 10)
# 
# 
# rf_cat_1_$F_build_manual_train()
# 
# rf_cat_1_$F_build_manual_predict(data_test = data_test)
# 
# rf_cat_1_$F_build_bootstrapOOB(iters = 2, keep.pred = TRUE)
# 
# rf_cat_1_$F_build_holdout(split = 2/3, keep.pred = TRUE)
# 
# rf_cat_1_$F_build_cv(iters = 2, keep.pred = TRUE)
# 
# rf_cat_1_$F_build_subsample(iters = 2)
# 
# rf_cat_1_$F_resampling_holdout(models = TRUE)
# 
# rf_cat_1_$resampling_holdout$models[[1]]$learner.model
# 
# 
# rf_cat_1_$F_calibration()
# 
# rf_cat_1_$F_plot_measures()







classif.mlr_ <- R6::R6Class(  
  
  classname = 'classif.mlr'
  
  , public = list(
    
    # VARIABLES:
    
    learnes_info = tibble::tribble(~learner, ~package, ~description
                                   ,'classif.randomForest' , 'randomFores', ''
                                   ,'classif.ada'          ,'','' 
                                   ,'classif.boosting'     ,'','' 
                                   ,'classif.gbm'          ,'','' 
                                   ,'classif.naiveBayes'   ,'','' 
                                   ,'classif.nnet'         ,'','' 
                                   ,'classif.svm'          ,'','' 
                                   ,'classif.lda'          ,'','' 
                                   ,'classif.saeDNN'       ,'','' 
                                   ,'classif.multinom'     ,'nnet', 'multinominal regression'
    )
    
    , task          = NULL
    , task_manual_test  = NULL
    , learner_name = 'classif.randomForest'
    , learner = NULL # slot for learner OBJECT
    , learner_params = list()
    , learner_params_tuned = NULL
    , obs_to_remove = NULL # logical vector
    , default_build_type = 'build_manual'
    , build_manual = list(   learner.id = NULL
                             , comment = NULL  # added slot
                             , task.id=NULL
                             , task.desc=NULL      
                             
                             , measures.train = NULL
                             , measures.train.extra = NULL
                             , measures.train.model = NULL
                             , measures.train.model.extra = NULL
                             
                             , measures.test = NULL
                             , measures.test.extra = NULL # added slot
                             , measures.test.model = NULL # added slot
                             , measures.test.model.extra = NULL  # added slot
                             
                             , confussion_matrix_train = NULL # added slot
                             , confussion_matrix_test = NULL # added slot
                             
                             , aggr = NULL
                             # , pred_train = NULL # addes slot
                             , pred = NULL
                             , models = NULL         
                             , err.msgs  = NULL
                             , err.dumps = NULL
                             , extract   = NULL
                             , runtime   = NULL  
    )
    
    , build_holdout     = NULL
    , build_subsample   = NULL
    , build_bootstrapOOB = NULL
    , build_cv          = NULL
    
    , threshold         = NULL
    
    , woe_info = NULL
    
    , measures_list = list(   mlr::kappa
                              , mlr::multiclass.au1u # Weighted average 1 vs. 1 multiclass AUC
                              , mlr::mmce # mean misclassification error
                              , mlr::multiclass.brier # Multiclass Brier score
    )
    
    
    # FUNCTIONS:
    
    
    
    # STAGE 1 - model preparation (task and learner) ----
    
    
    # task and learner definition
    , F_task = function(  
      data      = NULL  # data set
      , target    = NA    # name of target variable
      , use_tuned = FALSE # if use parameters from hypertunning
      , ID        = NULL  # ID for model
      
      # under/over sampling
      , undersampling_basic_prop = NULL
      , oversampling_basic_prop  = NULL
      , sampling_var             = NULL # var for over/under sampling
      
      # SMOTE
      , SMOTE_prop = NULL # Synthetic Minority Oversampling Technique
      , SMOTE_nn   = 5 # numer of neighbours for KNN method used by SMOTE
      
    ){
      
      
      #> setting names and comments
      date_time <- stringr::str_replace_all(as.character(Sys.time()), c('-'='_', ' '='_H_', ':'='_') )
      ID     <- if(is.null(ID)) paste0('t_',  date_time) else paste0(ID, '_', date_time)
      #<
      
      
      
      # task
      self$task <- mlr::makeClassifTask(  id     = ID
                                          , data   = data
                                          , target = target
      )  
      
      
      # optional undersampling of task
      if(!is.null(undersampling_basic_prop)){
        self$task <- mlr::undersample(  self$task
                                        , rate = undersampling_basic_prop
                                        , cl   = sampling_var # values [0,1]
        ) 
      }
      
      # optional oversampling of task
      if(!is.null(oversampling_basic_prop)){
        self$task <- mlr::oversample(  self$task
                                       , rate = oversampling_basic_prop
                                       , cl   = sampling_var # values > 1
        ) 
      }
      
      # optional SMOTE of task
      if(!is.null(SMOTE_prop)){
        self$task <- mlr::smote(  self$task
                                  , rate = SMOTE_prop
                                  , nn   = SMOTE_nn)
      }
      
      
    }
    
    
    # learner
    , F_learner = function(  learner      = NULL
                             , use_tuned    = FALSE
                             , predict.type = 'prob'
                             , threshold    = NULL
    ){
      
      #learner
      self$learner <- mlr::makeLearner(  self$learner_name
                                         , predict.type = predict.type
                                         , par.vals     = self$learner_params
                                         , predict.threshold = threshold
      )
      self$threshold <- threshold
      
      # optional use of hyper tuned parameters
      if(use_tuned){
        self$learner <- mlr::setHyperPars(  learner  = if(is.null(learner)) self$learner_name else learner
                                            , par.vals = self$learner_params_tuned)
      }
      
    }
    
    
    
    
    
    
    # STAGE 2 - model training and prediction ----
    
    
    , F_build_manual_train = function(  
      target     = NA
      , features   = NULL
      , comment    = ''
      , remove_obs = FALSE
      , add_train_model_to_list = TRUE){
      
      # comment
      self$build_manual$comment <- comment
      
      # subseting features
      if(!is.null(features)) self$task <- mlr::subsetTask(self$task, features = features)
      
      # train
      self$build_manual$models[[1]] <- mlr::train( self$learner
                                                   , self$task
                                                   , subset = if(remove_obs) self$obs_to_remove else NULL 
      )
      
      
      
    }
    
    
    , F_build_manual_predict = function(  threshold = NULL
                                          , ID     = NULL
                                          , data_test = NULL){
      
      #> id of the prediction
      date_time <- stringr::str_replace_all(as.character(Sys.time()), c('-'='_', ' '='_H_', ':'='_') )
      ID   <- if(is.null(ID)) paste0('p_',  date_time) else paste0(ID, '_', date_time)
      #<
      
      # task for prediction
      self$task_manual_test <- mlr::makeClassifTask(  id     = ID
                                                      , data   = data_test
                                                      , target = mlr::getTaskTargetNames(self$task))
      
      #> prediction
      if(is.null(threshold)){
        pred_train <- predict(self$build_manual$models[[1]], self$task)
        pred_test <- predict(self$build_manual$models[[1]], self$task_manual_test)
      }else{
        
        pred_train <- mlr::setThreshold(  predict(self$build_manual$models[[1]], self$task)
                                          , threshold = threshold)
        pred_test <- mlr::setThreshold(  predict(self$build_manual$models[[1]], self$task_manual_test)
                                         , threshold = threshold)
      }
      
      
      #> modifying prediction tables
      pred_train$data %<>% dplyr::mutate(iter = 1, set = 'train')
      pred_test$data  %<>% dplyr::mutate(iter = 1, set = 'test')
      pred <- pred_train
      pred$data <- rbind(pred_train$data, pred_test$data)
      self$build_manual$pred <- pred
      #<
      
      
      #> confussion matrices
      self$build_manual$confussion_matrix_train[[1]] <- mlr::calculateConfusionMatrix(pred = pred_train)
      self$build_manual$confussion_matrix_test[[1]]  <- mlr::calculateConfusionMatrix(pred = pred_test)
      #<
      
      
      #> performance
      self$build_manual$measures.train <- c(mlr::performance(  pred = pred_train 
                                                               , measures = self$measures_list
                                                               , task = self$task
                                                               , model = self$build_manual$model[[1]]), iter = 1) %>% t() %>% as.data.frame()
      self$build_manual$measures.test <- c(mlr::performance(  pred =  pred_test
                                                              , measures = self$measures_list
                                                              , task = self$task
                                                              , model = self$build_manual$model[[1]]
      ), iter = 1) %>% t() %>% as.data.frame()
      #<
      
      
      
    }
    
    
    , F_build_holdout = function(  iters = 10
                                   , split = 2/3
                                   , keep.pred = FALSE
                                   , keep.models = FALSE
                                   , keep.confussion_matrix = FALSE
                                   , measures_to_print = NULL
                                   , predict = 'test'
                                   , threshold = NULL){
      
      
      
      res_desc <- mlr::makeResampleDesc(  method = 'Holdout'
                                          , predict = predict
                                          , iters = iters
      )
      
      if(!is.null(threshold)) self$learner$predict.threshold <- threshold
      
      self$build_holdout <- mlr::resample(    learner = self$learner
                                              , task = self$task
                                              , measures = self$measures_list
                                              , resampling = res_desc
                                              , keep.pred = TRUE
                                              , models = keep.models
      )
      
      if(keep.confussion_matrix) self$F_resample_performance(build_type = 'build_holdout', predict = predict, conf_matrix = TRUE)
      
      if(!keep.pred) self$build_holdout$pred <- NULL
    }
    
    
    , F_build_cv = function(  iters = 10
                              , keep.pred = FALSE
                              , keep.models = FALSE
                              , keep.confussion_matrix = FALSE
                              , sort_prediction = FALSE
                              , predict = 'test'
                              , threshold = NULL
    ){
      
      res_desc <- mlr::makeResampleDesc(  method = 'CV'
                                          , predict = predict
                                          , iters = iters
                                          
      )
      
      if(!is.null(threshold)) self$learner$predict.threshold <- threshold
      
      self$build_cv <- mlr::resample(  learner = self$learner
                                       , task = self$task
                                       , measures = self$measures_list
                                       , resampling = res_desc
                                       , keep.pred = TRUE
                                       , models = keep.models
      )
      
      if(keep.confussion_matrix) self$F_resample_performance(build_type = 'build_cv', predict = predict, conf_matrix = TRUE)
      
      if(!keep.pred) self$build_cv$pred <- NULL
      
      if(sort_prediction & keep.pred){
        if(!is.null(self$build_cv$pred$data)){
          self$build_cv$pred$data <- self$build_cv$pred$data %>% dplyr::arrange(set, id) # nie sortowac po iter
        }else{
          warning('Prediction data were not sorted. Probably keep.pred = FALSE')
        }
      }
    }
    
    
    , F_build_bootstrapOOB = function(  iters = 10
                                        , keep.pred = FALSE
                                        , keep.models = FALSE
                                        , keep.confussion_matrix = FALSE
                                        , predict = 'test'
                                        , threshold = NULL){
      
      res_desc <- mlr::makeResampleDesc(  method = 'Bootstrap'
                                          , predict = predict
                                          , iters = iters
      )
      
      if(!is.null(threshold)) self$learner$predict.threshold <- threshold
      
      self$build_bootstrapOOB <- mlr::resample(  learner = self$learner
                                                 , task       = self$task
                                                 , measures   = self$measures_list
                                                 , resampling = res_desc
                                                 , keep.pred  = TRUE
                                                 , models     = keep.models
      )
      
      if(keep.confussion_matrix) self$F_resample_performance(build_type = 'build_bootstrapOOB', predict = predict, conf_matrix = TRUE)
      
      if(!keep.pred) self$build_bootstrapOOB$pred <- NULL
      
    }
    
    
    
    , F_build_subsample = function(  iters       =A 10
                                     , split       = 2/3
                                     , keep.pred   = FALSE
                                     , keep.models = FALSE
                                     , keep.confussion_matrix = FALSE
                                     , predict     = 'test'
                                     , threshold   = NULL){
      
      res_desc <- mlr::makeResampleDesc(  method = 'Subsample'
                                          , predict = predict
                                          , iters = iters
      )
      
      if(!is.null(threshold)) self$learner$predict.threshold <- threshold
      
      self$build_subsample <- mlr::resample(  learner = self$learner
                                              , task = self$task
                                              , measures = self$measures_list
                                              , resampling = res_desc
                                              , keep.pred = TRUE
                                              , models = keep.models
      )
      
      if(keep.confussion_matrix) self$F_resample_performance(build_type = 'build_subsample', predict = predict, conf_matrix = TRUE)
      
      if(!keep.pred) self$build_subsample$pred <- NULL
    }
    
    
    # STAGE 3 models performance and problems diagnostics -----
    
    , F_multicolinearity = function(){
      
    }
    
    , F_outliers = function(){
      
    }
    
    , F_heteroscedastity = function(){
      
    }
    
    
    # oparte tylko o task i learner
    , F_feature_importance = function(){
      
      generateFeatureImportanceData(task = self$task, learner = self$learner)
      
    }
    
    
    # don't use this function for build_manual !!!
    , F_resample_performance = function(build_type = 'build_cv', predict = 'test', conf_matrix = TRUE){
      
      pred_i <- self[[build_type]]$pred
      
      # train
      if(predict == 'train' | predict == 'both'){
        for(i in 1:max(self[[build_type]]$pred$data$iter)){
          pred_i$data <- self[[build_type]]$pred$data %>% dplyr::filter(iter == i & set == 'train')
          # if(i == 1){
          #   perf_data_train <- c(mlr::performance(pred_i, measures = self$measures_list), iter = 1)
          # }else{
          #   perf_data_train <- rbind(  perf_data_train
          #                            , c(mlr::performance(pred_i, measures = self$measures_list), iter = i)
          #                            )
          # }
          if(conf_matrix) self[[build_type]]$confussion_matrix_train[[i]] <- mlr::calculateConfusionMatrix(pred = pred_i)
        }
        # self[[build_type]]$measures.train <- perf_data_train
        # rm(perf_data_train)
      }
      
      
      # test
      if(predict == 'test' | predict == 'both'){
        for(i in 1:max(self[[build_type]]$pred$data$iter)){
          pred_i$data <- self[[build_type]]$pred$data %>% dplyr::filter(iter == i & set == 'test')
          # if(i == 1){
          #   perf_data_test <- c(mlr::performance(pred_i, measures = self$measures_list ), iter = 1)
          # }else{
          #   perf_data_test <- rbind(  perf_data_test
          #                          , c(mlr::performance(pred_i, measures = self$measures_list ), iter = i)
          #                          )
          # }
          if(conf_matrix) self[[build_type]]$confussion_matrix_test[[i]] <- mlr::calculateConfusionMatrix(pred = pred_i)
        }
        # self[[build_type]]$measures.test <- perf_data_test
      }
      
      
    }
    
    , F_plot_measures = function(  build_type =  self$default_build_type
                                   , measures_to_print = NULL  # if NULL all measures is printed
    ){
      
      if(is.null(measures_to_print)){
        self[[build_type]]$measures.test %>% 
          as.data.frame() %>%
          tidyr::gather(key = 'key', value = 'value', -iter) %>% 
          {ggplot(data=., aes(y=value, x=iter, colour=key)) + geom_line() + geom_point()}
        
      }else{
        self[[build_type]]$measures.test %>% 
          as.data.frame() %>% 
          dplyr::select(one_of(measures_to_print, 'iter')) %>%  
          tidyr::gather(key = 'key', value = 'value', -iter) %>% 
          {ggplot(data=., aes(y=value, x=iter, colour=key)) + geom_line() + geom_point()}
        
      }
    }
    
    , F_effect_plot_DALEX = function(  build_type = 'build_manual'
                                       , iter = 1
                                       , var = ''
                                       , x_min = NULL
                                       , x_max = NULL
                                       , unlog = NULL
                                       , type = 'pdp' # ale; pdp
    ){
      
      explain  <- DALEX::explain(getLearnerModel(self[[build_type]]$models[[iter]] ))
      
      if(is.null(explain)) warning('you probably had not kept models results!!!')
      
      response <- DALEX::variable_response(explain = explain, variable = var, prob = TRUE, type = type)
      
      if(!is.null(x_min)){
        response <- response[response$x >= x_min,]
      }
      
      if(!is.null(x_max)){
        response <- response[response$x <= x_max,]
      }
      
      if(!is.null(unlog)){
        response$y <- log(response$y)
      }
      
      # response
      plot(response)
    }
    
    
    # warning analysis based on data from task!!! (problem for compatibylity with resample)
    , F_effect_plot_IML = function(data = NULL
                                   , build_type = 'build_manual'
                                   , feature = NA
                                   , plot = TRUE
                                   , iter = 1
                                   , method = 'ale' # ale; pdp; ice; pdp+ice
                                   , grid.size = 20
    ){
      
      predictor <- iml::Predictor$new( if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                       , data = mlr::getTaskData(self$task)
                                       , y = mlr::getTaskTargetNames(self$task))
      
      ale <- iml::FeatureEffect$new(predictor = predictor
                                    , feature = feature
                                    , method = method
                                    , grid.size = grid.size
      )
      print(ale)
      if(plot) ale$plot()
    }
    
    
    , F_surrogate_global = function(  data = NULL
                                      , build_type = 'build_manual'
                                      , maxdepth = 2
                                      , plot = TRUE
                                      , iter = 1){
      
      predictor <- iml::Predictor$new( if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                       , data = mlr::getTaskData(self$task)
                                       , y = mlr::getTaskTargetNames(self$task))
      
      tree_surrogate <- iml::TreeSurrogate$new(predictor = predictor, maxdepth = maxdepth)
      print(tree_surrogate)
      if(plot) plot(tree_surrogate)
      
    }
    
    , F_interaction = function(data = NULL, build_type = 'build_manual', iter = 1, plot = TRUE){
      
      predictor <- iml::Predictor$new( if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                       , data = mlr::getTaskData(self$task)
                                       , y = mlr::getTaskTargetNames(self$task))
      
      interaction <- iml::Interaction$new(predictor = predictor)
      if(plot) plot(interaction)
      
    }
    
    , F_surrogate_local = function(data = NULL, build_type = 'build_manual', obs_nr = 1, k = 2, plot = TRUE, iter = 1){
      
      predictor <- iml::Predictor$new( if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                       , data = mlr::getTaskData(self$task)
                                       , y = mlr::getTaskTargetNames(self$task))
      
      local = iml::LocalModel$new(  predictor
                                    , x.interest = mlr::getTaskData(self$task)[   obs_nr # nr obserwacji
                                                                                  , -which(names(mlr::getTaskData(self$task))==mlr::getTaskTargetNames(self$task)) ] 
                                    , k = k)
      
      print(local)
      if(plot) plot(local)
    }
    
    , F_shapley = function(  data = NULL
                             , build_type = 'build_manual'
                             , obs_nr = 1
                             , plot = TRUE
                             , iter = 1){
      
      predictor <- iml::Predictor$new(  if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                        , data = mlr::getTaskData(self$task)
                                        , y = mlr::getTaskTargetNames(self$task))
      
      shapely = iml::Shapley$new(  predictor
                                   , x.interest = mlr::getTaskData(self$task)[   obs_nr # nr obserwacji
                                                                                 , -which(names(mlr::getTaskData(self$task))==getTaskTargetNames(self$task)) ] 
      )
      
      print(shapely)
      if(plot) plot(shapely)
      
    }
    
    
    , F_classification_visialisation = function(  vars_two = NULL
                                                  , pointsize = 1){
      
      mlr::plotLearnerPrediction(  learner = self$learner
                                   , task = self$task
                                   , features = vars_two
                                   , pointsize = pointsize)
      
    }
    
    
    ,  F_conf_matrix_function = function(  build_type =  self$default_build_type
                                           , iter = 1
                                           , mlr_conf_matrix = 'test'
    ){
      
      if(mlr_conf_matrix == 'train'){
        mlr_conf_matrix <- self[[build_type]]$confussion_matrix_train[[iter]]$result
      }else if(mlr_conf_matrix == 'test'){
        mlr_conf_matrix <- self[[build_type]]$confussion_matrix_test[[iter]]$result
      }else{
        mlr_conf_matrix <- mlr_conf_matrix
      }
      
      # mlr_conf_matrix <- rf4a$statistics_PR_basic$confusion_PR$result
      mlr_conf_matrix <- mlr_conf_matrix[-nrow(mlr_conf_matrix), -ncol(mlr_conf_matrix)]
      
      rownames_ <- paste(rownames(mlr_conf_matrix), 'true', sep='_')
      colnames_ <- paste(colnames(mlr_conf_matrix), 'pred', sep='_')
      
      
      diagonal <- diag(mlr_conf_matrix)
      
      row_sum <- apply(mlr_conf_matrix, MARGIN = 1, FUN = sum) #wierszowe
      col_sum <- apply(mlr_conf_matrix, MARGIN = 2, FUN = sum) #kolumnowe
      
      row_correct <- round(diagonal/row_sum * 100)
      col_correct <- round(diagonal/col_sum * 100)
      
      mlr_conf_matrix <- rbind(mlr_conf_matrix, correct=col_correct)
      mlr_conf_matrix <- cbind(mlr_conf_matrix, correct=c(row_correct,NA))
      
      mlr_conf_matrix %<>% as.data.table
      rownames(mlr_conf_matrix) <- c(rownames_, 'proc_correct')
      colnames(mlr_conf_matrix) <- c(colnames_, 'proc_correct')
      
      mlr_conf_matrix %>% tibble::rownames_to_column(var = ' ')
      
    }
    
    , F_prediction_density = function(  build_type = 'build_manual'
                                        , set = 'test'
                                        , prob_nr = 1
                                        , adjust = 0.5){
      
      prob <- names(self[[build_type]]$pred$data)[prob_nr+2]
      
      
      print(ggplot( data = self[[build_type]]$pred$data[ self[[build_type]]$pred$data$set == set ,] ) +
              geom_density(aes_string(x=prob, colour = 'truth'), adjust = adjust) +
              geom_vline(aes(xintercept = self[[build_type]]$pred$threshold[prob_nr]), linetype = 'dashed', size = 1) +
              facet_grid(.~iter) +
              scale_color_brewer(palette="Dark2") +
              theme_minimal() +
              ggtitle('probability density by dependent variable categories'))
      
    }
    
    
    , F_prediction_histogram = function(  build_type = 'build_manual'
                                          , set = 'test'
                                          , prob_nr = 1
                                          , alpha = 1){
      
      prob <- names(self[[build_type]]$pred$data)[prob_nr+2]
      
      
      print(ggplot( data = self[[build_type]]$pred$data[ self[[build_type]]$pred$data$set == set ,] ) +
              geom_histogram(aes_string(x=prob, fill = 'truth'), alpha = alpha, position="dodge2") +
              geom_vline(aes(xintercept = self[[build_type]]$pred$threshold[prob_nr]), linetype = 'dashed', size = 1) +
              facet_grid(.~iter) +
              scale_color_brewer(palette="Dark2") +
              theme_minimal() +
              ggtitle('probability density by dependent variable categories'))
      
    }
    
    
    
    # oparte tylko o task i learner
    , F_learning_curve = function(){
      
      curve_data = generateLearningCurveData(
        learners    = self$learner
        ,task       = self$task
        ,percs      = seq(0.1, 1, by = 0.2)
        ,measures   = self$measures_list
        ,resampling = makeResampleDesc(method = "CV", iters = 5)
        ,show.info  = FALSE
      )
      print(mlr::plotLearningCurve(curve_data))
      
    }
    
    
    , F_breakDown = function(  build_type = self$default_build_type
                               , iter = 1
                               , obs_nr = 1){
      
      plot(breakDown::broken(  model = getLearnerModel(self[[build_type]]$models[[iter]] )
                               , new_observation = mlr::getTaskData(self$task)[obs_nr,], baseline = 'Intercept') ) 
    }
    
    # STAGE - other things
    
    # only for test
    , F_calibration = function(  build_type =  self$default_build_type
                                 , iter = 1
                                 , groups = NULL # The number of bins to construct.
    ){
      pred <- self[[build_type]]$pred
      pred$data %<>% dplyr::filter(iter == 1, set == 'test')
      
      cal <- mlr::generateCalibrationData(pred, groups = groups)
      mlr::plotCalibration(cal)
      
    }
    
    # STAGE 4 - summary
    
    , F_summary_1 = function(build_type = 'build_manual', iter = 1, learning_curve = FALSE){
      
      
      print(glue::glue('train set size:      {self$task$task.desc$size}'))
      print(glue::glue('threshold:           {self[[build_type]]$pred$threshold} \n\n'))
      
      # cat('\n\n\n<<<confusion matrix TRAIN set>>>\n\n')
      # print(self$F_conf_matrix_function(self[[build_type]]$confussion_matrix_train[[iter]]))
      print(self$F_conf_matrix_function(build_type = build_type, mlr_conf_matrix = 'train'))
      
      # cat('\n\n\n<<<confusion matrix PREDICTION set>>>\n\n')
      # print(self$F_conf_matrix_function(self[[build_type]]$confussion_matrix_test[[iter]]))
      print(self$F_conf_matrix_function(build_type = build_type, mlr_conf_matrix = 'test'))
      
      self$F_feature_importance()
      
      self$F_prediction_density(build_type = build_type, set = 'train', prob_nr = 1)
      
      self$F_prediction_density(build_type = build_type, set = 'test', prob_nr = 1)
      
      if(learning_curve) self$F_learning_curve()
      
      self$F_plot_measures(build_type = build_type)
      
      
      
      # ,  F_conf_matrix_function = function(  build_type =  self$default_build_type
      #                                        , iter = 1
      #                                        , mlr_conf_matrix = 'test'
      # ){
      
      
    }
    
  )   
  , private = list()
)




# classif.RF ----------

classif.RF.mlr_ <- R6::R6Class(
  classname = 'classif.RF.mlr_'
  , inherit   = classif.mlr_
  
  ,public = list(
    
    learner = 'classif.randomForest'
    , learner_params = list(  ntree = 50
                              , proximity = TRUE)
    
    , F_feature_importance_randomForest = function(build_type = 'build_manual', iter = 1, type = 1){
      warning('randomForest should be run with importance = TRUE')
      randomForest::importance(self[[build_type]]$models[[iter]]$learner.model, type = type)
      
    }
    
    , F_outliers = function(buile_type = self$default_build_type, iter = 1){
      if(is.null(getLearnerModel(self[[build_type]]$models[[iter]])$proximity)) stop('no proximity matrix calculated')
      plot(randomForest::outlier(getLearnerModel(self[[build_type]]$models[[iter]])$proximity))
    }
    
    , F_tree_plot = function(buile_type = self$default_build_type, iter = 1){
      plot(getLearnerModel(self[[build_type]]$models[[iter]]))
    }
    
    , F_min_dept = function(buile_type = self$default_build_type, iter = 1){
      
      min_depth <- randomForestExplainer::min_depth_distribution(getLearnerModel(self[[build_type]]$models[[iter]]))
      randomForestExplainer::plot_min_depth_distribution(min_depth)
      
    }
    
    , F_multi_way_importance = function(  buile_type = self$default_build_type
                                          , iter = 1){
      
      importance_frame <- randomForestExplainer::measure_importance(getLearnerModel(self[[build_type]]$models[[iter]]))
      print(randomForestExplainer::plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes"))
      print(randomForestExplainer::plot_importance_ggpairs(importance_frame))
      
    }
  )
  ,private = list()
  ,active = list()
)






# classif.binary ----------


# diamonds_bin <- diamonds[1:1000,]
# diamonds_bin$bin <- sample(factor(c(0,1)), size = nrow(diamonds_bin), replace = TRUE)
# 
# r1 <- classif_binary.mlr_$new()
# 
# r1$F_task(data = diamonds_bin, target = 'bin' )
# 
# r1$F_learner()
# 
# r1$F_build_holdout( keep.pred = TRUE )
# 
# r1$F_build_cv( keep.pred = TRUE )
# 
# 
# r1$F_ROC(build_type = 'build_cv', set = 'test')
# 
# r1$F_build_manual_predict(data_test = )



classif_binary.mlr_ <- R6::R6Class(
  
  classname = 'classif_binary.mlr'
  
  , inherit = classif.mlr_
  
  , public = list(
    
    measures_list = list(  kappa
                           , mlr::multiclass.au1u # Weighted average 1 vs. 1 multiclass AUC
                           , mlr::mmce # mean misclassification error
                           , mlr::multiclass.brier # Multiclass Brier score
                           , mlr::tnr
                           , mlr::ppv
                           , mlr::npv
                           , mlr::tpr
    )
    
    , F_ROC = function(build_type = self$default_build_type, set = 'test'){
      
      pred <- self[[build_type]]$pred
      pred$data <- pred$data[pred$data$set == set,]
      df = mlr::generateThreshVsPerfData(pred, measures = list(fpr, tpr), aggregate = FALSE)
      
      # Precision/recall graph
      mlr::plotROCCurves(df, diagonal = TRUE)
      
    }
  )
)





# classif_binary.RF ----------

classif_binary.RF.mlr_ <- R6::R6Class(
  classname = 'classif_binary.mlr'
  , inherit   = classif_binary.mlr_
  
  ,public = list(
    
    learner = 'classif.randomForest'
    , learner_params = list(ntree = 50, proximity = TRUE)
    
    
    , F_feature_importance_randomForest = function(build_type = 'build_manual', iter = 1, type = 1){
      warning('randomForest should be run with importance = TRUE')
      randomForest::importance(self[[build_type]]$models[[iter]]$learner.model, type = type)
      
    }
    
    
    , F_outliers = function(  buile_type = self$default_build_type
                              , iter = 1
    ){
      if(is.null(getLearnerModel(self[[build_type]]$models[[iter]])$proximity)) stop('no proximity matrix calculated')
      plot(randomForest::outlier(getLearnerModel(self[[build_type]]$models[[iter]])$proximity))
    }
    
    , F_tree_plot = function(buile_type = self$default_build_type, iter = 1){
      plot(getLearnerModel(self[[build_type]]$models[[iter]]))
    }
    
    , F_min_dept = function(buile_type = self$default_build_type, iter = 1){
      
      min_depth <- randomForestExplainer::min_depth_distribution(getLearnerModel(self[[build_type]]$models[[iter]]))
      randomForestExplainer::plot_min_depth_distribution(min_depth)
      
    }
    
    , F_multi_way_importance = function(buile_type = self$default_build_type, iter = 1){
      
      importance_frame <- randomForestExplainer::measure_importance(getLearnerModel(self[[build_type]]$models[[iter]]))
      print(randomForestExplainer::plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes"))
      print(randomForestExplainer::plot_importance_ggpairs(importance_frame))
      
    }
  )
  ,private = list()
  ,active = list()
)



# classif_binary.GLM ----------


classif_binary.GLM.mlr <- R6::R6Class(
  classname = 'classif_binary.GLM.mlr'
  , inherit   = classif_binary.mlr_
  , public = list(
    
    learner_type   = 'classif.logreg'
    
    ,F_resample_performance_model = function(build_type = self$default_build_type, iter = 1){
      
      # UWAGA: przyidealnej wspliniowosci mozemy dostac taki blad:
      # Error in vif.default(getLearnerModel(self$train)) : 
      #   there are aliased coefficients in the model
      
      vif <- car::vif(getLearnerModel(self[[build_type]]$models[[iter]]))
      
      model_summary <- summary(getLearnerModel(self[[build_type]]$models[[iter]]))
      pseudo_r2     <- BaylorEdPsych::PseudoR2(getLearnerModel(self[[build_type]]$models[[iter]]))
      # gof <- LogisticDx::gof(getLearnerModel(self[[build_type]]$models[[iter]]))
      
      self[[build_type]]$measures.train.model <- list(  
        coef = model_summary$coefficients
        , different_stats1 = list(vif = vif)
        , different_stats2 = c(  
          log_ML = logLik(getLearnerModel(self[[build_type]]$models[[iter]])) #logarithm of max likehood
          , pseudo_r2[names(pseudo_r2) %in% c(  'McFadden'
                                                , 'Adj.McFadden'
                                                , 'Effron'
                                                , 'AIC'
                                                , 'Corrected.AIC')] )
      )
      
    }
    
    
    , F_outliers = function(build_type = self$default_build_type, iter = 1){
      
      print(plot(cooks.distance(self[[build_type]]$models[[iter]]$learner.model)))
      densityplot(cooks.distance(model = getLearnerModel(self[[built_type]]$models[[iter]])))
      
    }
    
    , F_outliers_extract = function(treshold){
      
      self$obs_to_remove <- cooks.distance(model = getLearnerModel(self[[built_type]]$models[[iter]])) < treshold
      
    }
    
    # , F_multicolinearity = function(){
    #     
    # }
    
    , F_effects = function(features = NULL, build_type = self$default_build_type, iter = 1){
      
      if(is.null(features)){
        plot(effects::allEffects(mod = getLearnerModel(self[[built_type]]$models[[iter]])))
      }else{
        plot(effects::effect(term = features, mod = getLearnerModel(self[[built_type]]$models[[iter]])))
      }
    }
    
    , F_coefficients = function(build_type = self$default_build_type, iter = 1){
      
      dotwhisker::dwplot(self[[built_type]]$models[[iter]]$learner.model)
      
    }
    
    
    , F_summary_model = function(build_type = 'build_manual', iter = 1){
      
      F_resample_performance_model(build_type = build_type, iter = iter)
      self[[build_type]]$measures.train.model
      
    }
    
    
  )
  , private = list()
  , active = list()
)












# regression ----





regr.mlr <- R6::R6Class(  
  
  classname = 'regr.mlr'
  
  , public = list(
    
    # VARIABLES:
    
    learnes_info = tibble::tribble(~learner, ~package, ~description
                                   ,'regr.bst' , '', ''
                                   ,'regr.fnn'          ,'','' 
                                   ,'regr.gamboost'     ,'mboost','Gradient Boosting with Smooth Components' 
                                   ,'regr.gbm'          ,'gbm','Gradient Boosting Machine' 
                                   ,'regr.kknn'   ,'kknn','K-Nearest-Neighbor regression' 
                                   ,'regr.ksvm'         ,'kernlab','Support Vector Machines' 
                                   ,'regr.lm'          ,'stats','Simple Linear Regression' 
    )
    
    , task          = NULL
    , task_manual_test  = NULL
    , learner_name = 'regr.lm'
    , learner = NULL
    , learner_params = list()
    , learner_params_tuned = NULL
    , obs_to_remove = NULL # logical vector
    , default_build_type = 'build_manual'
    , build_manual = list(   learner.id = NULL
                             , comment = NULL  # added slot
                             , task.id=NULL
                             , task.desc=NULL      
                             
                             , measures.train = NULL
                             , measures.train.extra = NULL
                             , measures.train.model = NULL
                             , measures.train.model.extra = NULL
                             
                             , measures.test = NULL
                             , measures.test.extra = NULL # added slot
                             , measures.test.model = NULL # added slot
                             , measures.test.model.extra = NULL  # added slot
                             
                             , confussion_matrix_train = NULL # added slot
                             , confussion_matrix_test = NULL # added slot
                             
                             , aggr = NULL
                             # , pred_train = NULL # addes slot
                             , pred = NULL
                             , models = NULL         
                             , err.msgs  = NULL
                             , err.dumps = NULL
                             , extract   = NULL
                             , runtime   = NULL  
    )
    
    , build_holdout     = NULL
    , build_subsample   = NULL
    , build_bootstrapOOB = NULL
    , build_cv          = NULL
    
    , threshold         = NULL
    
    , woe_info = NULL
    
    , measures_list = list(  arsq # Adjusted coefficient of determination
                             ,expvar # explained variance
                             ,mae # mean of abolute error
                             ,mape # mean absolute percentage error
                             ,medse # median of squared errors
                             ,rmse # root mean squared error
                             # ,rmsle
    )
    
    
    # FUNCTIONS:
    
    
    # STAGE 1 - model preparation (task and learner) -----
    
    
    # task and learner definition
    , F_task = function(  
      data      = NULL  # data set
      , target    = NA    # name of target variable
      , use_tuned = FALSE # if use parameters from hypertunning
      , ID        = NULL  # ID for model
      
    ){
      
      
      #> setting names and comments
      date_time <- stringr::str_replace_all(as.character(Sys.time()), c('-'='_', ' '='_H_', ':'='_') )
      ID     <- if(is.null(ID)) paste0('t_',  date_time) else paste0(ID, '_', date_time)
      #<
      
      
      # task
      self$task <- mlr::makeRegrTask(  id     = ID
                                       , data   = data
                                       , target = target
      )  
      
      
    }
    
    
    # learner
    , F_learner = function(  learner    = NULL
                             , use_tuned  = FALSE
    ){
      
      #learner
      self$learner <- mlr::makeLearner(  if(is.null(learner)) self$learner_name else learner
                                         , par.vals     = self$learner_params)
      
      
      # optional use of hyper tuned parameters
      if(use_tuned){
        selft$learner <- mlr::setHyperPars(  learner  = if(is.null(learner)) self$learner else learner
                                             , par.vals = self$learner_params_tuned)
      }
      
    }
    
    
    
    
    
    
    # STAGE 2 - model training and prediction ----
    
    
    , F_build_manual_train = function(  target     = NA
                                        , features   = NULL
                                        , comment    = ''
                                        , remove_obs = FALSE
                                        , add_train_model_to_list = TRUE){
      
      # comment
      self$build_manual$comment <- comment
      
      # subseting features
      if(!is.null(features)) self$task <- mlr::subsetTask(self$task, features = features)
      
      # train
      self$build_manual$models[[1]] <- mlr::train( self$learner
                                                   , self$task
                                                   , subset = if(remove_obs) self$obs_to_remove else NULL 
      )
      
      
      
    }
    
    
    , F_build_manual_predict = function(  threshold = NULL
                                          , ID     = NULL
                                          , data_test = NULL){
      
      #> id of the prediction
      date_time <- stringr::str_replace_all(as.character(Sys.time()), c('-'='_', ' '='_H_', ':'='_') )
      ID   <- if(is.null(ID)) paste0('p_',  date_time) else paste0(ID, '_', date_time)
      #<
      
      # task for prediction
      self$task_manual_test <- mlr::makeClassifTask(  id     = ID
                                                      , data   = data_test
                                                      , target = mlr::getTaskTargetNames(self$task))
      
      #> prediction
      if(is.null(threshold)){
        pred_train <- predict(self$build_manual$models[[1]], self$task)
        pred_test <- predict(self$build_manual$models[[1]], self$task_manual_test)
      }else{
        
        pred_train <- mlr::setThreshold(  predict(self$build_manual$models[[1]], self$task)
                                          , threshold = threshold)
        pred_test <- mlr::setThreshold(  predict(self$build_manual$models[[1]], self$task_manual_test)
                                         , threshold = threshold)
      }
      
      
      #> modifying prediction tables
      pred_train$data %<>% dplyr::mutate(iter = 1, set = 'train')
      pred_test$data  %<>% dplyr::mutate(iter = 1, set = 'test')
      pred <- pred_train
      pred$data <- rbind(pred_train$data, pred_test$data)
      self$build_manual$pred <- pred
      #<
      
      
      
      #> performance
      self$build_manual$measures.train <- c(mlr::performance(  pred = pred_train 
                                                               , measures = self$measures_list
                                                               , task = self$task
                                                               , model = self$build_manual$model[[1]]), iter = 1) %>% t() %>% as.data.frame()
      self$build_manual$measures.test <- c(mlr::performance(  pred =  pred_test
                                                              , measures = self$measures_list
                                                              , task = self$task
                                                              , model = self$build_manual$model[[1]]
      ), iter = 1) %>% t() %>% as.data.frame()
      #<
      
      
      
    }
    
    
    
    , F_build_holdout = function(  iters = 10
                                   , split = 2/3
                                   , keep.pred = FALSE
                                   , keep.models = FALSE
                                   , measures_to_print = NULL
                                   , predict = 'test'
    ){
      
      res_desc <- mlr::makeResampleDesc(  method = 'Holdout'
                                          , predict = predict
                                          , iters = iters
      )
      self$build_holdout <- mlr::resample(  learner = self$learner
                                            , task = self$task
                                            , measures = self$measures_list
                                            , resampling = res_desc
                                            , keep.pred = TRUE
                                            , models = keep.models
      )
      
      if(!keep.pred) self$build_holdout$pred <- NULL
      
    }
    
    
    , F_build_cv = function(  iters = 10
                              , keep.pred = FALSE
                              , keep.models = FALSE
                              , sort_prediction = FALSE
                              , predict = 'test'
    ){
      
      res_desc <- mlr::makeResampleDesc(  method = 'CV'
                                          , predict = predict
                                          , iters = iters
                                          
      )
      self$build_cv <- mlr::resample(  learner = self$learner
                                       , task = self$task
                                       , measures = self$measures_list
                                       , resampling = res_desc
                                       , keep.pred = TRUE
                                       , models = keep.models
      )
      
      if(!keep.pred) self$build_cv$pred <- NULL
      
      if(sort_prediction & keep.pred){
        if(!is.null(self$build_cv$pred$data)){
          self$build_cv$pred$data <- self$build_cv$pred$data %>% arrange(set, id) # nie sortowac po iter 
        }else{
          warning('Prediction data were not sorted. Probably keep.pred = FALSE')
        }
      }
    }
    
    
    
    
    , F_build_bootstrapOOB = function(  iters = 10
                                        , keep.pred = FALSE
                                        , keep.models = FALSE
                                        , predict = predict
    ){
      
      res_desc <- mlr::makeResampleDesc(  method = 'Bootstrap'
                                          , predict = predict
                                          , iters = iters
      )
      
      self$build_bootstrapOOB <- mlr::resample(  learner = self$learner
                                                 , task = self$task
                                                 , measures = self$measures_list
                                                 , resampling = res_desc
                                                 , keep.pred = TRUE
                                                 , models = keep.models
      )
      
      if(!keep.pred) self$build_bootstrapOOB$pred <- NULL
      
    }
    
    
    
    # STAGE 3 models performance and problems diagnostics -----
    
    , F_multicolinearity = function(){
      
    }
    
    , F_outliers = function(){
      
    }
    
    , F_heteroscedastity = function(){
      
    }
    
    
    # oparte tylko o task i learner
    , F_feature_importance = function(iterations = 50){
      
      mlr::generateFeatureImportanceData(  task = self$task
                                           , learner = self$learner
                                           , nmc = iterations)
      
    }
    
    
    , F_build_subsample = function(  iters = 10
                                     , split = 2/3
                                     , keep.pred = FALSE
                                     , keep.models = FALSE
                                     , predict = 'test'
    ){
      
      res_desc <- mlr::makeResampleDesc(  method = 'Subsample'
                                          , predict = 'test'
                                          , iters = iters
      )
      self$build_subsample <- mlr::resample(  learner = self$learner
                                              , task = self$task
                                              , measures = self$measures_list
                                              , resampling = res_desc
                                              , keep.pred = TRUE
                                              , models = keep.models
      )
      
      self$F_resample_performance(build_type = 'build_subsample', predict = predict)
      
      if(!keep.pred) self$build_subsample$pred <- NULL
      
    }
    
    , F_plot_measures = function(  build_type = 'build_manual'
                                   , measures_to_print = NULL  # if NULL all measires is printed
    ){
      
      if(is.null(measures_to_print)){
        self[[build_type]]$measures.test %>% 
          as.data.frame() %>% 
          tidyr::gather(key = 'key', value = 'value', -iter) %>% 
          {ggplot(data=., aes(y=value, x=iter, colour=key)) + geom_line() + geom_point()}
        
      }else{
        self[[build_type]]$measures.test %>% 
          as.data.frame() %>% 
          dplyr::select(one_of(measures_to_print, 'iter')) %>%  
          tidyr::gather(key = 'key', value = 'value', -iter) %>% 
          {ggplot(data=., aes(y=value, x=iter, colour=key)) + geom_line() + geom_point()}
        
      }
    }
    
    , F_effect_plot_DALEX = function(  build_type = 'build_manual'
                                       , iter = 1
                                       , var = ''
                                       , x_min = NULL
                                       , x_max = NULL
                                       , unlog = NULL
                                       , type = 'pdp' # ale; pdp
    ){
      
      explain  <- DALEX::explain(getLearnerModel(self[[build_type]]$models[[iter]] ))
      
      if(is.null(explain)) warning('you probably had not kept models results!!!')
      
      response <- DALEX::variable_response(explain = explain, variable = var, prob = TRUE, type = type)
      
      if(!is.null(x_min)){
        response <- response[response$x >= x_min,]
      }
      
      if(!is.null(x_max)){
        response <- response[response$x <= x_max,]
      }
      
      if(!is.null(unlog)){
        response$y <- log(response$y)
      }
      
      # response
      plot(response)
    }
    
    
    # warning analysis based on data from task!!! (problem for compatibylity with resample)
    , F_effect_plot_IML = function(  data = NULL
                                     , build_type = 'build_manual'
                                     , feature = NA
                                     , plot = TRUE
                                     , iter = 1
                                     , method = 'ale' # ale; pdp; ice; pdp+ice
                                     , grid.size = 20
    ){
      
      predictor <- iml::Predictor$new( if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                       , data = mlr::getTaskData(self$task)
                                       , y = mlr::getTaskTargetNames(self$task))
      
      ale <- iml::FeatureEffect$new(  predictor = predictor
                                      , feature = feature
                                      , method = method
                                      , grid.size = grid.size
      )
      print(ale)
      if(plot) ale$plot()
    }
    
    
    , F_surrogate_global = function(  data = NULL
                                      , build_type = 'build_manual'
                                      , maxdepth = 2
                                      , plot = TRUE
                                      , iter = 1){
      
      predictor <- iml::Predictor$new( if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                       , data = mlr::getTaskData(self$task)
                                       , y = mlr::getTaskTargetNames(self$task))
      
      tree_surrogate <- iml::TreeSurrogate$new(predictor = predictor, maxdepth = maxdepth)
      print(tree_surrogate)
      if(plot) plot(tree_surrogate)
      
    }
    
    , F_interaction = function(  data = NULL
                                 , build_type = 'build_manual'
                                 , iter = 1
                                 , plot = TRUE){
      
      predictor <- iml::Predictor$new( if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                       , data = mlr::getTaskData(self$task)
                                       , y = mlr::getTaskTargetNames(self$task))
      
      interaction <- iml::Interaction$new(predictor = predictor)
      if(plot) plot(interaction)
      
    }
    
    , F_surrogate_local = function(  data = NULL
                                     , build_type = 'build_manual'
                                     , obs_nr = 1
                                     , k = 2
                                     , plot = TRUE
                                     , iter = 1){
      
      predictor <- iml::Predictor$new( if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                       , data = mlr::getTaskData(self$task)
                                       , y = mlr::getTaskTargetNames(self$task))
      
      local = iml::LocalModel$new(  predictor
                                    , x.interest = mlr::getTaskData(self$task)[   obs_nr # nr obserwacji
                                                                                  , -which(names(mlr::getTaskData(self$task))==mlr::getTaskTargetNames(self$task)) ]
                                    , k = k)
      
      print(local)
      if(plot) plot(local)
    }
    
    , F_shapley = function( data = NULL
                            , build_type = 'build_manual'
                            , obs_nr = 1
                            , plot = TRUE
                            , iter = 1){
      
      predictor <- iml::Predictor$new(  if(is.null(data)) self[[build_type]]$models[[iter]] else data
                                        , data = mlr::getTaskData(self$task)
                                        , y = mlr::getTaskTargetNames(self$task))
      
      shapely = iml::Shapley$new(  predictor
                                   , x.interest = mlr::getTaskData(self$task)[   obs_nr # nr obserwacji
                                                                                 , -which(names(mlr::getTaskData(self$task))==getTaskTargetNames(self$task)) ] 
      )
      
      print(shapely)
      if(plot) plot(shapely)
      
    }
    
    
    
    # oparte tylko o task i learner
    , F_learning_curve = function(){
      
      curve_data = generateLearningCurveData(
        learners   = self$learner$id
        ,task       = self$task
        ,percs      = seq(0.1, 1, by = 0.2)
        ,measures   = self$measures_list
        ,resampling = makeResampleDesc(method = "CV", iters = 5)
        ,show.info  = FALSE
      )
      print(plotLearningCurve(curve_data))
      
    }
    
    , F_breakDown = function(  build_type = self$default_build_type
                               , iter = 1
                               , obs_nr = 1){
      
      plot(breakDown::broken(  model = getLearnerModel(self[[build_type]]$models[[iter]] )
                               , new_observation = mlr::getTaskData(self$task)[obs_nr,], baseline = 'Intercept') ) 
    }
    
    
    
    , F_plot_residuals = function(  build_type =  self$default_build_type
                                    , iter = 1
                                    , set = 'test'){
      
      
      pred <- self[[build_type]]$pred
      
      if(is.null(pred)) warning("pred is NULL. Probably you hadn't kept prediction results!!!")
      
      pred$data <- pred$data[pred$data$set == set & pred$data$iter == iter,]
      
      mlr::plotResiduals(pred, type = 'scatterplot', loess.smooth = TRUE, rug = TRUE , pretty.names = TRUE)
      
      
    }
  ))





# diamonds %<>% mutate_if(is.ordered, function(x) factor(x, ordered = FALSE) )
# 
# regr <- regr.mlr$new()
# regr$F_task(data = diamonds, target = 'price')
# regr$F_learner()
# 
# regr$F_build_subsample(iters = 2, keep.pred = TRUE, keep.models = TRUE)
# 
# regr$F_learning_curve()
# 
# regr$F_breakDown(build_type = 'build_subsample')
# 
# regr$build_subsample$models
# 
# # regr$F_plot_residuals(build_type = 'build_subsample')
# 
# regr$F_partial_plot_DALEX(build_type = 'build_subsample', var = 'x')