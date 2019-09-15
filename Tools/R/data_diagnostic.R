# R6 data object -----


data_obj <- R6::R6Class(
  
  classname = 'data_obj'
  
  ,public = list(
    
    data  = NULL
    , data2 = NULL
    , data3 = NULL
    
    
    
    , F_snippets = function(){
      cat(
        "\n mapowanie \n"
        ,"data %<>% dplyr::mutate(new_col = plyr::mapvalues(x = , from = c(1,2), to = c(1,1) ) )"
        ,"\n zamiana z NA \n"
        ,"data %<>% dplyr::mutate(col = tidyr::replace_na(var , replace = 'NA') )"
        ,"\n zamiana na NA \n"
        ,"data %<>% dplyr::mutate(col = case_when(cond_col ~ NA, TRUE ~ col))"
      )
    }
    
    
    , F_info = function(data=NULL){
      
      info_list <- list()
      
      if(is.null(data)){
        info_list[['dim']]         <- dim(self$data)
        info_list[['object_size']] <- pryr::object_size(self$data)
        info_list[['is_grouped']]  <- dplyr::is.grouped_df(self$data)
      }else{
        info_list[['dim']]         <- dim(data)
        info_list[['object_size']] <- pryr::object_size(data)
        info_list[['is_grouped']]  <- dplyr::is.grouped_df(data)
      }
      
      info_list
      
    }
    
    
    
    , F_metadata_small = function(data = NULL, view = TRUE){
      if(is.null(data)){
        names_ <- names(self$data)
        classes_ <- sapply(self$data, function(x) paste0(class(x), sep='_', collapse = '_') ) 
        NA_ <- sapply(self$data, function(x) sum(is.na(x)) )
      }else{
        names_ <- names(data)
        classes_ <- sapply(data, function(x) paste0(class(x), sep='_', collapse = '_') ) 
        NA_ <- sapply(data, function(x) sum(is.na(x)) )
      } 
      
      
      if(view){
        View(tibble(names = names_, class = classes_, NA_number = NA_))
      }else{
        print(tibble(names = names_, class = classes_, NA_number = NA_))
      }
      
      
    }
    
    
    , F_data_load = function(data, data1=NULL, data2=NULL){
      self$data = data
      if(!is.null(data1)) self$data1 <- data1
      if(!is.null(data2)) self$data2 <- data2
    }
    
    , F_metadata = function(){
      l.s::l.s.metadata.big(self$data)
    }
    
    , F_missing_amelia = function(vars = NULL){
      if(is.null(vars)){
        print(Amelia::missmap(self$data))
      }else{
        print(Amelia::missmap(self$data %>% select(!!!rlang::syms(vars))))
      }
      
    }
    
    
    , F_freq = function(data = NULL, vars = NULL, kable = TRUE, caption = ''){
      
      if(is.null(vars)) stop('no variables provided')
      
      if(!is.null(data)){
        freq_table <- data %>% group_by(!!!rlang::syms(vars)) %>% summarise(count = n()) %>% arrange(desc(count))
      }else{
        freq_table <- self$data %>% group_by(!!!rlang::syms(vars)) %>% summarise(count = n()) %>% arrange(desc(count))
      }
      
      if(kable) {
        kable(freq_table, caption = caption, format.args = list(big.mark=' '), digits = 0) %>% kable_styling(bootstrap_options = "striped", full_width = FALSE)
      }else{
        freq_table
      }
      
    }
    
    , F_missing_counts = function(data = NULL, vars = NULL, fill = NULL, group_x = NA, group_y = NA){
      
      
      # r1 <- dplyr::bind_rows(
      #   count  = diamonds %>% summarise_all(.funs = funs(sum(is.na(.))))
      #   ,percent = diamonds %>% summarise_all(.funs = funs(sum(is.na(.))/length(.)))
      #   ,.id = 'type'
      # )
      # r2 <- tidyr::gather(data = r1, key = 'key', value = 'value', -type )
      # tidyr::spread(data = r2, key = 'type', value = 'value')
      
      group <- private$group(fill = fill, group_x = group_x, group_y = group_y)
      
      if(is.null(data)){
        if(is.null(vars)){
          missing_counts <- group_by(self$data, !!!rlang::syms(group)) %>%
            summarise_all(.funs = funs(  count=sum(is.na(.))
                                         , percent=round(sum(is.na(.))/length(.),2)) )
        }else{
          missing_counts <- group_by(self$data, !!!rlang::syms(group)) %>%
            summarise_at(vars(!!vars), .funs = funs(  count=sum(is.na(.))
                                                      , percent=round(sum(is.na(.))/length(.),2)) )
        }
      }else{
        if(is.null(vars)){
          missing_counts <- group_by(data, !!!rlang::syms(group)) %>%
            summarise_all( .funs = funs(  count=sum(is.na(.))
                                          , percent=round(sum(is.na(.))/length(x),2)) )
        }else{
          missing_counts <- group_by(data, !!!rlang::syms(group)) %>%
            summarise_at(vars(!!vars), .funs = funs(  count=sum(is.na(.))
                                                      , percent=round(sum(is.na(.))/length(x),2)) )
        }
      }
      
      missing_counts
      
    }
    
    
    
    ,F_glimpse = function(data){
      
      tibble::glimpse(self$data)
      
    }
    
    ,F_point_plot = function(     x       = NA
                                  , y       = NA
                                  , data    = NULL
                                  , fill    = NULL
                                  , size    = 0.5
                                  , smooth  = FALSE
                                  , colour  = NULL
                                  , group_x = NA
                                  , group_y = NA
                                  , angle_x = 0
                                  , title   = ''
                                  , scale_free = c('fixed', 'free_x', 'free_y', 'free')
                                  , limits_x  = NULL
                                  , limits_y  = NULL
    ){
      
      if(is.na(group_x) & is.na(group_y)){
        grid <- NULL
      }else{
        formula_grid <- paste(  paste(tidyr::replace_na(group_x, '.'), collapse='+' )
                                , paste(tidyr::replace_na(group_y, '.'), collapse='+')
                                , sep = '~')
        grid <- facet_grid(formula_grid, scale = scale_free)
      }
      
      smooth <- if(smooth) geom_smooth() else NULL
      
      print(ggplot(data=if(is.null(data)) self$data else data, aes_string(x=x, y=y, colour=colour, fill=fill)) + 
              geom_point(size = size) + 
              grid +
              ggtitle(title) + 
              theme(axis.text.x = element_text(angle = angle_x)) + 
              scale_x_continuous(limits = limits_x) + 
              scale_y_continuous(limits = limits_y) + 
              smooth)
      
      
      group <- c(ifelse(is.null(fill), NA, fill), group_x, group_y)
      group <- group[!is.na(group)]
      if(length(group)==0) group <- NULL
      
      limits_y_table <- if(is.null(limits_y)) c(-Inf, Inf) else limits_y
      limits_x_table <- if(is.null(limits_x)) c(-Inf, Inf) else limits_x
      
      correlations <- self$data %>% 
        dplyr::filter(between(!!rlang::sym(y), limits_y_table[1], limits_y_table[2]) & between(!!rlang::sym(x), limits_x_table[1], limits_x_table[2])) %>%             
        dplyr::group_by(!!!rlang::syms(group)) %>% 
        tidyr::nest(.key = 'data') %>% 
        dplyr::mutate(cor = purrr::map_dbl(data, .f = function(d) round(stats::cor(d[,!!x], d[,!!y],  use = 'pairwise.complete.obs'),2 ))) %>% select(-data) %>% print
      
      
    }
    
    
    
    
    
    
    
    , F_density_plot = function(  x       = NA
                                  , data = NULL
                                  , fill    = NULL
                                  , size    = 0.5
                                  , adjust  = 0.5
                                  , alpha   = 0.5
                                  , colour  = NULL
                                  , group_x = NA
                                  , group_y = NA
                                  , angle_x = 0
                                  , title   = ''
                                  , special_values = NULL
                                  , scale_free = c('fixed', 'free_x', 'free_y', 'free')
                                  , limits  = NULL ){
      
      if(is.na(group_x) & is.na(group_y)){
        grid <- NULL
      }else{
        formula_grid <- paste(  paste(tidyr::replace_na(group_x, '.'), collapse='+' )
                                , paste(tidyr::replace_na(group_y, '.'), collapse='+')
                                , sep = '~')
        grid <- facet_grid(formula_grid, scale = scale_free)
      }
      
      
      print(ggplot(data=if(is.null(data)) self$data %>% dplyr::filter(! (!!rlang::sym(x) %in% special_values)) else data %>% filter(! (!!rlang::sym(x) %in% special_values)), aes_string(x=x, colour=colour, fill=fill)) + 
              geom_density(alpha = alpha, size = size, adjust = adjust) + 
              grid + 
              ggtitle(title) + 
              theme(axis.text.x = element_text(angle = angle_x)) + 
              scale_x_continuous(limits = limits))
      
      if(is.null(special_values)){
        print( data[[x]][data[[x]]%in% special_values] %>% table )
      }
      
      
    }
    
    
    
    
    
    
    
    , F_boxplot = function(    x       = NA
                               , y       = NA
                               , data    = NULL
                               , fill    = NULL
                               , size    = 0.5
                               , adjust  = 0.5
                               , alpha   = 0.5
                               , group_x = NA
                               , group_y = NA
                               , angle_x = 0
                               , title   = ''
                               , special_values = NULL
                               , scale_free = c('fixed', 'free_x', 'free_y', 'free')
                               , limits_y  = NULL ){
      
      if(is.na(group_x) & is.na(group_y)){
        grid <- NULL
      }else{
        formula_grid <- paste(  paste(tidyr::replace_na(group_x, '.'), collapse='+' )
                                , paste(tidyr::replace_na(group_y, '.'), collapse='+')
                                , sep = '~')
        grid <- facet_grid(formula_grid, scale = scale_free)
      }
      
      
      print(ggplot(data=if(is.null(data)) self$data %>% dplyr::filter(! (!!rlang::sym(y) %in% special_values)) else data, aes_string(x=x, y = y, fill=fill)) + 
              geom_boxplot() + 
              grid + 
              ggtitle(title) + 
              theme(axis.text.x = element_text(angle = angle_x)) + 
              scale_y_continuous(limits = limits_y))
      
      group <- c(ifelse(is.null(fill), NA, fill), group_x, group_y)
      group <- group[!is.na(group)]
      if(length(group)==0) group <- NULL
      
      limits_y_table <- if(is.null(limits_y)) c(-Inf, Inf) else limits_y
      
      self$data %>% 
        dplyr::filter(between(!!rlang::sym(y), limits_y_table[1], limits_y_table[2])) %>% 
        dplyr::filter(! (!!rlang::sym(y) %in% special_values)) %>% 
        dplyr::group_by(!!!rlang::syms(group)) %>% 
        dplyr::summarise( min    = quantile(!!rlang::sym(y), na.rm=TRUE, probs = 0.00)
                          ,kw_05  = quantile(!!rlang::sym(y), na.rm=TRUE, probs = 0.05)
                          ,kw_10  = quantile(!!rlang::sym(y), na.rm=TRUE, probs = 0.10)
                          ,kw_25  = quantile(!!rlang::sym(y), na.rm=TRUE, probs = 0.25)
                          ,median = median(!!rlang::sym(y), na.rm=TRUE)
                          ,kw_75  = quantile(!!rlang::sym(y), na.rm=TRUE, probs = 0.75)
                          ,kw_90  = quantile(!!rlang::sym(y), na.rm=TRUE, probs = 0.90)
                          ,kw_95  = quantile(!!rlang::sym(y), na.rm=TRUE, probs = 0.95)
                          ,max    = quantile(!!rlang::sym(y), na.rm=TRUE, probs = 1.00)
        ) %>% print
      
    }
    
    
    
    , F_barplot_count = function(  data= NULL
                                   , x = NA
                                   , fill = NULL
                                   , group_x = NA
                                   , group_y = NA
                                   , position = c('stack') # 'dodge', 'fill'
                                   , breaks_numeric = NULL ){
      
      if(!is.null(breaks_numeric)){
        scale <- scale_x_continuous(breaks = breaks_numeric)
      }else{
        scale <- NULL
      }
      
      facet <- if(is.na(group_x) & is.na(group_y)) NULL else facet_grid(private$grid_formula(group_x, group_y))
      
      ggplot( data=if(is.null(data)) self$data else data ) + 
        geom_bar(aes_string(x=x, fill = fill), position = position) + scale + facet
      
    }
    
    
    , F_parallel = function(data = NULL, vars = NULL){
      
      if(is.null(vars)){
        if(is.null(data)){
          iplots::ipcp(self$data)
        }else{
          iplots::ipcp(data)
        }
      }else{
        if(is.null(data)){
          iplots::ipcp(self$data %>% dplyr::select(one_of(vars)))
        }else{
          iplots::ipcp(data %>% dplyr::select(one_of(vars)))
        }
      }
      
      
      
      
    }
    
    , F_woe = function(  data           = NULL
                         , var_binary     = NA
                         , var            = NA
                         , special_values = NULL
                         , max_num_bin    = 8
                         , min_perc_fine_bin = 0.05 
                         , breaks         = NULL
                         , replace_var    = FALSE # if replace variable values with WoE
                         , print_woe      = TRUE
                         , plot_woe       = TRUE
    ){
      
      
      woe <- scorecard::woebin(dt = if(is.null(data)) self$data  else data
                               , y = var_binary 
                               , x = var
                               , max_num_bin = max_num_bin
                               , min_perc_fine_bin = min_perc_fine_bin
                               , special_values = special_values
      )
      
      if(replace_var){
        if(is.null(data)){
          self$data <- scorecard::woebin_ply(self$data, woe)
        }else{
          data <<- scorecard::woebin_ply(data, woe)
        }
        
      }
      
      if(print_woe) print(woe)
      if(plot_woe) scorecard::woebin_plot(woe)
      
      
      # if(!is.null(breaks)){
      #   data <- self$data %>% 
      #     dplyr::select(!!!rlang::syms(c(binary, var))) %>% 
      #     dplyr::mutate(!!binary := as.numeric(as.character(!!sym(binary))))
      #   
      #   data[[var]] <- cut(data[[var]], breaks = breaks, dig.lab = 7)
      #   
      # }else{
      #   data <- self$data %>% 
      #     dplyr::select(!!!rlang::syms(c(binary, var))) %>% 
      #     dplyr::mutate(!!binary := as.numeric(as.character(!!sym(binary))))
      # }
      # 
      # IV <- Information::create_infotables(data = data, y = binary, bins = bins)
      # print(IV)
      # 
      # print(Information::plot_infotables(IV, var))
      
    }
    
    
    , F_mutual_information = function(  data = NULL
                                        , formula = NA){
      
      
      FSelector::information.gain(  as.formula(formula)
                                    , data = if(is.null(data)) self$data else data) %>% 
        data.frame %>%
        tibble::rownames_to_column('feature') %>%
        arrange(desc(attr_importance))
      
      
    }
    
    
    , F_correlation_numeric = function(vars = NULL){
      
      if(!is.null(vars)){ # correlation for all numeric variables
        stats::cor(self$data %>% dplyr::select(!!!rlang::syms(vars)) %>%  select_if(is.numeric), use = 'pairwise.complete.obs')
      }else{
        stats::cor(self$data %>% select_if(is.numeric), use = 'pairwise.complete.obs')
      }
    }
    
    , F_correlation_categorical = function(vars = NA){
      
      if(is.na(vars)) stop('no variables passed')
      
      l.a::l.a.v_cramer(self$set_train, vars = vars)
      
    }
    
    , F_randomForestImportance = function(y = NA, x = NA){
      
      formula <- reformulate(termlabels = x, response = y, intercept = TRUE)
      
      print(nrow(self$data))
      f_data <- na.omit(self$data) %>% dplyr::mutate_if(is.character, as.factor)
      print(nrow(f_data))
      
      forest <- randomForest::randomForest(formula = formula, data = self$data)
      
      randomForest::importance(forest, type = 1)
      
    }
    
    
    
  )
  ,private = list(
    
    
    group = function(fill, group_x, group_y){
      group <- c(ifelse(is.null(fill), NA, fill), group_x, group_y)
      group <- group[!is.na(group)]
      if(length(group)==0) group <- NULL
      group
    }
    
    ,grid_formula = function(group_x, group_y){
      formula_grid <- paste(  paste(tidyr::replace_na(group_x, '.'), collapse='+' )
                              , paste(tidyr::replace_na(group_y, '.'), collapse='+')
                              , sep = '~')
    }
    
  )
  ,active = list()
)

# dt <- data_obj$new()
# d1 <- diamonds %>% select(price) %>% bind_rows( data.frame(price=rep(1,100000)) )
# dt$F_data_load(d1)