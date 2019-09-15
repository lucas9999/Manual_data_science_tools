require(FSelector)
l.a::l.a.v_cramer()
require(randomForest)
randomForest::
  
  
  require(ranger)
ranger::ranger(formula = )

cat, ord, numeric()


require(R6)


FSelector::information.gain(formula = 'table~clarity', data = diamonds)[1,1]


l.s::l.s.packages_basic()


data_t = tibble::tribble(~variable, ~type
                         ,"carat", 'ord'
                         ,"cut" , 'cat'     
                         ,"color" , 'cat'   
                         ,"clarity" , 'cat' 
                         ,"depth" , 'num'   
                         ,"table" , 'num'   
                         ,"price" , 'ord'   
                         ,"x" , 'num'      
                         ,"y" , 'num'       
                         ,"z" , 'num' 
)

# cor(diamonds[,'carat'], diamonds[,'table'])[1]

df = data.frame()

dplyr::bind_rows(df, data.frame(a=1, b=2) )


names(diamonds)

stats::cor(diamonds[,'carat'], diamonds[,'table'])

wynik = correlation_machine_calculations(diamonds, data_t, 5)
wynik


# l.a::l.a.v_cramer(diamonds, var = c('cut','color'))[1,2]


# data = diamonds
# data_types = data_t
# rm(data)
# rm(data_types)


correlation_machine_calculations = function(data, data_types, n_sampling = 1, proc_sample = 0.5){
  
  
  correlation_table = data.frame()
  variables_names = purrr::as_vector(data_types[,1])
  
  for(n in 1:n_sampling){
    
    # sampling
    if(proc_sample < 1 ){
      data_s = dplyr::sample_frac(data, proc_sample)
    }else{
      data_s = data
    }
    
    # i = 1 ;j = 2
    for(i in 1:nrow(data_types)){
      
      for(j in 1:nrow(data_types)){
        
        
        
        if(i > j){
          
          
          data_s_clean = tidyr::drop_na( data_s[,c(variables_names[i], variables_names[j])] ) %>% as.data.frame()
          
          formula_a_b = as.formula(paste0(variables_names[i],'~',variables_names[j]))
          formula_b_a = as.formula(paste0(variables_names[j],'~',variables_names[i]))
          
          
          # num num 
          if(data_types[i,2]=='num' & data_types[j,2]=='num'  ){
            
            cal_type = 'num_num'
            
            cor_pearson = stats::cor(data_s_clean[,1], data_s_clean[,2] )
            correlation_table = dplyr::bind_rows(  correlation_table
                                                   , data.frame(  n
                                                                  , data_types_ = cal_type
                                                                  , measure = 'cor_pearson'
                                                                  , var_1 = variables_names[i]
                                                                  , var_2 = variables_names[j]
                                                                  , value = cor_pearson[1] ))
            
            
            information_gain_a_b = FSelector::information.gain(data = data_s_clean, formula = formula_a_b )
            correlation_table = dplyr::bind_rows(correlation_table, data.frame( n
                                                                                , data_types_ = cal_type, measure = 'information_gain'
                                                                                , var_1 = variables_names[i]
                                                                                , var_2 = variables_names[j]
                                                                                , value = information_gain_a_b[1,1] ))
            
            # information_gain_b_a = FSelector::information.gain(data = data_s_clean, formula = formula_b_a )
            # correlation_table = dplyr::bind_rows(correlation_table, data.frame(  n 
            #                                                                    , data_types_ = cal_type, measure = 'information_gain'
            #                                                                    , var_1 = variables_names[j]
            #                                                                    , var_2 = variables_names[i]
            #                                                                    , value = information_gain_b_a[1,1] ))
            
            
            
          }
          
          
          # cat cat
          if(data_types[i,2]=='cat' & data_types[j,2]=='cat'){
            
            cal_type = 'cat_cat'
            
            v_cramer = l.a::l.a.v_cramer(data_s_clean, var = c(variables_names[i], variables_names[j]))
            correlation_table = dplyr::bind_rows(correlation_table, data.frame(  n
                                                                                 , data_types_ = cal_type
                                                                                 , measure = 'v_cramer', var_1 = variables_names[i], var_2 = variables_names[j], value = v_cramer[1,2] ))
            
            
            
            information_gain_a_b = FSelector::information.gain(data = data_s_clean, formula = formula_a_b )
            correlation_table = dplyr::bind_rows(correlation_table, data.frame(  n
                                                                                 , data_types_ = cal_type, measure = 'information_gain'
                                                                                 , var_1 = variables_names[i]
                                                                                 , var_2 = variables_names[j]
                                                                                 , value = information_gain_a_b[1,1] ))
            
            
            
            # information_gain_b_a = FSelector::information.gain(data = data_s_clean, formula = formula_b_a )
            # correlation_table = dplyr::bind_rows(correlation_table, data.frame(  n
            #                                                                    , data_types_ = cal_type, measure = 'information_gain'
            #                                                                    , var_1 = variables_names[j]
            #                                                                    , var_2 = variables_names[i]
            #                                                                    , value = information_gain_b_a[1,1] ))
            
            
            
          }
          
          
          # ord ord
          if(data_types[i,2]=='ord' & data_types[j,2]=='ord'  ){
            
            cal_type = 'ord_ord'
            
            cor_spearman = stats::cor(data_s_clean[,1], data_s_clean[,2] , method = 'spearman')
            correlation_table = dplyr::bind_rows(  correlation_table
                                                   , data.frame(    n
                                                                    , data_types_ = cal_type
                                                                    , measure = 'cor_spearman'
                                                                    , var_1 = variables_names[i]
                                                                    , var_2 = variables_names[j]
                                                                    , value = cor_spearman[1] ))
            
          }
          
          
          # num cat
          if((data_types[i,2]=='num' & data_types[j,2]=='cat') | (data_types[i,2]=='cat' & data_types[j,2]=='num') ){
            
            cal_type = 'num_cat'
            numeric = dplyr::bind_rows(data_types[i,], data_types[j,]) %>% dplyr::filter(type == 'num') %>% dplyr::select(variable)
            categorical = dplyr::bind_rows(data_types[i,], data_types[j,]) %>% dplyr::filter(type == 'cat') %>% dplyr::select(variable)
            
            formula_temp = as.formula(paste0(numeric, '~', categorical))
            
            
            information_gain_a_b = FSelector::information.gain(data = data_s_clean, formula = formula_a_b )
            correlation_table = dplyr::bind_rows(correlation_table, data.frame(  n
                                                                                 , data_types_ = cal_type, measure = 'information_gain'
                                                                                 , var_1 = variables_names[i]
                                                                                 , var_2 = variables_names[j]
                                                                                 , value = information_gain_a_b[1,1] ))
            
            # information_gain_b_a = FSelector::information.gain(data = data_s_clean, formula = formula_b_a )
            # correlation_table = dplyr::bind_rows(correlation_table, data.frame(  n
            #                                                                    , data_types_ = cal_type, measure = 'information_gain'
            #                                                                    , var_1 = variables_names[j]
            #                                                                    , var_2 = variables_names[i]
            #                                                                    , value = information_gain_b_a[1,1] ))
            
            
            
            
            kruskal_willis = stats::kruskal.test(formula = formula_temp, data = data_s_clean)
            correlation_table = dplyr::bind_rows(correlation_table, data.frame(  n
                                                                                 , data_types_ = cal_type, measure = ' kruskal_willis'
                                                                                 , var_1 = variables_names[j]
                                                                                 , var_2 = variables_names[i]
                                                                                 , value =  kruskal_willis$p.value))
            
            
          }
          
          
          
          
          # num - ord
          if((data_types[i,2]=='num' & data_types[j,2]=='ord') | (data_types[i,2]=='ord' & data_types[j,2]=='num')){
            
            cal_type = 'num_ord'
            
          }
          
          
          # cat - ord
          if((data_types[i,2]=='cat' & data_types[j,2]=='ord') | (data_types[i,2]=='ord' & data_types[j,2]=='cat') ){
            
            cal_type = 'cat_ord'
            
          }
          
          
        }
        
        
        
      }
      
    }
    
  } # end of sampling loop
  
  correlation_table
  
}




correlation_machine_summary(wynik)

data=wynik

correlation_machine_summary = function(data){
  
  # data = data %>% dplyr::mutate(vars=paste0(var_1, var_2))
  stats = data %>%
    dplyr::group_by(var_1, var_2, data_types_, measure) %>% 
    dplyr::summarise(  MEAN = mean(value, na.rm=TRUE)
                       , MEDIAN = stats::median(value, na.rm=TRUE) 
                       , SD = stats::sd(value, na.rm=TRUE)
    ) %>% 
    reshape2::melt(data = , id.vars = c('var_1','var_2', 'data_types_', 'measure'), variable.name = 'stat', value.name = 'value')
  
  
  data_types = stats %>% dplyr::select(data_types_) %>% unique %>% purrr::as_vector()
  
  tables_list = list()
  
  for(i in 1:length(data_types)){
    
    tables_list[[length(tables_list) + 1]] = stats %>% dplyr::filter(data_types_ == data_types[i]) %>% reshape2::dcast(., formula = data_types_ + var_1 + var_2 ~ measure + stat, value.var = 'value') %>% dplyr::ungroup()
    names(tables_list)[length(tables_list)] = data_types[i]
    
  }
  
  tables_list
  
}


correlation_machine_visualisation = function(data = data, measure = 'information_gain', variables = NA){
  
  
  data <- data %>% dplyr::mutate(vars=paste0(var_1, '_',var_2)) %>% dplyr::filter(measure == !!measure)
  
  if(!is.na(variables)){
    data = data[,variables]
  }
  
  print(ggplot(data = data) + geom_point(aes(x=n, y=value, colour = vars))) + geom_path(aes(x=n, y=value, colour = vars)) + ggtitle(measure)
  
}

correlation_machine_visualisation(wynik)





















data = airq

variables = c("Ozone",   "Solar.R", "Wind")

sufix = 'imp'

methods = list(integer = mlr::imputeMean())


rm(data)
rm(variables)
rm(sufix)
rm(methods)


data_imputation = function(data, variables = NA, target = character(0), methods = list(numeric = mlr::imputeMean()), override = FALSE, sufix = 'IMP', dummy_cols = character(0)){
  
  
  data_to_impute = data[,variables]
  
  
  data_imputed = mlr::impute(data_to_impute, classes = methods, dummy.cols = dummy_cols, force.dummies = TRUE  )$data
  
  
  
  if(isTRUE(override)){
    
    data_to_impute <- data_imputed
    
  }else{
    
    names(data_imputed)  <- paste(names(data_imputed), sufix,  sep = '_')
    data_to_impute = dplyr::bind_cols(data_to_impute, data_imputed)
    
  }
  
  data_to_impute
  
}

mlr::configureMlr()

wyn = data_imputation(airq, variables = c("Ozone",   "Solar.R", "Wind"), override = TRUE, dummy_cols = c("Ozone"), target = "Ozone", methods = list(numeric = mlr::imputeLearner("regr.gbm") )) 

wyn


df = data.frame()
dplyr::bind_cols(df, diamonds)



data(airquality)

airq = airquality
ind = sample(nrow(airq), 10)
airq$Wind[ind] = NA
airq$Wind = cut(airq$Wind, c(0,8,16,24))
summary(airq)



imp = impute(airq, classes = list(integer = imputeMean(), factor = imputeMode()))

imp %>% View