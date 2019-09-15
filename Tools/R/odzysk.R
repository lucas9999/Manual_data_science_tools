# Notes
# 
# Opis dzialania algorytmu:
#   
#  ZAKLADAM ZE WCZESNIEJ SPLATY ZOSTALY UCIAGLONE DO ODPOWIEDNICH ZAKRESOW DAT
# 
# 
# 1. 
# Pracuje na 2 zbiorach: (1) ze splatami i (2) z atrybutami per kredt
# - a atrubutach per kredyt mam zdarzenia dla kredytu i oraz inne atrybuty w tym atrybuty klienta (musze zdefiniowac klienta dominujacego). Tutaj tez bedzie stopa do npv jezeli stopa bedzie stala dla kredytu.
# 
# 2. 
# a) laduje oba zbiory danych. W zbiorach przyjmuje na sztywno nazyw zmiennych
# b) mam mozliwosc dodania dat na sztywno oraz dodania dat na ilosc miesiecy od zdarzenia (in plus i in minus). (Dodac zaokroglanie dat do zadanej jednostki)
# b) robie kopie zbioru z atrybutami odpowiednio z odpowiednimi modyfikacjami . Robie evant_start i event_end. Robie to poprzez podanie zdarzenia poczatkowego i koncowego. Jezeli dla danej ekspozycji nie ma zdarzenia (NA), moge podac zapasowe zdarzenie. Ekspozyje ktore w finalnym event_start lub event_end maja braki nie sa brane pod uwage do dalszej analizy.
# 
# 3. 
# Obliczam odzyski
# a) filtruje dane zakresm z tabeli wykonanej w punkcie 2
# b) zbudowanie zbioru z licznikiem naliczajacym jednostki czasu od zdarzenia poczatkowego
# c) okreslam czy chce uzyc NPV - jezeli tak to przeliczam splaty, CB i WB
# d) przeliczam rozne warianty odzyskow (dodac ktore warianty chce przezliczyc)
# 
# 4. 
# Robie wykry dla danych policzonych z punkcie 4.
# 
# 5. 
# (dodac) - tworzenie zmienna binarna dla zadanego punktu odciencia. 





# obiekt R6 ----------------------------------------------------------------



recovery <- R6::R6Class(
  
  classname = 'recovery'
  
  
  ,public  = list(
    
    data_recovery = NULL
    , data_events   = NULL
    , data_events_prepared = NULL
    , data_recovery_CB_by_period_weight    = NULL
    , data_recovery_CB_by_period_no_weight = NULL
    , data_recovery_CB_exposure_weight     = NULL
    , data_recovery_WB_by_period_weight    = NULL
    , data_recovery_WB_by_period_no_weight = NULL
    , data_recovery_WB_exposure_weight     = NULL
    , event_1       = NULL
    , evens_2       = NULL
    , group         = NULL
    , period_length = NULL
    , event_start   = NULL
    , event_end     = NULL
    , unit = 'month'
    
    
    , F_load_data = function(  data_recovery
                               , data_events 
                               
                               , exp_ID_recovery = 'NumerRachunku'
                               , exp_ID_events   = 'NumerRachunku'
                               , CB = 'CB'
                               , WB = 'WB'
                               , pay_off = 'Splata'
                               , pay_off_date = 'Data_i'
                               
                               , unit = c('month', 'day', 'year')
                               
    ){
      
      self$data_recovery <- data_recovery
      self$data_events   <- data_events
      
      self$unit <- unit
      
      #> porzadkowanie nazw kolumn
      self$data_recovery %<>% dplyr::rename(  
        exp_ID       = !!rlang::sym(exp_ID_recovery)
        , CB           = !!rlang::sym(CB)
        , WB           = !!rlang::sym(WB)
        , pay_off      = !!rlang::sym(pay_off)
        , pay_off_date = !!rlang::sym(pay_off_date))
      
      self$data_events %<>% dplyr::rename(exp_ID = !!rlang::sym(exp_ID_events))
      #<
      
      
      
    }
    
    
    # dodawanie dat na sztywno (takie same dla kazdej ekspozycji)
    , F_data_events_add_date = function(event_name = NULL, event_date = NULL, override = FALSE){
      
      if(is.null(event_name) | is.null(event_date) ) stop('event name of event date not provided')
      if(!override & (event_name %in% names(self$data_events))) stop('you will override existing variable')
      if(!is.Date(event_date)) stop('event_date is not a Date class')
      
      self$data_events %<>% dplyr::mutate(!!event_name := !!event_date)
      
      
    }
    
    
    # dodawanie nowych zdarzen ktore sa definiowane jako zdarzenie po uplywie n miesiecy od zdarzenia poczatkowego (mozna tez naliczac zdarzenia w tyl)
    , F_dates_event_add_data_period = function(  event        = NULL
                                                 , new_event    = event
                                                 , period_plus  = TRUE
                                                 , round        = c('no', 'up', 'down')
                                                 , period       = 12 # you ca use negative values too
    ){
      
      unit <- self$unit
      new_event_name = paste0(new_event, '_', abs(period), '_', unit, ifelse(period>=0, '_P', '_M'))
      period_fun <- switch( unit
                            ,month = base::months
                            ,day   = lubridate::days
                            ,year  = lubridate::years
      )
      
      self$data_events %<>% dplyr::mutate(!!new_event_name :=  !!sym(event) + period_fun(period))
    }
    
    
    
    
    # calculation of periods between events i data_events table
    , F_dates_event_preparation = function(  
      event_1 = NA
      , event_1_spare = NA
      , event_2 = NA
      , event_2_spare = NA
      
    ){
      
      
      
      data_events <- self$data_events
      
      event_start <- paste0(event_1, '_start')
      event_end   <- paste0(event_2, '_end')
      
      self$event_start <- event_start
      self$event_end   <- event_end
      
      if(!is.na(event_1_spare)){
        data_events %<>% dplyr::mutate( !!event_start := case_when(  is.na(!!rlang::sym(event_1))
                                                                     , !!rlang::sym(event_1_spare))
                                        , TRUE ~ !!rlang::sym(event_1))
      }else{
        data_events %<>% dplyr::mutate( !!event_start := !!rlang::sym(event_1))
      }
      
      if(!is.na(event_2_spare)){
        data_events %<>% dplyr::mutate( !!event_end := case_when(  is.na(!!rlang::sym(event_2))
                                                                   , !!rlang::sym(event_2_spare))
                                        , TRUE ~ !!rlang::sym(event_2))
      }else{
        data_events %<>% dplyr::mutate( !!event_end := !!rlang::sym(event_2))
      }
      
      
      
      
      # zamiana NA jezeli chce aby elementy ktore nie maja zdarzenia tez byly brake do obliczen
      # if(left_NA_remove){
      #   data_events <- data_events %>% mutate(!!event_1 := case_when(is.na(!!rlang::sym(event_1))~ymd(19000101), TRUE~!!rlang::sym(event_1) ))
      # }
      # if(right_NA_remove){
      #   data_events <- data_events %>% mutate(!!event_2 := case_when(is.na(!!rlang::sym(event_2))~ymd(20990101), TRUE~!!rlang::sym(event_2) ))
      # }
      
      
      period_length <- paste(event_start, event_end, sep='_')
      self$period_length <- period_length
      
      
      # unit <- 'month'
      period_fun <- switch( unit
                            ,month = base::months
                            ,day   = lubridate::days
                            ,year  = lubridate::years)
      
      data_events %<>% mutate(!!period_length:=lubridate::interval(  start = !!rlang::sym(event_start)
                                                                     , end   = !!rlang::sym(event_end)) %/% period_fun(1))
      
      self$data_events_prepared <- data_events
      
    }
    
    
    
    
    
    , F_recovery_calculation = function(  
      group = NA
      , CB_by_period_weight    = TRUE
      , CB_by_period_no_weight = FALSE
      , CB_by_exposure_weight  = TRUE
      , WB_by_period_weight    = TRUE
      , WB_by_period_no_weight = FALSE
      , WB_by_exposure_weight  = TRUE
      # , NPV = FALSE
      # , NPV_name = 'rate'
    ){
      
      
      
      self$group <- group 
      
      data_recovery <- self$data_recovery
      
      # filtering date
      data_recovery <- dplyr::left_join(  data_recovery
                                          , self$data_events_prepared
                                          , by = 'exp_ID') %>% 
        dplyr::filter(pay_off_date >= !!rlang::sym(self$event_start) & pay_off_date <= !!rlang::sym(self$event_end) )
      
      
      # 
      period_fun <- switch( self$unit
                            ,month = base::months
                            ,day   = lubridate::days
                            ,year  = lubridate::years
      )
      
      
      # dodanie numeracji 
      
      shift <- data_recovery %>% 
        dplyr::arrange(exp_ID, pay_off_date) %>% 
        dplyr::group_by(exp_ID) %>% 
        dplyr::summarise(shift_period = max(0, interval(first(!!rlang::sym(self$event_start)), min(pay_off_date, na.rm=TRUE)) %/% period_fun(1)))
      
      data_recovery %<>% 
        dplyr::arrange(exp_ID, pay_off_date) %>% 
        dplyr::group_by(exp_ID) %>% 
        dplyr::mutate(period_nr = dplyr::row_number())
      
      data_recovery %<>% 
        dplyr::left_join(shift, by = 'exp_ID') %>% 
        dplyr::mutate(period_nr = period_nr + shift_period) %>% 
        dplyr::select(-shift_period)
      
      
      # ostatni CB i WB zamiast 0
      
      data_recovery %<>% dplyr::mutate(  CB=dplyr::case_when(CB==0~NA_real_, TRUE~CB)
                                         , WB=dplyr::case_when(WB==0~NA_real_, TRUE~WB))
      
      
      data_recovery %<>% tidyr::fill(CB, WB, .direction = 'down') %>% tidyr::fill(CB, WB, .direction = 'up')
      
      data_recovery %<>% mutate(  recovery_WB = case_when(WB == 0 ~ NA_real_, TRUE ~ pay_off/WB)
                                  , recovery_CB = case_when(CB == 0 ~ NA_real_, TRUE ~ pay_off/CB))
      
      data_rec_test <<- data_recovery # zadolarowac na przyszlosc - funkcja pomocnicza do podgladu wynikw
      
      group_period_length <- purrr::discard(c(group, 'period_nr'),is.na)
      group_exp_ID        <- purrr::discard(c(group, 'exp_ID'),is.na) 
      
      # CB
      if(CB_by_period_weight){
        self$data_recovery_CB_by_period_weight <- data_recovery %>% 
          dplyr::group_by(!!!rlang::syms(group_period_length)) %>% 
          dplyr::summarise(recovery_period_weight = sum(recovery_CB * CB)/sum(CB) ) %>% ungroup()
      }
      if(CB_by_period_no_weight){
        self$data_recovery_CB_by_period_no_weight <- data_recovery %>% 
          dplyr::group_by(!!!rlang::syms(group_period_length)) %>% 
          dplyr::summarise(recovery_period_no_weight = sum(recovery_CB, na.rm=TRUE)/length(recovery_CB)) %>% ungroup()
      }
      if(CB_by_exposure_weight){
        self$data_recovery_CB_exposure_weight <- data_recovery %>% 
          dplyr::group_by(!!!rlang::syms(group_exp_ID)) %>% 
          dplyr::summarise(recovery_exposure_weight=sum(recovery_CB * CB, na.rm=TRUE)/sum(CB) ) %>% ungroup()
      }
      
      # WB
      if(WB_by_period_weight){
        self$data_recovery_WB_by_period_weight <- data_recovery %>% 
          dplyr::group_by(!!!rlang::syms(group_period_length)) %>% 
          dplyr::summarise(recovery_period_weight = sum(recovery_WB * WB)/sum(WB) ) %>% ungroup()
      }
      if(WB_by_period_no_weight){
        self$data_recovery_WB_by_period_no_weight <- data_recovery %>% 
          dplyr::group_by(!!!rlang::syms(group_period_length)) %>% 
          dplyr::summarise(recovery_period_no_weight = sum(recovery_WB, na.rm=TRUE)/length(recovery_WB)) %>% ungroup()
      }
      if(WB_by_exposure_weight){
        self$data_recovery_WB_exposure_weight <- data_recovery %>% 
          dplyr::group_by(!!!rlang::syms(group_exp_ID)) %>% 
          dplyr::summarise(recovery_exposure_weight=sum(recovery_WB * WB, na.rm=TRUE)/sum(WB) ) %>% ungroup()
      }
      
    }
    
    , F_plot = function(   CB_by_period_weight    = TRUE
                           , CB_by_period_no_weight = FALSE
                           , CB_by_exposure_weight  = TRUE
                           , WB_by_period_weight    = TRUE
                           , WB_by_period_no_weight = FALSE
                           , WB_by_exposure_weight  = TRUE
                           , color   = NULL
                           , group_x = NA
                           , group_y = NA
                           , scale_free = c('fixed', 'free_x', 'free_y', 'free')){
      
      
      if(is.na(group_x) & is.na(group_y)){
        grid <- NULL
      }else{
        formula_grid <- paste(  paste(tidyr::replace_na(group_x, '.'), collapse='+' )
                                , paste(tidyr::replace_na(group_y, '.'), collapse='+')
                                , sep = '~')
        grid <- facet_grid(formula_grid, scale = scale_free)
      }
      
      # CB
      if(CB_by_period_weight){
        print(ggplot(  data=self$data_recovery_CB_by_period_weight
                       , aes_string(x='period_nr', y='recovery_period_weight', color = color)) + 
                geom_line() + 
                geom_point() + 
                grid
        )
      }
      if(CB_by_period_no_weight){
        print(ggplot(  data=self$data_recovery_CB_by_period_no_weight
                       , aes_string(x='period_nr', y='recovery_period_no_weight', color = color)) +
                geom_line() +
                geom_point() +
                grid
        )
      }
      if(CB_by_exposure_weight){
        print(ggplot(  data=self$data_recovery_CB_exposure_weight
                       , aes_string(x='exp_ID', y='recovery_exposure_weight', color = color)) +
                geom_line() +
                geom_point() +
                grid
        )
        
        # WB
      }
      if(WB_by_period_weight){
        print(ggplot(  data=self$data_recovery_WB_by_period_weight
                       , aes_string(x='period_nr', y='recovery_period_weight', color = color)) +
                geom_line() +
                geom_point() +
                grid
        )
      }
      if(WB_by_period_no_weight){
        print(ggplot(  data=self$data_recovery_WB_by_period_no_weight
                       , aes_string(x='period_nr', y='recovery_period_no_weight', color = color)) +
                geom_line() + 
                geom_point() +
                grid
        )
      }
      if(WB_by_exposure_weight){
        print(ggplot(  data=self$data_recovery_WB_exposure_weight
                       , aes_string(x='exp_ID', y='recovery_exposure_weight', color = color)) +
                geom_line() + 
                geom_point() +
                grid
        )
      }
    }
  )
  ,private = list()
  ,active  = list()
  
)




# uruchomienie metody-----

pay_off <- tribble(						
  ~NumerRach 	,~plec 	,~produkt 	,~Splata 	,~CB 	,~WB 	,~Data_i 
  ,1 	,0 	,'k' 	,55.5 	,245.4 	,54.4 	,as.Date('2010-01-01')
  ,1 	,0 	,'k' 	,49.9 	,230 	,225.3 	,as.Date('2010-01-02')
  ,1 	,0 	,'r' 	,41.9 	,202.2 	,183.7 	,as.Date('2010-01-03')
  ,1 	,1 	,'r' 	,0 	,195.2 	,73.1 	,as.Date('2010-01-04')
  ,1 	,1 	,'r' 	,27.7 	,90.6 	,232.1 	,as.Date('2010-01-05')
  ,1 	,1 	,'k' 	,79.2 	,6.9 	,146.3 	,as.Date('2010-01-06')
  ,1 	,1 	,'k' 	,0 	,123.4 	,193.9 	,as.Date('2010-01-07')
  ,1 	,0 	,'r' 	,49.9 	,230 	,225.3 	,as.Date('2010-01-08')
  ,1 	,0 	,'r' 	,41.9 	,202.2 	,183.7 	,as.Date('2010-01-09')
  ,2 	,0 	,'r' 	,72 	,197.6 	,94.1 	,as.Date('2010-01-03')
  ,2 	,0 	,'r' 	,0 	,299.7 	,203.3 	,as.Date('2010-01-04')
  ,2 	,0 	,'k' 	,83.8 	,265.3 	,115.4 	,as.Date('2010-01-05')
  ,2 	,1 	,'k' 	,24.5 	,112.3 	,264.1 	,as.Date('2010-01-06')
  ,2 	,1 	,'k' 	,0 	,179.1 	,222.6 	,as.Date('2010-01-07')
  ,2 	,1 	,'k' 	,81.9 	,146.2 	,139.4 	,as.Date('2010-01-08')
  ,2 	,0 	,'r' 	,43.6 	,199 	,167.3 	,as.Date('2010-01-09')
  ,2 	,0 	,'r' 	,56.3 	,8.6 	,97.7 	,as.Date('2010-01-10')
  ,2 	,0 	,'r' 	,95 	,57.5 	,297.8 	,as.Date('2010-01-11')
  ,2 	,1 	,'r' 	,0 	,4 	,119.1 	,as.Date('2010-01-12')
)						


events <- tribble(						
  ~NrRach 	,~wypowiedzenie 	,~kancelaria 	,~komornik 	,~sekurytyzacja 	,~zamkniecie 	,~rate
  ,1 	,as.Date('2010-01-02') 	,NA	,as.Date('2010-01-06') 	,as.Date('2010-01-08') 	,as.Date('2010-01-09') 	,0.01
  ,2 	,as.Date('2010-01-01') 	,as.Date('2010-01-06') 	,NA	,as.Date('2010-01-10') 	,as.Date('2010-01-13') 	,0.02
)						


events %<>% mutate_at(vars(wypowiedzenie:zamkniecie), as.Date)


rec1 <- recovery$new()

rec1$F_load_data(
  # dane
  data_recovery = pay_off
  , data_events   = events
  
  # nazwy zmiennych 
  , exp_ID_recovery = 'NumerRach'
  , exp_ID_events   = 'NrRach'
  , CB = 'CBb'
  , WB = 'WBb'
  , pay_off      = 'Splata'
  , pay_off_date = 'Data_i'
  
  , unit          = 'day'
)


rec1$F_data_events_add_date(
  event_name = 'nowe'
  , event_date = ymd(20140111)
  , override   = FALSE
)
rec1$data_events

rec1$F_dates_event_add_data_period(
  event       = 'kancelaria'
  , new_event   = 'kancelaria2'
  , round       = TRUE
  , period      = -2
  , unit        = 'year'
)
rec1$data_events %>% View

rec1$F_dates_event_preparation(
  event_1       = 'wypowiedzenie'
  , event_1_spare = NA
  , event_2       = 'zamkniecie'
  , event_2_spare = NA
  
)
# rec1$data_events_prepared %>% View


rec1$data_events_prepared %>% names

rec1$F_recovery_calculation(
  group = NA
  , CB_by_period_weight    = TRUE
  , CB_by_period_no_weight = TRUE
  , CB_by_exposure_weight  = TRUE
  , WB_by_period_weight    = TRUE
  , WB_by_period_no_weight = TRUE
  , WB_by_exposure_weight  = TRUE
  # , NPV = TRUE
  # , NPV_rate_name = 'NPV_rate'
)


rec1$data_recovery_CB_by_period_weight
rec1$data_recovery_CB_exposure_weight

rec1$data_recovery_WB_by_period_weight
rec1$data_recovery_WB_exposure_weight

rec1$data_recovery_CB_by_period_no_weight
rec1$data_recovery_WB_by_period_no_weight



# NPV(cf0,cf,times,i,plot=FALSE)




rec1$F_plot(  CB_by_period_weight    = F
              , CB_by_period_no_weight = F
              , CB_by_exposure_weight  = F
              , WB_by_period_weight    = F
              , WB_by_period_no_weight = F
              , WB_by_exposure_weight  = T
              , color   = NULL
              , group_x = NA
              , group_y = NA
)



# dodac npv
# dodac ciecie po datach na sztywno dla wszystkich elementow (ale potem jako opcja)

# http://www.math-only-math.com/In-simple-interest-when-the-time-is-given-in-months-and-days.html
# 
# C = 100
# R = 0.08
# T = 0.5
# 
# C*((1-(1+R)^-T)/R)
# 
# C x {(1 - (1 + R)-T) / R}

# FinancialMath::NPV(cf0=0,cf=100,times=1/12,i=.01)






# test liczenia odzyskow z recznym przeliczeniem w excelu -----

# ..wariant 1 ----

events1 <- tribble(						
  ~NrRach 	,~wypowiedzenie 	,~kancelaria 	,~komornik 	,~sekurytyzacja 	,~zamkniecie 	,~rate
  ,1 	,as.Date('2010-01-01') 	,NA	,as.Date('2010-01-06') 	,as.Date('2010-01-08') 	,as.Date('2010-01-09') 	,0.01
  ,2 	,as.Date('2010-01-01') 	,as.Date('2010-01-06') 	,NA	,as.Date('2010-01-10') 	,as.Date('2010-01-13') 	,0.02
)						

rec1 <- recovery$new()

rec1$F_load_data(
  # dane
  data_recovery = pay_off
  , data_events   = events1
  
  # nazwy zmiennych 
  , exp_ID_recovery = 'NumerRach'
  , exp_ID_events   = 'NrRach'
  , CB = 'CBb'
  , WB = 'WBb'
  , pay_off      = 'Splata'
  , pay_off_date = 'Data_i'
  
  , unit          = 'day'
)

rec1$data_events %>% View

rec1$F_dates_event_preparation(
  event_1         = 'wypowiedzenie'
  , event_1_spare = NA
  , event_2       = 'zamkniecie'
  , event_2_spare = NA
)
rec1$data_events_prepared %>% View


rec1$data_events_prepared %>% names

rec1$F_recovery_calculation(
  group = NA
  , CB_by_period_weight    = TRUE
  , CB_by_period_no_weight = TRUE
  , CB_by_exposure_weight  = TRUE
  , WB_by_period_weight    = TRUE
  , WB_by_period_no_weight = TRUE
  , WB_by_exposure_weight  = TRUE
  # , NPV = TRUE
  # , NPV_rate_name = 'NPV_rate'
)


rec1$data_recovery_CB_by_period_weight
rec1$data_recovery_CB_exposure_weight
rec1$data_recovery_CB_by_period_no_weight

rec1$data_recovery_WB_by_period_weight
rec1$data_recovery_WB_exposure_weight
rec1$data_recovery_WB_by_period_no_weight










#..wariant 2 ----

events2 <- tribble(						
  ~NrRach 	,~wypowiedzenie 	,~kancelaria 	,~komornik 	,~sekurytyzacja 	,~zamkniecie 	,~rate
  ,1 	,as.Date('2010-01-01') 	,NA	,as.Date('2010-01-06') 	,as.Date('2010-01-08') 	,NA	,0.01
  ,2 	,as.Date('2010-01-01') 	,as.Date('2010-01-06') 	,NA	,as.Date('2010-01-10') 	,as.Date('2010-01-13') 	,0.02
)				
events2 %<>% mutate(kancelaria = as.Date(kancelaria), zamkniecie = as.Date(zamkniecie))
# View(events2)


rec2 <- recovery$new()

rec2$F_load_data(
  # dane
  data_recovery = pay_off
  , data_events   = events2
  
  # nazwy zmiennych 
  , exp_ID_recovery = 'NumerRach'
  , exp_ID_events   = 'NrRach'
  , CB = 'CBb'
  , WB = 'WBb'
  , pay_off      = 'Splata'
  , pay_off_date = 'Data_i'
  , unit = 'day'
)

# rec2$data_events %>% View

rec2$F_dates_event_preparation(
  event_1         = 'wypowiedzenie'
  , event_1_spare = NA
  , event_2       = 'zamkniecie'
  , event_2_spare = NA
)
# rec2$data_events_prepared %>% View


# rec2$data_events_prepared %>% names

rec2$F_recovery_calculation(
  group = NA
  , CB_by_period_weight    = TRUE
  , CB_by_period_no_weight = TRUE
  , CB_by_exposure_weight  = TRUE
  # , WB_by_period_weight    = TRUE
  # , WB_by_period_no_weight = TRUE
  # , WB_by_exposure_weight  = TRUE
  # , NPV = TRUE
  # , NPV_rate_name = 'NPV_rate'
)

data_rec_test %>% View


rec2$data_recovery_CB_by_period_weight
rec2$data_recovery_CB_exposure_weight
rec2$data_recovery_CB_by_period_no_weight
# rec2$data_recovery_WB_by_period_weight
# rec2$data_recovery_WB_exposure_weight
# rec2$data_recovery_WB_by_period_no_weight