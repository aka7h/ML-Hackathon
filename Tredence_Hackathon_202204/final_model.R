###################### Load Libraries and Data ########################
library(tidyverse)
library(skimr)
library(timetk)
library(tidygeocoder)
library(catboost)
library(h2o)
library(prophet)
library(Metrics)
library(rsample)
library(plotly)
library(lubridate)


train <- read_csv('train.csv')
test <- read_csv('test.csv')
sample_sub <- read_csv('sample_submission.csv')
weekly_sub <- read_csv('submission_weekly.csv')

#' Extract location of warehouse using lat/long data
#' 

wh_loc <- train %>% select(warehouse_ID,Latitude,Longitude) %>% distinct() %>%
  drop_na()

wh_loc <- wh_loc %>% reverse_geocode(lat = Latitude, long = Longitude)

wh_loc <- wh_loc %>% mutate(states = str_replace(word(address,-4),',',''),
                            county = str_replace(word(address,-6),',','')) %>%
  select(-Latitude,-Longitude,-address)


######################## Exploratory Data Analysis #####################
#' EDA - Heat Maps
#'

tr <- train %>% left_join(.,wh_loc,by="warehouse_ID") %>%
  unite(.,"WH_PT",c("states","county","Product_Type"),sep = "_",remove = FALSE) %>%
  group_by(WH_PT) %>% mutate(GRP_ID = cur_group_id()) %>% ungroup()

tr <- tr %>% arrange(WH_PT,date) %>%
  mutate(WK_NUM = as.integer(format(date, '%V')),
         month = lubridate::month(date,label=TRUE))


###' Visualize the seasonality at pre and post covid
###'     daily level
###'     monthly level 
###'     

## Daily Level - Overall

daily_lvl <- tr %>% mutate(dday = day(date)) %>%
  group_by(WH_PT,GRP_ID,year,dday) %>%
  summarise(avg_qty = mean(daily_dispatch_count))

daily_lvl %>% filter(GRP_ID %in% c(1:6)) %>% ggplot(aes(year,dday))+
  geom_tile(aes(fill=avg_qty),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')

daily_lvl %>% filter(GRP_ID %in% c(7:12)) %>% ggplot(aes(year,dday))+
  geom_tile(aes(fill=avg_qty),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')

daily_lvl %>% filter(GRP_ID %in% c(13:20)) %>% ggplot(aes(year,dday))+
  geom_tile(aes(fill=avg_qty),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free',ncol = 2)


#### Monthly level

monthly_lvl <- tr %>% mutate(dmon = month(date,label=T)) %>%
  group_by(WH_PT,GRP_ID,year,dmon) %>%
  summarise(avg_qty = mean(daily_dispatch_count))

monthly_lvl %>% filter(GRP_ID %in% c(1:6)) %>% ggplot(aes(year,dmon))+
  geom_tile(aes(fill=avg_qty),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')

monthly_lvl %>% filter(GRP_ID %in% c(7:12)) %>% ggplot(aes(year,dmon))+
  geom_tile(aes(fill=avg_qty),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')


monthly_lvl %>% filter(GRP_ID %in% c(13:20)) %>% ggplot(aes(year,dmon))+
  geom_tile(aes(fill=avg_qty),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')



### Weekly level


weekly_ds <- tr %>% group_by(WH_PT,GRP_ID,year,WK_NUM) %>%
  summarise(total_sls = mean(daily_dispatch_count)) 

col1 = "#d8e1cf" 
col2 = "#438484"

weekly_ds %>% filter(GRP_ID %in% c(1:4))  %>% ggplot(aes(year,WK_NUM))+
  geom_tile(aes(fill=total_sls),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')

weekly_ds %>% filter(GRP_ID %in% c(5:8))  %>% ggplot(aes(year,WK_NUM))+
  geom_tile(aes(fill=total_sls),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')

weekly_ds %>% filter(GRP_ID %in% c(9:12))  %>% ggplot(aes(year,WK_NUM))+
  geom_tile(aes(fill=total_sls),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')

weekly_ds %>% filter(GRP_ID %in% c(13:16))  %>% ggplot(aes(year,WK_NUM))+
  geom_tile(aes(fill=total_sls),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')

weekly_ds %>% filter(GRP_ID %in% c(17:20))  %>% ggplot(aes(year,WK_NUM))+
  geom_tile(aes(fill=total_sls),color='white',na.rm=TRUE)+
  scale_fill_gradient(low ="yellow",high = "red")+
  facet_wrap(~WH_PT, scales='free')

tr <- tr %>% mutate(month=lubridate::month(date,label = TRUE))

tr %>% filter(GRP_ID %in% c(1:4)) %>% 
  ggplot(aes(month,daily_dispatch_count,fill=factor(year)))+
  geom_boxplot()+facet_wrap(~WH_PT)

tr %>% filter(GRP_ID %in% c(5:8)) %>% 
  ggplot(aes(month,daily_dispatch_count,fill=factor(year)))+
  geom_boxplot()+facet_wrap(~WH_PT)

tr %>% filter(GRP_ID %in% c(9:12)) %>% 
  ggplot(aes(month,daily_dispatch_count,fill=factor(year)))+
  geom_boxplot()+facet_wrap(~WH_PT)

tr %>% filter(GRP_ID %in% c(13:16)) %>% 
  ggplot(aes(month,daily_dispatch_count,fill=factor(year)))+
  geom_boxplot()+facet_wrap(~WH_PT)

tr %>% filter(GRP_ID %in% c(17:20)) %>% 
  ggplot(aes(month,daily_dispatch_count,fill=factor(year)))+
  geom_boxplot()+facet_wrap(~WH_PT)


tr %>% 
  plot_ly(x=~date, y=~daily_dispatch_count, group=~WH_PT,
          type="scatter",color=~WH_PT, mode="lines")


#### Data Preparation & Feature Engineering ####

##' We see missing value in is_weekend, is_warehouse_closed, Lat/Long
##' 
##' We can extrapolate is_weekend using the date variable
##' We can extrapolate the is_warehouse_closed from last year values
##' Lat/Long are going to be same for all warehouse
##' 
##' we might have missing values in the test set hence lets combine both

train$is_train <- 1
test$is_train <- 0
test$daily_dispatch_count <- NA
test$weekly_dispatch_count <- NA



full_data <- rbind(train,test)

#' Merging warehouse location data 
full_data <- full_data %>% left_join(.,wh_loc,by="warehouse_ID")

#' Combining both warehouse id and product type to unique key
full_data <- full_data %>% 
  unite(.,"WH_PT",c("states","county","Product_Type"),sep = "_",remove = FALSE) %>%
  group_by(WH_PT) %>% mutate(GRP_ID = cur_group_id())

full_data <- full_data %>% 
  mutate(is_weekend = if_else(wday(date,label = TRUE) %in% c("Sun","Sat"),1,0))


#' replacing missing values of warehouse closed based on the last year lag
full_data <- full_data %>% group_by(WH_PT) %>%
  mutate(is_warehouse_lag = lag(is_warehouse_closed,n=365,default="SKIP"))

full_data %>% group_by(is_warehouse_closed,is_warehouse_lag) %>% count() %>%
  filter(is_warehouse_lag!='SKIP')


full_data <- full_data %>% mutate(is_warehouse_closed = if_else(is.na(is_warehouse_closed),
                                                                is_warehouse_lag,
                                                                is_warehouse_closed)) %>%
  mutate(is_wh_cls = if_else(is_warehouse_closed=="Yes",1,0)) %>%
  select(-is_warehouse_closed,-is_warehouse_lag)


#' Filling the missing values in both Latitude and Longitude
#' By filling the above or below value for the latitude and longitude
full_data <- full_data %>% fill(Latitude,.direction = 'downup') %>%
  fill(Longitude,.direction = 'downup')



#' Creating a covid-19 flag based on the first lockdown initiated
#' renaming date to ds and daily dispatch to y
full_data <- full_data %>% ungroup()  %>%
  rename(c("ds"="date","y"="daily_dispatch_count")) %>%
  arrange(WH_PT,ds) %>%
  mutate(c19_season = if_else(ds > '2020-03-15',1,0))

#' Augumenting US holidays flag
full_data <- full_data %>% tk_augment_holiday_signature(.date_var = ds,
                                                        .holiday_pattern = "none",
                                                        .locale_set = "US",
                                                        .exchange_set = "US")

#' Extract time based features
#' 
full_data <- full_data %>% ungroup() %>% 
  mutate(year= lubridate::year(ds),
         month = lubridate::month(ds,label = TRUE),
         n_quarter = lubridate::quarter(ds),
         n_day = lubridate::day(ds),
         n_dom = lubridate::days_in_month(ds),
         n_week = lubridate::isoweek(ds),
         n_wday = lubridate::wday(ds,label = TRUE)) %>% 
  tk_augment_fourier(.date_var = ds,.periods = c(7,30,365),.K=1)


#' Create floor and Cap values at each group level
#' create trend component for tree based models
#' Crete Lag values
full_data <- full_data %>%
  group_by(WH_PT) %>%
  # for logistic growth of prophet
  mutate(floor = min(y,na.rm = T),cap=max(y,na.rm = T)*1.25) %>%
  mutate(n_idx = row_number(),n_idx_sqr = row_number()^2) %>%
  arrange(WH_PT,ds)%>% 
  tk_augment_lags(y,.lags = c(1,7,30,365)) %>%
  ungroup()



################# Modeling #################
#' 
#' 

#' Create train and test data for ML models
tr_data <- full_data %>% filter(is_train==1) %>%
  mutate_if(is.factor,as.numeric) %>%
  mutate_if(is.character,as.factor) %>%
  mutate_at(all_of("year"),factor) %>%
  select(-GRP_ID) %>%
  replace_na(list(y_lag365=0,y_lag7=0,y_lag30=0))


te_data <- full_data %>% filter(is_train==0)%>%
  mutate_if(is.factor,as.numeric) %>%
  mutate_if(is.character,as.factor) %>%
  mutate_at(all_of("year"),factor) %>%
  select(-GRP_ID) %>%
  replace_na(list(y_lag365=0))


#' validation set - last 45 days
val_data <- tr_data %>% group_by(WH_PT) %>% slice_tail(n=45) %>% ungroup()
val_data %>% group_by(WH_PT) %>% summarise(min_dt = min(ds),
                                           max_dt = max(ds),
                                           cnt = n())


#' Use this data for all prophet models
hist_data <- full_data %>% filter(is_train==1) %>%
  select(WH_PT,ds,y,is_weekend,is_wh_cls,c19_season,locale_US,cap,floor)
fut_data <- full_data %>% filter(is_train==0)%>%
  select(WH_PT,ds,is_weekend,is_wh_cls,c19_season,locale_US,cap,floor)


#' Prep for prophet
#' 
unique_wh <- full_data %>% distinct(WH_PT) %>% pull(WH_PT)

xreg_prop <- colnames(hist_data)[c(4:7)]
xreg_prop_log <- colnames(hist_data)[c(4:9)]

validation_data <- tibble(NULL)
forecast_prediction <- tibble(NULL)
#wh<- unique_wh[1]

##### Prophet Model #####

#' run the for loop for prophet using xreg values
for(wh in unique_wh){
  ## Prophet
  message('====== Running prophet for ',wh)
  
  df_x <- hist_data %>% filter(WH_PT==wh) %>%
    select(-WH_PT)
  df_y <- fut_data %>% filter(WH_PT==wh) %>%
    select(-WH_PT)
  val_prophet <- val_data %>% filter(WH_PT==wh) %>%
    ungroup() %>% 
    select(-WH_PT) 
  
  fit_prophet <- prophet(daily.seasonality = FALSE)
  
  if(fit_prophet$growth=='linear'){
    model_name <- 'Prophet_Linear'
    xreg_cols <- xreg_prop
  }else{
    model_name <- 'Prophet_Logistic'
    xreg_cols <- xreg_prop_log
  }
  
  if(!is.null(xreg_cols)){ #
    for(xr in xreg_cols){
      fit_prophet <- add_regressor(fit_prophet,xr)
    }
  }
  
  fit_prophet <- fit.prophet(fit_prophet,df_x)
  
  prophet_val <- stats::predict(fit_prophet,val_prophet) %>% pull(yhat)
  
  prophet_pred <- stats::predict(fit_prophet,df_y) %>% pull(yhat)
  
  va_fit <- tibble(ds=val_prophet$ds,
                   actual=val_prophet$y,
                   yhat=pmax(prophet_val,0),
                   WH_PT=wh,
                   model = model_name)
  
  fut_fit <- tibble(ds=df_y$ds,
                    yhat=pmax(prophet_pred,0),
                    WH_PT=wh,
                    model = model_name)
  validation_data <- rbind(validation_data,va_fit)
  forecast_prediction <- rbind(forecast_prediction,fut_fit)
}

hist_samp <- sample(unique_wh,1)
validation_data %>% filter(WH_PT==hist_samp & model=='Prophet_Linear') %>%
  ggplot(aes(ds))+
  geom_line(aes(y=actual),color="red")+
  geom_line(aes(y=yhat),color="steelblue")+
  labs(title=hist_samp)


#Weekly prediction or submission - Prophet
weekly_sub <- weekly_sub %>% mutate(flg=1)

test_data <- full_data %>% filter(is_train==0) %>%
  select(ID,ds,WH_PT) %>% left_join(.,weekly_sub,by="ID") %>%
  mutate(WK_NBR = as.integer(format(ds, '%V'))) %>%
  left_join(.,forecast_prediction,by=c("ds"="ds","WH_PT")) %>%
  mutate(prophet=pmax(yhat,0))


weekly_submission <- test_data %>% group_by(WH_PT,WK_NBR) %>%
  mutate(weekly_dispatch_count = sum(yhat))


weekly_sub_final <- weekly_submission %>% filter(flg==1) %>% ungroup() %>%
  select(ID,weekly_dispatch_count)


#" Final submission - Prophet

filename <- paste('ak_prophet',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(weekly_sub_final,paste0(filename,'.csv',collapse = ''),row.names = FALSE)

write.csv(hist_fit,'prophet_hist_data_actual_pred.csv',row.names = FALSE)


#############################
#############################
#### Catboost Baseline


y <- 'y'
xreg_cols_cat <- te_data %>% select(starts_with('ds_'),
                                         starts_with('n_'),
                                         contains('is_w'),
                                         ends_with('_US'),
                                         'year','month','states',
                                         'county','warehouse_ID',
                                         'Product_Type','c19_season') %>%
  colnames()

tr_pool <- catboost.load_pool(data = tr_data[,xreg_cols_cat],
                              label = unlist(tr_data[,y]))

te_pool <- catboost.load_pool(data = te_data[,xreg_cols_cat])
va_pool <- catboost.load_pool(data = val_data[,xreg_cols_cat],
                              label = unlist(val_data[,y]))

baseline_model <- catboost.train(tr_pool)

bs_feat_imp <- as_tibble(catboost.get_feature_importance(baseline_model))%>% 
  rownames_to_column() %>% cbind(.,xreg_cols_cat) %>% arrange(desc(V1))
bs_feat_imp


#' Catboost with slight parameters 
model <- catboost.train(tr_pool,
                        params = list(
                          iterations=1000,
                          depth = 5,
                          learning_rate = 0.01,
                          loss_function="MAPE",
                          random_seed = 55,
                          l2_leaf_reg=8,
                          langevin=TRUE,
                          early_stopping_rounds=200,
                          od_type='Iter',
                          metric_period = 50,
                          od_wait=20,
                          rsm=0.5,
                          use_best_model=TRUE))

feat_imp <- as_tibble(catboost.get_feature_importance(model))%>% 
  rownames_to_column() %>% cbind(.,xreg_cols_cat) %>% arrange(desc(V1))
feat_imp


#'Traing prediction and evalualting for the last 45 days
bs_tr_pred <- catboost.predict(baseline_model,va_pool)
tr_pred <- catboost.predict(model,va_pool)


tr_validation <- tibble(ds = val_data$n_idx,WH_PT = val_data$WH_PT,
                        y=val_data$y, b_yhat=bs_tr_pred,
                        yhat = tr_pred)

tr_validation <- tr_validation %>%
  mutate(b_residual = y-b_yhat,
         residual = y-yhat)


hist_samp <- sample(unique_wh,1)
tr_validation %>% filter(WH_PT%in%hist_samp) %>% 
  ggplot(aes(ds))+
  geom_line(aes(y=y),color="red")+
  geom_line(aes(y=pmax(b_yhat,0)),color="steelblue")+
  geom_line(aes(y=pmax(yhat,0)),color='#00ba38')+
  scale_colour_manual("", 
                      breaks = c("Cat Variant", "Actual", "Baseline"),
                      values = c("Cat Variant"="#00ba38", "Actual"="red", 
                                 "Baseline"="steelblue"))+
  labs(title="Baseline vs Static tuned catboost model")+
  facet_wrap(~WH_PT,nrow = 4,ncol = 1)

#' Test predict
bs_te_pred <- catboost.predict(baseline_model,te_pool)
te_pred <- catboost.predict(model,te_pool)

te_full_pred <- tibble(WH_PT = te_data$WH_PT,
                       bs_yhat = pmax(bs_te_pred,0),
                       yhat = pmax(te_pred,0))%>%
  group_by(WH_PT) %>%
  mutate(idx=row_number()) %>% ungroup()



### weekly submission

weekly_sub <- weekly_sub %>% mutate(flg=1)

test_data <- full_data %>% filter(is_train==0) %>%
  select(ID,ds,WH_PT) %>% left_join(.,weekly_sub,by="ID") %>%
  mutate(WK_NBR = as.integer(format(ds, '%V'))) %>%
  group_by(WH_PT) %>% mutate(idx=row_number()) %>% ungroup() %>%
  left_join(.,te_full_pred,by=c("WH_PT","idx"))


weekly_submission1 <- test_data %>% group_by(WH_PT,WK_NBR) %>%
  mutate(weekly_dispatch_count = sum(bs_yhat)) %>% #yhat for MAPE loss function model
  ungroup() %>%
  filter(flg==1) %>% select(ID,weekly_dispatch_count)


weekly_submission2 <- test_data %>% group_by(WH_PT,WK_NBR) %>%
  mutate(weekly_dispatch_count = sum(yhat)) %>% ungroup() %>%
  filter(flg==1) %>% select(ID,weekly_dispatch_count)

# Baseline - 79.84
filename <- paste('catboost_baseline_RMSE',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(weekly_submission1,paste0(filename,'.csv',collapse = ''),row.names = FALSE)

#Slight tuning - 78.28
filename <- paste('catboost_baseline_MAPE',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(weekly_submission2,paste0(filename,'.csv',collapse = ''),row.names = FALSE)



##############################
##############################
##### H2O Modeling 



xreg_cols_h2o <- te_data %>% select(starts_with('ds_'),
                                starts_with('n_'),
                                contains('is_w'),
                                ends_with('_US'),
                                'year','month','states',
                                'county','warehouse_ID',
                                'Product_Type','c19_season') %>%
  colnames()

# initialize H2O
h2o.init()

tr_h2o <- as.h2o(tr_data[,c(xreg_cols_h2o,y)])
te_h2o <- as.h2o(te_data[,xreg_cols_h2o])


almname <- paste('ak_h2o_automl',format(Sys.time(),"%d%H%M%S"),sep = '_')
autoML <- h2o.automl(xreg_cols_h2o,y,training_frame = tr_h2o,seed=223,
                     max_models = 10,stopping_metric=c("RMSE"),
                     exclude_algos = c("StackedEnsemble",
                                       "DeepLearning"))

autoML
leader_name <- as_tibble(autoML@leaderboard) %>% 
  dplyr::slice(1) %>% dplyr::pull(model_id)
leader_model <- h2o.getModel(leader_name)

#h2o.download_mojo(leader_model, getwd(), FALSE)

h2o.varimp_heatmap(autoML)

h2o.varimp_plot(leader_model)

h2o.shap_explain_row_plot(leader_model,te_h2o,row_index = 2)

yhat <- h2o.predict(leader_model,te_h2o) %>% 
  as_tibble() %>%
  mutate(WH_PT=te_data$WH_PT,ds=te_data$ds)
summary(yhat)

# weekly submission
weekly_sub <- weekly_sub %>% mutate(flg=1)

test_data <- full_data %>% filter(is_train==0) %>%
  select(ID,ds,WH_PT) %>% left_join(.,weekly_sub,by="ID") %>%
  mutate(WK_NBR = as.integer(format(ds, '%V'))) %>%
  left_join(.,yhat,by=c("ds","WH_PT")) %>% ungroup() %>%
  mutate(predict = pmax(predict,0))



weekly_submission <- test_data %>% group_by(WH_PT,WK_NBR) %>%
  mutate(weekly_dispatch_count = sum(predict))


weekly_sub_final <- weekly_submission %>% filter(flg==1) %>% ungroup() %>%
  select(ID,weekly_dispatch_count)


filename <- paste('ak_h2o_baseline_gbm_pmax',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(weekly_sub_final,paste0(filename,'.csv',collapse = ''),row.names = FALSE)





