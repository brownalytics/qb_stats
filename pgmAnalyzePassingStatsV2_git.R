###############  PROGRAM HEADER #########################
# Program Name:  pgmAnalyzePassingStats_git
# 
# Purpose:  Analyze Passing data from PFR.com 
# 
# Program Change Log:
# 2020-01-17 Program created
#
#
############### END PROGRAM HEADER ##########################
options(stringsAsFactors = FALSE, warn = -1)

#### load requisite libraries ####
library(openxlsx)
library(sqldf)
library(glue)
library(tidyverse)
library(broom)
library(gt)
library(webshot)

#### read in dependent files ####
passer_data_pfr = read.csv('https://raw.githubusercontent.com/brownalytics/qb_stats/master/pfr_passer_stats.csv')

passer_data_fo = read.csv('https://raw.githubusercontent.com/brownalytics/qb_stats/master/fo_passer_stats.csv')

passer_data_nflscrapr = read.csv('https://raw.githubusercontent.com/brownalytics/qb_stats/master/nflscrapr_passer_stats.csv')

passer_data_airyards = read.csv('https://raw.githubusercontent.com/brownalytics/qb_stats/master/cpoe_airyards.csv')

today_dt = format(Sys.Date(), format = '%Y%m%d')

#### declare or source necessary functions ####

fncModelSummary_Univariate <- function(model = model){
  
  ## Purpose: Extract model name, p-value and R-square from univariate models and append them to dataframe
  ##          'model_summary' in the .GlobalEnv for comparison
  
  require(broom)
  require(tidyverse)
  
  model_name = deparse(substitute(model))
  model_ivar = tidy(model)$term[2]
  model_pval = round(as.numeric(format(glance(model)$p.value, scientific = FALSE)),9)
  model_r2 = round(glance(model)$adj.r.squared,4)
  
  if(exists('model_summary', envir = .GlobalEnv)==TRUE){
    get('model_summary', envir = .GlobalEnv)
    
    tmp_summary = tibble(model_name, model_ivar, model_pval, model_r2)
    
    model_summary <<- rbind(model_summary, tmp_summary)
    
  }else{
    
    model_summary <<-  tibble(model_name, model_ivar, model_pval, model_r2)
    
  } ## closing bracket: if(exists('model_summary', envir = .GlobalEnv)==TRUE)
  
} ## closing bracket: fncModelParams

#### wrangle pfr data ####

passer_data = passer_data_pfr

passer_data <- passer_data %>% 
  mutate(adj_ppr = (td + first_downs)/att
         ,ns_fd = first_downs-td
         ,ns_fd_pg = ns_fd/g
         ,ns_fd_pa = ns_fd/att
         ,td_int_ratio = td/int) %>% 
  filter(szn < 2019) %>% 
  mutate(td_int_ratio = ifelse(td_int_ratio == 'Inf', NA, td_int_ratio))

#### split data into train/test sets ####
set.seed(65)

#### analyze pfr correlations to win_pct ####

lm_cmp_pct = lm(data = passer_data
                ,win_pct ~ cmp_pct
                ,method = 'qr')
summary(lm_cmp_pct)


lm_ns_fd = lm(data = passer_data
              ,win_pct ~ ns_fd
              ,method = 'qr')
summary(lm_ns_fd)

lm_ns_fd_pg = lm(data = passer_data
                 ,win_pct ~ ns_fd_pg
                 ,method = 'qr')
summary(lm_ns_fd_pg)

lm_ns_fd_pa = lm(data = passer_data
                 ,win_pct ~ ns_fd_pa
                 ,method = 'qr')
summary(lm_ns_fd_pa)

lm_pass_yds = lm(data = passer_data
                 ,win_pct ~ pass_yds
                 ,method = 'qr')
summary(lm_pass_yds)


lm_td = lm(data = passer_data
           ,win_pct ~ td
           ,method = 'qr')
summary(lm_td)

lm_td_pct = lm(data = passer_data
               ,win_pct ~ td_pct
               ,method = 'qr')
summary(lm_td_pct)


lm_td_pg = lm(data = passer_data
              ,win_pct ~ td_pg
              ,method = 'qr')
summary(lm_td_pg)

lm_td_pc = lm(data = passer_data
              ,win_pct ~ td_pc
              ,method = 'qr')
summary(lm_td_pc)


lm_int = lm(data = passer_data
            ,win_pct ~ int
            ,method = 'qr')
summary(lm_int)

lm_int_pct = lm(data = passer_data
                ,win_pct ~ int_pct
                ,method = 'qr')
summary(lm_int_pct)

lm_int_pg = lm(data = passer_data
               ,win_pct ~ int_pg
               ,method = 'qr')
summary(lm_int_pg)

lm_int_pinc = lm(data = passer_data
                 ,win_pct ~ int_pinc
                 ,method = 'qr')
summary(lm_int_pinc)

lm_td_int_ratio = lm(data = passer_data
                     ,win_pct ~ td_int_ratio
                     ,method = 'qr')
summary(lm_td_int_ratio)


lm_first_downs = lm(data = passer_data
                    ,win_pct ~ first_downs
                    ,method = 'qr')
summary(lm_first_downs)

lm_fd_pg = lm(data = passer_data
              ,win_pct ~ fd_pg
              ,method = 'qr')
summary(lm_fd_pg)

lm_fd_pa = lm(data = passer_data
              ,win_pct ~ fd_pa
              ,method = 'qr')
summary(lm_fd_pa)

lm_ypa = lm(data = passer_data
            ,win_pct ~ ypa
            ,method = 'qr')
summary(lm_ypa)

lm_adj_ypa = lm(data = passer_data
                ,win_pct ~ adj_ypa
                ,method = 'qr')
summary(lm_adj_ypa)

lm_ypc = lm(data = passer_data
            ,win_pct ~ ypc
            ,method = 'qr')
summary(lm_ypc)

lm_ypg = lm(data = passer_data
            ,win_pct ~ ypg
            ,method = 'qr')
summary(lm_ypg)

lm_passer_rating = lm(data = passer_data
                      ,win_pct ~ rate
                      ,method = 'qr')
summary(lm_passer_rating)

lm_qbr = lm(data = passer_data
            ,win_pct ~ qbr
            ,method = 'qr')
summary(lm_qbr)

lm_sk = lm(data = passer_data
           ,win_pct ~ sk
           ,method = 'qr')
summary(lm_sk)

lm_sack_yds = lm(data = passer_data
                 ,win_pct ~ sack_yds
                 ,method = 'qr')
summary(lm_sack_yds)

lm_net_ypa = lm(data = passer_data
                ,win_pct ~ net_ypa
                ,method = 'qr')
summary(lm_net_ypa)

lm_adj_net_ypa = lm(data = passer_data
                    ,win_pct ~ adj_net_ypa
                    ,method = 'qr')
summary(lm_adj_net_ypa)

lm_sack_pct = lm(data = passer_data
                 ,win_pct ~ sack_pct
                 ,method = 'qr')
summary(lm_sack_pct)

lm_adj_ppr = lm(data = passer_data
                ,win_pct ~ adj_ppr
                ,method = 'qr')
summary(lm_adj_ppr)

models = c('lm_adj_net_ypa', 'lm_adj_ypa', 'lm_cmp_pct', 'lm_fd_pa', 'lm_fd_pg', 'lm_adj_ppr', 'lm_first_downs', 'lm_int'
           ,'lm_int_pct', 'lm_int_pg', 'lm_int_pinc', 'lm_net_ypa', 'lm_pass_yds', 'lm_qbr', 'lm_passer_rating', 'lm_sack_pct'
           ,'lm_sack_yds', 'lm_sk', 'lm_td', 'lm_td_pc', 'lm_td_pg', 'lm_ypa', 'lm_ypc')

# fncModelSummary_Univariate(model = lm_adj_net_ypa)
# fncModelSummary_Univariate(model = lm_adj_ypa)
fncModelSummary_Univariate(model = lm_cmp_pct)
# fncModelSummary_Univariate(model = lm_fd_pa)
# fncModelSummary_Univariate(model = lm_fd_pg)
# fncModelSummary_Univariate(model = lm_adj_ppr)
# fncModelSummary_Univariate(model = lm_first_downs)
fncModelSummary_Univariate(model = lm_int)
fncModelSummary_Univariate(model = lm_int_pct)
fncModelSummary_Univariate(model = lm_int_pg)
fncModelSummary_Univariate(model = lm_int_pinc)
# fncModelSummary_Univariate(model = lm_net_ypa)
fncModelSummary_Univariate(model = lm_pass_yds)
fncModelSummary_Univariate(model = lm_sack_pct)
fncModelSummary_Univariate(model = lm_sack_yds)
fncModelSummary_Univariate(model = lm_sk)
fncModelSummary_Univariate(model = lm_td)
fncModelSummary_Univariate(model = lm_td_pc)
fncModelSummary_Univariate(model = lm_td_pg)
fncModelSummary_Univariate(model = lm_ypa)
fncModelSummary_Univariate(model = lm_ypc)
fncModelSummary_Univariate(model = lm_td_int_ratio)
fncModelSummary_Univariate(model = lm_ns_fd)
fncModelSummary_Univariate(model = lm_ns_fd_pg)
fncModelSummary_Univariate(model = lm_ns_fd_pa)


univariate_models_smy = model_summary %>% 
  arrange(model_pval, desc(model_r2))

rm(model_summary)

#### test for stability ####

passer_stability = passer_data %>% 
  select(player, cmp_pct, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_cmp_pct = lag(cmp_pct)) %>% 
  ungroup()

lm_cmp_pct_stab = lm(data = passer_stability
                     ,cmp_pct ~ ly_cmp_pct
                     ,method = 'qr')
summary(lm_cmp_pct_stab)

passer_stability = passer_data %>% 
  select(player, ns_fd, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_ns_fd = lag(ns_fd)) %>% 
  ungroup()

lm_ns_fd_stab = lm(data = passer_stability
                   ,ns_fd ~ ly_ns_fd
                   ,method = 'qr')
summary(lm_ns_fd_stab)

passer_stability = passer_data %>% 
  select(player, ns_fd_pg, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_ns_fd_pg = lag(ns_fd_pg)) %>% 
  ungroup()

lm_ns_fd_pg_stab = lm(data = passer_stability
                      ,ns_fd_pg ~ ly_ns_fd_pg
                      ,method = 'qr')
summary(lm_ns_fd_pg_stab)

passer_stability = passer_data %>% 
  select(player, ns_fd_pa, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_ns_fd_pa = lag(ns_fd_pa)) %>% 
  ungroup()

lm_ns_fd_pa_stab = lm(data = passer_stability
                      ,ns_fd_pa ~ ly_ns_fd_pa
                      ,method = 'qr')
summary(lm_ns_fd_pa_stab)


passer_stability = passer_data %>% 
  select(player, td_int_ratio, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_td_int_ratio = lag(td_int_ratio)) %>% 
  ungroup()

lm_td_int_ratio_stab = lm(data = passer_stability
                          ,td_int_ratio ~ ly_td_int_ratio
                          ,method = 'qr')
summary(lm_td_int_ratio_stab)


passer_stability = passer_data %>% 
  select(player, pass_yds, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_pass_yds = lag(pass_yds)) %>% 
  ungroup()

lm_pass_yds_stab = lm(data = passer_stability
                      ,pass_yds ~ ly_pass_yds
                      ,method = 'qr')
summary(lm_pass_yds_stab)

passer_stability = passer_data %>% 
  select(player, td, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_td = lag(td)) %>% 
  ungroup()

lm_td_stab = lm(data = passer_stability
                ,td ~ ly_td
                ,method = 'qr')
summary(lm_td_stab)

passer_stability = passer_data %>%
  select(player, td_pct, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_td_pct = lag(td_pct)) %>%
  ungroup()

lm_td_pct_stab = lm(data = passer_stability
                    ,td_pct ~ ly_td_pct
                    ,method = 'qr')
summary(lm_td_pct_stab)


passer_stability = passer_data %>% 
  select(player, td_pg, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_td_pg = lag(td_pg)) %>% 
  ungroup()

lm_td_pg_stab = lm(data = passer_stability
                   ,td_pg ~ ly_td_pg
                   ,method = 'qr')
summary(lm_td_pg_stab)

passer_stability = passer_data %>%
  select(player, td_pc, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_td_pc = lag(td_pc)) %>%
  ungroup()

lm_td_pc_stab = lm(data = passer_stability
                   ,td_pc ~ ly_td_pc
                   ,method = 'qr')
summary(lm_td_pc_stab)


passer_stability = passer_data %>%
  select(player, int_pct, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_int_pct = lag(int_pct)) %>%
  ungroup()

lm_int_pct_stab = lm(data = passer_stability
                     ,int_pct ~ ly_int_pct
                     ,method = 'qr')
summary(lm_int_pct_stab)

passer_stability = passer_data %>%
  select(player, int_pg, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_int_pg = lag(int_pg)) %>%
  ungroup()

lm_int_pg_stab = lm(data = passer_stability
                    ,int_pg ~ ly_int_pg
                    ,method = 'qr')
summary(lm_int_pg_stab)

passer_stability = passer_data %>%
  select(player, int_pinc, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_int_pinc = lag(int_pinc)) %>%
  ungroup()

lm_int_pinc_stab = lm(data = passer_stability
                      ,int_pinc ~ ly_int_pinc
                      ,method = 'qr')
summary(lm_int_pinc_stab)

passer_stability = passer_data %>% 
  select(player, first_downs, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_first_downs = lag(first_downs)) %>% 
  ungroup()

lm_first_downs_stab = lm(data = passer_stability
                         ,first_downs ~ ly_first_downs
                         ,method = 'qr')
summary(lm_first_downs_stab)

passer_stability = passer_data %>% 
  select(player, fd_pg, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_fd_pg = lag(fd_pg)) %>% 
  ungroup()

lm_fd_pg_stab = lm(data = passer_stability
                   ,fd_pg ~ ly_fd_pg
                   ,method = 'qr')
summary(lm_fd_pg_stab)

passer_stability = passer_data %>% 
  select(player, fd_pa, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_fd_pa = lag(fd_pa)) %>% 
  ungroup()

lm_fd_pa_stab = lm(data = passer_stability
                   ,fd_pa ~ ly_fd_pa
                   ,method = 'qr')
summary(lm_fd_pa_stab)

passer_stability = passer_data %>%
  select(player, ypa, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_ypa = lag(ypa)) %>%
  ungroup()

lm_ypa_stab = lm(data = passer_stability
                 ,ypa ~ ly_ypa
                 ,method = 'qr')
summary(lm_ypa_stab)

passer_stability = passer_data %>%
  select(player, adj_ypa, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_adj_ypa = lag(adj_ypa)) %>%
  ungroup()

lm_adj_ypa_stab = lm(data = passer_stability
                     ,adj_ypa ~ ly_adj_ypa
                     ,method = 'qr')
summary(lm_adj_ypa_stab)

passer_stability = passer_data %>%
  select(player, ypc, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_ypc = lag(ypc)) %>%
  ungroup()

lm_ypc_stab = lm(data = passer_stability
                 ,ypc ~ ly_ypc
                 ,method = 'qr')
summary(lm_ypc_stab)

passer_stability = passer_data %>% 
  select(player, ypg, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_ypg = lag(ypg)) %>% 
  ungroup()

lm_ypg_stab = lm(data = passer_stability
                 ,ypg ~ ly_ypg
                 ,method = 'qr')
summary(lm_ypg_stab)

passer_stability = passer_data %>%
  select(player, rate, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_rate = lag(rate)) %>%
  ungroup()

lm_rate_stab = lm(data = passer_stability
                  ,rate ~ ly_rate
                  ,method = 'qr')
summary(lm_rate_stab)

passer_stability = passer_data %>%
  select(player, qbr, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_qbr = lag(qbr)) %>%
  ungroup()

lm_qbr_stab = lm(data = passer_stability
                 ,qbr ~ ly_qbr
                 ,method = 'qr')
summary(lm_qbr_stab)

passer_stability = passer_data %>%
  select(player, sk, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_sk = lag(sk)) %>%
  ungroup()

lm_sk_stab = lm(data = passer_stability
                ,sk ~ ly_sk
                ,method = 'qr')
summary(lm_sk_stab)

passer_stability = passer_data %>%
  select(player, net_ypa, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_net_ypa = lag(net_ypa)) %>%
  ungroup()

lm_net_ypa_stab = lm(data = passer_stability
                     ,net_ypa ~ ly_net_ypa
                     ,method = 'qr')
summary(lm_net_ypa_stab)

passer_stability = passer_data %>%
  select(player, adj_net_ypa, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_adj_net_ypa = lag(adj_net_ypa)) %>%
  ungroup()

lm_adj_net_ypa_stab = lm(data = passer_stability
                         ,adj_net_ypa ~ ly_adj_net_ypa
                         ,method = 'qr')
summary(lm_adj_net_ypa_stab)

passer_stability = passer_data %>% 
  select(player, sack_pct, szn) %>%
  arrange(player, szn) %>% 
  group_by(player) %>% 
  mutate(ly_sack_pct = lag(sack_pct)) %>% 
  ungroup()

lm_sack_pct_stab = lm(data = passer_stability
                      ,sack_pct ~ ly_sack_pct
                      ,method = 'qr')
summary(lm_sack_pct_stab)

passer_stability = passer_data %>%
  select(player, sack_yds, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_sack_yds = lag(sack_yds)) %>%
  ungroup()

lm_sack_yds_stab = lm(data = passer_stability
                      ,sack_yds ~ ly_sack_yds
                      ,method = 'qr')
summary(lm_sack_yds_stab)

passer_stability = passer_data %>%
  select(player, int, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_int = lag(int)) %>%
  ungroup()

lm_int_stab = lm(data = passer_stability
                 ,int ~ ly_int
                 ,method = 'qr')
summary(lm_int_stab)

passer_stability = passer_data %>%
  select(player, adj_ppr, szn) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_adj_ppr = lag(adj_ppr)) %>%
  ungroup()

lm_adj_ppr_stab = lm(data = passer_stability
                     ,adj_ppr ~ ly_adj_ppr
                     ,method = 'qr')
summary(lm_adj_ppr_stab)

# fncModelSummary_Univariate(model = lm_adj_net_ypa_stab)
# fncModelSummary_Univariate(model = lm_adj_ypa_stab)
fncModelSummary_Univariate(model = lm_cmp_pct_stab)
# fncModelSummary_Univariate(model = lm_fd_pa_stab)
# fncModelSummary_Univariate(model = lm_fd_pg_stab)
# fncModelSummary_Univariate(model = lm_adj_ppr_stab)
# fncModelSummary_Univariate(model = lm_first_downs_stab)
fncModelSummary_Univariate(model = lm_int_stab)
fncModelSummary_Univariate(model = lm_int_pct_stab)
fncModelSummary_Univariate(model = lm_int_pg_stab)
fncModelSummary_Univariate(model = lm_int_pinc_stab)
# fncModelSummary_Univariate(model = lm_net_ypa_stab)
fncModelSummary_Univariate(model = lm_pass_yds_stab)
fncModelSummary_Univariate(model = lm_sack_pct_stab)
fncModelSummary_Univariate(model = lm_sack_yds_stab)
fncModelSummary_Univariate(model = lm_sk_stab)
fncModelSummary_Univariate(model = lm_td_stab)
fncModelSummary_Univariate(model = lm_td_pc_stab)
fncModelSummary_Univariate(model = lm_td_pg_stab)
fncModelSummary_Univariate(model = lm_ypa_stab)
fncModelSummary_Univariate(model = lm_ypc_stab)
fncModelSummary_Univariate(model = lm_td_int_ratio_stab)
fncModelSummary_Univariate(model = lm_ns_fd_stab)
fncModelSummary_Univariate(model = lm_ns_fd_pg_stab)
fncModelSummary_Univariate(model = lm_ns_fd_pa_stab)


univariate_stability_smy = model_summary %>% 
  arrange(model_pval, desc(model_r2))


rm(model_summary)

#### models combined comparison ####

univariate_stability_smy = univariate_stability_smy %>% 
  mutate(model_ivar = gsub("ly_", "", model_ivar)) %>% 
  rename('yoy_stab_model_pval' = 'model_pval'
         ,'yoy_stab_model_r2' = 'model_r2') %>% 
  filter(!(model_ivar %in% c('qbr', 'adj_ppr', 'first_downs', 'fd_pa', 'fd_pg', 'net_ypa'
                             ,'adj_net_ypa', 'adj_ypa')))

mean_model_r2 = mean(univariate_models_smy$model_r2)
mean_yoy_model_r2 = mean(univariate_stability_smy$yoy_stab_model_r2)

## filter out any variables that are not individual stats (adj_ppr, qbr, first_downs (include td's)) ##
combined_univariate_models = univariate_models_smy %>% 
  left_join(univariate_stability_smy %>% select(model_ivar, yoy_stab_model_pval,yoy_stab_model_r2)
            ,by = 'model_ivar')

combined_model_comp = univariate_models_smy %>% 
  left_join(univariate_stability_smy %>% select(model_ivar, yoy_stab_model_pval,yoy_stab_model_r2)
            ,by = 'model_ivar') %>% 
  filter(model_ivar != 'qbr') %>% 
  filter(model_pval < 0.0001 & yoy_stab_model_pval < 0.0001 ) %>% 
  filter(yoy_stab_model_r2 >= mean_yoy_model_r2)

#### construct multivariate model from: 
#### 1) individually significant variables
#### 2) which are stable year-over-year (R^2 > avg. r2 for all conventional stats)

#### Model iteration #1: ####

lm_qb_win_pct = lm(data = passer_data
                   ,win_pct ~ td + td_pg + pass_yds + ns_fd + cmp_pct +
                     ns_fd_pg + sack_pct
                   ,method = 'qr')
summary(lm_qb_win_pct)

# Call:
#   lm(formula = win_pct ~ td + td_pg + pass_yds + ns_fd + cmp_pct + 
#        ns_fd_pg + sack_pct, data = passer_data, method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.52610 -0.12107 -0.00348  0.12157  0.65717 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.001e-01  1.168e-01  -0.857   0.3920    
# td           1.560e-03  4.963e-03   0.314   0.7534    
# td_pg        1.426e-01  6.436e-02   2.216   0.0271 *  
#   pass_yds    -5.259e-05  4.756e-05  -1.106   0.2693    
# ns_fd        2.708e-03  1.195e-03   2.267   0.0238 *  
#   cmp_pct      1.196e+00  2.097e-01   5.703 1.93e-08 ***
#   ns_fd_pg    -4.739e-02  1.002e-02  -4.732 2.84e-06 ***
#   sack_pct    -1.729e+00  3.696e-01  -4.678 3.66e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1811 on 545 degrees of freedom
# Multiple R-squared:  0.3911,	Adjusted R-squared:  0.3832 
# F-statistic:    50 on 7 and 545 DF,  p-value: < 2.2e-16


## treat collinearity

cor1 <- passer_data %>% 
  select(win_pct,td , td_pg , pass_yds , ns_fd , cmp_pct , ns_fd_pg , sack_pct) %>% 
  cor() 

View(cor1)

# cor1: r = .99 b/t pass_yds and ns_fd; dropped ns_fd 


#### Model iteration #2: ####

lm_qb_win_pct = lm(data = passer_data
                   ,win_pct ~ td + td_pg + pass_yds + cmp_pct +
                     ns_fd_pg + sack_pct
                   ,method = 'qr')
summary(lm_qb_win_pct)

# Call:
#   lm(formula = win_pct ~ td + td_pg + pass_yds + cmp_pct + ns_fd_pg + 
#        sack_pct, data = passer_data, method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.50517 -0.12334 -0.00346  0.11820  0.64097 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.115e-01  1.172e-01  -0.952    0.341    
# td           5.882e-03  4.600e-03   1.279    0.202    
# td_pg        6.261e-02  5.401e-02   1.159    0.247    
# pass_yds     3.589e-05  2.727e-05   1.316    0.189    
# cmp_pct      1.187e+00  2.105e-01   5.640 2.74e-08 ***
#   ns_fd_pg    -3.380e-02  8.051e-03  -4.198 3.15e-05 ***
#   sack_pct    -1.803e+00  3.695e-01  -4.879 1.40e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1818 on 546 degrees of freedom
# Multiple R-squared:  0.3853,	Adjusted R-squared:  0.3786 
# F-statistic: 57.04 on 6 and 546 DF,  p-value: < 2.2e-16

## treat collinearity

cor2 <- passer_data %>% 
  select(win_pct,td , td_pg , pass_yds , cmp_pct , ns_fd_pg , sack_pct) %>% 
  cor() 

View(cor2)

# cor2: r = .92 b/t pass_yds and td; dropped pass_yds 

#### Model iteration #3: ####

lm_qb_win_pct = lm(data = passer_data
                   ,win_pct ~ td + td_pg + cmp_pct + ns_fd_pg + sack_pct
                   ,method = 'qr')
summary(lm_qb_win_pct)

# Call:
#   lm(formula = win_pct ~ td + td_pg + cmp_pct + ns_fd_pg + sack_pct, 
#      data = passer_data, method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.50871 -0.12464 -0.00376  0.11972  0.63776 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.108720   0.117223  -0.927    0.354    
# td           0.011477   0.001758   6.527 1.53e-10 ***
#   td_pg        0.006942   0.033614   0.207    0.836    
# cmp_pct      1.191024   0.210582   5.656 2.50e-08 ***
#   ns_fd_pg    -0.026056   0.005501  -4.736 2.78e-06 ***
#   sack_pct    -1.808955   0.369757  -4.892 1.31e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1819 on 547 degrees of freedom
# Multiple R-squared:  0.3834,	Adjusted R-squared:  0.3777 
# F-statistic: 68.01 on 5 and 547 DF,  p-value: < 2.2e-16

## treat collinearity

cor3 <- passer_data %>% 
  select(win_pct,td , td_pg , cmp_pct , ns_fd_pg , sack_pct) %>% 
  cor() 

View(cor3)

# cor3: r = .90 b/t td_pg and td; dropped td_pg 

#### Model iteration #4: ####

lm_qb_win_pct = lm(data = passer_data
                   ,win_pct ~ td + cmp_pct + ns_fd_pg + sack_pct
                   ,method = 'qr')
summary(lm_qb_win_pct)

# Call:
#   lm(formula = win_pct ~ td + cmp_pct + ns_fd_pg + sack_pct, data = passer_data, 
#      method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.51091 -0.12360 -0.00351  0.11902  0.64067 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.108623   0.117120  -0.927    0.354    
# td           0.011760   0.001101  10.677  < 2e-16 ***
#   cmp_pct      1.195363   0.209348   5.710 1.85e-08 ***
#   ns_fd_pg    -0.025846   0.005402  -4.784 2.21e-06 ***
#   sack_pct    -1.816037   0.367841  -4.937 1.05e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1817 on 548 degrees of freedom
# Multiple R-squared:  0.3833,	Adjusted R-squared:  0.3788 
# F-statistic: 85.15 on 4 and 548 DF,  p-value: < 2.2e-16

## treat collinearity

cor4 <- passer_data %>% 
  select(win_pct, td , cmp_pct , ns_fd_pg , sack_pct) %>% 
  cor() 

View(cor4)

# cor4: r = .70 b/t ns_fd_pg and td; dropped ns_fd_pg

#### Model iteration #5: ####

lm_qb_win_pct = lm(data = passer_data
                   ,win_pct ~ td + cmp_pct + sack_pct
                   ,method = 'qr')
summary(lm_qb_win_pct)

# Call:
#   lm(formula = win_pct ~ td + cmp_pct + sack_pct, data = passer_data, 
#      method = "qr")
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.3773 -0.1261  0.0024  0.1246  0.6199 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.1285156  0.1193564  -1.077 0.282070    
# td           0.0091293  0.0009732   9.380  < 2e-16 ***
#   cmp_pct      0.8617391  0.2012892   4.281 2.19e-05 ***
#   sack_pct    -1.3784238  0.3633206  -3.794 0.000165 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1853 on 549 degrees of freedom
# Multiple R-squared:  0.3576,	Adjusted R-squared:  0.354 
# F-statistic: 101.8 on 3 and 549 DF,  p-value: < 2.2e-16

## treat collinearity << no evidence of collinearity present

cor5 <- passer_data %>% 
  select(win_pct, td , cmp_pct , sack_pct) %>% 
  cor() 

View(cor5)

# cor5: r values < .6 and evidence of collinearity is not present in the model

#### approach #2 ####


fncModelSummary_Univariate(model = lm_adj_net_ypa)
fncModelSummary_Univariate(model = lm_adj_ypa)
fncModelSummary_Univariate(model = lm_cmp_pct)
fncModelSummary_Univariate(model = lm_fd_pa)
fncModelSummary_Univariate(model = lm_fd_pg)
fncModelSummary_Univariate(model = lm_adj_ppr)
fncModelSummary_Univariate(model = lm_first_downs)
fncModelSummary_Univariate(model = lm_int)
fncModelSummary_Univariate(model = lm_int_pct)
fncModelSummary_Univariate(model = lm_int_pg)
fncModelSummary_Univariate(model = lm_int_pinc)
fncModelSummary_Univariate(model = lm_net_ypa)
fncModelSummary_Univariate(model = lm_pass_yds)
fncModelSummary_Univariate(model = lm_sack_pct)
fncModelSummary_Univariate(model = lm_sack_yds)
fncModelSummary_Univariate(model = lm_sk)
fncModelSummary_Univariate(model = lm_td)
fncModelSummary_Univariate(model = lm_td_pc)
fncModelSummary_Univariate(model = lm_td_pg)
fncModelSummary_Univariate(model = lm_ypa)
fncModelSummary_Univariate(model = lm_ypc)
fncModelSummary_Univariate(model = lm_td_int_ratio)
fncModelSummary_Univariate(model = lm_ns_fd)
fncModelSummary_Univariate(model = lm_ns_fd_pg)
fncModelSummary_Univariate(model = lm_ns_fd_pa)


univariate_models_smy_app2 = model_summary %>% 
  arrange(model_pval, desc(model_r2))

rm(model_summary)

fncModelSummary_Univariate(model = lm_adj_net_ypa_stab)
fncModelSummary_Univariate(model = lm_adj_ypa_stab)
fncModelSummary_Univariate(model = lm_cmp_pct_stab)
fncModelSummary_Univariate(model = lm_fd_pa_stab)
fncModelSummary_Univariate(model = lm_fd_pg_stab)
fncModelSummary_Univariate(model = lm_adj_ppr_stab)
fncModelSummary_Univariate(model = lm_first_downs_stab)
fncModelSummary_Univariate(model = lm_int_stab)
fncModelSummary_Univariate(model = lm_int_pct_stab)
fncModelSummary_Univariate(model = lm_int_pg_stab)
fncModelSummary_Univariate(model = lm_int_pinc_stab)
fncModelSummary_Univariate(model = lm_net_ypa_stab)
fncModelSummary_Univariate(model = lm_pass_yds_stab)
fncModelSummary_Univariate(model = lm_sack_pct_stab)
fncModelSummary_Univariate(model = lm_sack_yds_stab)
fncModelSummary_Univariate(model = lm_sk_stab)
fncModelSummary_Univariate(model = lm_td_stab)
fncModelSummary_Univariate(model = lm_td_pc_stab)
fncModelSummary_Univariate(model = lm_td_pg_stab)
fncModelSummary_Univariate(model = lm_ypa_stab)
fncModelSummary_Univariate(model = lm_ypc_stab)
fncModelSummary_Univariate(model = lm_td_int_ratio_stab)
fncModelSummary_Univariate(model = lm_ns_fd_stab)
fncModelSummary_Univariate(model = lm_ns_fd_pg_stab)
fncModelSummary_Univariate(model = lm_ns_fd_pa_stab)


univariate_stability_smy_app2 = model_summary %>% 
  arrange(model_pval, desc(model_r2))


rm(model_summary)

#### approach #2: models combined comparison ####

univariate_stability_smy_app2 = univariate_stability_smy_app2 %>% 
  mutate(model_ivar = gsub("ly_", "", model_ivar)) %>% 
  rename('yoy_stab_model_pval' = 'model_pval'
         ,'yoy_stab_model_r2' = 'model_r2')

mean_model_r2_app2 = mean(univariate_models_smy_app2$model_r2)
mean_yoy_model_r2_app2 = mean(univariate_stability_smy_app2$yoy_stab_model_r2)

## filter out any variables that are not individual stats (adj_ppr, qbr, first_downs (include td's)) ##
combined_univariate_models_app2 = univariate_models_smy_app2 %>% 
  left_join(univariate_stability_smy_app2 %>% select(model_ivar, yoy_stab_model_pval,yoy_stab_model_r2)
            ,by = 'model_ivar')

combined_model_comp_app2 = univariate_models_smy_app2 %>% 
  left_join(univariate_stability_smy_app2 %>% select(model_ivar, yoy_stab_model_pval,yoy_stab_model_r2)
            ,by = 'model_ivar') %>% 
  filter(model_ivar != 'qbr') %>% 
  filter(model_pval < 0.0001 & yoy_stab_model_pval < 0.0001 ) %>% 
  filter(yoy_stab_model_r2 >= mean_yoy_model_r2_app2)

#### Approach #2: Model iteration #1 ####

lm_qb_win_pct_app2 = lm(data = passer_data
                        ,win_pct ~ adj_ppr + td + fd_pa + td_pg + first_downs + pass_yds + ns_fd + cmp_pct +
                          fd_pg + ns_fd_pg + sack_pct
                        ,method = 'qr')
summary(lm_qb_win_pct_app2)
# 
# Call:
#   lm(formula = win_pct ~ adj_ppr + td + fd_pa + td_pg + first_downs + 
#        pass_yds + ns_fd + cmp_pct + fd_pg + ns_fd_pg + sack_pct, 
#      data = passer_data, method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.48500 -0.11623  0.00686  0.10980  0.61912 
# 
# Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.450e-01  1.110e-01  -2.206   0.0278 *  
#   adj_ppr      4.978e+00  3.655e+00   1.362   0.1737    
# td           3.117e-03  5.544e-03   0.562   0.5743    
# fd_pa       -3.112e+00  4.128e+00  -0.754   0.4512    
# td_pg       -8.230e-02  1.563e-01  -0.527   0.5987    
# first_downs -3.380e-04  1.178e-03  -0.287   0.7742    
# pass_yds     7.605e-05  4.717e-05   1.612   0.1075    
# ns_fd               NA         NA      NA       NA    
# cmp_pct      3.778e-01  2.184e-01   1.730   0.0842 .  
# fd_pg       -3.215e-02  1.918e-02  -1.676   0.0943 .  
# ns_fd_pg            NA         NA      NA       NA    
# sack_pct    -2.032e+00  3.481e-01  -5.837 9.13e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1697 on 543 degrees of freedom
# Multiple R-squared:  0.467,	Adjusted R-squared:  0.4582 
# F-statistic: 52.87 on 9 and 543 DF,  p-value: < 2.2e-16


## treat collinearity

cor1 <- passer_data %>% 
  select(win_pct , adj_ppr , td , fd_pa , td_pg , first_downs , pass_yds , ns_fd , cmp_pct ,
         fd_pg , ns_fd_pg , sack_pct) %>% 
  cor() 

View(cor1)

# cor1: dropped ns_fd and ns_fd_pg for singularities

#### Approach #2: Model iteration #2 ####

lm_qb_win_pct_app2 = lm(data = passer_data
                        ,win_pct ~ adj_ppr + td + fd_pa + td_pg + first_downs + pass_yds + cmp_pct +
                          fd_pg + sack_pct
                        ,method = 'qr')
summary(lm_qb_win_pct_app2)

# Call:
#   lm(formula = win_pct ~ adj_ppr + td + fd_pa + td_pg + first_downs + 
#        pass_yds + cmp_pct + fd_pg + sack_pct, data = passer_data, 
#      method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.48500 -0.11623  0.00686  0.10980  0.61912 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.450e-01  1.110e-01  -2.206   0.0278 *  
#   adj_ppr      4.978e+00  3.655e+00   1.362   0.1737    
# td           3.117e-03  5.544e-03   0.562   0.5743    
# fd_pa       -3.112e+00  4.128e+00  -0.754   0.4512    
# td_pg       -8.230e-02  1.563e-01  -0.527   0.5987    
# first_downs -3.380e-04  1.178e-03  -0.287   0.7742    
# pass_yds     7.605e-05  4.717e-05   1.612   0.1075    
# cmp_pct      3.778e-01  2.184e-01   1.730   0.0842 .  
# fd_pg       -3.215e-02  1.918e-02  -1.676   0.0943 .  
# sack_pct    -2.032e+00  3.481e-01  -5.837 9.13e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1697 on 543 degrees of freedom
# Multiple R-squared:  0.467,	Adjusted R-squared:  0.4582 
# F-statistic: 52.87 on 9 and 543 DF,  p-value: < 2.2e-16


## treat collinearity

cor2 <- passer_data %>% 
  select(win_pct , adj_ppr , td , fd_pa , td_pg , first_downs , pass_yds , cmp_pct ,
         fd_pg , sack_pct) %>% 
  cor() 

View(cor2)

# cor2: r = .99 b/t pass_yds and first_downs; dropped pass_yds

#### Approach #2: Model iteration #3 ####

lm_qb_win_pct_app2 = lm(data = passer_data
                        ,win_pct ~ adj_ppr + td + fd_pa + td_pg + first_downs + cmp_pct +
                          fd_pg + sack_pct
                        ,method = 'qr')
summary(lm_qb_win_pct_app2)

# Call:
#   lm(formula = win_pct ~ adj_ppr + td + fd_pa + td_pg + first_downs + 
#        cmp_pct + fd_pg + sack_pct, data = passer_data, method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.49585 -0.11250  0.00716  0.10916  0.61640 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.2276569  0.1106844  -2.057   0.0402 *  
#   adj_ppr      5.3095396  3.6543161   1.453   0.1468    
# td           0.0030168  0.0055522   0.543   0.5871    
# fd_pa       -3.6349723  4.1216211  -0.882   0.3782    
# td_pg       -0.0809354  0.1565313  -0.517   0.6053    
# first_downs  0.0012160  0.0006774   1.795   0.0732 .  
# cmp_pct      0.4348873  0.2158111   2.015   0.0444 *  
#   fd_pg       -0.0324389  0.0192122  -1.688   0.0919 .  
# sack_pct    -1.9798979  0.3471212  -5.704 1.93e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.17 on 544 degrees of freedom
# Multiple R-squared:  0.4645,	Adjusted R-squared:  0.4566 
# F-statistic: 58.98 on 8 and 544 DF,  p-value: < 2.2e-16

## treat collinearity

cor3 <- passer_data %>% 
  select(win_pct , adj_ppr , td , fd_pa , td_pg , first_downs , cmp_pct ,
         fd_pg , sack_pct) %>% 
  cor() 

View(cor3)

# cor3: r = .98 b/t fd_pa and adj_ppr; dropped fd_pa

#### Approach #2: Model iteration #4 ####

lm_qb_win_pct_app2 = lm(data = passer_data
                        ,win_pct ~ adj_ppr + td + td_pg + first_downs + cmp_pct +
                          fd_pg + sack_pct
                        ,method = 'qr')
summary(lm_qb_win_pct_app2)

# Call:
#   lm(formula = win_pct ~ adj_ppr + td + td_pg + first_downs + cmp_pct + 
#        fd_pg + sack_pct, data = passer_data, method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.51998 -0.11198  0.00631  0.10815  0.61030 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.2329524  0.1104989  -2.108   0.0355 *  
#   adj_ppr      2.0937398  0.2414123   8.673  < 2e-16 ***
#   td           0.0012990  0.0051983   0.250   0.8028    
# td_pg        0.0427637  0.0694782   0.615   0.5385    
# first_downs  0.0014090  0.0006409   2.198   0.0283 *  
#   cmp_pct      0.4265278  0.2155589   1.979   0.0484 *  
#   fd_pg       -0.0472723  0.0092836  -5.092 4.89e-07 ***
#   sack_pct    -1.9766007  0.3470303  -5.696 2.01e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1699 on 545 degrees of freedom
# Multiple R-squared:  0.4637,	Adjusted R-squared:  0.4568 
# F-statistic: 67.32 on 7 and 545 DF,  p-value: < 2.2e-16

## treat collinearity

cor4 <- passer_data %>% 
  select(win_pct , adj_ppr , td , td_pg , first_downs , cmp_pct ,fd_pg , sack_pct) %>% 
  cor() 

View(cor4)

# cor4: r = .92 b/t first_downs and td; dropped first_downs

#### Approach #2: Model iteration #5 ####

lm_qb_win_pct_app2 = lm(data = passer_data
                        ,win_pct ~ adj_ppr + td + td_pg + cmp_pct +
                          fd_pg + sack_pct
                        ,method = 'qr')
summary(lm_qb_win_pct_app2)

# Call:
#   lm(formula = win_pct ~ adj_ppr + td + td_pg + cmp_pct + fd_pg + 
#        sack_pct, data = passer_data, method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.51368 -0.10777  0.00698  0.11014  0.61665 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.236524   0.110874  -2.133   0.0333 *  
#   adj_ppr      2.114240   0.242077   8.734  < 2e-16 ***
#   td           0.012140   0.001650   7.356 7.01e-13 ***
#   td_pg       -0.088461   0.035681  -2.479   0.0135 *  
#   cmp_pct      0.419604   0.216291   1.940   0.0529 .  
# fd_pg       -0.030310   0.005181  -5.850 8.45e-09 ***
#   sack_pct    -2.024786   0.347551  -5.826 9.71e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1705 on 546 degrees of freedom
# Multiple R-squared:  0.4589,	Adjusted R-squared:  0.453 
# F-statistic: 77.19 on 6 and 546 DF,  p-value: < 2.2e-16

## treat collinearity

cor5 <- passer_data %>% 
  select(win_pct , adj_ppr , td , td_pg , cmp_pct ,fd_pg , sack_pct) %>% 
  cor() 

View(cor5)

# cor5: r = .90 b/t td_pg and td; dropped td_pg

#### Approach #2: Model iteration #6 ####

lm_qb_win_pct_app2 = lm(data = passer_data
                        ,win_pct ~ adj_ppr + td + cmp_pct +
                          fd_pg + sack_pct
                        ,method = 'qr')
summary(lm_qb_win_pct_app2)

# Call:
#   lm(formula = win_pct ~ adj_ppr + td + cmp_pct + fd_pg + sack_pct, 
#      data = passer_data, method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.50086 -0.11160  0.00599  0.11720  0.57219 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.221998   0.111239  -1.996   0.0465 *  
#   adj_ppr      1.880318   0.223980   8.395 4.01e-16 ***
#   td           0.009351   0.001213   7.707 6.10e-14 ***
#   cmp_pct      0.483568   0.215755   2.241   0.0254 *  
#   fd_pg       -0.033768   0.005013  -6.736 4.12e-11 ***
#   sack_pct    -1.958607   0.348151  -5.626 2.95e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1713 on 547 degrees of freedom
# Multiple R-squared:  0.4529,	Adjusted R-squared:  0.4479 
# F-statistic: 90.55 on 5 and 547 DF,  p-value: < 2.2e-16

## treat collinearity

cor6 <- passer_data %>% 
  select(win_pct , adj_ppr , td , cmp_pct ,fd_pg , sack_pct) %>% 
  cor() 

View(cor6)

# cor6: r = .78 b/t fd_pg and td; dropped fd_pg

#### Approach #2: Model iteration #7 ####

lm_qb_win_pct_app2 = lm(data = passer_data
                        ,win_pct ~ adj_ppr + td + cmp_pct + sack_pct
                        ,method = 'qr')
summary(lm_qb_win_pct_app2)

# Call:
#   lm(formula = win_pct ~ adj_ppr + td + cmp_pct + sack_pct, data = passer_data, 
#      method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.48167 -0.12184  0.01105  0.12260  0.48956 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.227508   0.115652  -1.967 0.049667 *  
#   adj_ppr      1.541833   0.226937   6.794 2.85e-11 ***
#   td           0.005279   0.001094   4.826 1.80e-06 ***
#   cmp_pct      0.166146   0.218906   0.759 0.448188    
# sack_pct    -1.342960   0.349279  -3.845 0.000135 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1781 on 548 degrees of freedom
# Multiple R-squared:  0.4075,	Adjusted R-squared:  0.4031 
# F-statistic: 94.21 on 4 and 548 DF,  p-value: < 2.2e-16

## treat collinearity

cor7 <- passer_data %>% 
  select(win_pct , adj_ppr , td , cmp_pct , sack_pct) %>% 
  cor() 

View(cor7)

# cor7: r = .71 b/t adj_ppr and td; dropped td

#### Approach #2: Model iteration #8 ####

lm_qb_win_pct_app2 = lm(data = passer_data
                        ,win_pct ~ adj_ppr + cmp_pct + sack_pct
                        ,method = 'qr')
summary(lm_qb_win_pct_app2)

# Call:
#   lm(formula = win_pct ~ adj_ppr + cmp_pct + sack_pct, data = passer_data, 
#      method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.57902 -0.11871  0.01608  0.12724  0.42877 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.4226     0.1105  -3.823 0.000147 ***
#   adj_ppr       2.1093     0.1980  10.653  < 2e-16 ***
#   cmp_pct       0.3272     0.2207   1.482 0.138823    
# sack_pct     -1.8017     0.3429  -5.255 2.12e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1817 on 549 degrees of freedom
# Multiple R-squared:  0.3823,	Adjusted R-squared:  0.3789 
# F-statistic: 113.2 on 3 and 549 DF,  p-value: < 2.2e-16

## treat collinearity

cor8 <- passer_data %>% 
  select(win_pct , adj_ppr , cmp_pct , sack_pct) %>% 
  cor() 

View(cor8)

# cor8: r = .67 b/t adj_ppr and cmp_pct; dropped cmp_pct

#### Approach #2: Model iteration #9 ####

lm_qb_win_pct_app2 = lm(data = passer_data
                        ,win_pct ~ adj_ppr + sack_pct
                        ,method = 'qr')
summary(lm_qb_win_pct_app2)

# Call:
#   lm(formula = win_pct ~ adj_ppr + sack_pct, data = passer_data, 
#      method = "qr")
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.59532 -0.11850  0.01928  0.12545  0.43338 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.29325    0.06797  -4.314 1.90e-05 ***
#   adj_ppr      2.29914    0.15119  15.207  < 2e-16 ***
#   sack_pct    -1.82920    0.34272  -5.337 1.38e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1819 on 550 degrees of freedom
# Multiple R-squared:  0.3798,	Adjusted R-squared:  0.3775 
# F-statistic: 168.4 on 2 and 550 DF,  p-value: < 2.2e-16


## no evidence of multicollinearity present!


#### predict xWinPct ###

## test data ##

qbs_2019 = passer_data_pfr %>% 
  mutate(adj_ppr = (first_downs+td)/att) %>% 
  filter(szn == 2019) %>% 
  mutate(xWinPct = predict(lm_qb_win_pct, newdata = ., type = 'response')
         ,xWinPct2 = predict(lm_qb_win_pct_app2, newdata = ., type = 'response')) %>% 
  select(player, tm, qbrec, td, cmp_pct, sack_pct,adj_ppr, win_pct, xWinPct, xWinPct2) %>% 
  mutate(WPOE = win_pct - xWinPct
         ,xWinPct_rank = rank(-xWinPct, ties.method = 'min', na.last = 'keep')
         ,WPOE2 = win_pct - xWinPct2
         ,xWinPct_rank2 = rank(-xWinPct2, ties.method = 'min', na.last = 'keep')) %>% 
  arrange(xWinPct_rank)

test_data = qbs_2019 %>% 
  mutate(abs_dev = abs(win_pct - xWinPct)
         ,abs_dev2 = abs(win_pct - xWinPct2))

MAD = mean(test_data$abs_dev)
MAD_games = MAD*16

MAD2 = mean(test_data$abs_dev2)
MAD_games2 = MAD2*16

#### advanced stats ####

## join football outsiders data ##
adv_passer_data = passer_data %>% 
  mutate(player_abbv = paste0(substr(player, 1,1),".",substr(player, regexpr(' ', player)+1, nchar(player)))) %>% 
  left_join(passer_data_fo %>% select(player, szn, dyar, dvoa, voa), by = c('player_abbv'='player', 'szn'='szn')) %>% 
  mutate(dyar = as.numeric(sub(",","",dyar)))

lm_qbr = lm(data = adv_passer_data
            ,win_pct ~ qbr
            ,method = 'qr')
summary(lm_qbr)

lm_dvoa = lm(data = adv_passer_data
             ,win_pct ~ dvoa
             ,method = 'qr')
summary(lm_dvoa)

lm_voa = lm(data = adv_passer_data
            ,win_pct ~ voa
            ,method = 'qr')
summary(lm_voa)

lm_dyar = lm(data = adv_passer_data
             ,win_pct ~ dyar
             ,method = 'qr')
summary(lm_dyar)

adv_passer_data = adv_passer_data %>% 
  left_join(passer_data_nflscrapr %>% select(passer_player_name, szn, epa_db, cpoe)
            , by = c('player_abbv'='passer_player_name', 'szn'='szn')) %>% 
  rename('brownalytics_cpoe' = 'cpoe')

lm_epa_db = lm(data = adv_passer_data
               ,win_pct ~ epa_db
               ,method = 'qr')
summary(lm_epa_db)

lm_brownalytics_cpoe = lm(data = adv_passer_data
                          ,win_pct ~ brownalytics_cpoe
                          ,method = 'qr')
summary(lm_brownalytics_cpoe)

adv_passer_data = adv_passer_data %>% 
  left_join(passer_data_airyards %>% select(full_name, szn, cpoe)
            , by = c('player'='full_name', 'szn'='szn')) %>% 
  rename('airyards_cpoe' = 'cpoe')

lm_airyards_cpoe = lm(data = adv_passer_data
                      ,win_pct ~ airyards_cpoe
                      ,method = 'qr')
summary(lm_airyards_cpoe)

fncModelSummary_Univariate(model = lm_dvoa)
fncModelSummary_Univariate(model = lm_dyar)
fncModelSummary_Univariate(model = lm_voa)
fncModelSummary_Univariate(model = lm_epa_db)
fncModelSummary_Univariate(model = lm_brownalytics_cpoe)
fncModelSummary_Univariate(model = lm_airyards_cpoe)
fncModelSummary_Univariate(model = lm_qbr)

adv_model_summary = model_summary
rm(model_summary)

#### advanced model stability ####
adv_passer_stability = adv_passer_data %>%
  select(player, szn, qbr) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_qbr = lag(qbr)) %>%
  ungroup()

lm_qbr_stab = lm(data = adv_passer_stability
                 ,qbr ~ ly_qbr
                 ,method = 'qr')
summary(lm_qbr_stab)

adv_passer_stability = adv_passer_data %>%
  select(player, szn, dvoa) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_dvoa = lag(dvoa)) %>%
  ungroup()

lm_dvoa_stab = lm(data = adv_passer_stability
                  ,dvoa ~ ly_dvoa
                  ,method = 'qr')
summary(lm_dvoa_stab)

adv_passer_stability = adv_passer_data %>%
  select(player, szn, dyar) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_dyar = lag(dyar)) %>%
  ungroup()

lm_dyar_stab = lm(data = adv_passer_stability
                  ,dyar ~ ly_dyar
                  ,method = 'qr')
summary(lm_dyar_stab)

adv_passer_stability = adv_passer_data %>%
  select(player, szn, voa) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_voa = lag(voa)) %>%
  ungroup()

lm_voa_stab = lm(data = adv_passer_stability
                 ,voa ~ ly_voa
                 ,method = 'qr')
summary(lm_voa_stab)

adv_passer_stability = adv_passer_data %>%
  select(player, szn, epa_db) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_epa_db = lag(epa_db)) %>%
  ungroup()

lm_epa_db_stab = lm(data = adv_passer_stability
                    ,epa_db ~ ly_epa_db
                    ,method = 'qr')
summary(lm_epa_db_stab)

adv_passer_stability = adv_passer_data %>%
  select(player, szn, brownalytics_cpoe) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_brownalytics_cpoe = lag(brownalytics_cpoe)) %>%
  ungroup()

lm_brownalytics_cpoe_stab = lm(data = adv_passer_stability
                               ,brownalytics_cpoe ~ ly_brownalytics_cpoe
                               ,method = 'qr')
summary(lm_brownalytics_cpoe_stab)

adv_passer_stability = adv_passer_data %>%
  select(player, szn, airyards_cpoe) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_airyards_cpoe = lag(airyards_cpoe)) %>%
  ungroup()

lm_airyards_cpoe_stab = lm(data = adv_passer_stability
                           ,airyards_cpoe ~ ly_airyards_cpoe
                           ,method = 'qr')
summary(lm_airyards_cpoe_stab)

fncModelSummary_Univariate(model = lm_dvoa_stab)
fncModelSummary_Univariate(model = lm_dyar_stab)
fncModelSummary_Univariate(model = lm_voa_stab)
fncModelSummary_Univariate(model = lm_epa_db_stab)
fncModelSummary_Univariate(model = lm_brownalytics_cpoe_stab)
fncModelSummary_Univariate(model = lm_airyards_cpoe_stab)
fncModelSummary_Univariate(model = lm_qbr_stab)

adv_stability_smy = model_summary
rm(model_summary)

#### combine model outputs ####

adv_stability_smy = adv_stability_smy %>% 
  mutate(model_ivar = gsub("ly_", "", model_ivar)) %>% 
  rename('yoy_stab_model_pval' = 'model_pval'
         ,'yoy_stab_model_r2' = 'model_r2')

adv_combined_model_comp = adv_model_summary %>% 
  left_join(adv_stability_smy %>% select(model_ivar, yoy_stab_model_pval,yoy_stab_model_r2)
            ,by = 'model_ivar') %>% 
  arrange(desc(model_r2), desc(yoy_stab_model_r2))

#### add fd+td-sack% to advanced table ####

rm(model_summary)

fncModelSummary_Univariate(model = lm_qb_win_pct)
fncModelSummary_Univariate(model = lm_qb_win_pct_app2)


tmp_model_smy = model_summary %>% 
  mutate(model_ivar = ifelse(model_ivar == 'td', 'td+cmp_pct-sack_pct', 
                             ifelse(model_ivar == 'adj_ppr', 'adj_ppr-sack_pct',NA)))
rm(model_summary)

adv_passer_stability = adv_passer_data %>%
  mutate(xWinPct = predict(lm_qb_win_pct, newdata =., type = 'response')) %>%
  select(player, szn, xWinPct) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_xWinPct = lag(xWinPct)) %>%
  ungroup()

lm_xWinPct_stab = lm(data = adv_passer_stability
                     ,xWinPct ~ ly_xWinPct
                     ,method = 'qr')
summary(lm_xWinPct_stab)


adv_passer_stability = adv_passer_data %>%
  mutate(xWinPct2 = predict(lm_qb_win_pct_app2, newdata =., type = 'response')) %>%
  select(player, szn, xWinPct2) %>%
  arrange(player, szn) %>%
  group_by(player) %>%
  mutate(ly_xWinPct2 = lag(xWinPct2)) %>%
  ungroup()

lm_xWinPct2_stab = lm(data = adv_passer_stability
                      ,xWinPct2 ~ ly_xWinPct2
                      ,method = 'qr')
summary(lm_xWinPct2_stab)

fncModelSummary_Univariate(model = lm_xWinPct_stab)
fncModelSummary_Univariate(model = lm_xWinPct2_stab)

tmp_adv_model_smy = model_summary %>% 
  mutate(model_ivar = ifelse(model_ivar == 'ly_xWinPct', 'conventional_stats', 
                             ifelse(model_ivar == 'ly_xWinPct2', 'adj_stats',NA))) %>% 
  rename('yoy_stab_model_pval' = 'model_pval'
         ,'yoy_stab_model_r2' = 'model_r2')

tmp_model_comp = tmp_model_smy %>% 
  left_join(tmp_adv_model_smy %>% select(model_ivar, yoy_stab_model_pval,yoy_stab_model_r2)
            ,by = 'model_ivar') 

adv_combined_model_comp = adv_combined_model_comp %>% 
  full_join(tmp_model_comp
            , by = c("model_name", "model_ivar", "model_pval", "model_r2", "yoy_stab_model_pval", "yoy_stab_model_r2")) %>% 
  arrange(desc(model_r2)) 


# lm_cpoe = lm(data = adv_passer_data, airyards_cpoe ~ brownalytics_cpoe, method = 'qr')

#### predict xWinPct from advanced stats ####

qbs_2019_adv = qbs_2019 %>% 
  mutate(szn = 2019
         ,player_abbv = paste0(substr(player, 1,1),".",substr(player, regexpr(' ', player)+1, nchar(player)))) %>% 
  left_join(passer_data_fo %>% select(player, szn, dyar, dvoa, voa), by = c('player_abbv'='player', 'szn'='szn')) %>% 
  mutate(dyar = as.numeric(dyar)) %>% 
  left_join(passer_data_nflscrapr %>% select(passer_player_name, szn, epa_db, cpoe)
            , by = c('player_abbv'='passer_player_name', 'szn'='szn')) %>% 
  rename('brownalytics_cpoe' = 'cpoe') %>% 
  left_join(passer_data_airyards %>% select(full_name, szn, cpoe)
            , by = c('player'='full_name', 'szn'='szn')) %>% 
  left_join(passer_data_pfr %>% select(player, szn, qbr)
            , by = c('player'='player', 'szn'='szn')) %>% 
  rename('airyards_cpoe' = 'cpoe'
         ,'xWinPct_ba' = 'xWinPct'
         , 'xWinPct_ba2' = 'xWinPct2') %>% 
  mutate(xWinPct_epa_db = predict(lm_epa_db, newdata = ., type = 'response')
         ,xWinPct_qbr = predict(lm_qbr, newdata = ., type = 'response')
         ,xWinPct_dvoa = predict(lm_dvoa, newdata = ., type = 'response')
         ,xWinPct_voa = predict(lm_voa, newdata = ., type = 'response')
         ,xWinPct_dyar = predict(lm_dyar, newdata = ., type = 'response')
         ,xWinPct_brownalytics_cpoe = predict(lm_brownalytics_cpoe, newdata = ., type = 'response')
         ,xWinPct_airyards_cpoe = predict(lm_airyards_cpoe, newdata = ., type = 'response'))

lm_xwin_epa = lm(data = qbs_2019_adv
                 ,xWinPct_ba ~ xWinPct_epa_db
                 ,method = 'qr')
summary(lm_xwin_epa)

lm_xwin_epa2 = lm(data = qbs_2019_adv
                  ,xWinPct_ba2 ~ xWinPct_epa_db
                  ,method = 'qr')
summary(lm_xwin_epa2)


lm_xwin_dvoa = lm(data = qbs_2019_adv
                  ,xWinPct_ba ~ xWinPct_dvoa
                  ,method = 'qr')
summary(lm_xwin_dvoa)

lm_xwin_dvoa2 = lm(data = qbs_2019_adv
                   ,xWinPct_ba2 ~ xWinPct_dvoa
                   ,method = 'qr')
summary(lm_xwin_dvoa2)


lm_xwin_voa = lm(data = qbs_2019_adv
                 ,xWinPct_ba ~ xWinPct_voa
                 ,method = 'qr')
summary(lm_xwin_voa)

lm_xwin_voa2 = lm(data = qbs_2019_adv
                  ,xWinPct_ba2 ~ xWinPct_voa
                  ,method = 'qr')
summary(lm_xwin_voa2)


lm_xwin_dyar = lm(data = qbs_2019_adv
                  ,xWinPct_ba ~ xWinPct_dyar
                  ,method = 'qr')
summary(lm_xwin_dyar)

lm_xwin_dyar2 = lm(data = qbs_2019_adv
                   ,xWinPct_ba2 ~ xWinPct_dyar
                   ,method = 'qr')
summary(lm_xwin_dyar2)


lm_xwin_brownalytics_cpoe = lm(data = qbs_2019_adv
                               ,xWinPct_ba ~ xWinPct_brownalytics_cpoe
                               ,method = 'qr')
summary(lm_xwin_brownalytics_cpoe)

lm_xwin_brownalytics_cpoe2 = lm(data = qbs_2019_adv
                                ,xWinPct_ba2 ~ xWinPct_brownalytics_cpoe
                                ,method = 'qr')
summary(lm_xwin_brownalytics_cpoe2)

lm_xwin_airyards_cpoe = lm(data = qbs_2019_adv
                           ,xWinPct_ba ~ xWinPct_airyards_cpoe
                           ,method = 'qr')
summary(lm_xwin_airyards_cpoe)

lm_xwin_airyards_cpoe2 = lm(data = qbs_2019_adv
                            ,xWinPct_ba2 ~ xWinPct_airyards_cpoe
                            ,method = 'qr')
summary(lm_xwin_airyards_cpoe2)


lm_xwin_qbr = lm(data = qbs_2019_adv
                 ,xWinPct_ba ~ xWinPct_qbr 
                 ,method = 'qr')
summary(lm_xwin_qbr)

lm_xwin_qbr2 = lm(data = qbs_2019_adv
                  ,xWinPct_ba2 ~ xWinPct_qbr 
                  ,method = 'qr')
summary(lm_xwin_qbr2)


rm(model_summary)

fncModelSummary_Univariate(model = lm_xwin_epa)
fncModelSummary_Univariate(model = lm_xwin_dvoa)
fncModelSummary_Univariate(model = lm_xwin_voa)
fncModelSummary_Univariate(model = lm_xwin_dyar)
fncModelSummary_Univariate(model = lm_xwin_qbr)
fncModelSummary_Univariate(model = lm_xwin_airyards_cpoe)
fncModelSummary_Univariate(model = lm_xwin_brownalytics_cpoe)
fncModelSummary_Univariate(model = lm_xwin_epa2)
fncModelSummary_Univariate(model = lm_xwin_dvoa2)
fncModelSummary_Univariate(model = lm_xwin_voa2)
fncModelSummary_Univariate(model = lm_xwin_dyar2)
fncModelSummary_Univariate(model = lm_xwin_qbr2)
fncModelSummary_Univariate(model = lm_xwin_airyards_cpoe2)
fncModelSummary_Univariate(model = lm_xwin_brownalytics_cpoe2)


xWinPct_models_comp = model_summary
rm(model_summary)

xWinPct_models_comp1 = xWinPct_models_comp %>% 
  mutate(model_tested = ifelse(substr(model_name, nchar(model_name), nchar(model_name))=='2'
                               , 'Adjusted Conventional Stats'
                               , 'Conventional Stats')) %>% 
  filter(model_tested == 'Conventional Stats')

xWinPct_models_comp2 = xWinPct_models_comp %>% 
  mutate(model_tested = ifelse(substr(model_name, nchar(model_name), nchar(model_name))=='2'
                               , 'Adjusted Conventional Stats'
                               , 'Conventional Stats')) %>% 
  filter(model_tested == 'Adjusted Conventional Stats') %>% 
  rename('adj_model_pval' = 'model_pval'
         ,'adj_model_r2' = 'model_r2')

xWinPct_models_comp = xWinPct_models_comp1 %>% 
  left_join(xWinPct_models_comp2 %>% select(model_ivar, adj_model_pval, adj_model_r2), by = 'model_ivar') %>% 
  mutate(model_name = NULL
         ,model_tested = NULL
         ,model_ivar = sub('xWinPct_','', model_ivar))




##### END OF PROGRAM ####
