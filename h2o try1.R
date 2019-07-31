setwd("~/RProjects/Git_test_project/Git_test_project")

!install.packages("h2o")

library(tidyverse)
library(readxl)
library(h2o)

path <- "bank_term_deposit_marketing_analysis.xlsx"

sheets <- excel_sheets(path)


sheets %>% 
  map(~ read_excel(path=path, sheet = .)) %>% 
  set_names(sheets)


data_joined_tbl <- sheets[4:7] %>%
  map(~ read_excel(path = path, sheet = .)) %>%
  reduce(left_join)


data_joined_tbl


h2o.init()


data_joined_tbl <- data_joined_tbl %>%
  mutate_if(is.character, as.factor)



train <- as.h2o(data_joined_tbl)

h2o.describe(train)

y <- "TERM_DEPOSIT"

x <- setdiff(names(train),c(y,"ID"))


aml <- h2o.automl(
  y=y,
  x=x,
  training_frame = train,
  project_name = "term_deposit",
  max_models = 10,
  seed = 1
)


lb <- aml@leaderboard


print(lb)

print(lb, n=nrow(lb))


model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value=TRUE)[1])
metalearner <- h2o.getModel(se@model$metalearner$name)

h2o.varimp(metalearner)
h2o.varimp_plot(metalearner)


xgb <- h2o.getModel(grep("XGBoost", model_ids, value=TRUE)[1])
h2o.varimp(xgb)
h2o.varimp_plot(xgb)

xgb@parameters
