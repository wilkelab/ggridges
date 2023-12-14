

test_that("rpart", {
  
  skip_on_cran()
  skip_if_not_installed("rpart")
  skip_if_not_installed("vip")
  skip_if_not_installed("caret")
  skip_if_not_installed("parsnip")
  
  test_rpart <- function(form, mode, resp_var, type = "vector"){
    
    df = select(mtcars2, -ids)
    
    set.seed(1)
    # train models --------------------------------------------
    set.seed(1)
    m <- rpart::rpart(form, df)
    
    set.seed(1)
    m_parsnip <- parsnip::decision_tree(mode = mode) %>%
      parsnip::set_engine("rpart") %>%
      parsnip::fit(form, df)
    
    set.seed(1)
    suppressWarnings({
      train <- caret::train(form, df, method = 'rpart')
    })
    
    m_wf <- parsnip::decision_tree(mode = mode) %>%
      parsnip::set_engine("rpart")
    
    rec_prep = recipes::recipe(form, df) %>%
      recipes::prep()
    
    wf <- workflows::workflow() %>%
      workflows::add_model(m_wf) %>%
      workflows::add_recipe(rec_prep) %>%
      parsnip::fit(df)
    
    # imp -----------------------------------------------------
    imp <- vip::vi_model(m)
    imp_parsnip <- vip::vi_model(m_parsnip)
    imp_caret <- caret::varImp(train)
    
    expect_error(tidy_imp(imp, df, resp_var = NULL))
    expect_error(tidy_imp(imp_parsnip, df, resp_var = NULL))
    
    imp <- tidy_imp(imp, df, resp_var = resp_var)
    imp_parsnip <- tidy_imp(imp_parsnip, df, resp_var = resp_var)
    imp_caret <- tidy_imp(imp_caret, df)
    
    expect_true(all(nrow(c(imp, imp_parsnip, imp_caret)) == nrow(df) - 1))
    
    # plots --------------------------------------------------
    dspace <- get_data_space(df, imp, degree = 3, bins = 5)
    pred <- predict(m, newdata = dspace, type = type)
    pred_pdp <- get_pdp_predictions(df, imp, m, degree = 3, bins = 5,
                                    .f_predict = function(...) predict(..., type = type))
    
    if(mode == "classification"){
      p <- alluvial_model_response(pred, dspace, imp, degree = 3)
      p <- alluvial_model_response(pred, dspace, imp, degree = 3, method = "pdp")
      p <- alluvial_model_response_caret(train, df, degree = 3)
      p <- alluvial_model_response_caret(train, df, degree = 3, method = "pdp")
      p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3)
      p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3, method = "pdp")
    }
    
    if(mode == "regression") {
      expect_warning(p <- alluvial_model_response(pred, dspace, imp, degree = 3))
      
      p <- alluvial_model_response(pred, dspace, imp, degree = 3,
                                   params_bin_numeric_pred = list(bins =2),
                                   bin_labels = c("H", "L"))
      
      p <- alluvial_model_response(pred_pdp, dspace, imp, degree = 3,
                                   method = "pdp",
                                   params_bin_numeric_pred = list(bins =2),
                                   bin_labels = c("H", "L"))
      
      p <- alluvial_model_response_caret(train, df, degree = 3,
                                         params_bin_numeric_pred = list(bins =2),
                                         bin_labels = c("H", "L"))
      
      p <- alluvial_model_response_caret(train, df, degree = 3, method = "pdp",
                                         params_bin_numeric_pred = list(bins =2),
                                         bin_labels = c("H", "L"))
      
      p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3,
                                           params_bin_numeric_pred = list(bins =2),
                                           bin_labels = c("H", "L"))
      
      p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3, method = "pdp",
                                           params_bin_numeric_pred = list(bins =2),
                                           bin_labels = c("H", "L"))
      
      # TODO there seems to be a weird thing when using rpart with workflows
      # p <- alluvial_model_response_parsnip(wf, df, degree = 3,
      #                                      params_bin_numeric_pred = list(bins =2),
      #                                      bin_labels = c("H", "L"), resp_var = resp_var)
      # 
      # p <- alluvial_model_response_parsnip(wf, df, degree = 3, method = "pdp",
      #                                      params_bin_numeric_pred = list(bins =2),
      #                                      bin_labels = c("H", "L"), resp_var = resp_var)
    }
    
  }
  
  test_rpart(form = disp ~ ., mode = "regression", resp_var = "disp", type = "vector")
  test_rpart(form = cyl ~ ., mode = "classification", resp_var = "cyl", type = "class")
  test_rpart(form = am ~ ., mode ="classification", resp_var = "am", type = "class")
  
})

test_that("earth", {
  skip_on_cran()
  skip_if_not_installed("earth")
  skip_if_not_installed("vip")
  skip_if_not_installed("caret")
  skip_if_not_installed("parsnip")
  
  test_earth <- function(form, mode, resp_var, type = "vector"){
    
    df = select(mtcars2, -ids)
    
    set.seed(1)
    # train models --------------------------------------------
    set.seed(1)
    m <- earth::earth(form, df)
    
    set.seed(1)
    m_parsnip <- parsnip::mars(mode = mode) %>%
      parsnip::set_engine("earth") %>%
      parsnip::fit(form, df)
    
    set.seed(1)
    suppressWarnings({
      train <- caret::train(form, df, method = 'earth')
    })
    # imp -----------------------------------------------------
    imp <- vip::vi_model(m)
    imp_parsnip <- vip::vi_model(m_parsnip)
    imp_caret <- caret::varImp(train)
    
    imp <- tidy_imp(imp, df)
    imp_parsnip <- tidy_imp(imp_parsnip, df)
    imp_caret <- tidy_imp(imp_caret, df, resp_var = resp_var)
    
    expect_true(all(nrow(c(imp, imp_parsnip, imp_caret)) == nrow(df) - 1))
    
    # plots --------------------------------------------------
    dspace <- get_data_space(df, imp, degree = 3, bins = 5)
    pred <- predict(m, newdata = dspace, type = type)
    pred_pdp <- get_pdp_predictions(df, imp, m, degree = 3, bins = 5)
    
    p <- alluvial_model_response(pred, dspace, imp, degree = 3)
    p <- alluvial_model_response(pred_pdp, dspace, imp, degree = 3, method = "pdp")
    p <- alluvial_model_response_caret(train, df, degree = 3, resp_var = resp_var)
    p <- alluvial_model_response_caret(train, df, degree = 3, method = "pdp", resp_var = resp_var)
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3)
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3, method = "pdp")
    
    
  }
  
  test_earth(form = disp ~ ., mode = "regression", resp_var = "disp", type = "link")
  
})

test_that("rf", {
  skip_on_cran()
  skip_if_not_installed("vip")
  skip_if_not_installed("caret")
  skip_if_not_installed("parsnip")
  
  test_rf <- function(form, mode, resp_var, type = "vector"){
    
    df = select(mtcars2, -ids)
    
    set.seed(1)
    # train models --------------------------------------------
    set.seed(1)
    m <- randomForest::randomForest(form, df)
    
    set.seed(1)
    m_parsnip <- parsnip::rand_forest(mode = mode) %>%
      parsnip::set_engine("randomForest") %>%
      parsnip::fit(form, df)
    
    set.seed(1)
    suppressWarnings({
      train <- caret::train(form, df, method = 'rf', trControl = caret::trainControl( method = 'none' ))
    })
    # imp -----------------------------------------------------
    imp <- vip::vi_model(m)
    imp_parsnip <- vip::vi_model(m_parsnip)
    imp_caret <- caret::varImp(train)
    
    imp <- tidy_imp(imp, df)
    imp_parsnip <- tidy_imp(imp_parsnip, df)
    imp_caret <- tidy_imp(imp_caret, df, resp_var = resp_var)
    
    expect_true(all(nrow(c(imp, imp_parsnip, imp_caret)) == nrow(df) - 1))
    
    # plots --------------------------------------------------
    dspace <- get_data_space(df, imp, degree = 3, bins = 5)
    pred <- predict(m, newdata = dspace, type = type)
    pred_pdp <- get_pdp_predictions(df, imp, m, degree = 3, bins = 5,
                                    .f_predict = function(...) predict(..., type = type))
    
    p <- alluvial_model_response(pred, dspace, imp, degree = 3)
    p <- alluvial_model_response(pred_pdp, dspace, imp, degree = 3, method = "pdp")
    p <- alluvial_model_response_caret(train, df, degree = 3)
    p <- alluvial_model_response_caret(train, df, degree = 3, method = "pdp")
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3)
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3, method = "pdp")
    
    
  }
  
  test_rf(form = disp ~ ., mode = "regression", resp_var = "disp", type = "response")
  test_rf(form = cyl ~ ., mode = "classification", resp_var = "cyl", type = "class")
  test_rf(form = am ~ ., mode ="classification", resp_var = "am", type = "class")
})

test_that("glm", {
  skip_on_cran()
  skip_if_not_installed("vip")
  skip_if_not_installed("caret")
  skip_if_not_installed("parsnip")
  
  test_glm <- function(form, mode, resp_var, type = "vector", family = "gaussian"){
    
    df = select(mtcars2, -ids)
    
    set.seed(1)
    # train models --------------------------------------------
    set.seed(1)
    suppressWarnings({
      m <- glm(form, df, family = family)
    })
    
    if(mode == "regression"){
      engine <- "lm"
      .f <- parsnip::linear_reg
    }else{
      engine <- "glm"
      .f <- parsnip::logistic_reg
    }
    set.seed(1)
    
    suppressWarnings({
    m_parsnip <- .f(mode = mode) %>%
      parsnip::set_engine(engine) %>%
      parsnip::fit(form, df)
    })
    
    set.seed(1)
    suppressWarnings({
      train <- caret::train(form, df, method = 'glm')
    })
    # imp -----------------------------------------------------
    imp <- vip::vi_model(m)
    imp_parsnip <- vip::vi_model(m_parsnip)
    imp_caret <- caret::varImp(train)
    
    imp <- tidy_imp(imp, df)
    imp_parsnip <- tidy_imp(imp_parsnip, df)
    imp_caret <- tidy_imp(imp_caret, df, resp_var = resp_var)
    
    expect_true(all(nrow(c(imp, imp_parsnip, imp_caret)) == nrow(df) - 1))
    
    # plots --------------------------------------------------
    dspace <- get_data_space(df, imp, degree = 3, bins = 5)
    pred <- predict(m, newdata = dspace, type = type)
    pred_pdp <- get_pdp_predictions(df, imp, m, degree = 3, bins = 5,
                                    .f_predict = function(...) predict(..., type = type))

    if(mode == "classification"){
      expect_warning(alluvial_model_response(pred, dspace, imp, degree = 3))
      p <- alluvial_model_response(pred, dspace, imp, degree = 3,
                                   bin_labels = c("H", "M", "L"),
                                   params_bin_numeric_pred = list(bins=3)                                  )
    }else{
      p <- alluvial_model_response(pred, dspace, imp, degree = 3)
      p <- alluvial_model_response(pred_pdp, dspace, imp, degree = 3, method = "pdp")
    }
    p <- alluvial_model_response_caret(train, df, degree = 3)
    p <- alluvial_model_response_caret(train, df, degree = 3, method = "pdp")
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3)
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3, method = "pdp")
    
    
  }
  test_glm(form = disp ~ ., mode = "regression", resp_var = "disp", type = "response", family = "gaussian")
  test_glm(form = am ~ ., mode ="classification", resp_var = "am", type = "response", family = "binomial")
})

test_that("xgboost", {
  skip("xgboost test skipped for performance")
  skip_on_cran()
  
  skip_if_not_installed("xgboost")
  skip_if_not_installed("vip")
  skip_if_not_installed("caret")
  skip_if_not_installed("parsnip")
  
  df = select(mtcars2, -ids) %>%
    mutate_if(is.factor, manip_factor_2_numeric)
  
  test_xgb <- function(form, mode, resp_var, X, y){
    
    
    set.seed(1)
    # train models --------------------------------------------
    set.seed(1)
    X <- X %>%
      as.matrix()
    
    m <- xgboost::xgboost(X, y, nrounds = 50)

    m_parsnip <- parsnip::boost_tree(mode = mode) %>%
      parsnip::set_engine("xgboost") %>%
      parsnip::fit(form, df)

    set.seed(1)
    suppressWarnings({
      train <- caret::train(form, df, method = 'xgbTree',
                            trControl = caret::trainControl(method = 'cv', repeats = 1))
    })
    # imp -----------------------------------------------------
    imp <- vip::vi_model(m)
    imp_parsnip <- vip::vi_model(m_parsnip)
    imp_caret <- caret::varImp(train)
    
    imp <- tidy_imp(imp, df, resp_var = resp_var)
    imp_parsnip <- tidy_imp(imp_parsnip, df, resp_var = resp_var)
    imp_caret <- tidy_imp(imp_caret, df, resp_var = resp_var)
    
    expect_true(all(nrow(c(imp, imp_parsnip, imp_caret)) == nrow(df) - 1))
    
    # plots --------------------------------------------------
    dspace <- get_data_space(df, imp, degree = 3, bins = 5)
    pred <- predict(m, newdata = as.matrix(dspace[,m$feature_names]), type = type)
    
    .f_predict <- function(m, newdata, type){
      predict(m, newdata = as.matrix(dspace[,m$feature_names]), type = type)
    }
    pred_pdp <- get_pdp_predictions(df, imp, m, degree = 3, bins = 5,
                                    .f_predict = .f_predict)
    
    p <- alluvial_model_response(pred, dspace, imp, degree = 3)
    p <- alluvial_model_response(pred_pdp, dspace, imp, degree = 3, method = "pdp")
    p <- alluvial_model_response_caret(train, df, degree = 3, resp_var = resp_var)
    p <- alluvial_model_response_caret(train, df, degree = 3, method = "pdp", resp_var = resp_var)
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3)
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3, method = "pdp")
    
    
  }
  test_xgb(form = disp ~ ., mode = "regression", resp_var = "disp",
           X = select(df, - disp), y = df$disp)
})

test_that("glmnet", {
  
  skip_on_cran()
  skip_if_not_installed("glmnet")
  skip_if_not_installed("vip")
  skip_if_not_installed("caret")
  skip_if_not_installed("parsnip")
  
  
  test_glm <- function(form, mode, resp_var, X, y, type = type, family = family){
    
    set.seed(1)
    # train models --------------------------------------------
    set.seed(1)
    X <- X %>%
      as.matrix()
    
    m <- glmnet::cv.glmnet(X, y, family = family, nfolds = 3)
    
    if(mode == "regression"){
      .f <- parsnip::linear_reg
    } else {
      .f <- parsnip::logistic_reg
    }
    
    m_parsnip <- .f(penalty = 0.5) %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::fit(form, df)
    
    set.seed(1)
    suppressWarnings({
      train <- caret::train(form, df, method = 'glmnet',
                            trControl = caret::trainControl(method = 'none'))
    })
    # imp -----------------------------------------------------
    imp <- vip::vi_model(m)
    imp_parsnip <- vip::vi_model(m_parsnip)
    imp_caret <- caret::varImp(train)
    
    imp <- tidy_imp(imp, df, resp_var = resp_var)
    imp_parsnip <- tidy_imp(imp_parsnip, df, resp_var = resp_var)
    imp_caret <- tidy_imp(imp_caret, df, resp_var = resp_var)
    
    expect_true(all(nrow(c(imp, imp_parsnip, imp_caret)) == nrow(df) - 1))
    
    # plots --------------------------------------------------
    dspace <- get_data_space(df, imp, degree = 3, bins = 5)
    pred <- predict(m, newx = as.matrix(dspace), type = type, s = "lambda.1se")
    
    .f_predict <- function(m, newdata){
      predict(m, newx = as.matrix(newdata), type = type, s = "lambda.1se")
    }
    pred_pdp <- get_pdp_predictions(df, imp, m, degree = 3, bins = 5,
                                    .f_predict = .f_predict)
    
    expect_warning(alluvial_model_response(pred, dspace, imp, degree = 3))
    
    p <- alluvial_model_response(pred, dspace, imp, degree = 3,
                                 params_bin_numeric_pred = list(bins=3),
                                 bin_labels = c("H", "M", "L"))
    
    p <- alluvial_model_response(pred_pdp, dspace, imp, degree = 3,
                                 params_bin_numeric_pred = list(bins=3),
                                 bin_labels = c("H", "M", "L"), method = "pdp")
    
    p <- alluvial_model_response_caret(train, df, degree = 3)
    p <- alluvial_model_response_caret(train, df, degree = 3, method = "pdp")
    
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3,
                                         params_bin_numeric_pred = list(bins=3),
                                         bin_labels = c("H", "M", "L"))
    
    p <- alluvial_model_response_parsnip(m_parsnip, df, degree = 3,
                                         params_bin_numeric_pred = list(bins=3),
                                         bin_labels = c("H", "M", "L"), method = "pdp")
    
    
  }
  
  df = select(mtcars2, -ids) %>%
    mutate_if(is.factor, manip_factor_2_numeric)
  
  test_glm(form = disp ~ ., mode = "regression", resp_var = "disp",
           X = select(df, - disp), y = df$disp, family = "gaussian", type = "link")
  
})
