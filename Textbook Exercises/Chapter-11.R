# Chapter 11 — Statistical Learning
library(sf)
library(raster)
library(mlr)
library(dplyr)
library(parallelMap)


# 11.2 Case study: Landslide susceptibility -------------------------------
data("landslides", package = "RSAGA")

# Sampling an equal number of non-landslide points for a valid analysis
        # select non-landslide points
        non_pts = filter(landslides, lslpts == FALSE)
        # select landslide points
        lsl_pts = filter(landslides, lslpts == TRUE)
        # randomly select 175 non-landslide points
        set.seed(11042018)
        non_pts_sub = sample_n(non_pts, size = nrow(lsl_pts))
        # create smaller landslide dataset (lsl)
        lsl = bind_rows(non_pts_sub, lsl_pts)

# Converting DEM (matrix) into a raster
        dem = raster(
                dem$data, 
                crs = dem$header$proj4string,
                xmn = dem$header$xllcorner, 
                xmx = dem$header$xllcorner + dem$header$ncols * dem$header$cellsize,
                ymn = dem$header$yllcorner,
                ymx = dem$header$yllcorner + dem$header$nrows * dem$header$cellsize
        )

# Loading attribute data from spDataLarge
        # attach landslide points with terrain attributes
        data("lsl", package = "spDataLarge")
        # attach terrain attribute raster stack
        data("ta", package = "spDataLarge")


# 11.3 Conventional Modeling Approach in R --------------------------------

# This commend will run a linear fit on a dataset
fit = glm(lslpts ~ slope + cplan + cprof + elev + log10_carea,
        family = binomial(),
        data = lsl)

# Using data from the fit, R can take the rest of the points and predict landslide occurence
        pred_glm = predict(object = fit, type = "response")
        head(pred_glm)
        
        # making the prediction BUT, assumes no autocorrelation 
        pred = raster::predict(ta, model = fit, type = "response")
        
        # Testing the prediction's accuracy with AUROC (.5 is rly bad, 1.0 is good)
        pROC::auc(pROC::roc(lsl$lslpts, fitted(fit)))


# 11.4 Spatial CV with mlr -------------------------

# Generalized Linear Model
        # coordinates needed for the spatial partitioning
        coords = lsl[, c("x", "y")]
        # select response and predictors to use in the modeling
        data = dplyr::select(lsl, -x, -y)
        # create task
        task = makeClassifTask(data = data, target = "lslpts",
                               positive = "TRUE", coordinates = coords)
        # select the learner
        lrn = makeLearner(cl = "classif.binomial",
                          link = "logit",
                          predict.type = "prob",
                          fix.factors.prediction = TRUE)
        # set the performance
        perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)

        # running the sampling
        set.seed(012348)
        sp_cv = mlr::resample(learner = lrn, task = task,
                              resampling = perf_level, 
                              measures = mlr::auc)
        # see summary statistics
        summary(sp_cv$measures.test$auc)
        mean(sp_cv$measures.test$auc)        

# Spatial tuning of machine-learning hyperparameters
        # select the learner
        lrn_ksvm = makeLearner("classif.ksvm",
                               predict.type = "prob",
                               kernel = "rbfdot")
        # performance estimation level
        perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)        

        # five spatially disjoint partitions
        tune_level = makeResampleDesc("SpCV", iters = 5)
        # use 50 randomly selected hyperparameters
        ctrl = makeTuneControlRandom(maxit = 50)
        # define the outer limits of the randomly selected hyperparameters
        ps = makeParamSet(
                makeNumericParam("C", lower = -12, upper = 15, trafo = function(x) 2^x),
                makeNumericParam("sigma", lower = -15, upper = 6, trafo = function(x) 2^x)
        )        
        # modify the learner
        wrapped_lrn_ksvm = makeTuneWrapper(learner = lrn_ksvm, 
                                           resampling = tune_level,
                                           par.set = ps,
                                           control = ctrl, 
                                           show.info = TRUE,
                                           measures = mlr::auc)
        
        # configuring the model to continue processing after errors
        configureMlr(on.learner.error = "warn", on.error.dump = TRUE)

        # setting up parallelization for better processing speed
        library(parallelMap)
        if (Sys.info()["sysname"] %in% c("Linux", "Darwin")) {
                parallelStart(mode = "multicore", 
                              # parallelize the hyperparameter tuning level
                              level = "mlr.tuneParams", 
                              # just use half of the available cores
                              cpus = round(parallel::detectCores() / 2),
                              mc.set.seed = TRUE)
        }
        
        if (Sys.info()["sysname"] == "Windows") {
                parallelStartSocket(level = "mlr.tuneParams",
                                    cpus =  round(parallel::detectCores() / 2))
        }
        
        # running the machine learning, takes a VERY long time
        set.seed(12345)
        result = mlr::resample(learner = wrapped_lrn_ksvm, 
                               task = task,
                               resampling = perf_level,
                               extract = getTuneResult,
                               measures = mlr::auc)
        # stop parallelization
        parallelStop()
        # save your result, e.g.:
        # saveRDS(result, "svm_sp_sp_rbf_50it.rds")
        
        