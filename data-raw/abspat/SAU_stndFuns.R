


#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
vc_check_lme4 <- function(indat, response) {

  if (response != "docktime") {
    indat$numdivers <- 1
  }
  
  indat <- indat %>% within({
    fishyear <- factor(fishyear)
    fishmonth <- factor(fishmonth)
    subblockno <- factor(subblockno)
    diver_id <- factor(diver_id)
    numdivers <- factor(numdivers)
    SAU <- factor(SAU)
  })
  
  ## Build predictor terms
  
  if (response == "logrtime") {
    fm_0 <- "log(catch_est) ~ log(logrtime) "
  } else if (response == "lograrea") {
    fm_0 <- "log(catch_est) ~ log(lograrea) "
  } else if (response== "docktime") {
    fm_0 <- "log(catch_est) ~ log(docktime) "
  } else {
    print("Did you forget the response variable?")
    break
  }
  
  
  if (max(facttonum(indat$numdivers)) > 1) {
    fm_1 <-  "fishyear + numdivers"
  } else {
    fm_1 <-  "fishyear"
  }
  
  fm_2  <-  "(1 | subblockno) +  (1 | subblockno:fishyear)"
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  
  if (length(unique(indat$subblockno)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  
  
  sau_lmer <- lmer(
    mymod,
    control = lmerControl(optimizer = "bobyqa"),
    data = indat
  )
  return(sau_lmer)
}

## end vc_check_fun
##---------------------------------------------------------------------------##



#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
#' # indat = b11
vc_check_TMB <- function(indat, response) {
  
  if (response != "docktime") {
    indat$numdivers <- 1
  }

  
    indat <- indat %>% within({
    fishyear <- factor(fishyear)
    fishmonth <- factor(fishmonth)
    subblockno <- factor(subblockno)
    diver_id <- factor(diver_id)
    numdivers <- factor(numdivers)
    SAU <- factor(SAU)
  })
  
  ## Build predictor terms

  if (response == "logrtime") {
    fm_0 <- "log(catch_est) ~ log(logrtime) "
  } else if (response == "lograrea") {
    fm_0 <- "log(catch_est) ~ log(lograrea) "
  } else if (response== "docktime") {
    fm_0 <- "log(catch_est) ~ log(docktime) "
  } else {
    print("Did you forget the response variable?")
    break
  }
  
  
  if (max(facttonum(indat$numdivers)) > 1) {
    fm_1 <-  "fishyear + numdivers"
  } else {
    fm_1 <-  "fishyear"
  }
  
  fm_2  <-  "(1 | subblockno) +  (1 | subblockno:fishyear)"
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  
  if (length(unique(indat$subblockno)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  
  sau_lmer <- glmmTMB(
    mymod,
    data = indat
  )
  return(sau_lmer)
}

## end vc_check_TMB
##---------------------------------------------------------------------------##


#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
mod_vc_lme4 <- function(indat) {
  names(indat)
  ipc <-  fixef(indat)[2]
  diags <-
    data.frame(
      ipc = ipc
    )
  return(diags)
}

## end mod_vc_fun
##---------------------------------------------------------------------------##


#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
#' indat <- mod_TMB
mod_vc_TMB <- function(indat) {
  names(indat)
  vc <- fixef(indat)
  diags <-
    data.frame(
      ipc = vc$cond[2]
    )
  return(diags)
}

## end mod_vc_fun
##---------------------------------------------------------------------------##







#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
stnd_fun <- function(indat) {
  indat <- indat %>% within({
    fishyear <- factor(droplevels(fishyear))
    fishmonth <- factor(droplevels(fishmonth))
    subblockno <- factor(droplevels(subblockno))
    diver_id <- factor(droplevels(diver_id))
    SAU <- factor(droplevels(SAU))
  })
  
  ## Build predictor terms
  if ("docktime" %in% names(indat)) {
    fm_0 <- "log(catch_est) ~ offset(log(docktime)) + numdivers"
  } else {
    fm_0 <- "log(catch_est) ~ offset(log(logrtime)) + numdivers"
  }
  
  fm_1 <-  "fishyear "
  fm_2  <-  "(1 | subblockno) +  (1 | subblockno:fishyear)"
  #fm_2  <-  "(1 | diver_id) + (1 | diver_id:fishyear) "
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  #fm_3  <-  "(1 | fishmonth)"
  
  if (length(unique(indat$subblockno)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  ## ignore subblocks
  # mymod <- formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  
  sau_lmer <- lmer(
    mymod,
    control = lmerControl(optimizer = "bobyqa"),
    data = indat
  )
  return(sau_lmer)
}

## end stnd_fun
##---------------------------------------------------------------------------##


#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
mod_diag_fun <- function(indat) {
  #names(indat)
  R2 <- r2_nakagawa(indat)
  diags <-
    data.frame(
      R2m = R2[1],
      R2c = R2[2]
    )
  return(diags)
}

## end mod_diag_fun
## ##---------------------------------------------------------------------------## 





#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
#' # indat = mod_out_CREF[[1]]
mod_icc_fun <- function(indat) {
  #names(indat)
  icc_val <- icc(indat, by_group = TRUE)
  return(icc_val)
}

## end mod_icc_fun
## ##---------------------------------------------------------------------------## 



#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
#' # indat = mod1_out[[1]]
raneff_prop <- function(indat) {
  vcp <- extract_vc(indat, ci_level = .95)
  return(vcp)
}



#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
#' # 
stnd_fun_lme4 <- function(indat, response) {
  
  # indat = dat; response = "logrtime"
  
  if (response != "docktime") {
    indat$numdivers <- 1
  }
  
  indat <- indat %>% within({
    fishyear <- factor(fishyear)
    fishmonth <- factor(fishmonth)
    subblockno <- factor(subblockno)
    diver_id <- factor(diver_id)
    numdivers <- factor(numdivers)
    SAU <- factor(SAU)
  })
  
  ## Build predictor terms
  if (response == "logrtime") {
    fm_0 <- "log(catch_est) ~ offset(log(logrtime)) "
  } else if (response == "lograrea") {
    fm_0 <- "log(catch_est) ~ offset(log(lograrea)) "
  } else if (response== "docktime") {
    fm_0 <- "log(catch_est) ~ offset(log(docktime)) "
  } else {
    print("Did you forget the reponse variable?")
    break
  }
  
  
  if (max(facttonum(indat$numdivers)) > 1) {
    fm_1 <-  "fishyear + numdivers"
  } else {
    fm_1 <-  "fishyear"
  }
  
  fm_2  <-  "(1 | subblockno) +  (1 | subblockno:fishyear)"
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  
  if (length(unique(indat$subblockno)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  sau_lmer <- lmer(
    mymod,
    na.action = na.exclude,
    data = indat
  )
  return(sau_lmer)
}

## end stnd_fun_lme4
## ##---------------------------------------------------------------------------## 



#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
#' # 
stnd_fun_tmb <- function(indat, response) {
  
  # indat = dat; response = "logrtime"
  
  if (response != "docktime") {
    indat$numdivers <- 1
  }
  
  indat <- indat %>% within({
    fishyear <- factor(fishyear)
    fishmonth <- factor(fishmonth)
    subblockno <- factor(subblockno)
    diver_id <- factor(diver_id)
    numdivers <- factor(numdivers)
    SAU <- factor(SAU)
  })
  
  ## Build predictor terms
  if (response == "logrtime") {
    fm_0 <- "log(catch_est) ~ offset(log(logrtime)) "
  } else if (response == "lograrea") {
    fm_0 <- "log(catch_est) ~ offset(log(lograrea)) "
  } else if (response== "docktime") {
    fm_0 <- "log(catch_est) ~ offset(log(docktime)) "
  } else {
    print("Did you forget the reponse variable?")
    break
  }
  

  if (max(facttonum(indat$numdivers)) > 1) {
    fm_1 <-  "fishyear + numdivers"
  } else {
    fm_1 <-  "fishyear"
  }

  fm_2  <-  "(1 | subblockno) +  (1 | subblockno:fishyear)"
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  
  if (length(unique(indat$subblockno)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  sau_lmer <- glmmTMB(
    mymod,
    na.action = na.exclude,
    data = indat
  )
  return(sau_lmer)
}

## end stnd_fun_tmb
## ##---------------------------------------------------------------------------## 



#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
#' # 
stnd_lme4_clust <- function(indat, response) {
  
  # indat = dat; response = "logrtime"
  
  if (response != "docktime") {
    indat$numdivers <- 1
  }
  
  indat <- indat %>% within({
    fishyear <- factor(fishyear)
    fishmonth <- factor(fishmonth)
    subblockno <- factor(subblockno)
    diver_id <- factor(diver_id)
    numdivers <- factor(numdivers)
    SAU <- factor(SAU)
  })
  
  ## Build predictor terms
  if (response == "logrtime") {
    fm_0 <- "log(catch_est) ~ offset(log(logrtime)) "
  } else if (response == "lograrea") {
    fm_0 <- "log(catch_est) ~ offset(log(lograrea)) "
  } else if (response== "docktime") {
    fm_0 <- "log(catch_est) ~ offset(log(docktime)) "
  } else {
    print("Did you forget the reponse variable?")
    break
  }
  
  
  if (max(facttonum(indat$numdivers)) > 1) {
    fm_1 <-  "fishyear + numdivers"
  } else {
    fm_1 <-  "fishyear"
  }
  
  fm_2  <-  "(1 | cluster_id) +  (1 | cluster_id:fishyear)"
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  
  if (length(unique(indat$cluster_id)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  sau_lmer <- lmer(
    mymod,
    na.action = na.exclude,
    data = indat
  )
  return(sau_lmer)
}
## end stnd_lme4_clust
## ##---------------------------------------------------------------------------## 


#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
#' # 
stnd_tmb_clust <- function(indat, response) {
  # indat = dat_list$SAU11;  = "logrtime"
  
  if (response != "docktime") {
    indat$numdivers <- 1
  }
  
  indat <- indat %>% within({
    fishyear <- factor(fishyear)
    fishmonth <- factor(fishmonth)
    subblockno <- factor(subblockno)
    diver_id <- factor(diver_id)
    numdivers <- factor(numdivers)
    SAU <- factor(SAU)
  })
  
  ## Build predictor terms
  if (response == "logrtime") {
    fm_0 <- "log(catch_est) ~ offset(log(logrtime)) "
  } else if (response == "lograrea") {
    fm_0 <- "log(catch_est) ~ offset(log(lograrea)) "
  } else if (response == "docktime") {
    fm_0 <- "log(catch_est) ~ offset(log(docktime)) "
  } else {
    print("Did you forget the reponse variable?")
    break
  }
  
  
  if (max(facttonum(indat$numdivers)) > 1) {
    fm_1 <-  "fishyear + numdivers"
  } else {
    fm_1 <-  "fishyear"
  }
  
  fm_2  <-  "(1 | cluster_id) +  (1 | cluster_id:fishyear)"
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"

  
  if (length(unique(indat$cluster_id)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  sau_lmer <- glmmTMB(
    mymod,
    na.action = na.exclude,
    data = indat
  )
  return(sau_lmer)
}

## end stnd_tmb_clust
## ##---------------------------------------------------------------------------## 



#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
stnd_fun_gamma <- function(indat) {
  indat <- indat %>% within({
    fishyear <- factor(droplevels(fishyear))
    fishmonth <- factor(droplevels(fishmonth))
    subblockno <- factor(droplevels(subblockno))
    diver_id <- factor(droplevels(diver_id))
    SAU <- factor(droplevels(SAU))
  })
  
  ## Build predictor terms
  if ("docktime" %in% names(indat)) {
    fm_0 <- "catch_est ~ offset(log(docktime)) + numdivers"
  } else {
    fm_0 <- "catch_est ~ offset(log(logrtime)) + numdivers"
  }
  
  fm_1 <-  "fishyear "
  fm_2  <-  "(1 | subblockno) +  (1 | subblockno:fishyear)"
  #fm_2  <-  "(1 | diver_id) + (1 | diver_id:fishyear) "
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  #fm_3  <-  "(1 | fishmonth)"
  
  if (length(unique(indat$subblockno)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  
  sau_glmer <- glmer(
    mymod,
    family = Gamma(link = log),
    control = glmerControl(optimizer = "bobyqa"),
    data = indat
  )
  return(sau_glmer)
}
## end stnd_fun_gamma
## ##---------------------------------------------------------------------------## 





#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
hyperst_fun_tmb <- function(indat, response) {
  
  if (response != "docktime") {
    indat$numdivers <- 1
  }
  
  
  indat <- indat %>% within({
    fishyear <- factor(fishyear)
    fishmonth <- factor(fishmonth)
    subblockno <- factor(subblockno)
    diver_id <- factor(diver_id)
    numdivers <- factor(numdivers)
    SAU <- factor(SAU)
  })
  
  
    
  ## Build predictor terms
  if (response == "logrtime") {
    fm_0 <- "log(logrtime) ~ offset(log(catch_est)) "
  } else if (response == "lograrea") {
    fm_0 <- "log(lograrea) ~ offset(log(catch_est)) "
  } else if (response== "docktime") {
    fm_0 <- "log(docktime) ~ offset(log(catch_est)) "
  } else {
    print("Did you forget the reponse variable?")
    break
  }
  

  if (max(facttonum(indat$numdivers)) > 1) {
    fm_1 <-  "fishyear + numdivers"
  } else {
    fm_1 <-  "fishyear"
  }

  fm_2  <-  "(1 | subblockno) +  (1 | subblockno:fishyear)"
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  
  if (length(unique(indat$subblockno)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  

    sau_lmer_hs <- glmmTMB(
    mymod,
    data = indat
  )
  return(sau_lmer_hs)
}

## end hyperst_fun_tmb
## ##---------------------------------------------------------------------------## 



#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
hyperst_fun_lme4 <- function(indat, response) {
  
  if (response != "docktime") {
    indat$numdivers <- 1
  }
  
  
  indat <- indat %>% within({
    fishyear <- factor(fishyear)
    fishmonth <- factor(fishmonth)
    subblockno <- factor(subblockno)
    diver_id <- factor(diver_id)
    numdivers <- factor(numdivers)
    SAU <- factor(SAU)
  })
  
  
  
  ## Build predictor terms
  if (response == "logrtime") {
    fm_0 <- "log(logrtime) ~ offset(log(catch_est)) "
  } else if (response == "lograrea") {
    fm_0 <- "log(lograrea) ~ offset(log(catch_est)) "
  } else if (response== "docktime") {
    fm_0 <- "log(docktime) ~ offset(log(catch_est)) "
  } else {
    print("Did you forget the reponse variable?")
    break
  }
  
  
  if (max(facttonum(indat$numdivers)) > 1) {
    fm_1 <-  "fishyear + numdivers"
  } else {
    fm_1 <-  "fishyear"
  }
  
  fm_2  <-  "(1 | subblockno) +  (1 | subblockno:fishyear)"
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  
  if (length(unique(indat$subblockno)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  
  sau_lmer_hs <- lmer(
    mymod,
    na.action = na.exclude,
    data = indat
  )
  return(sau_lmer_hs)
}

## end hyperst_fun_lme4
## ##---------------------------------------------------------------------------## 



#' Title
#'
#' @param indat 
#'
#' @return
#' @export
#'
#' @examples
stnd_fun_tmb_spatial <- function(indat, response) {
  if (response != "docktime") {
    indat$numdivers <- 1
  }
  
  indat <- indat %>% within({
    fishyear <- factor(fishyear)
    fishmonth <- factor(fishmonth)
    subblockno <- factor(subblockno)
    diver_id <- factor(diver_id)
    numdivers <- factor(numdivers)
    SAU <- factor(SAU)
  })
  
  ## Build predictor terms
  if (response == "logrtime") {
    fm_0 <- "log(catch_est) ~ offset(log(logrtime)) "
  } else if (response == "lograrea") {
    fm_0 <- "log(catch_est) ~ offset(log(lograrea)) "
  } else if (response== "docktime") {
    fm_0 <- "log(catch_est) ~ offset(log(docktime)) "
  } else {
    print("Did you forget the reponse variable?")
    break
  }
  
  
  if (max(facttonum(indat$numdivers)) > 1) {
    fm_1 <-  "fishyear + numdivers"
  } else {
    fm_1 <-  "fishyear"
  }
  
  fm_2  <-  "(1 | subblockno) +  (1 | subblockno:fishyear)"
  #fm_2  <-  "(1 | diver_id) + (1 | diver_id:fishyear) "
  fm_3  <-  "(1 | diver_id) + (1 | fishmonth)"
  #fm_3  <-  "(1 | fishmonth)"
  fm_4 <- 
  
  if (length(unique(indat$subblockno)) > 2) {
    mymod <-
      formula(paste(fm_0, fm_1, fm_2, fm_3, sep = " + "))
  } else {
    mymod <- formula(paste(fm_0, fm_1, fm_3, sep = " + "))
  }
  
  
  sau_lmer <- glmmTMB(
    mymod,
    data = indat
  )
  return(sau_lmer)
}

## end stnd_fun_tmb_spatial
## ##---------------------------------------------------------------------------## 




# mod5 <- mod1_out$SAU5
# 
# dat <- mod5$frame


#' Title predict_TMB_fun
#'
#' @param indat 
#' @param  
#'
#' @return
#' @export
#'
#' @examples
#' # inmod = mod 
predict_TMB_fun <- function(inmod, response) {
 

  #  pdat <- pdat %>% within({
  #   fishyear <- factor(fishyear)
  #   fishmonth <- factor(fishmonth)
  #   subblockno <- factor(subblockno)
  #   numdivers <- factor(numdivers)
  #   diver_id <- factor(diver_id)
  # })
  
  if ("subblockno" %in% all.vars(formula(inmod, random.only = TRUE))) {
    ## Prediction for SAU with a withinSAU term
   #print("TRUE condition")
    framedat <- inmod$frame
    pdat <- framedat |>  
      #ungroup() |>
      dplyr::select(fishyear, subblockno) |>
      unique() |> 
      arrange(fishyear, subblockno) |>
      within({
        numdivers <- factor(1, levels = c(1,2))
        diver_id <- framedat$diver_id[1]
        fishmonth <- framedat$fishmonth[1]
        year <- as.numeric(as.character(fishyear))
      }) |> untibble()  
    
    if (response == "docktime") {
      pdat$docktime <- 60
    } else if (response == "logrtime") {
      pdat$logrtime <- 60
    } 
    
     if (!"numdivers" %in% all.vars(formula(inmod, fixed.only = TRUE))) {
      pdat$numdivers <- NULL
    }
      
    
    
  vc <- VarCorr(inmod)$cond
  s2 <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                            .[c("diver_id", "fishmonth", "subblockno", 
                                "subblockno:fishyear")] %>% 
                            unlist() %>% 
                            sum())
  
  s2c <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                             .[c("diver_id", "fishmonth")] %>% 
                             unlist() %>% 
                             sum())
  
  
  pdat$fmod_BC <- (predict(inmod , newdata = pdat, re.form = ~0) + s2/2) |> exp()
  
  
  ### start by hand
  x <- ranef(inmod)$cond$subblockno |>
    as.data.frame() |>
    setNames("sub_") |>
    rownames_to_column("subblockno")
  y <- ranef(inmod )$cond$`subblockno:fishyear` |>
    setNames("sub_fish_") |>
    rownames_to_column("z") |>
    separate_wider_delim(z, ":", names = c("subblockno", "fishyear"))
  tmp <- pdat |> left_join(x) |> left_join(y)
  ## end by hand
  
  pdat$pmod_BC <- with(tmp, exp(log(fmod_BC) + sub_ + sub_fish_ + s2c/2))
  
  } else {
  
    #print("FALSE condition")
    ## Prediction for SAU with no withinSAU term
    framedat <- inmod$frame
    pdat <- framedat |>
      #ungroup() |>
      dplyr::select(fishyear) |>
      unique() |>
      arrange(fishyear) |>
      within({
        numdivers <- factor(1, levels = c(1, 2))
        diver_id <- framedat$diver_id[1]
        fishmonth <- framedat$fishmonth[1]
        year <- as.numeric(as.character(fishyear))
      }) |> untibble() 
    
    if (response == "docktime") {
      pdat$docktime <- 60
    } else {
      pdat$logrtime <- 60
    }
    
    
    if (!"numdivers" %in% all.vars(formula(inmod, fixed.only = TRUE))) {
      pdat$numdivers <- NULL
    }
    
    vc <- VarCorr(inmod)$cond
    s2 <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                                            .[c("diver_id", "fishmonth")] %>% 
                                            unlist() %>% 
                                            sum())
  pdat$fmod_BC <-
      (predict(inmod , newdata = pdat, re.form = ~
                 0) + s2 / 2) |> exp()
    
  }
  
  return(pdat)
} # end predict_TMB_fun
## end mod_diag_fun
## ##---------------------------------------------------------------------------## 



#' Title predict_lme4_fun
#'
#' @param indat 
#' @param  
#'
#' @return
#' @export
#'
#' @examples
#' # inmod = mod1_out[[1]]; response = "logrtime"
#' # inmod = mod1_out[[1]]; response = "docktime"  
predict_lme4_fun <- function(inmod, response) {
  

  if ("subblockno" %in% all.vars(formula(inmod, random.only = TRUE))) {
    ## Prediction for SAU with a withinSAU term
    #print("TRUE condition")
    framedat <- inmod@frame
    pdat <- framedat |>  
      #ungroup() |>
      dplyr::select(fishyear, subblockno) |>
      unique() |> 
      arrange(fishyear, subblockno) |>
      within({
        numdivers <- factor(1, levels = c(1,2))
        diver_id <- framedat$diver_id[1]
        fishmonth <- framedat$fishmonth[1]
        year <- as.numeric(as.character(fishyear))
      }) |> untibble()  
    
    
    if (response == "docktime") {
      pdat$docktime <- 60
    } else if (response == "logrtime") {
      pdat$logrtime <- 60
    } 
    
    
    if (!"numdivers" %in% all.vars(formula(inmod, fixed.only = TRUE))) {
      pdat$numdivers <- NULL
    }
    
    vc <- VarCorr(inmod)$cond
    s2 <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                               .[c("diver_id", "fishmonth", "subblockno", 
                                   "subblockno:fishyear")] %>% 
                               unlist() %>% 
                               sum())
    
    s2c <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                                .[c("diver_id", "fishmonth")] %>% 
                                unlist() %>% 
                                sum())

    pdat$fmod_BC <- exp(predict(inmod, pdat, re.form = ~0) + s2/2)
    pdat$pmod_BC <- exp(predict(inmod, pdat, re.form = ~(1|subblockno) + 
                                  (1|subblockno:fishyear)) + s2c/2)
  } else {
    
    #print("FALSE condition")
    ## Prediction for SAU with no withinSAU term
    framedat <- inmod@frame
    pdat <- framedat |>
      #ungroup() |>
      dplyr::select(fishyear) |>
      unique() |>
      arrange(fishyear) |>
      within({
        numdivers <- factor(1, levels = c(1, 2))
        diver_id <- framedat$diver_id[1]
        fishmonth <- framedat$fishmonth[1]
        year <- as.numeric(as.character(fishyear))
      }) |> untibble() 
    
    
    if (response == "docktime") {
      pdat$docktime <- 60
    } else {
      pdat$logrtime <- 60
    }
    
    
    
    
    if (!"numdivers" %in% all.vars(formula(inmod, fixed.only = TRUE))) {
      pdat$numdivers <- NULL
    }
    
    vc <- VarCorr(inmod)$cond
    s2 <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                               .[c("diver_id", "fishmonth")] %>% 
                               unlist() %>% 
                               sum())
    
    pdat$fmod_BC <- exp(predict(inmod, pdat, re.form = ~ 0) + s2/2)
  }
  
  return(pdat)
} # end predict_lme4_fun
## end mod_diag_fun
## ##---------------------------------------------------------------------------## 





#' Title predict_TMB_hyp
#'
#' @param indat 
#' @param  
#'
#' @return
#' @export
#'
#' @examples
#' # inmod = mod 
predict_TMB_hyp <- function(inmod, response) {
  
  
  #  pdat <- pdat %>% within({
  #   fishyear <- factor(fishyear)
  #   fishmonth <- factor(fishmonth)
  #   subblockno <- factor(subblockno)
  #   numdivers <- factor(numdivers)
  #   diver_id <- factor(diver_id)
  # })
  
  if ("subblockno" %in% all.vars(formula(inmod, random.only = TRUE))) {
    ## Prediction for SAU with a withinSAU term
    #print("TRUE condition")
    framedat <- inmod$frame
    pdat <- framedat |>  
      #ungroup() |>
      dplyr::select(fishyear, subblockno) |>
      unique() |> 
      arrange(fishyear, subblockno) |>
      within({
        numdivers <- factor(1, levels = c(1,2))
        diver_id <- framedat$diver_id[1]
        fishmonth <- framedat$fishmonth[1]
        year <- as.numeric(as.character(fishyear))
      }) |> untibble()  
    
    if (response == "logrtime") {
      pdat$logrtime <- 60
    } else if (response == "lograrea") {
      pdat$catch_est <- 100
    }
    
    catch_est <- 100
    
    
    if (!"numdivers" %in% all.vars(formula(inmod, fixed.only = TRUE))) {
      pdat$numdivers <- NULL
    }
    
    
    
    vc <- VarCorr(inmod)$cond
    s2 <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                               .[c("diver_id", "fishmonth", "subblockno", 
                                   "subblockno:fishyear")] %>% 
                               unlist() %>% 
                               sum())
    
    s2c <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                                .[c("diver_id", "fishmonth")] %>% 
                                unlist() %>% 
                                sum())
    
    
    pdat$fmod_BC <- (predict(inmod , newdata = pdat, re.form = ~0) + s2/2) |> exp()
    
    
    ### start by hand
    x <- ranef(inmod)$cond$subblockno |>
      as.data.frame() |>
      setNames("sub_") |>
      rownames_to_column("subblockno")
    y <- ranef(inmod )$cond$`subblockno:fishyear` |>
      setNames("sub_fish_") |>
      rownames_to_column("z") |>
      separate_wider_delim(z, ":", names = c("subblockno", "fishyear"))
    tmp <- pdat |> left_join(x) |> left_join(y)
    ## end by hand
    
    pdat$pmod_BC <- with(tmp, exp(log(fmod_BC) + sub_ + sub_fish_ + s2c/2))
    
  } else {
    
    #print("FALSE condition")
    ## Prediction for SAU with no withinSAU term
    framedat <- inmod$frame
    pdat <- framedat |>
      #ungroup() |>
      dplyr::select(fishyear) |>
      unique() |>
      arrange(fishyear) |>
      within({
        numdivers <- factor(1, levels = c(1, 2))
        diver_id <- framedat$diver_id[1]
        fishmonth <- framedat$fishmonth[1]
        year <- as.numeric(as.character(fishyear))
      }) |> untibble() 
    
    if (response == "logrtime") {
      pdat$logrtime <- 60
    } else if (response == "lograrea") {
      pdat$catch_est <- 100
    }

    if (!"numdivers" %in% all.vars(formula(inmod, fixed.only = TRUE))) {
      pdat$numdivers <- NULL
    }
    
    vc <- VarCorr(inmod)$cond
    s2 <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                               .[c("diver_id", "fishmonth")] %>% 
                               unlist() %>% 
                               sum())
    pdat$fmod_BC <-
      (predict(inmod , newdata = pdat, re.form = ~
                 0) + s2 / 2) |> exp()
    
  }
  
  return(pdat)
} # end predict_TMB_hyp
##---------------------------------------------------------------------------## 


#' Title predict_lme4_hyp
#'
#' @param indat 
#' @param  
#'
#' @return
#' @export
#'
#' @examples
#' # inmod = mod1_hy_time[[1]]; response = "logrtime" 
predict_lme4_hyp <- function(inmod, response) {
  
  
  if ("subblockno" %in% all.vars(formula(inmod, random.only = TRUE))) {
    ## Prediction for SAU with a withinSAU term
    #print("TRUE condition")
    framedat <- inmod@frame
    pdat <- framedat |>  
      #ungroup() |>
      dplyr::select(fishyear, subblockno) |>
      unique() |> 
      arrange(fishyear, subblockno) |>
      within({
        numdivers <- factor(1, levels = c(1,2))
        diver_id <- framedat$diver_id[1]
        fishmonth <- framedat$fishmonth[1]
        year <- as.numeric(as.character(fishyear))
        catch_est <- 100
      }) |> untibble()  
    
    
    # if (response == "logrtime") {
    #   pdat$logrtime <- 60
    # } else if (response == "lograrea") {
    #   pdat$catch_est <- 100
    # }
    
    
    
    if (!"numdivers" %in% all.vars(formula(inmod, fixed.only = TRUE))) {
      pdat$numdivers <- NULL
    }
    
    vc <- VarCorr(inmod)$cond
    s2 <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                               .[c("diver_id", "fishmonth", "subblockno", 
                                   "subblockno:fishyear")] %>% 
                               unlist() %>% 
                               sum())
    
    s2c <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                                .[c("diver_id", "fishmonth")] %>% 
                                unlist() %>% 
                                sum())
    
    pdat$fmod_BC <- exp(predict(inmod, pdat, re.form = ~0) + s2/2)
    pdat$pmod_BC <- exp(predict(inmod, pdat, re.form = ~(1|subblockno) + 
                                  (1|subblockno:fishyear)) + s2c/2)
    
  } else {
    
    #print("FALSE condition")
    ## Prediction for SAU with no withinSAU term
    framedat <- inmod@frame
    pdat <- framedat |>
      #ungroup() |>
      dplyr::select(fishyear) |>
      unique() |>
      arrange(fishyear) |>
      within({
        numdivers <- factor(1, levels = c(1, 2))
        diver_id <- framedat$diver_id[1]
        fishmonth <- framedat$fishmonth[1]
        year <- as.numeric(as.character(fishyear))
        catch_est <- 100
      }) |> untibble() 
    
    
    # if (response == "logrtime") {
    #   pdat$logrtime <- 60
    # } else if (response == "lograrea") {
    #   pdat$catch_est <- 100
    # }
    
    
    
    
    if (!"numdivers" %in% all.vars(formula(inmod, fixed.only = TRUE))) {
      pdat$numdivers <- NULL
    }
    
    vc <- VarCorr(inmod)$cond
    s2 <- sigma(inmod )^2 + (vc %>% unclass() %>% 
                               .[c("diver_id", "fishmonth")] %>% 
                               unlist() %>% 
                               sum())
    
    pdat$fmod_BC <- exp(predict(inmod, pdat, re.form = ~0) + s2/2)
  }
  
  return(pdat)
} # end predict_lme4_hyp

##---------------------------------------------------------------------------## 




