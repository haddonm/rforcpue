## Load required packages 
knitr::opts_chunk$set(fig.align = "center",
                      # fig.show = "hold",
                      echo = FALSE,
                      message = FALSE, 
                      warning = FALSE,
                      comment = "",
                      out.height = "95%",
                      out.width = "95%",
                      ft.keepnext = T)
options(knitr.kable.NA = '')



suppressPackageStartupMessages({
  library(MASS, exclude = "select")
  library(MASSExtra)
  library(lme4)
  library(glmmTMB)
  library(tidyverse)
  library(purrr)
  library(knitr)
  library(sf)
  library(ggplot2)
  library(cowplot)
  library(ggExtra)
  library(patchwork)
  library(ggeffects)
  library(performance)
  library(insight)
  library(flextable)
  library(gt)
  library(codeutils)
  library(collapse)
})

untibble <- function(tibble) {
  data.frame(unclass(tibble), stringsAsFactors = FALSE, check.names = FALSE)
}


## sf CRS
GDA94 <- st_crs(28355)
WGS84 <- st_crs(4326)


# Bill's colours
redish    <- "#DF536B"
greenish  <- "#61D04F"
blueish   <- "#2297E6"
cyanish   <- "#28E2E5"
mauveish  <- "#CD0BBC"
yellowish <- "#F5C710"