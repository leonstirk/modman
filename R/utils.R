#' Make a variable index from a data set
#' @param df data.frame
#' @param keep character
#' @param drop character
#' @return data.frame
#' @export
## mf <- df %>% getMF()
getMF <- function(df, keep = NA, drop = NA) {
  if(!is.na(drop) & is.vector(drop) & length(drop) > 0) {
    df <- df[!which(names(df) %in% drop)]
  }
  if(!is.na(keep) & is.vector(keep) & length(keep) > 0) {
    df <- df[keep]
  }
  mf <- as.data.frame(cbind(names(df), names(df)))
  names(mf) <- c('specification', 'vector')
  rownames(mf) <- seq(1:nrow(mf))
  return(mf)
}

#' Add variables not automatically created i.e. interactions and polynomials
#' @param mf data.frame
#' @param name character
#' @param poly integer
#' @return data.frame
#' @export
## mf <- mf %>% addVar('building_floor_area', 2)
## mf <- mf %>% addVar(c('flood', 'after_flood'))
addVar <- function(mf, name, poly = NA) {
  out <- rep(0, ncol(mf))
  names(out) <- names(mf)
  ## polynomial of degree poly
  if(!is.na(poly) & is.numeric(poly) & poly > 1 & is.vector(name) & length(name) == 1) {
    out['specification'] <- paste(c('poly(', name, ', ', poly, ', raw = TRUE)'), collapse = '')
    out['vector'] <- name
  }
  ## interaction case
  else if(is.vector(name) & length(name) > 1 & is.na(poly)) {
    out['specification'] <- paste(name, collapse = '*')
    out['vector'] <- paste(name, collapse = ':')
  }
  else {
    out['specification'] <- name
    out['vector'] <- name
  }
  return(rbind(mf,out))
}

#' Define a model
#' @param mf data.frame
#' @param model_name character
#' @param var_names character
#' @return data.frame
#' @export
## mf <- mf %>% setMod('resp', 'ln_sale_price')
## mf <- mf %>% setMod('genome', c('flood*after_flood', 'poly(building_floor_area, 2, raw = TRUE)'))
setMod <- function(mf, model_name, var_names) {
  mf[model_name] <- 0
  mf[which(mf[,'specification'] %in% var_names),c(model_name)] <- 1
  return(mf)
}

#' Recall a model specification
#' @param mf data.frame
#' @param resp character
#' @param model_names character
#' @return formula
#' @export
## mf %>% modelSpec('resp', 'genome')
modelSpec <- function(mf, resp, model_names) {
  stats::as.formula(
    paste(
      mf[which(mf[resp] == 1), c('specification')],
      '~',
      paste(unique(mf[which(apply(mf[model_names], 1, function(x) { sum(x) }) > 0), 'specification']), collapse = " + ")
    )
  )
}

#' Recall a vector of variable names
#' @param mf data.frame
#' @param model_names character
#' @return character
#' @export
## mf %>% modelVec('genome')
modelVec <- function(mf, model_names) {
  unique(mf[which(apply(mf[model_names], 1, function(x) { sum(x) }) > 0), 'vector'])
}
