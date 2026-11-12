#' @title Combine data from pool and treated groups
#' @description If your data is stored in separate files, you can use this function to merge them.
#' @param POOL_data Data frame with POOL data, where you want to sample from.
#' @param TG_data Data frame with TG (treated groups) data, all treated groups together!
#' @param indicator_name Name of the variable that is created for further use in the package, Default: 'treated'
#' @return Data frame with all covariates that were present in both files and with new indicator factor POOL vs TG
#' @examples
#'
#' pool_data <- data.frame(
#'   cov1   = rnorm(100, 11, 2),
#'   cov2   = rnorm(100, 11, 2),
#'   cov3   = rnorm(100, 11, 2),
#'   sex    = rbinom(100, 1, 0.5))
#'
#' tg_data <- data.frame(
#'   cov2   = rnorm(20, 12, 1),
#'   cov3   = rnorm(20, 12, 1),
#'   cov4   = rnorm(20, 12, 1),
#'   sex    = rbinom(20, 1, 0.5))
#'
#'  dx <- combine_data(pool_data, tg_data)
#'  str(dx)
#'
#' @rdname combine_data
#' @export



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
combine_data <- function(POOL_data, TG_data, indicator_name='treated'){
  POOL_data <- as.data.frame(POOL_data)
  TG_data   <- as.data.frame(TG_data)

  if(nrow(TG_data)>nrow(POOL_data)) warning('Your TG data is larger than your POOL data; normally it is the other way around.
                                             Make sure you have entered them in the correct order.
                                            The first data frame should contain the POOL data, the second the TG data.')

  pool_names <- colnames(POOL_data)
  tg_names   <- colnames(TG_data)
  overlapp   <- pool_names[which(pool_names%in%tg_names)]
  POOL_data  <- POOL_data[, overlapp, drop = FALSE]
  TG_data    <- TG_data[, overlapp, drop = FALSE]

  if(ncol(POOL_data)==0 | ncol(TG_data)==0) stop('No overlapping covariates were found. Covariates must have the same name in both data frames.')

  POOL_data[[indicator_name]] <- "POOL"
  TG_data[[indicator_name]]   <- "TG"
  out <- rbind(POOL_data, TG_data)
  out[[indicator_name]] <- factor(out[[indicator_name]], levels = c('POOL', 'TG'))
  return(out)
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
