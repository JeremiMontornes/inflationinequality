#' Calculate product quantile weights
#' Note: quintiles are 5-quantiles
#' Columns are COICOP, year, country, series name, product quantile weights
#' @importFrom data.table :=
#' @export
calculate_product_quantile_weights <- function() {
    df_weights_FR <- load_weights("FR", 3)
    df_hbs_FR <- load_hbs("FR", 3)

    # Left join
    # This assumes that df_weights's coicop ids are 100% correct
    # Possible optimisation: update df_weights_FR by reference
    # df_product_quantile_weights_FR <- df_hbs_FR[df_weights_FR, on="coicop", allow.cartesian=TRUE]
    df_product_quantile_weights_FR <- merge(df_weights_FR, df_hbs_FR, by = "coicop", all.x = FALSE, allow.cartesian=TRUE)

    ### Equation (1)
    df_product_quantile_weights_FR[, weighted_consumption := weight * consumption]
    df_product_quantile_weights_FR[quantile != "TOT", weighted_consumption := weighted_consumption / consumption[quantile == "TOT"], by = .(year, coicop)]
    ###
    df_product_quantile_weights_FR <- df_product_quantile_weights_FR[quantile != "TOT", .(coicop, year, quantile, weighted_consumption)]
    return(df_product_quantile_weights_FR)
}

#' Normalise product quantile weights over a given year and quantile
#' This means a normalised weight summed over a given year and quantile sums to 100
#' @importFrom data.table :=
#' @export
normalise_product_quantile_weights <- function(df_product_quantile_weights_FR) {
    df_product_quantile_weights_FR[, weighted_consumption := weighted_consumption * 100 / sum(weighted_consumption), by = .(year, quantile)]
    return(df_product_quantile_weights_FR)
}
