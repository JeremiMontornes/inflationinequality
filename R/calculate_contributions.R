calculate_contributions <- function() {
    # Load data
    df_cpi_FR <- load_cpi("FR", 3)
    df_product_quantile_weights_FR <- calculate_product_quantile_weights()
    df_product_quantile_norm_weights_FR <- normalise_product_quantile_weights(df_product_quantile_weights_FR)

    # Remark that CPI data and normalised product weight data do not the same set of COICOP codes
    COICOPs <- intersect(unique(df_cpi_FR$coicop), unique(df_product_quantile_norm_weights_FR$coicop))
    df_cpi_FR <- df_cpi_FR[coicop%in%COICOPs,]
    df_product_quantile_norm_weights_FR <- df_product_quantile_norm_weights_FR[coicop%in%COICOPs,]

    contrib <- data.table::data.table(
        year = numeric(),
        coicop = character(),
        month = numeric(),
        quantile = character(),
        contribution = numeric())
    for (j in unique(df_cpi_FR$coicop)) {
        for (y in years[3:length(years)]) {
            for (m in unique(df_cpi_FR[year == y, month])) {
                for (q in unique(df_product_quantile_norm_weights_FR$quantile)) {
                    p_y1_12 <- sum(df_cpi_FR[month == 12
                                         & year == y-1, value])
                    p_y1_m <- sum(df_cpi_FR[month == m
                                            & year == y-1, value])
                    p_y1_12_j <- df_cpi_FR[coicop == j
                                         & month == 12
                                         & year == y-1, value]
                    p_y1_m_j <- df_cpi_FR[coicop == j
                                        & month == m
                                        & year == y-1, value]
                    w_y1_j_q <- df_product_quantile_norm_weights_FR[coicop == j
                                                                    & year == y-1
                                                                    & quantile == q,
                                                                    weighted_consumption]
                    p_y_m_j <- df_cpi_FR[coicop == j
                                         & month == m
                                         & year == y, value]
                    p_y2_12 <- sum(df_cpi_FR[month == 12
                                         & year == y-2, value])
                    p_y2_12_j <- df_cpi_FR[coicop == j
                                           & month == m
                                           & year == y-2, value]
                    w_y2_j_q <- df_product_quantile_norm_weights_FR[coicop == j
                                                                    & year == y-2
                                                                    & quantile == q,
                                                                    weighted_consumption]
                    value <- (p_y1_12 / p_y1_m) * w_y1_j_q * ((p_y_m_j - p_y1_12_j) / p_y1_12_j) + (p_y2_12 / p_y1_m) * w_y2_j_q * ((p_y1_12_j - p_y1_m_j) / p_y2_12_j)

                    contrib <- rbind(contrib, list(y, j, m, q, value))
                }
            }
        }
    }
    return(contrib)
}
