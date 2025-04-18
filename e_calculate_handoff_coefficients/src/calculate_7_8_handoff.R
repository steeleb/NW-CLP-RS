#' @title Landsat 7 to 8 Handoff Coefficients
#' 
#' @description
#' Function to calculate the handoff coefficients between Landsat 7 and 8, which
#' will be used to normalize the LS 7 to relative LS 8 values
#' 
#' @param filtered filtered regional dataset used to calculate handoffs
#' @param band any band name that is shared between the two satellites
#' @returns silently returns summary .csvs in the mid folder, and figures of the
#' polynomial regression
#' 
#' 
calculate_7_8_handoff <- function(filtered, band){
  # filter for the overlapping date range from sites that have at least 7y of data
  filter_summary <- filtered %>%
    filter(date > ymd("2013-02-11"), 
           date < ymd("2022-04-16"), 
           mission %in% c("LANDSAT_8", "LANDSAT_7")) %>% 
    group_by(mission, r_id) %>% 
    summarize(n_years = length(unique(year(date)))) %>% 
    filter(n_years >= 7) %>% 
    ungroup()
  
  # filter out for Landsat 8, limiting input sites to those with 8 years of data
  y <- filtered %>%
    filter(date > ymd("2013-02-11"), 
           date < ymd("2022-04-16"), 
           mission == "LANDSAT_8") %>% 
    inner_join(., filter_summary)
  # make quantile summary of band, dropping 0 and 1
  y_q <- y %>%
    pull(band) %>%
    as.numeric(.) %>% 
    quantile(., seq(.01,.99, .01))
  
  # do the same for LS 7
  x <- filtered %>% 
    filter(date > ymd("2013-02-11"), 
           date < ymd("2022-04-16"), 
           mission == "LANDSAT_7") %>% 
    inner_join(., filter_summary)
  # make quantile summary of band, dropping 0 and 1
  x_q <- x %>%
    pull(band) %>%
    as.numeric(.) %>% 
    quantile(., seq(.01,.99, .01))
  
  
  poly <- lm(y_q ~ poly(x_q, 2, raw = T))
  
  # plot and save handoff fig
  jpeg(file.path("e_calculate_handoff_coefficients/figs/", 
                 paste0(band, "_7_8_poly_handoff.jpg")), 
       width = 350, height = 350)
  plot(y_q ~ x_q,
       main = paste0(band, " LS 7-8 handoff"),
       xlab = "0.01 Quantile Values for LS8 Rrs",
       ylab = "0.01 Quantile Values for LS7 Rrs")
  lines(sort(x_q),
        fitted(poly)[order(x_q)],
        col = "blue",
        type = "l")
  dev.off()
  
  # plot and save residuals from fit
  jpeg(file.path("e_calculate_handoff_coefficients/figs/", 
                 paste0(band, "_7_8_poly_residuals.jpg")), 
       width = 350, height = 200)
  plot(poly$residuals,
       main = paste0(band, " LS 7-8 poly handoff residuals"))
  dev.off()
  
  # create a summary table
  summary <- tibble(band = band, 
                    intercept = poly$coefficients[[1]], 
                    B1 = poly$coefficients[[2]], 
                    B2 = poly$coefficients[[3]],
                    min_in_val = min(x_q),
                    max_in_val = max(x_q),
                    sat_corr = "LANDSAT_7",
                    sat_to = "LANDSAT_8",
                    L8_scene_count = length(unique(y$system.index)),
                    L7_scene_count = length(unique(x$system.index))) 
  write_csv(summary, file.path("e_calculate_handoff_coefficients/mid/",
                               paste0(band, "_7_8_poly_handoff.csv")))
}
