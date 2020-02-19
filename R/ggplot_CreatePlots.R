
###############################################################################
###############################################################################
#' ggplot_plot_total
#'
#' @param location_code a
#' @param x_tag a
#' @import data.table
#' @export
#'
ggplot_plot_total <- function(location_code, x_tag) {
  data_long <- fd::tbl("spuls_standard_results") %>%
    dplyr::filter(granularity_time=="weekly") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(tag== !!x_tag) %>%
    dplyr::filter(age== "Totalt") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  data_long[,season:=fhi::season(yrwk)]

  seasons <- rev(unique(data_long$season))[1:5]
  labs <- unique(data_long[, c("week", "x")])
  labs <- labs[as.numeric(week) %in% seq(2, 52, 2)]

  data_long <- data_long[season %in% seasons]
  yrange <- max(data_long$n)

  title <- glue::glue(
    "{syndrome}, {location}, alle aldersgrupper",
    syndrome = sykdomspuls::CONFIG$SYNDROMES[tag==x_tag]$namesLong,
    location = data_long$location_name[1]
  )

  q <- ggplot(data_long, aes(x = x, y = n, color = season))
  q <- q + geom_vline(xintercept = fhi::x(51), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(2), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(11), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(17), lty = 2)
  q <- q + annotate("label", x = (fhi::x(51) + fhi::x(2)) / 2, y = yrange * 0.03, label = "Jul/Nytt\u00E5r", size = 3)
  q <- q + annotate("label", x = fhi::x(14), y = yrange * 0.03, label = "P\u00E5ske", size = 3)
  q <- q + geom_line(lwd = 0.5)
  q <- q + geom_line(data=data_long[season==max(season)],lwd = 1.5)
  q <- q + fhiplot::theme_fhi_basic(10)
  q <- q + fhiplot::scale_color_fhi("", palette = "combination", direction = -1)
  q <- q + guides(color = guide_legend(reverse = TRUE))
  q <- q + expand_limits(y = 0)
  q <- q + scale_y_continuous("Antall konsultasjoner", expand = expand_scale(mult = c(0, 0.1)))
  q <- q + scale_x_continuous(
    "Ukenummer",
    expand = expand_scale(mult = c(0, 0)),
    breaks = labs$x,
    labels = labs$week
  )
  q <- q + labs(title = title)
  q
}

#' ggplot_plot_1
#'
#' @param location_code a
#' @param x_tag a
#' @import data.table
#' @export
#'
ggplot_plot_ages <- function(location_code, x_tag) {
  data_long <- fd::tbl("spuls_standard_results") %>%
    dplyr::filter(granularity_time=="weekly") %>%
    dplyr::filter(location_code== !!location_code) %>%
    dplyr::filter(tag== !!x_tag) %>%
    dplyr::filter(age!= "Totalt") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  data_long[,season:=fhi::season(yrwk)]
  data_long[,age:=car::recode(
    age,
    glue::glue(
      "c('5-14','15-19')='5-19';",
      "c('20-29','30-64')='20-64'"
    ))]
  data_long <- data_long[,.(
    n=sum(n)
  ),keyby=.(
    age,
    season,
    x,
    week,
    location_name
  )]
  data_long[,age:=factor(
    age,
    levels=c("0-4","5-19","20-64","65+"),
    labels =c(
      glue::glue("0-4 {fhi::nb$aa}r"),
      glue::glue("5-19 {fhi::nb$aa}r"),
      glue::glue("20-64 {fhi::nb$aa}r"),
      glue::glue("65+ {fhi::nb$aa}r")
      )
    )]

  seasons <- rev(unique(data_long$season))[1:5]
  labs <- unique(data_long[, c("week", "x")])
  labs <- labs[as.numeric(week) %in% seq(2, 52, 4)]

  data_long <- data_long[season %in% seasons]
  yrange <- max(data_long$n)

  title <- glue::glue(
    "{syndrome}, {location}, aldersfordelt",
    syndrome = sykdomspuls::CONFIG$SYNDROMES[tag==x_tag]$namesLong,
    location = data_long$location_name[1]
  )

  q <- ggplot(data_long, aes(x = x, y = n, color = season))
  q <- q + geom_vline(xintercept = fhi::x(51), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(2), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(11), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(17), lty = 2)
  q <- q + geom_line(lwd = 0.5)
  q <- q + geom_line(data=data_long[season==max(season)],lwd = 1.5)
  q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "all", scales="free")
  q <- q + fhiplot::theme_fhi_basic(10)
  q <- q + fhiplot::scale_color_fhi("", palette = "combination", direction = -1)
  q <- q + guides(color = FALSE)
  q <- q + expand_limits(y = 0)
  q <- q + scale_y_continuous("Antall konsultasjoner", expand = expand_scale(mult = c(0, 0.1)))
  q <- q + scale_x_continuous(
    "Ukenummer",
    expand = expand_scale(mult = c(0, 0)),
    breaks = labs$x,
    labels = labs$week
  )
  q <- q + labs(title = title)
  q
}
