
###############################################################################
###############################################################################
#' CreatePlots1
#'
#' @param d a
#' @param weeknow a
#' @param Ukenummer a
#' @param title a
#' @param yrange a
#' @import data.table
#' @export
#'
ggplot_CreatePlots1 <- function(d, weeknow, Ukenummer, title, yrange) {
  y <- as.numeric(row.names(d)[dim(d)[1]])
  w <- weeknow
  y5 <- y - 5
  ind <- which(row.names(d) == y5)

  par(mfrow = c(1, 1), mar = c(2.6, 2.6, 1.4, .5), oma = c(0, 0, 0, 0))

  if (w >= 1 && w <= 29) {
    plot(c(d[ind - 1, 30:52], d[ind, 1:29]),
      type = "l", col = "green", xlim = c(1, 52), ylim = c(0, yrange),
      main = "", xlab = "", ylab = "", lwd = 1.5, cex.lab = 0.75, cex.main = 2, axes = F
    )
    lines(c(d[ind, 30:52], d[ind + 1, 1:29]), type = "l", col = "red", lwd = 1.5)
    lines(c(d[ind + 1, 30:52], d[ind + 2, 1:29]), type = "l", col = "orange", lwd = 1.5)
    lines(c(d[ind + 2, 30:52], d[ind + 3, 1:29]), type = "l", col = "purple", lwd = 1.5)
    lines(c(d[ind + 3, 30:52], d[ind + 4, 1:29]), type = "l", col = "blue", lwd = 1.5)
    lines(c(d[ind + 4, 30:52], d[ind + 5, 1:weeknow]), type = "l", col = "black", lwd = 2)

    legend <- NULL
    for (r in 6:1) {
      rr <- r - 1
      legend <- c(legend, (paste(y - r, "/", y - rr)))
    }
  } else if (w >= 30 && w <= 52) {
    plot(c(d[ind, 30:52], d[ind + 1, 1:29]),
      type = "l", col = "green", xlim = c(1, 52), ylim = c(0, yrange),
      main = "", xlab = "", ylab = "", lwd = 1.5, cex.lab = 0.75, cex.main = 2, axes = F
    )
    lines(c(d[ind + 1, 30:52], d[ind + 2, 1:29]), type = "l", col = "red", lwd = 1.5)
    lines(c(d[ind + 2, 30:52], d[ind + 3, 1:29]), type = "l", col = "orange", lwd = 1.5)
    lines(c(d[ind + 3, 30:52], d[ind + 4, 1:29]), type = "l", col = "purple", lwd = 1.5)
    lines(c(d[ind + 4, 30:52], d[ind + 5, 1:29]), type = "l", col = "blue", lwd = 1.5)
    lines(d[ind + 5, 30:weeknow], type = "l", col = "black", lwd = 2)

    legend <- NULL
    for (r in 5:0) {
      rr <- r - 1
      legend <- c(legend, (paste(y - r, "/", y - rr)))
    }
  }

  q <- axis(1, at = 1:52, labels = Ukenummer, las = 1, cex.axis = 0.7)
  axis(2, las = 3, cex.axis = 0.75)
  box(lwd = 1)


  abline(v = c(22, 25), col = "black", lty = 2)
  abline(v = c(34, 40), col = "black", lty = 2)
  text(23.5, 0, "Jul/Nytt\u00E5r", col = "black", cex = 0.75)
  text(37, 0, "P\u00E5ske", col = "black", cex = 0.75)

  q <- legend("topright",
    inset = .00, roundUpNice(yrange), legend = legend,
    lty = 1, col = c("green", "red", "orange", "purple", "blue", "black"),
    lwd = c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2), cex = 0.45, box.lty = 1, box.lwd = 1, text.font = 1, seg.len = 2
  )
  mtext(title, outer = F, cex = 0.8, font = 2, line = .5)
  mtext(text = "Ukenummer", side = 1, line = 1.8, cex = .75)
  mtext(text = "Antall konsultasjoner", side = 2, line = 1.8, cex = .75)
  # box("figure", col="blue")
}


###############################################################################
###############################################################################
#' CreatePlots1
#'
#' @param data_long a
#' @param weeknow a
#' @param Ukenummer a
#' @param title a
#' @param yrange a
#' @import data.table
#' @export
#'
ggplot_CreatePlotsNorway <- function(data_long, weeknow, Ukenummer, title, yrange) {
  seasons <- rev(unique(data_long$season))[1:5]
  labs <- unique(data_long[, c("week", "x")])
  labs <- labs[as.numeric(week) %in% seq(2, 52, 2)]

  q <- ggplot(data_long[season %in% seasons], aes(x = x, y = value, color = season))
  q <- q + geom_vline(xintercept = fhi::x(51), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(2), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(11), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(17), lty = 2)
  q <- q + annotate("text", x = (fhi::x(51) + fhi::x(2)) / 2, y = yrange * 0.03, label = "Jul/Nytt\u00E5r", size = 10)
  q <- q + annotate("text", x = fhi::x(14), y = yrange * 0.03, label = "P\u00E5ske", size = 10)
  q <- q + geom_line(lwd = 3)
  q <- q + fhiplot::theme_fhi_basic(24)
  q <- q + fhiplot::scale_color_fhi("", palette = "combination")
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

##############################################################################
#' CreatePlots2
#'
#' @param d1 a
#' @param weeknow a
#' @param Ukenummer a
#' @param Fylkename a
#' @param S a
#' @param mytittle a
#' @import data.table
#' @export
#'
ggplot_CreatePlots2 <- function(d1, weeknow, Ukenummer, Fylkename, S, mytittle) {
  ageGroups <- c("0 - 4 \u00E5r", "5 - 19 \u00E5r", "20 - 64 \u00E5r", "65+ \u00E5r")

  par(mfrow = c(2, 2), mar = c(2, 2, 2, .5), oma = c(0, 0, 2, 0))

  for (ii in 1:4) {
    d <- selectAgeGroups(d1, ageG = ii, S = S)
    yrange <- max(d, na.rm = T) + (roundUpNice(max(d, na.rm = T)) * .25)
    y <- as.numeric(row.names(d)[dim(d)[1]])
    w <- weeknow
    y5 <- y - 5
    ind <- which(row.names(d) == y5)

    if (w >= 1 && w <= 29) {
      plot(c(d[ind - 1, 30:52], d[ind, 1:29]),
        type = "l", col = "green", xlim = c(1, 52), ylim = c(0, yrange),
        main = ageGroups[ii], xlab = "Ukenummer", ylab = "Antall konsultasjoner",
        lwd = 1, cex.lab = 0.75, cex.main = 0.85, axes = F
      )
      lines(c(d[ind, 30:52], d[ind + 1, 1:29]), type = "l", col = "red", lwd = 1)
      lines(c(d[ind + 1, 30:52], d[ind + 2, 1:29]), type = "l", col = "orange", lwd = 1)
      lines(c(d[ind + 2, 30:52], d[ind + 3, 1:29]), type = "l", col = "purple", lwd = 1)
      lines(c(d[ind + 3, 30:52], d[ind + 4, 1:29]), type = "l", col = "blue", lwd = 1)
      lines(c(d[ind + 4, 30:52], d[ind + 5, 1:weeknow]), type = "l", col = "black", lwd = 2)
    } else if (w >= 30 && w <= 52) {
      plot(c(d[ind, 30:52], d[ind + 1, 1:29]),
        type = "l", col = "green", xlim = c(1, 52), ylim = c(0, yrange),
        main = ageGroups[ii], xlab = "Ukenummer", ylab = "Antall konsultasjoner",
        lwd = 1, cex.lab = 0.75, cex.main = 0.85, axes = F
      )
      lines(c(d[ind + 1, 30:52], d[ind + 2, 1:29]), type = "l", col = "red", lwd = 1)
      lines(c(d[ind + 2, 30:52], d[ind + 3, 1:29]), type = "l", col = "orange", lwd = 1)
      lines(c(d[ind + 3, 30:52], d[ind + 4, 1:29]), type = "l", col = "purple", lwd = 1)
      lines(c(d[ind + 4, 30:52], d[ind + 5, 1:29]), type = "l", col = "blue", lwd = 1)
      lines(d[ind + 5, 30:weeknow], type = "l", col = "black", lwd = 2)
    }

    q <- axis(1, at = 1:52, labels = Ukenummer, las = 1, cex.axis = 0.7)
    axis(2, las = 3, cex.axis = 0.75)
    box(lwd = 1)

    abline(v = c(22, 25), col = "black", lty = 2)
    abline(v = c(34, 40), col = "black", lty = 2)
  }

  mtext(paste(mytittle, Fylkename, "aldersfordelt", sep = ", "), outer = TRUE, cex = 0.8, font = 2)
  # box("figure", col="blue")
  # box("outer", lty="solid", col="green")
}
###############################################################################

##############################################################################
#' CreatePlotsNorwayByAge
#'
#' @param data_long a
#' @param weeknow a
#' @param Ukenummer a
#' @param Fylkename a
#' @param S a
#' @param mytittle a
#' @import data.table
#' @export
#'
ggplot_CreatePlotsNorwayByAge <- function(data_long, weeknow, Ukenummer, Fylkename, S, mytittle) {
  ageGroups <- c("0 - 4 \u00E5r", "5 - 19 \u00E5r", "20 - 64 \u00E5r", "65+ \u00E5r")
  data_long[, age := factor(newage, levels = c(1:4), labels = ageGroups)]

  seasons <- rev(unique(data_long$season))[1:5]
  labs <- unique(data_long[, c("week", "x")])
  labs <- labs[as.numeric(week) %in% c("30", "37", "44", "51", "5", "11", "18", "25")]

  q <- ggplot(data_long[season %in% seasons], aes(x = x, y = value, color = season))
  q <- q + geom_vline(xintercept = fhi::x(51), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(2), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(11), lty = 2)
  q <- q + geom_vline(xintercept = fhi::x(17), lty = 2)
  q <- q + geom_line(lwd = 3)
  q <- q + fhiplot::theme_fhi_basic(24)
  q <- q + fhiplot::scale_color_fhi("", palette = "combination")
  q <- q + expand_limits(y = 0)
  q <- q + scale_y_continuous("Antall konsultasjoner", expand = expand_scale(mult = c(0, 0.1)))
  q <- q + scale_x_continuous(
    "Ukenummer",
    expand = expand_scale(mult = c(0, 0)),
    breaks = labs$x,
    labels = labs$week
  )
  q <- q + lemon::facet_rep_wrap(~age, repeat.tick.labels = "all", scales = "free")
  q <- q + labs(title = paste(mytittle, "Norge, aldersfordelt", sep = ", "))
  q
}
