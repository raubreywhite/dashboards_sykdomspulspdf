#' Ukenummer
#' @export Ukenummer
Ukenummer <- c(30:52, 1:29)
###############################################################################
###############################################################################
#' Title
#'
#' @param x a
#' @export firstup
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
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
#' @export CreatePlots1
#'
CreatePlots1 <- function(d, weeknow, Ukenummer, title, yrange) {
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
  }

  q <- axis(1, at = 1:52, labels = Ukenummer, las = 1, cex.axis = 0.7)
  axis(2, las = 3, cex.axis = 0.75)
  box(lwd = 1)


  abline(v = c(22, 25), col = "black", lty = 2)
  abline(v = c(34, 40), col = "black", lty = 2)
  text(23.5, 0, "Jul/Nytt\u00E5r", col = "black", cex = 0.75)
  text(37, 0, "P\u00E5ske", col = "black", cex = 0.75)

  legend <- NULL
  for (r in 6:1) {
    rr <- r - 1
    legend <- c(legend, (paste(y - r, "/", y - rr)))
  }

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
#' @param d a
#' @param weeknow a
#' @param Ukenummer a
#' @param title a
#' @param yrange a
#' @import data.table
#' @export CreatePlotsNorway
#'
CreatePlotsNorway <- function(d, weeknow, Ukenummer, title, yrange) {
  par(mfrow = c(1, 1), mar = c(4, 5, 4, 2))


  y <- as.numeric(row.names(d)[dim(d)[1]])
  w <- weeknow
  y5 <- y - 5
  ind <- which(row.names(d) == y5)

  if (w >= 1 && w <= 29) {
    plot(c(d[ind - 1, 30:52], d[ind, 1:29]),
      type = "l", col = "green", xlim = c(1, 52), ylim = c(0, yrange),
      main = "", xlab = "", ylab = "", lwd = 1.5, cex.lab = 1.4, cex.main = 1.7, axes = F, dev.new(width = 40, height = 26)
    )
    lines(c(d[ind, 30:52], d[ind + 1, 1:29]), type = "l", col = "red", lwd = 4)
    lines(c(d[ind + 1, 30:52], d[ind + 2, 1:29]), type = "l", col = "orange", lwd = 4)
    lines(c(d[ind + 2, 30:52], d[ind + 3, 1:29]), type = "l", col = "purple", lwd = 4)
    lines(c(d[ind + 3, 30:52], d[ind + 4, 1:29]), type = "l", col = "blue", lwd = 4)
    lines(c(d[ind + 4, 30:52], d[ind + 5, 1:weeknow]), type = "l", col = "black", lwd = 6)
  } else if (w >= 30 && w <= 52) {
    plot(c(d[ind, 30:52], d[ind + 1, 1:29]),
      type = "l", col = "green", xlim = c(1, 52), ylim = c(0, yrange),
      main = "", xlab = "", ylab = "", lwd = 4, cex.lab = 0.75, cex.main = 2, axes = F, dev.new(width = 40, height = 26)
    )
    lines(c(d[ind + 1, 30:52], d[ind + 2, 1:29]), type = "l", col = "red", lwd = 4)
    lines(c(d[ind + 2, 30:52], d[ind + 3, 1:29]), type = "l", col = "orange", lwd = 4)
    lines(c(d[ind + 3, 30:52], d[ind + 4, 1:29]), type = "l", col = "purple", lwd = 4)
    lines(c(d[ind + 4, 30:52], d[ind + 5, 1:29]), type = "l", col = "blue", lwd = 4)
    lines(d[ind + 5, 30:weeknow], type = "l", col = "black", lwd = 6)
  }

  q <- axis(1, at = 1:52, labels = Ukenummer, las = 2, cex.axis = 1.2)
  axis(2, las = 3, cex.axis = 1.2)
  box(lwd = 1)


  abline(v = c(22, 25), col = "black", lty = 2)
  abline(v = c(34, 40), col = "black", lty = 2)
  text(23.5, 0, "Jul/Nytt\u00E5r", col = "black", cex = 1.5)
  text(37, 0, "P\u00E5ske", col = "black", cex = 1.5)

  legend <- NULL
  for (r in 6:1) {
    rr <- r - 1
    legend <- c(legend, (paste(y - r, "/", y - rr)))
  }

  q <- legend("topright",
    inset = .00, roundUpNice(yrange), legend = legend,
    lty = 1, col = c("green", "red", "orange", "purple", "blue", "black"),
    lwd = c(4, 4, 4, 4, 4, 4), cex = 1.2, text.width = 3.5, y.intersp = .4
  )


  mtext(title, outer = F, cex = 1.7, font = 2, line = .5)
  mtext(text = "Ukenummer", side = 1, line = 2.8, cex = 1.5, font = 2)
  mtext(text = "Antall konsultasjoner", side = 2, line = 2.8, cex = 1.5, font = 2)
  # box("figure", col="blue")
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
#' @export CreatePlots2
#'
CreatePlots2 <- function(d1, weeknow, Ukenummer, Fylkename, S, mytittle) {
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
#' @param d1 a
#' @param weeknow a
#' @param Ukenummer a
#' @param Fylkename a
#' @param S a
#' @param mytittle a
#' @import data.table
#' @export CreatePlotsNorwayByAge
#'
CreatePlotsNorwayByAge <- function(d1, weeknow, Ukenummer, Fylkename, S, mytittle) {
  ageGroups <- c("0 - 4 \u00E5r", "5 - 19 \u00E5r", "20 - 64 \u00E5r", "65+ \u00E5r")

  par(mfrow = c(2, 2), mar = c(4, 5, 4, 2), oma = c(3, 3, 3, 3))

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
        lwd = 2, cex.lab = 1.5, cex.main = 2, axes = F
      )

      lines(c(d[ind, 30:52], d[ind + 1, 1:29]), type = "l", col = "red", lwd = 2)
      lines(c(d[ind + 1, 30:52], d[ind + 2, 1:29]), type = "l", col = "orange", lwd = 2)
      lines(c(d[ind + 2, 30:52], d[ind + 3, 1:29]), type = "l", col = "purple", lwd = 2)
      lines(c(d[ind + 3, 30:52], d[ind + 4, 1:29]), type = "l", col = "blue", lwd = 2)
      lines(c(d[ind + 4, 30:52], d[ind + 5, 1:weeknow]), type = "l", col = "black", lwd = 4)
    } else if (w >= 30 && w <= 52) {
      plot(c(d[ind, 30:52], d[ind + 1, 1:29]),
        type = "l", col = "green", xlim = c(1, 52), ylim = c(0, yrange),
        main = ageGroups[ii], xlab = "Ukenummer", ylab = "Antall konsultasjoner",
        lwd = 2, cex.lab = 1.5, cex.main = 2, axes = F
      )

      lines(c(d[ind + 1, 30:52], d[ind + 2, 1:29]), type = "l", col = "red", lwd = 2)
      lines(c(d[ind + 2, 30:52], d[ind + 3, 1:29]), type = "l", col = "orange", lwd = 2)
      lines(c(d[ind + 3, 30:52], d[ind + 4, 1:29]), type = "l", col = "purple", lwd = 2)
      lines(c(d[ind + 4, 30:52], d[ind + 5, 1:29]), type = "l", col = "blue", lwd = 2)
      lines(d[ind + 5, 30:weeknow], type = "l", col = "black", lwd = 4)
    }

    q <- axis(1, at = 1:52, labels = Ukenummer, las = 1, cex.axis = 1.2)
    axis(2, las = 3, cex.axis = 1.2)
    box(lwd = 1)

    abline(v = c(22, 25), col = "black", lty = 2)
    abline(v = c(34, 40), col = "black", lty = 2)
  }

  mtext(paste(mytittle, "Norge, aldersfordelt", sep = ", "), outer = TRUE, cex = 2, font = 1)
  # box("figure", col="blue")
  # box("outer", lty="solid", col="green")
}
