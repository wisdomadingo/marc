.lo <- function(rown,
                coln,
                nrow,
                ncol,
                cellHeight = NA,
                cellWidth = NA,
                treeHeightCol,
                treeHeightRow,
                legend,
                annotationRow,
                annotationCol,
                annotationColors,
                annotationLegend,
                annotationNamesRow,
                annotationNamesCol,
                main,
                fontSize,
                fontSizeRow,
                fontSizeCol,
                gapsRow,
                gapsCol,
                ...) {
  # Get height of colnames and length of rownames
  if (!is.null(coln[1]) |
      (!.is.na2(annotationRow) & annotationNamesRow)) {
    if (!is.null(coln[1])) {
      t <- coln
    } else {
      t <- ""
    }
    tw <- strwidth(t, units = "in", cex = fontSizeCol / fontSize)
    if (annotationNamesRow) {
      t <- c(t, colnames(annotationRow))
      tw <- c(tw, strwidth(colnames(annotationRow), units = "in"))
    }
    longestColn <- which.max(tw)
    gp <- list(fontSize = ifelse(longestColn <= length(coln),
                                 fontSizeCol,
                                 fontSize), ...)
    colnHeight <- unit(1,
                       "grobheight",
                       textGrob(t[longestColn],
                                rot = 90,
                                gp = do.call(gpar, gp))) +
      unit(10, "bigpts")
  } else {
    colnHeight <- unit(5, "bigpts")
  }
  
  if (!is.null(rown[1])) {
    t <- rown
    tw <- strwidth(t, units = "in", cex = fontSizeRow / fontSize)
    if (annotationNamesCol) {
      t <- c(t, colnames(annotationCol))
      tw <- c(tw, strwidth(colnames(annotationCol), units = "in"))
    }
    longestRown <- which.max(tw)
    gp <- list(fontSize = ifelse(longestRown <= length(rown),
                                 fontSizeRow,
                                 fontSize), ...)
    rownWidth <- unit(1,
                      "grobwidth",
                      textGrob(t[longestRown],
                               gp = do.call(gpar, gp))) +
      unit(10, "bigpts")
  } else {
    rownWidth <- unit(5, "bigpts")
  }
  
  gp <- list(fontSize = fontSize, ...)
  # Legend position
  if (!.is.na2(legend)) {
    longestBreak <- which.max(nchar(names(legend)))
    longestBreak <- unit(1.1,
                         "grobwidth",
                         textGrob(as.character(names(legend))[longestBreak],
                                  gp = do.call(gpar, gp)))
    titleLength <- unit(1.1,
                        "grobwidth",
                        textGrob("Scale",
                                 gp = gpar(fontface = "bold",
                                           ...)))
    legendWidth <- unit(12, "bigpts") + longestBreak * 1.2
    legendWidth <- max(titleLength, legendWidth)
  } else {
    legendWidth <- unit(0, "bigpts")
  }
  
  # Set main title height
  if (is.na(main)) {
    mainHeight <- unit(0, "npc")
  } else {
    mainHeight <- unit(1.5,
                       "grobheight",
                       textGrob(main,
                                gp = gpar(fontSize = 1.3 * fontSize,
                                          ...)))
  }
  
  # Column annotations
  textheight <- unit(fontSize, "bigpts")
  
  if (!.is.na2(annotationCol)) {
    # Column annotation height
    annotColHeight <-
      ncol(annotationCol) *
      (textheight + unit(2, "bigpts")) +
      unit(2, "bigpts")
    
    # Width of the correponding legend
    t <- c(as.vector(as.matrix(annotationCol)), colnames(annotationCol))
    annotColLegendWidth <- unit(1.2,
                                "grobwidth",
                                textGrob(t[which.max(nchar(t))],
                                         gp = gpar(...))) +
      unit(12, "bigpts")
    if (!annotationLegend) {
      annotColLegendWidth <- unit(0, "npc")
    }
  } else {
    annotColHeight <- unit(0, "bigpts")
    annotColLegendWidth <- unit(0, "bigpts")
  }
  
  # Row annotations
  if (!.is.na2(annotationRow)) {
    # Row annotation width
    annotRowWidth <- ncol(annotationRow) *
      (textheight + unit(2, "bigpts")) +
      unit(2, "bigpts")
    
    # Width of the correponding legend
    t <- c(as.vector(as.matrix(annotationRow)),
           colnames(annotationRow))
    annotRowLegendWidth <- unit(1.2,
                                "grobwidth",
                                textGrob(t[which.max(nchar(t))],
                                         gp = gpar(...))) +
      unit(12,
           "bigpts")
    
    if (!annotationLegend) {
      annotRowLegendWidth <- unit(0, "npc")
    }
  } else {
    annotRowWidth <- unit(0, "bigpts")
    annotRowLegendWidth <- unit(0, "bigpts")
  }
  
  annotLegendWidth <- max(annotRowLegendWidth, annotColLegendWidth)
  
  # Tree height
  treeHeightCol <- unit(treeHeightCol, "bigpts") + unit(5, "bigpts")
  treeHeightRow <- unit(treeHeightRow, "bigpts") + unit(5, "bigpts")
  
  # Set cell sizes
  if (is.na(cellWidth)) {
    matWidth <- unit(1, "npc") -
      rownWidth -
      legendWidth -
      treeHeightRow -
      annotRowWidth -
      annotLegendWidth
  } else {
    matWidth <- unit(cellWidth * ncol, "bigpts") +
      length(gapsCol) *
      unit(0, "bigpts")
  }
  
  if (is.na(cellHeight)) {
    matHeight <- unit(1, "npc") -
      mainHeight -
      colnHeight -
      treeHeightCol -
      annotColHeight
  } else {
    matHeight <- unit(cellHeight * nrow, "bigpts") +
      length(gapsRow) *
      unit(0, "bigpts")
  }
  
  # Produce gtable
  gt <- gtable::gtable(widths = unit.c(treeHeightRow,
                                       annotRowWidth,
                                       matWidth,
                                       rownWidth,
                                       legendWidth,
                                       annotLegendWidth),
                       heights = unit.c(mainHeight,
                                        treeHeightCol,
                                        annotColHeight,
                                        matHeight,
                                        colnHeight),
                       vp = viewport(gp = do.call(gpar, gp)))
  
  cw <- convertWidth(matWidth -
                       (length(gapsCol) * unit(0, "bigpts")),
                     "bigpts", valueOnly = TRUE) / ncol
  ch <- convertHeight(matHeight -
                        (length(gapsRow) * unit(0, "bigpts")),
                      "bigpts", valueOnly = TRUE) / nrow
  
  # Return minimal cell dimension in bigpts to decide if borders are drawn
  mindim <- min(cw, ch)
  
  res <- list(gt = gt, mindim = mindim)
  
  return(res)
}

.findCoordinates <- function(n, gaps, m = seq(1, n)) {
  if (length(gaps) == 0) {
    return(list(
      coord = unit(m / n, "npc"),
      size = unit(1 / n, "npc")))
  }
  
  if (max(gaps) > n) {
    stop("Gaps do not match with matrix size")
  }
  
  size <- (1 / n) *
    (unit(1, "npc") - length(gaps) * unit("0", "bigpts"))
  
  gaps2 <- base::apply(vapply(gaps,
                              function(gap, x) {
                                x > gap
                              },
                              integer(n), m), 1, sum)
  coord <- m * size + (gaps2 * unit("0", "bigpts"))
  
  return(list(coord = coord, size = size))
}

.drawDendrogram <- function(hc, gaps, horizontal = TRUE) {
  h <- hc$height / max(hc$height) / 1.05
  m <- hc$merge
  o <- hc$order
  n <- length(o)
  
  m[m > 0] <- n + m[m > 0]
  m[m < 0] <- abs(m[m < 0])
  
  dist <- matrix(0,
                 nrow = 2 * n - 1,
                 ncol = 2,
                 dimnames = list(NULL, c("x", "y")))
  dist[seq(1, n), 1] <- 1 / n / 2 + (1 / n) *
    (match(seq(1, n), o) - 1)
  
  for (i in seq(1, nrow(m))) {
    dist[n + i, 1] <- (dist[m[i, 1], 1] + dist[m[i, 2], 1]) / 2
    dist[n + i, 2] <- h[i]
  }
  
  drawConnection <- function(x1, x2, y1, y2, y) {
    res <- list(x = c(x1, x1, x2, x2),
                y = c(y1, y, y, y2))
    
    return(res)
  }
  
  x <- rep(NA, nrow(m) * 4)
  y <- rep(NA, nrow(m) * 4)
  id <- rep(seq(nrow(m)), rep(4, nrow(m)))
  
  for (i in seq(1, nrow(m))) {
    c <- drawConnection(dist[m[i, 1], 1],
                        dist[m[i, 2], 1],
                        dist[m[i, 1], 2],
                        dist[m[i, 2], 2],
                        h[i])
    k <- (i - 1) * 4 + 1
    x[seq(k, k + 3)] <- c$x
    y[seq(k, k + 3)] <- c$y
  }
  
  x <- .findCoordinates(n, gaps, x * n)$coord
  y <- unit(y, "npc")
  
  if (!horizontal) {
    a <- x
    x <- unit(1, "npc") - y
    y <- unit(1, "npc") - a
  }
  res <- polylineGrob(x = x, y = y, id = id)
  
  return(res)
}

.drawMatrix <- function(matrix,
                        borderColor,
                        gapsRows,
                        gapsCols,
                        fmat,
                        fontSizeNumber,
                        numberColor) {
  
  n <- nrow(matrix)
  m <- ncol(matrix)
  
  coordX <- .findCoordinates(m, gapsCols)
  coordY <- .findCoordinates(n, gapsRows)
  
  x <- coordX$coord -
    0.5 * coordX$size
  y <- unit(1, "npc") -
    (coordY$coord - 0.5 * coordY$size)
  
  coord <- expand.grid(y = y, x = x)
  
  res <- gList()
  
  res[["rect"]] <- rectGrob(x = coord$x,
                            y = coord$y,
                            width = coordX$size,
                            height = coordY$size,
                            gp = gpar(fill = matrix, col = borderColor))
  
  if (attr(fmat, "draw")) {
    res[["text"]] <- textGrob(x = coord$x,
                              y = coord$y,
                              label = fmat,
                              gp = gpar(col = numberColor, fontSize = fontSizeNumber))
  }
  
  res <- gTree(children = res)
  
  return(res)
}

.drawColnames <- function(coln, gaps, ...) {
  coord <- .findCoordinates(length(coln), gaps)
  x <- coord$coord - 0.5 * coord$size
  
  res <- textGrob(coln,
                  x = x,
                  y = unit(1, "npc") -
                    unit(3, "bigpts"),
                  vjust = 0.5,
                  hjust = 0,
                  rot = 270,
                  gp = gpar(...))
  
  return(res)
}

.drawRownames <- function(rown, gaps, ...) {
  coord <- .findCoordinates(length(rown), gaps)
  y <- unit(1, "npc") - (coord$coord - 0.5 * coord$size)
  
  res <- textGrob(rown,
                  x = unit(3, "bigpts"),
                  y = y,
                  vjust = 0.5,
                  hjust = 0,
                  gp = gpar(...))
  
  return(res)
}

.drawLegend <- function(color, breaks, legend, ...) {
  height <- min(unit(1, "npc"), unit(150, "bigpts"))
  
  legendPos <- (legend - min(breaks)) / (max(breaks) - min(breaks))
  legendPos <- height * legendPos + (unit(1, "npc") - height)
  
  breaks <- (breaks - min(breaks)) / (max(breaks) - min(breaks))
  breaks <- height * breaks + (unit(1, "npc") - height)
  
  h <- breaks[-1] - breaks[-length(breaks)]
  
  rect <- rectGrob(x = 0,
                   y = breaks[-length(breaks)],
                   width = unit(10, "bigpts"),
                   height = h,
                   hjust = 0,
                   vjust = 0,
                   gp = gpar(fill = color, col = "#FFFFFF00"))
  
  text <- textGrob(names(legend),
                   x = unit(14, "bigpts"),
                   y = legendPos,
                   hjust = 0,
                   gp = gpar(...))
  
  res <- grobTree(rect, text)
  
  return(res)
}

.convertAnnotations <- function(annotation, annotationColors) {
  new <- annotation
  for (i in seq(ncol(annotation))) {
    a <- annotation[, i]
    b <- annotationColors[[colnames(annotation)[i]]]
    if (is.character(a) | is.factor(a)) {
      a <- as.character(a)
      
      if (length(setdiff(setdiff(a, NA), names(b))) > 0) {
        stop(sprintf("Factor levels on variable %s do not match
                     with annotationColors",
                     colnames(annotation)[i]))
      }
      new[, i] <- b[a]
    } else {
      a <- cut(a, breaks = 100)
      new[, i] <- colorRampPalette(b)(100)[a]
    }
  }
  return(as.matrix(new))
}

.drawAnnotations <- function(convertedAnnotations,
                             borderColor,
                             gaps,
                             fontSize,
                             horizontal) {
  
  n <- ncol(convertedAnnotations)
  m <- nrow(convertedAnnotations)
  
  coordX <- .findCoordinates(m, gaps)
  
  x <- coordX$coord - 0.5 * coordX$size
  
  # y = cumsum(rep(fontSize, n)) - 4 + cumsum(rep(2, n))
  y <- cumsum(rep(fontSize, n)) +
    cumsum(rep(2, n)) -
    fontSize / 2 + 1
  y <- unit(y, "bigpts")
  
  if (horizontal) {
    coord <- expand.grid(x = x, y = y)
    res <- rectGrob(x = coord$x,
                    y = coord$y,
                    width = coordX$size,
                    height = unit(fontSize, "bigpts"),
                    gp = gpar(fill = convertedAnnotations, col = borderColor))
  } else {
    a <- x
    x <- unit(1, "npc") - y
    y <- unit(1, "npc") - a
    
    coord <- expand.grid(y = y, x = x)
    res <- rectGrob(x = coord$x,
                    y = coord$y,
                    width = unit(fontSize, "bigpts"),
                    height = coordX$size,
                    gp = gpar(fill = convertedAnnotations, col = borderColor))
  }
  
  return(res)
}

.drawAnnotationNames <- function(annotations, fontSize, horizontal) {
  n <- ncol(annotations)
  
  x <- unit(3, "bigpts")
  
  y <- cumsum(rep(fontSize, n)) +
    cumsum(rep(2, n)) -
    fontSize / 2 + 1
  
  y <- unit(y, "bigpts")
  
  if (horizontal) {
    res <- textGrob(colnames(annotations),
                    x = x,
                    y = y,
                    hjust = 0,
                    gp = gpar(fontSize = fontSize, fontface = 2))
  } else {
    a <- x
    x <- unit(1, "npc") - y
    y <- unit(1, "npc") - a
    
    res <- textGrob(colnames(annotations),
                    x = x,
                    y = y,
                    vjust = 0.5,
                    hjust = 0,
                    rot = 270,
                    gp = gpar(fontSize = fontSize, fontface = 2))
  }
  
  return(res)
}

.drawAnnotationLegend <- function(annotation,
                                  annotationColors,
                                  borderColor,
                                  ...) {
  
  y <- unit(1, "npc")
  textHeight <- unit(1,
                     "grobheight",
                     textGrob("FGH", gp = gpar(...)))
  
  res <- gList()
  for (i in names(annotation)) {
    res[[i]] <- textGrob(i,
                         x = 0,
                         y = y,
                         vjust = 1,
                         hjust = 0,
                         gp = gpar(fontface = "bold", ...))
    
    y <- y - 1.5 * textHeight
    if (is.character(annotation[[i]]) |
        is.factor(annotation[[i]])) {
      n <- length(annotationColors[[i]])
      yy <- y - (seq(n) - 1) * 2 * textHeight
      
      res[[paste(i, "r")]] <- rectGrob(x = unit(0, "npc"),
                                       y = yy,
                                       hjust = 0,
                                       vjust = 1,
                                       height = 2 * textHeight,
                                       width = 2 * textHeight,
                                       gp = gpar(col = borderColor, fill = annotationColors[[i]]))
      
      res[[paste(i, "t")]] <- textGrob(names(annotationColors[[i]]),
                                       x = textHeight * 2.4,
                                       y = yy - textHeight,
                                       hjust = 0,
                                       vjust = 0.5,
                                       gp = gpar(...))
      
      y <- y - n * 2 * textHeight
    } else {
      yy <- y - 8 * textHeight + seq(0, 1, 0.25)[-1] * 8 * textHeight
      h <- 8 * textHeight * 0.25
      
      res[[paste(i, "r")]] <- rectGrob(x = unit(0, "npc"),
                                       y = yy,
                                       hjust = 0,
                                       vjust = 1,
                                       height = h,
                                       width = 2 * textHeight,
                                       gp = gpar(col = NA,
                                                 fill = colorRampPalette(annotationColors[[i]])(4)))
      res[[paste(i, "r2")]] <- rectGrob(x = unit(0, "npc"),
                                        y = y,
                                        hjust = 0,
                                        vjust = 1,
                                        height = 8 * textHeight,
                                        width = 2 * textHeight,
                                        gp = gpar(col = borderColor, fill = NA))
      
      txt <- rev(range(grid::grid.pretty(range(annotation[[i]],
                                               na.rm = TRUE))))
      
      yy <- y - c(1, 7) * textHeight
      res[[paste(i, "t")]] <- textGrob(txt,
                                       x = textHeight * 2.4,
                                       y = yy,
                                       hjust = 0,
                                       vjust = 0.5,
                                       gp = gpar(...))
      y <- y - 8 * textHeight
    }
    y <- y - 1.5 * textHeight
  }
  
  res <- gTree(children = res)
  
  return(res)
}

.drawMain <- function(text, ...) {
  res <- textGrob(text, gp = gpar(fontface = "bold", ...))
  
  return(res)
}

vplayout <- function(x, y) {
  return(viewport(layout.pos.row = x, layout.pos.col = y))
}

#' @importFrom gtable gtable_height
#' @importFrom gtable gtable_width
#' @importFrom gtable gtable_add_grob
#' @import grDevices
.heatmapMotor <- function(matrix,
                          borderColor,
                          cellWidth,
                          cellHeight,
                          treeCol,
                          treeRow,
                          treeHeightCol,
                          treeHeightRow,
                          fileName,
                          width,
                          height,
                          breaks,
                          color,
                          legend,
                          annotationRow,
                          annotationCol,
                          annotationColors,
                          annotationLegend,
                          annotationNamesRow,
                          annotationNamesCol,
                          main,
                          fontSize,
                          fontSizeRow,
                          fontSizeCol,
                          fmat,
                          fontSizeNumber,
                          numberColor,
                          gapsCol,
                          gapsRow,
                          labelsRow,
                          labelsCol,
                          ...) {
  # Set layout
  lo <- .lo(coln = labelsCol,
            rown = labelsRow,
            nrow = nrow(matrix),
            ncol = ncol(matrix),
            cellWidth = cellWidth,
            cellHeight = cellHeight,
            treeHeightCol = treeHeightCol,
            treeHeightRow = treeHeightRow,
            legend = legend,
            annotationCol = annotationCol,
            annotationRow = annotationRow,
            annotationColors = annotationColors,
            annotationLegend = annotationLegend,
            annotationNamesRow = annotationNamesRow,
            annotationNamesCol = annotationNamesCol,
            main = main,
            fontSize = fontSize,
            fontSizeRow = fontSizeRow,
            fontSizeCol = fontSizeCol,
            gapsRow = gapsRow,
            gapsCol = gapsCol,
            ...)
  
  res <- lo$gt
  mindim <- lo$mindim
  
  if (!is.na(fileName)) {
    if (is.na(height)) {
      height <- convertHeight(gtable::gtable_height(res),
                              "inches",
                              valueOnly = TRUE)
    }
    if (is.na(width)) {
      width <- convertWidth(gtable::gtable_width(res),
                            "inches",
                            valueOnly = TRUE)
    }
    
    # Get file type
    r <- regexpr("\\.[a-zA-Z]*$", fileName)
    if (r == -1)
      stop("Improper fileName")
    ending <- substr(fileName,
                     r + 1,
                     r + attr(r, "match.length"))
    
    f <- switch(ending,
                pdf = function(x, ...)
                  pdf(x, ...),
                png = function(x, ...)
                  png(x, units = "in",
                      res = 300, ...),
                jpeg = function(x, ...)
                  jpeg(x, units = "in",
                       res = 300, ...),
                jpg = function(x, ...)
                  jpeg(x, units = "in",
                       res = 300, ...),
                tiff = function(x, ...)
                  tiff(x,
                       units = "in",
                       res = 300,
                       compression = "lzw",
                       ...),
                bmp = function(x, ...)
                  bmp(x, units = "in",
                      res = 300, ...),
                stop("File type should be: pdf, png, bmp, jpg, tiff"))
    
    # print(sprintf("height:%f width:%f", height, width))
    
    # gt = .heatmapMotor(matrix,
    #     cellWidth = cellWidth,
    #     cellHeight = cellHeight,
    #     borderColor = borderColor,
    #     treeCol = treeCol,
    #     treeRow = treeRow,
    #     treeHeightCol = treeHeightCol,
    #     treeHeightRow = treeHeightRow,
    #     breaks = breaks,
    #     color = color,
    #     legend = legend,
    #     annotationCol = annotationCol,
    #     annotationRow = annotationRow,
    #     annotationColors = annotationColors,
    #     annotationLegend = annotationLegend,
    #     fileName = NA, main = main,
    #     fontSize = fontSize,
    #     fontSizeRow = fontSizeRow,
    #     fontSizeCol = fontSizeCol,
    #     fmat = fmat,
    #     fontSizeNumber =  fontSizeNumber,
    #     numberColor = numberColor,
    #     labelsRow = labelsRow,
    #     labelsCol = labelsCol,
    #     gapsCol = gapsCol,
    #     gapsRow = gapsRow, ...)
    
    f(fileName, height = height, width = width)
    gt <- .heatmapMotor(matrix,
                        cellWidth = cellWidth,
                        cellHeight = cellHeight,
                        borderColor = borderColor,
                        treeCol = treeCol,
                        treeRow = treeRow,
                        treeHeightCol = treeHeightCol,
                        treeHeightRow = treeHeightRow,
                        breaks = breaks,
                        color = color,
                        legend = legend,
                        annotationCol = annotationCol,
                        annotationRow = annotationRow,
                        annotationColors = annotationColors,
                        annotationLegend = annotationLegend,
                        annotationNamesRow = annotationNamesRow,
                        annotationNamesCol = annotationNamesCol,
                        fileName = NA,
                        main = main,
                        fontSize = fontSize,
                        fontSizeRow = fontSizeRow,
                        fontSizeCol = fontSizeCol,
                        fmat = fmat,
                        fontSizeNumber = fontSizeNumber,
                        numberColor = numberColor,
                        labelsRow = labelsRow,
                        labelsCol = labelsCol,
                        gapsCol = gapsCol,
                        gapsRow = gapsRow,
                        ...)
    grid.draw(gt)
    dev.off()
    
    return(gt)
  }
  
  # Omit border color if cell size is too small
  if (mindim < 3) {
    borderColor <- NA
  }
  
  # Draw title
  if (!is.na(main)) {
    elem <- .drawMain(main, fontSize = 1.3 * fontSize, ...)
    res <- gtable::gtable_add_grob(res,
                                   elem,
                                   t = 1,
                                   l = 3,
                                   name = "main",
                                   clip = "off")
  }
  
  # Draw tree for the columns
  if (!.is.na2(treeCol) & treeHeightCol != 0) {
    elem <- .drawDendrogram(treeCol, gapsCol, horizontal = TRUE)
    res <- gtable::gtable_add_grob(res,
                                   elem,
                                   t = 2,
                                   l = 3,
                                   name = "col_tree")
  }
  
  # Draw tree for the rows
  if (!.is.na2(treeRow) & treeHeightRow != 0) {
    elem <- .drawDendrogram(treeRow, gapsRow, horizontal = FALSE)
    res <- gtable::gtable_add_grob(res,
                                   elem,
                                   t = 4,
                                   l = 1,
                                   name = "row_tree")
  }
  
  # Draw matrix
  elem <- .drawMatrix(matrix,
                      borderColor,
                      gapsRow,
                      gapsCol,
                      fmat,
                      fontSizeNumber,
                      numberColor)
  
  res <- gtable::gtable_add_grob(res,
                                 elem,
                                 t = 4,
                                 l = 3,
                                 clip = "off",
                                 name = "matrix")
  
  # Draw colnames
  if (length(labelsCol) != 0) {
    pars <- list(labelsCol,
                 gaps = gapsCol,
                 fontSize = fontSizeCol,
                 ...)
    elem <- do.call(.drawColnames, pars)
    res <- gtable::gtable_add_grob(res,
                                   elem,
                                   t = 5,
                                   l = 3,
                                   clip = "off",
                                   name = "col_names")
  }
  
  # Draw rownames
  if (length(labelsRow) != 0) {
    pars <- list(labelsRow,
                 gaps = gapsRow,
                 fontSize = fontSizeRow, ...)
    elem <- do.call(.drawRownames, pars)
    res <- gtable::gtable_add_grob(res,
                                   elem,
                                   t = 4,
                                   l = 4,
                                   clip = "off",
                                   name = "row_names")
  }
  
  # Draw annotation tracks on cols
  if (!.is.na2(annotationCol)) {
    # Draw tracks
    convertedAnnotation <- .convertAnnotations(annotationCol,
                                               annotationColors)
    elem <- .drawAnnotations(convertedAnnotation,
                             borderColor,
                             gapsCol,
                             fontSize,
                             horizontal = TRUE)
    res <- gtable::gtable_add_grob(res,
                                   elem,
                                   t = 3,
                                   l = 3,
                                   clip = "off",
                                   name = "col_annotation")
    
    # Draw names
    if (annotationNamesCol) {
      elem <- .drawAnnotationNames(annotationCol,
                                   fontSize,
                                   horizontal = TRUE)
      res <- gtable::gtable_add_grob(res,
                                     elem,
                                     t = 3,
                                     l = 4,
                                     clip = "off",
                                     name = "col_annotation_names")
    }
  }
  
  # Draw annotation tracks on rows
  if (!.is.na2(annotationRow)) {
    # Draw tracks
    convertedAnnotation <- .convertAnnotations(annotationRow,
                                               annotationColors)
    elem <- .drawAnnotations(convertedAnnotation,
                             borderColor,
                             gapsRow,
                             fontSize,
                             horizontal = FALSE)
    res <- gtable::gtable_add_grob(res,
                                   elem,
                                   t = 4,
                                   l = 2,
                                   clip = "off",
                                   name = "row_annotation")
    
    # Draw names
    if (annotationNamesRow) {
      elem <- .drawAnnotationNames(annotationRow,
                                   fontSize,
                                   horizontal = FALSE)
      res <- gtable::gtable_add_grob(res,
                                     elem,
                                     t = 5,
                                     l = 2,
                                     clip = "off",
                                     name = "row_annotation_names")
    }
  }
  
  # Draw annotation legend
  annotation <- c(annotationCol[seq(length(annotationCol), 1)],
                  annotationRow[seq(length(annotationRow), 1)])
  annotation <- annotation[unlist(lapply(annotation,
                                         function(x) !.is.na2(x)))]
  
  if (length(annotation) > 0 & annotationLegend) {
    elem <- .drawAnnotationLegend(annotation,
                                  annotationColors,
                                  borderColor,
                                  fontSize = fontSize,
                                  ...)
    
    t <- ifelse(is.null(labelsRow), 4, 3)
    res <- gtable::gtable_add_grob(res,
                                   elem,
                                   t = t,
                                   l = 6,
                                   b = 5,
                                   clip = "off",
                                   name = "annotationLegend")
  }
  
  # Draw legend
  if (!.is.na2(legend)) {
    elem <- .drawLegend(color, breaks, legend, fontSize = fontSize, ...)
    
    t <- ifelse(is.null(labelsRow), 4, 3)
    res <- gtable::gtable_add_grob(res,
                                   elem,
                                   t = t,
                                   l = 5,
                                   b = 5,
                                   clip = "off",
                                   name = "legend")
  }
  
  return(res)
}

.generateBreaks <- function(x, n, center = FALSE) {
  if (center) {
    m <- max(abs(c(min(x, na.rm = TRUE),
                   max(x, na.rm = TRUE))))
    res <- seq(-m, m, length.out = n + 1)
  } else {
    res <- seq(min(x, na.rm = TRUE),
               max(x, na.rm = TRUE),
               length.out = n + 1)
  }
  
  return(res)
}

.scaleVecColours <- function(x, col = rainbow(10), breaks = NA) {
  return(col[as.numeric(cut(x, breaks = breaks, include.lowest = TRUE))])
}

.scaleColours <- function(mat,
                          col = rainbow(10),
                          breaks = NA) {
  mat <- as.matrix(mat)
  return(matrix(
    .scaleVecColours(as.vector(mat), col = col, breaks = breaks),
    nrow(mat),
    ncol(mat),
    dimnames = list(rownames(mat), colnames(mat))))
}

## changed the original clusterMat() in the pheatmap.r
#' @importFrom scales hue_pal
.clusterMat <- function(mat, labels, distance, method) {
  # this funciton is going to change the .clusterMat() in pheatmap
  
  if (!(method %in% c("ward.D",
                      "ward.D2",
                      "ward",
                      "single",
                      "complete",
                      "average",
                      "mcquitty",
                      "median",
                      "centroid"))) {
    stop("clustering method has to one form the list:
         'ward',
         'ward.D',
         'ward.D2',
         'single',
         'complete',
         'average',
         'mcquitty',
         'median'
         or 'centroid'.")
  }
  
  class.label <- unique(labels)
  
  nGroup <- length(class.label) # [#group]
  # get "hclust" object for each group then wrap them up as group.hclust
  
  # distance function preparation
  dis <- function(mat, distance) {
    if (!(distance[1] %in% c("correlation",
                             "euclidean",
                             "maximum",
                             "manhattan",
                             "canberra",
                             "binary",
                             "minkowski")) &
        !methods::is(distance, "dist")) {
      stop("distance has to be a dissimilarity structure as produced by",
           " dist or one measure  form the list: 'correlation',",
           " 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary',",
           " 'minkowski'")
    }
    
    if (distance[1] == "correlation") {
      # this part should be confirmed whether being wrong?
      #ToDo: how is the correlation matrix converted to a dsit matrix
      d <- stats::as.dist(1 - stats::cor(t(mat)))
    } else {
      d <- stats::dist(mat, method = distance)
    }
    
    return(d)
  }
  
  # initiate the final returning value: a "hclust" object
  cum.hclust <- list()
  
  if (nGroup == 1) {
    # matrix has only 1 group
    if (length(labels) == 1) {
      stop("only one row/column for the matrix")
    }
    group.hclust <- stats::hclust(dis(mat = mat,
                                      distance = distance),
                                  method = method)
    
    cum.hclust <- group.hclust
  } else {
    #  matrix has more than 1 groups
    group.hclust <- vapply(class.label, function(x) {
      # get the positions of class label
      class.pos <- which(labels == x)
      
      if (length(class.pos) == 1) {
        # if only 1 row in the group return a manually made "hclust"
        # object
        sub.hclust <- as.list(seq(7))
        names(sub.hclust) <- c("merge",
                               "height",
                               "order",
                               "labels",
                               "method",
                               "call",
                               "dist.method")
        
        class(sub.hclust) <- "hclust"
        sub.hclust$merge <- matrix(c(0, 0), nrow = 1)
        sub.hclust$height <- 0
        sub.hclust$order <- 1
        return(sub.hclust)
      } else if (length(class.pos) > 1) {
        # if >1 rows return the "hclust" object
        return(stats::hclust(dis(mat = mat[class.pos, ],
                                 distance = distance),
                             method = method))
      }
    },
    list("merge" = 0,
         "height" = 0,
         "order" = 0,
         "labels" = 0,
         "method" = 0,
         "call" = 0,
         "dist.method" = 0))
    # the length(group.hclust) is the [#group] == nGroup   ,
    # group.hclust[[i]] to get each "hclust"
    
    # then modify the "hclust" object and get them merged into one
    # "hclust" object
    
    # initiate the final "hclust" object
    cum.hclust <- group.hclust[, nGroup]
    
    # merge function preparation
    mergeHclust <- function(hclust1, hclust2) {
      # "hclust" object modifying function preparation
      if (hclust1$merge[1, 1] == 0 &
          hclust2$merge[1, 1] == 0) {
        # both groups have only 1 row
        hclustCom <- as.list(seq(7))
        names(hclustCom) <-
          c("merge",
            "height",
            "order",
            "labels",
            "method",
            "call",
            "dist.method")
        
        class(hclustCom) <- "hclust"
        hclustCom$merge <- matrix(c(-1, -2), nrow = 1)
        # check for different matrix whether 1 should be good
        hclustCom$height <- 1
        hclustCom$order <- c(1, 2)
        return(hclustCom)
      } else if (hclust1$merge[1, 1] != 0 &
                 hclust2$merge[1, 1] != 0) {
        # both group have >1 rows
        
        # nodes in the hclust1 group, so actually the #rows should
        # be dim()[1]+1
        row.1 <- dim(hclust1$merge)[1]
        # nodes in the hclust2 group
        row.2 <- dim(hclust2$merge)[1]
        hclustCom <- list()
        mer <- hclust2$merge
        # modify the hclust2$merge matrix
        hclustCom$merge <- (mer > 0) *
          (mer + row.1) + (mer < 0) *
          (mer - row.1 - 1)
        # combine the merge matrix from the 2 groups
        hclustCom$merge <- rbind(hclust1$merge,
                                 hclustCom$merge)
        hclustCom$merge <- rbind(hclustCom$merge,
                                 c(row.1, row.1 + row.2))
        hclustCom$height <- c(hclust1$height, hclust2$height)
        # check for different matrix whether 1 should be good
        hclustCom$height <- c(hclustCom$height,
                              max(hclustCom$height) + 1)
        hclustCom$order <- c(hclust1$order,
                             hclust2$order + row.1 + 1)
        class(hclustCom) <- "hclust"
      } else {
        # one group has only 1 row, the other group has >1 rows
        if (hclust1$merge[1, 1] == 0) {
          # hclust1 has 1 row , hclust2 has >1 rows
          
          # nodes in the hclust2 group
          row.2 <- dim(hclust2$merge)[1]
          hclustCom <- as.list(seq(7))
          names(hclustCom) <- c("merge",
                                "height",
                                "order",
                                "labels",
                                "method",
                                "call",
                                "dist.method")
          class(hclustCom) <- "hclust"
          mer <- hclust2$merge
          hclustCom$merge <- (mer > 0) *
            (mer) +
            (mer < 0) *
            (mer - 1)
          hclustCom$merge <- rbind(hclustCom$merge,
                                   c(-1, row.2))
          # check for different matrix whether 1 should be good
          hclustCom$height <- c(hclust2$height,
                                max(hclust2$height) + 1)
          hclustCom$order <- c(1, hclust2$order + 1)
        } else if (hclust2$merge[1, 1] == 0) {
          # the hclust1 has >1 rows , and hclust2 has 1 row
          
          #nodes in the hclust1 group
          row.1 <- dim(hclust1$merge)[1]
          hclustCom <- as.list(seq(1, 7))
          names(hclustCom) <-
            c("merge",
              "height",
              "order",
              "labels",
              "method",
              "call",
              "dist.method")
          class(hclustCom) <- "hclust"
          hclustCom$merge <- hclust1$merge
          hclustCom$merge <- rbind(hclustCom$merge,
                                   c(row.1, - (row.1 + 2)))
          hclustCom$height <- c(hclust1$height,
                                max(hclust1$height) + 1)
          hclustCom$order <- c(hclust1$order,
                               max(hclust1$order) + 1)
        }
      }
      
      return(hclustCom)
    }
    
    # merge the "hclust" object into the final one "hclust" object
    for (i in seq(nGroup - 1, 1, -1)) {
      cum.hclust <- mergeHclust(group.hclust[, i], cum.hclust)
    }
  }
  
  cum.hclust$labels <- NULL
  cum.hclust$call <- NULL
  cum.hclust$method <- NULL
  cum.hclust$dist.method <- NULL
  
  return(cum.hclust)
}

.scaleRows <- function(x) {
  m <- base::apply(x, 1, mean, na.rm = TRUE)
  s <- base::apply(x, 1, stats::sd, na.rm = TRUE)
  return((x - m) / s)
}

.scaleMat <- function(mat, scale) {
  if (!(scale %in% c("none", "row", "column"))) {
    stop("scale argument shoud take values: 'none', 'row' or 'column'")
  }
  mat <- switch(scale,
                none = mat,
                row = .scaleRows(mat),
                column = t(.scaleRows(t(mat))))
  return(mat)
}

#' @importFrom scales dscale
#' @importFrom scales brewer_pal
.generateAnnotationColours <- function(annotation,
                                       annotationColors,
                                       drop) {
  
  if (.is.na2(annotationColors)) {
    annotationColors <- list()
  }
  count <- 0
  for (i in seq(length(annotation))) {
    annotation[[i]] <- annotation[[i]][!is.na(annotation[[i]])]
    if (is.character(annotation[[i]]) |
        is.factor(annotation[[i]])) {
      if (is.factor(annotation[[i]]) & !drop) {
        count <- count + length(levels(annotation[[i]]))
      } else {
        count <- count + length(unique(annotation[[i]]))
      }
    }
  }
  
  factorColors <- scales::dscale(factor(seq(1, count)),
                                 scales::hue_pal(l = 75))
  
  contCounter <- 2
  for (i in seq(length(annotation))) {
    if (!(names(annotation)[i] %in% names(annotationColors))) {
      if (is.character(annotation[[i]]) |
          is.factor(annotation[[i]])) {
        n <- length(unique(annotation[[i]]))
        
        if (is.factor(annotation[[i]]) & !drop) {
          n <- length(levels(annotation[[i]]))
        }
        
        ind <- sample(seq_along(factorColors), n)
        annotationColors[[names(annotation)[i]]] <-
          factorColors[ind]
        l <- levels(as.factor(annotation[[i]]))
        l <- l[l %in% unique(annotation[[i]])]
        if (is.factor(annotation[[i]]) & !drop) {
          l <- levels(annotation[[i]])
        }
        
        names(annotationColors[[names(annotation)[i]]]) <- l
        factorColors <- factorColors[-ind]
      } else {
        annotationColors[[names(annotation)[i]]] <-
          scales::brewer_pal("seq", contCounter)(5)[seq(4)]
        contCounter <- contCounter + 1
      }
    }
  }
  return(annotationColors)
}


.findGaps <- function(tree, cutreeN) {
  v <- stats::cutree(tree, cutreeN)[tree$order]
  gaps <- which((v[-1] - v[-length(v)]) != 0)
  return(gaps)
}

.is.na2 <- function(x) {
  if (is.list(x) | length(x) > 1) {
    return(FALSE)
  }
  if (length(x) == 0) {
    return(TRUE)
  }
  
  return(is.na(x))
}

.identity2 <- function(x, ...) {
  return(x)
}
semiPheatmap <- function(mat,
                         color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
                         kmeansK = NA,
                         breaks = NA,
                         borderColor = "grey60",
                         cellWidth = NA,
                         cellHeight = NA,
                         scale = "none",
                         clusterRows = TRUE,
                         clusterCols = TRUE,
                         clusteringDistanceRows = "euclidean",
                         clusteringDistanceCols = "euclidean",
                         clusteringMethod = "complete",
                         clusteringCallback = .identity2,
                         cutreeRows = NA,
                         cutreeCols = NA,
                         treeHeightRow = ifelse(clusterRows, 50, 0),
                         treeHeightCol = ifelse(clusterCols, 50, 0),
                         legend = TRUE,
                         legendBreaks = NA,
                         legendLabels = NA,
                         annotationRow = NA,
                         annotationCol = NA,
                         annotation = NA,
                         annotationColors = NA,
                         annotationLegend = TRUE,
                         annotationNamesRow = TRUE,
                         annotationNamesCol = TRUE,
                         dropLevels = TRUE,
                         showRownames = TRUE,
                         showColnames = TRUE,
                         main = NA,
                         fontSize = 10,
                         fontSizeRow = fontSize,
                         fontSizeCol = fontSize,
                         displayNumbers = FALSE,
                         numberFormat = "%.2f",
                         numberColor = "grey30",
                         fontSizeNumber = 0.8 * fontSize,
                         gapsRow = NULL,
                         gapsCol = NULL,
                         labelsRow = NULL,
                         labelsCol = NULL,
                         fileName = NA,
                         width = NA,
                         height = NA,
                         silent = FALSE,
                         rowLabel,
                         colLabel,
                         ...) {
  
  # Set labels
  if (is.null(labelsRow) & !is.null(rownames(mat))) {
    labelsRow <- rownames(mat)
  }
  if (is.null(labelsRow) & is.null(rownames(mat))) {
    labelsRow <- seq(nrow(mat))
    rownames(mat) <- seq(nrow(mat))
  }
  
  if (is.null(labelsCol) & !is.null(colnames(mat))) {
    labelsCol <- colnames(mat)
  }
  if (is.null(labelsCol) & is.null(colnames(mat))) {
    labelsCol <- seq(ncol(mat))
    colnames(mat) <- seq(ncol(mat))
  }
  
  
  if (.is.na2(breaks)) {
    breaks <- .generateBreaks(mat, length(color), center = TRUE)
  }
  
  
  # Kmeans
  if (!is.na(kmeansK)) {
    # Cluster data
    km <- stats::kmeans(mat, kmeansK, iter.max = 100)
    mat <- km$centers
    
    # Compose rownames
    t <- table(km$cluster)
    labelsRow <- sprintf("Cluster: %s Size: %d", names(t), t)
  } else {
    km <- NA
  }
  
  # Format numbers to be displayed in cells
  if (is.matrix(displayNumbers) | is.data.frame(displayNumbers)) {
    if (nrow(displayNumbers) != nrow(mat) |
        ncol(displayNumbers) != ncol(mat)) {
      stop("If displayNumbers provided as matrix,
           its dimensions have to match with mat")
    }
    
    displayNumbers <- as.matrix(displayNumbers)
    fmat <- matrix(as.character(displayNumbers),
                   nrow = nrow(displayNumbers),
                   ncol = ncol(displayNumbers))
    fmatDraw <- TRUE
  } else {
    if (displayNumbers) {
      fmat <- matrix(sprintf(numberFormat, mat),
                     nrow = nrow(mat),
                     ncol = ncol(mat))
      fmatDraw <- TRUE
    } else {
      fmat <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
      fmatDraw <- FALSE
    }
  }
  
  # Do clustering for rows
  if (clusterRows == TRUE) {
    if (is.null(rowLabel)) {
      rowLabel <- rep(1, nrow(mat))
    } else {
      o <- order(rowLabel)
      mat <- mat[o, , drop = FALSE]
      fmat <- fmat[o, , drop = FALSE]
      rowLabel <- rowLabel[o]
      if (!is.na(annotationRow)) {
        annotationRow <- annotationRow[o, , drop = FALSE]
      }
    }
    
    treeRow <- .clusterMat(mat,
                           rowLabel,
                           distance = clusteringDistanceRows,
                           method = clusteringMethod)
    treeRow <- clusteringCallback(treeRow, mat)
    
    mat <- mat[treeRow$order, , drop = FALSE]
    fmat <- fmat[treeRow$order, , drop = FALSE]
    labelsRow <- labelsRow[treeRow$order]
    if (!is.na(annotationRow)){
      annotationRow <- annotationRow[treeRow$order, , drop=FALSE]
    }
    
    if (!is.na(cutreeRows)) {
      gapsRow <- .findGaps(treeRow, cutreeRows)
    } else {
      gapsRow <- NULL
    }
  } else {
    treeRow <- NA
    treeHeightRow <- 0
  }
  
  
  ## Do clustering for columns
  if (clusterCols == TRUE) {
    if (is.null(colLabel)) {
      colLabel <- rep(1, ncol(mat))
    } else {
      o <- order(colLabel)
      mat <- mat[, o, drop = FALSE]
      fmat <- fmat[, o, drop = FALSE]
      colLabel <- colLabel[o]
      if (!is.na(annotationCol)) {
        annotationCol <- annotationCol[o, , drop = FALSE]
      }
    }
    
    treeCol <- .clusterMat(t(mat),
                           colLabel,
                           distance = clusteringDistanceCols,
                           method = clusteringMethod)
    treeCol <- clusteringCallback(treeCol, t(mat))
    
    mat <- mat[, treeCol$order, drop = FALSE]
    fmat <- fmat[, treeCol$order, drop = FALSE]
    labelsCol <- labelsCol[treeCol$order]
    if (!is.na(annotationCol)) {
      annotationCol <- annotationCol[treeCol$order, , drop = FALSE]
    }
    
    if (!is.na(cutreeCols)) {
      gapsCol <- .findGaps(treeCol, cutreeCols)
    } else {
      gapsCol <- NULL
    }
  } else {
    treeCol <- NA
    treeHeightCol <- 0
  }
  
  attr(fmat, "draw") <- fmatDraw
  
  # Colors and scales
  if (!.is.na2(legendBreaks) & !.is.na2(legendLabels)) {
    if (length(legendBreaks) != length(legendLabels)) {
      stop("Lengths of legendBreaks and legendLabels must be the same")
    }
  }
  
  
  if (.is.na2(breaks)) {
    breaks <- .generateBreaks(as.vector(mat), length(color))
  }
  if (legend & .is.na2(legendBreaks)) {
    legend <- grid::grid.pretty(range(as.vector(breaks)))
    names(legend) <- legend
  }
  else if (legend & !.is.na2(legendBreaks)) {
    legend <- legendBreaks[legendBreaks >= min(breaks) &
                             legendBreaks <= max(breaks)]
    
    if (!.is.na2(legendLabels)) {
      legendLabels <- legendLabels[legendBreaks >= min(breaks) &
                                     legendBreaks <= max(breaks)]
      names(legend) <- legendLabels
    } else {
      names(legend) <- legend
    }
  } else {
    legend <- NA
  }
  mat <- .scaleColours(mat, col = color, breaks = breaks)
  
  annotation <- c(annotationRow, annotationCol)
  annotation <- annotation[unlist(lapply(annotation,
                                         function(x) !.is.na2(x)))]
  if (length(annotation) != 0) {
    annotationColors <- .generateAnnotationColours(annotation,
                                                   annotationColors,
                                                   drop = dropLevels)
  } else {
    annotationColors <- NA
  }
  
  labelsRow <- rownames(mat)
  labelsCol <- colnames(mat)
  
  if (!showRownames) {
    labelsRow <- NULL
  }
  
  if (!showColnames) {
    labelsCol <- NULL
  }
  
  # Draw heatmap
  gt <- .heatmapMotor(mat,
                      borderColor = borderColor,
                      cellWidth = cellWidth,
                      cellHeight = cellHeight,
                      treeHeightCol = treeHeightCol,
                      treeHeightRow = treeHeightRow,
                      treeCol = treeCol,
                      treeRow = treeRow,
                      fileName = fileName,
                      width = width,
                      height = height,
                      breaks = breaks,
                      color = color,
                      legend = legend,
                      annotationRow = annotationRow,
                      annotationCol = annotationCol,
                      annotationColors = annotationColors,
                      annotationLegend = annotationLegend,
                      annotationNamesRow = annotationNamesRow,
                      annotationNamesCol = annotationNamesCol,
                      main = main,
                      fontSize = fontSize,
                      fontSizeRow = fontSizeRow,
                      fontSizeCol = fontSizeCol,
                      fmat = fmat,
                      fontSizeNumber = fontSizeNumber,
                      numberColor = numberColor,
                      gapsRow = gapsRow,
                      gapsCol = gapsCol,
                      labelsRow = labelsRow,
                      labelsCol = labelsCol,
                      ...)
  
  if (is.na(fileName) & !silent) {
    grid.newpage()
    grid.draw(gt)
  }
  
  invisible(list(treeRow = treeRow,
                 treeCol = treeCol,
                 gtable = gt))
}
