#' Create a surface plot for any function
#'
#' @param xlim
#' @param ylim
#' @param fun
#' @param colors
#' @param showscale
#' @param showlegend
#' @param xlab
#' @param ylab
#' @param zlab
#'
#' @return
#' @export
#'
#' @examples
surface = function(expr, xlim = c(0,20), ylim = c(0,20),
                   colors = c("#ff6c59", "#ffd859"),
                   showscale = FALSE, showlegend = FALSE,
                   xlab = "x", ylab = "y", zlab = "z",
                   n = 100){

  sexpr = substitute(expr)

  f = function(x,y){
    eval(sexpr)
  }

  x.seq = seq(xlim[1], xlim[2], length.out = n)
  y.seq = seq(ylim[1], ylim[2], length.out = n)
  z = t(outer(x.seq, y.seq, function(x,y) f(x,y)))

  surf = plotly::plot_ly(x = x.seq, y = y.seq, z = z, type = "surface", colors = colors,
                         showscale = showscale, showlegend = showlegend)

  out_plot = plotly::layout(surf, scene = list(xaxis = list(title = xlab),
                                               yaxis = list(title = ylab),
                                               zaxis = list(title = zlab)))

  out_plot
}
