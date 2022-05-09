#' Title
#'
#' @param x
#' @param y
#' @param z
#' @param col
#' @param colors
#' @param xlab
#' @param ylab
#' @param zlab
#'
#' @return
#' @export
#'
#' @examples
simply_scatter = function(x, y, z, size = 5, color = "#000000",
                          colorvar = NULL, colors = c("red", "blue"), opacity = 0.9, xlab = "x", ylab = "y", zlab = "z",
                          showscale = FALSE, showlegend = FALSE){


  if (is.null(colorvar)){
    data = as.data.frame(cbind(x, y, z))

    fig = plotly::plot_ly(data)
    scatter = plotly::add_trace(fig, data = data, x = ~x, y = ~y, z = ~z, mode = "markers", type = "scatter3d",
                          marker = list(color = color, opacity = 0.9, size = size))
    out = plotly::layout(scatter, scene = list(xaxis = list(title = xlab),
                       yaxis = list(title = ylab),
                       zaxis = list(title = zlab)))
  }

  else{
    data = as.data.frame(cbind(x, y, z, colorvar))

    fig = plotly::plot_ly(data, showlegend = FALSE)
    scatter = plotly::add_trace(fig, data = data, x = ~x, y = ~y, z = ~z, mode = "markers", type = "scatter3d",
                            color = ~colorvar, colors = colors,
                            marker = list(opacity = 0.9, size = size))
    out = plotly::layout(scatter, scene = list(xaxis = list(title = xlab),
                         yaxis = list(title = ylab),
                         zaxis = list(title = zlab)))
  }

  out
}
