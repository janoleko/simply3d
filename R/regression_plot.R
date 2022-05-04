#' Create a scatterplot with the added regression function
#'
#' @param mod
#' @param xvar
#' @param yvar
#' @param zvar
#' @param data
#' @param colors
#' @param showscale
#' @param showlegend
#' @param xlab
#' @param ylab
#' @param zlab
#' @param n
#'
#' @return
#' @export
#'
#' @examples
regression_plot = function(mod, data, colors = c("#ff6c59", "#ffd859"), showscale = FALSE, showlegend = FALSE, n = 50){

  f = function(x,y){
    df = as.data.frame(cbind(x,y))
    colnames(df) = c(names(coef(mod))[2], names(coef(mod))[3])
    predict(mod, newdata = df, type = "response")
  }

  x.data = data[,which(colnames(data) == names(coef(mod))[2])]
  y.data = data[,which(colnames(data) == names(coef(mod))[3])]
  z.data = data[,which(colnames(data) == formula(mod)[[2]])]

  x.seq = seq(min(na.omit(x.data)), max(na.omit(x.data)), length.out = n)
  y.seq = seq(min(na.omit(y.data)), max(na.omit(y.data)), length.out = n)
  z = t(outer(x.seq, y.seq, function(x,y) f(x,y)))

  surf = plotly::plot_ly(x = x.seq, y = y.seq, z = z, type = "surface", colors = colors,
                     showscale = showscale, showlegend = showlegend)
  scatter = plotly::add_trace(surf, data = data, x = x.data, y = y.data, z = z.data, mode = "markers", type = "scatter3d",
              marker = list(color = "#000000", opacity = 0.9, size = 4))
  out_plot = plotly::layout(scatter, scene = list(
                        xaxis = list(title = names(coef(mod))[2]),
                        yaxis = list(title = names(coef(mod))[3]),
                        zaxis = list(title = as.character(formula(mod)[[2]]))))
  out_plot
}
