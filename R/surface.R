#' Create a 3D surface plot for any function
#'
#' @param expr An expression or a function containing x and y as variables
#' @param xlim A vector of limits for the x-axis
#' @param ylim A vector of limits for the y-axis
#' @param colors A vector of two colors (can also be the same)
#' @param showscale Option for showing the colorscale
#' @param showlegend Option for showing the legend
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param zlab y-axis label
#' @param n number of points where function is evalutated (actually n^2)
#'
#' @return Returns a interactive 3D surface plot
#' @export
#'
#' @examples
surface = function(expr, xlim = c(0,40), ylim = c(0,40),
                   colors = c("#ff6c59", "#ffd859"),
                   showscale = FALSE, showlegend = FALSE,
                   xlab = "x", ylab = "y", zlab = "z",
                   n = 100){

  sexpr = substitute(expr)

  x.seq = seq(xlim[1], xlim[2], length.out = n)
  y.seq = seq(ylim[1], ylim[2], length.out = n)

  if (is.name(sexpr)){ # checking if expression is a function
    error = tryCatch(expr(x.seq[1:2], y.seq[1:2]), error = function(e) e) # checking if function is vectorizable
    if(methods::is(error, "error")){
      f_scalar = expr
      f = function(x,y){ # making f vectorizable
        K = length(x)
        ret = numeric(K)
        for (k in 1:K){
          ret[k] = f_scalar(x[k], y[k])
        }
        return(ret)
      }
    }
    else f = expr
  }
  else{
    f = function(x,y){
    eval(sexpr)
    }
  }

  z = t(outer(x.seq, y.seq, function(x,y) f(x,y)))

  surf = plotly::plot_ly(x = x.seq, y = y.seq, z = z, type = "surface", colors = colors,
                         showscale = showscale, showlegend = showlegend)

  out_plot = plotly::layout(surf, scene = list(xaxis = list(title = xlab),
                                               yaxis = list(title = ylab),
                                               zaxis = list(title = zlab)))

  out_plot
}
