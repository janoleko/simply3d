#' Create a 3D surface plot for any function
#'
#' @param fig Optional: A scatterplot onto which to plot the surface. If provided please also provide the x and y-variables used for the scatterplot. If not provided please specify the xlim and ylim arguments for the plot.
#' @param expr An expression or a function containing x and y as variables.
#' @param x x-variable used for the scatterplot if surface should be plotted ontop (Only needs to be provided if fig is provided).
#' @param y y-variable used for the scatterplot if surface should be plotted ontop (Only needs to be provided if fig is provided).
#' @param xlim A vector of limits for the x-axis.
#' @param ylim A vector of limits for the y-axis.
#' @param colors A vector of two colors (can also be the same) for the color gradient.
#' @param showscale Option for showing or hiding the colorscale
#' @param showlegend Option for showing or hiding the legend
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param zlab z-axis label
#' @param n number of points in each dimension where function is evaluated. The function will be evaluated n^2 times.
#'
#' @return Returns a interactive 3D surface plot, either only the surface or the surface plotted ontop of a scatterplot if the scatterplot is provided as the fig argument.
#' @export
#'
#' @examples
#'
#' ## Example 1: Creating a surface plot from a simple expression
#'
#' surface(expr = 4*x + 3*y + 0.5*x*y)
#'
#' # changing the colors
#' surface(expr = 4*x + 3*y + 0.5*x*y, color = c("green", "blue"))
#'
#'
#' ## Example 2: Creating a surface plot from a predefined function
#'
#' f = function(x,y){
#' -x*y*exp(-x^2-y^2)
#' }
#'
#' # and increasing n for a higher resolution (as function has high curvature).
#' # Also changing xlim and ylim.
#' surface(expr = f, xlim = c(-5,5), ylim = c(-5,5), n = 200)
#'
#'
#' ## Example 3: Adding a surface to a regression plot (e.g. to plot a linear regression)
#'
#' # generating data
#' x = rnorm(1000, 10, 20)
#' y = rnorm(1000, 20, 20)
#' z = 4 + 0.3*x + 0.2*y + rnorm(1000, 0, 4)
#'
#' # plotting scatterplot with simply_scatter
#' scatter = simply_scatter(x,y,z)
#'
#' # fitting a linear regression model
#' mod = lm(z ~ x + y)
#'
#' # adding model plane into scatterplot
#' surface(fig = scatter, expr = coef(mod)[1] + coef(mod)[2]*x + coef(mod)[3]*y, x = x, y = y)
#'
#' # you can also use the pipe operator
#' require(dplyr)
#' simply_scatter(x,y,z) %>%
#'   surface(expr = coef(mod)[1] + coef(mod)[2]*x + coef(mod)[3]*y, x = x, y = y)
#'
#' # for more complex models defining an expression as above might be tedious.
#' # You can also just define your model prediction as a function
#' f = function(x,y){
#' predict(mod, newdata = data.frame(x,y))
#' }
#'
#' surface(fig = scatter, expr = f, x = x, y = y)

surface = function(fig = NULL, expr, x = NULL, y = NULL, xlim = c(0,40), ylim = c(0,40),
                   colors = c("#ff6c59", "#ffd859"),
                   showscale = TRUE, showlegend = FALSE,
                   xlab = "x", ylab = "y", zlab = "z",
                   n = 50){

  if(!is.null(x) & !is.null(y) & !is.null(fig)){
    xlim = c(min(x, na.rm = T), max(x, na.rm = T))
    ylim = c(min(y, na.rm = T), max(y, na.rm = T))
  }

  sexpr = substitute(expr)

  x.seq = seq(xlim[1], xlim[2], length.out = n)
  y.seq = seq(ylim[1], ylim[2], length.out = n)

  if(is.name(sexpr)){ # checking if expression is a function
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
    } else f = expr
  } else{
    f = function(x,y){
    eval(sexpr)
    }
  }

  z = t(outer(x.seq, y.seq, function(x,y) f(x,y)))

  if(is.null(fig)){
    surf = plotly::plot_ly(x = x.seq, y = y.seq, z = z, type = "surface", colors = colors,
                           showscale = showscale, showlegend = showlegend)

    out_plot = plotly::layout(surf, scene = list(xaxis = list(title = xlab),
                                                 yaxis = list(title = ylab),
                                                 zaxis = list(title = zlab)))
  } else{
    out_plot = plotly::add_surface(fig, x = x.seq, y = y.seq, z = z, colorscale = list(c(0,1), colors),
                                   showscale = showscale, showlegend = showlegend)
  }

  out_plot
}
