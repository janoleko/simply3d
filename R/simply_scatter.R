#' Dynamic 3D Scatterplot
#'
#' @param x x-variable
#' @param y y-variable
#' @param z z-variable
#' @param size Pointsize
#' @param color Pointcolor (either a single color or a vector the same length of the data)
#' @param opacity Opacity of the points
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param zlab z-axis label
#' @param showscale Option for showing or hiding the colorscale
#' @param showlegend Option for showing or hiding the legend
#'
#' @return Returns a interactive 3D scatter plot
#' @export
#'
#' @examples
#'
#' ## Example 1: Simple scatterplot
#'
#' simply_scatter(mtcars$hp, mtcars$cyl, mtcars$mpg, xlab = "hp", ylab = "cyl", zlab = "mpg")
#'
#'
#' ## Example 2: Coloring data points differently according to a color variable
#'
#' colorvariable = rep("cornflowerblue", nrow(mtcars))
#' colorvariable[which(mtcars$vs == 1)] = "orange"
#'
#' simply_scatter(mtcars$hp, mtcars$cyl, mtcars$mpg,
#'   color = colorvariable, xlab = "hp", ylab = "cyl", zlab = "mpg")
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

simply_scatter = function(x, y, z, size = 5, color = "#000000",
                          opacity = 0.9, xlab = "x", ylab = "y", zlab = "z",
                          showscale = FALSE, showlegend = FALSE){

  if(length(color) == 1 | length(color) == length(x)){
    data = as.data.frame(cbind(x, y, z))

    fig = plotly::plot_ly(data)
    scatter = plotly::add_trace(fig, data = data, x = ~x, y = ~y, z = ~z, mode = "markers", type = "scatter3d",
                          marker = list(color = color, opacity = 0.9, size = size))
    out = plotly::layout(scatter, scene = list(xaxis = list(title = xlab),
                       yaxis = list(title = ylab),
                       zaxis = list(title = zlab)))
  } else{
    warning("Argument color either needs to be a color name or a vector the same length as x, y and z to color each point in the plot.")
  }

  out
}
