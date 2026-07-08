#' Compare two sets and report differences and intersection
#'
#' Computes the elements that are in `x` but not `y`, in `y` but not `x`,
#' their intersection, and their symmetric difference. The function prints
#' summary counts and the corresponding elements.
#'
#' @param x A vector or set-like object.
#' @param y A vector or set-like object.
#'
#' @return Invisibly returns a list with the following components:
#' \describe{
#'   \item{in_x_and_not_y}{Elements present in `x` but not in `y`.}
#'   \item{in_y_and_not_x}{Elements present in `y` but not in `x`.}
#'   \item{xy_intersection}{Elements common to both `x` and `y`.}
#'   \item{symmetric_diff}{Elements in either `x` or `y` but not both.}
#' }
#'
#' @examples
#' set_difff(1:5, 4:8)
#'
#' @export
set_difff = function(x,y){
#' Function to find differences in two sets along with intersection
#'

in_x_and_not_y = setdiff(x,y)
in_y_and_not_x = setdiff(y,x)
xy_intersection = intersect(x,y)
symmetric_diff = c(in_x_and_not_y,in_y_and_not_x)

message = cat(
  "in_x_and_not_y length =",length(in_x_and_not_y),"\n",
  "in_y_and_not_x length =",length(in_y_and_not_x),"\n",
  "xy_intersection length =",length(xy_intersection),"\n" ,
  "symmetric_diff length =",length(symmetric_diff),"\n" ,
  "in_x_and_not_y:", in_x_and_not_y,"\n",
  "in_y_and_not_x:", in_y_and_not_x,"\n",
  "xy_intersection:", xy_intersection,"\n",
  "symmetric_diff:", symmetric_diff,"\n"
)

print(message)
}
