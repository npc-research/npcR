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
