library(stringr)
library(sf)
library(ggplot2)
library(dplyr)

### UTILITY FUNCTIONS ###

# Check that the d attribute string of path can be processed
simplicity_check <- function(dstring){
  first_chr <- substr(d_string,1,1)
  last_chr <- substr(d_string, nchar(d_string), nchar(d_string))

  has_problem <- FALSE
  p_messages <- c()

  # Check that it begins with m and ends with z
  simple_and_closed <- identical(first_chr, "m") & identical(last_chr, "z")
  if(!simple_and_closed){
    has_problem <- TRUE
    p_messages <- append(p_messages,"path needs to start with m and end with z")
  }

  # Check that path doesn't contain more than one move to command
  internal_m_count <- length(grep("[Mm]", d_string))
  if(internal_m_count > 1){
    has_problem <- TRUE
    p_messages <- append(p_messages, "path has more than one m component")
  }

  # TODO Check only linear (mlLHvz only)

  if(has_problem){ cat(p_messages)}
  return(has_problem)
}

# Convert path description to list of parameters labeled with the name of the command
path_desc_to_cmdlist <- function(dstring){
  cmd_names <- str_extract_all(dstring, '\\p{Alphabetic}')[[1]]
  cmd_params <- str_split(dstring, '\\p{Alphabetic}', simplify = F)[[1]]

  # Remove str_split artifact: empty part of dstring prior to first command letter
  cmd_params_t <- as.list(cmd_params[-1])

  # Trim leading and trailing whitespace
  cmd_params_t <- lapply(cmd_params_t, trimws)

  # Add names
  names(cmd_params_t) <- cmd_names
  return(cmd_params_t)
}

# convert vector of x and y strings values to numeric and apply names
convert_xy_str <- function(str_vec){
  xy <- sapply(str_vec, as.numeric, USE.NAMES = F)
  names(xy) <- c('x','y')
  xy
}

# convert list of xy param strings into list of named numerics
convert_xy_params <- function(cmd_params){
  # split into list of "x,y" strings
  pair_str <- str_split(cmd_params, " ")[[1]]

  # split int list of vectors of "x" and "y" strings
  param_xy_str <- lapply(pair_str, function(x){str_split(x, ",")[[1]]} )

  param_xy <- lapply(param_xy_str, convert_xy_str )
}

# Convert list of xy values to xy coordinate data frame
frame_xy <- function(list_xy){ data.frame(x=sapply(list_xy,`[[`,1), y=sapply(list_xy,`[[`,2)) }

# get deltas dataframe from m command parameters
get_m_deltas <- function(param_xy){param_xy[-1]}

# get start point in dataframe from m command parameters
get_m_start <- function(param_xy){param_xy[1]}

# Given start dataframe and deltas list, emit data frame of x & y coords
# Corresponds to SVG's path 'l'
process_rel_l <- function(start, cmd_params){
  params_xy <- convert_xy_params(cmd_params)
  process_rel_l_internal(start, params_xy)
}

process_rel_l_internal <- function(start, params){
  params_df <- frame_xy(params)
  origin <- start[nrow(start), c("x","y")]
  points_x <- origin$x + cumsum(params_df$x)
  points_y <- origin$y + cumsum(params_df$y)
  data.frame(x=points_x, y=points_y)
}

# Given a list of points, emit data frame of x & y coords
# Corresponds to SVG's path 'L'
process_abs_l <- function(cmd_params){
  params <- convert_xy_params(cmd_params)
  frame_xy(params)
}

# Given emit data frame of x&y coords for params.  Start is ignored.
# Corresponds to SVG path 'm'
process_m <- function(start, cmd_params){
  # convert params to numeric xy pairs
  params <- convert_xy_params(cmd_params)
  origin <- frame_xy(get_m_start(params))

  # get x and y deltas, for m, all non-origin parameters
  deltas <- get_m_deltas(params)

  # process deltas as l command
  extra_coords <- process_rel_l_internal(origin, deltas)

  # Return origin and subsequent coordinates
  rbind(origin, extra_coords)
}

process_abs_v <- function(start, cmd_params){
  origin <- start[nrow(start), c("x","y")]
  data.frame(x=origin$x, y=sapply(cmd_params, as.numeric, USE.NAMES = F))
}

process_abs_h <- function(start, cmd_params){
  origin <- start[nrow(start), c("x","y")]
  data.frame(x=sapply(cmd_params, as.numeric, USE.NAMES = F), y=origin$y)
}

# Return the first point of the start coordinates dataframe
process_z <- function(start, params){
  start[1, c("x","y")]
}

# Given single letter dstring command name, return associated function
lookup_function <- function(cmd_name){
  function_name <- switch(cmd_name,
                          m="process_m",
                          l="process_rel_l",
                          L="process_abs_l",
                          h="process_rel_h",
                          H="process_abs_h",
                          v="process_rel_v",
                          V="process_abs_v",
                          z="process_z",
                          "unhandled")

  if(identical(function_name, "unrecognized")){
    rlang::abort(paste0("unhandled path command: ",cmd_name))
  }else{
    return(function_name)
  }
}

# convert dstring to dataframe of x&y coordinates
dstring_to_svg_coords <- function(d_string){
  cmd_list <- path_desc_to_cmdlist(d_string)
  fun_list <- sapply(names(cmd_list), lookup_function)

  result <- data.frame()
  for(i in 1:length(cmd_list)){
    coords_df <- do.call(fun_list[i], list(result, cmd_list[i]))
    result <- rbind(result, coords_df, make.row.names=F)
  }
  result
}

# affine transformation for rotation
rot <- function(a){matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)}

# Convert svg coordinates to ploygon and flip horizontally to account for
#  differnce in origin between coordinate systems (topleft vs bottomleft)
svg_coords_to_polygon <- function(coords){
  outline <- st_polygon(list(as.matrix(coords)))

  # rotated 180
  rotated <- (outline) * rot(pi)

  # Move shape center to complete flip
  flipped <- rotated + st_point(c(max(coords$x), max(coords$y)))
}

# internal use function to drow a regtangle polygon
rect_poly <- function(ymin, ymax, xmin, xmax){
  x_vals <- c(xmin,xmin,xmax,xmax,xmin)
  y_vals <- c(ymin,ymax,ymax,ymin,ymin)
  points_matrix <- matrix(c(x_vals,y_vals),ncol=2)
  st_polygon(list(points_matrix))
}

# Generated sequence of rectangles to divide polygon
generate_dividers <- function(polyg, n){
  coords <- st_coordinates(polyg)
  min_y <- min(coords[,'Y'])
  max_y <- max(coords[,'Y'])
  min_x <- min(coords[,'X'])
  max_x <- max(coords[,'X'])

  # Create a sequence from min to max for y.
  y_seq <- seq(min_y, max_y, length.out=n+1)

  # Bottom edges of rects don't include ymax
  y_lowers <- y_seq[-length(y_seq)]
  # Top edges of rects don't include ymin
  y_uppers <- y_seq[-1]

  # apply over lower and upper y edges. x min and max are static
  rect_list <- mapply(FUN=rect_poly,
                      y_lowers, y_uppers,
                      MoreArgs=list(xmin=min_x, xmax=max_x),
                      SIMPLIFY=F)
}

# Given polygon and list of polygon dividers, return shape frame of intersections
#  between polygon and dividers.  In short, chop up the polygon.
intersect_dividers <- function(polyg, dividers){
  intersect_list <- lapply(dividers, st_intersection, polyg)
  # Conversion to sf (simple feature) frame
  st_sf(st_sfc(intersect_list))
}

### EXAMPLE DATA
# image path d attribute
d_string <- "m 79.858905,181.53725 -0.63078,-0.0528 -0.63271,-0.30764 -0.60558,-0.50723 -0.54939,-0.65161 -0.46412,-0.74075 -0.34978,-0.77466 -0.20639,-0.75335 -0.0339,-0.6768 V 20.554833 l -0.0242,-0.491317 -0.0176,-0.4823 -0.0178,-0.472177 -0.0249,-0.460948 -0.0388,-0.448614 -0.0596,-0.435175 -0.0873,-0.420629 -0.12179,-0.404977 -0.16316,-0.388221 -0.21138,-0.370359 -0.26645,-0.35139 -0.32838,-0.331317 -0.39716,-0.310137 -0.4728,-0.287852 -0.5553,-0.264461 -0.64464,-0.239964 H 6.9331215 l -0.603946,-0.037 -0.605953,-0.108731 -0.602419,-0.176182 -0.593342,-0.239347 -0.578724,-0.298227 -0.558564,-0.352824 -0.532863,-0.403133 -0.50162,-0.44916 -0.464834,-0.490901 -0.422507,-0.528356 -0.374639,-0.561527 -0.32122903,-0.590414 -0.262276,-0.6150143 -0.197782,-0.635331 -0.127747,-0.651362 -0.05217,-0.663109 0.05232,-1.266049 0.194117,-1.114324 0.315882,-0.972262 0.41761603,-0.839866 0.499318,-0.717135 0.560991,-0.604069 0.602631,-0.500668 0.62424,-0.406932 0.62582,-0.32286104 0.607366,-0.248455 0.568882,-0.183715 0.510368,-0.128639 0.431822,-0.08323 0.333244,-0.04748 0.214636,-0.0214 0.076,-0.005 H 165.25882 l 0.076,0.005 0.21464,0.0214 0.33324,0.04748 0.43182,0.08323 0.51037,0.128639 0.56888,0.183715 0.60737,0.248455 0.62582,0.32286104 0.62424,0.406932 0.60263,0.500668 0.56099,0.604069 0.49932,0.717135 0.41761,0.839866 0.31589,0.972262 0.19411,1.114324 0.0523,1.266049 -0.0522,0.663109 -0.12774,0.651362 -0.19779,0.635331 -0.26227,0.6150143 -0.32123,0.590414 -0.37464,0.561527 -0.42251,0.528356 -0.46483,0.490901 -0.50162,0.44916 -0.53286,0.403133 -0.55857,0.352824 -0.57872,0.298227 -0.59334,0.239347 -0.60242,0.176182 -0.60596,0.108731 -0.60394,0.037 H 99.071515 l -0.64464,0.239964 -0.5553,0.264461 -0.4728,0.287852 -0.39716,0.310137 -0.32837,0.331317 -0.26645,0.35139 -0.21138,0.370359 -0.16316,0.388221 -0.12179,0.404977 -0.0873,0.420629 -0.0596,0.435175 -0.0388,0.448614 -0.0249,0.460948 -0.0178,0.472177 -0.0176,0.4823 -0.0242,0.491317 V 177.07241 l -0.0339,0.6768 -0.20639,0.75335 -0.34978,0.77466 -0.46412,0.74075 -0.54939,0.65161 -0.60558,0.50723 -0.63271,0.30764 -0.63078,0.0528 z"

### PROCESSING EXAMPLE DATA TO POLYGON
coords <- dstring_to_svg_coords(d_string)
polyg <- svg_coords_to_polygon(coords)

### GENERATE 100 rectangles that cover the polygon
rects <- generate_dividers(polyg, 100)

### Chop up polygon into 100 equal vertical slices
slices <- intersect_dividers(polyg, rects)

### Add some performance results as a status column
slices[1:80, 'status'] <- 'complete'
slices[81:100, 'status'] <- 'incomplete'

ggplot() + geom_point(aes(x=x,y=y),data=coords)
ggplot() + geom_sf(data=polyg)
ggplot() + geom_sf(data=st_sf(st_sfc(rects)))

ggplot() +
  geom_sf(aes(fill=status), data=slices, lwd=0) +
  geom_sf(data=polyg, fill=NA, lwd=2) +
  scale_fill_manual(values=c('complete'='light blue', 'incomplete'='white'))




