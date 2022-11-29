input <- readLines("input.txt")
box   <- read.csv(text = gsub("..", ",", gsub("target area: x=| y=", "", input), fixed = TRUE),
                  header = FALSE,
                  col.names = c("x1", "x2", "y1", "y2"))
# part 1
#  ideas:
#    x does not matter
#    the highest probe will land in the highest part of the box (max y)
#    highest probe will descend from v_y = 0 incrementing by 1 each time
#    so highest probe will be sum of ints from max y (less 1 to account for time steps)
max_vy0 <- max(abs(box$y1) - 1)
max_y   <- sum(1:max_vy0)
max_y

# part 2
max_t   <- 2*(abs(box[["y1"]])) + 1  # longest flight < time steps of highest valid projectile
max_vx0 <- box$x2                    # fastest vx0 cannot surpass farthest trench boundary
min_vx0 <- floor((-1 + sqrt(1 + 4*2*box$x1))/2)  # inverting Gauss' sum of integers
vxy     <- expand.grid(vx0 = min_vx0:max_vx0, vy0 = box$y1:max_y)

compute_flight <- function(vx0, vy0, max_t, box) {
  x_coords <- cumsum(c(vx0:1, rep(0, max_t)))[1:(max_t+1)]
  y_coords <- cumsum(vy0 - (0:max_t))
  is_hit   <- any(x_coords <= box$x2 & x_coords >= box$x1 &
                  y_coords <= box$y2 & y_coords >= box$y1)
  data.frame(vx0 = vx0, vy0 = vy0, is_hit = is_hit)
}

x <- do.call(rbind,
        mapply(compute_flight, vx0 = vxy$vx0, vy0 = vxy$vy0,
               MoreArgs = list(max_t = max_t, box = box), SIMPLIFY = FALSE))
sum(x$is_hit)




inside_target <- function(coords, min, max) coords >= min & coords <= max 

y_hit <- function(v_y, target) {
  max_steps <- sum(abs(1:target$y1))       # this many steps will surely cross the box
  coords    <- cumsum(v_y - (0:max_steps)) # all y-coordinates for that many steps
  is_hit    <- inside_target(coords, target$y1, target$y2)   # does it intersect box?
  data.frame(v_y       = v_y,
             hit_y     = if(any(is_hit)) coords[is_hit] else NA,
             hit_time  = if(any(is_hit)) which(is_hit) else NA)
}


x_hit <- function(v_x, hit_time, target, max_time) {
  coords <- cumsum(v_x:1)
  coord  <- c(coords, rep(tail(coords, 1), max_time))[hit_time]
  is_hit <- inside_target(coord, target$x1, target$x2)
  data.frame(v_x      = v_x,
             hit_time = hit_time,
             hit_x    = coord,
             is_hit   = if(is.na(is_hit)) FALSE else is_hit)
}
vy_check <- (box$y1):250      # v_y can't be more negative than lowest part of box
vy_candidates <- do.call(rbind, lapply(vy_check, y_hit, target = box))
y_data <- na.omit(vy_candidates)

vx_check <- 1:(box$x2)   # v_x can't be more than max X of target or it won't hit
grid <- expand.grid(vx_check, unique(y_data$hit_time))
names(grid) <- c("v_x", "hit_time")
vx_candidates <- do.call(rbind, lapply(1:nrow(grid), function(i)
                           x_hit(grid$v_x[i], grid$hit_time[i], box, max(y_data$hit_time))))
x_data <- vx_candidates[vx_candidates$is_hit, ]

all_hits <- merge(x_data, y_data, by = "hit_time")[c("v_x", "v_y")]
nrow(all_hits[!duplicated(all_hits), ])