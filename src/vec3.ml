
type t = float * float * float

let create x y z : t = x, y, z

let neg (x, y, z) = -.x, -.y, -.z

let dot (x1, y1, z1) (x2, y2, z2) = x1 *. x2 +. y1 *. y2 +. z1 *. z2

let cross (x1, y1, z1) (x2, y2, z2) =
  y1 *. z2 -. z1 *. y2,
  z1 *. x2 -. x1 *. z2,
  x1 *. y2 -. y1 *. x2

let scale (x, y, z) s = x *. s, y *. s, z *. s

let norm_square (x, y, z) = x *. x +. y *. y +. z *. z
let norm v = sqrt (norm_square v)

let (/) v s = scale v (1. /. s)

let normalize v = v / norm v

let (+) (x1, y1, z1) (x2, y2, z2) = x1 +. x2, y1 +. y2, z1 +. z2
let (+|) s (x, y, z) = x +. s, y +. s, z +. s

let (-) (x1, y1, z1) (x2, y2, z2) = x1 -. x2, y1 -. y2, z1 -. z2

let( *. ) = dot
let( * ) = cross
let( *|) s v = scale v s

let map (x, y, z) ~f = f x, f y, f z
