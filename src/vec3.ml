open Core

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

let line t a b = (1. -. t) *| a + t *| b

module Random = struct
  let range ~min ~max =
    create
      (Random.float_range min max)
      (Random.float_range min max)
      (Random.float_range min max)

  let zero_one () = range ~min:0. ~max:1.

  let in_unit_sphere () =
    let rec loop () =
      let v = range ~min:(-1.) ~max:1. in
      if Float.(norm_square v < 1.) then v else loop ()
    in
    loop ()

  let unit_vector () =
    let a = Random.float_range 0. Float.(2. * pi) in
    let z = Random.float_range (-1.) 1. in
    let r = sqrt Float.(1. - z * z) in
    create Float.(r * cos a) Float.(r * sin a) z

  let in_hemisphere normal =
    let r = in_unit_sphere () in
    scale r
      (if Float.(r *. normal > 0.0) then 1. else -1.)
end
