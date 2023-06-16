open Core

type t = Vec3.t

let create = Vec3.create

let sampling ~samples_per_pixel v = Vec3.(v / float_of_int samples_per_pixel)

let gamma_correction = Vec3.map ~f:Float.sqrt

let to_256 = Vec3.map ~f:Float.(fun x ->
    to_int @@ 256. *. clamp_exn x ~min:0. ~max:0.999
  )

let black = create 0.0 0.0 0.0
let white = create 1.0 1.0 1.0
let blue  = create 0.5 0.7 1.0

let scale k c = Vec3.scale c k

let (+) = Vec3.(+)

let gradation = Vec3.line
