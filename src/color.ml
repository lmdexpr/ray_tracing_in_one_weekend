open Core

type t = Vec3.t

let create = Vec3.create

let sampling ~samples_per_pixel c =
  Vec3.scale c (1.0 /. float_of_int samples_per_pixel)
  
let to_256 = Vec3.map ~f:(fun x ->
    Float.clamp_exn ~min:0. ~max:0.999 x
    |> ( *. ) 256.
    |> Float.to_int
  )

let black = create 0.0 0.0 0.0
let white = create 1.0 1.0 1.0
let blue  = create 0.5 0.7 1.0

let scale k c = Vec3.scale c k

let (+) = Vec3.(+)

let gradation = Vec3.line
