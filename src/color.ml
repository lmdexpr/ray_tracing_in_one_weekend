let to_256 = Vec3.map ~f:(fun x -> int_of_float @@ 255.999 *. x)

include Vec3
