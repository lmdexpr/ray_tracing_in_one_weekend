open Core

module Oriented = struct
  type vec3 =
    | FrontFace of Vec3.t
    | BackFace  of Vec3.t

  let of_vec3 ~direction v : vec3 =
    if Float.(Vec3.(direction *. v) < 0.)
    then FrontFace v
    else BackFace  v

  let to_vec3 = function
    | FrontFace v -> v
    | BackFace  v -> Vec3.(-1. *| v)
end

module Hit_record = struct
  type t = {
    t : float;
    p : Point3.t;
    normal : Oriented.vec3;
  }

  let get_t = function { t; _ } -> t

  let create t p normal : t = { t; p; normal }
end

let in_unit_sphere record =
  let normal = Oriented.to_vec3 record.Hit_record.normal in
  let rec loop () =
    let open Vec3 in
    let v = Random.range ~min:(-1.) ~max:1. in
    if Float.(norm_square v >= 1.) then loop ()
    else 
      record.p + normal + v
  in
  loop ()

let unit_vector record =
  let normal = Oriented.to_vec3 record.Hit_record.normal in
  let a = Random.float_range 0. Float.(2. * pi) in
  let z = Random.float_range (-1.) 1. in
  let r = sqrt Float.(1. - z * z) in
  let v = Vec3.create Float.(r * cos a) Float.(r * sin a) z in
  Vec3.(record.p + normal + v)

let in_hemisphere record =
  let normal = Oriented.to_vec3 record.Hit_record.normal in
  let r      = in_unit_sphere record in
  Vec3.(record.p + if Float.(r *. normal > 0.0) then r else -1. *| r)
