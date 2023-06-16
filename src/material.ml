open Core

type t =
  | Lambertian of { albedo : Color.t }
  | Metal      of { albedo : Color.t }

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
  type mat_t = t
  type t = {
    t : float;
    p : Point3.t;
    normal : Oriented.vec3;
    material : mat_t;
  }

  let get_t = function { t; _ } -> t

  let create t p normal material : t = { t; p; normal; material }
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

open struct
  let random_unit_vector () =
    let a = Random.float_range 0. Float.(2. * pi) in
    let z = Random.float_range (-1.) 1. in
    let r = sqrt Float.(1. - z * z) in
    Vec3.create Float.(r * cos a) Float.(r * sin a) z
end

let unit_vector record =
  let normal = Oriented.to_vec3 record.Hit_record.normal in
  Vec3.(record.p + normal + random_unit_vector ())

let in_hemisphere record =
  let normal = Oriented.to_vec3 record.Hit_record.normal in
  let r      = in_unit_sphere record in
  Vec3.(record.p + if Float.(r *. normal > 0.0) then r else -1. *| r)

type scattered_result = { scattered : Ray.t; attenuation : Color.t }

let scatter ray record =
  let normal = Oriented.to_vec3 record.Hit_record.normal in
  let origin = record.p in
  match record.material with
  | Lambertian { albedo } ->
    let scattered   = Ray.create ~origin Vec3.(normal + random_unit_vector ()) in
    let attenuation = albedo in
    Some { scattered; attenuation }
  | Metal { albedo } ->
    let reflected   = Vec3.(reflect (normalize ray.Ray.direction) normal) in
    let scattered   = Ray.create ~origin reflected in
    let attenuation = albedo in
    Option.some_if Float.(Vec3.dot scattered.direction normal > 0.)
      { scattered; attenuation }
