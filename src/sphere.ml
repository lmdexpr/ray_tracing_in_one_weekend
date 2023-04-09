open Core

type t = {
  center : Point3.t;
  radius : float;
}

let discriminant (sphere : t) (ray : Ray.t) =
  let oc     = Vec3.(ray.origin - sphere.center) in
  let a      = Vec3.norm_square ray.direction in
  let half_b = Vec3.(oc *. ray.direction) in
  let c      = Vec3.norm_square oc -. sphere.radius *. sphere.radius in
  half_b *. half_b -. a *. c

let hit (sphere : t) (ray : Ray.t) t_min t_max (record : Hit.t) =
  let oc     = Vec3.(ray.origin - sphere.center) in
  let a      = Vec3.norm_square ray.direction in
  let half_b = Vec3.(oc *. ray.direction) in
  let c      = Vec3.norm_square oc -. sphere.radius *. sphere.radius in
  let discriminant = half_b *. half_b -. a *. c in
  let in_range t = Float.(t_min < t && t < t_max) in
  let create_record temp : Hit.t =
    {
      t = temp;
      p = Ray.at ray record.t;
      normal = Point3.((record.p - sphere.center) / sphere.radius);
    }
  in
  if Float.(discriminant <= 0.0) then None
  else
    let root = sqrt discriminant in
    let temp_minus = (-. half_b -. root) /. a in
    let temp_plus  = (-. half_b +. root) /. a in
    if in_range temp_minus then Some (create_record temp_minus)
    else if in_range temp_plus then Some (create_record temp_plus)
    else None
