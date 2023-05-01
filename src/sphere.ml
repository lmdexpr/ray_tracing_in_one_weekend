open Core

type t = {
  center : Point3.t;
  radius : float;
}

let create center radius : t = { center; radius }

let hit ?(t_min=0.) ?(t_max=Float.infinity) ({center; radius}: t) (ray : Ray.t) =
  let oc     = Vec3.(ray.origin - center) in
  let a      = Vec3.norm_square ray.direction in
  let half_b = Vec3.(oc *. ray.direction) in
  let c      = Vec3.norm_square oc -. radius *. radius in

  let discriminant = half_b *. half_b -. a *. c in

  let in_range t = Float.(t_min < t && t < t_max) in

  let create_record t : Material.Hit_record.t =
    let open Ray.Infix in
    let p      = ray.@(t) in
    let normal =
      Vec3.((p - center) / radius)
      |> Material.Directed.of_vec3 ~direction:ray.direction
    in
    { t; p; normal }
  in

  if Float.(discriminant <= 0.0) then None
  else
    let root = sqrt discriminant in
    let t_minus = (-. half_b -. root) /. a in
    let t_plus  = (-. half_b +. root) /. a in
    if in_range t_minus then Some (create_record t_minus)
    else if in_range t_plus then Some (create_record t_plus)
    else None

module List = struct
  let hit ?t_min ?t_max spheres ray =
    let f closest_so_far sphere =
      hit ?t_min ?t_max sphere ray
      |> Option.merge closest_so_far ~f:Tuple2.(curry get2)
    in
    List.fold ~init:None ~f spheres
end
