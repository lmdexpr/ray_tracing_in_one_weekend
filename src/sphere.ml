open Core

type t = {
  center : Point3.t;
  radius : float;
}

let create center radius : t = { center; radius }

let hit ?(t_min=0.001) ?(t_max=Float.infinity)
    ({center; radius}: t)
    ({origin; direction} as ray : Ray.t)
  =
  let oc     = Vec3.(origin - center) in
  let a      = Vec3.norm_square direction in
  let half_b = Vec3.(oc *. direction) in
  let c      = Vec3.norm_square oc -. radius *. radius in

  let discriminant = half_b *. half_b -. a *. c in

  let create_record t : Material.Hit_record.t =
    let open Ray.Infix in
    let p = ray.@(t) in
    Material.(
      Hit_record.create t p
      @@ Oriented.of_vec3 ~direction Vec3.((p - center) / radius)
    )
  in
  if Float.(discriminant <= 0.0) then None
  else
    let rootd = sqrt discriminant in
    let opt t =
      Option.some_if Float.(t_min < t && t < t_max) (create_record t)
    in
    Option.merge ~f:(fun t _ -> t)
      (opt @@ (-. half_b -. rootd) /. a)
      (opt @@ (-. half_b +. rootd) /. a)

module List = struct
  let hit ?t_min ?t_max spheres ray =
    let f closest_so_far sphere =
      hit ?t_min ?t_max sphere ray
      |> Option.merge closest_so_far ~f:Tuple2.(curry get2)
    in
    List.fold ~init:None ~f spheres

  let rec color ?(scalar=1.0) ?t_min ?t_max spheres ray =
    let epsilon = 1e-16 in
    if Float.(scalar <= epsilon) then Color.black
    else
      match hit ?t_min ?t_max spheres ray with
      | Some { t = _; p; normal } ->
        let normal = Material.Oriented.to_vec3 normal in
        Vec3.(p + Random.in_hemisphere normal)
        |> Ray.create ~origin:p 
        |> color ~scalar:(scalar *. 0.5) ?t_min ?t_max spheres
      | None ->
        let _, y_unit_direction, _ = Vec3.normalize ray.direction in
        let t = 0.5 *. (y_unit_direction +. 1.0) in
        Color.(scale scalar @@ gradation t white blue)
end
