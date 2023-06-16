open Core

type t = {
  center : Point3.t;
  radius : float;
  material : Material.t;
}

let create center radius material : t = { center; radius; material }

let hit ?(t_min=0.001) ?(t_max=Float.infinity)
    ({center; radius; material}: t)
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
        (Oriented.of_vec3 ~direction Vec3.((p - center) / radius))
        material
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

  let max_depth = 50
  let rec color ?(depth=0) ?t_min ?t_max spheres ray =
    if max_depth < depth then Color.black
    else
      let depth = depth + 1 in
      match hit ?t_min ?t_max spheres ray with
      | Some record -> begin
          match Material.scatter ray record with
          | None                           -> Color.black
          | Some { scattered; attenuation} ->
            Vec3.(attenuation * color ~depth ?t_min ?t_max spheres scattered)
        end
      | None ->
        let _, y_unit_direction, _ = Vec3.normalize ray.direction in
        let t = 0.5 *. (y_unit_direction +. 1.0) in
        Color.(gradation t white blue)
end
