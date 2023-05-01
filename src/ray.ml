open Core

type t = { origin : Point3.t; direction : Vec3.t }

let create ?(origin=Point3.origin) direction = { origin; direction }

let at ray t = Vec3.(ray.origin + t *| ray.direction)

module Infix = struct
  let (.@()) = at
end

let color ~(f: t -> Material.Hit_record.t option) (ray : t) =
  match f ray with
  | Some record ->
    let normal = Material.Directed.to_vec3 record.normal in
    Vec3.(normal + (1.0, 1.0, 1.0)) 
    |> Color.scale 0.5
  | None ->
    let _, y_unit_direction, _ = Vec3.normalize ray.direction in
    let t = 0.5 *. (y_unit_direction +. 1.0) in
    Color.(gradation t white blue)
