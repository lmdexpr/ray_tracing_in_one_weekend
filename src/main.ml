open Core

let aspect_ratio = 16.0 /. 9.0

let width = 384
let height = int_of_float @@ float width /. aspect_ratio

let _ = Sphere.hit

let hit_sphere center radius (ray : Ray.t) =
  let oc     = Vec3.(ray.origin - center) in
  let a      = Vec3.norm_square ray.direction in
  let half_b = Vec3.(oc *. ray.direction) in
  let c      = Vec3.norm_square oc -. radius *. radius in
  let discriminant = half_b *. half_b -. a *. c in
  Option.some_if
    Float.(discriminant > 0.)
    Float.((half_b -. sqrt discriminant) /. a)

let ray_color (ray : Ray.t) =
  match hit_sphere (0.0, 0.0, -1.0) 0.5 ray with
  | Some t ->
    let v = Vec3.(Ray.at ray t - (0.0, 0.0, -1.0)) in
    Color.( 0.5 *| (1.0 +| v) )
  | None ->
    let _, y_unit_direction, _ = Vec3.normalize ray.direction in
    let t = 0.5 *. (y_unit_direction +. 1.0) in
    let white = Color.create 1.0 1.0 1.0 in
    let blue  = Color.create 0.5 0.7 1.0 in
    Color.((1.0 -. t) *| white + t *| blue)

let () =
  print_endline "P3";
  printf "%d %d\n" width height;
  print_endline "255"

module Viewport = struct
  let height = 2.0
  let width  = aspect_ratio *. height
end
let focal_length    = 1.0

let horizontal = Point3.create Viewport.width 0.0 0.0
let vertical   = Point3.create 0.0 Viewport.height 0.0

let lower_left_corner =
  let open Vec3 in
  Point3.origin - horizontal / 2.0 - vertical / 2.0 - (0.0, 0.0, focal_length)

let()= 
  for j = height - 1 downto 0 do
    eprintf "\rScanlines remaining: %d %!" j;

    for i = 0 to width - 1 do
      let u = float i /. float (width - 1) in
      let v = float j /. float (height - 1) in
      Ray.create
        Point3.origin
        Vec3.(lower_left_corner + u *| horizontal + v *| vertical - Point3.origin)
      |> ray_color
      |> Color.to_256
      |> Tuple3.uncurry (printf "%d %d %d\n");
    done
  done;
  eprintf "\n Done.\n"
