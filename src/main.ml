open Core

let _ = Sphere.hit

let width  = 384
let height = int_of_float @@ float width /. Camera.aspect_ratio

let samples_per_pixel = 100

let sphere : Sphere.t = { center = (0.0,    0.0, -1.0); radius = 0.5 }
let ground : Sphere.t = { center = (0.0, -100.5, -1.0); radius = 100.0 }

let world = [ ground; sphere ]

let jitter x = float x +. Random.float_range 0.0 1.0

let () = 
  eprintf "Start.\n";

  printf "P3\n";
  printf "%d %d\n" width height;
  printf "255\n";

  let height = height - 1 in
  let width  = width  - 1 in

  for j = height downto 0 do
    eprintf "\rScanlines remaining: %d/%d %!" j height;

    for i = 0 to width do
      List.init samples_per_pixel ~f:(fun _ ->
          Sphere.List.color world
          @@ Camera.get_ray
            (jitter i /. float width)
            (jitter j /. float height)
        )
      |> List.fold ~init:Color.black ~f:Color.(+)
      |> Color.sampling ~samples_per_pixel
      |> Color.gamma_correction
      |> Color.to_256
      |> Tuple3.uncurry @@ printf "%d %d %d\n"
    done

  done;
  eprintf "\nDone.\n"
