open Core

let _ = Sphere.hit

let width  = 384
let height = int_of_float @@ float width /. Camera.aspect_ratio

let samples_per_pixel = 100

let sphere : Sphere.t = { center = (0.0,    0.0, -1.0); radius = 0.5 }
let world  : Sphere.t = { center = (0.0, -100.5, -1.0); radius = 100.0 }

let () = 
  printf "P3\n";
  printf "%d %d\n" width height;
  printf "255\n";

  for j = height - 1 downto 0 do
    eprintf "\rScanlines remaining: %d %!" j;

    for i = 0 to width - 1 do
      List.init samples_per_pixel ~f:(fun _ ->
        let u = (float i +. Random.float_range 0.0 1.0) /. float (width - 1) in
        let v = (float j +. Random.float_range 0.0 1.0) /. float (height - 1) in

        Camera.(get_ray u v)
        |> Ray.color ~f:(Sphere.List.hit [ world; sphere ])
      )
      |> List.fold ~init:Color.black ~f:Color.(+)
      |> Color.sampling ~samples_per_pixel
      |> Color.to_256
      |> Tuple3.uncurry @@ printf "%d %d %d\n"
    done

  done;

  eprintf "\n Done.\n"
