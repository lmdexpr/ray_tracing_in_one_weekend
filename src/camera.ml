let aspect_ratio = 16.0 /. 9.0

open struct
  module Viewport = struct
    let height = 2.0
    let width  = aspect_ratio *. height
  end
  open Viewport

  let horizontal = Point3.create width 0.0    0.0
  let vertical   = Point3.create 0.0   height 0.0

  let focal_length = 1.0

  open Vec3
  let lower_left_corner =
    let h = horizontal / 2.0 and v = vertical   / 2.0 in
    Point3.origin - h - v - (0.0, 0.0, focal_length)
end

let get_ray u v = Ray.create Vec3.(lower_left_corner + u *| horizontal + v *| vertical)
