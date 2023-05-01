type t = { origin : Point3.t; direction : Vec3.t }

let create ?(origin=Point3.origin) direction = { origin; direction }

let at ray t = Vec3.(ray.origin + t *| ray.direction)

module Infix = struct
  let (.@()) = at
end
