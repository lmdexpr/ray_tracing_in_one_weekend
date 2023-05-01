module Oriented = struct
  type vec3 =
    | FrontFace of Vec3.t
    | BackFace  of Vec3.t

  let of_vec3 ~direction v : vec3 =
    if Vec3.(direction *. v) < 0.
    then FrontFace v
    else BackFace  v

  let to_vec3 = function
    | FrontFace v -> v
    | BackFace  v -> Vec3.(-1. *| v)
end

module Hit_record = struct
  type t = {
    t : float;
    p : Point3.t;
    normal : Oriented.vec3;
  }

  let get_t = function { t; _ } -> t

  let create t p normal : t = { t; p; normal }
end

type t
