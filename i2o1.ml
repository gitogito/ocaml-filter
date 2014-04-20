module Base = struct
  type t = {
    update : float -> float -> float;
  }
end

module Multiplier = struct
  include I0o1.Base

  type t' = {
    gen1 : I0o1.Base.t;
    gen2 : I0o1.Base.t;
  }

  let update self () =
    let vo1 = self.gen1.I0o1.Base.update () in
    let vo2 = self.gen1.I0o1.Base.update () in
    vo1 *. vo2

  let init gen1 gen2 =
    let self = { gen1; gen2 } in
    { update = update self }
end
