module Base = struct
  type t = {
    update : unit -> float;
  }
end

module Multiplier = struct
  include Base

  type t' = {
    gen1 : Base.t;
    gen2 : Base.t;
  }

  let update self () =
    let vo1 = self.gen1.Base.update () in
    let vo2 = self.gen1.Base.update () in
    vo1 *. vo2

  let init gen1 gen2 =
    let self = { gen1; gen2 } in
    { update = update self }
end
