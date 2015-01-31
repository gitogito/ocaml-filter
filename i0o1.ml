open Base

module Multiplier = struct
  include I0o1_base

  type t' = {
    gen1 : I0o1_base.t;
    gen2 : I0o1_base.t;
  }

  let update self () =
    let vo1 = self.gen1.I0o1_base.update () in
    let vo2 = self.gen1.I0o1_base.update () in
    vo1 *. vo2

  let init gen1 gen2 =
    let self = { gen1; gen2 } in
    { update = update self }
end
