module Square = struct
  include I0o1.Base

  type generator = {
    n : int;
    mutable counter : int;
  }

  let update self () =
    let output =
      if Util.is_even (self.counter / (self.n / 2)) then
        1.0
      else
        -1.0
    in
    self.counter <- self.counter + 1;
    output

  let init n =
    if n mod 2 = 1 then
      failwith "n must be even";
    let self = { n; counter = 0 } in
    { update = update self }
end

module Sine = struct
  include I0o1.Base

  type generator = {
    n : int;
    mutable counter : int;
  }

  let update self () =
    let output = sin (float self.counter /. float self.n *. 2.0 *. Util.pi) in
    self.counter <- self.counter + 1;
    output

  let init n =
    let self = { n; counter = 0 } in
    { update = update self }
end