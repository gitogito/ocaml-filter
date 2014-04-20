module RC = struct
  include I1o1.Base

  type filter = {
    r : float;
    c : float;
    dt : float;
    mutable vo : float;
  }

  let update self vi =
    let curr = (vi -. self.vo) /. self.r in
    self.vo <- self.vo +. 1.0 /. self.c *. curr *. self.dt;
    self.vo

  let init ~r ~c ~dt =
    let self = { r; c; dt; vo = 0.0 } in
    { update = update self }
end

module BT = struct
  include I1o1.Base

  type filter = {
    btfilter : Btfilter.t;
  }

  let update self vi =
    Btfilter.update self vi

  let init ~tstep ~freq_3db =
    let self = Btfilter.make tstep freq_3db in
    { update = update self }
end
