open Base

module RC = struct
  include I1o1_base

  (* vo / vi = (a + b * z_1) / (c + b * z_1) *)
  type t' = {
    dt : float;
    a : float; b : float; c : float; d : float;
    mutable vi_1 : float;
    mutable vo_1 : float;
  }

  let update self vi =
    let vo = 1.0 /. self.c *. (self.a *. vi +. self.b *. self.vi_1 -. self.d *. self.vo_1) in
    self.vi_1 <- vi;
    self.vo_1 <- vo;
    vo

  let init ~r ~c ~dt =
    let tau = r *. c in
    let a = dt in
    let b = dt in
    let c = dt +. 2.0 *. tau in
    let d = dt -. 2.0 *. tau in
    let self = { dt; a; b; c; d; vi_1 = 0.0; vo_1 = 0.0 } in
    { update = update self }
end

module BT = struct
  include I1o1_base

  let update self vi =
    Btfilter.update self vi

  let init ~tstep ~freq_3db =
    let self = Btfilter.make tstep freq_3db in
    { update = update self }
end
