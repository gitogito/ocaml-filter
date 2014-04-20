(*
 * BT filter (4th order)
 * 105/(105+105*s+45*s^2+10*s^3+s^4)
 *)

let pow a n = a ** (float n)

let freq_3db0 = 2.113917674904216 /. (2.0 *. Util.pi)
let order = 4
let ary_size = order + 1

type t = {
  den : float array;
  num : float array;
  x2_old : float array;
}

let make tstep freq_3db =
  let a = freq_3db0 /. freq_3db in
  let den = Array.make ary_size 0.0 in
  let num = Array.make ary_size 0.0 in
  let x2_old = Array.make ary_size 0.0 in
  let open Flop in
  den.(0) <- 105. * pow tstep 4 + 210. * a * pow tstep 3 + 180. * pow a 2 * pow tstep 2 + 80. * pow a 3 * tstep + 16. * pow a 4;
  den.(1) <- 420. * pow tstep 4 + 420. * a * pow tstep 3 - 160. * pow a 3 * tstep - 64. * pow a 4;
  den.(2) <- 630. * pow tstep 4 - 360. * pow a 2 * pow tstep 2 + 96. * pow a 4;
  den.(3) <- 420. * pow tstep 4 - 420. * a * pow tstep 3 + 160. * pow a 3 * tstep - 64. * pow a 4;
  den.(4) <- 105. * pow tstep 4 - 210. * a * pow tstep 3 + 180. * pow a 2 * pow tstep 2 - 80. * pow a 3 * tstep + 16. * pow a 4;

  num.(0) <- 105. * pow tstep 4;
  num.(1) <- 420. * pow tstep 4;
  num.(2) <- 630. * pow tstep 4;
  num.(3) <- 420. * pow tstep 4;
  num.(4) <- 105. * pow tstep 4;

  x2_old.(0) <- -777.7;	(* this is not used *)
  x2_old.(1) <- 0.0;
  x2_old.(2) <- 0.0;
  x2_old.(3) <- 0.0;
  x2_old.(4) <- 0.0;

  { den; num; x2_old }

let update self inp =
  let den = self.den in
  let num = self.num in
  let x2_old = self.x2_old in
  let x1 = let open Flop in ref (num.(0) / den.(0) * inp) in
  let x2 = ref !x1 in
  for i = 1 to ary_size - 1 do
    x2 := let open Flop in !x2 - den.(i) / den.(0) * x2_old.(i)
  done;
  let out = ref !x2 in
  for i = 1 to ary_size - 1 do
    out := let open Flop in !out + num.(i) / num.(0) * x2_old.(i)
  done;

  for i = ary_size - 2 downto 1 do
    x2_old.(i + 1) <- x2_old.(i)
  done;
  x2_old.(1) <- !x2;
  !out
