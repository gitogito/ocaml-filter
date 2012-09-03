open Printf
open Filter.BTFilter4

let () =
  let filter = make 0.1e-12 7.5e9 in
  for i = 0 to 10000 do
    let t = float i *. 0.1e-12 in
    let out = update filter 1.0 in
    if i mod 10 = 0 then printf "%g\t%g\n" t out
  done
