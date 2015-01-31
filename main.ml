open Printf

let dt = 0.01
let gen_n = Util.round (1.0 /. dt)
let tmax = 100.0
let r = 1.0 /. (2.0 *. Util.pi)
let c = 1.0

let rec sim generator filter n =
  let t = float n *. dt in
  if t > tmax then
    ()
  else begin
    let vi = generator.I0o1.Base.update () in
    let vo = filter.I1o1.Base.update vi in
    printf "%g\t%g\t%g\n" t vi vo;
    sim generator filter (n + 1)
  end

let () =
  let gen1 = Generator.Sine.init gen_n in
  let gen2 = Generator.Sine.init gen_n in
  let multi = I0o1.Multiplier.init gen1 gen2 in
  let filter =
    if Sys.argv.(1) = "a" then begin
      Filter.RC.init ~r ~c ~dt
    end else if Sys.argv.(1) = "b" then begin
      let filter1 = Filter.RC.init ~r ~c ~dt in
      let filter2 = Filter.RC.init ~r ~c ~dt in
      I1o1.Combine.init filter1 filter2
    end else begin
      Filter.BT.init ~tstep:dt ~freq_3db:1.0
    end
  in
  sim multi filter 0
