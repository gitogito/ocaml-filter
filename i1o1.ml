open Base

module Combine = struct
  include I1o1_base

  type t' = {
    f1 : I1o1_base.t;
    f2 : I1o1_base.t;
  }

  let update self vi =
    let vo = self.f1.I1o1_base.update vi in
    let vo = self.f2.I1o1_base.update vo in
    vo

  let init f1 f2 =
    let self = { f1; f2 } in
    { update = update self }
end

module Delay = struct
  include I1o1_base

  type t' = {
    queue : float Queue.t;
  }

  let update self vi =
    Queue.add vi self.queue;
    Queue.take self.queue

  let init n =
    if n < 0 then raise (Invalid_argument "Delay.init");
    let queue = Queue.create () in
    for _ = 1 to n do
      Queue.add 0.0 queue
    done;
    let self = { queue } in
    { update = update self }
end
