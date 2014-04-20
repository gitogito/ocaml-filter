module Base = struct
  type t = {
    update : float -> float;
  }
end

module Combine = struct
  include Base

  type filter = {
    f1 : Base.t;
    f2 : Base.t;
  }

  let update self vi =
    let vo = self.f1.Base.update vi in
    let vo = self.f2.Base.update vo in
    vo

  let init f1 f2 =
    let self = { f1; f2 } in
    { update = update self }
end
