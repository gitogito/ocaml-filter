module I0o1_base = struct
  type t = {
    update : unit -> float;
  }
end

module I1o1_base = struct
  type t = {
    update : float -> float;
  }
end
