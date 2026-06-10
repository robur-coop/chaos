(* This code is the same as Miou but it permits to use Chaos with another
   scheduler and be sure that some operations are really synchronous. *)

module Trigger = struct
  type state =
    | Signaled
    | Awaiting : (t -> 'x -> 'y -> unit) * 'x * 'y -> state
    | Initial

  and t = state ref

  let create () = ref Initial
  let is_signaled t = !t == Signaled

  let signal t =
    match !t with
    | Signaled -> ()
    | Initial -> t := Signaled
    | Awaiting (fn, x, y) ->
        t := Signaled;
        fn t x y

  let[@inline never] awaiting _ = invalid_arg "Trigger: already awaiting"

  let on_signal t x y fn =
    match !t with
    | Initial ->
        t := Awaiting (fn, x, y);
        true
    | Signaled -> false
    | Awaiting _ as any -> awaiting any
end

module Computation = struct
  type 'a state =
    | Cancelled of exn
    | Returned of 'a
    | Continue of { balance: int; triggers: Trigger.t list }

  and 'a t = 'a state ref

  let create () = ref (Continue { balance= 0; triggers= [] })

  let rec gc length triggers = function
    | [] -> Continue { balance= length; triggers }
    | r :: rs ->
        if Trigger.is_signaled r then gc length triggers rs
        else gc (length + 1) (r :: triggers) rs

  let attach t trigger =
    match !t with
    | Returned _ | Cancelled _ -> false
    | Continue r when Trigger.is_signaled trigger = false ->
        let t' =
          if 0 <= r.balance then
            let balance = r.balance + 1 in
            let triggers = trigger :: r.triggers in
            Continue { balance; triggers }
          else gc 1 [ trigger ] r.triggers
        in
        t := t';
        true
    | Continue _ -> false

  let peek t =
    match !t with
    | Cancelled exn -> Some (Error exn)
    | Returned v -> Some (Ok v)
    | Continue _ -> None

  let terminate t after =
    match !t with
    | Returned _ | Cancelled _ -> false
    | Continue { triggers; _ } ->
        t := after;
        List.iter Trigger.signal triggers;
        true

  let return t value = terminate t (Returned value)
  let cancel t exn = terminate t (Cancelled exn)
  let is_running t = match !t with Continue _ -> true | _ -> false
end
