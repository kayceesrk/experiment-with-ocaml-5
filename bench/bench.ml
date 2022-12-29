open! Core

module Benchmark_id = struct
  module Seq_or_par = struct
    type t =
      | Seq
      | Par
    [@@deriving sexp]
  end

  module T = struct
    type t = Distance of Seq_or_par.t * string * string [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)
end

module Memo = struct
  let memo ~get ~set f v =
    let res = get v in
    if res = -1 then begin
        let res = f v in
        set v res;
        res
    end else res

  let memo_rec ~get ~set f_norec =
    let f_ref = ref (fun _ -> assert false) in
    let f_rec_memo = memo ~get ~set (fun x -> f_norec !f_ref x) in
    f_ref := f_rec_memo;
    f_rec_memo
end

module SuffixString = struct
  type t = {str : String.t; len: int}

  let mk s = {str = s; len = String.length s}
  let length {len; _} = len
  let drop_suffix {str; len} n = {str; len = len - n}
  let get {str; _} n = str.[n]
end

module Sequential = struct
  let edit_distance f (s, t) =
    match SuffixString.length s, SuffixString.length t with
    | 0, x | x, 0 -> x
    | len_s, len_t ->
        let s' = SuffixString.drop_suffix s 1 in
        let t' = SuffixString.drop_suffix t 1 in
        let cost_to_drop_both =
          if Char.( = ) (SuffixString.get s (len_s - 1))
                        (SuffixString.get t (len_t - 1))
          then 0 else 1
        in
        let d1 = f (s', t) + 1 in
        let d2 = f (s, t') + 1 in
        let d3 = f (s', t') + cost_to_drop_both in
        let ( ++ ) = Int.min in
        d1 ++ d2 ++ d3
end

module Parallel = struct
  module Task = Domainslib.Task

  let edit_distance ~num_domains seq_ed s t =
    let pool = Task.setup_pool ~num_domains () in
    let async = Task.async pool in

    (* Fill the memo table by computing the edit distance for the 4 quadrants
     * (recursively, for a small recursion depth) in parallel. *)
    let rec helper depth s t =
      if depth > 3 then ignore @@ seq_ed (s,t)
      else begin
        let open SuffixString in
        let s' = drop_suffix s (length s / 2) in
        let t' = drop_suffix t (length t / 2) in
        ignore @@ async (fun _ -> helper (depth+1) s' t');
        ignore @@ async (fun _ -> helper (depth+1) s t');
        ignore @@ async (fun _ -> helper (depth+1) s' t);
        ignore @@ async (fun _ -> helper (depth+1) s t)
      end
    in
    helper 0 s t;
    let res = seq_ed (s,t) in
    Task.teardown_pool pool;
    res
  ;;
end

let () =
  Command.basic
    ~summary:"Benchmark a thing"
    (let%map_open.Command () = return ()
     and num_domains = anon ("domains" %: int)
     and benchmark =
       anon ("benchmark" %: Command.Arg_type.create Benchmark_id.of_string)
     in
     fun () ->
       match benchmark with
       | Distance (seq_or_par, a, b) ->
           let open SuffixString in
           let a = mk a in
           let b = mk b in
           let table =
             Array.make_matrix ~dimx:(length a + 1) ~dimy:(length b + 1) (-1)
           in
           (* We don't use atomic instructions to read and write to the memo
              table. The memory model ensures that there are no out-of-thin-air
              values. Hence, the value in a cell will either be the initial
              value [-1] or the computed result. Multiple tasks may compute the
              result for the same cell. But all of them compute the same
              result. *)
           let get (a,b) = table.(length a).(length b) in
           let set (a,b) res = table.(length a).(length b) <- res in
           let seq_ed = Memo.memo_rec ~get ~set Sequential.edit_distance in
           (match seq_or_par with
             | Seq -> printf "%d\n" (seq_ed (a, b) : int)
             | Par -> printf "%d\n" (Parallel.edit_distance ~num_domains:(num_domains - 1) seq_ed a b : int)
           ))
  |> Command_unix.run
;;
