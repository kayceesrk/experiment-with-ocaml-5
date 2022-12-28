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

  let edit_distance f (seq_ed, pool, s, t) =
    let async f = Task.async pool f in
    let await x = Task.await pool x in
    match SuffixString.length s, SuffixString.length t with
    | 0, x | x, 0 -> x
    | len_s, len_t when len_s * len_t < 400_000_000 -> seq_ed (s, t)
    | len_s, len_t ->
        let s' = SuffixString.drop_suffix s 1 in
        let t' = SuffixString.drop_suffix t 1 in
        let cost_to_drop_both =
          if Char.( = ) (SuffixString.get s (len_s - 1))
                        (SuffixString.get t (len_t - 1))
          then 0 else 1
        in
        let d1 = async (fun () -> f (seq_ed, pool, s', t) + 1) in
        let d2 = async (fun () -> f (seq_ed, pool, s, t') + 1) in
        let d3 = async (fun () -> f (seq_ed, pool, s', t') + cost_to_drop_both) in
        let ( ++ ) = Int.min in
        await d1 ++ await d2 ++ await d3

  let edit_distance ~get ~set ~num_domains seq_ed s t =
    let pool = Task.setup_pool ~num_domains () in
    let ed = Memo.memo_rec ~get ~set edit_distance in
    let res = Task.run pool (fun () -> ed (seq_ed, pool, s, t)) in
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
           let a = SuffixString.mk a in
           let b = SuffixString.mk b in
           let table =
             Array.make_matrix ~dimx:(SuffixString.length a + 1)
             ~dimy:(SuffixString.length b + 1) (-1)
           in
           let get (a,b) =
             table.(SuffixString.length a).(SuffixString.length b)
             in
           let set (a,b) res =
             table.(SuffixString.length a).(SuffixString.length b) <- res
           in
           let seq_ed = Memo.memo_rec ~get ~set Sequential.edit_distance in
           (match seq_or_par with
             | Seq -> printf "%d\n" (seq_ed (a, b) : int)
             | Par -> printf "%d\n" (Parallel.edit_distance
                 ~get:(fun (_,_,a,b) -> get (a,b))
                 ~set:(fun (_,_,a,b) v -> set (a,b) v)
                 ~num_domains:(num_domains - 1) seq_ed a b : int)))
  |> Command_unix.run
;;
