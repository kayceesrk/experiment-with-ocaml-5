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

module Sequential = struct
  let rec edit_distance table s t =
    match String.length s, String.length t with
    | 0, x | x, 0 -> x
    | len_s, len_t ->
        begin match table.(len_s).(len_t) with
        | Some v -> v
        | None ->
          let s' = String.drop_suffix s 1 in
          let t' = String.drop_suffix t 1 in
          let cost_to_drop_both =
            if Char.( = ) s.[len_s - 1] t.[len_t - 1] then 0 else 1
          in
          let d1 = edit_distance table s' t + 1 in
          let d2 = edit_distance table s t' + 1 in
          let d3 = edit_distance table s' t' + cost_to_drop_both in
          let ( ++ ) = Int.min in
          let res = d1 ++ d2 ++ d3 in
          table.(len_s).(len_t) <- Some res;
          res
        end
end

module Parallel = struct
  module Task = Domainslib.Task

  let rec edit_distance table pool s t =
    let async f = Task.async pool f in
    let await x = Task.await pool x in
    match String.length s, String.length t with
    | 0, x | x, 0 -> x
    | len_s, len_t when len_s < 10 && len_t < 10 ->
        Sequential.edit_distance table s t
    | len_s, len_t ->
        begin match table.(len_s).(len_t) with
        | Some v -> v
        | None ->
            let s' = String.drop_suffix s 1 in
            let t' = String.drop_suffix t 1 in
            let cost_to_drop_both =
              if Char.( = ) s.[len_s - 1] t.[len_t - 1] then 0 else 1
            in
            let d1 = async (fun () -> edit_distance table pool s' t + 1) in
            let d2 = async (fun () -> edit_distance table pool s t' + 1) in
            let d3 = async (fun () -> edit_distance table pool s' t' + cost_to_drop_both) in
            let ( ++ ) = Int.min in
            let res = await d1 ++ await d2 ++ await d3 in
            table.(len_s).(len_t) <- Some res;
            res
        end

  let edit_distance table ~num_domains s t =
    let pool = Task.setup_pool ~num_domains () in
    let res = Task.run pool (fun () -> edit_distance table pool s t) in
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
         let table = Array.make_matrix ~dimx:(String.length a + 1) ~dimy:(String.length b + 1) None in
         (match seq_or_par with
          | Seq -> printf "%d\n" (Sequential.edit_distance table a b : int)
          | Par -> printf "%d\n" (Parallel.edit_distance table ~num_domains:(num_domains - 1) a b : int)))
  |> Command_unix.run
;;
