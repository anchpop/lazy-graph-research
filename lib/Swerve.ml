(* Catamorphism *)
(* Applicative f => (a -> f b) -> t a -> f (t b) *)

module Id_map = Map.Make (Int)

(* module type Graph = sig
     type 'a neighbor = 'a t Lazy.t
     and 'a neighbors = 'a neighbor list

     and 'a t = {
       id : int;
       neighbors : 'a neighbors;
       content : 'a;
     }

     type 'b idmap = ('b t Lazy.t Id_map.t)
     val map : fn:('a -> 'b) -> 'a t -> 'b t
     val fold_left_map : ('b idmap -> 'a t -> 'b idmap * 'b neighbor) -> 'b idmap -> ('a t) list -> 'b idmap * 'b neighbors

   end *)

module Graph = struct
  type 'a neighbor = 'a t Lazy.t
  and 'a neighbors = 'a neighbor list
  and id = int
  and 'a t = { id : id; neighbors : 'a t Lazy.t list; content : 'a }

  type 'b idmap = 'b t Lazy.t Id_map.t

  let map (fn : 'a -> 'b) (t : 'a t) : 'b t =
    let rec go (cache : 'b t Lazy.t Id_map.t)
        ({ id; neighbors; content } : 'a t) =
      let content = fn content in
      let cache, neighbors =
        List.fold_left_map
          (fun cache t ->
            let t = Lazy.force t in
            match Id_map.find_opt t.id cache with
            | None ->
                let cache = Id_map.add t.id (lazy (go cache t)) cache in
                (cache, Id_map.find t.id cache)
            | Some t -> (cache, t))
          cache neighbors
      in
      { id; neighbors; content }
    in
    let result = go Id_map.empty t in
    let _ = List.map Lazy.force result.neighbors in
    result

  let map (fn : 'a -> 'b) (t : 'a t) : 'b t =
    let rec go (cache : 'b t Lazy.t Id_map.t)
        ({ id; neighbors; content } : 'a t) =
      let content = fn content in
      let cache, neighbors =
        List.fold_left_map
          (fun cache t ->
            let t = Lazy.force t in
            match Id_map.find_opt t.id cache with
            | None ->
                let cache = Id_map.add t.id (lazy (go cache t)) cache in
                (cache, Id_map.find t.id cache)
            | Some t -> (cache, t))
          cache neighbors
      in
      { id; neighbors; content }
    in
    let result = go Id_map.empty t in
    let _ = List.map Lazy.force result.neighbors in
    result

  let f content = if String.equal "a" content then 1 else 2

  let rec basic_graph_1 =
    { id = 1; neighbors = [ lazy basic_graph_2 ]; content = "a" }

  and basic_graph_2 =
    { id = 2; neighbors = [ lazy basic_graph_1 ]; content = "b" }

  and basic_graph_3 =
    { id = 3; neighbors = [ lazy basic_graph_4 ]; content = "b" }

  and basic_graph_4 = { id = 3; neighbors = []; content = "b" }

  (*
  let map : fn:('a -> 'b) -> 'a t -> 'b t = fun ~fn t -> 
    let rec go t cache = 
      {
        t with content = ~fn content;
        neighbors = List.map neighbors ~fn 
      }

    in failwith "todo"
*)
end

(*
module Graph_recursion_scheme : sig
  type id = int
  type ('f, 'a) t = { id : id; neighbors : 'f list; content : 'a }
  type 'a t_rec = ('a t_rec, 'a) t

  val fmap : 'f 'ff 'a. ('f -> 'ff) -> ('f, 'a) t -> ('ff, 'a) t
  (*'f 'a 'b . ('f -> 'ff) -> ('f, 'a) t -> ('ff, 'a) t*)

  val cata : 'f 'a. (('f, 'a) t -> 'f) -> 'a t_rec -> 'f
end = struct
  type id = int
  type ('f, 'a) t = { id : id; neighbors : 'f list; content : 'a }
  type 'a t_rec = ('a t_rec, 'a) t

  let fmap f { id; neighbors; content } =
    let neighbors = List.map f neighbors in
    { id; neighbors; content }

  let cata : 'f 'a. (('f, 'a) t -> 'f) -> 'a t_rec -> 'f =
   fun f t ->
    let cache = Hashtbl.create 0 in
      let rec c t =
        match Hashtbl.find_opt cache t.id with
        | None ->
            let a = t |> fmap c in
            Hashtbl.add cache t.id a;
            f a
        | Some a -> f a
      in
    c t
end

repMax :: [Int] -> Int -> (Int, [Int])
repMax [] rep = (rep, [])
repMax [x] rep = (x, [rep])
repMax (l : ls) rep = (m', rep : ls')
  where (m, ls') = repMax ls rep
        m' = max m l

doRepMax :: [Int] -> [Int]
doRepMax xs = xs'
  where (largest, xs') = repMax xs largest
*)

module type Laziness = sig
  val repMax : int list -> int Lazy.t -> int * int Lazy.t list
  val doRepMax : int list -> int list
end

module Laziness : Laziness = struct
  let rec repMax l rep =
    match l with
    | [] -> ((*rep*) 0, [])
    | [ x ] -> (x, [ rep ])
    | l :: ls ->
        let m, ls' = repMax ls rep in
        let m' = max m l in
        (m', rep :: ls')

  let rec doRepMax (xs : int list) : int list =
    let rec res : (int * int Lazy.t list) Lazy.t =
      lazy (repMax xs (Lazy.map_val fst res))
    in
    List.map Lazy.force (Lazy.force (Lazy.map_val snd res))
end

(*

type 'a strictOption = 
  | None
  | Some of 'a Lazy.t


type 'a option = strictOption Lazy.t


let rec basic_graph_1 = 
  {
    id = 1;
    neighbors = [lazy basic_graph_2];
    content = "a"
  }

and basic_graph_2 =
  {
    id = 2;
    neighbors = [];
    content = "b";  
  };;



*)

(*
        /-> B
       A <-/
*)

(*
      fold :
         0:"string"     
        / \           => [1 -> "string";
       1   2              2 -> "string"]
        
        
      map :
        0:"string"
       / \ 
1:"string" 2:"string"
*)
