(* 
   This is a slightly rewritten data/array.ml to not introduce a separate
   constructor and thus structure the type in Lua as {__tag = "Array",
   {length = n, backing = {...}}}, instead using just a plain raw table
   without a tag or length
*)

open import "amulet/exception.ml"
open import "prelude.ml"
include import "data/index.ml"

type rawarray 'a

external private val tabulate : int -> (int -> 'a) -> rawarray 'a =
  "function(index, cont) \
     local r = {} \
     for i = 1, index do \
       r[i] = cont(i) \
     end \
     return r \
   end"

external private val empty_storage : rawarray 'a = "{}"
external private val geti : rawarray 'a -> int -> 'a = "rawget"
external private val seti : rawarray 'a -> int -> 'a -> rawarray 'a = "rawset"
external private val length : rawarray 'a -> int = "function(t) return #t end"

let private is_in_bounds i arr =
  i >= 0 && i < (length arr)

let private in_bounds str r i =
  if is_in_bounds i r then () else
  throw (Invalid (str ^ ": index " ^ show i ^ " is out of bounds"))


let make len arg =
  if len < 0 then
    throw (Invalid ("init: can't initialise rawarray of size " ^ show len))
  else
    tabulate len (fun _ -> arg)

let size = length

let init len gen =
  if len < 0 then
    throw (Invalid ("init: can't initialise rawarray of size " ^ show len))
  else
    tabulate len (fun i -> gen (i - 1))

let copy arr =
  tabulate (length arr) (geti arr)

let from_list (li : list 'a) =
  let backing = tabulate 0 (fun _ -> (error "") : 'a)
  let rec loop i = function
    | [] -> i
    | Cons (x, xs) ->
        let _ = seti backing (i + 1) x
        loop (i + 1) xs
  let _ = loop 0 li
  backing

let range len = init len (fun i -> i)

instance functor rawarray begin
  let f <$> arr = tabulate (length arr) (fun i -> f (geti arr i))
end

instance foldable rawarray begin
  let foldr func acc arr =
    let rec loop acc i =
      if i < 1 then
        acc
      else
        loop (func (geti arr i) acc) (i - 1)
    loop acc (length arr)

  let foldl func acc arr =
    let len = length arr
    let rec loop acc i =
      if i > len then
        acc
      else
        loop (func acc (geti arr i)) (i + 1)
    loop acc 1
end

instance show 'a => show (rawarray 'a) begin
  let show arr = "to_list " ^ show (to_list arr)
end


instance eq 'a => eq (rawarray 'a) begin
  let a == b =
    let elem_cmp (x : 'a) (y : 'a) = x == y
    let len_a = length a
    len_a == (length b) &&
      let rec loop (i : int) : bool =
        if i > len_a then
          true
        else
          geti a i `elem_cmp` geti b i && loop (i + 1)
      loop 1
end

let empty = empty_storage

let iter f (x : rawarray 'a) = foldl (fun () -> f) () x

let iteri f (x : rawarray 'a) = foldl (fun i x -> f i x; i + 1) 0 x

instance index (rawarray 'a) begin
  type key = int
  type value = 'a

  let ( .() ) arr i =
    in_bounds "(.())" arr i
    geti arr (i + 1)

  let ( .?() ) arr i =
    if is_in_bounds i arr then
      geti arr (i + 1) |> Some
    else None
end

instance mut_index (rawarray 'a) begin
  let ( .[]<- ) arr i x =
    in_bounds "update" arr i
    let _ = seti arr (i + 1) x
    ()
end

let append ra rb =
  let len_ra = length ra
  let new_length = len_ra + (length rb)
  tabulate new_length (fun i ->
    if i > len_ra then
      geti rb (i - len_ra)
    else
      geti ra i)