import "lib/github.com/diku-dk/sorts/radix_sort"

type~ state [n] =
  { matrix: [n][n]i32
  , classes: [n]i32
  , lows: [n]i64
  , lefts: [n]i64
  , arglows: [n]i64
  , is_positive: [n]bool
  }

let dotprod [n] (xs: [n]i32) (ys: [n]i32): i32 =
  reduce (+) 0 (map2 (*) xs ys)

-- OBS: this is for row-major, but we're col-major atm. Flip order to get it
-- right.
let matmul [n][p][m] (xss: [n][p]i32) (yss: [p][m]i32): [n][m]i32 =
  map (\xs -> map (dotprod xs) (transpose yss)) xss

let last_occurrence [n] 't (xs: [n]t) (pred: t -> bool): i64 =
  let is = map (\i -> if pred xs[i] then i else -1) (iota n)
  in reduce i64.max (-1) is

let first_occurrence [n] 't (xs: [n]t) (pred: t -> bool): i64 =
  let i = last_occurrence (reverse xs) pred
  in if i == -1 then -1 else n - 1 - i

let left [n] (d: [n][n]i32) (i: i64): i64 =
  first_occurrence d[:,i] (==1)

let low (c: []i32): i64 =
  last_occurrence c (==1)

let beta_j [n] (d: [n][n]i32) (j: i64): i64 =
  let lefts = map (left d) (iota n)
  in last_occurrence lefts (==j)

let phase_0 [n] (s: state[n]): state[n] =
  let betas = map (beta_j s.matrix) (iota n)

  -- If low(j) == β_j, mark j as negative
  let idxs = map (\j -> if betas[j] == s.lows[j] then j else -1) (iota n)
  let classes' = scatter (copy s.classes) idxs (replicate n (-1))
  -- Alternatively:
  -- let classes' =
    -- map (\j -> if betas[j] == s.lows[j] then -1 else s.classes[j]) (iota n)

  -- If low(j) == β_j, mark low(j) as positive and clear it
  let idxs' = map (\j -> if betas[j] == s.lows[j] then s.lows[j] else -1) (iota n)
  let classes'' = scatter (copy classes') idxs' (replicate n 1)
  let lows' = scatter (copy s.lows) idxs' (replicate n (-1))
  let arglows' = scatter (copy s.arglows) idxs' (iota n)
  -- the actual clearing is unnecessary for the algorithm to work
  let matrix' = scatter (copy s.matrix) idxs' (replicate n (replicate n 0))

  in s with classes = classes''
       with lows = lows'
       with arglows = arglows'
       with matrix = matrix'

let phase_1 [n] (s: state[n]): state[n] =
  -- TODO confirm that radix_sort_by_key is stable
  let (sorted_lows, sorted_js) = unzip <|
    radix_sort_by_key (.0) 64 i64.get_bit (zip s.lows (iota n))

  -- If sorted_lows[j] == sorted_lows[j-1], map it to -1;
  -- keeps only those lows that are not unique, so those that have "collided"
  let collisions_sorted =
    map2 (\x y -> if x == y then x else -1)
         sorted_lows
         (rotate (-1) sorted_lows)
  -- Unsort them
  let collisions = scatter (replicate n 0) sorted_js collisions_sorted
  -- The low of a column has to be larger than this to be a pivot
  -- Ex: [2, 3, 3, 4, 1] -> [-1, -1, 3, 3, 3]
  let max_collisions = scan i64.max i64.lowest collisions

  -- Flag/mark j ∈ sorted_js if it's a potential pivot
  -- Ex: [2, 3, 3, 4, 1] -> [T, T, F, T, T]
  let is_potential_pivot = map2 (!=) sorted_lows (rotate (-1) sorted_lows)

  -- j ∈ pivot_js is either the index of a pivot, or -1
  -- Ex: [2, 3, 3, 4, 1] -> [T, T, F, T, F]
  -- 2, 1st 3, 4 are pivots because they're leftmost and > max_collision.
  -- 2nd 3 is not a pivot because it's not leftmost, so not potential pivot.
  -- 1 is leftmost but is not > max_collision = 3, so it's not a pivot.
  let pivot_js = 
    map (\j -> if is_potential_pivot[j] && sorted_lows[j] > max_collisions[j]
               then sorted_js[j]
               else -1)
        (iota n)
  -- low(pivot_js) elementwise
  let pivot_low_js = map (\j -> if j == -1 then -1 else s.lows[j]) pivot_js

  let arglows' = scatter (copy s.arglows) pivot_low_js pivot_js
  let classes' = scatter (copy s.classes) pivot_js (replicate n (-1))
  let is_positive'= scatter (copy s.is_positive) pivot_low_js (replicate n true)
                        
  in s with arglows = arglows'
       with classes = classes'
       with is_positive = is_positive'

let phase_2 [n] (s: state[n]): state[n] =
  let can_be_reduced =
    map (\j -> s.lows[j] != -1 && s.arglows[s.lows[j]] != j) (iota n)

  -- In col-major notation, v[j,i] = a means we add a*v[i] to v[j]
  let v = trace <| tabulate_2d n n
    (\j i -> if i == j then 1
             else let low_j = s.lows[j]
             in if low_j > -1 && i == s.arglows[low_j] then 1 else 0)

  let new_matrix = (v `matmul` s.matrix) |> map (map (% 2))

  -- let new_lows = map low new_matrix
  let new_lows =
    map (\j -> if can_be_reduced[j] then low new_matrix[j] else s.lows[j])
        (iota n)

  -- Identify new pivots and clear
  let is_new_pivot =
    map (\j -> can_be_reduced[j] && j == s.lefts[new_lows[j]]) (iota n)
  let new_classes =
    map (\j -> if is_new_pivot[j] then -1 else s.classes[j]) (iota n)

  let new_pivot_is = map (\j -> if is_new_pivot[j] then j else -1) (iota n)
  let new_arglow_is = map (\j -> if j > -1 then new_lows[j] else -1) new_pivot_is
  let new_arglows = scatter (copy s.arglows) new_arglow_is (iota n)

  let new_positive_is = map (\j -> if can_be_reduced[j] then new_lows[j] else -1) (iota n)
  let new_is_positive = scatter (copy s.is_positive) new_positive_is (replicate n true)

  in s with matrix = new_matrix
       with lows = new_lows
       with classes = new_classes
       with is_positive = new_is_positive
       with arglows = new_arglows

let clear_positives [n] (s: state[n]): state[n] =
  let lows' =
    map (\j -> if s.is_positive[j] then -1 else s.lows[j]) (iota n)
  let classes' =
    map (\j -> if s.is_positive[j] then 1 else s.classes[j]) (iota n)
  in s with lows = lows'
       with classes = classes'

let is_reduced [n] (s: state[n]): bool =
  all (\j -> s.lows[j] == -1 || s.arglows[s.lows[j]] == j) (iota n)

let initialise_state [n] (d: [n][n]i32): state[n] =
  { matrix = d
  , classes = replicate n 0
  , lows = map low d
  , lefts = map (left d) (iota n)
  , arglows = replicate n (-1)
  , is_positive = replicate n false
  }

let reduce_state [n] (s_init: state[n]): state[n] =
  let s0 = s_init
  let s1 = phase_0 s0

  let final_state = (.1) <|
      loop (converged, s) = (false, s1) while !converged do

    let prev_lows = s.lows

    let s0 = s with is_positive = replicate n false
    let s1 = s0 |> phase_1 |> clear_positives

    let s2 = s1 with is_positive = replicate n false
    let s3 = s2 |> phase_2 |> clear_positives

    -- TODO we may need the is_reduced test, if lows don't always decrease
    let converged = is_reduced s3
    -- let converged = prev_lows == s4.lows

    in (converged, s3)
      
  in final_state

entry reduce_matrix [n] (matrix: [n][n]i32): ([n][n]i32, [n]i64) =
  let s = initialise_state matrix |> reduce_state
  in ( s.matrix
     , s.lows
     )

let d0: [][]i32 = transpose
 [[0,0,0,0,0,0,0,0,1,0],
  [0,0,0,0,0,0,0,1,0,0],
  [0,0,0,0,0,0,1,0,1,1],
  [0,0,0,0,0,0,1,1,0,0],
  [0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,1],
  [0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0]]

let d1: [][]i32 = transpose
 [[0,0,0,0,1,0,1,0],
  [0,0,0,0,0,1,1,0],
  [0,0,0,0,1,1,0,0],
  [0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,1],
  [0,0,0,0,0,0,0,1],
  [0,0,0,0,0,0,0,1],
  [0,0,0,0,0,0,0,0]]

let s0 = initialise_state d0

