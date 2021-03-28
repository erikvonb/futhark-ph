import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/segmented/segmented"
import "sparse"

type~ state [n] =
  { matrix: csc_mat
  , const_matrix: const_mat
  , lows: [n]i64
  , lefts: [n]i64
  , arglows: [n]i64
  , is_positive: [n]bool
  , iteration: i32
  , debug: i64
  }

let move_pivots_to_const [n] (s: state[n]): state[n] =
  -- TODO these should only be drawn from the indices of the non-constant matrix
  let pivot_idxs =
    (iota n)
    -- Keep index j only if it is nonempty in the dynamic matrix and a pivot
    |> map (\j -> if s.lows[j] != -1
                     && s.arglows[s.lows[j]] == j then j else -1)
    |> filter (> -1)

  let new_const_col_offsets =
    pivot_idxs
    |> map (csc_col_nnz s.matrix)
    |> scan (+) 0
    |> map (+ last s.const_matrix.col_offsets)

  let new_const_row_idxs =
    expand (csc_col_nnz s.matrix)
           (\j k -> s.matrix.row_idxs[s.matrix.col_offsets[j] + k])
           pivot_idxs

  let new_const_col_idx_map =
    scatter (copy s.const_matrix.col_idx_map)
            pivot_idxs
            (map (+ s.const_matrix.map_ptr) (indices pivot_idxs))

  let new_const_matrix =
    s.const_matrix with col_offsets = s.const_matrix.col_offsets ++ new_const_col_offsets
                   with row_idxs = s.const_matrix.row_idxs ++ new_const_row_idxs
                   with col_idx_map = new_const_col_idx_map
                   with map_ptr = s.const_matrix.map_ptr + length pivot_idxs

  in s with const_matrix = new_const_matrix

let beta_j (lefts: []i64) (j: i64): i64 =
  last_occurrence lefts (==j)

-- let betas_and_lefts (col_idxs: []i32) (row_idxs: []i32) (n: i64):
                    -- ([]i64, []i64) =
  -- let nnz = length col_idxs
  -- let (betas, lefts, _) =
    -- loop (betas, lefts, visited)
         -- = (replicate n (-1), replicate n (-1), replicate n false)
         -- for k < nnz do
      -- let j = col_idxs[k]
      -- let i = row_idxs[k]
      -- in if !visited[i]
         -- then let betas[j] = if betas[j] > i then betas[j] else i
              -- let visited[i] = true
              -- let lefts[i] = j
              -- in (betas, lefts, visited)
         -- else (betas, lefts, visited)
  -- in (map i64.i32 betas, map i64.i32 lefts)

let phase_0 [n] (s: state[n]): state[n] =
  let betas = map (beta_j s.lefts) (iota n)

  -- If low(j) == β_j, mark low(j) as positive and clear it
  let idxs' =
    map (\j -> if betas[j] == s.lows[j] then s.lows[j] else -1)
        (iota n)
  let lows' = scatter (copy s.lows) idxs' (replicate n (-1))
  let arglows' = scatter (copy s.arglows) idxs' (iota n)

  in s with lows = lows'
       with arglows = arglows'

let phase_1 [n] (s: state[n]): state[n] =
  let (sorted_lows, sorted_js) = unzip <|
    radix_sort_by_key (.0) 64 i64.get_bit (zip s.lows (iota n))

  -- Flag/mark j ∈ sorted_js if it's a potential pivot
  -- Ex: [2, 3, 3, 4, 1] -> [T, T, F, T, T]
  let is_potential_pivot = map2 (!=) sorted_lows (rotate (-1) sorted_lows)

  -- j ∈ pivot_js is either the index of a pivot, or -1
  -- Ex: [2, 3, 3, 4, 1] -> [T, T, F, T, F]
  -- 2, 1st 3, 4 are pivots because they're leftmost and > max_collision.
  -- 2nd 3 is not a pivot because it's not leftmost, so not potential pivot.
  -- 1 is leftmost but is not > max_collision = 3, so it's not a pivot.
  let pivot_js = 
    map (\j -> if is_potential_pivot[j]
               then sorted_js[j]
               else -1)
        (iota n)

  -- low(pivot_js) elementwise
  let pivot_low_js = map (\j -> if j == -1 then -1 else s.lows[j]) pivot_js

  let arglows' = scatter (copy s.arglows) pivot_low_js pivot_js
  let is_positive'= scatter (copy s.is_positive) pivot_low_js (replicate n true)
                        
  in s with arglows = arglows'
       with is_positive = is_positive'

let phase_2 [n] (s: state[n]): state[n] =
  let can_be_reduced =
    map (\j -> s.lows[j] != -1 && s.arglows[s.lows[j]] != j) (iota n)

  let new_matrix = reduce_step s.matrix s.lows s.arglows s.iteration

  let new_lows =
    -- map (\j -> if can_be_reduced[j] then low new_matrix j else s.lows[j])
        -- (iota n)
    map (low new_matrix) (iota n)

  -- Identify new pivots and clear
  let is_new_pivot =
    map (\j -> can_be_reduced[j]
               && new_lows[j] > -1
               && j == s.lefts[new_lows[j]])
        (iota n)

  let new_pivot_is = map (\j -> if is_new_pivot[j] then j else -1) (iota n)
  let new_arglow_is = map (\j -> if j > -1 then new_lows[j] else -1) new_pivot_is
  let new_arglows = scatter (copy s.arglows) new_arglow_is (iota n)

  let new_positive_is = map (\j -> if can_be_reduced[j] then new_lows[j] else -1) (iota n)
  let new_is_positive = scatter (copy s.is_positive) new_positive_is (replicate n true)

  in s with matrix = new_matrix
       with lows = new_lows
       with is_positive = new_is_positive
       with arglows = new_arglows

let clear_positives [n] (s: state[n]): state[n] =
  let lows' =
    map (\j -> if s.is_positive[j] then -1 else s.lows[j]) (iota n)
  in s with lows = lows'

entry is_reduced [n] (s: state[n]): bool =
  all (\j -> s.lows[j] == -1 || s.arglows[s.lows[j]] == j) (iota n)

let reset_positives [n] (s: state[n]): state[n] =
  s with is_positive = replicate n false

entry iterate_step [n] (s: state[n]): state[n] =
  let s = s |> reset_positives |> phase_1 |> clear_positives
            ---|> move_pivots_to_const
            |> reset_positives |> phase_2 |> clear_positives
  in s with iteration = s.iteration + 1

entry init_state (col_idxs: []i32) (row_idxs: []i32) (n: i64): state[n] =
  -- let (betas, lefts) = betas_and_lefts col_idxs row_idxs n
  let d = coo2_to_csc (zip col_idxs row_idxs |> sort_coo2) n
  in { matrix      = d
     , const_matrix    = empty_const_mat n
     , lows        = map (low d) (iota n)
     , lefts       = map (left d) (iota n)
     , arglows     = replicate n (-1)
     , is_positive = replicate n false
     , iteration = 0
     , debug = 0
     }
     |> phase_0

entry state_nnz [n] (s: state[n]): i64 =
  -- length <| s.matrix.row_idxs
  s.matrix.row_idxs |> map (\i -> if i > -1 then 1 else 0) |> reduce (+) 0

let count 't (xs: []t) (p: t -> bool): i32 =
  xs |> map (\x -> if p x then 1 else 0) |> reduce (+) 0

entry state_n_zero_cols [n] (s: state[n]): i32 =
  count s.lows (== -1)

entry state_n_additions_available [n] (s: state[n]): i32 =
  count s.lows (\l -> l != -1 && s.arglows[l] != l)
  -- s.lows |> map (\l -> if l == -1 || s.arglows[l] == l then 0 else 1)
         -- |> reduce (+) 0
  -- let sorted_lows = radix_sort 64 i64.get_bit s.lows
  -- let segments = map2 (==) sorted_lows (rotate (-1) sorted_lows)
  -- in (iota n) |> map (\j -> if s.lows[j] == -1 || segments[j] then 0 else 1)
              -- |> reduce (+) 0

entry state_matrix_coo [n] (s: state[n]): ([]i32, []i32) =
  let (col_idxs, row_idxs) = s.matrix |> csc_to_coo2 |> filter (\(_,i) -> i != -1) |> unzip
  in (col_idxs, row_idxs)

entry state_lows [n] (s: state[n]): []i64 =
  s.lows

entry state_contents_debug [n] (s: state[n]): i64 = s.debug

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

let d1_co: []i64 = [0, 0, 0, 0, 0, 2, 4, 6, 9]
let d1_ri: []i32 = [0, 2,  1, 2,  0, 1,  4, 5, 6]

-- ../datasets/sparse/test.txt
let d2_cs: []i32 = [3, 4, 3, 5, 4, 5, 6, 6, 6]
let d2_rs: []i32 = [0, 0, 1, 1, 2, 2, 3, 4, 5]
let d2_n: i64 = 7
-- . . . 1 1 . .
-- . . . 1 . 1 .
-- . . . . 1 1 .
-- . . . . . . 1
-- . . . . . . 1
-- . . . . . . 1
-- . . . . . . .

