import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/segmented/segmented"
import "sparse"

type~ state [n] =
  { matrix: csc_mat
  -- , const_matrix: const_mat
  , lows: [n]i64
  , arglows: [n]i64
  , iteration: i32
  , debug: i64
  }

-- let move_pivots_to_const [n] (s: state[n]): state[n] =
  -- -- TODO these should only be drawn from the indices of the non-constant matrix
  -- let pivot_idxs =
    -- (iota n)
    -- -- Keep index j only if it is nonempty in the dynamic matrix and a pivot
    ---|> map (\j -> if s.lows[j] != -1
                     -- && s.arglows[s.lows[j]] == j then j else -1)
    ---|> filter (> -1)

  -- let new_const_col_offsets =
    -- pivot_idxs
    ---|> map (csc_col_nnz s.matrix)
    ---|> scan (+) 0
    ---|> map (+ last s.const_matrix.col_offsets)

  -- let new_const_row_idxs =
    -- expand (csc_col_nnz s.matrix)
           -- (\j k -> s.matrix.row_idxs[s.matrix.col_offsets[j] + k])
           -- pivot_idxs

  -- let new_const_col_idx_map =
    -- scatter (copy s.const_matrix.col_idx_map)
            -- pivot_idxs
            -- (map (+ s.const_matrix.map_ptr) (indices pivot_idxs))

  -- let new_const_matrix =
    -- s.const_matrix with col_offsets = s.const_matrix.col_offsets ++ new_const_col_offsets
                   -- with row_idxs = s.const_matrix.row_idxs ++ new_const_row_idxs
                   -- with col_idx_map = new_const_col_idx_map
                   -- with map_ptr = s.const_matrix.map_ptr + length pivot_idxs

  -- in s with const_matrix = new_const_matrix

let beta_j (lefts: []i64) (j: i64): i64 =
  last_occurrence lefts (==j)

let phase_1 [n] (s: state[n]): state[n] =
  let arglows' = reduce_by_index (replicate n i64.highest)
                                 i64.min
                                 i64.highest
                                 s.lows
                                 (iota n)
                 |> map (\x -> if x == i64.highest then -1 else x)
  in s with arglows = arglows'

-- Clears the columns low(j) for all j
let clear [n] (s: state[n]): state[n] =
  let lows' = scatter (copy s.lows) s.lows (replicate n (-1))
  in s with lows = lows'

let phase_2 [n] (s: state[n]): state[n] =
  let can_be_reduced =
    map (\j -> s.lows[j] != -1 && s.arglows[s.lows[j]] != j) (iota n)

  let new_matrix = reduce_step s.matrix s.lows s.arglows

  let new_lows =
    map (\j -> if can_be_reduced[j] then low new_matrix j else s.lows[j])
        (iota n)

  -- Clear low(j) for all non-pivots j that were just changed by the reduction step;
  -- like the `clear` function, but only for the newly reduced ones
  let idxs_to_clear = map (\j -> if can_be_reduced[j] then new_lows[j] else -1) (iota n)
  let new_lows' = scatter (copy new_lows) idxs_to_clear (replicate n (-1))

  in s with matrix = new_matrix
       with lows = new_lows'

entry is_reduced [n] (s: state[n]): bool =
  all (\j -> s.lows[j] == -1 || s.arglows[s.lows[j]] == j) (iota n)

entry iterate_step [n] (s: state[n]): state[n] =
  let s = s |> clear |> phase_1 |> phase_2
  in s with iteration = s.iteration + 1

entry init_state (col_idxs: []i32) (row_idxs: []i32) (n: i64): state[n] =
  let d = coo2_to_csc (zip col_idxs row_idxs |> sort_coo2) n
  in { matrix      = d
     -- , const_matrix    = empty_const_mat n
     , lows        = map (low d) (iota n)
     , arglows     = replicate n (-1)
     , iteration = 0
     , debug = 0
     }

entry state_nnz [n] (s: state[n]): i64 =
  -- length <| s.matrix.row_idxs
  s.matrix.row_idxs |> map (\i -> if i > -1 then 1 else 0) |> reduce (+) 0

let count 't (xs: []t) (p: t -> bool): i32 =
  xs |> map (\x -> if p x then 1 else 0) |> reduce (+) 0

entry state_n_zero_cols [n] (s: state[n]): i32 =
  count s.lows (== -1)

entry state_n_additions_available [n] (s: state[n]): i32 =
  count (iota n) (\j -> s.lows[j] != -1 && s.arglows[s.lows[j]] != j)

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

let d3_cs: []i32 = [6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 13, 14, 14, 14]
let d3_rs: []i32 = [0, 3, 1, 5, 2, 4, 0, 5,  3,  4,  0,  2,  0,  4,  8, 11, 12,  6, 10, 12]
let d3_n: i64 = 15
-- ......1..1.11..
-- .......1.......
-- ........1..1...
-- ......1...1....
-- ........1.1.1..
-- .......1.1.....
-- ..............1
-- ...............
-- .............1.
-- ...............
-- ..............1
-- .............1.
-- .............11
-- ...............
-- ...............

