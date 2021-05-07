import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/segmented/segmented"
import "sparse"

type debug_t = (i64, i64)
let debug_ne: debug_t = (0, 0)

type~ state [n] =
  { matrix: csc_mat
  -- , const_matrix: const_mat
  , lows: [n]i64
  , arglows: [n]i64
  , nonzero_js: []i64
  }

let clear [n] (s: state[n]): state[n] =
  let lows' = scatter (copy s.lows) s.lows (replicate n (-1))
  let nonzero_js' = filter (\j -> lows'[j] != -1) s.nonzero_js
  in s with lows = lows'
       with nonzero_js = nonzero_js'

let update_lookup [n] (s: state[n]): state[n] =
  let arglows' = reduce_by_index (replicate n i64.highest)
                                 i64.min
                                 i64.highest
                                 s.lows
                                 (iota n)
                 |> map (\x -> if x == i64.highest then -1 else x)
  in s with arglows = arglows'

let add_columns [n] (s: state[n]): state[n] =
  let new_matrix = reduce_step s.matrix s.lows s.arglows s.nonzero_js
  let new_lows =
    scatter (copy s.lows)
            s.nonzero_js
            (map (low new_matrix) s.nonzero_js)
  in s with matrix = new_matrix
       with lows   = new_lows

entry is_reduced [n] (s: state[n]): bool =
  all (\j -> s.lows[j] == -1 || s.arglows[s.lows[j]] == j) (iota n)

entry iterate_step [n] (s: state[n]): state[n] =
  s |> clear |> update_lookup |> add_columns

entry init_state (col_idxs: []i32) (row_idxs: []i32) (n: i64): state[n] =
  let d = coo2_to_csc (zip col_idxs row_idxs |> sort_coo2) n
  let lows = tabulate n (low d)
  in { matrix      = d
     , lows        = lows
     , arglows     = replicate n (-1)
     , nonzero_js = filter (\j -> lows[j] != -1) (iota n)
     }

entry reduce_state [n] (s: state[n]): state[n] =
  loop s while !(is_reduced s) do
    iterate_step s

-- Takes the column indices of all nonzeroes in the original, non-reduced,
-- matrix as the first input, in order to compute the dimension of each simplex
-- (we currently don't use dimensions anywhere so they're not available).
entry persistence_intervals [n] [nnz] (col_idxs: [nnz]i32) (lows: [n]i64)
                                    : [][3]i64 =
  let nonzero_js = filter (\j -> lows[j] != -1) (iota n)
  let dims =
    reduce_by_index (replicate n 0)
                    (+)
                    0
                    (map i64.i32 col_idxs)
                    (map (const 1) col_idxs)
    |> map (\x -> if x > 0 then x - 1 else 0)

  let finite_ints =
    map (\j -> [ dims[lows[j]], lows[j], j ]) nonzero_js

  let is_low = scatter (replicate n false) lows (replicate n true)
  let esssential_ints =
    tabulate n (\j -> if lows[j] == -1 && !is_low[j]
                      then [ dims[j], j, i64.highest ]
                      else [-1, 0, 0])
    |> filter (\xs -> head xs != -1)

  in esssential_ints ++ finite_ints

entry state_nnz [n] (s: state[n]): i64 =
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

-- entry state_contents_debug [n] (s: state[n]): (i64, i64) = s.debug

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

let d4_rs: []i32 =
  [16,24,21,23,18,9,26,28,13,21,29,1,0,2,6,8,7,12,1,3,19,10,18,22,19,12,4,3,22,7,26,2,0,30,24,15,29,1,6,11,7,9,14,16,13,15,5,4,11,22,18,3,0,8,19,26,6,0,14,28,30,23,2,1,8,10,9,11,10,12,2,3,6,3]
let d4_cs: []i32 =
  [25,25,25,25,28,28,28,31,31,31,31,9,9,12,12,15,15,15,18,18,21,21,21,24,24,24,27,27,30,30,30,8,8,33,33,33,33,11,11,14,14,14,17,17,17,17,20,20,23,23,23,26,26,29,29,29,7,7,32,32,32,32,10,10,13,13,13,16,16,16,19,19,22,22]
let d4_n: i64 = 34


