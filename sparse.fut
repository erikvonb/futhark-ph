import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/sorts/radix_sort"

type~ csc_mat =
  { col_offsets: []i64
  , col_lengths: []i64
  , row_idxs: []i32
  -- , values: []i32
  }

-- Append-only matrix type, for storing the reduced columns
type~ const_mat =
  { col_offsets: []i64
  , row_idxs: []i32
  -- Could be a hash map?
  , col_idx_map: []i64
  , map_ptr: i64
  }

let empty_const_mat (n: i64): const_mat =
  { col_offsets = [0]
  , row_idxs = []
  , col_idx_map = replicate n (-1)
  , map_ptr = 0
  }

-- (j, i); column index, row index
type coo2_mat [n] = [n](i32, i32)

let csc_col_nnz (d: csc_mat) (j: i64): i64 =
  d.col_lengths[j]

let sort_coo2 [n] (d: coo2_mat[n]): coo2_mat[n] =
  radix_sort
    64
    (\bit (j,i) -> i64.get_bit bit <| (i64.i32 j << 32) + i64.i32 i)
    d

let low (d: csc_mat) (j: i64): i64 =
  if csc_col_nnz d j == 0
    then -1
    else i64.i32 d.row_idxs[ d.col_offsets[j] + d.col_lengths[j] - 1 ]
  
let coo2_to_csc [n] (d: coo2_mat[n]) (n_cols: i64): csc_mat =
  let col_idxs = map i64.i32 (unzip2 d).0
  let row_idxs = (unzip2 d).1

  let col_lengths =
    reduce_by_index (replicate n_cols 0) (+) 0 col_idxs (map (const 1) col_idxs)
  let col_offsets = [0] ++ init (scan (+) 0 col_lengths)

  in { col_offsets = col_offsets, col_lengths = col_lengths, row_idxs = row_idxs }

let csc_to_coo2 (d: csc_mat): [](i32, i32) =
  let n = length d.col_lengths
  in expand (\j -> d.col_lengths[j])
            (\j k -> ( i32.i64 j, d.row_idxs[d.col_offsets[j] + k]))
            (iota n)

-- Creates a new CSC matrix, initialising all arrays and settings offsets such
-- that there is guaranteed space for all new columns after one iteration of
-- reduction.  Sets column lengths to the maximum possible. Does not set any
-- row indices.
let init_new_matrix [n] (d: csc_mat) (lows: [n]i64) (arglows: [n]i64): csc_mat =
  let new_col_lengths_bounds =
    tabulate n (\j ->
      if lows[j] != -1 && arglows[lows[j]] != j
      then d.col_lengths[j] + d.col_lengths[arglows[lows[j]]] - 2
      else d.col_lengths[j])
  let new_col_offsets = [0] ++ init (scan (+) 0 new_col_lengths_bounds)
  let new_row_idxs = replicate (i64.sum new_col_lengths_bounds) (-1)
  in { col_offsets = new_col_offsets
     , col_lengths = new_col_lengths_bounds
     , row_idxs    = new_row_idxs
     }

-- TODO we can avoid the copy in scatter by moving this into init_new_matrix
let copy_columns (js: []i64) (d1: csc_mat) (d2: csc_mat): csc_mat =
  let (is, as) =
    expand (\j -> d1.col_lengths[j])
           (\j k -> (d2.col_offsets[j] + k, d1.row_idxs[d1.col_offsets[j] + k]))
           js
    |> unzip
  let new_row_idxs = scatter (copy d2.row_idxs) is as
  -- The non-changing columns should have been allocated exactly their size, so
  -- the maximum column length that was set in `init_new_matrix` will be
  -- correct.
  in d2 with row_idxs = new_row_idxs
  
let add_pairs [n0] (left_right_pairs: [n0](i64, i64)) (d1: csc_mat) (d2: csc_mat): csc_mat =
  let update_idxs = (unzip left_right_pairs).1
  let pxs = replicate n0 (0: i64)
  let pys = replicate n0 (0: i64)
  let pzs = replicate n0 (0: i64)
  let offsets = map (\j -> d2.col_offsets[j]) update_idxs
  -- At this point `d2.col_lengths` should be the maximum possible, i.e. the
  -- upper bound of the column length.
  let bounds = map (\j -> d2.col_lengths[j]) update_idxs
  let row_idxs = copy d2.row_idxs

  let (row_idxs,_,_,pzs_final) = loop (row_idxs, pxs, pys, pzs) for i < i64.maximum bounds do
    let pairs =
      tabulate n0 (\j ->
        let (u,v) = left_right_pairs[j]
        let x = if pxs[j] < d1.col_lengths[u]
                then d1.row_idxs[d1.col_offsets[u] + pxs[j]]
                else i32.highest
        let y = if pys[j] < d1.col_lengths[v]
                then d1.row_idxs[d1.col_offsets[v] + pys[j]]
                else i32.highest
        in (x,y))

    let row_idxs' =
      scatter row_idxs
              (tabulate n0 (\j -> if i < bounds[j]
                                 then offsets[j] + pzs[j]
                                 else -1))
              (tabulate n0 (\j -> let (x,y) = pairs[j]
                                 in   if x < y then x
                                 else if y < x then y
                                 else -1))

    let (pxs', pys', pzs') =
      (iota n0) |> map (\j -> let (x,y) = pairs[j]
                              in if x == y  then (pxs[j]+1, pys[j]+1, pzs[j])
                              else if x < y then (pxs[j]+1, pys[j], pzs[j]+1)
                              else               (pxs[j], pys[j]+1, pzs[j]+1))
                |> unzip3
    in (row_idxs', pxs', pys', pzs')

  -- Set the final column lengths of the merged columns.
  let new_col_lengths =
    scatter (copy d2.col_lengths) (update_idxs :> [n0]i64) pzs_final

  in d2 with col_lengths = new_col_lengths
        with row_idxs    = row_idxs

let reduce_step [n] (d: csc_mat) (lows: [n]i64) (arglows: [n]i64) (nonzero_js: []i64): csc_mat =
  let (update_idxs, const_idxs) =
    partition (\j -> arglows[lows[j]] != j) nonzero_js

  -- (j,k) ∈ left_right_pairs iff we should assign d_k <- d_k + d_j,
  -- i.e. column j is to the left of k and we should add j to k.
  let left_right_pairs =
    map (\j -> (arglows[lows[j]], j)) update_idxs

  let new_matrix = init_new_matrix d lows arglows
                   |> copy_columns const_idxs d
                   |> add_pairs left_right_pairs d

  in new_matrix

-- 0 0 1 0
-- 0 0 1 1
-- 0 0 0 1
-- 0 0 0 0
let d0: csc_mat =
  { col_offsets = [0, 0, 0, 2]
  , col_lengths = [0, 0, 2, 2]
  , row_idxs = [0, 1, 1, 2]
  }

let d0_lows: []i64 = [-1, -1, 1, 2]
let d0_arglows: []i64 = [-1, 2, 3, -1]


-- 0 0 1 0 0
-- 0 0 0 1 1
-- 0 0 1 1 1
-- 0 0 0 0 1
-- 0 0 0 0 0
let d1: csc_mat =
  { col_offsets = [0, 0, 0, 2, 4]
  , col_lengths = [0, 0, 2, 2, 3]
  , row_idxs = [0, 2, 1, 2, 1, 2, 3]
  }

let d1_lows: []i64 = [-1, -1, 2, 2, 3]
let d1_arglows: []i64 = [-1, -1, 2, 4, -1]

