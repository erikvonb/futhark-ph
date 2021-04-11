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

let get_csc_col (d: csc_mat) (j: i64): []i32 =
  d.row_idxs[d.col_offsets[j] : d.col_offsets[j+1]]

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
  let col_offsets = [0] ++ scan (+) 0 col_lengths

  in { col_offsets = col_offsets, col_lengths = col_lengths, row_idxs = row_idxs }

let csc_to_coo2 (d: csc_mat): [](i32, i32) =
  let n = length d.col_offsets - 1
  in expand (\j -> d.col_offsets[j + 1] - d.col_offsets[j])
            (\j k -> ( i32.i64 j, d.row_idxs[d.col_offsets[j] + k]))
            (iota n)

let reduce_step [n] (d: csc_mat) (lows: [n]i64) (arglows: [n]i64): csc_mat =

  let will_change = map (\j -> lows[j] != -1 && arglows[lows[j]] != j) (iota n)
  
  let (update_idxs, const_idxs) =
    (iota n) |> filter (\j -> lows[j] != -1)
             |> partition (\j -> will_change[j])

  -- (j,k) ∈ left_right_pairs iff we should assign d_k <- d_k + d_j,
  -- i.e. column j is to the left of k and we should add j to k.
  let left_right_pairs =
    map (\j -> (arglows[lows[j]], j)) update_idxs

  let new_col_lengths_bounds =
    map (\j -> if will_change[j]
               then d.col_lengths[j] + d.col_lengths[arglows[lows[j]]] - 2
               else d.col_lengths[j])
        (iota n)

  let new_col_offsets = [0] ++ scan (+) 0 new_col_lengths_bounds
  let row_idxs = replicate (i64.sum new_col_lengths_bounds) (-1)

  -- Copy the columns that will not change.
  let (is, as) =
    expand (\j -> d.col_lengths[j])
           (\j k -> (new_col_offsets[j] + k, d.row_idxs[d.col_offsets[j] + k]))
           const_idxs
    |> unzip
  let row_idxs = scatter row_idxs is as

  -- Merge columns into those that will change.  Let j ∈ update_idxs be the
  -- index of a column in the sum matrix we're about to construct. Then pxs[j]
  -- and pys[j] are the two index pointers used for merging the two columns in
  -- left_right_pairs[j]; If left_right_pairs[j] = (u,v), then pxs[j] is the
  -- index of the next value in column u to be merged and pys[j] the next value
  -- in column v to be merged. Further, offsets[j] is the column offset of
  -- column j into the final CSC matrix. From the code above, bounds[j] is the
  -- size allocated to column j in the final matrix.
  let n0 = length left_right_pairs
  let pxs = replicate n0 (0: i64)
  let pys = replicate n0 (0: i64)
  let pzs = replicate n0 (0: i64)
  let offsets =
    (iota n) |> filter (\j -> will_change[j])
             |> map (\j -> new_col_offsets[j])
             :> [n0]i64
  let bounds =
    (iota n) |> filter (\j -> will_change[j])
             |> map (\j -> new_col_lengths_bounds[j])

  let (row_idxs,_,_,pzs_final) = loop (row_idxs, pxs, pys, pzs) for i < i64.maximum bounds do
    let pairs =
      map (\j -> let (u,v) = left_right_pairs[j]
                 let x = if pxs[j] < d.col_lengths[u] then d.row_idxs[d.col_offsets[u] + pxs[j]] else i32.highest
                 let y = if pys[j] < d.col_lengths[v] then d.row_idxs[d.col_offsets[v] + pys[j]] else i32.highest
                 in (x,y))
          (iota n0)

    let row_idxs' =
      scatter row_idxs
              (map (\j -> if i < bounds[j] then offsets[j] + pzs[j] else -1)
                   (iota n0))
              (map (\j -> let (x,y) = pairs[j]
                          in if      x < y then x
                             else if y < x then y
                             else -1)
                   (iota n0))
                
    let (pxs', pys', pzs') =
      (iota n0) |> map (\j -> let (x,y) = pairs[j]
                              in if x == y  then (pxs[j]+1, pys[j]+1, pzs[j])
                              else if x < y then (pxs[j]+1, pys[j], pzs[j]+1)
                              else               (pxs[j], pys[j]+1, pzs[j]+1))
                |> unzip3

    in (row_idxs', pxs', pys', pzs')

  -- Set the final column lengths of the merged columns.
  let new_col_lengths =
    scatter (copy d.col_lengths) (update_idxs :> [n0]i64) pzs_final

  in { col_offsets = new_col_offsets
     , col_lengths = new_col_lengths
     , row_idxs = row_idxs
     }

-- 0 0 1 0
-- 0 0 1 1
-- 0 0 0 1
-- 0 0 0 0
let d0: csc_mat =
  { col_offsets = [0, 0, 0, 2, 4]
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
  { col_offsets = [0, 0, 0, 2, 4, 7]
  , col_lengths = [0, 0, 2, 2, 3]
  , row_idxs = [0, 2, 1, 2, 1, 2, 3]
  }

let d1_lows: []i64 = [-1, -1, 2, 2, 3]
let d1_arglows: []i64 = [-1, -1, 2, 4, -1]

