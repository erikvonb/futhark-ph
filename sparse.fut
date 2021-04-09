import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/sorts/radix_sort"

type~ csc_mat =
  { col_offsets: []i64
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
  d.col_offsets[j+1] - d.col_offsets[j]

let sort_coo2 [n] (d: coo2_mat[n]): coo2_mat[n] =
  radix_sort
    64
    (\bit (j,i) -> i64.get_bit bit <| (i64.i32 j << 32) + i64.i32 i)
    d

let low (d: csc_mat) (j: i64): i64 =
  if csc_col_nnz d j == 0
    then -1
    else i64.i32 d.row_idxs[ d.col_offsets[j+1] - 1 ]
  
let coo2_to_csc [n] (d: coo2_mat[n]) (n_cols: i64): csc_mat =
  let col_idxs = (unzip2 d).0
  let row_idxs = (unzip2 d).1
  let segments = map2 (!=) (rotate (-1) col_idxs) col_idxs

  let (elems_per_col, col_idxs) =
    zip (map (const 1) col_idxs) col_idxs
    |> segmented_reduce
         (\(x1,j1) (x2,j2) -> (x1 + x2, i32.min j1 j2))
         (0, i32.highest)
         segments
    |> unzip

  let col_offsets =
    scatter (replicate (n_cols + 1) 0) (map ((+1) <-< i64.i32) col_idxs) elems_per_col
    |> scan (+) 0

  in { col_offsets = col_offsets, row_idxs = row_idxs }

let csc_to_coo2 (d: csc_mat): [](i32, i32) =
  let n = length d.col_offsets - 1
  in expand (\j -> d.col_offsets[j + 1] - d.col_offsets[j])
            (\j k -> ( i32.i64 j, d.row_idxs[d.col_offsets[j] + k]))
            (iota n)

let reduce_step [n] (d: csc_mat) (lows: [n]i64) (arglows: [n]i64): csc_mat =

  let col_sizes = init <| map2 (-) (rotate 1 d.col_offsets) d.col_offsets
  let will_change = map (\j -> lows[j] != -1 && arglows[lows[j]] != j) (iota n)
  
  let (update_idxs, const_idxs) =
    (iota n) |> filter (\j -> lows[j] != -1)
             |> partition (\j -> will_change[j])

  -- (j,k) ∈ left_right_pairs iff we should assign d_k <- d_k + d_j,
  -- i.e. column j is to the left of k and we should add j to k.
  let left_right_pairs =
    map (\j -> (arglows[lows[j]], j)) update_idxs

  let new_col_sizes =
    map (\j -> if will_change[j]
               then col_sizes[j] + col_sizes[arglows[lows[j]]] - 2
               else col_sizes[j])
        (iota n)

  let col_offsets = [0] ++ scan (+) 0 new_col_sizes
  let row_idxs = replicate (i64.sum new_col_sizes) (-1)

  -- Copy the columns that will not change.
  let (is, as) =
    expand (csc_col_nnz d)
           (\j k -> (col_offsets[j] + k, d.row_idxs[d.col_offsets[j] + k]))
           const_idxs
    |> unzip
  let row_idxs = scatter row_idxs is as

  -- Merge columns into those that will change.  Let j ∈ update_idxs be the
  -- index of a column in the sum matrix we're about to construct. Then ks[j]
  -- and ls[j] are the two index pointers used for merging the two columns in
  -- left_right_pairs[j]; If left_right_pairs[j] = (u,v), then ks[j] is the
  -- index of the next value in column u to be merged and ls[j] the next value
  -- in column v to be merged. Further, offsets[j] is the column offset of
  -- column j into the final CSC matrix. From the code above, bounds[j] is the
  -- size allocated to column j in the final matrix.
  let n0 = length left_right_pairs
  let ks = replicate n0 0
  let ls = replicate n0 0
  let offsets =
    (iota n) |> filter (\j -> will_change[j])
             |> map (\j -> col_offsets[j])
             :> [n0]i64
  let bounds =
    (iota n) |> filter (\j -> will_change[j])
             |> map (\j -> new_col_sizes[j])

  let (row_idxs,_,_) = loop (row_idxs, ks, ls) for i < i64.maximum bounds do
    let pairs =
      map (\j -> let (u,v) = left_right_pairs[j]
                 let x = if ks[j] < col_sizes[u] then d.row_idxs[d.col_offsets[u] + ks[j]] else i32.highest
                 let y = if ls[j] < col_sizes[v] then d.row_idxs[d.col_offsets[v] + ls[j]] else i32.highest
                 in (x,y))
          (iota n0)

    let row_idxs' =
      scatter row_idxs
              (map (\j -> if i < bounds[j] then offsets[j] + i else -1)
                   (iota n0))
              (map (\j -> let (x,y) = pairs[j]
                          in if      x < y then x
                             else if y < x then y
                             else -1)
                   (iota n0))
                
    let (ks', ls') =
      (iota n0) |> map (\j -> let (x,y) = pairs[j]
                              in if   x < y then (ks[j]+1, ls[j]  )
                              else if y < x then (ks[j],   ls[j]+1)
                              else (ks[j]+1, ls[j]+1))
                |> unzip

    in (row_idxs', ks', ls')

  -- Filter out all the empty elements
  -- TODO
  in { col_offsets = col_offsets
     , row_idxs = row_idxs
     } |> csc_to_coo2
       |> filter (\(_,i) -> i != -1)
       |> \d -> coo2_to_csc d n

-- 0 0 1 0
-- 0 0 1 1
-- 0 0 0 1
-- 0 0 0 0
let d0: csc_mat =
  { col_offsets = [0, 0, 0, 2, 4]
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
  , row_idxs = [0, 2, 1, 2, 1, 2, 3]
  }

let d1_lows: []i64 = [-1, -1, 2, 2, 3]
let d1_arglows: []i64 = [-1, -1, 2, 4, -1]

