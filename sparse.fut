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
  , col_idx_map: []i64
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
  -- TODO if rows are sorted, low is just the last element
  let i = get_csc_col d j |> i32.maximum |> i64.i32
  in if i < 0 then -1 else i

let last_occurrence [n] 't (xs: [n]t) (pred: t -> bool): i64 =
  let is = map (\i -> if pred xs[i] then i else -1) (iota n)
  in reduce i64.max (-1) is

let first_occurrence [n] 't (xs: [n]t) (pred: t -> bool): i64 =
  let i = last_occurrence (reverse xs) pred
  in if i == -1 then -1 else n - 1 - i
  
let left (d: csc_mat) (i: i64): i64 =
  let n = length d.col_offsets - 1
  -- let flag j = any (== i32.i64 i) (get_csc_col d j)
  -- in first_occurrence (map flag (iota n)) id
  in (loop (j0, done) = (-1, false) for j < n do
        if !done && any (== i32.i64 i) (get_csc_col d j)
          then (j, true) else (j0, done)
     ).0

-- let clear_column (d: csc_mat) (j: i64): csc_mat =
  -- let col_start = d.col_offsets[j]
  -- let col_end = d.col_offsets[j + 1]
  -- let col_length = col_end - col_start

  -- let row_idxs' = d.row_idxs[0:col_start - 1] ++ d.row_idxs[col_end:]
  -- let col_offsets' = d.col_offsets[0:j + 1]
                     -- ++ map (\x -> x - col_length) d.col_offsets[j + 1:]

  -- in { col_offsets = col_offsets', row_idxs = row_idxs' }

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
  -- Define functions for expansion
  let col_sizes = init <| map2 (-) (rotate 1 d.col_offsets) d.col_offsets
  let new_col_size_bound j =
    -- If d_j is zero
    if lows[j] == -1
    then 0
    -- If d_j has no pivot assigned to it yet, or it is a pivot
    else if arglows[lows[j]] == -1 || arglows[lows[j]] == j
    then col_sizes[j]
    -- If d_j is reducible and we've identified its pivot
    -- TODO if rows are always sorted, we can immediately remove the low,
    -- and subtract 2 from this expression
    else col_sizes[j] + col_sizes[arglows[lows[j]]]
  let make_coo2_element j j_row =
    ( i32.i64 j
    , (get_csc_col d j ++ get_csc_col d arglows[lows[j]])[j_row]
    -- , let l = csc_col_nnz d j
      -- in if j_row < l
         -- then (get_csc_col d j)[j_row]
         -- else (get_csc_col d arglows[lows[j]])[j_row - l]
    )

  -- Expand into intermediate COO matrix
  let coo2 = expand new_col_size_bound make_coo2_element (iota n)

  -- Sort the columns by row index
  let (sorted_cols, sorted_rows) = unzip <| sort_coo2 coo2

  -- Compress the sorted COO matrix into a COO3 matrix (i.e. with values)
  let segments_1 = map2 (!=) (rotate (-1) sorted_cols) sorted_cols
  let segments_2 = map2 (!=) (rotate (-1) sorted_rows) sorted_rows
  -- Start new segment whenever column index or row index changes
  let segments = map2 (||) segments_1 segments_2
  let coo3 =
    segmented_reduce
      (\(j1, i1, v1) (j2, i2, v2) -> (i32.min j1 j2, i32.min i1 i2, (v1 + v2) % 2))
      (i32.highest, i32.highest, 0)
      segments
      (zip3 sorted_cols sorted_rows <| map (const 1) sorted_cols)

  -- Convert COO3 back into CSC
  let coo3' = filter (\(_,_,v) -> v != 0) coo3
  let coo2' = zip (unzip3 coo3').0 (unzip3 coo3').1
  in coo2_to_csc coo2' (length d.col_offsets - 1)

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

