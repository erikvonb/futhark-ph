let rm_cm = transpose

let d0: [][]i64 = rm_cm
  [[0,0,0,1,1,0,0],
   [0,0,0,1,0,1,0],
   [0,0,0,0,1,1,0],
   [0,0,0,0,0,0,1],
   [0,0,0,0,0,0,1],
   [0,0,0,0,0,0,1],
   [0,0,0,0,0,0,0]]

type~ state [n] =
  { matrix: [n][n]i64
  , classes: [n]i64
  , lows: [n]i64
  , lefts: [n]i64
  , arglows: [n]i64
  , is_positive: [n]bool
  , visited: [n]bool
  , complex_dim: i64
  , dimensions: [n]i64
  , dims_order: [n]i64
  , dims_next: [n]i64
  , dims_start: []i64
  }

let (+.) [n] (c1: [n]i64) (c2: [n]i64): [n]i64 =
  map2 (\x y -> (x + y) % 2) c1 c2

let last_occurrence [n] 't (xs: [n]t) (pred: t -> bool): i64 =
  let is = map (\i -> if pred xs[i] then i else -1) (iota n)
  in reduce i64.max (-1) is

let first_occurrence [n] 't (xs: [n]t) (pred: t -> bool): i64 =
  let i = last_occurrence (reverse xs) pred
  in if i == -1 then -1 else n - 1 - i

let left [n] (d: [n][n]i64) (i: i64): i64 =
  first_occurrence d[:,i] (==1)

let low (c: []i64): i64 =
  last_occurrence c (==1)

let beta_j [n] (d: [n][n]i64) (j: i64): i64 =
  let lefts = map (left d) (iota n)
  in last_occurrence lefts (==j)

let compute_dimensions [n] (s: state[n]): state[n] =
  let dims = map ((\x -> x - 1) <-< reduce (+) 0) s.matrix
  let complex_dim = reduce i64.max (-1) dims

  let cdim = complex_dim + 2

  let dims_order = replicate n 0
  let dims_order_aux = replicate cdim 0
  let dims_next = replicate n (-1)
  let past_cdims = replicate cdim (-1)

  let (dims_order', _, dims_next', _) =
    loop (dims_order, dims_order_aux, dims_next, past_cdims) for i < n do
      let cdim_pos = dims[i] + 1
      let dims_order[i] = dims_order_aux[cdim_pos]
      let dims_order_aux[cdim_pos] = dims_order_aux[cdim_pos] + 1
      let past_cdim = past_cdims[cdim_pos]
      let dims_next =
        if past_cdim > -1 then dims_next with [past_cdim] = i else dims_next
      let past_cdims[cdim_pos] = i
      in (dims_order, dims_order_aux, dims_next, past_cdims)

  in s with dimensions = dims
       with dims_order = dims_order'
       with dims_next = dims_next'
       with complex_dim = complex_dim

let create_dims_start [n] (s: state[n]): state[n] =
  let cdim = s.complex_dim + 2
  let (is, vs) = unzip <|
    map (\i -> if s.dims_order[i] == 0
                 then (s.dimensions[i] + 1, i)
                 else (-1, 0))
        (iota n)
  in s with dims_start = scatter (replicate cdim (-1)) is vs

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
  let cdim = s.complex_dim + 2
  let ceil_cdim = replicate cdim (-1)

  let (visited', _, arglow', classes', is_positive') =
    loop (visited', ceil_cdim', arglow', classes', is_positive') =
         (s.visited, ceil_cdim, s.arglows, s.classes, s.is_positive)
    for d < s.complex_dim do

      let j0 = s.dims_start[d]

      let (_, visited'', ceil_cdim'', arglow'', classes'', is_positive'') =
        loop (j, visited'', ceil_cdim'', arglow'', classes'', is_positive'') =
             (j0, copy visited', copy ceil_cdim', copy arglow', copy classes', copy is_positive')
        while j > -1 do
          let low_j = s.lows[j]
          let cdim_pos = s.dimensions[j] + 1
          let dim_ceil = ceil_cdim''[cdim_pos]

          let is_pivot = low_j > -1 && !visited''[low_j] && classes''[j] == 0 && low_j > dim_ceil
          let arglow'' = if is_pivot
            then arglow'' with [low_j] = j else arglow''
          let classes'' = if is_pivot
            then classes'' with [j] = -1 else classes''
          let is_positive'' = if is_pivot
            then is_positive'' with [low_j] = true else is_positive''

          let ceil_cdim''[cdim_pos] = if low_j > -1 && visited''[low_j]
            then i64.max low_j dim_ceil else ceil_cdim''[cdim_pos]

          let visited''[j] = if low_j > -1 then true else visited''[j]

          let j' = s.dims_next[j]
          in (j', visited'', ceil_cdim'', arglow'', classes'', is_positive'')

      in (visited'', ceil_cdim'', arglow'', classes'', is_positive'')

  in s with visited = visited'
       with arglows = arglow'
       with classes = classes'
       with is_positive = is_positive'

let phase_2 [n] (s: state[n]): state[n] =
  let can_be_reduced = map (==0) s.classes

  let new_matrix =
    map (\j -> if can_be_reduced[j]
                 then let pivot = s.arglows[s.lows[j]]
                      in s.matrix[j] +. s.matrix[pivot]
                 else s.matrix[j])
        (iota n)

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
  !( any (\j -> s.lows[j] > -1 && s.visited[j]) (iota n) )

entry initialise_state [n] (d: [n][n]i64): state[n] =
  { matrix = d
  , classes = replicate n 0
  , lows = map low d
  , lefts = map (left d) (iota n)
  , arglows = replicate n (-1)
  , is_positive = replicate n false
  , visited = replicate n false
  , complex_dim = 0
  , dimensions = replicate n 0
  , dims_order = replicate n 0
  , dims_next = replicate n 0
  , dims_start = replicate n 0
  }

let reduce_state [n] (s_init: state[n]): state[n] =
  let s0 = s_init |> compute_dimensions |> create_dims_start
  let s1 = phase_0 s0

  let final_state = (.1) <|
    loop (converged, s) = (false, s1) while !converged do

      let s0 = s with visited = replicate n false
                 with is_positive = replicate n false
      let s1 = s0 |> phase_1 |> clear_positives

      let s2 = s1 with is_positive = replicate n false
      let s3 = s2 |> phase_2 |> clear_positives

      let converged = is_reduced s3

      in (converged, s1)
      
  in final_state

entry reduce_matrix [n] (matrix: [n][n]i64): ([n][n]i64, [n]i64) =
  let s = initialise_state matrix |> reduce_state
  in ( s.matrix
     , s.lows
     )

entry foo [n] (matrix: [n][n]i64): i32 =
  -- let (r,l) = reduce matrix
  -- in i32.i64 l[0]
  i32.i64 matrix[0,0]

entry bar [n] (matrix: [n][n]i64): [n][n]i64 =
  reduce_matrix matrix |> (.0)
  -- map (map (+1)) matrix

