import sequtils

# ----------- convenience procs -------------------------

template canImport(x): bool =
  compiles:
    import x

when canImport(arraymancer):
  import arraymancer
  proc argmin*[T](a: AnyTensor[T], axis: int): int =
    let `min` = min(a)
    for i, x in a:
      if x == `min`:
        return i[axis]

  proc argmin*[T](a: AnyTensor[T]): int =
    # argmin for 1D tensors
    let `min` = min(a)
    for i, x in a:
      if x == `min`:
        return i[0]

proc arange*(start, stop, step = 1, endpoint = false): seq[int] =
  ## returns seq containing all elements from incl. `start` to excl. `stop`
  ## given a stepping of `step`
  ## `endpoint` allows to include `stop` in the output array.
  ## Similar to Numpy's arange
  result = @[]
  var mstop = stop
  if endpoint == true:
    mstop = stop + 1
  for i in start..<mstop:
    if (i - start) mod step == 0:
      result.add(i)

proc linspace*(start, stop: float, num: int, endpoint = true): seq[float] =
  ## linspace similar to numpy's linspace
  ## returns a seq containing a linear spacing starting from `start` to `stop`
  ## eitther including (endpoint == true) or excluding (endpoint == false) `stop`
  ## with a number of `num` elements
  result = @[]
  var 
    step = start
    diff: float
  if endpoint == true:
    diff = (stop - start) / float(num - 1)
  else:
    diff = (stop - start) / float(num)
  if diff < 0:
    # in case start is bigger than stop, return an empty sequence
    return @[]
  else:
    for i in 0..<num:
      result.add(step)
      # for every element calculate new value for next iteration
      step += diff


# ----------- convenience procs on seqs and open arrays -------------------

proc `[]`*[T](a: openArray[T], inds: seq[int]): seq[T] {.inline.} =
  ## given two openArrays, return a sequence of all elements whose indices
  ## are given in 'inds'
  ## inputs:
  ##    a: seq[T] = the sequence from which we take values
  ##    inds: openArray[int] = the array which contains the indices for the
  ##         arrays, which we take from 'array'
  ## outputs:
  ##    seq[T] = a sequence of all elements s.t. array[ind] in numpy indexing
  result = newSeq[T](inds.len)
  for i, ind in inds:
    result[i] = a[ind]

proc transpose*[T](x: openArray[seq[T]]): seq[seq[T]] =
  ## transpose a seq[seq[]]
  ##
  ## A 2 x 3-element becomes a 3 x 2-element seq[seq[]]
  ## ``transpose(@[ @[1,2,3], @[4,5,6]])`` produces ``@[ @[1,4], @[2,5], @[3,6]]``
  let alen = x.len
  let blen = len(x[0])
  result = newSeqWith(blen, newSeq[T](alen))
  for i in 0..<blen:
    for j in 0..<alen:
      result[i][j] = x[j][i]

proc flatten*[T: SomeNumber](a: seq[T]): seq[T] = a
  ## Exists so that recursive proc stops with this proc.
proc flatten*[T: seq](a: seq[T]): auto =
  ## Note: only works due to usage of `auto` as return value, as
  ## noted by Araq here:
  ## https://github.com/nim-lang/Nim/pull/6807
  ## maybe not nice, but still useful :/
  ## proc to flatten a nested sequence `a` to a 1D sequence,
  ## by recursively applying concat to the remaining sequence
  ## makes use of a stopping proc `flatten(T: SomeNumber)()`
  ##
  ## Example:
  ##   let a = @[ @[1, 2, 3], @[4, 5, 6]]
  ##   let a_flat = a.flatten
  ##   echo a_flat
  ##   -> @[1, 2, 3, 4, 5, 6]
  a.concat.flatten

proc shape*[T: (SomeNumber | bool | char | string)](x: T): seq[int] = @[]
  ## Exists so that recursive proc stops with this proc.

proc shape*[T](x: openArray[T]): seq[int] =
  ## recursively determine the dimension of a nested sequence.
  ## we simply append the dimension of the current seq to the
  ## result and call this function again recursively until
  ## we hit the type at core, which is catched by the above proc
  ## 
  ## Example:
  ##    let x = @[ @[ @[1, 2, 3], @[1, 2, 3]],
  ##               @[ @[1, 2, 3], @[1, 2, 3]] ]
  ##    echo x.shape
  ##    -> @[2, 2, 3]
  result = @[]
  if len(x) > 0:
    result.add(len(x))
    result.add(shape(x[0]))

template getIndexSeq(ind: int, shape: openArray[int]): seq[int] =
  ## given an index for a 1D array (flattened from nD), calculate back
  ## the indices of that index in terms of N dimensions
  ## e.g. if shape is [2, 4, 10] and index ind == 54:
  ## returns a seq of: @[1, 1, 4], because:
  ## x = 1
  ## y = 1
  ## z = 4
  ## => ind = x + y * 10 + z * 4 * 10
  let dim = foldl(@shape, a * b)
  let n_dims = len(shape)
  var result = newSeq[int](n_dims)
  var
    # set our remaining variable to ind as the start
    rem = ind
    # variable for dimensionality, starting by 1, multiplying with each j in shape
    d = 1
  for i, j in shape:
    # multiply with current dimensionality
    d *= j
    # given remainder, get the current index by dividing out the rest of the
    # dimensionality 
    result[i] = rem div int(dim / d)
    rem = rem mod int(dim / d)
  result

proc newSeqOf2D*[T](shape: openArray[int]): seq[seq[T]] =
  ## returns a nested (2D) sequence of the given dimensionality
  assert shape.len == 2
  result = newSeqWith(shape[0], newSeq[T](shape[^1]))

proc newSeqOf3D*[T](shape: openArray[int]): seq[seq[seq[T]]] =
  ## returns a nested (3D) sequence of the given dimensionality
  ## utilizes newSeqOf2D to build the 3D seq
  assert shape.len == 3
  result = newSeqWith(shape[0],
                      newSeqWith(shape[1],
                                 newSeq[T](shape[^1])))

proc reshape2D*[T](s: seq[T], shape: openArray[int]): seq[seq[T]] =
  ## returns a reshaped version of `s` to the given shape of `shape`
  assert s.len == foldl(@shape, a * b)
  result = newSeqOf2D[T](shape)
  for i, el in s:
    # TODO: replace by running indices mimicking the calculation that
    # happens inside of getIndexSeq. 
    let inds = getIndexSeq(i, shape)
    result[inds[0]][inds[1]] = el

proc reshape3D*[T](s: seq[T], shape: openArray[int]): seq[seq[seq[T]]] =
  ## returns a reshaped version of `s` to the given shape of `shape`  
  assert s.len == foldl(@shape, a * b)
  result = newSeqOf3D[T](shape)
  for i, el in s:
    let inds = getIndexSeq(i, shape)
    result[inds[0]][inds[1]][inds[2]] = el

template reshape*[T](s: seq[T], shape: array[2, int]): seq[seq[T]] =
  ## convenience template around reshape2D using 2 element array as input
  s.reshape2D(shape)

template reshape*[T](s: seq[T], shape: array[3, int]): seq[seq[seq[T]]] =
  ## convenience template around reshape3D using 3 element array as input
  s.reshape3D(shape)  

