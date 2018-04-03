import math, algorithm, sequtils, strutils, typetraits

{.deadCodeElim: on.}

type
  Point* = object  ## a point in 2-dimensional space, with ``x`` and ``y`` coordinates
                   ## used with the ``bezier()`` proc
    x*: float
    y*: float

  PtileInterp = enum
    linear, lower, higher, nearest, midpoint

  # error to be raised when a feature is used, which is not (fully) implemented yet
  NotImplementedError = object of Exception

template toFloat(f: float): float = f

# ----------- Point -----------------------

proc `>`*[T:Point](a, b: T): bool {.inline.} =
  result = (a.x*a.x + a.y*a.y) > (b.x*b.x + b.y*b.y)

proc `<`*[T:Point](a, b: T): bool {.inline.} =
  result = (a.x*a.x + a.y*a.y) < (b.x*b.x + b.y*b.y)

proc `<=`*[T:Point](a, b: T): bool =
  result = (a.`<`(b) or a.`==`(b))

proc `>=`*[T:Point](a, b: T): bool =
  result = (a.`>`(b) or a.`==`(b))

proc swap*(a: var Point) {.inline.} =
  ## swap ``x`` and ``y`` inplace
  let t = a.x
  a.x = a.y
  a.y = t

proc swap*(a: Point): Point {.inline.} =
  ## returns Point ``a`` with ``x`` and ``y`` swapped
  let t = a.x
  result.x = a.y
  result.y = t


#proc toFloat*(x: openArray[int]): seq[float] =
#  ## return a copy of ``x`` with all elements
#  ## converted to ``float``
#  result = newSeq[float](x.len)
#  for i in 0..<x.len:
#    result[i] = x[i].float

# ----------- nested seq math ---------------------------

template liftScalarProc*(fname) =
  ## Lift a proc taking one scalar parameter and returning a
  ## scalar value (eg ``proc sssss[T](x: T): float``),
  ## to provide templated procs that can handle a single
  ## parameter of seq[T] or nested seq[seq[]] or the same type
  ##
  ## .. code-block:: Nim
  ##  liftScalarProc(abs)
  ##  # now abs(@[@[1,-2], @[-2,-3]]) == @[@[1,2], @[2,3]]
  proc fname*[T](x: openarray[T]): auto =
    var temp: T
    type outType = type(fname(temp))
    result = newSeq[outType](x.len)
    for i in 0..<x.len:
      result[i] = fname(x[i])

## Available math functions for nested sequences
##----
## **From the system module**
##
## - toFloat
## - toInt
## - toBiggestFloat
## - toBiggestInt
## - abs
##
## **From the math module**
##
## - classify
## - binom
## - fac
## - isPowerOfTwo
## - nextPowerOfTwo
## - countBits32
## - random
## - sqrt
## - cbrt
## - log10
## - log2
## - exp
## - arccos
## - arcsin
## - arctan
## - cos
## - cosh
## - sin
## - sinh
## - tan
## - tanh
## - erf
## - erfc
## - lgamma
## - tgamma
## - trunc
## - round
## - floor
## - ceil
## - degToRad
## - radToDeg

# ---- from system.nim -------------
liftScalarProc(toFloat)
liftScalarProc(toInt)
liftScalarProc(toBiggestFloat)
liftScalarProc(toBiggestInt)
liftScalarProc(abs)
# ---- from math.nim --------------
liftScalarProc(classify)
liftScalarProc(binom)
liftScalarProc(fac)
liftScalarProc(isPowerOfTwo)
liftScalarProc(nextPowerOfTwo)
liftScalarProc(countBits32)
liftScalarProc(random)
liftScalarProc(sqrt)
liftScalarProc(cbrt)
liftScalarProc(log10)
liftScalarProc(log2)
liftScalarProc(exp)
#liftScalarProc2(fexp)
liftScalarProc(arccos)
liftScalarProc(arcsin)
liftScalarProc(arctan)
#liftScalarProc2(arctan2)
liftScalarProc(cos)
liftScalarProc(cosh)
#liftScalarProc2(hypot)
liftScalarProc(sin)
liftScalarProc(sinh)
liftScalarProc(tan)
liftScalarProc(tanh)
#liftScalarProc2(pow)
liftScalarProc(erf)
liftScalarProc(erfc)
liftScalarProc(lgamma)
liftScalarProc(tgamma)
liftScalarProc(trunc)
liftScalarProc(floor)
liftScalarProc(ceil)
liftScalarProc(degToRad)
liftScalarProc(radToDeg)
liftScalarProc(gcd)
liftScalarProc(lcm)


# round cannot be lifted using the template above, since it has an
# optional parameter `round`
proc round*[T](x: openArray[T], places = 0): seq[T] =
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = round(x[i], places)


template liftCompareProc(op) =
  ## lift an comparator operator like `<` to work element wise
  ## on two openArrays `x`, `y` and return a `seq[bool]`
  proc `op`*[T](x, y: openArray[T]): seq[bool] =
    result = newSeq[bool](x.len)
    for i, xy in zip(x, y):
      result[i] = op(xy[0], xy[1])

# lift comparator operators
liftCompareProc(`<`)
liftCompareProc(`>`)
liftCompareProc(`<=`)
liftCompareProc(`>=`)

# ----------- cumulative seq math -----------------------

proc cumProd*[T](x: openArray[T]): seq[T] =
  ## cumulative product for each element of ``x``
  ##
  ## ``cumProd(@[1,2,3,4])`` produces ``@[1,2,6,24]``
  result = newSeq[T](x.len)
  var cp = T(1)
  for i in 0..<x.len:
    cp = cp * x[i]
    result[i] = cp

proc cumSum*[T](x: openArray[T]): seq[T] =
  ## cumulative sum for each element of ``x``
  ##
  ## ``cumSum(@[1,2,3,4])`` produces ``@[1,3,6,10]``
  result = newSeq[T](x.len)
  var cp = T(0)
  for i in 0..<x.len:
    cp = cp + x[i]
    result[i] = cp

proc cumCount*[T](x: openArray[T], v: T): seq[T] =
  ## cumulative count of a number in ``x``
  ##
  ## the cumulative count of ``3`` for ``@[1,3,3,2,3]`` produces ``@[0,1,2,2,3]``
  result = newSeq[T](x.len)
  var cp = T(0)
  for i in 0..<x.len:
    if x[i] == v: inc(cp)
    result[i] = cp

proc cumPowSum*[T](x: openArray[T], p: T): seq[float] =
  ## cumulative sum of ``pow(x[], p)`` for each element
  ## The resultant sequence is of type ``float``
  ##
  ## ``cumPowSum([1,2,3,4],2)`` produces ``@[1, 5, 14, 30]``
  result = newSeq[float](x.len)
  var cps = 0.0
  for i in 0..<x.len:
    cps += pow(x[i].toFloat, p.toFloat)
    result[i] = cps

# ----------- single-result seq math -----------------------

proc product*[T](x: openArray[T]): T =
  ## sum each element of ``x``
  ## returning a single value
  ##
  ## ``product(@[1,2,3,4])`` produces ``24`` (= 1 * 2 * 3 * 4)
  var cp = T(1)
  for i in 0..<x.len: cp *= x[i]
  result = cp

proc sumSquares*[T](x: openArray[T]): T =
  ## sum of ``x[i] * x[i]`` for each element
  ## returning a single value
  ##
  ## ``sumSquares(@[1,2,3,4])``
  ## produces ``30``  (= 1*1 + 2*2 + 3*3 + 4*4)
  var ps = T(0)
  for i in items(x):
    ps += i*i
  result = ps

proc powSum*[T](x: openArray[T], p: T): float =
  ## sum of ``pow(x[], p)`` of each element
  ## returning a single value
  ##
  ## ``powSum(@[1,2], 3)``
  ## produces ``9``  (= pow(1,3) + pow(2,3))
  var ps = 0.0
  for i in 0..<x.len: ps += pow(x[i].toFloat, p.toFloat)
  result = ps

proc max*[T](x: openArray[T], m: T): seq[T] =
  ## Maximum of each element of ``x`` compared to the value ``m``
  ## as a sequence
  ##
  ## ``max(@[-1,-2,3,4], 0)`` produces ``@[0,0,3,4]``
  if x.len == 0: result = @[m]
  else:
    result = newSeq[T](x.len)
    for i in 0..<x.len:
      result[i] = max(m, x[i])

proc max*[T](x, y: seq[T]): seq[T] =
  ## Note: previous definition using an openArray as the type
  ## does not work anymore, since it clashes with with
  ## system.max[T](x, y: T) now
  
  ## Maximum value of each element of ``x`` and
  ## ``y`` respectively, as a sequence.
  ##
  ## ``max(@[-1,-2,3,4], @[4,3,2,1])`` produces ``@[4,3,3,4]``
  if x.len == 0: result = @[]
  else:
    result = newSeq[T](x.len)
    let xLen = max(x.len, y.len)
    let nlen = min(x.len, y.len)
    for i in 0..<xLen:
      if i < nlen: result[i] = max(x[i], y[i])
      elif i < x.len: result[i] = x[i]
      else: result[i] = y[i]
      
proc min*[T](x: openArray[T], m: T): seq[T] =
  ## Minimum of each element of ``x`` compared to the value ``m``
  ## as a sequence
  ##
  ## ``min(@[1,2,30,40], 10)`` produces ``@[1,2,10,10]``
  if x.len == 0: result = @[m]
  else:
    result = newSeq[T](x.len)
    for i in 0..<x.len:
      result[i] = min(m, x[i])

proc min*[T](x, y: seq[T]): seq[T] =
  ## Note: previous definition using an openArray as the type
  ## does not work anymore, since it clashes with with
  ## system.min[T](x, y: T) now
  
  ## Minimum value of each element of ``x`` and
  ## ``y`` respectively, as a sequence.
  ##
  ## ``min(@[-1,-2,3,4], @[4,3,2,1])`` produces ``@[-1,-2,2,1]``
  if x.len == 0: result = newSeq[T](x.len)
  else:
    result = newSeq[T](x.len)
    let xLen = max(x.len, y.len)
    let nlen = min(x.len, y.len)
    for i in 0..<xLen:
      if i < nlen: result[i] = min(x[i], y[i])
      elif i < x.len: result[i] = x[i]
      else: result[i] = y[i]


# ----------- per element seq math -----------------------

proc eAdd*[T](x, y: openArray[T]): seq[T] =
  ## add each element ``x`` and ``y`` respectively,
  ## returning a sequence
  ##
  ## ``eAdd(@[1,2], @[3,4])`` produces ``@[4,6]``
  assert(x.len <= y.len, "eAdd() parameter lengths must match")
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = x[i] + y[i]

proc eAdd*[T](x: openArray[T], y: T): seq[T] =
  ## add ``y`` to each element of ``x``
  ##
  ## ``eAdd(@[1,2], 4)`` produces ``@[5,6]``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = x[i] + y

proc eSub*[T](x, y: openarray[T]): seq[T] =
  ## ``subtract`` each element of ``y`` from ``x`` as a sequence,
  ## where x.len <= y.len
  ##
  ## ``eSub(@[1,2], @[3,4])`` produces ``@[-2,-2]``
  assert(x.len <= y.len, "eSubtract() parameter lengths must match")
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = x[i] - y[i]

proc eSub*[T](x: openArray[T], y: T): seq[T] =
  ## ``subtract`` ``y`` from each element of ``x``
  ##
  ## ``eSub(@[1,2], 2)`` produces ``@[-1,0]``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = x[i] - y

proc eMul*[T](x, y: openArray[T]): seq[T] =
  ## ``multiply`` each element of ``x`` and ``y`` as a sequence,
  ## where x.len <= y.len
  ##
  ## ``eMul(@[1,2], @[3,4])`` produces ``@[3,8]``
  assert(x.len <= y.len, "eMultiply() parameter lengths must match")
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = x[i] * y[i]

proc eMul*[T](x: openArray[T], y: T): seq[T] =
  ## ``multiply`` ``y`` by each element of ``x`` as a sequence
  ##
  ## ``eMul(@[1,2], 8)`` produces``@[8,16]``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = x[i] * y

proc eDiv*[T](x, y: openArray[T]): seq[float] =
  ## ``divide`` each element of ``x`` by ``y`` as a sequence,
  ## where dividing by zero is a zero result,
  ## and the results are of type ``float``
  ## and x.len <= y.len
  ##
  ## ``eDiv(@[1,2], @[2,0])`` produces ``@[0.5,0.0]``
  assert(x.len <= y.len, "eDivide() parameter lengths must match")
  result = newSeq[float](x.len)
  for i in 0..<x.len:
    if y[i] == T(0): result[i] = 0.0
    else: result[i] = x[i] / y[i]

proc eDiv*[T](x: openArray[T], y: T): seq[float] =
  ## ``divide`` each element of ``x`` by ``y`` as a sequence,
  ## where dividing by zero is a zero result
  ## and the results are of type ``float``
  ##
  ## ``eDiv(@[1,2], 2)`` produces ``@[0.5,1.0]``
  result = newSeq[float](x.len)
  for i in 0..<x.len:
    if y == T(0): result[i] = 0.0
    else: result[i] = x[i].toFloat / y.toFloat

proc eMod*[T](x, y: openArray[T]): seq[float] =
  ## ``modulus`` or ``remainder`` from the division of each
  ## corresponding element of ``x`` and ``y`` as a sequence,
  ## where dividing by zero is a zero result
  ## and the results are of type ``float``
  ## and x.len <= y.len
  ##
  ## ``eMod(@[1.0,2.0], @[2.0,1.5])`` produces ``@[1.0, 0.5]``
  assert(x.len <= y.len, "eRemainder() parameter lengths must match")
  result = newSeq[float](x.len)
  for i in 0..<x.len:
    if y[i] == 0: result[i] = 0.0
    else: result[i] = fmod(x[i].toFloat, y[i].toFloat)

proc eMod*[T](x: openArray[T], y: T): seq[float] =
  ## ``modulus`` or ``remainder`` of ``x`` and ``y`` as a sequence,
  ## remainder from the division of each element of ``x`` by ``y``
  ## as a sequence,
  ## where dividing by zero is a zero result
  ## and the results are of type ``float``
  ##
  ## ``eMod(@[1,2], 1.5)`` produces ``@[1.0,0.5]``
  result = newSeq[float](x.len)
  for i in 0..<x.len:
    if y == 0: result[i] = 0.0
    else: result[i] = fmod(x[i].toFloat, y.toFloat)

proc eRem*[T](x, y: openArray[T]): seq[float] =
  ## ``remainder`` - use eMod
  eMod(x,y)

proc eRem*[T](x: openArray[T], y: T): seq[float] =
  ## ``remainder`` - use eMod
  eMod(x,y)

proc exp2*[T](x: openArray[T]): seq[T] =
  ## ``pow(2,x)`` for each element of ``x``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = pow(2,x[i].toFloat())

proc expm1*[T](x: openArray[T]): seq[T] =
  ## ``exp(x)-1`` for each element of ``x``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = exp(x[i].toFloat) - 1

proc log1p*[T](x: openArray[T]): seq[T] =
  ## ``log(1+x)`` for each element of ``x``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = log10(1 + x[i].toFloat())

proc eDiff*[T](x: openArray[T]): seq[T] =
  ## ``difference`` between adjacent element of ``x`
  ## where ``diff = x[n+1] - x[n]``
  ##
  ## returns @[] if ``x`` is empty
  if x.len == 0: return @[]
  result = newSeq[T](x.len-1)
  var prev = x[0]
  for i in 1..<x.len:
    result[i-1] = x[i] - prev
    prev = x[i]

proc diff*[T](x: openArray[T], n: int = 1): seq[T] =
  ## ``difference`` between adjacent element of ``x`
  ## where ``diff = x[i+1] - x[i]``
  ## and ``n`` is the number of times to recursively take the
  ## difference (default = 1), reducing the length of
  ## the returned sequence on each recursion
  ##
  ## if ``n`` is ``0`` or ``1`` when ``x`` contains data, then
  ## one difference only is performed.
  result = eDiff(x)
  for k in 1..<n:
    result = eDiff(result)

proc reciprocal*[T](x: openArray[T]): seq[float] =
  ## ``1/x[]`` for each element of ``x``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = 1.0 / x[i].toFloat()

proc negative*[T](x: openArray[T]): seq[T] =
  ## ``- x[]`` for each element of ``x``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] =  - x[i]

proc clip*[T](x: openArray[T], min, max: T): seq[T] =
  ## limit each element of ``x`` to ``min<=x[]<=max``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    if i < min: result[i] = min
    elif i > max: result[i] = max
    else: result[i] = x[i]

proc clamp*[T](x: openArray[T], min, max: T): seq[T] =
  result = clip(x, min, max)

proc clip*[T](x: var openArray[T], min, max: T) =
  ## limit each element of ``x`` to ``min<=x[]<=max``
  for i in 0..<x.len:
    if x[i] < min: x[i] = min
    if x[i] > max: x[i] = max

proc clamp*[T](x: var openArray[T], min, max: T) =
  ## limits the value ``x`` within the interval [a, b]
  clip(x, min, max)

#proc abs*[T](x: openArray[T]): seq[T] =
#  ## return the absolute value of each element of ``x``
#  result = newSeq[T](x.len)
#  for i in 0..<x.len:
#    result[i] = abs(x[i])

proc sign*[T](x: openArray[T]): seq[int] =
  ## return the sign of each element of ``x``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = sign(x[i])

proc square*[T](x: openArray[T]): seq[T] =
  ## return ``x[]*x[]`` for each element of ``x``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = x[i]*x[i]

proc cube*[T](x: openArray[T]): seq[T] =
  ## return ``x[]*x[]*x[]`` for each element of ``x``
  let limt = pow(high(T),0.33333333)
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    let v = x[i]
    if v > limt: result[i] = high(T)
    else: result[i] = v*v*v

proc sort[T](x: openarray[T]): seq[T] =
  ## return a sorted copy of ``x`` according to the
  ## element type ``T``
  result = newSeq[T](x.len)
  for i in 0..<x.len:
    result[i] = x[i]
  sort(result, cmp[T])

proc percentile*[T](x: openArray[T], p: int, interp = PtileInterp.linear): float =
  ## statistical percentile value of ``x``, where ``p`` percentile value
  ## is between ``0`` and ``100`` inclusively,
  ## and ``p=0`` gives the min value, ``p=100`` gives the max value
  ## and ``p=50`` gives the median value.
  ##
  ## The ``interp`` is the ``PercentileInterp`` interpolation of the
  ## percentile when it does not lie on a discrete element of ``x``,
  ## but between two
  ## arbitrary elements ``x[i]`` and ``x[j]``, and can be one of
  ## - ``linear`` => i + (j-1)*fraction
  ## - ``lower`` => i
  ## - ``higher`` => j
  ## - ``nearest`` => i if fraction < 0.5 else j
  ## - ``midpoint`` => (i+j)/2
  ##
  ## ``x`` does not need to be sorted, because ``percentile`` sorts
  ## a copy of the data itself
  if x.len == 0: result = 0.0
  elif p <= 0: result = min(x).float
  elif p >= 100: result = max(x).float
  else:
    var a = sort(x)
    let f: float =  (x.len-1) * p / 100
    let i: int = floor(f).int
    let frac: float = f - i.float
    if f == i.float:  result = a[i].float
    else:
      case interp
      of PtileInterp.lower: result = a[i].float
      of PtileInterp.higher: result = a[i+1].float
      of PtileInterp.nearest:
        if frac < 0.5:  result = a[i].float
        else: result = a[i+1].float
      of PtileInterp.midpoint:
        result = (a[i] + a[i+1])/2
      else:  # PtileInterp.linear
        result = (a[i].float + (a[i+1] - a[i]).float * frac)

proc median*[T](x: openArray[T], q: int, interp = "linear"): float =
  ## median is the middle value of sorted ``x`` if ``x.len`` is odd,
  ## and is the average of the middle two elements if ``x.len`` is even
  result = percentile(x, 50)

proc interpRatio*[T:float](v, a, b: T): float {.inline.} =
  ## The ratio of ``v`` along the linear line ``a``<->``b``
  ## with no checking that v is between ``a`` and ``b``
  ##
  ## Returns ``0.0`` when ``a`` and ``b`` are equal
  if b == a: result = 0.0
  else:
    result = (v - a) / (b - a)

proc interp*[T:float](v: T, x, y: openArray[T], left, right: T): float =
  ## interpolation of ``v`` where ``v`` is between some two elements in ``x`` (float values),
  ## where ``x`` and ``y`` values are in separate sequences.
  ## Searches through ``x`` until finding the two elements, then returns
  ## the ``interp`` of the two corresponding ``y`` elements.
  ##
  ## ``left`` and ``right`` (if not -1) are used when ``v`` is
  ## outside the elements of ``x``,
  ## else returns the first or last elements of ``x`` respectively.
  result = NaN
  if x.len < 2 or x[1] <= x[0]: return
  # ascending
  if v <= x[0]:
    if left > -1.0:  result = left
    else: result = y[0]
  elif x[x.high] <= v:
    if right > -1.0:  result = right
    else: result = y[x.high]
  else:
    for i in 1..<x.len:
      if v > x[i]:  # v between x[i] and x[i+1]
        let dy = (y[i+1] - y[i])
        let dx = (x[i+1] - x[i])
        result =  y[i] + (v - x[i]) * dy / dx
        break

proc interp*[T:int](v: float, x, y: openArray[T], left, right: T): float =
  ## interpolation of ``v`` where ``v`` is between some two
  ## elements in ``x`` (int values),
  ## where ``x`` and ``y`` values are in separate sequences.
  ## Searches through ``x`` until finding the two elements, then returns
  ## the ``interp`` of the two corresponding ``y`` elements.
  ##
  ## ``left`` and ``right`` (if not -1) are used when ``v`` is
  ## outside the elements of ``x``,
  ## else returns the first or last elements of ``x`` respectively.
  let x = x.toFloat
  let y = y.toFloat
  result = interp(v, x, y, left.toFloat, right.toFloat)

proc interp*[T:int](v: float, x, y: openArray[T]): float =
  ## interpolation of ``v`` where ``v`` is between some two
  ## elements in ``x`` (float values),
  ## where ``x`` and ``y`` values are in separate sequences.
  ## Searches through ``x`` until finding the two elements, then returns
  ## the ``interp`` of the two corresponding ``y`` elements.
  ##
  ## ``left`` and ``right`` (if not -1) are used when ``v`` is
  ## outside the elements of ``x``,
  ## else returns the first or last elements of ``x`` respectively.
  let x = x.toFloat
  let y = y.toFloat
  result = interp(v, x, y, -1.0, -1.0)

proc interpRaw*[T:float](v: T, p0, p1: Point): float =
  ## linear interpolation of the y value corresponding to x value ``v``
  ## between the ``x`` values of the two points ``p0`` and ``p1``
  ## with no checking that ``v`` is between ``p0.x`` and ``p1.x``
  let dy = (p1.y - p0.y)
  let dx = (p1.x - p0.x)
  if dx == 0.0: result = p0.y
  else:
    result =  p0.y + (v - p0.x) * dy / dx

proc interp*[T:float](v: T, p: openArray[Point], left, right: T): float =
  ## interpolation of ``v`` where ``v`` is between the ``x`` values of two consecutive
  ## elements in ``p`` (float values), where ``p`` is in ``Point`` form.
  ## Searches through ``p`` until finding the two elements, then returns
  ## the interp of the two corresponding ``y`` elements.
  ##
  ## ``left`` and ``right`` (if not -1) are used when ``v`` is
  ## outside the elements of ``p``,
  ## else returns the first or last elements of ``p`` respectively.
  result = NaN
  if p.len < 2 or p[1].x <= p[0].x: return
  # ascending
  if v <= p[0].x:
    if left > -1.0:  result = left
    else: result = p[0].y
  elif p[p.high] <= v:
    if right > -1.0:  result = right
    else: result = p[p.high].y
  else:
    for i in 1..<p.len:
      if v > p[i].x:  # v between p[i].x and p[i+1].x
        let dy = (p[i+1].y - p[i].y)
        let dx = (p[i+1].x - p[i].x)
        result =  p[i].y + (v - p[i].x) * dy / dx
        break

proc bincount*(x: openArray[int], sorted = false): seq[int] =
  ## Count of the number of occurrences of each value in
  ## sequence ``x`` of non-negative ints.
  ## If `sorted` is true, we do not perform a sort of the input
  ## sequence. Useful for input sequence is known to be sorted
  ## already.
  ##
  ## The result is an sequence of length ``max(x)-min(x)+1``
  ## and covering every integer from ``min(x)`` to ``max(x)``
  var ss: seq[int]
  if not sorted:
    ss = sort(x)
  else:
    ss = @x
  let sslow = max(0, ss[ss.low])
  result = newSeq[int](ss[ss.len-1] - sslow + 1)
  # relies on newSeq clearing values to zero!!
  for i in 0..<ss.len:
    if ss[i] < 0: continue
    inc(result[ss[i] - sslow])

proc digitize*[T](x: openArray[T], bins: openArray[T], right = false): seq[int] =
  ## Return the indices of the ``bins`` to which each value of ``x`` belongs.
  ##
  ## Each returned index for *increasing ``bins``* is ``bins[i-1]<=x< bins[i]``
  ## and if ``right`` is true, then returns ``bins[i-1]<x<=bins[i]``
  ##
  ## Each returned index for *decreasing ``bins``* is ``bins[i-1] > x >= bins[i]``
  ## and if ``right`` is true, then returns ``bins[i-1] >= x > bins[i]``
  ##
  ## Note: if ``x`` has values outside of ``bins``, then ``digitize`` returns an index
  ## outside the range of ``bins`` (``0`` or ``bins.len``)
  doAssert(bins.len > 1,"digitize() must have two or more bin values")
  result = newSeq[int](x.len)
  # default of increasing bin values
  for i in 0..<x.len:
    result[i] = bins.high + 1
    if bins[1] > bins[0]:
      for k in 0..<bins.len:
        if x[i] < bins[k] and not right:
          result[i] = k
          break
        elif x[i] <= bins[k] and right:
          result[i] = k
          break
    #decreasing bin values
    else:
      for k in 0..<bins.len:
        if x[i] >= bins[k] and not right:
          result[i] = k
          break
        elif x[i] > bins[k] and right:
          result[i] = k
          break

proc histogram*[T](x: openArray[T],
                   bins: (int | string),
                   range: tuple[mn, mx: float] = (0.0, 0.0),
                   normed = false,
                   weights: seq[T] = nil,
                   density = false): seq[int] =
  ## Compute the histogram of a set of data. Adapted from Numpy's code.
  result = @[]
  if x.len == 0:
    raise newException(ValueError, "Cannot compute histogram of empty array!")

  if weights.len > 0 and weights.len != x.len:
    raise newException(ValueError, "The number of weights needs to be equal to the number of elements in the input seq!")

  # parse the range parameter
  var (mn, mx) = range
  if mn == 0.0 and mx == 0.0:
    mn = x.min.float
    mx = x.max.float
  if mn > mx:
    raise newException(ValueError, "Max range must be larger than min range!")
  elif mn == mx:
    mn -= 0.5
    mx += 0.5
  # from here on mn, mx unchanged
  when type(bins) is string:
    # to be implemented to guess the number of bins from different algorithms. Looking at the Numpy code
    # for the implementations it's only a few LoC
    raise newException(NotImplementedError, "Automatic choice of number of bins based on different " &
                       "algorithms not implemented yet.")
  when type(bins) is int:
    # init empty float histogram
    var n = newSeq[float](bins)
    # normalization
    let
      norm = bins.float / (mx - mn)
      bin_edges = linspace(mn, mx, bins + 1, endpoint = true)
    # make sure input array is float and filter to all elements inside valid range
    # x_keep is used to calculate the indices whereas x_data is used for hist calc
    when T isnot float:
      var x_data = mapIt(@x, it.float)
    else:
      var x_data = x
    var x_keep = filterIt(x_data, it >= mn and it <= mx)
    x_data = x_keep
    # remove potential offset
    applyIt(x_keep, it - mn)
    #x_keep -= mn
    applyIt(x_Keep, it * norm)
    # x_keep *= norm

    # compute bin indices
    var indices = mapIt(x_keep, it.int)
    # for indices which are equal to the max value, subtract 1
    indices.apply do (it: int) -> int:
      if it == bins:
        it - 1
      else:
        it
    # since index computation not guaranteed to give exactly consistent results within
    # ~1 ULP of the bin edges, decrement some indices
    let decrement = x_data < bin_edges[indices]
    for i in 0 .. indices.high:
      if decrement[i] == true:
        indices[i] -= 1
      if x_data[i] >= bin_edges[indices[i] + 1] and indices[i] != (bins - 1):
        indices[i] += 1

    # currently weights and min length not implemented for bincount
    result = bincount(indices)

proc likelihood*[T, U](hist: openArray[T], val: U, bin_edges: seq[U]): float =
  ## calculates the likelihood of the value `val` given the hypothesis `hist`
  ## assuming the histogram is binned using `bin_edges`
  assert hist.len == bin_edges.len
  # get indices from bin edges using lower bound (in sorted seq simply looks for
  # potential insertion order of element. Given bin_edges that's precisely the index)
  let ind = bin_edges.lowerBound(val).int
  result = hist[ind].float / hist.sum.float

proc logLikelihood*[T, U](hist: openArray[T], val: U, bin_edges: seq[U]): float =
  ## calculates the logLikelihood for the value `val` given hypothesis `hist` 
  ## assuming a binning `bin_edges`. Uses above `likelihood` proc, takes the log
  ## and checks for valid values.
  ## Returns NegInf for cases in which likelihood returns 0 (bin content of bin is 0)
  let lhood = likelihood(hist, val, bin_edges)
  if lhood == 0:
    result = NegInf
  else:
    result = ln(lhood)
  
proc unwrap*[T](p: openArray[T], discont = PI): seq[float] =
  ## unwrap radian values of ``p`` by changing deltas between
  ## consecutive elements to its ``2*pi`` complement
  ##
  ## If the delta between an element and the previous element of ``p``
  ## is greater than ``discont`` (``p[i]-p[i-1] > discont``)
  ## then the new value is in the range ``-PI`` to ``PI``
  result = newSeq[float](p.len)
  var prevd, d = 0.0
  result[0] = p[0].toFloat
  let PI2 = 2*PI
  for i in 1..<p.len:  # 3rd element onwards
    if i < 3:
      result[i] = p[i].toFloat
      prevd = p[i] - p[i-1]
      continue
    d = p[i].toFloat - p[i-1].toFloat
    if d != prevd:
      if d < PI and d > discont: continue
      # normalise back to range of -PI to PI
      let m = fmod(p[i], PI2)
      if m > PI:
        result[i] = m - PI2
      else:
        result[i] = m
    else:
      # if a discontinuity, then add diff to that value
      result[i] = result[i-1] + d

proc ptp[T: SomeNumber](x: T): T = (result = T(0))
  # this is required for liftScalarProc(ptp)

proc ptp*[T: SomeNumber](x: seq[T]): T =
  ## ``peak to peak`` returns the difference
  ## between the maximum and minimum elements
  ## of ``x`` (a seq or nested seq[])
  if x.len == 0: return T(0)
  var a = x[0]
  var b = x[0]
  for i in 1..<x.len:
    if x[i] < a: a = x[i]
    if x[i] > b: b = x[i]
    result = abs[T](b - a)

liftScalarProc(ptp)

proc pointOnLinePC(p0, p1: Point, dPC: float): Point =
  ## get the ``Point`` (`x`` and ``y`` values) a percentage ``dPC``
  ## of the way along the ``x`` axis between the two points
  ## ``p0`` and ``p1``
  ## If the ``x`` difference is zero, then get the percentage
  ## position along the vertical line.
  let dx = (p1.x - p0.x)
  if dx == 0.0:
    let dy = (p1.y - p0.y)
    if dy == 0.0: return p0
    result.y = p0.y + dy * dPC
    result.x = interpRaw(result.y, swap(p0), swap(p1))
  else:
    result.x = p0.x + (p1.x - p0.x) * dPC
    result.y = interpRaw(result.x, p0, p1)

proc pointOnLine*(p0, p1: Point, v: float): Point =
  ## get the ``Point`` (`x=v`` and ``y`` values) corresponding
  ## to ``v`, a value along the ``x`` axis between the two points
  ## ``p0`` and ``p1`.
  ## swap the ``x`` and ``y`` values of both points to get ``v`` as
  ## a percentage of the ``y`` axis
  if p1.x >= p0.x:
    # positive slope
    if v < p0.x: return p0
    elif p1.x < v: return p1
  else:
    if v > p0.x: return p0
    elif p1.x > v: return p1
  result.x = v
  result.y = interpRaw(result.x, p0, p1)

proc bezierOnePoint(p: openArray[Point], delta: float): Point =
  # get single point that is a point on the bezier curve for a
  # p.len-1 is the order.  When p.len == 2 then
  # have final line to find point on.
  # delta is percentage of x difference to find, so
  # delta=0.1 is one tenth of diff between x of two points
  # for which to return the y value
  var
    newP = newSeq[Point](p.len)
    theLen = p.len - 1
  # copy all elements
  for i in 0..theLen:
    newP[i] = p[i]
  # take point on derived lines between points, and use those
  # new points to recursively get points on the lines until
  # only one line with a point, which is the result
  while theLen > 0:
    for i in 0..<theLen:
      newP[i] = pointOnLinePC(newP[i], newP[i+1], delta)
    dec(theLen)
  result = newP[0]

proc bezier*(p: openArray[Point], n: int): seq[Point] =
  ## generate a bezier curve with n data points from a sequence of
  ## 2 or more data points,
  ## where 2 data points is a linear interpolation (see `interp`:idx: )
  ##
  ## The more points provided in ``p``, the smoother the
  ## resulting Bezier curve, and the slower the function.
  ##
  ## The first and last points of the Bezier curve are the first and last
  ## points in ``p``.
  ##
  ## The algorithm interpolates y values by breaking the x values into
  ## equal x axis segments, which does not provide the smoothest possible
  ## Bezier curve.
  result = newSeq[Point](n)
  for nr in 0..<n:
    if nr == 0:
      result[nr] = p[0]
    elif nr == n-1:
      result[nr] = p[p.high]
    else:
      result[nr] = bezierOnePoint(p, nr/(n-1))


# ----------- other additions to math ---------------------------
# scalar additional functions (some mainly used for tests) are also lifted below
# if possible

proc gauss*[T](x, mean, sigma: T , norm = false): float =
  ## based on the ROOT implementation of TMath::Gaus:
  ## https://root.cern.ch/root/html524/src/TMath.cxx.html#dKZ4iB
  ## inputs are converted to float
  if sigma == 0:
    result = 1.0e30
  let
    arg = (x - mean).float / sigma.float
    res = exp(-0.5 * arg * arg)
  if norm == false:
    result = res
  else:
    result = res / (2.50662827463100024 * sigma) # sqrt(2*Pi)=2.5066282746310002

proc gauss*[T](x: openArray[T], mean, sigma: T, norm = false): seq[float] =
  ## version of gauss working on openArrays
  result = newSeq[float](x.len)
  for i in 0..x.high:
    result[i] = gauss(x[i], mean, sigma, norm)
