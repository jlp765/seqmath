## Extensions of the math module to work with sequences
## and arrays, and with nested sequences
##
## The returned results are sequences, not arrays,
## but the procs accept arrays as parameters
##
## Not all functions work with nested sequences
##
##  *Note:* ``financial`` formulas are discrete
##  and not the result of solving non-linear equations.
##  The results produced will differ from the
##  results produced by a library like numpy.

import seqmath/util
import seqmath/smath
import seqmath/finance

import math
export math

export util
export smath
export finance
