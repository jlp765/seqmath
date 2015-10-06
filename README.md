# seqmath
### Nim sequence math library for sequences and nested sequences

This is aimed at doing simple sequence and nested sequence maths.

It is loosely based on the functionality of SciPy.org's NumPy for the Python language.

It is NOT a wiz-bang linear algebra, vector, matrix, statistical, financial, ... libray, but hopefully it will save you from  re-inventing the wheel, if you are only after the basics.

It also lifts all (let me know if not) of the single parameter scalar procs from the Nim math library to work with sequences and nested sequences.  Currently it cannot lift procs like arctan2 that take two parameters.

Use the `liftScalarProc()` template to lift your own scalar procs to work with sequences and nested sequences.
