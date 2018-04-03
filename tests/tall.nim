import seqmath

when isMainModule:
  doAssert(Point(x:2.0, y:2.0) != Point(x:2.0001, y:2.0) )
  doAssert(Point(x:2.0, y:2.0) == Point(x:2.0, y:2.0) )
  doAssert(Point(x:2.0, y:2.0) > Point(x:1.99999, y:2.0) )
  doAssert(Point(x:1.99998, y:2.0) < Point(x:1.99999, y:2.0) )
  doAssert(Point(x:1.99999, y:2.0) <= Point(x:1.99999, y:2.0) )
  doAssert(Point(x:1.99999, y:2.0) >= Point(x:1.99999, y:2.0) )
  doAssert(swap(Point(x:1.99999, y:2.0)) == Point(x:2.0, y:1.99999) )
  doAssert(swap(Point(x:1.99999, y:2.0)) == Point(x:2.0, y:1.99999) )
  var p = Point(x:1.99999, y:2.0)
  swap(p)
  doAssert( p == Point(x:2.0, y:1.99999) )

  doAssert( min(@[-1,-2,3,4]) == -2 )
  doAssert( max(@[-1,-2,3,4]) == 4 )
  doAssert( max(@[-1,-2,3,4], 0) == @[0,0,3,4] )
  doAssert( max(@[-1,-2,3,4], @[4,3,2,1]) == @[4,3,3,4] )
  doAssert( min(@[-1,-2,3,4], @[4,3,2,1]) == @[-1,-2,2,1] )

  # tests for convenience procs
  doAssert( (@[1, 2, 3, 4] < @[0, 3, 5, 6]) == @[false, true, true, true])
  doAssert( (@[1, 2, 3, 6] <= @[0, 1, 5, 6]) == @[false, false, true, true])  
  doAssert( (@[1, 2, 3, 4] > @[0, 3, 5, 6]) == @[true, false, false, false])
  doAssert( (@[1, 2, 3, 4] >= @[0, 2, 5, 6]) == @[true, true, false, false])
  doAssert( arange(0, 5) == @[0, 1, 2, 3, 4] )
  doAssert( arange(0, 7, 2) == @[0, 2, 4, 6] )
  doAssert( arange(0, 6, 2) == @[0, 2, 4] )
  doAssert( arange(0, 6, 2, endpoint = true) == @[0, 2, 4, 6] )
  doAssert( flatten(@[ @[1, 2, 3], @[4, 5, 6] ]) == @[1, 2, 3, 4, 5, 6])
  doAssert( flatten(@[ @[ @[1, 2, 3], @[4, 5, 6] ], @[ @[1, 2, 3], @[4, 5, 6] ] ]) == @[1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6])
  # currently not working, due to no support for openArray in shape
  doAssert( shape([@[1,2,3], @[4,5,6]]) == @[2,3] )
  doAssert( shape(@[@[1,2,3], @[4,5,6]]) == @[2,3] )
  doAssert( shape(@[@[@[@[1,2,3], @[4,5,6]]]]) == @[1,1,2,3] )

  # assuming shape works, test newSeqOf2D and newSeqOf3D
  doAssert( shape(newSeqOf2D[int]([9, 9])) == @[9, 9] )
  doAssert( shape(newSeqOf2D[int]([3, 9])) == @[3, 9] )
  doAssert( shape(newSeqOf3D[int]([2, 2, 5])) == @[2, 2, 5] )
  doAssert( shape(newSeqOf3D[int]([10, 10, 10])) == @[10, 10, 10] )
  doAssert( shape(newSeqOf3D[int]([3, 10, 5])) == @[3, 10, 5] )

  doAssert( shape(reshape2D(newSeq[int](100), [10, 10])) == @[10, 10] )
  doAssert( shape(reshape(newSeq[int](100), [10, 10])) == @[10, 10] )
  doAssert( shape(reshape3D(newSeq[int](1000), [10, 10, 10])) == @[10, 10, 10] )
  doAssert( shape(reshape(newSeq[int](1000), [10, 10, 10])) == @[10, 10, 10] )
  doAssert( shape(reshape2D(newSeq[int](100), [5, 20])) == @[5, 20] )
  doAssert( shape(reshape(newSeq[int](100), [5, 20])) == @[5, 20] )
  doAssert( shape(reshape3D(newSeq[int](1000), [5, 20, 10])) == @[5, 20, 10] )
  doAssert( shape(reshape(newSeq[int](1000), [5, 20, 10])) == @[5, 20, 10] )
  
  doAssert( histogram(arange(0, 100, 1), bins = 10).len == 10 )
  doAssert( histogram(arange(0, 100, 1), bins = 11).len == 11 )
  doAssert( histogram(arange(0, 100, 1), bins = 10) == @[10, 10, 10, 10, 10, 10, 10, 10, 10, 10] )
  let xar = @[0, 1, 1, 1, 3, 5, 1, 3, 6, 8]
  doAssert( histogram(xar, bins = 6, range = (0.0, 5.0)) == @[1, 4, 0, 2, 0, 1] )

  # TODO: add tests for likelihood, logLikelihood and gauss  

  doAssert( cumProd([1,2,3,4]) == @[1,2,6,24])
  doAssert( cumSum([1,2,3,4]) == @[1,3,6,10])
  doAssert( cumCount([1,3,3,2,3],3) == @[0,1,2,2,3])
  doAssert( cumPowSum(@[1,2,3,4], 2) == @[1.0, 5.0, 14.0, 30.0])

  doAssert( sum([1,2,3,4]) == 10 )
  doAssert( product([1,2,3,4]) == 24 )
  doAssert( sumSquares(@[1,2,3,4]) == 30 )
  doAssert( powSum(@[1,2], 3) == 9 )

  doAssert( eAdd(@[1,2,-1], @[3,4,-2]) == @[4,6,-3] )
  doAssert( eAdd(@[1,2], 4) == @[5,6] )
  doAssert( eSub(@[1,2,-1], @[3,1,-2]) == @[-2,1,1] )
  doAssert( eSub(@[1,20], 4) == @[-3,16] )
  doAssert( eSub( @[1.0, 2.0, -1.0], @[3.0, 1.0, -2.1231]) == @[-2.0,1.0,1.1231] )
  doAssert( eSub(@[1.0,20.321], 4.0) == @[-3.0,16.321] )
  doAssert( eMul(@[1,2,-1], @[3,1,-2]) == @[3,2,2] )
  doAssert( eMul(@[-1,20], 4) == @[-4,80] )
  doAssert( eMul( @[1.0, 2.0, -1.0], @[3.0, 1.0, -2.1231]) == @[3.0,2.0,2.1231] )
  doAssert( eMul(@[-1.111,20.0], 4.0) == @[-4.444,80.0] )
  doAssert( eDiv(@[1,2,-1], @[1,0,-2]) == @[1.0,0.0,0.5] )
  doAssert( eDiv(@[-1,20], 4) == @[-0.25,5.0] )
  doAssert( eDiv( @[1.0, 2.0, -1.1231], @[1.0, 0.0, -1.0]) == @[1.0,0.0,1.1231] )
  doAssert( eDiv(@[-4.444,20.0], 4.0) == @[-1.111,5.0] )
  doAssert( eMod(@[2,4,-5], @[3,0,-4]) == @[2.0,0.0,-1.0] )
  doAssert( eMod(@[-5,20], 4) == @[-1.0,0.0] )
  doAssert( eMod( @[2.0, 4.0, -5.1231], @[3.0, 0.0, -4.0]) == @[2.0,0.0,-1.1231] )
  doAssert( eMod(@[-5.0,20.0], 4.0) == @[-1.0,0.0] )
  doAssert( eDiff([1,2,4,7,0]) == @[1,2,3,-7] )
  var w: seq[int] = @[]
  doAssert( eDiff(w) == @[] )
  doAssert( diff([1,2,4,7,0],0) == @[1,2,3,-7] )
  doAssert( diff([1,2,4,7,0],1) == @[1,2,3,-7] )
  doAssert( diff([1,2,4,7,0],2) == @[1,1,-10] )
  doAssert( diff([1,2,4,7,0],3) == @[0,-11] )
  doAssert( diff([1,2,4,7,0],4) == @[-11] )
  doAssert( diff([1,2,4,7,0],7) == @[] )

  doAssert( percentile(@[10,7,4,3,2,1],0) == 1.0 )
  doAssert( percentile(@[10,7,4,3,2,1],100) == 10.0 )
  doAssert( percentile(@[10,7,4,3,2,1,0],50) == 3.0 )
  doAssert( percentile(@[10,7,4,3,2,1],50) == 3.5 )
  doAssert( bincount(@[1,-1,0,1,3,2,0,2,3,2,1,5,-2,-3]) == @[2,3,3,2,0,1] )
  doAssert( digitize(@[1.2, 10.0, 12.4, 15.5, 20.0], @[0.0,5.0,10.0,15.0,20.0], true) == @[1,2,3,4,4] )
  doAssert( digitize(@[1.2, 10.0, 12.4, 15.5, 20.0], @[0.0,5.0,10.0,15.0,20.0]) == @[1,3,3,4,5] )
  doAssert( digitize(@[1.2, 10.0, 12.4, 15.5, 20.0], @[20.0,15.0,10.0,5.0,0.0], true) == @[4,3,2,1,1] )
  doAssert( digitize(@[1.2, 10.0, 12.4, 15.5, 20.0], @[20.0,15.0,10.0,5.0,0.0]) == @[4,2,2,1,0] )
  doAssert( eDiv(@[4,2],@[2,1]) == @[2.0,2.0] )
  doAssert( eDiv(@[4.0,2.0],@[2.0,1.0]) == @[2.0,2.0] )
  doAssert( eRem(@[1.0,2.0],@[2.0,1.5]) == @[1.0,0.5] )
  doAssert( eRem(@[1.0,2.0], 1.5) == @[1.0,0.5] )
  let x = eMul(@[0.0, 0.1, 0.2, 0.3, 1.5, 1.7, 1.8, 1.9, 2.0, 2.4, 2.5, 2.6], PI)
  doAssert(x.unwrap().eDiv(PI) == @[0.0, 0.1, 0.2, 0.3, -0.5, -0.3, -0.2, -0.1, 0.0, 0.4, 0.5, 0.6] )
  doAssert(transpose(@[ @[1,2,3], @[4,5,6]]) == @[ @[1,4], @[2,5], @[3,6]] )
  doAssert( @[@[1,2],@[3,4],@[5,6]].shape() == @[3,2] )
  doAssert( @[@[1,3,4,5,6,7,8],@[1,1,1,1,1,5,6]].shape() == @[2,7] )
  doAssert( transpose(@[@[1,3,4,5,6,7,8],@[1,1,1,1,1,5,6]]).shape == @[7,2] )

  doAssert( round(fv(0.05/12, 10*12, -100.0, -100)*10000)/10000 == 15692.9289 )
  doAssert( pv(0.05/12, 10*12, -100.0, -100.0) ==  9488.851136853376)
  doAssert( pmt(0.05/12, 10*12, -100.0, -8000.0) == 52.57973401031784)
  doAssert( round(pmt(0.05/12, 10*12, 0.0, -8000.0)*10000)/10000 == 51.5191 )
  let cf = @[-100_000.0, 10_000.0, 10_000.0, 10_000.0, 10_000.0, 10_000.0, 10_000.0, 10_000.0, 10_000.0, 10_000.0, 10_000.0, 10_000.0, 10_000.0]
  var myPv = newSeq[float](13)
  doAssert( round(npv(cf, myPv, 0.10)*10000)/10000 == -31863.0818 )

  doAssert( ptp(@[1,3,2,6]) == 5 )
  doAssert( ptp(@[@[1,3],@[2,6]]) == @[2,4] )
  doAssert( ptp(@[@[@[1,3],@[2,6]], @[@[100,10],@[-5,-11]]]) == @[@[2,4], @[90,6]] )

  doAssert( pointOnLine(Point(x:2.0, y:2.0), Point(x:5.0, y:2.0), 3.0) == Point(x:3.0, y:2.0) )
  doAssert( interp(2.5, @[1,2,3], @[3,2,0]) == 1.0)
  doAssert( interpRaw(2.5, Point(x:2.0, y:2.0), Point(x:3.0, y:0.0)) == 1.0)
  doAssert( interpRaw(2.0, Point(x:2.0, y:2.0), Point(x:2.0, y:5.0)) == 2.0)  # p0.y = 2.0
  doAssert( bezier([Point(x:2.0, y:2.0), Point(x:5.0, y:2.0)], 4) ==
    @[Point(x: 2.0, y: 2.0), Point(x: 3.0, y: 2.0), Point(x: 4.0, y: 2.0), Point(x: 5.0, y: 2.0)] )
  doAssert( bezier([Point(x:2.0, y:2.0), Point(x:2.0, y:5.0)], 4) ==
    @[Point(x: 2.0, y: 2.0), Point(x: 2.0, y: 3.0), Point(x: 2.0, y: 4.0), Point(x: 2.0, y: 5.0)] )

  doAssert( cos(sin(@[@[0.0],@[PI],@[2*PI]])) == @[@[1.0],@[1.0],@[1.0]] )
  doAssert( abs(@[@[1,-2,-2,-3]]) == @[@[1,2,2,3]] )
  doAssert( toInt(toFloat(@[@[1,-2,-2,-3]])) == @[@[1,-2,-2,-3]] )
  doAssert( nextPowerOfTwo(@[@[1,15,25,99]]) == @[@[1,16,32,128]] )
  doAssert( round(@[@[1.1,2.213,25.52,99.9999999999]]) == @[@[1'f64,2,26,100]] )

  # assuming round works as it should, we can test linspace as well
  doAssert( linspace(2.0, 3.0, num = 5).len == 5)
  doAssert( linspace(2.0, 3.0, num = 5, endpoint = false).len == 5)
  doAssert( round(linspace(2.0, 3.0, num = 5), places = 2) == @[2.00, 2.25, 2.5, 2.75, 3.00] )
  doAssert( round(linspace(2.0, 3.0, num = 5, endpoint = false), places = 2) == @[2.00, 2.20, 2.40, 2.60, 2.80] )

  



  
