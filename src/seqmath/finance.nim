import math

import util
import smath

type
  FinObj* = object ## optional object for interfacing with financial calculations
    pv*: float ## ``present value`` (negative for outgoing cash flow)
    fv*: float ## ``future value`` (negative for outgoing cash flow)
    rate*: float ## annual ``interest rate`` as 0.05 for 5% (monthly as 0.05/12)
    nper*: float ## ``number of periods`` 10*12 for 10 years monthly
    pmt*: float ## ``payment`` amount (negative for outgoing cash flow)


    # ---------------- financial --------------------

proc fv*(rate: float, nper: float, pmt: float, pv: float = 0.0): float =
  ## ``future value`` at a ``rate`` for ``nper`` periods at ``pmt`` payments
  ## per period with an option initial present value ``pv``
  ##
  ## ``pmt`` and ``pv`` are outgoing cash flows (negative values)
  ##
  ## .. code-block:: Nim
  ##  # 5% for 10 years of monthly payments of $100 payments
  ##  # with initial $100 value
  ##  echo fv(0.05/12, 10*12, -100, -100)
  ##  # result of 15692.92889433589
  ##
  ## ``fv = (pv + pmt/rate-pmt/(rate*pow(1+rate,nper))) * pow(1+rate,nper)``
  let rVal = pow(1.0 + rate, nper)
  let pmtVal = pmt / rate - (pmt / (rate * rVal))
  result = - (pv + pmtVal) * rVal
  #if pv == 0.0:
  # result = - pmt * (pow((1.0 + rate), nper) - 1.0) / rate
  #else:
  #  # fv of pv is pv*(1+r)^^n)
  #  result = (- pv * pow((1.0 + rate), nper)) - pmt * (pow((1.0 + rate), nper) - 1.0) / rate

proc fv*(rate: float, nper: int, pmt: float, pv: float = 0.0): float {.inline.} =
  ## ``future value`` with ``nper`` periods as an ``int`` type
  fv(rate, nper.toFloat, pmt, pv)

proc fv*(f: var FinObj) {.inline.} =
  ## ``future value`` calculated using ``FinObj`` as the parameter.
  ##
  ## Supply ``f.pmt``, ``f.pv``, ``f.nper`` and ``f.rate``, and
  ## the result will be returned in ``f.fv``
  f.fv = fv(f.rate, f.nper, f.pmt, f.pv)

proc nper*(rate: float, pmt: float, pv: float): float =
  ## ``number of periods`` for regular payments ``pmt``
  ## at annual interest ``rate`` to reach a present value ``pv``
  if pv == 0.0: result = 0.0
  else:
    result = log10((1.0 + (pv * rate) / - pmt))

proc nper*(f: var FinObj) {.inline.} =
  ## ``number of periods`` calculated using ``FinObj`` as the parameter.
  ##
  ## Supply ``f.pmt``, ``f.pv``, ``f.rate``, and
  ## the result will be returned in ``f.nper``
  f.nper = nper(f.rate, f.pmt, f.pv)

proc pv*(rate: float, nper: float, pmt: float, fv: float = 0.0): float =
  ## ``present value`` at a ``rate`` for ``nper`` periods at ``pmt`` payments
  ## per period with an option initial future value ``fv``
  ##
  ## ``pv = fv/pow(1+rate,nper) - pmt/rate + pmt/(rate*pow(1+rate,nper))``
  let rVal = pow(1.0 + rate, nper)
  result = - (fv / rVal - (- pmt) / rate + (- pmt) / (rate * rVal))

proc pv*(rate: float, nper: int, pmt: float, fv: float = 0.0): float {.inline.} =
  ## ``present value`` for a period of ``int`` type.
  pv(rate, nper.toFloat, pmt, fv)

proc pv*(f: var FinObj) {.inline.} =
  ## ``present value`` calculated using ``FinObj`` as the parameter.
  ##
  ## Supply ``f.pmt``, ``f.fv``, ``f.rate``, ``f.nper``, and
  ## the result will be returned in ``f.pv``
  f.pv = pv(f.rate, f.nper, f.pmt, f.fv)

proc pmt*(rate: float, nper: float, pv: float, fv: float = 0.0): float =
  ## ``payment`` required per ``nper`` periods at an interest ``rate``
  ## for a present value ``pv`` to become a future value ``fv``
  ##
  ## ``pmt = abs(pv - fv/pow(1+rate,nper))*(rate*pow(1+rate,nper)
  ## /(1-pow(1+rate,nper)))``
  let rVal = pow(1.0 + rate, nper)
  result = - abs(- pv - fv/rVal) * rate * rVal/(1.0 - rVal)

proc pmt*(f: var FinObj) =
  ## ``payment`` calculated using ``FinObj`` as the parameter.
  ##
  ## Supply ``f.pv``, ``f.fv``, ``f.rate``, ``f.nper``, and
  ## the result will be returned in ``f.pmt``
  let rVal = pow(1.0 + f.rate, f.nper)
  f.pmt = - abs(- f.pv - f.fv/rVal) * f.rate * rVal/(1.0 - rVal)

proc npv*(cashflows: openArray[float], pv: var openArray[float], rate: float): float =
  ## ``net present value`` of ``cashflows`` at discount ``rate`` (0.05 for 5%)
  ##
  ## .. code-block:: Nim
  ##  # Project initial cost of $1000.00 returns $100.00 benefits for 3 years
  ##  # at an effective annual discount rate of 10%
  ##  var myPv = newSeq[float](4)
  ##  echo "Net present Value: ", npv(@[-1000.0, 350.0, 350.0, 350.0], myPv, 0.10),
  ##  echo "with present values: ", myPv
  ##  # returns  -129.6018031555224
  ##  #          @[-1000.0, 318.1818181818181, 289.2561983471074,
  ##  #          262.9601803155521]
  ##  # so a net loss of $129.60 (not a good project!!)
  var netPV = cashflows[0]
  pv[0] = cashflows[0]
  for i in 1..<cashflows.len:
    pv[i] = - pv(rate, i, 0.0, cashflows[i])
    netPV += pv[i]
  result = netPV

