import Crucible
import TrellisTests.LayoutTests
import TrellisTests.PerformanceTests

open Crucible

def main : IO UInt32 := do
  runAllSuites
