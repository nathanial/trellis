import Lake
open Lake DSL

package trellis where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

-- Test dependencies
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.7"
require chronos from git "https://github.com/nathanial/chronos-lean" @ "v0.0.2"

@[default_target]
lean_lib Trellis where
  roots := #[`Trellis]

lean_lib TrellisTests where
  roots := #[`TrellisTests]
  globs := #[.submodules `TrellisTests]

@[test_driver]
lean_exe trellis_tests where
  root := `TrellisTests.Main

lean_exe trellis_perf_tests where
  root := `TrellisTests.PerformanceTests
