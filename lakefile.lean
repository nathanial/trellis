import Lake
open Lake DSL

package trellis where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

-- Test dependencies
require crucible from ".." / "crucible"

@[default_target]
lean_lib Trellis where
  roots := #[`Trellis]

lean_lib TrellisTests where
  roots := #[`TrellisTests]
  globs := #[.submodules `TrellisTests]

@[test_driver]
lean_exe trellis_tests where
  root := `TrellisTests.Main
