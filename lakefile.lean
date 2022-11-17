import Lake
open Lake DSL

package sciLeanExamples {
  -- add package configuration options here
}


require scilean from git "https://github.com/lecopivo/SciLean" @ "master"
require widgetKit from git "https://github.com/EdAyers/WidgetKit" @ "physics"


lean_lib SciLeanExamples {
  -- add library configuration options here
}


@[default_target]
lean_exe sciLeanExamples {
  root := `Main
}
