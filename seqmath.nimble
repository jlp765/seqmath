# Package

version     = "0.1.1"
author      = "James Parkinson"
description = "math for sequences and nested sequences"
license     = "MIT"
srcDir      = "src"

requires "nim >= 0.18.0"


task test, "Run all tests":
  exec "nim c -r tests/tall.nim"
