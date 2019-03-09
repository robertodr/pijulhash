# pijulhash
Compile Pijul revision info into Haskell projects

[![Build Status](https://travis-ci.com/robertodr/pijulhash.svg?branch=master)](https://travis-ci.com/robertodr/pijulhash)
[![Build status](https://ci.appveyor.com/api/projects/status/q37l0005hxxsh8ru/branch/master?svg=true)](https://ci.appveyor.com/project/robertodr/pijulhash/branch/master)

Some handy Template Haskell splices for including the current Pijul hash
and branch in the code of your project. Useful for including in panic
messages, `--version` output, or diagnostic info for more informative
bug reports.

Adapted from [`GitHash`](https://github.com/snoyberg/githash).
