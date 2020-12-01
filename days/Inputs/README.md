Local copy of this folder holds `.hs` representations of my personal input
files. Intentionally excluded from source control.

If I get around to it (probably by day 5 or so), this will instead have modules
for reading inputs into a Haskell data representation from text-based source
files; at that point, the loading code will be included in source control and
only the source files omitted.

Until then, if you want to produce your own modules that will work with the
rest of the code, a module should be `Inputs.DayN` and export an `input` value.