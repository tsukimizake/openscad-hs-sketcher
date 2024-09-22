# OpenSCAD-hs-Sketcher
OpenSCAD-hs-Sketcher is a Haskell-based Domain Specific Language (DSL) for generating OpenSCAD code from 2D sketches. It allows users to model and design 2D objects programmatically, leveraging the power of Haskell's type system and functional programming.

## Features
Create 2D sketches with Haskell code.
Automatically generate OpenSCAD scripts.

## Installation
Clone this repository and build using Stack:

```sh
git clone https://github.com/tsukimizake/openscad-hs-sketcher.git
cd openscad-hs-sketcher
stack build
```

## Usage
Define your 2D sketch in Haskell, and compile it to OpenSCAD code:

```hs
main = do
  let sketch = circle 10
  writeOpenSCAD "output.scad" sketch
```
