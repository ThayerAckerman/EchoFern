# Changelog for `EchoFern`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 1.1.1.1 - 2023-12-6

### Added

- Gradients variable now stores a list of preset 3-color gradients
- Colorize function colors pictures based on a gradient and their depth in the fractal
- The user can now use '-' and '+' to zoom in and out
- The user can now use the arrows keys to move around the window

### Fixed

- Colorize function now works for 3-color gradients
- Colorize works for odd and even fractal depths

### Changed

- The fracture function now takes in a gradient and max recursion integer for coloring pictures
- The lines of the polygon builder are color-coded, blue for base, red for recurse
- Backspace / delete now allows user to exit back into the polygon builder

### Removed

- Hard-coded color gradients

## 1.1.0.0 - 2023-12-5

### Added

- HandleKeys function uses mouse clicks to generate polygons while maintaining the base edge
- World type keeps track of max and current recursion depth, and the current BasePoly
- PictureClicking and pictureWorld added to distinguish between visualizing BasePoly creation and fractal rendering
- Firas Zaidan's triangulation methods adapted to work with gloss points and concave polygons
- Pressing enter now advances the fractal depth
- Pressing backspace / delete subtracts fractal depth
- Function location has been reordered with comments for readability

### Fixed

- The order that points build polygons now allows triangulation to function appropriately

### Changed

- Main now calls the play function for user interaction

### Removed

- Hard-coded BasePoly objects, replaced by polygon builder

## 1.0.0.0 - 2023-12-4

### Added
- Custom BasePoly polygon type with labeled edges (Base, Recurse, Plain)
- Picturize makes Picture form BasePoly
- Linear Transformations for BasePolys
- Helper functions for getting the length, location of unit circle, realtive angle, and length
- FracTransform transforms a child BasePoly based on a parent and recurse edge
- Fracture creates a fractal based on a BasePoly and recursion depth
- Main method renders picture in a 1000 x 1000 window

