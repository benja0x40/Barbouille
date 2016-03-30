Version History
================================================================================
* * *
Barbouille Release 0.1.0
--------------------------------------------------------------------------------
* * *
### 1. New Features ###

* added function transformColors:
    + support for HSV colorspace transformations which can easily change a color
      scale into desaturated, darker or complementary colors
    + support for RGB colorspace transformations allowing for instance to generate
      warmer (red/yellow) and colder (blue/green) color scales
    + color transformations can be applied directly to colorParameters and
      groupParameters objects as well as to standard R color vectors

* updated function makeColors:
    + bypass of color mapping now allows use cases with group coloring only
    + improved transparency control with 3 methods: global, piecewise or per-value
        - length(alpha) == 1                  => global
        - length(alpha) == length(thresholds) => same piecewise mapping as for colors
        - length(alpha) == length(v)          => per value (predefined by user)
        - length(alpha) == unexpected         => error
    + added the "override" parameter to determine if the transparency of
      below and above colors should override the provided alpha value(s)
    + na and group-associated colors always override provided alpha value(s)

### 2. Bug Fixes ###

* fixed several issues with parameter checking, group index management,
   and color overwriting when using group coloring

* functions defineColors, makeColors:
    + changed default na color value to NA (invisible)

### 3. Internals ###

* moved source code of groupIndex function from defineGroups.R to groupIndex.R

* objects resulting from defineColors and defineGroups now belong
     to the S3 classes colorParameters and groupParameters respectively
    + source code making use of class colorParameters:
      defineColors.R, transformColors.R,  makeColors.R, colorLegend.R
      updateDefinition.R
    + source code making use of class groupParameters:
      defineGroups.R, groupIndex.R, groupLegend.R, makeColors.R
      updateDefinition.R
    + removed reserved functions: .is.color.parameters, .is.group.parameters

* refactored groupPameter attribute from "color" to "colors"
   Having the same name for color attribute in groupPameters and
   colorParameters simplifies coding for similar operations on these
   two classes

### 4. Other ###

* Package name:
  Barbouille, French name of the painting character in Barbapapa.

* * *
Barbouille Release 0.0.1
--------------------------------------------------------------------------------
* * *
### 1. Features ###

* added function scatterPlot
  generic (x, y) plot function supporting color mapping and group representation

* modified function makeColors to implement group-associated coloring

* added support for the representation of group memberships (color, symbol, ...)
    + added functions:
      defineGroups, groupIndex, groupLegend
    + modified functions:
      makeColors

* added function colorLegend
  represent the color scales used with makeColors and defined with defineColors

* modified makeColors to make use of defineColors and predefined parameters

* added function defineColors
    + allows persistence and sharing of color mapping parameters between functions
    + provides useful shortcuts for parameter setting
    + detects invalid parameters and produces error messages

* implemented a new color mapping function derived from makeColors from
  package MRA.TA
    + rewrote sections that were imposing the use of positive numeric values only
    + rewrote the management of NA values
    + refactored input logic and parameter names:
        - "thresholds" and "colors" vector elements are now associated 1 to 1
        - removed the "background" color
        - added the lower limit color "below"
        - changed name of the upper limit color from "overflow" to "above"
        - added "na" to specify the color of NA values
        - added "levels" to determine the number of color levels between thresholds
        - added "alpha" to determine the global transparency of generated colors
    + improved code structure

* implemented function plotLabels based on the R script of Itys
  overlays text labels and arrows to annotate a set of points

### 2. Documentation ###

* started function documentation using roxygen.
  covered sections: title, description, arguments, author(s), see also, examples

