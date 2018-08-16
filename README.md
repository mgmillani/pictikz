![On the left side there is a sketchy handdraw grid, while on the right side it looks perfect.](example.png?raw=true "Example of what pictikz can do.")

## Compiling

Install `ghc` and `cabal`. Then run

    cabal update
    cabal sandbox init
    cabal install --dependencies-only
    cabal build

This will generate the binary `picktiz` in `dist/build/pictikz/`.

# Synopsis

    pictikz [OPTIONS...] FILE

# Description

Tikz is often used to draw graphs (i.e., networks) in LaTeX. Even though the resulting image can be very clean, manually writing tikz code can be time consuming, especially when the picture needs to be modified afterwards.

On the other side of the spectrum, drawing with graphical tools like inkscape is easy, but getting a clean-looking result can be complicated.

With pictikz you get the best of both worlds: You draw using a graphical tool and pictikz converts the resulting SVG file to tikz code, making some automatic style adjustments if you desire.

# Features

  - Directed and undirected graphs.
  - Coloured, dashed, dotted and bold vertices and edges.
  - Temporal graphs (for beamer).
  - Labels for vertices.

# Options

  **`-c, --colours <FILE>`**

  Load colour definitions from FILE. See *Colours* section below.

  **`-f, --fit WIDTH HEIGHT`**

  Fit coordinates into a box of size WIDTH x HEIGHT without changing aspect ratio.

  **`-g, --grid [PERCENT]`**

  Fit coordinates into a grid (implies --uniform [PERCENT]). By default PERCENT = 20.

  **`-h, --help`**

  Show help.

  **`--latex-colours`**

  Output colours in LaTeX.

  **`--min-dist X Y`**

  Scale coordinates such that the minimum distance in the x-axis is X and the minimum distance in the y-axis is Y.
  
  **`-o, --output FILE`**

  Write output into FILE instead of stdout.

  **`--rename`**

  Rename vertices to v1, v2,... (default)

  **`--no-rename`**

  Do not rename vertices, using the same IDs as in the SVG file.

  **`-s, --scale WIDTH HEIGHT`**

  Scale coordinates into a box of size WIDTH x HEIGHT.

  **`-t, --temporal [START]`**

  Treat SVG layers as frames, using overlay specifications in the output.

  **`   --text-as-nodes`**

  Treat text as individual nodes instead of labels.

  **`-u, --uniform [PERCENT]`**

  Group coordinates by distance. Maximum distance for grouping is PERCENT of the axis in question.

  **`-v, --version`**

  Output version and exit

# SVG Conversion

The SVG standard uses the concept of shapes in its definition. That is, there are circles, rectangles, lines and so on instead of simply pixels like bitmap formats.
This allows a program like `pictikz` to infer what a user meant and automatically produce a prettier output.

`pictikz` uses its own tikz style. So a dashed edges receives the tikz style `pictikz-dashed` instead of the conventional `dashed` for tikz. This is done in order to allow easier customization by the user. If you want all dashed edges to be also curved, you can do this by providing your own definition for `pictikz-dashed`. A sample style is given at the examples folder.

## Nodes

  Every circle and ellipse in the SVG file becomes a \\node[pictikz-node] in tikz.

  Every rect becomes a \\node[pictikz-rectangle].

  Text objects in SVG can become labels for nodes. For each text, the nearest node is taken.

## Edges

  A path is converted to an edge from its initial point to its destination. Intermediary points of the path are irrelevant. The nearest nodes are used as endpoints of the edge.

  If the marker-end attribute of an edge is set, it is converted to \\draw[pictikz-edgeto] in tikz. The attribute marker-start produces edgefrom instead, and both together produce edgeboth.

  The pictikz-thick property in tikz is set if the stroke-width attribute of the path is at least 50% larger than the minimum stroke-width and larger than the average between the minimum and maximum stroke-width.

  An object becomes pictikz-dashed if the stroke-dasharray property is set and the length of the first dash is at least twice the stroke-width of the object. If it is set but smaller than that, the object becomes pictikz-dotted instead.

## Frames

  For temporal graphs (also known as multi-layer graphs), SVG groups are used.
  Groups with an `id` starting with `layer` are treated as frames.
  The order of the frames is the same as in the file.
  Vertices which are close to each other in subsequent layers and are equal will be merged into one vertex.
  The same is valid for edges.

# Colours

Colours can be specified in a file where each line is in one of the following formats

    <NAME> RGB [0 - 255] [0 - 255] [0 - 255]
    <NAME> RGB #RRGGBB
    <NAME> RGB #RGB
    <NAME> HSL [0 - 359] [0 - 100] [0 - 100]

Values can also be specified as floats between 0 and 1. In this case, they are interpreted as a percentage of their range.

# Bugs

No known bugs.

# Author

Marcelo Garlet Millani (marcelogmillani@gmail.com)
