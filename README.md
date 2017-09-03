![On the left side there is a sketchy handdraw grid, while on the right side it looks perfect.](example.png?raw=true "Example of what pictikz can do.")

## pictikz

Tikz is often used to draw graphs (i.e., networks) in LaTeX. Even though the resulting image can be very clean, manually writing tikz code can be time consuming, especially when the picture needs to be modified afterwards.

On the other side of the spectrum, drawing with graphical tools like inkscape is easy, but getting a clean-looking result can be complicated.

With pictikz you get the best of both worlds: You draw using a graphical tool and pictikz converts the resulting SVG file to tikz code, making some automatic style adjustments if you desire.

## Compiling

Install `ghc` and `cabal`. Then run

    cabal update
    cabal sandbox init
    cabal install --dependencies-only
    cabal build

This will generate the binary `picktiz` in `dist/build/pictikz/`.

## Usage

Running `pictikz FILE` will convert the file using standard settings. The list of options is

        -c, --colours FILE         load colour definitions from FILE. See Colours section below for more information.
	-f, --fit WIDTH HEIGHT     fit coordinates into a box of size WIDTH x HEIGHT without
	                             changing aspect ratio.
	-g, --grid [PERCENT]       fit coordinates into a grid (implies --uniform [PERCENT]).
	-h, --help                 show help.
	-o, --output FILE          writes output into FILE instead of stdout.
	-s, --scale WIDTH HEIGHT   scale coordinates into a box of size WIDTH x HEIGHT.
	-u, --uniform [PERCENT]    group coordinates by distance. Maximum distance for grouping
	                             is PERCENT of the axis in question.
	-v, --version              output version and exit.

Check the `examples` folder or the man page (`pictikz.1`) for more details.

## Conversion

The SVG standard uses the concept of shapes in its definition. That is, there are circles, rectangles, lines and so on instead of simply pixels like bitmap formats.
This allows a program like `pictikz` to infer what a user meant and automatically produce a prettier output.

Details of how the conversion works are given in the man page and illustrated on the examples. It is important to note that `pictikz` uses its own tikz style. So a dashed edges receives the tikz style `pictikz-dashed` instead of the conventional `dashed` for tikz. This is done in order to allow easier customization by the user. If you want all dashed edges to be actually dotted instead, you can do this by providing your own definition for `pictikz-dashed`. A sample style is given at the examples folder.

## Colours
Colours can be specified in a file where each line is in one of the following formats

    <NAME> RGB [0 - 255] [0 - 255] [0 - 255]
    <NAME> RGB #RRGGBB
    <NAME> RGB #RGB
    <NAME> HSL [0 - 360] [0 - 100] [0 - 100]

Values can also be specified as floats between 0 and 1. In this case, they are interpreted as a percentage of their range.
