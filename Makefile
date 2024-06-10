default: box.stl f.stl

%.stl: %.scad
	openscad $< -o $@

box.scad f.scad: *.hs
	cabal v2-build
	cabal v2-exec -- k
