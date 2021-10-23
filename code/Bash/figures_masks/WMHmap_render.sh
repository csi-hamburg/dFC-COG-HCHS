#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
BASEDIR="$DIR/../../.."

OUTDIR="$BASEDIR/derivatives/figures/WMHmap_render"
if [ ! -d "$OUTDIR" ]; then
	mkdir -p "$OUTDIR"
fi

#--outfile $OUTDIR/test.png \
for i in 1 2; do
fsleyes render \
	--outfile $OUTDIR/test_${i}.png \
	--scene lightbox \
       	--worldLoc -6.383024297029465 18.549444007086464 15.719957271339368 \
       	--displaySpace $BASEDIR/Input/mri/tmp.tkNQPmZIAV.WMHmap_PERI_SMOOTH.nii.gz \
       	--zaxis 2 \
       	--sliceSpacing 10 \
       	--zrange 60 120 \
       	--ncols 6 \
       	--nrows 1 \
       	--hideCursor \
       	--bgColour 1.0 1.0 1.0 \
       	--fgColour 0.0 0.0 0.0 \
       	--cursorColour 0.0 1.0 0.0 \
	--showColourBar \
       	--colourBarLocation bottom \
       	--colourBarLabelSide bottom-right \
       	--colourBarSize 25 \
       	--labelSize 12 \
       	--performance 3 \
       	--highDpi \
	-std1mmb \
	-o $i \
	$BASEDIR/Input/mri/tmp.tkNQPmZIAV.WMHmap_PERI_SMOOTH.nii.gz \
       	--name "Periventricular lesions" \
	--overlayType volume \
	--alpha 100.0 \
       	--brightness 49.892703862660944 \
       	--contrast 50.214592274678104 \
       	--cmap hot \
       	--negativeCmap greyscale \
       	--displayRange 3.0 699.0 \
       	--clippingRange 3.0 705.99 \
       	--gamma 0 \
       	--cmapResolution 256 \
       	--interpolation none \
       	--numSteps 100 \
       	--blendFactor 0.1 \
       	--smoothing 0 \
       	--resolution 100 \
       	--numInnerSteps 10 \
       	--clipMode intersection \
       	--volume 0 \
       	$BASEDIR/Input/mri/tmp.trGGBOW7Hu.WMHmap_DEEP_SMOOTH.nii.gz \
       	--name "Deep lesions" \
       	--overlayType volume \
       	--alpha 100.0 \
       	--brightness 48.828125 \
       	--contrast 52.34375 \
       	--cmap cool \
       	--negativeCmap greyscale \
       	--displayRange 3.0 64.0 \
       	--clippingRange 3.0 64.64 \
       	--gamma 0 \
       	--cmapResolution 256 \
       	--interpolation none \
       	--numSteps 100 \
       	--blendFactor 0.1 \
       	--smoothing 0 \
       	--resolution 100 \
       	--numInnerSteps 10 \
       	--clipMode intersection \
       	--volume 0
	
	convert $OUTDIR/test_${i}.png -crop x500 -trim +repage $OUTDIR/tile_${i}-%d.png
	mv $OUTDIR/tile_${i}-0.png $OUTDIR/lightboxview_${i}.png
	mv $OUTDIR/tile_${i}-1.png $OUTDIR/colourbar_${i}.png
done

montage -mode concatenate -tile 2x1 $OUTDIR/colourbar_* +repage $OUTDIR/colourbar.png
convert -gravity center $OUTDIR/lightboxview_1.png $OUTDIR/colourbar.png -append $OUTDIR/LBV.png


