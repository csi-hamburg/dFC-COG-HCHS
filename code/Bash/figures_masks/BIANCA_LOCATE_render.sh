#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
BASEDIR="$DIR/../../.."

OUTDIR="$BASEDIR/derivatives/figures/BIANCA_LOCATE"
if [ ! -d "$OUTDIR" ]; then
	mkdir -p "$OUTDIR"
fi

subID=496a509f
subID=00a5f155
subID=1fe27156

z=10

export FSLOUTPUTTYPE=NIFTI_GZ


## collect mri data from Hummel

if [[ ! -d $BASEDIR/Input/mri/sub-${subID} ]]; then
	mkdir -p $BASEDIR/Input/mri/sub-${subID}/{bianca,LOCATE}
	cd $BASEDIR/Input/mri/sub-${subID}
	scp -r -i ~/.ssh/id_rsa_hummel fawx493@hummel1.rrz.uni-hamburg.de:/work/fawx493/bids/derivatives/bianca/sub-${subID}/* $BASEDIR/Input/mri/sub-${subID}/bianca
	
	scp -r -i ~/.ssh/id_rsa_hummel fawx493@hummel1.rrz.uni-hamburg.de:/work/fawx493/bids/derivatives/bianca/output_classify/sub-${subID}/* $BASEDIR/Input/mri/sub-${subID}/bianca
	
	scp -r -i ~/.ssh/id_rsa_hummel fawx493@hummel1.rrz.uni-hamburg.de:/work/fawx493/bids/derivatives/bianca/LOCATE/classify/LOCATE_results_directory/sub-${subID}* $BASEDIR/Input/mri/sub-${subID}/LOCATE
fi




fsleyes render --outfile  $OUTDIR/${subID}_FLAIR.png  --scene ortho --worldLoc 10.183388383994952 -37.65394936147146 $z --displaySpace \
       	$BASEDIR/Input/mri/sub-${subID}/bianca/work/FLAIR_brain.nii.gz --xcentre -0.00000  0.00000 --ycentre  0.00000  0.00000 --zcentre  0.00000 -0.00000 --xzoom 105.4443102215519 --yzoom 100.0 --zzoom 100.0 --hideLabels --layout horizontal --hidex --hidey --hideCursor --bgColour 1.0 1.0 1.0 --fgColour 0.0 0.0 0.0 --cursorColour 0.0 1.0 0.0 --showColourBar --colourBarLocation top --colourBarLabelSide top-left --colourBarSize 25.0 --labelSize 12 --performance 3 --movieSync -hd \
       	$BASEDIR/Input/mri/sub-${subID}/bianca/work/FLAIR_brain.nii.gz --name "FLAIR" --overlayType volume --alpha 100.0 --brightness 50.0 --contrast 50.0 --cmap greyscale --negativeCmap greyscale --displayRange 0.0 325.0008239746094 --clippingRange 0.0 328.25083221435546 --gamma 0.0 --cmapResolution 256 --interpolation none --numSteps 100 --blendFactor 0.1 --smoothing 0 --resolution 100 --numInnerSteps 10 --clipMode intersection --volume 0

convert $OUTDIR/${subID}_FLAIR.png -trim +repage  -annotate +10+10 '' -chop x110+0+80 $OUTDIR/${subID}_FLAIR_trim.png


fsleyes render --outfile  $OUTDIR/${subID}_LPM.png  --scene ortho --worldLoc 10.183388383994952 -37.65394936147146 $z --displaySpace \
       	$BASEDIR/Input/mri/sub-${subID}/bianca/work/FLAIR_brain.nii.gz --xcentre -0.00000  0.00000 --ycentre  0.00000  0.00000 --zcentre  0.00000 -0.00000 --xzoom 105.4443102215519 --yzoom 100.0 --zzoom 100.0 --hideLabels --layout horizontal --hidex --hidey --hideCursor --bgColour 1.0 1.0 1.0 --fgColour 0.0 0.0 0.0 --cursorColour 0.0 1.0 0.0 --showColourBar --colourBarLocation top --colourBarLabelSide top-left --colourBarSize 25.0 --labelSize 12 --performance 3 --movieSync -hd \
       	$BASEDIR/Input/mri/sub-${subID}/bianca/work/FLAIR_brain.nii.gz --name "FLAIR_brain" --overlayType volume --alpha 100.0 --brightness 50.0 --contrast 50.0 --cmap greyscale --negativeCmap greyscale --displayRange 0.0 325.0008239746094 --clippingRange 0.0 328.25083221435546 --gamma 0.0 --cmapResolution 256 --interpolation none --numSteps 100 --blendFactor 0.1 --smoothing 0 --resolution 100 --numInnerSteps 10 --clipMode intersection --volume 0 \
       	$BASEDIR/Input/mri/sub-${subID}/bianca/sub-${subID}_bianca_output.nii.gz --name "Lesion probability" --overlayType volume --alpha 100.0 --brightness 31.25 --contrast 87.5 --cmap brain_colours_5redyell --negativeCmap greyscale --displayRange 0.0 1.0 --clippingRange 0.0 1.01 --gamma 0.0 --cmapResolution 256 --interpolation none --numSteps 100 --blendFactor 0.1 --smoothing 0 --resolution 100 --numInnerSteps 10 --clipMode intersection --volume 0


convert $OUTDIR/${subID}_LPM.png -trim +repage  -annotate +10+10 '' -chop x110+0+80 $OUTDIR/${subID}_LPM_trim.png


fsleyes render --outfile $OUTDIR/${subID}_voronoi.png --scene ortho --worldLoc 10.183388383994952 -37.65394936147146 $z --displaySpace \
       	$BASEDIR/Input/mri/sub-${subID}/bianca/work/FLAIR_brain.nii.gz --xcentre -0.00000  0.00000 --ycentre  0.00000  0.00000 --zcentre  0.00000 -0.00000 --xzoom 105.4443102215519 --yzoom 100.0 --zzoom 100.0 --hideLabels --layout horizontal --hidex --hidey --hideCursor --bgColour 1.0 1.0 1.0 --fgColour 0.0 0.0 0.0 --cursorColour 0.0 1.0 0.0 --showColourBar --colourBarLocation top --colourBarLabelSide top-left --colourBarSize 25.0 --labelSize 12 --performance 3 --movieSync -hd \
       	$BASEDIR/Input/mri/sub-${subID}/bianca/work/FLAIR_brain.nii.gz --name "FLAIR_brain" --overlayType volume --alpha 100.0 --brightness 50.0 --contrast 50.0 --cmap greyscale --negativeCmap greyscale --displayRange 0.0 325.0008239746094 --clippingRange 0.0 328.25083221435546 --gamma 0.0 --cmapResolution 256 --interpolation none --numSteps 100 --blendFactor 0.1 --smoothing 0 --resolution 100 --numInnerSteps 10 --clipMode intersection --volume 0 \
       	$BASEDIR/Input/mri/sub-${subID}/LOCATE/sub-${subID}_thresholdsmap.nii.gz --name "Threshold map" --overlayType volume --alpha 100.0 --brightness 36.84210509804807 --contrast 76.31578980390385 --cmap brain_colours_2winter --negativeCmap greyscale --displayRange 0.9 0.949999988079071 --clippingRange 0.9 0.9594999879598618 --gamma 0.0 --cmapResolution 256 --interpolation none --numSteps 100 --blendFactor 0.1 --smoothing 0 --resolution 100 --numInnerSteps 10 --clipMode intersection --volume 0


convert $OUTDIR/${subID}_voronoi.png -trim +repage  -annotate +10+10 '' -chop x110+0+80 $OUTDIR/${subID}_voronoi_trim.png

fslmaths $BASEDIR/Input/mri/sub-${subID}/bianca/work/FLAIR_dist_to_vent.nii.gz -sub 10 -bin -mul 2 -sub 1 -mul $BASEDIR/Input/mri/sub-${subID}/LOCATE/sub-${subID}_BIANCA_LOCATE_binarylesionmap.nii.gz $BASEDIR/Input/mri/sub-${subID}/signed_lesion.nii.gz
fsleyes render --outfile $OUTDIR/${subID}_BLM.png --scene ortho --worldLoc 10.183388383994952 -37.65394936147146 $z --displaySpace \
       	$BASEDIR/Input/mri/sub-${subID}/bianca/work/FLAIR_brain.nii.gz --xcentre -0.00000  0.00000 --ycentre  0.00000  0.00000 --zcentre  0.00000 -0.00000 --xzoom 105.4443102215519 --yzoom 100.0 --zzoom 100.0 --hideLabels --layout horizontal --hidex --hidey --hideCursor --bgColour 1.0 1.0 1.0 --fgColour 0.0 0.0 0.0 --cursorColour 0.0 1.0 0.0 --showColourBar --colourBarLocation top --colourBarLabelSide top-left --colourBarSize 25.0 --labelSize 12 --performance 3 --movieSync -hd \
       	$BASEDIR/Input/mri/sub-${subID}/bianca/work/FLAIR_brain.nii.gz --name "FLAIR_brain" --overlayType volume --alpha 100.0 --brightness 50.0 --contrast 50.0 --cmap greyscale --negativeCmap greyscale --displayRange 0.0 325.0008239746094 --clippingRange 0.0 328.25083221435546 --gamma 0.0 --cmapResolution 256 --interpolation none --numSteps 100 --blendFactor 0.1 --smoothing 0 --resolution 100 --numInnerSteps 10 --clipMode intersection --volume 0 \
       	$BASEDIR/Input/mri/sub-${subID}/signed_lesion.nii.gz --name "Segmented WMH" --overlayType volume --alpha 100.0 --brightness 50.0 --contrast 50.0 --negativeCmap brain_colours_diverging_bwr_iso --cmap brain_colours_4cool  --useNegativeCmap --displayRange 0.0 1.0 --clippingRange 0.0 1.01 --gamma 0.0 --cmapResolution 256 --interpolation none --numSteps 100 --blendFactor 0.1 --smoothing 0 --resolution 100 --numInnerSteps 10 --clipMode intersection --volume 0



convert $OUTDIR/${subID}_BLM.png -trim +repage -annotate +10+10 '' -chop x110+0+80 $OUTDIR/${subID}_BLM_trim.png

echo -e "A, B, C, D" > labs.txt
montage -mode concatenate -tile 4x1 $OUTDIR/${subID}_{FLAIR,LPM,voronoi,BLM}_trim.png $OUTDIR/${subID}_${z}.png
