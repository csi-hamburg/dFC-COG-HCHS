files=dir('Input/mri/validation_mri/*BIANCA*');

HD = nan(numel(files),1);

for idx = 1:numel(files)
    f = files(idx);
    s = f.name;
    ID = s(5:12)
    
    bianca=niftiread(['Input/mri/validation_mri/sub-' ID '_BIANCA_LOCATE_binarylesionmap.nii']);
    dc=niftiread(['Input/mri/validation_mri/sub-' ID '_DC_FLAIR_label3.nii']);
    I=find(bianca); [i,j,k]=ind2sub(size(bianca),I);
    X=find(dc); [x,y,z]=ind2sub(size(dc),X);
    %HD(idx) = 
    ModHausdorffDist([.9*i,.75*j,.75*k],[.9*x,.75*y,.75*z])
    ModHausdorffDist2([.9*i,.75*j,.75*k],[.9*x,.75*y,.75*z])
    
end