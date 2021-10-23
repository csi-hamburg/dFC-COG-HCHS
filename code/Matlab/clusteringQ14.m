sdata=readtable('Input/structural_params.csv', 'PreserveVariableNames', true);

idx1 = sdata.WMHsmooth < quantile(sdata.WMHsmooth, .25);
dd1 = reshape(d(:,~incomplete(idx1),:), [sum(idx1)*nTP, nROI]);

idx4 = sdata.WMHsmooth > quantile(sdata.WMHsmooth, .75);
dd4 = reshape(d(:,~incomplete(idx4),:), [sum(idx4)*nTP, nROI]);

nstates = 5;
[IDX1, C1, SUMD1, D1] = kmeans(dd1, nstates, 'Distance', 'correlation', 'MaxIter', 1e3);
for i = 1:nstates
    sum(IDX1==i)/numel(IDX1)
end

nstates = 5;
[IDX4, C4, SUMD4, D4] = kmeans(dd4, nstates, 'Distance', 'correlation', 'MaxIter', 1e3);
for i = 1:nstates
    sum(IDX4==i)/numel(IDX4)
end

%%
figure
bar(diag(corr(C1([3 2 4 5 1],:)', C4')))
colorbar
