design = '36p';
atlas = 'schaefer400x7';
%%
files = dir(fullfile('xcpengine',design,'sub-*','fcon', atlas, '*ts.1D'));
files = files(1:numel(files)); %% remove . and ..

n = numel(files);

subIDs = cellfun(@(s)(s(5:12)),{files.name},'UniformOutput',false);


[nTP, nROI]=size(dlmread(fullfile(files(1).folder, files(1).name)));

%%
d = nan(nTP, n, nROI);

for i = 1:n
    file = files(i);
    ts = dlmread(fullfile(file.folder, file.name));
    if all(size(ts) == [nTP, nROI])
        d(:, i, :) = ts;
    end
end

incomplete = arrayfun(@(i)(any(any(isnan(squeeze(d(:,i,:))')))), 1:n, 'UniformOutput', true);
nn = n - sum(incomplete);

dd = reshape(d(:,~incomplete,:), [nn*nTP, nROI]);

subIDs = subIDs(~incomplete);

%% elbow criterion
[IDX0, C0, SUMD0, D0] = kmeans(dd, 1, 'Distance', 'correlation', 'MaxIter', 1e3);

%% 
reps = 20;
kmax = 12;

%% only run once, takes long time

IDXcell = cell(kmax,1);
Ccell = cell(kmax,1);
SUMDcell = cell(kmax,1);
Dcell = cell(kmax,1);
vcell = cell(kmax,1); %% within-cluster variance

notvisitingallstates = nan(reps, kmax);

for k = 1:kmax
    
    IDXarray = nan(size(dd,1), reps);
    Carray = nan(k, nROI, reps);
    SUMDarray = nan(k, reps);
    Darray = nan(size(dd,1), k, reps);
    
    v = nan(reps,1); 
    
    for j=1:reps
        [IDXarray(:,j), Carray(:,:,j), SUMDarray(:,j), Darray(:,:,j)] = kmeans(dd, k, 'Distance', 'correlation', 'MaxIter', 1e3);
        
        v(j) = sum(arrayfun(@(i)Darray(i,IDXarray(i,j),j)^2, 1:numel(IDXarray(:,j))));
        temp = reshape(IDXarray(:,j), [nTP, nn]);
        notvisitingallstates(j,k) = sum(cellfun(@(s)(length(unique(s))),num2cell(temp,1)) < k);
    end
    IDXcell{k}  = IDXarray;
    Ccell{k}    = Carray;
    SUMDcell{k} = SUMDarray;
    Dcell{k}    = Darray;
    vcell{k}    = v;
end

[~,I]   = cellfun(@min,vcell, 'unif', false);
IDX     = cellfun(@(array,idx)(array(:,idx)), IDXcell,I,'UniformOutput',false);
C       = cellfun(@(array,idx)(array(:,:,idx)), Ccell,I,'UniformOutput',false);
SUMD    = cellfun(@(array,idx)(array(:,idx)), SUMDcell,I,'UniformOutput',false);
D       = cellfun(@(array,idx)(array(:,:,idx)), Dcell,I,'UniformOutput',false);

save(['./derivatives/data/matlab/clusteringdata_' design '.mat'], 'vcell', 'I', 'IDX', 'C', 'SUMD', 'D', 'notvisitingallstates')


%%

load(['./derivatives/data/matlab/clusteringdata_' design '.mat'])

M = [kron((1:kmax)', ones(reps,1)), repmat((1:reps)',[kmax,1]), cell2mat(vcell), notvisitingallstates(:)];
dlmwrite(['./derivatives/data/kmeansdata_' design '.dat'], M);

nstates = 5;
m = reshape(IDX{nstates}, [nTP, nn]);
dlmwrite(['./derivatives/data/stateidxdata_' design '.dat'], m);

save(['Ccelldata_' design '.mat'], 'Ccell')

%%

dists=nan(nstates);
distsc=nan(nstates);

for i=1:nstates
    for j = 1:nstates
        dists(i,j)=C{nstates}(i,:)*C{nstates}(j,:)'/(norm(C{nstates}(i,:))*norm(C{nstates}(j,:)));
        distsc(i,j)=corr(C{nstates}(i,:)',C{nstates}(j,:)');
    end
end
%% load labels
fid = fopen(['Input/Schaefer2018_', num2str(nROI), 'Parcels_7Networks_order.txt']);
labeldata = textscan(fid, '%d%s%d%d%d%d');
fclose(fid);

NW7=cellfun(@(s)subsref(strsplit(s,'_'), struct('type','{}','subs',{{3}})), labeldata{2}, 'UniformOutput', false);
NW7u = unique(NW7);

%% plot spider web plots
design = '36p';
load(['./derivatives/data/matlab/clusteringdata_' design '.mat'])

posNW = nan(nstates,7);
negNW = nan(nstates,7);

spiderdatall = []; %% state, NW, sign, value 

figure
for j = 1:nstates
    for i = 1:numel(NW7u)
        nw = NW7u{i};
        u = double(cellfun(@(s)strcmp(s,nw),NW7));
        vpos = max(C{nstates}(j,:),0);
        vneg = max(-C{nstates}(j,:),0);
        posNW(j,i) = vpos*u/(norm(u)*norm(vpos));
        negNW(j,i) = vneg*u/(norm(u)*norm(vneg));
        
        spiderdatall = [spiderdatall; [ [j;j], [i;i], [1;-1], [posNW(j,i); negNW(j,i)]  ] ];
        
    end
end

dlmwrite(['./derivatives/data/spiderdatall_' design '.dat'], spiderdatall)

%% number of states by subject
design = '36p';
load(['./derivatives/data/matlab/clusteringdata_' design '.mat'])
m = reshape(IDX{nstates}, [nTP, nn]);
nnn = length(subIDs);

%% global metrics
SR = cellfun(@(s)(sum(diff(s)~=0)), num2cell(m,1));
MDT = cellfun(@(s)(mean(diff(find(diff(s))))), num2cell(m,1));

fid = fopen(['./derivatives/data/meandynamics' design '.dat'], 'w');
fprintf(fid, 'ID, switchrate, meandwelltime\n');
for i = 1:numel(subIDs)
    fprintf(fid, '%s, %f, %f\n', subIDs{i}, SR(i), MDT(i));
end
fclose(fid);

%% fractional occupancy
fracocc = nan(numel(subIDs), nstates);
for i = 1:nstates
    fracocc(:,i) = sum(m==i)/nTP;
end

%% dwell time

dts = cellfun(@dwelltimes, num2cell(m,1), 'unif', false);
dts = cell2mat(dts');

for i = 1:nstates
    dt = dts(:,i);
end

%% export
fid = fopen(['./derivatives/data/statedata_' design '.dat'], 'w');
str = ['ID,', sprintf('fracocc_%d,',1:nstates), sprintf('dwell_%d,',1:nstates)];
fprintf(fid, [str(1:end-1) '\n']);
clear str
format=['sub-%s,', repmat('%f,',[1, 2*nstates])];
for i = 1:numel(subIDs)
    fprintf(fid, [format(1:end-1) '\n'], subIDs{i}, fracocc(i,:), dts(i,:));
end
fclose(fid);
clear format
%% transition probabilities
tps = cellfun(@transitionprobs, num2cell(m,1), 'unif', false);
tps = cell2mat(permute(tps,[1,3,2]));
for r = 1:nstates
    for s= 1:nstates
        if r == s; continue; end
    tp = squeeze(tps(r,s,:));
    end
end

%% persistenec probabilities
pps = cellfun(@persistenceprobs, num2cell(m,1), 'unif', false);
pps = cell2mat(permute(pps,[1,2]));

%% export transition probs

fid = fopen(['./derivatives/data/transitionprobsdata_' design '.dat'], 'w');
fprintf(fid, 'ID, state_in, state_out, prob\n');
for i = 1:numel(subIDs)
    for state_in = 1:nstates
        for state_out = 1:nstates
            if (state_in == state_out)
               prob =  pps(state_in, i);
            else
               prob =  tps(state_in, state_out, i);
            end
            fprintf(fid, '%s, %d, %d, %f\n', subIDs{i}, state_in, state_out, prob);

        end
    end
end
fclose(fid);
