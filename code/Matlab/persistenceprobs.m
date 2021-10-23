function M = persistenceprobs(x)

x = reshape(x,1,[]);
states = 1:max(x);% unique(x);

i = find(diff(x));
y = [x(i) x(end)];

M = nan(numel(states),1);
for s = states
    M(s) = sum(x(find(x(1:end-1)==s)+1)==s) / sum(x(1:end-1)==s);
end

end