function M = transitionprobs(x)

    x = reshape(x,1,[]);
    states = 1:max(x);% unique(x);

    i = find(diff(x));
    y = [x(i) x(end)];
    
    M = nan(numel(states));
    for r = states
        for s = states
                M(r,s) = sum(y(find(y(1:end-1)==r)+1)==s) / sum(y(1:end-1)==r);
        end
    end
    
end