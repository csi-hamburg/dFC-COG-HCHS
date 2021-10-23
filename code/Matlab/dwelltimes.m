function dt = dwelltimes(x)

    x = reshape(x,1,[]);
    states = 1:max(x);% unique(x);

    i = find(diff(x));
    n = [i numel(x)] - [0 i];
  
    dt = arrayfun(@(s)(mean(n([x(i) x(end)]==s))), states, 'unif', true);
    
end