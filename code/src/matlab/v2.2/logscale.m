function logscale()
h_check = findobj('String','log scale y-axis');
if get(h_check,'Value') ~= 0 % use log Y-scale.
    set(gca,'Yscale','log');
else % use linear scale.
    set(gca,'Yscale','linear');
end

