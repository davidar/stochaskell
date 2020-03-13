function z=func3(s)
x = [0 25 50 75 100]';
y = [2 3 1 2.5 3]';
z = interp1(x, y, s, 'linear');
end
