% 1 b
syms x
xs=[-.075, -0.5, -0.25, 0]
fxs=[-0.071815, -0.02475, 0.3349375, 1.101]
p01=((x-xs(1))*(fxs(2))-(x-xs(2))*(fxs(1)))/(xs(2)-xs(1))
p12=((x-xs(2))*(fxs(3))-(x-xs(3))*(fxs(2)))/(xs(3)-xs(2))
p012=((x-xs(1))*(p12)-(x-xs(3))*(p01))/(xs(3)-xs(1))
p23=((x-xs(3))*(fxs(4))-(x-xs(4))*(fxs(3)))/(xs(4)-xs(3))
p123=((x-xs(2))*(p23)-(x-xs(4))*(p12))/(xs(4)-xs(2))
p0123=((x-xs(1))*(p123)-(x-xs(4))*(p012))/(xs(4)-xs(1))

expand(p01)
expand(p012)
expand(p0123)

x=-1/3
subs(p01)
subs(p012)
subs(p0123)

% 4 a
syms y
ys=[8.1, 8.3, 8.6, 8.7]
fys=[16.9441, 17.56492, 18.50515, 18.82091]
py1=fys(1)+(y-ys(1))*((fys(2)-fys(1))/(ys(2)-ys(1)))
py2=py1+(y-ys(1))*(y-ys(2))*( (fys(3)-fys(2))/(ys(3)-ys(2)) - (fys(2)-fys(1))/(ys(2)-ys(1)))/(ys(3)-ys(1))
py3=py2+(y-ys(3))/(ys(4)-ys(1))*( (y-ys(1))*(y-ys(2))/(ys(4)-ys(2))*( (fys(4)-fys(3))/(ys(4)-ys(3))-(fys(3)-fys(2))/(ys(3)-ys(2)))+py1-py2)

expand(py1)
expand(py2)
expand(py3)

y=ys(2)
subs(py1)
subs(py2)
subs(py3)

% 4 b
syms z
zs=[0.6, 0.7, 0.8, 1.0]
fzs=[0.1769446, 0.01375227, 0.22363362, 0.65809197]
pz1=fzs(1)+(z-zs(1))*((fzs(2)-fzs(1))/(zs(2)-zs(1)))
pz2=pz1+(z-zs(1))*(z-zs(2))*( (fzs(3)-fzs(2))/(zs(3)-zs(2)) - (fzs(2)-fzs(1))/(zs(2)-zs(1)))/(zs(3)-zs(1))
pz3=pz2+(z-zs(3))/(zs(4)-zs(1))*( (z-zs(1))*(z-zs(2))/(zs(4)-zs(2))*( (fzs(4)-fzs(3))/(zs(4)-zs(3))-(fzs(3)-fzs(2))/(zs(3)-zs(2)))+pz1-pz2)

expand(pz1)
expand(pz2)
expand(pz3)

z=zs(1)
subs(pz1)
subs(pz2)
subs(pz3)