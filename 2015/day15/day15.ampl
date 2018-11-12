# ingredients
set I;

param capacity{c in I};
param durability{d in I};
param flavor{f in I};
param texture{t in I};
param calories{c in I};

# quantity of each ingredient
var q{i in I};

set ps := capacity durability flavor texture calories;

maximize score:
  product{p in ps} (sum{i in I} p[i]*q[i]);

s.t. (sum{i in I} q[i]) == 100;


data;

set I := Sprinkles Butterscotch Chocolate Candy;

param capacity :=
 Sprinkles    2
 Butterscotch 0
 Chocolate    0
 Candy        0
 ;
param durability :=
 Sprinkles    0
 Butterscotch 5
 Chocolate    0
 Candy        -1
 ;
param flavor :=
 Sprinkles    -2
 Butterscotch -3
 Chocolate    5 
 Candy        0
 ;
param texture :=
 Sprinkles    0 
 Butterscotch 0 
 Chocolate    -1
 Candy        5
 ;
param calories :=
 Sprinkles    3
 Butterscotch 3
 Chocolate    8
 Candy        8
 ;