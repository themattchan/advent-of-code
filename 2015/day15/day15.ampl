# https://ampl.com/cgi-bin/ampl/amplcgi

# commands:
#
# solve;
# display score;
#

# ingredients
set I;

param capacity{I};
param durability{I};
param flavor{I};
param texture{I};
param calories{I};

# quantity of each ingredient
var q{I};


maximize score:
    (sum{i in I} (capacity[i]*q[i])) *
    (sum{i in I} (durability[i]*q[i])) *
    (sum{i in I} (flavor[i]*q[i])) *
    (sum{i in I} (texture[i]*q[i]));
##    (sum{i in I} calories[i]*q[i]) ;
subject to c1:
  (sum {i in I} q[i]) = 100;

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
