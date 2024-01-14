(* ::Package:: *)

computeHistogram[data_, min_, max_, binCount_]:=Module[{binSize, measure},
binSize=(max-min)/binCount;
measure={};
For[i=1, i<=binCount,
cc=Length[Select[data, (i-1)*binSize<=#<(i)*binSize &]];
measure=Append[measure, {(i-0.5)*binSize, cc}];
i++];
measure]


computeDistance[A_, B_]:=Module[{i1, i2},
i1=Length[A];
i2=Length[B];
If[i1!=i2, Break];
Sqrt[Total[(A[[#]]-B[[#]])^2&/@Range[i1]]]]


throwRandomPointInCircle[radius_]:=Module[{point, D, finalPoint},
point=(2*Random[]-1)&/@Range[2];
D=computeDistance[point, {0,0}];
finalPoint =If[D<radius, point,throwRandomPointInCircle[radius] ]
]


checkBelowLine[point_, m_, b_]:=Module[{y},
y=m*point[[1]]+b;
If[point[[2]]<y, 1, 0]]


throwRandomPointInTriangle[]:=Module[{point, result, finalPoint},
point={N[2/3^(1/4)]*Random[]-1/3^(1/4), Random[]*3^(1/4)};
result=If[point[[1]]> 0, checkBelowLine[point, -3^(1/2), 3^(1/4)], checkBelowLine[point, 3^(1/2), 3^(1/4)]];
finalPoint=If[result==1, point,throwRandomPointInTriangle[] ]]


generateRandomPointOnCircle[r_]:=Module[{theta},
theta = 2* Pi * Random[];
{r*Cos[theta], r*Sin[theta], theta}]


computeHeron[A_, B_, C_]:=Module[{AB, BC, CA, S, area},
AB=computeDistance[A, B];
BC=computeDistance[B,C];
CA=computeDistance[C,A];
S=(AB+BC+CA)/2;
area=Sqrt[S*(S-AB)*(S-BC)*(S-CA)]]


computeEqualHeron[A_, B_, C_]:=Module[{AB, BC, CA, S, idealArea},
AB=computeDistance[A, B];
BC=computeDistance[B,C];
CA=computeDistance[C,A];
S=(AB+BC+CA)/2;
idealArea=N[S^2*Sqrt[1/27]]] 


generatePin[L_]:=Module[{x1, x2, y1, y2, theta},
x1=Random[];
y1=Random[];
theta = 2*Pi*Random[];
(* This doesn't provide any restriction that the line stays in the 1 x 1 square, but that is the whole point - want to see if the pin stays in the square. *)
x2=x1+L*Cos[theta];
y2 = y1 + L*Sin[theta];
{{x1, y1}, {x2, y2}}]


checkCrosses[pin_]:=Module[{x1, y1, x2, y2, xCross, yCross},
{{x1, y1}, {x2, y2}} = pin;
(* A nifty way to check for crosses :) *)
xCross=Abs[Floor[x1]-Floor[x2]];
yCross = Abs[Floor[y1]-Floor[y2]];
{xCross, yCross}]


getPinFit[L_]:=Module[{lineTemp},
lineTemp=generatePin[L];
If[Total[checkCrosses[lineTemp]]==0, lineTemp, getPinFit[L]]]


throwNeedle[L_, D_]:=Module[{Theta, x1=0, y1, x2, y2},
(* This function is different than getPinFit, because this needle doesnt have to fit. *)
(* What does D represent here, distance between two lines skip one? OR dimensionality?*)
y1= (D-1)*Random[];
Theta=Random[]*Pi/2;
y2=y1+N[L*Sin[Theta]];
x2=x1+N[L*Cos[Theta]];
{{x1, y1}, {x2, y2}}]


throwNeedles[L_, D_, N_]:=throwNeedle[L, D]&/@Range[N]


generatePinSub[xMax_, yMax_, xMin_, yMin_]:=Module[{x1, x2, y1, y2, theta},
(* This starts the pin on a subset of the available slots *)
x1=xMin+(xMax-xMin)*Random[];
y1=yMin+ (yMax-yMin)*Random[];
theta = 2*Pi*Random[];
x2=x1+Cos[theta];
y2 = y1 + Sin[theta];
{{x1, y1}, {x2, y2}}]


computePinPropensity[x_, y_]:=Module[{NN, dats},
NN=1000;
dats=Total[checkCrosses[generatePinSub[x, y, x-0.01, y-0.01]]]&/@Range[NN];
N[Length[Select[dats, #==0 &]]/NN]]


throwCoin[d_, R_]:={d*Random[], d*Random[], R}


pinTileCross[{x_, y_, R_}]:=Module[{xCross, yCross},
xCross=Abs[Floor[x+R]-Floor[x-R]];
yCross=Abs[Floor[y+R]-Floor[y-R]];
{xCross, yCross, If[xCross+yCross > 0, 1, 0]}]


computeDistance[A_, B_]:=Module[{i1, i2},
i1=Length[A];
i2=Length[B];
If[i1!=i2, Break];
Sqrt[Total[(A[[#]]-B[[#]])^2&/@Range[i1]]]]


generateThings[num_, radius_]:=Module[{i, things={{Random[], Random[]}}, position, distances},
For[i=1, i<num, 
(* Generate a new set of coordinates for a new particle.*)
position={(1-2*radius)*Random[]+radius, (1-2*radius)*Random[]+radius};
(* Compute the distances to all the existing particle things to determine which overlap *)
distances=computeDistance[position, #]&/@things;
L=Length[Select[distances, # < 2*radius &]];
(* Things is the list of non intersecting spheres or other objects *)
things=If[L==0, Append[things, position], things];
i++];
things]


determineDensity[DtoL_, worlds_]:=Module[{density={}, filling={}},
For[i=1,i< worlds,
(* Instead of 100 attempts here, we should try proportional to the likelihood of continuing, or stop once we've had X failures. *)
(* This is where efficiency comes in.  We need to give each scenario the same chances to converge. *)
 H=generateThings[300, DtoL/2]; 
density=Append[density, Length[H]*Pi*(DtoL/2)^2];
filling=Append[filling, If[Length[H]>1, 1, 0]];
i++];
{Mean[density], Mean[filling]}]


generateThings2[num_, radius_, height_]:=Module[{i, things={{Random[], Random[]}}, position, distances},
For[i=1, i<num, 
position={(1-2*radius)*Random[]+radius, (height-2*radius)*Random[]+radius};
distances=computeDistance[position, #]&/@things;
L=Length[Select[distances, # < 2*radius &]];
things=If[L==0, Append[things, position], things];
i++];
things]


pointInCircleS[point_, disks_, radius_]:=Module[{tally={}},
For[i=1, i<Length[disks],
tally=Append[tally, If[computeDistance[disks[[i]], point]<radius, 1, 0]];
i++];
Total[tally]]


measureSlice[disks_, xSlice_, radius_]:=Module[{i, isIn={}},
For[i=1, i<1000,
isIn=Append[isIn, pointInCircleS[{xSlice, i/100}, Q, radius]];
i++];
N[Total[isIn]/1000]]
