% Generate element position coordinates (x,y,z) for a square
% phased array layout in a plane where z=0.
%
% Input: Nant = scalar            number of antenna elements
%        elemspacing = scalar     spacing in meters
%
% Output: elemXYZ = 3 x Nant      element positions in meters
%
function [elemXYZ]=subspcrfi_elemXYZ(Nant, elemspacing)
 Lbox = round(sqrt(Nant));
 
 % Square array
 elemXYZ = zeros(3, Nant);
 if (mod(Lbox,2)==0),
    for row = (1:Lbox),
        idcs = (row-1)*Lbox + (1:Lbox);
        elemXYZ(1,idcs) = (1:Lbox) - (Lbox/2) - 0.5;
        elemXYZ(2,idcs) = row - (Lbox/2) - 0.5;
    end
    elemXYZ(3,:) = 0;
 else
    for row = (1:Lbox),
        idcs = (row-1)*Lbox + (1:Lbox);
        elemXYZ(1,idcs) = (1:Lbox) - ceil(Lbox/2);
        elemXYZ(2,idcs) = row - ceil(Lbox/2);
    end
    elemXYZ(3,:) = 0;
 end
 elemXYZ = elemspacing * elemXYZ;
 