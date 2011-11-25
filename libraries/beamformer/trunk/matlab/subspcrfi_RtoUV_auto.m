% Convert covariance matrix entries into UV visibilities
% Array elements i assumed to be on equispaced square grid
% and the order of Rxx values matches that of the physical layout
%
%  x\y
%    1   2   3   4   5   6   7   8
%    9  10  11  12  13  14  15  16
%    ....

function [uvd]=subspcrfi_RtoUV_auto(Rxx, freq_hz, elemXYZ)
 Nant = size(Rxx,1);
 Lbox = round(sqrt(Nant));
 center = ceil(sqrt(2*(Lbox-1)^2));
 mid_elem = round(Nant/2);
 
 uvd = zeros(2*center,2*center);
 weights = zeros(2*center,2*center);
 lambda = 299792458/freq_hz;
 elemXYZ = elemXYZ ./ lambda;
 
 for ii=1:Nant,
    ant_pos = [ (mod((ii-1),Lbox)+1) , (floor((ii-1)/Lbox)+1) ]; 
    ant_pos_mid = [ (mod((mid_elem-1),Lbox)+1) , (floor((mid_elem-1)/Lbox)+1) ]; 

    B = (ant_pos - ant_pos_mid) * eye(2); % (u,v), no projection
        
    uvpos1 = B + center;  % centered (u,v) + base (u,v)
    uvpos2 = -B + center; % centered (u,v) - base (u,v)
    uvpos1 = round(uvpos1);
    uvpos2 = round(uvpos2);

    % place autocorrelation
    vis = Rxx(ii,ii);
    uvd(uvpos1(1),uvpos1(2)) = uvd(uvpos1(1),uvpos1(2)) + vis; % uvd(u,v)
    %uvd(uvpos2(1),uvpos2(2)) = uvd(uvpos2(1),uvpos2(2)) + conj(vis); % uvd(v,u)
    weights(uvpos1(1),uvpos1(2)) = weights(uvpos1(1),uvpos1(2)) + 1;
    %weights(uvpos2(1),uvpos2(2)) = weights(uvpos2(1),uvpos2(2)) + 1;
 end
 
 % weight down to zero any missing points
 missing = (weights<=0);
 weights(missing) = 1e10;
 
 % apply uniform weighting
 uvd = uvd ./ weights;
 