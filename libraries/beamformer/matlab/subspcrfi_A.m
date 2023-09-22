% Generate steering vectors [a1 a2 a3 .. a_Nch] for given direction
%
% Input: freqs = 1 x Nch          channel frequencies in Hertz
%        elemXYZ = 3 x Nant       antenna positions (x,y,z) in meters
%        phi = scalar             angle between plane normal and signal direction
%        theta = scalar           azimuth of signal direction
%
% Output: A = Nch x Nant          weight sets for each channel
%
function [A]=subspcrfi_A(freqs, elemXYZ, phi, theta)
 Nant = size(elemXYZ, 2);
 Nch = size(freqs, 2);
 A = zeros(Nch, Nant);

 % Process each frequency
 clight = 299792458;
 wavelens = clight ./ freqs; 
 for cc=1:Nch,
     a = subspcrfi_steer(phi, theta, wavelens(cc), elemXYZ);
     A(cc,:) = a;
 end

 return
 % Debug plot of phase, should show a tilted plane
 [xyz]=subspcrfi_elemXYZ(64, 10e-2);
 [At]=subspcrfi_A([1.4e9], xyz, 45*pi/180, 22.5*pi/180);
 Axy = reshape(unwrap(angle(At(1,:))), [8 8]); surf(Axy);
 