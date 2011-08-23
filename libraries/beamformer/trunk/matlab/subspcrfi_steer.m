% Generate steering vectors for given direction (the phase shift
% of an arriving plane wave, measured at all element positions)
%
% Input: 
%        phi = scalar             angle between plane normal and signal direction
%        theta = scalar           azimuth of signal direction
%        lambda = scalar          wavelength
%        elempos = 3 x Nant       antenna positions (x,y,z in meters)
%
function [a]=subspcrfi_steer(phi, theta, lambda, elempos)

  pt_k = (2*pi/lambda)*[sin(theta)*cos(phi) sin(theta)*sin(phi) cos(theta)];
  pt_weights = exp(-j*pt_k*elempos);
  pt_weights = transpose(pt_weights);
  a = pt_weights;
 