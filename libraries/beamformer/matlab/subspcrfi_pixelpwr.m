% Calculate average of power around a pixel
% Pixel neighbour weights are near-Gaussian:
%   16 24 16
%   24 36 24
%   16 24 16
%
%  pwr=subspcrfi_pixelpwr(img, x, y)
%
function [pwr]=subspcrfi_pixelpwr(img, x, y)

  pwrU = 16*img(x-1,y-1) + 24*img(x,y-1) + 16*img(x+1,y-1);
  pwrM = 24*img(x-1,y)   + 36*img(x,y)   + 24*img(x+1,y);
  pwrL = 16*img(x-1,y+1) + 24*img(x,y+1) + 16*img(x+1,y+1);
  pwr = (pwrU + pwrM + pwrL) / (4*24+4*16+36);
