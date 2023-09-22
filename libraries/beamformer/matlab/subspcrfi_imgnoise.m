% Returns "rms" noise of 2D image (1 color channel) or noise of 2D matrix
%
% RMS noise is computed by reshaping matrix into vector, then doing std()
% on it to get standard deviation (which is interpreted as the rms).
% Also returns the mean of the data.
%
% [rms,M]=subspcrfi_imgnoise(img)
%
function [rms,M]=subspcrfi_imgnoise(img)
  N1 = size(img,1);
  N2 = size(img,2);
  img2 = reshape(img, [1 N1*N2]);
  rms = std(img2);
  M = mean(img2);
  