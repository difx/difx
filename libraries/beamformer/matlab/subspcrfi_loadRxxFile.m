% Import multi-channel covariance data and frequency list from a file.
% The C++ beamformer library can write these files.
%
% Output:
%   Rxx = Nant x Nant x Nch = multi-channel covariance data
%   freqs = 1 x Nant = list of channel frequencies in Hertz
%
function [Rxx,freqs]=subspcrfi_loadRxxFile(fn)

 fd = fopen(fn, 'r');

 Nch = fread(fd, 1, 'uint32');
 Nant = fread(fd, 1, 'uint32');
 Nmatrices = fread(fd, 1, 'uint32');
 nada = fread(fd, 1, 'uint32');
 
 freqs = fread(fd, [1 Nch], 'double');
 Rxx = zeros(Nant, Nant, Nch);
 
 for cc=1:Nch,
     for ii=1:Nant,
         cx_realimag_vec = fread(fd, [2 Nant], 'double');
         cx_vec = cx_realimag_vec(1,:) + i*cx_realimag_vec(2,:);
         Rxx(:, ii, cc) = cx_vec;
     end
 end
 
 fclose(fd);
