% Exports multi-channel covariance data and frequency list into a file.
% The C++ beamformer library can read these files.
%
% Inputs:
%   fn = file name and path
%   Rxx = Nant x Nant x Nch = multi-channel covariance data
%   freqs = 1 x Nant = list of channel frequencies
%
function subspcrfi_writeRxxFile(fn, Rxx, freqs)
 Nant = size(Rxx,1);
 Nch = size(Rxx,3);
 Nmatrices = 1;
 
 fd = fopen(fn, 'w+');

 fwrite(fd, Nch, 'uint32');
 fwrite(fd, Nant, 'uint32');
 fwrite(fd, Nmatrices, 'uint32');
 fwrite(fd, 0, 'uint32');
 
 freqs = freqs(1:Nch);
 fwrite(fd, freqs, 'double');
 
 cx_realimag_vec = zeros([2 Nant]);
 for cc=1:Nch,
     for ii=1:Nant,
         cx_vec = squeeze(Rxx(:, ii, cc));
         cx_realimag_vec(1, :) = real(cx_vec);
         cx_realimag_vec(2, :) = imag(cx_vec);
         fwrite(fd, cx_realimag_vec, 'double');
     end
 end

 % For reference the same as above in a more readable but slower form:
 %for cc=1:Nch,
 %    for ii=1:Nant,
 %        for jj=1:Nant,
 %            cx = Rxx(jj,ii,cc);
 %            fwrite(fd, real(cx), 'double');
 %            fwrite(fd, imag(cx), 'double');
 %        end
 %    end
 %end
  