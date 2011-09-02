% Exports multi-channel covariance data and frequency list into a file.
% The C++ beamformer library can read these files.
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
 
 for cc=1:Nch,
     for ii=1:Nant,
         for jj=1:Nant,
             cx = Rxx(jj,ii,cc);
             fwrite(fd, real(cx), 'double');
             fwrite(fd, imag(cx), 'double');
             if ((cc==1) && (ii<7) && (jj<7)),
                 fprintf(1, '(%f,%f) ', real(cx), imag(cx));
                 if (jj==6),
                     fprintf(1, '\n');
                 end
             end
         end
     end
 end
 
 fclose('all');

 