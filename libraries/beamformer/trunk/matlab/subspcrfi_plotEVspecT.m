% Plot magnitude of all eigenvalues=totalpowers (versus frequency)
% Overlays the Nant antennas into the same plot.
%
% Input: espec = 2 x Nant x Nch
% Input: Ndom = 1x1
function subspcrfi_plotEVspecT(espec)
  Nsets = size(espec, 1);
  Nant = size(espec, 2);
  Nch = size(espec, 3);

  clf;

  pax = [0, Nch, min(min(min(espec,[],2),[],3),[],1), max(max(max(espec,[],2),[],3),[],1)];
  %pax = [0, Nch, 10*log10(min(min(min(espec,[],2),[],3),[],1)), 10*log10(max(max(max(espec,[],2),[],3),[],1))];
  for pp=1:Nsets,
      subplot(Nsets,1,pp), hold on;
      for ii=1:Nch,
        pcol = [0 0.2 0.2] + 0.8 * [1 0 0] * (Nch-ii+1)/Nch;
        evalues = squeeze(espec(pp,:,ii));
        evalues = abs(evalues); % real(evalues); 
        plot((1:Nant), evalues, 'Color', pcol),
        %axis(pax);
      end
      if (pp==1), tstr='On-Source'; else tstr='Off-source'; end
      title([tstr ' ' int2str(Nant) '-antenna powers, overlaid for each frequency']);
      ylabel('abs(eigenvalue)'), xlabel('antenna');
   end
