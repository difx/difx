% Plot a subset of the magnitudes given in the 'espec' eigenvalues matrix.
% Overlays the Ndom most dominant values into the same plot.
%
% Input: espec = 2 x Nant x Nch
% Input: Ndom = 1x1
function [pax]=subspcrfi_plotEVspec(espec, Ndom, heading, evspecAx)
  Nsets = size(espec, 1);
  Nant = size(espec, 2);
  Nch = size(espec, 3);
 
  if (nargin == 4),
    pax = evspecAx;
  else
    pax = [0, Nch, min(min(min(espec,[],2),[],3),[],1), max(max(max(espec,[],2),[],3),[],1)];
    pax = [0, Nch, 10*log10(min(min(min(espec,[],2),[],3),[],1)), 10*log10(max(max(max(espec,[],2),[],3),[],1))];
  end

  for pp=1:Nsets,
      subplot(Nsets,1,pp), hold on;
      for ii=1:Ndom,
        pcol = [0 0.2 0.2] + 0.8 * [1 0 0] * (Ndom-ii+1)/Ndom;
        evalues = abs(squeeze(espec(pp,ii,1:Nch)));
        plot((1:Nch), 10*log10(evalues), 'Color', pcol),
        axis(pax);
      end
      if (pp==1), tstr='On-Source'; else tstr='Off-source'; end
      title([heading tstr ' ' int2str(Ndom) '/' int2str(Nant) ' most dominant eigenvalues']);
      ylabel('[dB]'), xlabel('channel');
      
      % plot a curve of channel-medians
      medcurve = squeeze(median(espec(pp,1:Ndom,1:Nch),2));
      semilogy((1:Nch), 10*log10(abs(medcurve)), 'Color',[0.2 0.8 0.2]);
      
      % plot a curve of channel-averages
      avgcurve = squeeze(mean(espec(pp,1:Ndom,1:Nch),2));
      plot((1:Nch), 10*log10(abs(avgcurve)), 'Color',[0.2 0.2 0.8]);
  end
