function result = mie_thetascan(m, x, nsteps)
 
 % Computation and plot of Mie Power Scattering function for given
 % complex refractive-index ratio m=m'+im", size parameters x=k0*a,
 % according to Bohren and Huffman (1983) BEWI:TDD122
 % C. Maetzler, May 2002
 
  nsteps = nsteps;
  
  m1 = real(m);
  m2 = imag(m);
  
  nx = (1 : nsteps);
  dtheta = pi / (nsteps - 1);
  theta = (nx = 1) .* dtheta;
  for j = 1 : nsteps
    u = cos(theta(j));
    a(:, j) = mie_s12(m, x, u);
    sl(j) = real(a(1, j)' * a(1, j));
    sr(j) = real(a(2, j)' * a(2, j));
  end
  y = [theta, theta + pi; sl sr(nsteps : -1 : 1)]';
  polar(y(:, 1), y(:, 2))
  title(sprintf('Mie angular scattering: m=%g+%gi, x=%g', m1, m2, x));
  xlabel('Scattering Angle')
  result = y;
 end
