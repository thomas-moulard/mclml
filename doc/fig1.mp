prologues:=3;
u:=25;                    % 25 = 25bp = 25 PostScript points = 30/72 in
wi:=10;                   % width  in units u
he:=7;                    % height in units u
hoehe:=he*u;              % height
breite:=wi*u;             % width

beginfig(1)
  % --- Grid ---
  for i=0 upto he:
    draw (0, i*u)--(breite, i*u) withcolor .7white;
  endfor
  for j=0 upto wi:
    draw (j*u, 0)--(j*u, hoehe) withcolor .7white;
  endfor
  % --- End Grid ---

  % Robot
  pickup pencircle scaled 1.5;
  draw fullcircle scaled 3u shifted (5u, 4u) withcolor red;
  draw (5u, 4u)--(5u, 5.5u) withcolor red;

  % Axis
  drawarrow (2u, 1u)--(2u, 6u) withcolor black;
  drawarrow (2u, 1u)--(7u, 1u) withcolor black;

  % Label
  label.lft(btex $ (X, Y, \theta) ? $ etex, (8.25u, 5.25u));

endfig;
end
