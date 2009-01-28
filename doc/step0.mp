prologues:=3;
u:=25;                    % 25 = 25bp = 25 PostScript points = 30/72 in
wi:=10;                   % width  in units u
he:=8;                    % height in units u
hoehe:=he*u;              % height
breite:=wi*u;             % width

rx := 5u;
ry := 2u;

beginfig(1)
  % --- Grid ---
  for i=0 upto he:
    draw (0, i*u)--(breite, i*u) withcolor .7white;
  endfor
  for j=0 upto wi:
    draw (j*u, 0)--(j*u, hoehe) withcolor .7white;
  endfor
  % --- End Grid ---

  % Robot (init)
  pickup pencircle scaled 1.5;
  draw fullcircle scaled 3u shifted (rx, ry) withcolor red;
  draw (rx, ry)--(rx, ry+1.5u) withcolor red;

  % Robot (virtual)
  pickup pencircle scaled 1.5;
  draw fullcircle scaled 3u shifted (rx, ry+4u) withcolor blue;
  draw (rx, ry+4u)--(rx, ry+1.5u+4u) withcolor blue;

  % Robot (real)
  pickup pencircle scaled 1.5;
  draw fullcircle scaled 3u shifted (rx+.5u, ry+4u-.1u) withcolor red;
  draw (rx+.5u, ry+4u-.1u)--(rx+.5u, ry+1.5u+4u-.1u) withcolor red;

  % Motion model
  drawarrow (rx+3u, ry)--(rx+3u, ry+4u) withcolor black;

  % Axis
  drawarrow (2u, 1u)--(2u, 6u) withcolor black;
  drawarrow (2u, 1u)--(7u, 1u) withcolor black;

  % Label
  label.lft(btex $mouvement$ $attendu$ etex, (10.5u, 1.5u));

  label.lft(btex $position$ $attendue$ etex, (3.5u, 7u));
  label.lft(btex $position$ $du$ $robot$ etex, (10u, 7u));
endfig;
end
