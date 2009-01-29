prologues:=3;
u:=25;                    % 25 = 25bp = 25 PostScript points = 30/72 in
wi:=10;                   % width  in units u
he:=8;                    % height in units u
hoehe:=he*u;              % height
breite:=wi*u;             % width

rx := 5u;
ry := 2u;
radius := 3u;

dy := 2u;

vardef draw_robot (expr rx, ry, c) =
  pickup pencircle scaled 1.5;
  draw fullcircle scaled (radius) shifted (rx, ry) withcolor c;
  draw (rx, ry)--(rx, ry+radius/2) withcolor c;
enddef;


beginfig(1)
  % --- Grid ---
  for i=0 upto he:
    draw (0, i*u)--(breite, i*u) withcolor .7white;
  endfor
  for j=0 upto wi:
    draw (j*u, 0)--(j*u, hoehe) withcolor .7white;
  endfor
  % --- End Grid ---

  % Obstacle
  fill (3, hoehe-3)--(breite-3, hoehe-3)--(breite-3, hoehe-6)--(3, hoehe-6)--cycle withcolor .75red;

  % Robots
  draw_robot (rx-0.9u, ry+0.3u+dy+0.1u, red);
  draw_robot (rx+1.3u, ry-0.25u+dy-0.1u, red);

  draw_robot (rx+0.1u, ry-0.2u+dy, red);
  draw_robot (rx-0.2u, ry+0.1u++dy-0.2u, red);

  draw_robot (rx, ry+dy, green);

  % Motion model
  drawarrow (rx+4u, ry)--(rx+4u, ry+dy) withcolor black;

endfig;
end
