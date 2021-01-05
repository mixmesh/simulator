%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    General ellipse:
%%%    Ax² + Bxy + Cy² + Dx + Ey + F = 0
%%%    when B² - 4AC < 0
%%% @end
%%% Created : 28 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(ellipse).

-compile(export_all).

params_ab(X0,Y0, A,B, Theta) ->
    params_ab_(X0,Y0,A,B,math:cos(Theta),math:sin(Theta)).

params_ab_(X0,Y0,A,B,Ct,St) ->
    A2 = A*A,
    B2 = B*B,
    St2 = St*St,
    Ct2 = Ct*Ct,
    Ap = A2*St2 + B2*Ct2,
    Bp = 2*(B2-A2)*St*Ct,
    Cp = A2*Ct2 + B2*St2,
    Dp = -2*Ap*X0 - Bp*Y0,
    Ep = -Bp*X0 - 2*Cp*Y0,
    Fp = Ap*X0*X0 + Bp*X0*Y0 + Cp*Y0*Y0 - A2*B2,
    {Ap,Bp,Cp,Dp,Ep,Fp}.

%% x0=y0=0
params_ab(A,B,Theta) ->
    params_ab_(A,B,math:cos(Theta),math:sin(Theta)).
params_ab_(A,B,Ct,St) ->
    A2 = A*A,
    B2 = B*B,
    St2 = St*St,
    Ct2 = Ct*Ct,
    Ap = A2*St2 + B2*Ct2,
    Bp = 2*(B2-A2)*St*Ct,
    Cp = A2*Ct2 + B2*St2,
    Fp = -A2*B2,
    {Ap,Bp,Cp,Fp}.

%% foci coordinates (0,+-C) C^2 = A^2 - B^2
%% R = 2*A (A semi-major axis) B = minor axis
ellipse_r(X0, Y0, X1, Y1, R) ->
    Dx = X1-X0,
    Dy = Y1-Y0,
    Theta = math:atan2(-Dy,Dx),
    %% focal distance from center
    F = math:sqrt(Dx*Dx + Dy*Dy)/2,
    %% center
    Xc = (X0+X1)/2,
    Yc = (Y0+Y1)/2,
    %% F = sqrt(A^2 + B^2)
    %% B^2 = F^2 - A^2 = F^2 - (R/2)^2
    A = R/2,
    B = math:sqrt(F*F - A*F),
    io:format("A = ~.2f, B = ~.2f, Theta = ~.2f\n", [A,B,Theta]),
    if Xc == 0, Yc == 0 ->
	    params_ab(A, B, Theta);
       true -> params_ab(Xc, Yc, A, B, Theta)
    end.
    
%% on ellipse curve
on_elliplse(X,Y, {A,B,C,F}) ->
    (A*X*X + B*X*Y + C*Y*Y + F == 0);
on_elliplse(X,Y, {A,B,C,D,E,F}) ->
    (A*X*X + B*X*Y + C*Y*Y + D*X + E*Y + F == 0).

%%
%%  E(x,y) = Ax2 + Bxy + Cy2 + F  = 0
%% 
%%  E(x,y) = Ax2 + Bxy + Cy2 + F 
%%  E(x+1,y) = A(x+1)^2 + B(x+1)y + Cy2 + F =
%%           = Ax^2 + 2Ax + A + Bxy + By + Cy2 + F =
%%           = E(x,y) + A + By + 2Ax
%%

ellipse_ab(Xc,Yc,A,B,Theta) ->
    ellipse_ab_r(Xc,Yc,A,B,(Theta/180)*math:pi()).

ellipse_ab_r(Xc,Yc,A,B,Theta) ->
    Ct = math:cos(Theta),
    St = math:sin(Theta),
    ellipse_ab_(Xc,Yc,A,B,Ct,St).

ellipse_ab_(Xc,Yc,A,B,Ct,St) ->
    Pc = {Xc,Yc},
    CS = {Ct,St},
    %% bounding box used is the coordinates
    Ps  = [{A,B},{-A,B},{-A,-B},{A,-B}],
    Ps1 = [ rotate_pt_ccw(P,CS) || P <- Ps ],
    Ps2 = [ add_pt(Pc,P) || P <- Ps1],
    Xs  = [X || {X,_} <- Ps1],
    Ys  = [Y || {_,Y} <- Ps1],
    {X0,Y0} = {lists:min(Xs),lists:min(Ys)},
    {X1,Y1} = {lists:max(Xs),lists:max(Ys)},
    W = X1-X0+1,
    H = Y1-Y0+1,
    Box = {X0,Y0,W,H},
    Params = params_ab_(A,B,Ct,St),
    epx_view:show(
      fun(Pix) ->
	      epx_gc:set_fill_style(none),
	      epx_gc:set_line_style(none),
	      draw_poly(Pix,Ps2),
	      epx_view:draw_rectangle(Pix,X0+Xc,Y0+Yc,W,H),
	      P0 = point(0,Pc,A,B,CS),
	      for(
		fun(Phi,Pi) ->
			Pj = point(Phi,Pc,A,B,CS),
			epx_view:draw_line(Pix,Pi,Pj),
			Pj
		end, P0, 0.1, 2*math:pi(), 0.1),
	      draw_box(Pix, Pc, Box, Params)
      end, {50, 50, 640, 480}),
    Box.

point(Phi,Pc,A,B,CS) ->
    Pi = {A*math:cos(Phi),B*math:sin(Phi)},
    P1 = rotate_pt_ccw(Pi, CS),
    add_pt(P1, Pc).

draw_box(Pix, Pc, {X,Y,W,H}, Params) ->
    epx_gc:set_line_style(blend),
    draw_y(Pix,Pc, Y,Y+H,X,W,Params).

draw_y(Pix,Pc, Yi,Yn,X,W,Params) ->
    if Yi < Yn ->
	    draw_x(Pix,Pc,X,X+W,Yi,Params),
	    draw_y(Pix,Pc,Yi+1,Yn,X,W,Params);
       true ->
	    ok
    end.

draw_x(Pix,Pc,X,Xm,Yi,{A,B,C,F}) ->
    E0 = A*X*X + B*X*Yi + C*Yi*Yi + F,
    if E0 > 0 ->
	    draw_out(Pix,Pc,X,Xm,Yi, E0, A + B*Yi + 2*A*X, 2*A);
       true ->
	    draw_in(Pix,Pc,X,Xm,Yi, E0, A + B*Yi + 2*A*X, 2*A)
    end.

%% Ei' = Ei + A+By + 2Ax
draw_out(Pix,Pc,X,Xm,Yi,Ei,Es,Esx) ->
    if X < Xm ->
	    Ei1 = Ei+Es+Esx,
	    Es1 = Es+Esx,
	    if Ei > 0 ->
		    draw_out(Pix,Pc,X+1,Xm,Yi,Ei1,Es1,Esx);
	       Ei =:= 0 ->
		    Pi = add_pt(Pc,{X,Yi}),
		    epx_gc:set_foreground_color(black),
		    epx_view:draw_point(Pix,Pi),
		    draw_in(Pix,Pc,X+1,Xm,Yi,Ei1,Es1,Esx);
	       Ei < 0 ->
		    draw_in(Pix,Pc,X+1,Xm,Yi,Ei1,Es1,Esx)
	    end;
       true ->
	    ok
    end.
		    
draw_in(Pix,Pc,X,Xm,Yi,Ei,Es,Esx) ->
    if X < Xm ->
	    Pi = add_pt(Pc,{X,Yi}),
	    Ei1 = Ei+Es+Esx,
	    Es1 = Es+Esx,
	    if Ei < 0 ->
		    %% epx_view:put_pixel(Pix,Pi,{10,127,127,127}),
		    epx_gc:set_foreground_color({127,127,127,127}),
		    epx_view:draw_point(Pix,Pi),
		    draw_in(Pix,Pc,X+1,Xm,Yi,Ei1,Es1,Esx);
	       Ei == 0 ->
		    epx_gc:set_foreground_color(black),
		    epx_view:draw_point(Pix,Pi);
	       true ->
		    ok
	    end;
       true ->
	    ok
    end.

%% clockwise
rotate_pt_cw({X,Y}, {C,S}) ->
    {X*C+Y*S, -X*S + Y*C}.
%% counter clockwise
rotate_pt_ccw({X,Y}, {C,S}) ->
    {X*C-Y*S, X*S + Y*C}.

add_pt({X0,Y0},{X1,Y1}) ->
    {X0+X1,Y0+Y1}.

scale_pt({X0,Y0}, F) ->
    {X0*F, Y0*F}.

draw_poly(Pix, [P0 | Ps=[P1|_]]) ->
    epx_view:draw_line(Pix, P0, P1),
    draw_poly_(Pix, Ps, P0).

draw_poly_(Pix, [P0 | Ps=[P1 |_]], Pn) ->
    epx_view:draw_line(Pix, P0, P1),
    draw_poly_(Pix, Ps, Pn);
draw_poly_(Pix, [Pi], Pn) ->
    epx_view:draw_line(Pix, Pi, Pn).

%% utility create a window and attach a drawing callback

for(Fun, Acc, Cur, Stop, Step) when Step>0, Cur < Stop ->
    Acc1 = Fun(Cur,Acc),
    for(Fun, Acc1, Cur+Step, Stop, Step);
for(Fun, Acc,  Cur, Stop, Step) when Step<0, Cur > Stop ->
    Acc1 = Fun(Cur, Acc),
    for(Fun, Acc1, Cur+Step, Stop, Step);
for(_Fun, Acc, _Cur, _Stop, _Step) ->
    Acc.

    


