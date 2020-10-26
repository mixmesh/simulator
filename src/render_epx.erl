-module(render_epx).
-export([start_link/0, stop/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("player/include/player_serv.hrl").

-define(DEFAULT_DATASET, square).

-record(state,
	{
	 parent :: pid(),
	 width :: integer(),
	 height :: integer(),
	 color :: epx:color(),
	 format :: epx:epx_pixel_format(),
	 font   :: epx:epx_font(),
	 ascent :: integer(),
	 decent :: integer(),
	 header :: integer(),
	 pixels :: epx:epx_pixmap(),
	 background :: epx:epx_pixmap(),
	 window :: epx:epx_window() ,
	 interval :: non_neg_integer(),
	 area :: {number(),number(),number(),number()},
	 scale :: {number(),number()},
	 %% current position
	 pos0 :: #{ binary() => {X::float(),Y::float() }},
	 %% target position
	 pos1 :: #{ binary() => {X::float(),Y::float() }}
	}).

-define(DEFAULT_WIDTH,  1000).
-define(DEFAULT_HEIGHT, 1000).
-define(DEFAULT_COLOR,  {255,0,255,255}).
-define(DEFAULT_FORMAT, argb).
-define(DEFAULT_INTERVAL, 2000).

%% Exported: start_link

start_link() ->
    application:ensure_all_started(epx),
    ?spawn_server_opts(fun init/1,
                       fun message_handler/1,
                       #serv_options{name = ?MODULE}).

%% Exported: stop

stop() ->
    serv:call(?MODULE, stop).

%%
%% Server
%%
init(Parent) ->
    SimulatorModule = config:lookup([simulator, 'data-set']),
%%    MetersToDegrees = fun ?SIMULATOR_MODULE:meters_to_degrees/1,
%%    NeighbourDistance = MetersToDegrees(?NEIGHBOUR_DISTANCE_IN_METERS),
    init(Parent,[{dataset, SimulatorModule}]).

init(Parent,Options) ->
    Width  = proplists:get_value(width, Options, ?DEFAULT_WIDTH),
    Height = proplists:get_value(height, Options, ?DEFAULT_HEIGHT),
    Color  = proplists:get_value(color, Options, ?DEFAULT_COLOR),
    Format = proplists:get_value(format, Options, ?DEFAULT_FORMAT),
    SimulatorModule = proplists:get_value(dataset, Options, ?DEFAULT_DATASET),
    Interval = proplists:get_value(interval, Options, ?DEFAULT_INTERVAL),
    DefaultArea = SimulatorModule:get_area(),
    Area = proplists:get_value(area, Options, DefaultArea),
    {ok,Font} = epx_font:match([{size,10}]),
    Ascent = epx:font_info(Font, ascent),
    Descent = epx:font_info(Font, descent),
    Header = Descent + Ascent + 2,
    Pixels = epx:pixmap_create(Width,Height,Format),
    epx:pixmap_attach(Pixels),
    Background = epx:pixmap_create(Width,Height,Format),
    epx:pixmap_fill(Background, Color),
    Win = epx:window_create(30, 30, Width, Height,
			    [button_press, button_release]),
    epx:window_attach(Win),
    Steps = 30,
    Dt = 1/Steps,
    self() ! {update,Dt,0.0},
    ?daemon_log_tag_fmt(system, "Render epx server has been started", []),

    {MinX, MaxX, MinY, MaxY} = Area,
    ScaleX = Width/(MaxX - MinX),
    ScaleY = Height/(MaxY - MinY),
    Pos = get_positions(),
    {ok, #state{parent = Parent,
		width = Width,
		height = Height,
		color = Color,
		format = Format,
		font = Font,
		ascent = Ascent,
		decent = Ascent,
		header = Header,
		pixels = Pixels,
		background = Background,
		window = Win,
		interval = Interval,
		area = Area,
		scale = {ScaleX,ScaleY},
		pos0 = Pos,
		pos1 = Pos
	       }}.


message_handler(S=#state{parent = Parent }) ->
    receive
        {update,Dt,T} ->  %% Dt, T should be [0..1] Dt < T
	    %% draw into background
	    %% clear
	    Px = S#state.background,
	    Font = S#state.font,
	    epx_gc:set_font(Font),
	    Ascent = S#state.ascent,
	    {MinX, MaxX, MinY, MaxY} = S#state.area,
	    Width = S#state.width,
	    Height = S#state.height,
	    %% X' = Width*(X-MinX)/(MaxX-MinX)
	    %% Y' = Height*(Y-MinX)/(MaxY-MinY)
	    %% X = Sx*X0-Sx*MinX
	    Sx = Width/(MaxX-MinX),
	    Sy = Height/(MaxY-MinY),
	    Kx = Sx, Cx = Sx*MinX,
	    Ky = Sy, Cy = Sy*MinY,
	    epx:pixmap_fill(Px, S#state.color),
	    %% draw neighbour connections
	    Pos0 = S#state.pos0,
	    Pos1 = S#state.pos1,

	    player_db:foldl(
	      fun(#db_player{nym=Nym,neighbours=Ns}, _Acc) ->
		      {X0,Y0} = interp(Nym,T,Pos0,Pos1),
		      Xi = Kx*X0 - Cx,
		      Yi = Ky*Y0 - Cy,
		      lists:foreach(
			fun({N,up}) ->
				epx_gc:set_foreground_color(black),
				{X1,Y1} = interp(N,T,Pos0,Pos1),
				Xj = Kx*X1 - Cx,
				Yj = Ky*Y1 - Cy,
				Cnt = count(Nym, N),
				draw_dashed_line(Px, Xi, Yi, Xj, Yj, Cnt, 5);
			   ({N,pending}) ->
				epx_gc:set_foreground_color(lightgray),
				{X1,Y1} = interp(N,T,Pos0,Pos1),
				Xj = Kx*X1 - Cx,
				Yj = Ky*Y1 - Cy,
				epx:draw_line(Px, Xi, Yi, Xj, Yj);
			   ({_N,_}) ->
				ok
			end, Ns)
	      end, ok),
	    %% draw nodes
	    player_db:foldl(
	      fun(#db_player{nym=Nym}, _Acc) ->
		      {X0,Y0} = interp(Nym,T,Pos0,Pos1),
		      X = Kx*X0 - Cx,
		      Y = Ky*Y0 - Cy,
		      %% io:format("draw pos = ~p\n", [{X,Y}]),
		      epx_gc:set_fill_style(solid),
		      epx_gc:set_fill_color(red),
		      epx:draw_ellipse(Px, X-3, Y-3, 6, 6),
		      %% precompute!?
		      String = binary_to_list(Nym),
		      {W,H} = epx_font:dimension(Font, String),
		      epx_gc:set_fill_color({215,215,215}),
		      Lx = X-3-10,
		      Ly = Y-3-4-Ascent,
		      epx:draw_rectangle(Px, Lx-2, Ly, W+4, H),
		      epx_gc:set_foreground_color({0,0,0,0}),
		      epx:draw_string(Px, Lx, Ly + Ascent, String)
	      end, ok),
	    update_window(S),
	    T1 = T+Dt,
	    Interval = trunc(S#state.interval*Dt),
	    if T1 >= 1 ->
		    Pos = get_positions(),
		    erlang:send_after(Interval,self(),{update,Dt,0.0}),
		    {noreply, S#state { pos0 = Pos1, pos1 = Pos }};
	       true ->
		    erlang:send_after(Interval, self(),{update,Dt,T1}),
		    noreply
	    end;

	{epx_event,W,close} when W =:= S#state.window ->
	    epx:window_detach(S#state.window),
	    epx:window_detach(S#state.pixels),
	    stop;

        {call, From, stop} ->
	    epx:window_detach(S#state.window),
	    epx:window_detach(S#state.pixels),
            {stop, From, ok};

        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.

count(A, B) ->
    Key = [A|B],
    Cnt = case get(Key) of
	      undefined -> 1;
	      N -> N+1
	  end,
    put(Key, Cnt),
    Cnt.

interp(Nym,T,Pos0,Pos1) ->
    {X0,Y0} = maps:get(Nym,Pos0,{0,0}),
    {X1,Y1} = maps:get(Nym,Pos1,{0,0}),
    {X0+(X1-X0)*T, Y0+(Y1-Y0)*T}.

-define(ANIM_BITS, 3).
draw_dashed_line(Px, X0, Y0, X1, Y1, Cnt, SegLen) ->
    Dx = (X1-X0),
    Dy = (Y1-Y0),
    Len = math:sqrt(Dx*Dx + Dy*Dy),
    N = max(1, round(Len / SegLen)),
    C = Cnt band ((1 bsl ?ANIM_BITS)-1),
    Dl = 1/N,
    L0 = C*(Dl / (1 bsl ?ANIM_BITS)),
    draw_segments(1, N, L0, 1/N, Px, X0, Y0, X0, Y0, Dx, Dy).

draw_segments(I, N, L, Dl, Px, X0, Y0, X1, Y1, Dx, Dy) ->
    if I > N -> ok;
       true ->
	    X2 = X0 + Dx*L,
	    Y2 = Y0 + Dy*L,
	    if (I band 1) =:= 1 ->
		    epx:draw_line(Px, X1, Y1, X2, Y2);
	       true -> ok
	    end,
	    draw_segments(I+1, N, L+Dl, Dl, Px, X0, Y0, X2, Y2, Dx, Dy)
    end.


%% Get target positions
get_positions() ->
    player_db:foldl(
      fun(#db_player{nym=N,x=X,y=Y}, Pos1) ->
	      Pos1#{ N => {X,Y}}
      end, #{}).

update_window(S) ->
    epx:pixmap_copy_to(S#state.background, S#state.pixels),
    epx:pixmap_draw(S#state.pixels,
		    S#state.window, 0, 0, 0, 0,
		    S#state.width,
		    S#state.height).
