-module(render_epx).
-export([start_link/0, stop/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include("../include/player_db.hrl").

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
	 scale :: {number(),number()}
	}).

-define(DEFAULT_WIDTH,  1000).
-define(DEFAULT_HEIGHT, 1000).
-define(DEFAULT_COLOR,  {255,0,255,255}).
-define(DEFAULT_FORMAT, argb).
-define(DEFAULT_INTERVAL, 500).

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
    self() ! update,
    ?daemon_tag_log(system, "Render epx server has been started", []),

    {MinX, MaxX, MinY, MaxY} = Area,
    ScaleX = Width/(MaxX - MinX),
    ScaleY = Height/(MaxY - MinY),
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
		scale = {ScaleX,ScaleY}
	       }}.


message_handler(S=#state{parent = Parent }) ->
    receive
        update ->
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
	    epx_gc:set_foreground_color(black),
	    player_db:foldl(	    
	      fun(#db_player{x=X0,y=Y0,neighbours=Ns}, _Acc) ->
		      Xi = Kx*X0 - Cx,
		      Yi = Ky*Y0 - Cy,
		      lists:foreach(
			fun(N) ->
				case ets:lookup(player_db, N) of
				    [#db_player{x=X1,y=Y1}] ->
					Xj = Kx*X1 - Cx,
					Yj = Ky*Y1 - Cy,
					epx:draw_line(Px, Xi, Yi, Xj, Yj);
				    _ ->
					skip
				end
			end, Ns)
	      end, ok),
	    %% draw nodes
	    player_db:foldl(
	      fun(#db_player{x=X0,y=Y0,name=Name}, _Acc) ->
		      X = Kx*X0 - Cx,
		      Y = Ky*Y0 - Cy,
		      %% io:format("draw pos = ~p\n", [{X,Y}]),
		      epx_gc:set_fill_style(solid),
		      epx_gc:set_fill_color(red),
		      epx:draw_ellipse(Px, X-3, Y-3, 6, 6),
		      %% precompute!?
		      String = binary_to_list(Name),
		      {W,H} = epx_font:dimension(Font, String),
		      epx_gc:set_fill_color({215,215,215}),
		      Lx = X-3-10,
		      Ly = Y-3-4-Ascent,
		      epx:draw_rectangle(Px, Lx-2, Ly, W+4, H),
		      epx_gc:set_foreground_color({0,0,0,0}),
		      epx:draw_string(Px, Lx, Ly + Ascent, String)
	      end, ok),
	    update_window(S),
            erlang:send_after(S#state.interval, self(), update),
            noreply;

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

update_window(State) ->
    epx:pixmap_copy_to(State#state.background, State#state.pixels),
    epx:pixmap_draw(State#state.pixels, 
		    State#state.window, 0, 0, 0, 0, 
		    State#state.width, 
		    State#state.height).
