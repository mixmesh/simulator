-module(habitat2).
-compile(export_all).
-export([demo/0]).

-define(PI, 3.141592).

-define(PIXEL_FORMAT, rgb).
%% https://lospec.com/palette-list/cingi-25
-define(BACKGROUND_COLOR, {232, 236, 242}).
-define(HABITAT_COLOR, {31, 37, 54}).
-define(PLAYER_COLOR0, {255, 0, 0}).
-define(PLAYER_COLOR1, {255, 0, 255}).
-define(PLAYER_COLOR2, {0, 255, 255}).
-define(PLAYER_COLOR3, {128, 0, 128}).
-define(PLAYER_COLOR4, {0, 255, 0}).
-define(MAX_SAVED_LOCATIONS, 200).

-record(habitat,
        {f1 :: {number(), number()},
         f2 :: {number(), number()},
         r :: number()}).

demo() ->
    {_X, _Y, _Z, Side, MetersPerPixel} = location(),
    Speed = 2.8, %% 10 km/h
    demo(Side, Side, MetersPerPixel, Speed, "home.png").

location() ->
    %% St Olofsgatan 60A
    %% https://developers.google.com/maps/documentation/maps-static/start
    %% https://maps.googleapis.com/maps/api/staticmap?center=59.865906,17.644667&zoom=16&size=400x400&scale=2&key=AIzaSyA2xg80wkvGOFsLdHZ72BhH5PmpDDA3lvQ
    Side = 400,
    Longitude = 17.644667,
    Latitude = 59.865906,
    Altitude = 15,
    Zoom = 16,
    Scale = 2,
    {X, Y, Z} =
        locationlib:geodetic_to_ecef_coordinates(Latitude, Longitude, Altitude),
    MetersPerPixel = locationlib:meters_per_pixel(Latitude, Zoom),
    {X, Y, Z, Side * Scale, MetersPerPixel * Scale}.

demo(Width, Height, MetersPerPixel, Speed, BackgroundFilename) ->
    application:ensure_all_started(epx),
    Pixmap = epx:pixmap_create(Width, Height, ?PIXEL_FORMAT),
    epx:pixmaps_attach(Pixmap),
    Filename = filename:join([code:priv_dir(simulator), BackgroundFilename]),
    {ok, BackgroundImage} = epx_image:load(Filename),
    Background = hd(epx_image:pixmaps(BackgroundImage)),
    Canvas = epx:pixmap_create(Width, Height, ?PIXEL_FORMAT),
    epx:pixmap_copy_to(Background, Canvas),
    Window = epx:window_create(
               0, 0, Width, Height, [button_press, button_release]),
    epx:window_attach(Window),
    UpdateFrequency = 1,
    W = UpdateFrequency * 3600, % Updates per hour
    T = 0.1,  %% Time to consider in hours (6 minutes)    
    Alpha = calculate_alpha(W, T),
    Beta = 60,
    Width4 = Width / 4,
    Height4 = Height / 4,
    Plot = fun(clear) ->
                   epx:pixmap_copy_to(Background, Canvas);
              (update_window) ->
                   update_window(Width, Height, Window, Pixmap, Canvas);
              ({{X, Y}, Color, SavedLocations}) ->
                   epx_gc:set_fill_color(Color),
                   epx:draw_rectangle(Canvas, X - 2, Y - 2, 4, 4),
                   epx_gc:set_foreground_color(Color),
                   (fun F([{X1, Y1}, {X2, Y2}|Rest]) ->
                            epx:draw_line(Canvas, X1, Y1, X2, Y2),
                            F([{X2, Y2}|Rest]);
                        F(_) ->
                            ok
                    end)([{X, Y}|SavedLocations]),
                   {X, Y};
              (#habitat{r = 0}) ->
                   skip;
              (#habitat{f1 = {F1X, F1Y}, f2 = {F2X, F2Y}, r = R}) ->
                   epx_gc:set_foreground_color(?HABITAT_COLOR),
                   draw_ellipse(Canvas, F1X, F1Y, F2X, F2Y, R)
           end,
    iterate(Window, Width, Height, MetersPerPixel,
            [{fun() -> noise(0.05) end,
              fun() -> noise(0.05) end,
              Width4, Height4, ?PLAYER_COLOR0, not_set, 1, 1, []},
             {fun() -> noise(0.05) end,
              fun() -> noise(0.05) end,
              Width4 * 3, Height4, ?PLAYER_COLOR1, not_set, 1, 1, []},
             {fun() -> noise(0.05) end,
              fun() -> noise(0.05) end,
              Width4 * 2, Height4 * 2, ?PLAYER_COLOR2, not_set, 1, 1, []},
             {fun() -> noise(0.05) end,
              fun() -> noise(0.05) end,
              Width4, Height4 * 3, ?PLAYER_COLOR3, not_set, 1, 1, []},
             {fun() -> noise(0.05) end,
              fun() -> noise(0.05) end,
              Width4 * 3, Height4 * 3, ?PLAYER_COLOR4, not_set, 1, 1, []}],
            UpdateFrequency, Speed, Alpha, Beta, Plot).

iterate(Window, Width, Height, MetersPerPixel, Players, UpdateFrequency, Speed,
        Alpha, Beta, Plot) ->
    Plot(clear),
    UpdatedPlayers =
        lists:foldl(
          fun({NextXDelta, NextYDelta, X, Y, Color, Habitat,
               XDirection, YDirection, SavedLocations},
              Acc) ->
                  {XDelta, EvenNextXDelta} = NextXDelta(),
                  XMovement = Speed / UpdateFrequency * XDelta / MetersPerPixel,
                  io:format("XDelta: ~p\n", [{XDelta, XMovement}]),
                  {NewX, NewXDirection} =
                      maybe_change_direction(X, XDirection, XMovement, Width),
                  {YDelta, EvenNextYDelta} = NextYDelta(),
                  YMovement = Speed / UpdateFrequency * YDelta / MetersPerPixel,
                  io:format("YDelta: ~p\n", [{YDelta, YMovement}]),
                  {NewY, NewYDirection} =
                      maybe_change_direction(Y, YDirection, YMovement, Height),
                  Plot({{NewX , NewY}, Color, SavedLocations}),
                  UpdatedHabitat =
                      case Habitat of
                          not_set ->
                              init_habitat({NewX, NewY});
                          _ ->
                              update_habitat(Habitat, {NewX, NewY}, Alpha, Beta)
                      end,
                  Plot(UpdatedHabitat),
                  UpdatedSavedLocations =
                      case length(SavedLocations) > ?MAX_SAVED_LOCATIONS of
                          true ->
                              [{NewX, NewY}|lists:droplast(SavedLocations)];
                          false ->
                              [{NewX, NewY}|SavedLocations]
                      end,
                  [{EvenNextXDelta, EvenNextYDelta, NewX, NewY, Color,
                    UpdatedHabitat, NewXDirection, NewYDirection,
                    UpdatedSavedLocations}|Acc]
          end, [], Players),
    Plot(update_window),
    case is_window_closed(Window, 0) of
        true ->
            ok;
        false ->
            Delay = trunc(1000 / UpdateFrequency),
            io:format("BAJS: ~p\n", [Delay]),
            timer:sleep(Delay),
            iterate(Window, Width, Height, MetersPerPixel, UpdatedPlayers,
                    UpdateFrequency, Speed, Alpha, Beta, Plot)
    end.

maybe_change_direction(C, Direction, Movement, Limit) ->
    case C + Direction * Movement of
        NewC when NewC > 0 andalso NewC < Limit ->
            {NewC, Direction};
        _ ->
            NewDirection = Direction * -1,
            {C + NewDirection * Movement, NewDirection}
    end.

%% https://stackoverflow.com/questions/11944767/draw-an-ellipse-based-on-its-foci/11947391#11947391
draw_ellipse(Canvas, X1, Y1, X2, Y2, K) ->
    draw_ellipse(Canvas, X1, Y1, X2, Y2, K, 2 * ?PI, ?PI / 20, not_set, 0).

draw_ellipse(Canvas, X1, Y1, X2, Y2, K, Stop, Step, Last, T)
  when T > Stop ->
    draw_ellipse(Canvas, X1, Y1, X2, Y2, K, Stop, Step, {stop, Last}, Stop);
draw_ellipse(Canvas, X1, Y1, X2, Y2, K, Stop, Step, Last, T) ->
    %% Major axis
    A = K / 2.0, 
    %% Coordinates of the center
    Xc = (X1 + X2) / 2.0,
    Yc = (Y1 + Y2) / 2.0,
    %% Distance of the foci to the center
    Dc = math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)) / 2,
    %% Minor axis
    B = math:sqrt(abs(math:pow(A, 2) - math:pow(Dc, 2))),
    Phi = math:atan(abs(Y1 - Y2) / abs(X1 - X2)),
    Xt = Xc + A * math:cos(T) * math:cos(Phi) - B * math:sin(T) * math:sin(Phi),
    Yt = Yc + A * math:cos(T) * math:sin(Phi) + B * math:sin(T) * math:cos(Phi),
    case Last of
        not_set ->
            draw_ellipse(Canvas, X1, Y1, X2, Y2, K, Stop, Step, {Xt, Yt},
                         T + Step);
        {stop, {X, Y}} ->
            epx:draw_line(Canvas, X, Y, Xt, Yt);
        {X, Y} ->
            epx:draw_line(Canvas, X, Y, Xt, Yt),
            draw_ellipse(Canvas, X1, Y1, X2, Y2, K, Stop, Step, {Xt, Yt},
                         T + Step)
    end.

is_window_closed(Window, Timeout) ->
    receive
        {epx_event, Window, close} ->
            epx:window_detach(Window),
            true
    after
        Timeout ->
            false
    end.

update_window(Width, Height, Window, Pixmap, Canvas) ->
    epx:pixmap_copy_to(Canvas, Pixmap),
    epx:pixmap_draw(Pixmap, Window, 0, 0, 0, 0, Width, Height).

%% Section 3.3.1
init_habitat(L0) ->
    #habitat{f1 = L0, f2 = L0, r = 0}.

update_habitat(H, NewL, Alpha, Beta) ->
    {F1, F2} = update_focal_points(H, NewL, Alpha, Beta),
    R = update_radius(H#habitat.r, NewL, Alpha, F1, F2),
    #habitat{f1 = F1, f2 = F2, r = R}.

%% Section 3.3.2
update_focal_points(#habitat{f1 = F1, f2 = F2, r = _R}, L, Alpha, Beta) ->
    {F1Old, F2Old} =
        case {d(F1, L), d(F2, L)} of
            {F1LDistance, F2LDistance} when F1LDistance < F2LDistance ->
                {F1, F2};
            _ ->
                {F2, F1}
        end,
    {new_f1(L, Alpha, F1Old), new_f2(L, Alpha, Beta, F2Old)}.

d({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

new_f1({X, Y}, Alpha, {XOld, YOld}) ->
    {X * Alpha + XOld * (1 - Alpha), Y * Alpha + YOld * (1 - Alpha)}.

new_f2({X, Y}, Alpha, Beta, {XOld, YOld}) ->
    AlphaBetaQuota = Alpha / Beta,
    {X * AlphaBetaQuota + XOld * (1 - AlphaBetaQuota),
     Y * AlphaBetaQuota + YOld * (1 - AlphaBetaQuota)}.

%% Section 3.3.3
update_radius(OldR, L, Alpha, F1, F2) ->
    (d(L, F1) + d(L, F2)) * Alpha + OldR * (1 - Alpha).

%% Section 3.3.4
calculate_alpha(W, T) ->
    2 / (T * W + 1).

%%
%% Perlin noise
%%

%% https://en.wikipedia.org/wiki/Perlin_noise
noise(Step) ->
    A = rand:uniform(),
    B = rand:uniform(),
    {A, fun() -> noise(Step, Step, A, B) end}.

noise(1.0, Step, _A, B) ->
    {B, fun() -> noise(Step, Step, B, rand:uniform()) end};
noise(Travel, Step, _A, B) when Travel > 1 ->
    NextB = rand:uniform(),
    InterpolatedB = smoothstep(B, NextB, 1 - Travel),
    {InterpolatedB, fun() -> noise(Step - (1 - Travel), Step, B, NextB) end};
noise(Travel, Step, A, B) ->
    InterpolatedB = smoothstep(A, B, Travel),
    {InterpolatedB, fun() -> noise(Travel + Step, Step, A, B) end}.

%% https://en.wikipedia.org/wiki/Smoothstep
smoothstep(A, B, W) ->
    (B - A) * (3.0 - W * 2.0) * W * W + A.
