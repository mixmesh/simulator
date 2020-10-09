-module(roma_taxi).
-export([get_area/0, generate_area/0]).
-export([get_location_generator/1, get_location_index/0]).
-export([neighbour_distance_in_meters/0]).
-export([degrees_to_meters/1, meters_to_degrees/1]).
-export([width_height_in_meters/0]).
-export([zscoring/0]).

-include_lib("apptools/include/shorthand.hrl").

-define(A_FEW_CABS, [<<"2">>,<<"3">>,<<"4">>,<<"7">>,<<"8">>,<<"11">>,<<"12">>,<<"13">>,<<"14">>,<<"17">>,<<"21">>]).
-define(A_FEW_CABS_AREA,
        {12.0326453781964,12.8296019878516,41.6899184854779,42.2023616666667}).

-define(A_FEW_CABS_ZSCORED, [<<"11">>,<<"12">>,<<"13">>,<<"8">>]).
-define(A_FEW_CABS_ZSCORED_AREA,
        {12.2229865677838,12.8296019878516,41.6899184854779,42.0587389930412}).

%% 19 is strange (removed)
-define(ALL_CABS, [<<"2">>,<<"3">>,<<"4">>,<<"7">>,<<"8">>,<<"11">>,<<"12">>,<<"13">>,<<"14">>,<<"17">>,<<"21">>,<<"22">>,<<"23">>,<<"24">>,<<"28">>,<<"29">>,<<"30">>,<<"31">>,<<"32">>,<<"37">>,<<"39">>,<<"40">>,<<"41">>,<<"42">>,<<"43">>,<<"44">>,<<"45">>,<<"46">>,<<"47">>,<<"48">>,<<"49">>,<<"50">>,<<"51">>,<<"53">>,<<"54">>,<<"55">>,<<"56">>,<<"57">>,<<"58">>,<<"59">>,<<"60">>,<<"61">>,<<"62">>,<<"65">>,<<"66">>,<<"67">>,<<"68">>,<<"70">>,<<"71">>,<<"72">>,<<"73">>,<<"74">>,<<"75">>,<<"76">>,<<"78">>,<<"79">>,<<"80">>,<<"81">>,<<"82">>,<<"83">>,<<"84">>,<<"85">>,<<"87">>,<<"88">>,<<"89">>,<<"90">>,<<"91">>,<<"92">>,<<"93">>,<<"94">>,<<"95">>,<<"96">>,<<"97">>,<<"99">>,<<"100">>,<<"101">>,<<"102">>,<<"103">>,<<"104">>,<<"105">>,<<"106">>,<<"107">>,<<"109">>,<<"110">>,<<"111">>,<<"112">>,<<"113">>,<<"115">>,<<"117">>,<<"118">>,<<"119">>,<<"120">>,<<"121">>,<<"122">>,<<"123">>,<<"124">>,<<"125">>,<<"126">>,<<"127">>,<<"128">>,<<"129">>,<<"130">>,<<"131">>,<<"132">>,<<"134">>,<<"135">>,<<"137">>,<<"138">>,<<"139">>,<<"140">>,<<"141">>,<<"143">>,<<"144">>,<<"145">>,<<"147">>,<<"148">>,<<"149">>,<<"150">>,<<"151">>,<<"152">>,<<"153">>,<<"154">>,<<"155">>,<<"156">>,<<"157">>,<<"158">>,<<"159">>,<<"160">>,<<"162">>,<<"163">>,<<"166">>,<<"167">>,<<"169">>,<<"170">>,<<"171">>,<<"172">>,<<"173">>,<<"174">>,<<"175">>,<<"176">>,<<"178">>,<<"179">>,<<"180">>,<<"181">>,<<"182">>,<<"183">>,<<"184">>,<<"185">>,<<"186">>,<<"187">>,<<"188">>,<<"190">>,<<"191">>,<<"192">>,<<"193">>,<<"194">>,<<"195">>,<<"196">>,<<"197">>,<<"198">>,<<"199">>,<<"200">>,<<"201">>,<<"202">>,<<"203">>,<<"204">>,<<"205">>,<<"206">>,<<"208">>,<<"209">>,<<"210">>,<<"211">>,<<"212">>,<<"213">>,<<"214">>,<<"215">>,<<"217">>,<<"220">>,<<"221">>,<<"222">>,<<"223">>,<<"224">>,<<"225">>,<<"226">>,<<"228">>,<<"229">>,<<"230">>,<<"231">>,<<"232">>,<<"233">>,<<"234">>,<<"235">>,<<"236">>,<<"237">>,<<"238">>,<<"239">>,<<"240">>,<<"241">>,<<"242">>,<<"243">>,<<"244">>,<<"245">>,<<"246">>,<<"248">>,<<"249">>,<<"250">>,<<"252">>,<<"253">>,<<"254">>,<<"255">>,<<"256">>,<<"257">>,<<"258">>,<<"259">>,<<"260">>,<<"261">>,<<"262">>,<<"264">>,<<"266">>,<<"267">>,<<"268">>,<<"269">>,<<"270">>,<<"273">>,<<"274">>,<<"275">>,<<"276">>,<<"278">>,<<"279">>,<<"280">>,<<"281">>,<<"282">>,<<"283">>,<<"284">>,<<"285">>,<<"287">>,<<"288">>,<<"289">>,<<"290">>,<<"291">>,<<"293">>,<<"294">>,<<"295">>,<<"296">>,<<"297">>,<<"298">>,<<"300">>,<<"301">>,<<"302">>,<<"303">>,<<"304">>,<<"305">>,<<"306">>,<<"308">>,<<"309">>,<<"310">>,<<"311">>,<<"312">>,<<"313">>,<<"314">>,<<"315">>,<<"316">>,<<"317">>,<<"318">>,<<"320">>,<<"321">>,<<"322">>,<<"323">>,<<"324">>,<<"325">>,<<"326">>,<<"328">>,<<"329">>,<<"330">>,<<"331">>,<<"332">>,<<"334">>,<<"335">>,<<"336">>,<<"337">>,<<"338">>,<<"339">>,<<"340">>,<<"341">>,<<"342">>,<<"343">>,<<"344">>,<<"345">>,<<"346">>,<<"347">>,<<"348">>,<<"349">>,<<"350">>,<<"351">>,<<"352">>,<<"353">>,<<"355">>,<<"356">>,<<"357">>,<<"358">>,<<"359">>,<<"360">>,<<"361">>,<<"362">>,<<"363">>,<<"365">>,<<"366">>,<<"367">>,<<"368">>,<<"369">>,<<"370">>,<<"371">>,<<"372">>,<<"373">>,<<"374">>]).
-define(ALL_CABS_AREA,
        {11.7531545505395,16.2340827,39.3623147,42.302192553508}).

%% 19 is strange (removed)
-define(ALL_CABS_ZSCORED, [<<"100">>,<<"101">>,<<"102">>,<<"103">>,<<"104">>,<<"105">>,<<"106">>,<<"107">>,<<"109">>,<<"110">>,<<"111">>,<<"112">>,<<"113">>,<<"115">>,<<"117">>,<<"118">>,<<"119">>,<<"120">>,<<"121">>,<<"122">>,<<"123">>,<<"124">>,<<"125">>,<<"126">>,<<"127">>,<<"128">>,<<"129">>,<<"130">>,<<"131">>,<<"132">>,<<"134">>,<<"135">>,<<"137">>,<<"138">>,<<"139">>,<<"140">>,<<"141">>,<<"143">>,<<"144">>,<<"145">>,<<"147">>,<<"148">>,<<"149">>,<<"150">>,<<"151">>,<<"152">>,<<"153">>,<<"154">>,<<"155">>,<<"156">>,<<"157">>,<<"158">>,<<"159">>,<<"160">>,<<"162">>,<<"163">>,<<"166">>,<<"167">>,<<"169">>,<<"170">>,<<"171">>,<<"172">>,<<"173">>,<<"174">>,<<"175">>,<<"176">>,<<"178">>,<<"179">>,<<"180">>,<<"181">>,<<"182">>,<<"183">>,<<"184">>,<<"185">>,<<"186">>,<<"187">>,<<"188">>,<<"190">>,<<"191">>,<<"192">>,<<"193">>,<<"194">>,<<"195">>,<<"196">>,<<"197">>,<<"198">>,<<"199">>,<<"200">>,<<"201">>,<<"202">>,<<"203">>,<<"204">>,<<"205">>,<<"206">>,<<"208">>,<<"209">>,<<"210">>,<<"211">>,<<"212">>,<<"213">>,<<"214">>,<<"215">>,<<"217">>,<<"220">>,<<"221">>,<<"222">>,<<"223">>,<<"224">>,<<"225">>,<<"226">>,<<"228">>,<<"229">>,<<"230">>,<<"231">>,<<"232">>,<<"233">>,<<"234">>,<<"235">>,<<"236">>,<<"237">>,<<"238">>,<<"239">>,<<"240">>,<<"241">>,<<"242">>,<<"243">>,<<"244">>,<<"245">>,<<"246">>,<<"248">>,<<"249">>,<<"250">>,<<"252">>,<<"253">>,<<"254">>,<<"255">>,<<"256">>,<<"257">>,<<"258">>,<<"259">>,<<"260">>,<<"261">>,<<"262">>,<<"264">>,<<"266">>,<<"267">>,<<"268">>,<<"269">>,<<"270">>,<<"273">>,<<"274">>,<<"275">>,<<"276">>,<<"278">>,<<"279">>,<<"280">>,<<"281">>,<<"282">>,<<"283">>,<<"284">>,<<"285">>,<<"287">>,<<"288">>,<<"289">>,<<"290">>,<<"291">>,<<"293">>,<<"294">>,<<"295">>,<<"296">>,<<"297">>,<<"298">>,<<"300">>,<<"301">>,<<"302">>,<<"303">>,<<"304">>,<<"305">>,<<"306">>,<<"308">>,<<"309">>,<<"310">>,<<"311">>,<<"71">>,<<"72">>,<<"73">>,<<"74">>,<<"75">>,<<"76">>,<<"78">>,<<"79">>,<<"80">>,<<"81">>,<<"82">>,<<"83">>,<<"84">>,<<"85">>,<<"87">>,<<"88">>,<<"89">>,<<"90">>,<<"91">>,<<"92">>,<<"93">>,<<"94">>,<<"95">>,<<"96">>,<<"97">>,<<"99">>]).

-define(ALL_CABS_ZSCORED_AREA,
        {11.7531545505395,12.9634722613942,41.4459657,42.302192553508}).

-define(CABS, ?ALL_CABS_ZSCORED).
-define(AREA, ?ALL_CABS_ZSCORED_AREA).

%% Exported: get_area

get_area() ->
    %% Generated with generate_area/0
    ?AREA.

%% Exported: generate_area

generate_area() ->
    generate_area(?CABS).

generate_area(Names) ->
    simulator:generate_area(
      get_location_index(Names), fun parse_line/1, false).

parse_line(Line) ->
    [_, Time, Point] = string:lexemes(Line, ";"),
    [_, Latitude, Longitude, _] = re:split(Point, "POINT\\(| |\\)"),
    LongitudeNumber = simulator:binary_to_number(Longitude),
    LatitudeNumber = simulator:binary_to_number(Latitude),
    Timestamp = to_seconds(?b2l(string:chomp(Time))),
    {LongitudeNumber, LatitudeNumber, Timestamp}.

to_seconds(Time) ->
    %% 2014-03-02 23:59:58.943143+01
    [Date, Hms] = string:lexemes(Time, " "),
    [_Year, Month, Day] = string:lexemes(Date, "-"),
    [Hours, Minutes, Seconds] = string:lexemes(Hms, ":"),
    StrippedSeconds = string:slice(Seconds, 0, length(Seconds) - 3),
    Timestamp = (?l2i(Day) - 1) * 24 * 3600 +
        ?l2i(Hours) * 3600 +
        ?l2i(Minutes) * 60 +
        to_float(StrippedSeconds),
    case ?l2i(Month) of
        2 ->
            Timestamp;
        3 ->
            28 * 24 * 3600 + Timestamp
    end.

to_float(Seconds) ->
    try
        ?l2f(Seconds)
    catch
        _:_ ->
            ?l2i(Seconds) * 1.0
    end.

%% Exported: get_location_generator

get_location_generator(Path) ->
    simulator:get_location_generator(Path, fun(Line) ->
                                                   {parse_line(Line), 1}
                                           end).

%% Exported: get_location_index

get_location_index() ->
    get_location_index(?CABS).

get_location_index(Names) ->
    DataDir = code:priv_dir(simulator),
    simulator:get_location_index(
      Names,
      fun(Name) ->
              filename:join(
                [DataDir, <<"roma_taxi">>, ?l2b([Name, <<".txt">>])])
      end).

%% Exported: neighbour_distance_in_meters

neighbour_distance_in_meters() ->
    500.

%% Exported: degrees_to_meters

degrees_to_meters(Degrees) ->
    simulator:degrees_to_meters(Degrees, get_area()).

%% Exported: meters_to_degrees

meters_to_degrees(Meters) ->
    simulator:meters_to_degrees(Meters, get_area()).

%% Exported: width_height_in_meters

width_height_in_meters() ->
    {MinLongitude, MaxLongitude, MinLatitude, MaxLatitude} = get_area(),
    simulator:width_height_in_meters(MinLongitude, MaxLongitude,
                                     MinLatitude, MaxLatitude).

%% Exported: zscoring

zscoring() ->
    Index = get_location_index(?ALL_CABS),
    simulator:zscoring(Index, fun parse_line/1, 1).
