-module(epfl_mobility).
-export([get_area/0, generate_area/0]).
-export([get_location_generator/1, get_location_index/0,
         read_location_index/0]).
-export([degrees_to_meters/1, meters_to_degrees/1]).
-export([width_height_in_meters/0]).
-export([zscoring/0]).

-include_lib("apptools/include/shorthand.hrl").

-define(A_FEW_CABS, [<<"oipial">>,<<"opnown">>,<<"appsri">>,<<"egteir">>,<<"oncalku">>,<<"ogijtri">>,<<"odoywug">>,<<"ansyut">>,<<"ipdioisi">>,<<"avglybic">>,<<"utkibedy">>,<<"ithpett">>,<<"iaggrijo">>,<<"acitva">>,<<"icdultha">>,<<"eagbaci">>,<<"ichankru">>,<<"uthsyis">>,<<"ictlypt">>,<<"evijcey">>,<<"iacbyb">>,<<"iafbagci">>,<<"urfhod">>,<<"evfler">>,<<"usbekhia">>,<<"obacvau">>,<<"eemafbij">>,<<"itmeps">>]).

-define(A_FEW_CABS_ZSCORED, [<<"acitva">>,<<"eagbaci">>,<<"evfler">>,<<"evijcey">>,<<"iacbyb">>,<<"iafbagci">>,<<"iaggrijo">>,<<"icdultha">>,<<"ichankru">>,<<"ictlypt">>,<<"obacvau">>,<<"urfhod">>,<<"usbekhia">>,<<"uthsyis">>]).

-define(ALL_CABS, [<<"enyenewl">>,<<"ockoac">>,<<"ikdagcy">>,<<"ayshekki">>,<<"ancorjo">>,<<"idvowwed">>,<<"iatmeuns">>,<<"unwrain">>,<<"atsfiv">>,<<"aupclik">>,<<"afmorc">>,<<"ochotcil">>,<<"enkkand">>,<<"amwibs">>,<<"ibflsruc">>,<<"esnddceb">>,<<"objoyhi">>,<<"amciuca">>,<<"okblahed">>,<<"arlwras">>,<<"afpansh">>,<<"oadwowd">>,<<"arcuim">>,<<"uggdye">>,<<"atfrim">>,<<"edomboys">>,<<"egnatab">>,<<"oxdoghic">>,<<"onwohyss">>,<<"edjiye">>,<<"osoccef">>,<<"ifelas">>,<<"imwrytsy">>,<<"owbsuscs">>,<<"ockihoba">>,<<"aichash">>,<<"epabcadu">>,<<"orkbats">>,<<"eitpem">>,<<"eccest">>,<<"iawxben">>,<<"ecjeody">>,<<"ofodwo">>,<<"iapberci">>,<<"oocsodpl">>,<<"itquirla">>,<<"adnerst">>,<<"eenjoug">>,<<"okquakar">>,<<"ippfeip">>,<<"eovsyt">>,<<"evmaja">>,<<"enidmopt">>,<<"isekdyp">>,<<"eavflid">>,<<"ockolral">>,<<"omwyek">>,<<"efgoaku">>,<<"ojwyky">>,<<"edlyxy">>,<<"ijprukfr">>,<<"imoikcha">>,<<"abniar">>,<<"effneomi">>,<<"uvjeahot">>,<<"ogjoch">>,<<"udveoyx">>,<<"enjubpl">>,<<"utjanre">>,<<"owufrey">>,<<"arphaud">>,<<"oquiat">>,<<"ujnawwe">>,<<"asigno">>,<<"ickphiwa">>,<<"odklobci">>,<<"acpegho">>,<<"onyacju">>,<<"awrenba">>,<<"oowfmu">>,<<"ogofvoit">>,<<"ubjajo">>,<<"abtyff">>,<<"efcati">>,<<"ioivkn">>,<<"oggluv">>,<<"uthiss">>,<<"oivnabyo">>,<<"eggfrij">>,<<"umhenwed">>,<<"udwiull">>,<<"ucovlato">>,<<"owgves">>,<<"idjokva">>,<<"osacmu">>,<<"eosluip">>,<<"odlorhem">>,<<"iblool">>,<<"ackgrica">>,<<"onyenre">>,<<"elswcky">>,<<"uklsdryo">>,<<"ujkiwe">>,<<"amvefarl">>,<<"efghakdi">>,<<"eotcue">>,<<"ubnankke">>,<<"edodblea">>,<<"ushfeoj">>,<<"ishvirv">>,<<"ijcoga">>,<<"indtwrat">>,<<"idlorra">>,<<"uchjafy">>,<<"oilrag">>,<<"onannsov">>,<<"otscokdr">>,<<"ibpijda">>,<<"ojbaso">>,<<"eicmynts">>,<<"ophugbe">>,<<"ukgoaf">>,<<"abyalwif">>,<<"ebdafya">>,<<"agyamker">>,<<"owhagme">>,<<"ilkble">>,<<"edrafs">>,<<"eesbaj">>,<<"askmecle">>,<<"iovlito">>,<<"igglitby">>,<<"ebstic">>,<<"ioangio">>,<<"epkiapme">>,<<"oyclsdas">>,<<"iagods">>,<<"aswift">>,<<"ijhigof">>,<<"edeejdru">>,<<"insodfl">>,<<"egreosko">>,<<"oljnek">>,<<"amknywr">>,<<"eydadgio">>,<<"itbejsco">>,<<"atnokvo">>,<<"obheujvo">>,<<"inquay">>,<<"onvahe">>,<<"eefayb">>,<<"akyeerbi">>,<<"ifkalda">>,<<"ejkipub">>,<<"ioytgor">>,<<"uvreoipy">>,<<"anidgib">>,<<"ailbcuv">>,<<"ivheoo">>,<<"oshyon">>,<<"acduou">>,<<"eiboidne">>,<<"ickguwa">>,<<"ioajdig">>,<<"occeyd">>,<<"akvutvo">>,<<"ajsips">>,<<"esudcejo">>,<<"utwoab">>,<<"iphespha">>,<<"imneesu">>,<<"udguic">>,<<"ofjeequo">>,<<"ikkimm">>,<<"oblyaga">>,<<"alsheth">>,<<"oydtesa">>,<<"inghoghe">>,<<"okavkau">>,<<"utvohovy">>,<<"obceoo">>,<<"eolgug">>,<<"aimidd">>,<<"ovkojy">>,<<"ootyecls">>,<<"auctjir">>,<<"oknscocy">>,<<"erjcrey">>,<<"evdyffu">>,<<"idodly">>,<<"aducrisi">>,<<"opjermp">>,<<"ojumna">>,<<"elfyea">>,<<"abdremlu">>,<<"ascley">>,<<"itacfl">>,<<"icwiroic">>,<<"aquawaw">>,<<"inshfola">>,<<"apsskyul">>,<<"ipjiert">>,<<"eoydba">>,<<"awnbeb">>,<<"erjtail">>,<<"oasthul">>,<<"erjboyni">>,<<"eoffrij">>,<<"aggjuo">>,<<"oksbosfl">>,<<"orptme">>,<<"iackaw">>,<<"usadrifo">>,<<"udwebir">>,<<"ugifmav">>,<<"ujtrud">>,<<"icdaheos">>,<<"orsyalf">>,<<"ispawwye">>,<<"ancedvab">>,<<"anupwr">>,<<"ononma">>,<<"ouvdank">>,<<"eytups">>,<<"eifvid">>,<<"aivtytan">>,<<"oktulsy">>,<<"abwecnij">>,<<"egwicjuv">>,<<"otasfier">>,<<"ugatna">>,<<"eapceou">>,<<"eckecky">>,<<"aviltly">>,<<"ijdeyd">>,<<"arcksy">>,<<"agdrea">>,<<"orrrar">>,<<"icodmup">>,<<"adpaifo">>,<<"aiwalb">>,<<"agvexpia">>,<<"otvasdar">>,<<"andyol">>,<<"uljior">>,<<"ufdoyno">>,<<"okneydy">>,<<"ovnacgad">>,<<"ubgihoot">>,<<"isvayd">>,<<"uvigcho">>,<<"ilyutje">>,<<"eavcaifo">>,<<"apigcyd">>,<<"ojfieza">>,<<"afsfat">>,<<"eadectio">>,<<"eoivqued">>,<<"uthomoov">>,<<"efspheco">>,<<"ayfsdu">>,<<"ipdraw">>,<<"equoaw">>,<<"eddreba">>,<<"ichikiga">>,<<"abmuyawm">>,<<"aniajcr">>,<<"iphybcay">>,<<"ejcrur">>,<<"inghoth">>,<<"aucjun">>,<<"osckro">>,<<"aurjyen">>,<<"oafhynu">>,<<"ektamy">>,<<"ojfequia">>,<<"otswrank">>,<<"abjoolaw">>,<<"oygvar">>,<<"ajtreo">>,<<"iorjtwav">>,<<"omdrid">>,<<"iodlac">>,<<"ogshtcew">>,<<"udwadla">>,<<"omluaj">>,<<"ebcicy">>,<<"onkawcel">>,<<"ocwyit">>,<<"unquekov">>,<<"ancyclsu">>,<<"arcurbig">>,<<"oncixpi">>,<<"ogikligu">>,<<"irjfen">>,<<"ayquexar">>,<<"ootquoa">>,<<"iatojbo">>,<<"imwyojy">>,<<"agjitfa">>,<<"ognowwoz">>,<<"ejshigib">>,<<"adkavy">>,<<"arhilby">>,<<"igsoysla">>,<<"ucbiyaym">>,<<"ecfesy">>,<<"ogurjupo">>,<<"inhiala">>,<<"eybmij">>,<<"udtiogi">>,<<"occroksy">>,<<"ofikco">>,<<"eshroa">>,<<"idholv">>,<<"elbnaxa">>,<<"ojfanni">>,<<"ecgojtyt">>,<<"aysmsla">>,<<"utthwaju">>,<<"iodjiesh">>,<<"odolfev">>,<<"etnads">>,<<"eufdod">>,<<"umsmtha">>,<<"icdapnu">>,<<"equioc">>,<<"efonve">>,<<"ifefrof">>,<<"easnsvu">>,<<"evjoko">>,<<"ashgati">>,<<"ayshowg">>,<<"uvburki">>,<<"edglevi">>,<<"idsmuk">>,<<"acgerl">>,<<"erulghiv">>,<<"ojpoota">>,<<"ocjeng">>,<<"atidfi">>,<<"eghanwib">>,<<"ecdiwovu">>,<<"eokracy">>,<<"anucks">>,<<"ebenfrag">>,<<"oirzgont">>,<<"upsjavci">>,<<"atcilav">>,<<"ewvnil">>,<<"igvidth">>,<<"ojroigna">>,<<"obsimjio">>,<<"ophgir">>,<<"akgicjud">>,<<"ecnoplsi">>,<<"edgrat">>,<<"ubjaju">>,<<"imhacy">>,<<"ifanfadd">>,<<"inckkiv">>,<<"aitpygg">>,<<"ijhipvo">>,<<"isnthli">>,<<"oapwycti">>,<<"ejnoax">>,<<"edmugrip">>,<<"evbacclu">>,<<"ikujfurk">>,<<"upthin">>,<<"iovhar">>,<<"idbaybim">>,<<"ugdifbl">>,<<"etalrab">>,<<"amnurgji">>,<<"aslagni">>,<<"ibwicim">>,<<"erwumu">>,<<"ankped">>,<<"odscixam">>,<<"indyep">>,<<"ijthind">>,<<"inlica">>,<<"opmisa">>,<<"ekyosjo">>,<<"icedpa">>,<<"ifragcic">>,<<"ekfrab">>,<<"ojachme">>,<<"ainplin">>,<<"uvjova">>,<<"umvohug">>,<<"ayquasto">>,<<"eacderph">>,<<"eersdo">>,<<"itpivoa">>,<<"opgovebo">>,<<"eedigkr">>,<<"ellimtbu">>,<<"onghoji">>,<<"eryeig">>,<<"odocjeca">>,<<"ewbglo">>,<<"ucgewft">>,<<"eoccoirv">>,<<"ijtowhur">>,<<"ejtobu">>,<<"ewufri">>,<<"ochtin">>,<<"enarwee">>,<<"eegcef">>,<<"ednillo">>,<<"iahorou">>,<<"elaflevy">>,<<"exskafvo">>,<<"artgagvu">>,<<"agsuec">>,<<"eglohabs">>,<<"ajsnedsi">>,<<"isfikti">>,<<"icagpony">>,<<"enyonsn">>,<<"ocshitcl">>,<<"ideutgoa">>,<<"iofplik">>,<<"oygmebra">>,<<"upchimy">>,<<"idjoat">>,<<"ojvighta">>,<<"utlurva">>,<<"ivkockim">>,<<"eybyay">>,<<"epemvagu">>,<<"oogchyog">>,<<"agivle">>,<<"aydwaho">>,<<"abnovkak">>,<<"iafstnue">>,<<"ijpiawl">>,<<"alnihi">>,<<"ecforj">>,<<"abgibo">>,<<"ebrulra">>,<<"evtitdea">>,<<"acdiyito">>,<<"omcuky">>,<<"okchympy">>,<<"itcemra">>,<<"ilcoby">>,<<"ekenucki">>,<<"ucvepnuv">>,<<"egoiwroi">>,<<"oakpikoo">>,<<"ewedkl">>,<<"atfrav">>,<<"ibgryk">>,<<"ujtosh">>,<<"idjojwa">>,<<"ajthof">>,<<"uttorhig">>,<<"otmegok">>,<<"ogphagpi">>,<<"elvgid">>,<<"agcowktu">>,<<"eshria">>,<<"ilkedve">>,<<"ucdewy">>,<<"ujhuki">>,<<"ifeshce">>,<<"atzumbon">>,<<"enekbl">>,<<"elhuyoi">>,<<"ogawjem">>,<<"ovyijna">>,<<"avdyab">>,<<"otfahona">>,<<"okvats">>,<<"etirnfew">>,<<"ugthfu">>,<<"awribig">>,<<"aldhidd">>,<<"ickviya">>,<<"abcoij">>,<<"idtwal">>,<<"iotklojy">>,<<"acvebr">>,<<"oiphye">>,<<"oahudflo">>,<<"elwesnn">>,<<"afweka">>,<<"itbadpi">>,<<"eincufpy">>,<<"efmymo">>,<<"ictmuog">>,<<"ajixdo">>,<<"ogdygdyd">>,<<"abboip">>,<<"ektuyo">>,<<"izowcu">>,<<"aypudby">>,<<"orocdu">>,<<"ejesbay">>,<<"avpavi">>,<<"udthon">>,<<"ubzachy">>,<<"idshywr">>,<<"aygpha">>,<<"idcadgi">>,<<"oipial">>,<<"opnown">>,<<"appsri">>,<<"egteir">>,<<"oncalku">>,<<"ogijtri">>,<<"odoywug">>,<<"ansyut">>,<<"ipdioisi">>,<<"avglybic">>,<<"utkibedy">>,<<"ithpett">>,<<"iaggrijo">>,<<"acitva">>,<<"icdultha">>,<<"eagbaci">>,<<"ichankru">>,<<"uthsyis">>,<<"ictlypt">>,<<"evijcey">>,<<"iacbyb">>,<<"iafbagci">>,<<"urfhod">>,<<"evfler">>,<<"usbekhia">>,<<"obacvau">>,<<"eemafbij">>,<<"itmeps">>]).

-define(ALL_CABS_ZSCORED, [<<"abboip">>,<<"abcoij">>,<<"abgibo">>,<<"abjoolaw">>,<<"abmuyawm">>,<<"abnovkak">>,<<"abwecnij">>,<<"acdiyito">>,<<"acgerl">>,<<"acitva">>,<<"acvebr">>,<<"adkavy">>,<<"adpaifo">>,<<"afsfat">>,<<"afweka">>,<<"agcowktu">>,<<"agdrea">>,<<"aggjuo">>,<<"agivle">>,<<"agjitfa">>,<<"agsuec">>,<<"agvexpia">>,<<"ainplin">>,<<"aitpygg">>,<<"aivtytan">>,<<"aiwalb">>,<<"ajixdo">>,<<"ajsnedsi">>,<<"ajthof">>,<<"ajtreo">>,<<"akgicjud">>,<<"aldhidd">>,<<"alnihi">>,<<"amnurgji">>,<<"ancedvab">>,<<"ancyclsu">>,<<"andyol">>,<<"aniajcr">>,<<"ankped">>,<<"ansyut">>,<<"anucks">>,<<"anupwr">>,<<"apigcyd">>,<<"appsri">>,<<"apsskyul">>,<<"aquawaw">>,<<"arcksy">>,<<"arcurbig">>,<<"arhilby">>,<<"artgagvu">>,<<"ashgati">>,<<"aslagni">>,<<"atcilav">>,<<"atfrav">>,<<"atidfi">>,<<"atzumbon">>,<<"aucjun">>,<<"aurjyen">>,<<"avdyab">>,<<"avglybic">>,<<"aviltly">>,<<"avpavi">>,<<"awnbeb">>,<<"awribig">>,<<"aydwaho">>,<<"ayfsdu">>,<<"aygpha">>,<<"aypudby">>,<<"ayquasto">>,<<"ayquexar">>,<<"ayshowg">>,<<"aysmsla">>,<<"eacderph">>,<<"eadectio">>,<<"eagbaci">>,<<"eapceou">>,<<"easnsvu">>,<<"eavcaifo">>,<<"ebcicy">>,<<"ebenfrag">>,<<"ebrulra">>,<<"ecdiwovu">>,<<"ecfesy">>,<<"ecforj">>,<<"ecgojtyt">>,<<"eckecky">>,<<"ecnoplsi">>,<<"eddreba">>,<<"edglevi">>,<<"edgrat">>,<<"edmugrip">>,<<"ednillo">>,<<"eedigkr">>,<<"eegcef">>,<<"eemafbij">>,<<"eersdo">>,<<"efmymo">>,<<"efonve">>,<<"efspheco">>,<<"eghanwib">>,<<"eglohabs">>,<<"egoiwroi">>,<<"egteir">>,<<"egwicjuv">>,<<"eifvid">>,<<"eincufpy">>,<<"ejcrur">>,<<"ejesbay">>,<<"ejnoax">>,<<"ejshigib">>,<<"ejtobu">>,<<"ekenucki">>,<<"ekfrab">>,<<"ektamy">>,<<"ektuyo">>,<<"ekyosjo">>,<<"elaflevy">>,<<"elbnaxa">>,<<"elhuyoi">>,<<"ellimtbu">>,<<"elvgid">>,<<"elwesnn">>,<<"enarwee">>,<<"enekbl">>,<<"enyonsn">>,<<"eoccoirv">>,<<"eoffrij">>,<<"eoivqued">>,<<"eokracy">>,<<"eoydba">>,<<"epemvagu">>,<<"equioc">>,<<"equoaw">>,<<"erjboyni">>,<<"erjtail">>,<<"erulghiv">>,<<"erwumu">>,<<"eryeig">>,<<"eshria">>,<<"eshroa">>,<<"etalrab">>,<<"etirnfew">>,<<"etnads">>,<<"eufdod">>,<<"evbacclu">>,<<"evfler">>,<<"evijcey">>,<<"evjoko">>,<<"evtitdea">>,<<"ewbglo">>,<<"ewedkl">>,<<"ewufri">>,<<"ewvnil">>,<<"exskafvo">>,<<"eybmij">>,<<"eybyay">>,<<"eytups">>,<<"iacbyb">>,<<"iackaw">>,<<"iafbagci">>,<<"iafstnue">>,<<"iaggrijo">>,<<"iahorou">>,<<"iatojbo">>,<<"ibgryk">>,<<"ibwicim">>,<<"icagpony">>,<<"icdaheos">>,<<"icdapnu">>,<<"icdultha">>,<<"icedpa">>,<<"ichankru">>,<<"ichikiga">>,<<"ickviya">>,<<"icodmup">>,<<"ictlypt">>,<<"ictmuog">>,<<"icwiroic">>,<<"idbaybim">>,<<"idcadgi">>,<<"ideutgoa">>,<<"idholv">>,<<"idjoat">>,<<"idjojwa">>,<<"idshywr">>,<<"idsmuk">>,<<"idtwal">>,<<"ifanfadd">>,<<"ifefrof">>,<<"ifeshce">>,<<"ifragcic">>,<<"igsoysla">>,<<"igvidth">>,<<"ijdeyd">>,<<"ijhipvo">>,<<"ijpiawl">>,<<"ijthind">>,<<"ijtowhur">>,<<"ikujfurk">>,<<"ilcoby">>,<<"ilkedve">>,<<"ilyutje">>,<<"imhacy">>,<<"imwyojy">>,<<"inckkiv">>,<<"indyep">>,<<"inghoth">>,<<"inhiala">>,<<"inlica">>,<<"inshfola">>,<<"iodjiesh">>,<<"iodlac">>,<<"iofplik">>,<<"iorjtwav">>,<<"iotklojy">>,<<"iovhar">>,<<"ipdioisi">>,<<"ipdraw">>,<<"iphybcay">>,<<"ipjiert">>, <<"irjfen">>,<<"isfikti">>,<<"isnthli">>,<<"ispawwye">>,<<"isvayd">>,<<"itacfl">>,<<"itbadpi">>,<<"itcemra">>,<<"ithpett">>,<<"itmeps">>,<<"itpivoa">>,<<"ivkockim">>,<<"izowcu">>,<<"oafhynu">>,<<"oahudflo">>,<<"oakpikoo">>,<<"oapwycti">>,<<"oasthul">>,<<"obacvau">>,<<"obsimjio">>,<<"occroksy">>,<<"ochtin">>,<<"ocjeng">>,<<"ocshitcl">>,<<"ocwyit">>,<<"odocjeca">>,<<"odolfev">>,<<"odoywug">>, <<"odscixam">>,<<"ofikco">>,<<"ogawjem">>,<<"ogdygdyd">>,<<"ogijtri">>,<<"ogikligu">>,<<"ognowwoz">>,<<"ogphagpi">>,<<"ogshtcew">>,<<"ogurjupo">>,<<"oiphye">>,<<"oipial">>,<<"oirzgont">>,<<"ojachme">>,<<"ojfanni">>,<<"ojfequia">>,<<"ojfieza">>,<<"ojpoota">>,<<"ojroigna">>,<<"ojvighta">>,<<"okchympy">>,<<"okneydy">>,<<"oksbosfl">>,<<"oktulsy">>,<<"okvats">>,<<"omcuky">>,<<"omdrid">>,<<"omluaj">>,<<"oncalku">>,<<"oncixpi">>,<<"onghoji">>,<<"onkawcel">>,<<"ononma">>,<<"oogchyog">>,<<"ootquoa">>,<<"opgovebo">>,<<"ophgir">>,<<"opmisa">>,<<"opnown">>,<<"orocdu">>,<<"orptme">>,<<"orrrar">>,<<"orsyalf">>,<<"osckro">>,<<"otasfier">>,<<"otfahona">>,<<"otmegok">>,<<"otswrank">>,<<"otvasdar">>,<<"ouvdank">>,<<"ovnacgad">>,<<"ovyijna">>,<<"oygmebra">>,<<"oygvar">>,<<"ubgihoot">>,<<"ubjaju">>,<<"ubzachy">>,<<"ucbiyaym">>,<<"ucdewy">>,<<"ucgewft">>,<<"ucvepnuv">>,<<"udthon">>,<<"udtiogi">>,<<"udwadla">>,<<"udwebir">>,<<"ufdoyno">>,<<"ugatna">>,<<"ugdifbl">>,<<"ugifmav">>,<<"ugthfu">>,<<"ujhuki">>,<<"ujtosh">>,<<"ujtrud">>,<<"uljior">>,<<"umsmtha">>,<<"umvohug">>,<<"unquekov">>,<<"upchimy">>,<<"upsjavci">>,<<"upthin">>,<<"urfhod">>,<<"usadrifo">>,<<"usbekhia">>,<<"uthomoov">>, <<"uthsyis">>,<<"utkibedy">>,<<"utlurva">>,<<"utthwaju">>,<<"uttorhig">>,<<"uvburki">>,<<"uvigcho">>,<<"uvjova">>]).
-define(ALL_CABS_ZSCORED_AREA, {-127.08143,-122,32.8697,50.30546}).

-define(CABS, ?ALL_CABS_ZSCORED).
%%-define(AREA, ?ALL_CABS_ZSCORED_AREA).

%% Exported: get_area

get_area() ->
    %% Generated with generate_area/0
    {-122.55327,-122.00045,37.32341,37.98978}.

%% Exported: generate_area

generate_area() ->
    generate_area(?CABS).

generate_area(Names) ->
    simulator:generate_area(
      get_location_index(Names), fun parse_line/1, false).

parse_line(Line) ->
    [Latitude, Longitude, _, Time] = string:lexemes(Line, " "),
    LongitudeNumber = simulator:binary_to_number(Longitude),
    LatitudeNumber = simulator:binary_to_number(Latitude),
    Timestamp = ?b2i(string:chomp(Time)),
    {LongitudeNumber, LatitudeNumber, Timestamp}.

%% Exported: get_location_generator

get_location_generator(Path) ->
    simulator:get_location_generator(Path,
                                     fun(Line) ->
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
              filename:join([DataDir,
                             <<"epfl_mobility">>,
                             ?l2b([<<"new_">>, Name, <<".txt">>])])
      end).

%% Exported: read_location_index

read_location_index() ->
    DataDir = code:priv_dir(simulator),
    Path = filename:join([DataDir, <<"epfl_mobility">>, <<"cabs.dat">>]),
    {ok, File} = file:open(Path, [read, raw, read_ahead, binary]),
    Index = read_location_index(DataDir, File),
    file:close(File),
    Index.

read_location_index(DataDir, File) ->
    case file:read_line(File) of
        eof ->
            ok = file:close(File),
            [];
        {ok, Data} ->
            [Name, _] = string:lexemes(Data, " "),
            [{Name, filename:join(
                      [DataDir,
                       <<"epfl_mobility">>,
                       ?l2b([<<"new_">>, Name, <<".txt">>])])}|
             read_location_index(DataDir, File)]
    end.

%% Exported: degrees_to_meters

degrees_to_meters(Degrees) ->
    simulator:degrees_to_meters(Degrees, get_area()).

%% Exported: meters_to_degrees

meters_to_degrees(Meters) ->
    simulator:meters_to_degrees(Meters, get_area()).

%% Exported: width_height_in_meters

width_height_in_meters() ->
    {MinLongitude, MaxLongitude, MinLatitude, MaxLatitude} = get_area(),
    simulator:width_height_in_meters(
      MinLongitude, MaxLongitude, MinLatitude, MaxLatitude).

%% Exported: zscoring

zscoring() ->
    Index = get_location_index(?ALL_CABS),
    simulator:zscoring(Index, fun parse_line/1, 1).
