# A Simulator

> Installation instructions and information on how to start the
> simulator can be found in the [Mixmesh
> repository](https://github.com/mixmesh/mixmesh/blob/master/README.md).

The default simulation shows an 1x1 km square area in Silicon Valley
(anchor point: 37.41504612935415, -121.96489836466651). This square
house 100 players which move about with an average velocity of 25
km/h. As soon as players come closer than 50 meters to each other they
exchange re-encrypted messages.

![A 1x1 km square area in Silicon Valley](https://github.com/mixmesh/mixmesh/blob/master/doc/simulation.gif?raw=true)

The players are labeled p1-p100:XYZ (where XYZ is the size of each
player's message buffer). In the animation above p42 sends an email to
p8, i.e. the source player p42 is immediately recolored in red and
the target player p8 in blue. p42 sends mails via its SMTP server
running on 127.0.0.1:16058 and p8 eventually reads the incoming mail
via its POP3 server running on 127.0.0.1:32092.

The swaks and mpop utilities (found elsewhere) come in handy when
working with sending/receiving of mails from a terminal.

p42 adds 10 encrypted copies (as introduced by Spiridon 2017) of the new
mail mail to its own message buffer and as soon as other intermediary
players have one of these messages in their buffers they are recolored
in yellow.

In the above simulation each player moves in circles with a randomized
radius and alternating velocities. This is kind of silly (no pun
intended) but has been valuable during development. Other more
realistic data sets are available as well:

<dl>
  <dt>./priv/epfl_mobility/</dt>
  <dd>This data set contains mobility traces of taxi cabs in San Francisco, USA. It contains GPS coordinates of approximately 500 taxis collected over 30 days in the San Francisco Bay Area. [https://crawdad.org/epfl/mobility/20090224/]</dd>
  <dt>./priv/it_vr2marketbaiaotrial/</dt>
  <dd>Data set contains simultaneous GPS traces collected at 1 Hz from a team of firefighters during a forest fire exercise. The traces were generated by Android phones placed in each of four firefighters and a generic GPS device placed in the firetruck. [https://crawdad.org/it/vr2marketbaiaotrial/20190916/]</dd>
  <dt>./priv/roma_taxi/</dt>
  <dd>This data set contains mobility traces of taxi cabs in Rome, Italy. It contains GPS coordinates of approximately 320 taxis collected over 30 days [https://crawdad.org/roma/taxi/20140717/]</dd>
</dl>

To use any of these data sets the ./src/simulator_serv.erl (and more) have to be recompiled using appropriate settings. Not ideal.

## Files

<dl>
  <dt>./src/simulator_app.erl</dt>
  <dd>The simulator application module</dd>
  <dt>./src/simulator_sup.erl</dt>
  <dd>The top-level supervisor</dd>
  <dt>./src/simulator_players_sup.erl</dt>
  <dd>A player supervisor which supervises a dynamic number of
  ./player/src/player_sup.erl supervisors (added dynamically by
  simulator_serv.erl)</dd>
  <dt>./src/neighbour_serv.erl</dt>
  <dd>A neighbour server which keeps track of how close players are to
  each other. It uses the rstar repository to do this.</dd>
  <dt>./src/simulator_serv.erl</dt>
  <dd>See above</dd>
  <dt>./src/render_serv.erl</dt>
  <dd>A render server which collects data and send it down to a NIF that
  do the actual demo rendering on screen, i.e. using Simple2D</dd>
  <dt>./src/simulator_keydir_serv.erl</dt>
  <dd>A simulator server which works as an extra keydir server keeping
  track of all player instances</dd>
  <dt>./src/simulator.erl</dt>
  <dd>A helper module which contains a marshalling API on top of
  functionality in simulator_nif.erl, but also house a number of GPS
  specific utility functions</dd>
</dl>

player_db.erl and stats_db.erl wraps public ETS tables which the
simulator relies upon to gain a global view of each player's state.

The dummy_circle.erl, epfl_mobility.erl, it_vr2marketbaiaotrial.erl,
roma_taxi.erl, square.erl are the modules referred to above when it
comes to available data sets.
