# simulator

  <dt>./src/player_db.erl</dt>
  <dd>SIMULATION: An API towards a public table which players use to store information which in turn is used by the simulator application</dd>
  <dt>./src/mail_serv.erl</dt>
  <dd>DEBUG: A server which can be used to send test emails</dd>
# simulator

# Install SDL & simple2D

You need the following packages installed to successful build simulator,
on ubuntu you install the following:

	sudo apt install libsdl2-dev
	sudo apt install libsdl2-image-dev
	sudo apt install libsdl2-mixer-dev
	sudo apt install libsdl2-ttf-dev
	
Then clone simple2d from github

	git clone https://github.com/simple2d/simple2d
	cd simple2d
	make
	sudo make install
	
	 
