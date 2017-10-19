# compilation flags
JFLAGS = -g -d

Sprite.class: Sprite.java
	javac $JFLAGS . Sprite.java

Alien.class: Alien.java Sprite.class
	javac $JFLAGS . Alien.java

Shot.class: Shot.java Sprite.class
	javac $JFLAGS . Shot.java

Commons.class: Commons.java
	javac $JFLAGS . Commons.java

Player.class: Player.java Sprite.class Commons.class Shot.class
	javac $JFLAGS . Player.java

Board.class: Board.java Alien.class Commons.class Player.class
	javac $JFLAGS . Board.java

SpaceInvaders.class: SpaceInvaders.java Board.class Commons.class
	javac $JFLAGS .SpaceInvaders.java

clean:
	rm *.class