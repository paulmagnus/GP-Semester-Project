# compilation flags
JFLAGS = -g -Xlint -d .

default: SpaceInvaders.class #/GameState.class

# SpaceInvaders.jar: SpaceInvaders.class
# 	jar cvfe SpaceInvaders.jar com.zetcode.SpaceInvaders ./*.class

SpaceInvaders.class: SpaceInvaders.java Board.class Commons.class
	javac $(JFLAGS) SpaceInvaders.java

Board.class: Board.java Alien.class Commons.class Player.class
	javac $(JFLAGS) Board.java
#javac $(JFLAGS) -cp '.:/home/pmagnus/307/GP-Semester-Project/src/push307/clojure.jar' Board.java

Player.class: Player.java Sprite.class Commons.class Shot.class
	javac $(JFLAGS) Player.java

Commons.class: Commons.java
	javac $(JFLAGS) Commons.java

Shot.class: Shot.java Sprite.class
	javac $(JFLAGS) Shot.java

Alien.class: Alien.java Sprite.class
	javac $(JFLAGS) Alien.java

Sprite.class: Sprite.java
	javac $(JFLAGS) Sprite.java

clean:
	rm *.class