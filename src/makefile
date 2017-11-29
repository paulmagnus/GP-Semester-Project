# compilation flags
JFLAGS = -g -d .

default: push307/SpaceInvaders.class #/GameState.class

# SpaceInvaders.jar: SpaceInvaders.class
# 	jar cvfe SpaceInvaders.jar com.zetcode.SpaceInvaders ./*.class

push307/SpaceInvaders.class: SpaceInvaders.java push307/Board.class push307/Commons.class
	javac $(JFLAGS) SpaceInvaders.java

push307/Board.class: Board.java push307/Alien.class push307/Commons.class push307/Player.class
	javac $(JFLAGS) Board.java
#javac $(JFLAGS) -cp '.:/home/pmagnus/307/GP-Semester-Project/src/push307/clojure.jar' Board.java

push307/Player.class: Player.java push307/Sprite.class push307/Commons.class push307/Shot.class
	javac $(JFLAGS) Player.java

push307/Commons.class: Commons.java
	javac $(JFLAGS) Commons.java

push307/Shot.class: Shot.java push307/Sprite.class
	javac $(JFLAGS) Shot.java

push307/Alien.class: Alien.java push307/Sprite.class
	javac $(JFLAGS) Alien.java

push307/Sprite.class: Sprite.java
	javac $(JFLAGS) Sprite.java

clean:
	rm *.class