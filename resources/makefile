# compilation flags
JFLAGS = -g -d

SpaceInvaders.jar: com/zetcode/SpaceInvaders.class
	jar cvfe SpaceInvaders.jar com.zetcode.SpaceInvaders ./com/zetcode/*.class

com/zetcode/SpaceInvaders.class: SpaceInvaders.java com/zetcode/Board.class com/zetcode/Commons.class
	javac $(JFLAGS) . SpaceInvaders.java

com/zetcode/Board.class: Board.java com/zetcode/Alien.class com/zetcode/Commons.class com/zetcode/Player.class
	javac $(JFLAGS) . Board.java

com/zetcode/Player.class: Player.java com/zetcode/Sprite.class com/zetcode/Commons.class com/zetcode/Shot.class
	javac $(JFLAGS) . Player.java

com/zetcode/Commons.class: Commons.java
	javac $(JFLAGS) . Commons.java

com/zetcode/Shot.class: Shot.java com/zetcode/Sprite.class
	javac $(JFLAGS) . Shot.java

com/zetcode/Alien.class: Alien.java com/zetcode/Sprite.class
	javac $(JFLAGS) . Alien.java

com/zetcode/Sprite.class: Sprite.java
	javac $(JFLAGS) . Sprite.java

clean:
	rm -r ./com