package push307;

// import java.awt.Color;
// import java.awt.Dimension;
// import java.awt.Font;
// import java.awt.FontMetrics;
// import java.awt.Graphics;
// import java.awt.Toolkit;
// import java.awt.event.KeyAdapter;
// import java.awt.event.KeyEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;

// import javax.swing.ImageIcon;
// import javax.swing.JPanel;

import java.lang.Long;

import java.io.IOException;

import java.lang.Math;

// clojure integration
import clojure.java.api.Clojure;
import clojure.lang.RT;
import clojure.lang.Var;
import clojure.lang.PersistentList;
import clojure.lang.IFn;

// import javax.swing.WindowEvent;

public class Board implements Runnable, Commons {

    private static final long serialVersionUID = 1L;

    // private Dimension d;
    private ArrayList<Alien> aliens;
    private Player player;
    private Shot shot;

    private final int ALIEN_INIT_X = 150;
    private final int ALIEN_INIT_Y = 5;
    private int direction = -1;
    private int deaths = 0;

    private boolean ingame = true;
    private final String explImg = "./resources/images/explosion.png";
    private String message = "Game Over";

    // private Thread animator;

    private IFn pushInterpreter;
    private PersistentList pushProgram;

    private Random generator;

    public class GameState {
        private Board board;

        public GameState(Board board) {
            this.board = board;
        }

        public int[] getPlayerPosition() {
            Player player = board.getPlayer();

            return new int[] {player.getX(), player.getY()};
        }

        public boolean playerShotExists() {
            Shot shot = board.getShot();

            return shot.isVisible();
        }

        public int[] getShotPosition() {
            Shot shot = board.getShot();

            return new int[] {shot.getX(), shot.getY()};
        }

        private double distance(int x1, int y1, int x2, int y2) {
            return Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));
        }
    
        public int[] getAlienPosition() {
            ArrayList<Alien> aliens = board.getAliens();
            // int[][] positions = new int[aliens.size()][2];

            double min_distance = 100000;
            int[] closest_pos = new int[2];
            for(int i = 0; i < aliens.size(); i++) {
                // positions[i] = new int[] {aliens.get(i).getX(),
                //                           aliens.get(i).getY()};

                double dist = distance(player.getX(),
                                       player.getY(),
                                       aliens.get(i).getX(),
                                       aliens.get(i).getY());

                if(dist < min_distance) {
                    closest_pos = new int[] {aliens.get(i).getX(),
                                             aliens.get(i).getY()};
                    min_distance = dist;
                }
            }

            // return positions;
            return closest_pos;
        }

        public long distanceToNearestAlien() {
            ArrayList<Alien> aliens = board.getAliens();

            double min_distance = 1000000;
            for (int i = 0; i < aliens.size(); i++) {
                double dist = distance(player.getX(),
                                       player.getY(),
                                       aliens.get(i).getX(),
                                       aliens.get(i).getY());

                if (dist < min_distance) {
                    min_distance = dist;
                }
            }

            return Math.round(min_distance);
        }

        public int[] getBombPosition() {
            ArrayList<Alien> aliens = board.getAliens();
            ArrayList<Alien.Bomb> bombs = new ArrayList<Alien.Bomb>();
        
            for(Alien a : aliens) {
                if(! a.getBomb().isDestroyed()) {
                    bombs.add(a.getBomb());
                }
            }

            // int[][] positions = new int[bombs.size()][2];

            double min_distance = 1000000;
            int[] closest_position = new int[2];
            
            for(int i = 0; i < bombs.size(); i++) {
                // positions[i] = new int[] {bombs.get(i).getX(),
                //                           bombs.get(i).getY()};

                double dist = distance(player.getX(),
                                       player.getY(),
                                       bombs.get(i).getX(),
                                       bombs.get(i).getY());

                if (dist < min_distance) {
                    min_distance = dist;
                    closest_position = new int[] {bombs.get(i).getX(),
                                                 bombs.get(i).getY()};
                }
            }

            // return positions;
            return closest_position;
        }

        public long distanceToNearestBomb() {
            ArrayList<Alien> aliens = board.getAliens();
            ArrayList<Alien.Bomb> bombs = new ArrayList<Alien.Bomb>();
        
            for(Alien a : aliens) {
                if(! a.getBomb().isDestroyed()) {
                    bombs.add(a.getBomb());
                }
            }

            // int[][] positions = new int[bombs.size()][2];

            double min_distance = 1000000;
            
            for(int i = 0; i < bombs.size(); i++) {
                // positions[i] = new int[] {bombs.get(i).getX(),
                //                           bombs.get(i).getY()};

                double dist = distance(player.getX(),
                                      player.getY(),
                                      bombs.get(i).getX(),
                                      bombs.get(i).getY());

                if (dist < min_distance) {
                    min_distance = dist;
                }
            }

            // return positions;
            return Math.round(min_distance);
        }
    }

    public Board(PersistentList program, long seed) {

        generator = new Random(seed);

        initBoard();

        pushProgram = program;

        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("push307.core"));
        pushInterpreter = Clojure.var("push307.core",
                                           "java-push-interpreter");

        run();
    }

    public int getScore() {
        return NUMBER_OF_ALIENS_TO_DESTROY - deaths;
    }

    private void initBoard() {

        // addKeyListener(new TAdapter());
        // setFocusable(true);
        // d = new Dimension(BOARD_WIDTH, BOARD_HEIGHT);
        // setBackground(Color.black);
        // setVisible(true);

        gameInit();
        // setDoubleBuffered(true);
    }

    // @Override
    public void addNotify() {

        // super.addNotify();
        gameInit();
    }

    public Player getPlayer() {
        return player;
    }

    public Shot getShot() {
        return shot;
    }

    public ArrayList<Alien> getAliens() {
        return aliens;
    }

    public void gameInit() {

        // System.out.println("gameInit");

        aliens = new ArrayList<>();

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 6; j++) {

                Alien alien = new Alien(ALIEN_INIT_X + 18 * j, ALIEN_INIT_Y + 18 * i);
                aliens.add(alien);
            }
        }

        player = new Player();
        shot = new Shot();

        // if (animator == null || !ingame) {

        //     animator = new Thread(this);
        //     animator.start();
        // }
    }

    // public void drawAliens(Graphics g) {
    //     // System.out.println("drawAliens");

    //     Iterator<Alien> it = aliens.iterator();

    //     for (Alien alien: aliens) {

    //         if (alien.isVisible()) {

    //             g.drawImage(alien.getImage(), alien.getX(), alien.getY(), this);
    //         }

    //         if (alien.isDying()) {

    //             alien.die();
    //         }
    //     }
    // }

    // public void drawPlayer(Graphics g) {

    //     if (player.isVisible()) {
            
    //         g.drawImage(player.getImage(), player.getX(), player.getY(), this);
    //     }

    //     if (player.isDying()) {

    //         player.die();
    //         ingame = false;
    //     }
    // }

    // public void drawShot(Graphics g) {

    //     if (shot.isVisible()) {
            
    //         g.drawImage(shot.getImage(), shot.getX(), shot.getY(), this);
    //     }
    // }

    // public void drawBombing(Graphics g) {

    //     for (Alien a : aliens) {
            
    //         Alien.Bomb b = a.getBomb();

    //         if (!b.isDestroyed()) {
                
    //             g.drawImage(b.getImage(), b.getX(), b.getY(), this);
    //         }
    //     }
    // }

    // @Override
    // public void paintComponent(Graphics g) {
    //     super.paintComponent(g);

    //     g.setColor(Color.black);
    //     g.fillRect(0, 0, d.width, d.height);
    //     g.setColor(Color.green);

    //     if (ingame) {

    //         g.drawLine(0, GROUND, BOARD_WIDTH, GROUND);
    //         drawAliens(g);
    //         drawPlayer(g);
    //         drawShot(g);
    //         drawBombing(g);
    //     }

    //     Toolkit.getDefaultToolkit().sync();
    //     g.dispose();
    // }

    // public void gameOver() {
    //     this.getParent().setEnabled(false);
    // }

    // public void gameOver() {

    //     Graphics g = this.getGraphics();

    //     g.setColor(Color.black);
    //     g.fillRect(0, 0, BOARD_WIDTH, BOARD_HEIGHT);

    //     g.setColor(new Color(0, 32, 48));
    //     g.fillRect(50, BOARD_WIDTH / 2 - 30, BOARD_WIDTH - 100, 50);
    //     g.setColor(Color.white);
    //     g.drawRect(50, BOARD_WIDTH / 2 - 30, BOARD_WIDTH - 100, 50);

    //     Font small = new Font("Helvetica", Font.BOLD, 14);
    //     FontMetrics metr = this.getFontMetrics(small);

    //     g.setColor(Color.white);
    //     g.setFont(small);
    //     g.drawString(message, (BOARD_WIDTH - metr.stringWidth(message)) / 2,
    //                  BOARD_WIDTH / 2);
    // }

    public void animationCycle() {

        if (deaths == NUMBER_OF_ALIENS_TO_DESTROY) {

            ingame = false;
            message = "Game won!";
        }
        else {

        // player
        player.act();

        // shot
        if (shot.isVisible()) {

            int shotX = shot.getX();
            int shotY = shot.getY();

            for (Alien alien: aliens) {

                int alienX = alien.getX();
                int alienY = alien.getY();

                if (alien.isVisible() && shot.isVisible()) {
                    if (shotX >= (alienX)
                        && shotX <= (alienX + ALIEN_WIDTH)
                        && shotY >= (alienY)
                        && shotY <= (alienY + ALIEN_HEIGHT)) {
                        // ImageIcon ii
                        //     = new ImageIcon(explImg);
                        // alien.setImage(ii.getImage());
                        alien.setDying(true);

                        deaths++;
                        shot.die();
                    }
                }
            }

            int y = shot.getY();
            y -= 4;

            if (y < 0) {
                shot.die();
            } else {
                shot.setY(y);
            }
        }

        // aliens

        for (Alien alien: aliens) {

            int x = alien.getX();

            if (x >= BOARD_WIDTH - BORDER_RIGHT && direction != -1) {

                direction = -1;
                Iterator<Alien> i1 = aliens.iterator();

                while (i1.hasNext()) {

                    Alien a2 = i1.next();
                    a2.setY(a2.getY() + GO_DOWN);
                }
            }

            if (x <= BORDER_LEFT && direction != 1) {

                direction = 1;

                Iterator<Alien> i2 = aliens.iterator();

                while (i2.hasNext()) {

                    Alien a = i2.next();
                    a.setY(a.getY() + GO_DOWN);
                }
            }
        }

        Iterator<Alien> it = aliens.iterator();

        while (it.hasNext()) {
            
            Alien alien = it.next();
            
            if (alien.isVisible()) {

                int y = alien.getY();

                if (y > GROUND - ALIEN_HEIGHT) {
                    ingame = false;
                    message = "Invasion!";
                }

                alien.act(direction);
            }
        }

        // bombs
        // Random generator = new Random();

        for (Alien alien: aliens) {

            int shot = generator.nextInt(10000);
            Alien.Bomb b = alien.getBomb();

            if (shot < CHANCE && alien.isVisible() && b.isDestroyed()) {
                // if (shot < CHANCE && alien.isVisible()) {

                b.setDestroyed(false);
                b.setX(alien.getX());
                b.setY(alien.getY());
            }

            int bombX = b.getX();
            int bombY = b.getY();
            int playerX = player.getX();
            int playerY = player.getY();

            if (player.isVisible() && !b.isDestroyed()) {

                if (bombX >= (playerX)
                    && bombX <= (playerX + PLAYER_WIDTH)
                    && bombY >= (playerY)
                    && bombY <= (playerY + PLAYER_HEIGHT)) {
                    // ImageIcon ii
                    //     = new ImageIcon(explImg);
                    // player.setImage(ii.getImage());
                    player.setDying(true);
                    b.setDestroyed(true);
                    ingame = false;
                    // System.out.println("Lost");
                }
            }

            if (!b.isDestroyed()) {
                
                b.setY(b.getY() + 1);
                
                if (b.getY() >= GROUND - BOMB_HEIGHT) {
                    b.setDestroyed(true);
                }
            }
        }
        }
    }

    // @Override
    public void run() {

        long beforeTime, timeDiff, sleep;

        beforeTime = System.currentTimeMillis();

        while (ingame) {            
            Object push_result = null;

            pushInterpreter = Clojure.var("push307.core",
                                          "java-push-interpreter");

	    GameState gs = new GameState(this);

            
            if (pushProgram != null &&
                pushInterpreter != null) {
		try {
		    push_result = pushInterpreter.invoke(gs,
							 pushProgram);
		}
		catch (Exception e) {
		    System.out.println("Stack trace:");
		    e.printStackTrace();
		    System.out.println("Push program:");
		    System.out.println(pushProgram);
		    System.exit(1);
		}
            }
            else {
                System.out.println("Error");
                System.exit(1);
            }

            try {
                @SuppressWarnings("unchecked")
                ArrayList<Long> result = (ArrayList<Long>)push_result;

                if (result.get(0) == 0) {
                    // no move
                    player.setDX(0);
                }
                else if (result.get(0) == 1) {
                    // right
                    player.setDX(2);
                }
                else if (result.get(0) == 2) {
                    // left
                    player.setDX(-2);
                }
                
                
                if (result.get(1) == 1) {
                    int x = player.getX();
                    int y = player.getY();
                    
                    if (ingame) {
                        if (!shot.isVisible()) {
                            shot = new Shot(x, y);
                        }
                    }
                }
            }
            catch (Exception e) {
                System.out.println("Push return object could not be converted to ArrayList<Long>");
                System.exit(1);
            }

            
            // System.out.println("repaint");
            // repaint();
            animationCycle();

            // only for when watching the game
            // timeDiff = System.currentTimeMillis() - beforeTime;
            // sleep = DELAY - timeDiff;

            // if (sleep < 0) {
            //     sleep = 2;
            // }

            // try {
            //     Thread.sleep(sleep);
            // } catch (InterruptedException e) {
            //     System.out.println("interrupted");
            // }
            
            // beforeTime = System.currentTimeMillis();
        }

        // System.out.println("Done");

        // gameOver();
    }

    // private class TAdapter extends KeyAdapter {

    //     @Override
    //     public void keyReleased(KeyEvent e) {

    //         player.keyReleased(e);
    //     }

    //     @Override
    //     public void keyPressed(KeyEvent e) {

    //         player.keyPressed(e);

    //         int x = player.getX();
    //         int y = player.getY();

    //         int key = e.getKeyCode();

    //         if (key == KeyEvent.VK_SPACE) {
                
    //             if (ingame) {
    //                 if (!shot.isVisible()) {
    //                     shot = new Shot(x, y);
    //                 }
    //             }
    //         }
    //     }
    // }
}
