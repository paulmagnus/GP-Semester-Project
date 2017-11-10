// THIS IS NO LONGER USED
package com.zetcode;

import java.util.ArrayList;

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
    
    public int[][] getAlienPositions() {
        ArrayList<Alien> aliens = board.getAliens();
        int[][] positions = new int[aliens.size()][2];

        for(int i = 0; i < aliens.size(); i++) {
            positions[i] = new int[] {aliens.get(i).getX(),
                                      aliens.get(i).getY()};
        }

        return positions;
    }

    public int[][] getBombPositions() {
        ArrayList<Alien> aliens = board.getAliens();
        ArrayList<Alien.Bomb> bombs = new ArrayList<Alien.Bomb>();
        
        for(Alien a : aliens) {
            if(! a.getBomb().isDestroyed()) {
                bombs.add(a.getBomb());
            }
        }

        int[][] positions = new int[bombs.size()][2];
        
        for(int i = 0; i < bombs.size(); i++) {
            positions[i] = new int[] {bombs.get(i).getX(),
                                      bombs.get(i).getY()};
        }

        return positions;
    }
}